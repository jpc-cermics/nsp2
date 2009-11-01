/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Graphic library
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"
#include "nsp/object.h"

extern void nsp_figure_change3d_orientation(BCG *Xgc,NspGraphic *Obj,
					    double theta, double alpha,int *pt);
extern void nsp_figure_unzoom(NspGraphic *Obj);
extern void nsp_figure_zoom(BCG *Xgc,NspGraphic *Obj, int *bbox1);



struct rec_object {int code; void *obj;};
struct rec_scale { int code; double *Wrect,*Frect,*Frect_kp; char logflag[2]; } ;
struct rec_nscale { int code; char *flag;double *Wrect,*Frect,*Arect,*Frect_kp; char logflag[2]; } ;
typedef void  (*record_action) (void*); 
typedef void  (*record_scale_change)(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
typedef void  (*record_new_angles)(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);

typedef  struct  {
  int  code ;        
  void  (*replay) (BCG *,void*);
  void  (*clean) (void*);  
  void  (*unscale) (void*);
  void  (*scale_change)(BCG *,void *plot, int *flag, double *bbox, int *aaint,char *strf, int undo, int *bbox1, double *subwin, int win_num);
  void  (*new_angles)(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox, int *pt);
} Record_table ;

#define CODEobject               0
#define CODEendplots             1

static void replay_graphic_object(BCG *Xgc,void  *theplot);
static void clean_graphic_object(void *plot);
static void scale_change_graphic_object(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag, 
					int undo, int *bbox1, double *subwin, int win_num);
static void unscale_graphic_object(void *plot);
static void new_angles_graphic_object(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);

static Record_table record_table [] ={
  {CODEobject,replay_graphic_object,clean_graphic_object,unscale_graphic_object,
   scale_change_graphic_object   ,new_angles_graphic_object}, 
};     	


static int store_record(BCG *Xgc,int code ,void *plot);

/* still used in new graphics 
 * 
 */

void tape_store_graphic_object(BCG *Xgc,void *vobj)
{
  NspObject *obj_cp;
  struct rec_object *lplot= MALLOC(sizeof(struct rec_object));
  if (lplot != NULL)
    {
      if ((obj_cp = nsp_object_copy_and_name("Obj",vobj)) != NULLOBJ )
	{
	  lplot->obj = obj_cp ;
	  store_record(Xgc,CODEobject, lplot);
	  return;
	}
    }
  Scistring("Out of memory in ostore_graphic_object \n");
  return; 
}

static void replay_graphic_object(BCG *Xgc,void  *theplot)
{
  struct rec_object *lplot= theplot ;
  NspGraphic *G = (NspGraphic *) lplot->obj;
  G->type->draw(Xgc,G,NULL);
}

static void clean_graphic_object(void *plot) 
{
  NspObject *obj = ((struct rec_object *) plot)->obj;
  nsp_object_destroy(&obj);
};


static void new_angles_graphic_object(BCG *Xgc,void *plot, double *theta, 
				      double *alpha, int *iflag, int *flag, double *bbox,int *pt)
{
  struct rec_object *lplot= plot ;
  NspGraphic *G = (NspGraphic *) lplot->obj;
  nsp_figure_change3d_orientation(Xgc,G,*theta,*alpha,pt);
}

static void scale_change_graphic_object(BCG *Xgc,void *plot, int *flag, double *b1,
					int *aaint,char *strflag, 
					int undo, int *bbox1, double *subwin, int win_num)
{
  struct rec_object *lplot= plot ;
  NspGraphic *G = (NspGraphic *) lplot->obj;
  nsp_figure_zoom(Xgc,G,bbox1);
}

static void unscale_graphic_object(void *plot)
{
  struct rec_object *lplot= plot ;
  NspGraphic *G = (NspGraphic *) lplot->obj;
  nsp_figure_unzoom(G);
}

static int store_record(BCG *Xgc,int code ,void *plot)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE && code != CODEobject ) return 1 ;
  if ( list == NULL)
    {
      list  = MALLOC(sizeof(list_plot));
      if (list != NULL)
	{
	  ((plot_code *) plot )->code = code; 
	  list->theplot=plot;
	  list->next=NULL;
	  list->previous=NULL;
	  Xgc->plots= list;
	  Xgc->last_plot= list;
	}
      else
	{
	  Scistring("ostore_ (store-1): malloc No more Place");
	  return FAIL;
	}
    }
  else 
    {
      list = Xgc->last_plot;
      list->next=MALLOC(sizeof(list_plot));
      if (list->next != NULL)
	{
	  ((plot_code *) plot )->code = code; 
	  list->next->theplot=plot;
	  list->next->previous=list;
	  list->next->next=NULL;
	  Xgc->last_plot = list->next;
	}
      else 
	{
	  Scistring("ostore_ (store-3):No more Place\n");
	  return FAIL;
	}
    }
  return OK;
}


void tape_clean_plots(BCG *Xgc,int winnumber)
{
  int flag = FAIL;
  list_plot *list = Xgc->plots,* list1 ;
  while (list)
    {
      if (list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code ;
	  if ( record_table[code].clean != NULL) record_table[code].clean(list->theplot);
	  FREE(list->theplot);
	  flag = OK;
	}
      list1=list;
      list =list->next;
      FREE(list1);
    }
  Xgc->plots = NULL;
  /* nothing to do if window was not present */
  if ( flag == FAIL ) return ;
  /* reset scales to default */ 
  xgc_reset_scales_to_default(Xgc);
}


/*-------------------------------------------------------------------------
 * Change les angles alpha theta dans tous les plot3d stockes 
 * change  aussi flag et box suivant la valeur de iflag.
 * iflag est de longueur [4] si iflag[i] != 0 cela veut dire qu'il faut changer le 
 * flag[i] en utilisant celui de l'argument flag.
 * iflag[3] sert a dire s'il faut ou pas changer bbox 
 *---------------------------------------------------------------------------*/

void tape_new_angles_plots(BCG *Xgc, int winnumber, double *theta, double *alpha, 
		      int *iflag, int *flag, double *bbox, int *pt)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE ) return ;
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  if ( record_table[code].new_angles !=  NULL) 
	    record_table[code].new_angles(Xgc,list->theplot,theta,alpha,iflag,flag,bbox,pt);
	}
      list =list->next;
    }
}

/*--------------------------------------------------
 * change the scale in all the recorded graphics 
 *   if flag[0]== 1  bbox is changed 
 *   if flag[1]== 1  aaint is changed 
 *   if flag[2]== 1  then strfflag[1] is changed if (strflag[2] coincide with given one )
 *   if undo = 1 then the work can be undone (with unzoom)
 *   else undo = 1 unzoom cannot be performed 
 *   if subwin == NULL 
 *       => we must find the subwin asscoiated to bbox1
 *--------------------------------------------------*/

static void scale_change_plots(BCG *Xgc,int winnumber, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE ) return ;
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  if ( record_table[code].scale_change != NULL) 
	    record_table[code].scale_change(Xgc,list->theplot,flag,bbox,aaint,strflag,
					    undo,bbox1,subwin,winnumber);
	}
      list =list->next;
    }
}

/*--------------------------------------------------------------------
 * scale undo : used in unzoom 
 *--------------------------------------------------------------------*/

static void unscale_plots(BCG *Xgc,int winnumber)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE ) return ;
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  if ( record_table[code].unscale != NULL)  record_table[code].unscale(list->theplot);
	}
      list =list->next;
    }
}

/*-------------------------------------------------------
 * checks if recorded list contains 3d graphics 
 *-------------------------------------------------------*/

int tape_check_recorded_3D(BCG *Xgc,int winnumber)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE ) return FAIL ;
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  switch (code) 
	    {
	      return OK;
	      break;
	    }
	}
      list =list->next;
    }
  return FAIL;
}


/*---------------------------------------------------------------------
 *  restore scales (unzoom) and redraw stored graphics 
 *---------------------------------------------------------------------------*/

void tape_replay_undo_scale(BCG *Xgc,int winnumber)
{ 
  unscale_plots(Xgc,winnumber);
  tape_replay(Xgc,winnumber);
}

/*---------------------------------------------------------------------
 * used when zooming: replay with a new scale 
 * the new scale is coded in bbox=[xmin,ymin,xmax,ymax] 
 * the problem is a bit complex if we have many subwindows 
 *---------------------------------------------------------------------------*/

void tape_replay_new_scale(BCG *Xgc,int winnumber, int *flag, int *aaint,double *bbox, 
			   int *ibbox)
{ 
  /* get the bounding box in pixel */
  int bbox1[4];
  if ( ibbox != NULL) 
    {
      scale_change_plots(Xgc,winnumber,flag,bbox,aaint,NULL,1,ibbox,NULL);
    }
  else
    {
      bbox1[0]= XDouble2Pixel(Xgc->scales,bbox[0]);
      bbox1[1]= YDouble2Pixel(Xgc->scales,bbox[1]);
      bbox1[2]= XDouble2Pixel(Xgc->scales,bbox[2]);
      bbox1[3]= YDouble2Pixel(Xgc->scales,bbox[3]);
      scale_change_plots(Xgc,winnumber,flag,bbox,aaint,NULL,1,bbox1,NULL);
    }
  tape_replay(Xgc,winnumber);
}

/*---------------------------------------------------------------------
 * replay with a new Scale but undo is impossible 
 * used for automatic scales 
 *---------------------------------------------------------------------------*/

void tape_replay_new_scale_1(BCG *Xgc,int winnumber, int *flag, int *aaint, double *bbox,char *strflag)
{ 
  /* here we want to change (bbox,aaint,strfag) but only for recorded graphics 
   * which are on the same subwin as the current one 
   * and we do not want this operation to be undone ==> undo =0 
   */
  scale_change_plots(Xgc,winnumber,flag,bbox,aaint,strflag,0,NULL,Xgc->scales->subwin_rect);
  tape_replay(Xgc,winnumber);
}

/*---------------------------------------------------------------------
 *  changes theta, alpha  flag and bbox 
 *  then redraw recorded graphics 
 *---------------------------------------------------------------------------*/

void tape_replay_new_angles(BCG *Xgc,int winnumber,int *iflag, int *flag,double *theta, double *alpha, double *bbox)
{ 
  tape_new_angles_plots(Xgc,winnumber,theta,alpha,iflag,flag,bbox,NULL);
  tape_replay(Xgc,winnumber);
}

/*---------------------------------------------------------------------
 * redraw stored graphics 
 * in the window (or file) described by Xgc
 *---------------------------------------------------------------------------*/

void tape_replay(BCG *Xgc,int winnumber)
{ 
  double WRect[]={0,0,1,1};
  list_plot *list;
  if ( Xgc == NULL ) return ;
  if ( Xgc->record_flag == FALSE ) return ;
  Xgc->record_flag = FALSE; /* be sure not to record during replay */
  list = Xgc->plots ;
  /* be sure that scales are back to default ? */
  move_subwindow_scale_to_front(Xgc,WRect);
  while (list)
    {
      if ( list->theplot != NULL) 
	record_table[((plot_code *) list->theplot)->code ].replay(Xgc,list->theplot);
      list =list->next;
    }
  /* Is there a replay handler */
  nsp_gr_handler(Xgc,winnumber);
  Xgc->record_flag = TRUE; /* be sure to set back record_flg to its proper stat */
}

/* replay the plots of an Xgc with an other Xgc 
 * Xgc1 is used for list of recorded graphics.
 */

void tape_replay_mix(BCG *Xgc,BCG *Xgc1, int winnumber)
{ 
  list_plot *list;
  if ( Xgc == NULL ) return ;
  if ( Xgc->record_flag == FALSE ) return ;
  Xgc->record_flag = FALSE; /* be sure not to record during replay */
  list = Xgc1->plots ;
  while (list)
    {
      if ( list->theplot != NULL) 
	record_table[((plot_code *) list->theplot)->code ].replay(Xgc,list->theplot);
      list =list->next;
    }
  /* Is there a replay handler */
  nsp_gr_handler(Xgc,winnumber);
  Xgc->record_flag = TRUE; /* be sure to set back record_flg to its proper stat */
}


/*
 * search a graphic object in the recorded objects 
 */

NspObject * tape_search_graphic_object(BCG *Xgc,int winnumber)
{ 
  list_plot *list;
  if ( Xgc == NULL ) return NULL ;
  list = Xgc->plots ;
  while (list)
    {
      if ( list->theplot != NULL && ((plot_code *) list->theplot)->code == CODEobject )
	return ((struct rec_object *) list->theplot)->obj ;
      list =list->next;
    }
  return NULL;
}


static int load_LI(XDR *xdrs, int *ix);
static int load_VectC(XDR *xdrs, char **nx);
static char *SciF_version;

#define assert(ex) {if (!(ex)){ sciprint("Graphic load_/Save Error \r\n");return(0);}}

static int load_object(BCG *Xgc)
{
  struct rec_object *lplot;
  lplot= ((struct rec_object *) MALLOC(sizeof(struct rec_object)));
  if (lplot != NULL)
    {
      NspObject *obj = nsp_object_xdr_load(Xgc->xdrs);
      if ( obj == NULL) return 0;
      lplot->obj = obj ;
      if ( store_record(Xgc,CODEobject,(char *) lplot) == 0)
	return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (champ): No more place \n");
      return(0);
    }
  return(1);
}


/*---------------------------------------------------------------------
 *  code for reload operations 
 *---------------------------------------------------------------------------*/

static char RFname[128];
static FILE *RF ;

typedef  struct  {
  int code;
  char *name;
  int  (*load)(BCG *);
} Load_Table;

static Load_Table load_table [] ={
  {CODEobject       	     ,"object",           load_object }
};     	

#ifdef __MSC__
#ifndef __STDC__
#define __STDC__
#endif
#endif 

int tape_load(BCG *Xgc,const char *fname1)
{
  XDR xdrs[1];
  int type,record;
  strncpy(RFname,fname1,128);
#ifdef __STDC__
  RF = fopen(RFname,"rb") ;
#else
  RF = fopen(RFname,"r") ;
#endif
  if( RF == NULL)
    {
      sciprint("fopen failed\r\n") ;
      return(0);
    }
  xdrstdio_create(xdrs, RF, XDR_DECODE) ;
  Xgc->xdrs = xdrs;
  if ( load_VectC(Xgc->xdrs,&SciF_version) == 0 ) 
    {
      sciprint("Wrong plot file : %s\n\n",fname1);
      return(0);
    }

  if ( strncmp(SciF_version,"Nsp",3) != 0 )
    {
      sciprint("Not a save graphics file: %s\n\n",fname1);
      return(0);
    }
  if ( strcmp(SciF_version,"Nsp_1.0") != 0 )
    {
      sciprint("Wrong version of saved graphics %s : %s\n\n",
	       SciF_version,fname1);
      return(0);
    }

  while ( load_LI(Xgc->xdrs,&type) != 0 ) 
    {
      /* sciprint("XXXsaved code %d\n",type); */
      if ( type < 0 || type >  CODEendplots) 
	{
	  sciprint("Something wrong while reloading %s\n\n",fname1);
	  break;
	}
      if ( type ==  CODEendplots ) break; /* normal end */
      if ( load_table[type].load(Xgc) == 0 ) break;
    }
  assert(fflush((FILE *)xdrs->x_private) != EOF) ; 
  assert(fclose(RF) != EOF) ;

  /** we plot the load_ed graphics **/
  record= Xgc->graphic_engine->xget_recording(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,TRUE);
  Xgc->graphic_engine->pixmap_resize(Xgc);
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_replay(Xgc,Xgc->CurWindow);
  Xgc->graphic_engine->xset_recording(Xgc,record);
  return(0);
}

/*-------------------------------------------------------------
 * utilities for xdr coded data reloading 
 *-------------------------------------------------------------*/


static int save_LI(XDR *xdrs,int ix);
static int save_VectC(XDR *xdrs,char *nx, int l);
static int save_object(BCG *Xgc,void * theplot )
{
  struct rec_object *lplot= theplot ;
  NspObject *obj = (NspObject *) lplot->obj;
  if ( obj->type->save(Xgc->xdrs,obj) == FAIL) return 0;
  return 1;
}

typedef  struct  {
  int code;
  char *name;
  int  (*save)(BCG *Xgc,void *);
} Save_Table;

static Save_Table save_table [] ={
  {CODEobject       	     ,"object",           save_object }
};     	

int tape_save(BCG *Xgc,const char *fname1, int winnumber)
{
  char fname[128]; /* FIXME */
  FILE *F ;
  XDR xdrs[1] ;
  list_plot *list = Xgc->plots ;
  static char scig[]={"Nsp_1.0"};
  if ( Xgc->record_flag == FALSE ) return 0 ;
#ifdef lint 
  *unused;
#endif
  strncpy(fname,fname1,128);
#ifdef __STDC__
  F = fopen(fname,"wb") ;
#else
  F = fopen(fname,"w") ;
#endif
  if( F == NULL)
    {
      sciprint("fopen failed\n") ;
      return(0);
    }
  xdrstdio_create(xdrs, F, XDR_ENCODE) ;
  Xgc->xdrs = xdrs;
  save_VectC(Xgc->xdrs,scig,((int)strlen(scig))+1) ;
  
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  /* sciprint("XXXsaved code %d\n",code); */
	  if (save_LI(Xgc->xdrs,code) == 0) break;
	  if (save_table[code].save(Xgc,list->theplot) == 0) break;
	}
      list =list->next;
    }
  save_LI(Xgc->xdrs,CODEendplots);
  assert(fflush((FILE *)xdrs->x_private) != EOF) ; 
  assert(fclose(F) != EOF) ;
  return(0);
}


static int save_LI(XDR *xdrs,int ix)
{
  u_int szof = sizeof(int) ;
  u_int count = 1;
  assert( xdr_vector(xdrs, (char *)&ix, count, szof, (xdrproc_t) xdr_int)) ;
  return(1);
}

static int save_VectC(XDR *xdrs,char *nx, int l)
{ 
  char nx1='1';
  u_int szof = l*sizeof(char);
  assert( xdr_vector(xdrs,(char *) &szof,(unsigned)1,(unsigned) sizeof(unsigned),
		     (xdrproc_t) xdr_u_int)) ;
  if ( nx == (char  *) NULL && l == (int) 1)
    { assert( xdr_opaque(xdrs, &nx1,szof)); } 
  else 
    { assert( xdr_opaque(xdrs, nx,szof)); }
  return(1);
}



static int load_LI(XDR *xdrs, int *ix)
{
  u_int rszof = sizeof(int) ;
  u_int rcount = (u_int)1;
  assert( xdr_vector(xdrs, (char *)ix, rcount, rszof, (xdrproc_t) xdr_int)) ;
  return(1);
}


static int load_VectC(XDR *xdrs, char **nx)
{
  u_int rszof;
  assert( xdr_vector(xdrs,(char *) &rszof,(u_int)1,(u_int) sizeof(u_int),
		     (xdrproc_t) xdr_u_int)) ;
  *nx = (char *)  MALLOC(rszof);
  if ( *nx == NULL) return(0);
  assert( xdr_opaque(xdrs, *nx,rszof));
  return(1);
}

 



