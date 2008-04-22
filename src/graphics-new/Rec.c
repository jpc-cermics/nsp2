/*/* Nsp
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

/*------------------------------------------------------------------------
 * Graphic High Level Recording function 
 * Oct 2004 
 *   le graphique est maintenant asynchrone 
 *   cest pourquoi il y a incr_plots et plots 
 *   Il reste a faire jouer un role a recorded_flag 
 *   different i.e l'enlever et qu'il soit juste actif 
 *   quand on recopie pour savoir si incr_plot est copie ou 
 *   detruit 
 *   La question en suspens est: est-ce necessaire de restorer 
 *   un graphique synchrone sachant que xset('whow') soit le rendre 
 *   synchrone 
 *   Enfin peut-il y a voir ambiguité ? 
 *   on croit qu'on doit dessiner juste l'incrément alors que non ? 
 *---------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/graphics/Rec_private.h"
typedef int gboolean ;
#include "nsp/graphic.h" 

static int MaybeCopyVect3dPLI  (int ,int **,const int *,int l); 
static int CopyVectLI  (int **,const int *,int ); 
static int CopyVectF  (double **,const double *,int ); 
static int CopyVectC  (char **,const char *,int ); 
static int CopyVectS  (char ***,char **); 

static void store_void(BCG *Xgc,int code);
static void store_int(BCG *Xgc,int code,int val);
static void store_int2(BCG *Xgc,int code,int val, int val1);
static void store_int4(BCG *Xgc,int code,int vals[]);
static void store_double4(BCG *Xgc,int code,double vals[]);

/*---------------------------------------------------------------------
 * basic primitives 
 *---------------------------------------------------------------------------*/

void store_initialize_gc(BCG *Xgc) {  store_void(Xgc,CODEinitialize_gc); }

static void replay_initialize_gc(BCG *Xgc,void * theplot ) { Xgc->graphic_engine->scale->initialize_gc(Xgc);}


void store_clipping_p(BCG *Xgc,int x,int y,int w,int h)
{
  int vals[4];
  vals[0]= x; vals[1]= y; vals[2] = w ; vals[3] = h;
  store_int4(Xgc,CODEclipping_p,vals);
}

static void replay_clipping_p(BCG *Xgc,void *theplot)
{
  struct rec_int4 *lplot  = theplot;
  Xgc->graphic_engine->scale->xset_clipping_p(Xgc, lplot->vals[0], lplot->vals[1],lplot->vals[2],lplot->vals[3]);
}

void store_clipgrf(BCG *Xgc) {  store_void(Xgc,CODEclipgrf); }

static void replay_clipgrf(BCG *Xgc,void * theplot ) { Xgc->graphic_engine->scale->xset_clipgrf(Xgc);}

void store_alufunction1(BCG *Xgc,int val)
{
  store_int(Xgc,CODEalufunction1,val);
}

static void replay_alufunction1(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_alufunction1(Xgc,val);
}


void store_background(BCG *Xgc,int val)
{
  store_int(Xgc,CODEbackground,val);
}

static void replay_background(BCG *Xgc,void *theplot)
{
  int val =  ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_background(Xgc,val);
}



void store_unclip(BCG *Xgc)
{
  store_void(Xgc,CODEunclip);
}

static void replay_unclip(BCG *Xgc,void * theplot ) { Xgc->graphic_engine->scale->xset_unclip(Xgc);}


void store_test(BCG *Xgc)
{
  store_void(Xgc,CODEtest);
}

static void replay_test(BCG *Xgc,void * theplot ) 
{ 
  Xgc->graphic_engine->scale->xset_test(Xgc);
}


void store_clip(BCG *Xgc,double x[])
{
  store_double4(Xgc,CODEclip,x);
}

static void replay_clip(BCG *Xgc,void *theplot)
{
  struct rec_double4 *lplot  = theplot;
  Xgc->graphic_engine->scale->xset_clip(Xgc,lplot->vals);
}

void store_pattern(BCG *Xgc,int val)
{
  store_int(Xgc,CODEpattern,val);
}

static void replay_pattern(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_pattern(Xgc,val);
}

void store_font_size(BCG *Xgc,int val)
{
  store_int(Xgc,CODEfont_size,val);
}

static void replay_font_size(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_font_size(Xgc,val);
}
void store_font(BCG *Xgc,int val,int val1)
{
  store_int2(Xgc,CODEfont,val,val1);
}

static void replay_font(BCG *Xgc,void *theplot)
{
  struct rec_int2 *lplot  = theplot;
  Xgc->graphic_engine->scale->xset_font(Xgc,lplot->val,lplot->val1);
}

void store_foreground(BCG *Xgc,int val)
{
  store_int(Xgc,CODEforeground,val);
}

static void replay_foreground(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_foreground(Xgc,val);
}

void store_hidden3d(BCG *Xgc,int val)
{
  store_int(Xgc,CODEhidden3d,val);
}

static void replay_hidden3d(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_hidden3d(Xgc,val);
}

void store_absourel(BCG *Xgc,int val)
{
  store_int(Xgc,CODEabsourel,val);
}

static void replay_absourel(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_absourel(Xgc,val);
}

void store_dash(BCG *Xgc,int val)
{
  store_int(Xgc,CODEdash,val);
}

static void replay_dash(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_dash(Xgc,val);
}

void store_mark_size(BCG *Xgc,int val)
{
  store_int(Xgc,CODEmark_size,val);
}

static void replay_mark_size(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_mark_size(Xgc,val);
}

void store_mark(BCG *Xgc,int val,int val1)
{
  store_int2(Xgc,CODEmark,val,val1);
}

static void replay_mark(BCG *Xgc,void *theplot)
{
  struct rec_int2 *lplot  = theplot;
  Xgc->graphic_engine->scale->xset_mark(Xgc,lplot->val,lplot->val1);
}

void store_pixmapOn(BCG *Xgc,int val)
{
  store_int(Xgc,CODEpixmapOn,val);
}

static void replay_pixmapOn(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,val);
}

void store_thickness(BCG *Xgc,int val)
{
  store_int(Xgc,CODEthickness,val);
}

static void replay_thickness(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  Xgc->graphic_engine->scale->xset_thickness(Xgc,val);
}

void store_usecolor(BCG *Xgc,int val)
{
  store_int(Xgc,CODEusecolor,val);
}


static int special_color=0;

void UseColorFlag(int flag)
{
  special_color=flag;
}

static void replay_usecolor(BCG *Xgc,void *theplot)
{
  int val = ((struct rec_int *) theplot)->val;
  if ( special_color == 0) 
    Xgc->graphic_engine->scale->xset_usecolor(Xgc,val);
}

void store_show(BCG *Xgc) { store_void(Xgc,CODEshow); }

static void replay_show(BCG *Xgc,void * theplot ) { Xgc->graphic_engine->scale->xset_show(Xgc);}

void store_pixmapclear(BCG *Xgc) {  store_void(Xgc,CODEpixmapclear);}

static void replay_pixmapclear(BCG *Xgc,void * theplot ) { Xgc->graphic_engine->scale->xset_pixmapclear(Xgc);}

void store_fpf_def(BCG *Xgc) { store_void(Xgc,CODEfpf_def); }

static void replay_fpf_def(BCG *Xgc,void * theplot ) { Xgc->graphic_engine->scale->xset_fpf_def(Xgc);}

void store_fpf(BCG *Xgc,char *fpf) { 
  struct rec_str *lplot= MALLOC(sizeof(struct rec_str));
  if (lplot != NULL)
    {
      if ( CopyVectC(&(lplot->str),fpf,strlen(fpf)+1) )
	{
	  store_record(Xgc,CODEfpf, lplot);
	  return;
	}
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

static void replay_fpf(BCG *Xgc,void * theplot ) { 
  struct rec_str *lplot = theplot;
  Xgc->graphic_engine->scale->xset_fpf(Xgc,lplot->str);
}

static void clean_fpf(void *plot) {
  struct rec_str *lplot = plot;
  FREE(lplot->str);
};




/*-----------------------------------------------------------------------------
 * colormap
 *-----------------------------------------------------------------------------*/

void store_colormap(BCG *Xgc,int m, int n,double colors[])
{
  struct rec_colormap *lplot = MALLOC(sizeof(struct rec_colormap));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->m = m; 
      lplot->n = n; 
      lplot->colors= NULL;
      if ( CopyVectF(&(lplot->colors),colors,m*n) )
	{
	  store_record(Xgc,CODEColormap, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}

static void replay_colormap(BCG *Xgc,void  *theplot)
{
  struct rec_colormap *lplot = theplot;
  Xgc->graphic_engine->scale->xset_colormap(Xgc,lplot->m,lplot->colors);
}

static void clean_colormap(void *theplot) {
  struct rec_colormap *lplot = theplot;
  FREE(lplot->colors);
};

void store_default_colormap(BCG *Xgc) { store_void(Xgc,CODEdefault_colormap); }

static void replay_default_colormap(BCG *Xgc,void * theplot ) 
{ 
  Xgc->graphic_engine->scale->xset_default_colormap(Xgc);
}



/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

void store_drawarc_1(BCG *Xgc,double arc[])
{ 
  int i;
  struct rec_drawarc *lplot= ((struct rec_drawarc *) MALLOC(sizeof(struct rec_drawarc)));
  if (lplot != NULL)
    {
      for ( i = 0 ; i < 6 ; i++) lplot->arc[i]=arc[i];
      store_record(Xgc,CODEdrawarc_1,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

static void replay_drawarc_1(BCG *Xgc,void  *theplot)
{
  struct rec_drawarc *lplot = theplot;
  Xgc->graphic_engine->scale->drawarc(Xgc,lplot->arc);
}


static void clean_drawarc_1(void *plot) {};

/*-----------------------------------------------------------------------------
 * fillarcs 
 *-----------------------------------------------------------------------------*/

static void store_fillarcs_G(BCG *Xgc,int code,double vects[],int fillvect[], int n,int size)
{
  struct rec_fillarcs *lplot = MALLOC(sizeof(struct rec_fillarcs));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->n = n; 
      lplot->vects= NULL;
      lplot->fillvect=NULL;
      if (
	  CopyVectF(&(lplot->vects),vects,n*size) &&
	  CopyVectLI(&(lplot->fillvect),fillvect,n)
	  )
	{
	  store_record(Xgc,code, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}

void store_fillarcs_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  store_fillarcs_G(Xgc,CODEfillarcs_1,vects,fillvect,n,6);
}

static void replay_fillarcs_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  Xgc->graphic_engine->scale->fillarcs(Xgc, lplot->vects, lplot->fillvect,lplot->n );
}

static void clean_fillarcs_1(void *theplot) {
  struct rec_fillarcs *lplot = theplot;
  FREE(lplot->vects);
  FREE(lplot->fillvect);
};

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

void store_drawarcs_1(BCG *Xgc,double vects[], int style[], int n)
{
  store_fillarcs_G(Xgc,CODEdrawarcs_1,vects,style,n,6);
}


static void replay_drawarcs_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  Xgc->graphic_engine->scale->drawarcs(Xgc, lplot->vects, lplot->fillvect,lplot->n );
}


static void clean_drawarcs_1(void *plot) {
  struct rec_fillarcs *lplot = plot;
  FREE(lplot->vects);
  FREE(lplot->fillvect);
};



/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static void store_fillpolyline_G(BCG *Xgc,int code,double vx[],double vy[], int n,int closeflag)
{
  struct rec_fillpolyline *lplot = MALLOC(sizeof(struct rec_fillpolyline));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->n = n; 
      lplot->closeflag = closeflag; 
      lplot->vx= NULL;
      lplot->vy= NULL;
      if (
	  CopyVectF(&(lplot->vx),vx,n) &&
	  CopyVectF(&(lplot->vy),vy,n)
	  )
	{
	  store_record(Xgc,code, lplot); return;
	}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


void store_fillpolyline_1(BCG *Xgc,double *vx, double *vy,int n,int closeflag)
{
  store_fillpolyline_G(Xgc,CODEfillpolyline_1,vx,vy,n,closeflag);
}


static void replay_fillpolyline_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  Xgc->graphic_engine->scale->fillpolyline(Xgc,lplot->vx, lplot->vy,lplot->n, lplot->closeflag);
}

static void clean_fillpolyline_1(void *plot) {
  struct rec_fillpolyline *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
};



/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

void store_drawarrows_1(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag)
{ 
  struct rec_arrows *lplot = MALLOC(sizeof(struct rec_arrows));
  int rep = TRUE ;
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->n = n; 
      lplot->iflag = iflag ; 
      lplot->as = as; 
      lplot->vx= NULL;
      lplot->vy=NULL;
      if (iflag != 0) 
	rep= CopyVectLI(&(lplot->style),style,n/2);
      else 
	lplot->def_style = *style;
      if ( rep &&  CopyVectF(&(lplot->vx),vx,n) &&  CopyVectF(&(lplot->vy),vy,n) )
	{
	  store_record(Xgc,CODEdrawarrows_1, lplot);
	  return;
	}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}

static void replay_drawarrows_1(BCG *Xgc,void  *theplot)
{
  struct rec_arrows *lplot = theplot;
  if (lplot->iflag != 0) 
    Xgc->graphic_engine->scale->drawarrows(Xgc,lplot->vx, lplot->vy,lplot->n,lplot->as,lplot->style,lplot->iflag);
  else 
    Xgc->graphic_engine->scale->drawarrows(Xgc,lplot->vx, lplot->vy,lplot->n,lplot->as,&lplot->def_style,lplot->iflag);
}


static void clean_drawarrows_1(void *plot) {
  struct rec_arrows *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
  if ( lplot->iflag != 0) FREE(lplot->style);
};


/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

void store_drawaxis_1(BCG *Xgc,double *alpha, int *nsteps,double *initpoint, double *size)
{
  /* 
     int initpoint1[2],alpha1;
     double size1[3];
  */
  struct rec_drawaxis *lplot = MALLOC(sizeof(struct rec_drawaxis));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->alpha = *alpha; 
      lplot->nsteps = *nsteps ; 
      lplot->initpoint[0] = initpoint[0] ; 
      lplot->initpoint[1] = initpoint[1] ; 
      lplot->size[0] = size[0] ; 
      lplot->size[1] = size[1] ; 
      lplot->size[2] = size[2] ; 
      store_record(Xgc,CODEdrawaxis_1, lplot);
      return;
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_drawaxis_1(BCG *Xgc,void  *theplot)
{
  struct rec_drawaxis *lplot = theplot;
  Xgc->graphic_engine->scale->drawaxis(Xgc,&lplot->alpha,&lplot->nsteps,lplot->initpoint,lplot->size);
}

static void clean_drawaxis_1(void  *theplot) {}


/*-----------------------------------------------------------------------------
 *  cleararea
 *-----------------------------------------------------------------------------*/

void store_cleararea_1(BCG *Xgc,double x, double y, double w, double h)
{
  double vals[4]={x,y,w,h};
  store_double4(Xgc,CODEcleararea_1,vals);
}


static void replay_cleararea_1(BCG *Xgc,void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  Xgc->graphic_engine->scale->cleararea(Xgc,lplot->vals[0], lplot->vals[1],lplot->vals[2],lplot->vals[3]);
}

static void clean_cleararea_1(void  *theplot) {}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

void store_fillarc_1(BCG *Xgc,double arc[])
{ 
  struct rec_drawarc *lplot= ((struct rec_drawarc *) MALLOC(sizeof(struct rec_drawarc)));
  if (lplot != NULL)
    {
      int i;
      for ( i = 0 ; i < 6 ; i++) lplot->arc[i]=arc[i];
      store_record(Xgc,CODEfillarc_1,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}


static void replay_fillarc_1(BCG *Xgc,void  *theplot)
{
  struct rec_drawarc *lplot = theplot;
  Xgc->graphic_engine->scale->fillarc(Xgc,lplot->arc);
}

static void clean_fillarc_1(void  *theplot) {}

/*-----------------------------------------------------------------------------
 *  fillrectangle
 *-----------------------------------------------------------------------------*/

void store_fillrectangle_1(BCG *Xgc,double rect[])
{ 
  store_double4(Xgc,CODEfillrectangle_1,rect);
}


static void replay_fillrectangle_1(BCG *Xgc,void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  Xgc->graphic_engine->scale->fillrectangle(Xgc, lplot->vals);
}

static void clean_fillrectangle_1(void  *theplot) {}

/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

void store_drawpolyline_1(BCG *Xgc, double *vx, double *vy ,int n, int closeflag)
{
  store_fillpolyline_G(Xgc,CODEdrawpolyline_1,vx,vy,n,closeflag);
}


static void replay_drawpolyline_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  Xgc->graphic_engine->scale->drawpolyline(Xgc,lplot->vx, lplot->vy,lplot->n, lplot->closeflag);
}


static void clean_drawpolyline_1(void *plot) {
  struct rec_fillpolyline *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
};




/*-----------------------------------------------------------------------------
 *  fillpolylines
 *-----------------------------------------------------------------------------*/

void store_fillpolylines_1(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p, int v1)
{
  struct rec_fillpolylines *lplot = MALLOC(sizeof(struct rec_fillpolylines));
  int rep= TRUE;
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->n = n; 
      lplot->p = p ; 
      lplot->v1 = v1 ; 
      lplot->vx= NULL;
      lplot->vy=NULL;
      lplot->fillvect=NULL;
      if ( v1 == 2 ) 
	rep = CopyVectLI(&(lplot->fillvect),fillvect,n*p);
      else 
	rep = CopyVectLI(&(lplot->fillvect),fillvect,n);
      if (
	  rep && 
	  CopyVectF(&(lplot->vx),vx,n*p) &&
	  CopyVectF(&(lplot->vy),vy,n*p) 
	  )
	{
	  store_record(Xgc,CODEfillpolylines_1, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_fillpolylines_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolylines *lplot = theplot;
  Xgc->graphic_engine->scale->fillpolylines(Xgc,lplot->vx, lplot->vy,lplot->fillvect,lplot->n, lplot->p,lplot->v1);
}



static void clean_fillpolylines_1(void *plot) {
  struct rec_fillpolylines *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
  FREE(lplot->fillvect);
};




/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

void store_drawpolymark_1(BCG *Xgc,double *vx, double *vy,int n)
{
  store_fillpolyline_G(Xgc,CODEdrawpolymark_1,vx,vy,n,0);
}


static void replay_drawpolymark_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  Xgc->graphic_engine->scale->drawpolymark(Xgc,lplot->vx, lplot->vy,lplot->n);
}


static void clean_drawpolymark_1(void *plot) {
  struct rec_fillpolyline *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
};


/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

void store_displaynumbers_1(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha)
{
  struct rec_displaynumbers *lplot = MALLOC(sizeof(struct rec_displaynumbers));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->x = NULL; 
      lplot->y = NULL; 
      lplot->n = n ; 
      lplot->z = NULL;
      lplot->alpha = NULL;
      lplot->flag = flag ;
      if (
	  CopyVectF(&(lplot->x),x,n) &&
	  CopyVectF(&(lplot->y),y,n) && 
	  CopyVectF(&(lplot->z),z,n) &&
	  CopyVectF(&(lplot->alpha),alpha,n) 
	  )
	{
	  store_record(Xgc,CODEdisplaynumbers_1, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_displaynumbers_1(BCG *Xgc,void  *theplot)
{
  struct rec_displaynumbers *lplot = theplot;
  Xgc->graphic_engine->scale->displaynumbers(Xgc,lplot->x, lplot->y,lplot->n,lplot->flag, lplot->z,lplot->alpha);
}


static void clean_displaynumbers_1(void *plot) {
  struct rec_displaynumbers *lplot = plot;
  FREE(lplot->x);
  FREE(lplot->y);
  FREE(lplot->z);
  FREE(lplot->alpha);
};

/*-----------------------------------------------------------------------------
 *   drawpolylines
 *-----------------------------------------------------------------------------*/

void store_drawpolylines_1(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p)
{
  struct rec_drawpolylines *lplot = MALLOC(sizeof(struct rec_drawpolylines));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->n = n; 
      lplot->p = p ; 
      lplot->vx= NULL;
      lplot->vy=NULL;
      lplot->drawvect=NULL;
      if (
	  CopyVectF(&(lplot->vx),vx,n*p) &&
	  CopyVectF(&(lplot->vy),vy,n*p) && 
	  CopyVectLI(&(lplot->drawvect),drawvect,n)
	  )
	{
	  store_record(Xgc,CODEdrawpolylines_1, lplot);
	  return;
	}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_drawpolylines_1(BCG *Xgc,void  *theplot)
{
  struct rec_drawpolylines *lplot = theplot;
  Xgc->graphic_engine->scale->drawpolylines(Xgc,lplot->vx, lplot->vy,lplot->drawvect,lplot->n, lplot->p);
}


static void clean_drawpolylines_1(void *plot) {
  struct rec_drawpolylines *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
  FREE(lplot->drawvect);
};




/*-----------------------------------------------------------------------------
 *   drawrectangle
 *-----------------------------------------------------------------------------*/

void store_drawrectangle_1(BCG *Xgc,double rect[])
{
  store_double4(Xgc,CODEdrawrectangle_1,rect);
}


static void replay_drawrectangle_1(BCG *Xgc,void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  Xgc->graphic_engine->scale->drawrectangle(Xgc, lplot->vals);
}


static void clean_drawrectangle_1(void *plot) {}

/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

void store_drawrectangles_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  store_fillarcs_G(Xgc,CODEdrawrectangles_1,vects,fillvect,n,4);
}


static void replay_drawrectangles_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  Xgc->graphic_engine->scale->drawrectangles(Xgc,lplot->vects, lplot->fillvect,lplot->n);
}


static void clean_drawrectangles_1(void *plot) {
  struct rec_fillarcs *lplot = plot;
  FREE(lplot->vects);
  FREE(lplot->fillvect);
};

/*-----------------------------------------------------------------------------
 *  drawsegments
 *-----------------------------------------------------------------------------*/

void store_drawsegments_1(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag)
{
  int rep=TRUE;
  struct rec_segment *lplot = MALLOC(sizeof(struct rec_segment));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->n = n; 
      lplot->iflag = iflag ; 
      lplot->vx= NULL;
      lplot->vy=NULL;
      if (iflag != 0) 
	rep= CopyVectLI(&(lplot->style),style,n/2);
      else 
	rep= CopyVectLI(&(lplot->style),style,1);
      if ( rep &&  CopyVectF(&(lplot->vx),vx,n) && CopyVectF(&(lplot->vy),vy,n)) 
	{
	  store_record(Xgc,CODEdrawsegments_1, lplot);
	  return;
	}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_drawsegments_1(BCG *Xgc,void  *theplot)
{
  struct rec_segment *lplot = theplot;
  Xgc->graphic_engine->scale->drawsegments(Xgc,lplot->vx, lplot->vy,lplot->n,lplot->style,lplot->iflag);
}


static void clean_drawsegments_1(void *plot) {
  struct rec_segment *lplot = plot;
  FREE(lplot->vx);
  FREE(lplot->vy);
  if ( lplot->iflag != 0) FREE(lplot->style);
};



/*-----------------------------------------------------------------------------
 *  displaystring
 *-----------------------------------------------------------------------------*/

void store_displaystring_1(BCG *Xgc,char *string,double x, double y,int flag,double angle)
{
  struct rec_displaystring *lplot = MALLOC(sizeof(struct rec_displaystring));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->x = x; 
      lplot->y = y ; 
      lplot->flag = flag ; 
      lplot->angle= angle;
      lplot->string=NULL;
      if (
	  CopyVectC(&(lplot->string),string,((int)strlen(string))+1) 
	  )
	{
	  store_record(Xgc,CODEdisplaystring_1, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}

static void replay_displaystring_1(BCG *Xgc,void  *theplot)
{
  struct rec_displaystring *lplot = theplot;
  Xgc->graphic_engine->scale->displaystring(Xgc,lplot->string, lplot->x,lplot->y,lplot->flag,lplot->angle);
}


static void clean_displaystring_1(void *plot) {
  struct rec_displaystring *lplot = plot;
  FREE(lplot->string);
};




/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

void store_displaystringa_1(BCG *Xgc,char *string, int ipos)
{
  struct rec_displaystringa *lplot = MALLOC(sizeof(struct rec_displaystringa));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->ipos = ipos; 
      if (
	  CopyVectC(&(lplot->string),string,((int)strlen(string))+1) 
	  )
	{
	  store_record(Xgc,CODEdisplaystringa_1, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_displaystringa_1(BCG *Xgc,void  *theplot)
{
  struct rec_displaystringa *lplot = theplot;
  Xgc->graphic_engine->scale->displaystringa(Xgc,lplot->string, lplot->ipos);
}


static void clean_displaystringa_1(void *plot) {
  struct rec_displaystringa *lplot = plot;
  FREE(lplot->string);
};




/*-----------------------------------------------------------------------------
 * a string in a bounded box : with font size change to fit into the 
 * specified box (only works with driver which properly estimate string sizes)
 *-----------------------------------------------------------------------------*/

void store_xstringb_1(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd)
{
  struct rec_xstringb *lplot = MALLOC(sizeof(struct rec_xstringb));
  if (lplot != NULL)
    {
      /* initialize  */ 
      lplot->x = *xd; 
      lplot->y = *yd ; 
      lplot->wd = *wd ; 
      lplot->hd = *hd;
      lplot->flag = *fflag;
      lplot->string=NULL;
      if (
	  CopyVectC(&(lplot->string),str,((int)strlen(str))+1) )
	{
	  store_record(Xgc,CODExstringb_1, lplot);
	  return;}
    }
  Scistring("\nstore_ Plot (XXXX): No more place \n");
}


static void replay_xstringb_1(BCG *Xgc,void  *theplot)
{
  struct rec_xstringb *lplot = theplot;
  Xgc->graphic_engine->scale->xstringb(Xgc,lplot->string, &lplot->flag,&lplot->x,&lplot->y,&lplot->wd,&lplot->hd);
}


static void clean_xstringb_1(void *plot) {
  struct rec_xstringb *lplot = plot;
  FREE(lplot->string);
};




/*---------------------------------------------------------------------
 * xsetech 
 *---------------------------------------------------------------------------*/

void store_Ech(BCG *Xgc,double *WRect, double *FRect, char *logflag)
{
  struct rec_scale *lplot;
  lplot= ((struct rec_scale *) MALLOC(sizeof(struct rec_scale)));
  if (lplot != NULL)
    {
      lplot->logflag[0]=logflag[0];
      lplot->logflag[1]=logflag[1];
      if ( 
	  CopyVectF(&(lplot->Wrect),WRect,4L) &&
	  CopyVectF(&(lplot->Frect),FRect,4L) &&
	  CopyVectF(&(lplot->Frect_kp),FRect,4L) 
	  ) 
	{
	  store_record(Xgc,CODEEch, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storeEch): No more place \n");
}


static void replay_Ech(BCG *Xgc,void *theplot)
{
  struct rec_scale *plch = theplot;
  setscale2d(Xgc,plch->Wrect,plch->Frect,plch->logflag);
}


static void clean_Ech(void *plot)
{
  struct rec_scale *theplot = plot ;
  FREE(theplot->Wrect);
  FREE(theplot->Frect);     
  FREE(theplot->Frect_kp);     
}


/*---------------------------------------------------------------------
 * xsetech (new)
 *---------------------------------------------------------------------------*/

void store_NEch(BCG *Xgc,char *flag, double *WRect, double *ARect, double *FRect, char *logflag)
{
  struct rec_nscale *lplot;
  lplot= ((struct rec_nscale *) MALLOC(sizeof(struct rec_nscale)));
  if (lplot != NULL)
    {
      lplot->logflag[0]=logflag[0];
      lplot->logflag[1]=logflag[1];
      if ( 
	  CopyVectC(&(lplot->flag),flag,((int)strlen(flag))+1) &&
	  CopyVectF(&(lplot->Wrect),WRect,4L) &&
	  CopyVectF(&(lplot->Frect),FRect,4L) &&
	  CopyVectF(&(lplot->Arect),ARect,4L) &&
	  CopyVectF(&(lplot->Frect_kp),FRect,4L) 
	  ) 
	{
	  store_record(Xgc,CODENEch, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storeEch): No more place \n");
}


static void replay_NEch(BCG *Xgc,void *theplot)
{
  struct rec_nscale *plch = theplot;
  set_scale(Xgc,plch->flag,plch->Wrect,plch->Frect,NULL,plch->logflag,plch->Arect);
}


static void clean_NEch(void *plot)
{
  struct rec_nscale *theplot = plot;
  FREE(theplot->flag);    
  FREE(theplot->Wrect);
  FREE(theplot->Frect);     
  FREE(theplot->Arect);     
  FREE(theplot->Frect_kp);     
}
  

/*---------------------------------------------------------------------
 * 2D plots 
 *---------------------------------------------------------------------------*/

static void store_Plot_G(BCG *Xgc,int code, char *xf, double *x, double *y, int *n1, int *n2, int *style,
			 char *strflag,const  char *legend,int legend_pos, double *brect, int *aint)
{
  int nstyle,n1n2;
  struct rec_plot2d *lplot;
  lplot= ((struct rec_plot2d *) MALLOC(sizeof(struct rec_plot2d)));
  if ( *n1==1) nstyle= *n1+1;else nstyle= *n1;
  if (lplot != NULL)
    {
      int n=0;
      switch (xf[0])
	{
	case 'g': n=(*n1)*(*n2);break;
	case 'e': n=0;break;
	case 'o': n=(*n2);break;
	}
      lplot->n1= *n1;
      lplot->n2= *n2;
      /* to be sure that lplot is corectly initialized */
      lplot->x = lplot->y = NULL;
      lplot->legend_pos = legend_pos;
      n1n2=(*n1)*(*n2);
      if ( 
	  CopyVectC(&(lplot->xf),xf,((int)strlen(xf))+1) &&
	  ((n == 0) ? 1 : CopyVectF(&(lplot->x),x,n)) &&
	  ((n1n2==0)? 1 : CopyVectF(&(lplot->y),y,n1n2)) &&
	  CopyVectLI(&(lplot->style),style,nstyle) &&
	  CopyVectC(&(lplot->strflag),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->strflag_kp),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->legend),legend,((int)strlen(legend))+1) && 
	  CopyVectF(&(lplot->brect),brect,4L) &&
	  CopyVectF(&(lplot->brect_kp),brect,4L) &&
	  CopyVectLI(&(lplot->aint),aint,4) &&
	  CopyVectLI(&(lplot->aint_kp),aint,4) 
	  ) 
	{
	  store_record(Xgc,code, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storeplot): No more place \n");
}

void store_Plot(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		const char *legend,int legend_pos, double *brect, int *aint)
{
  store_Plot_G(Xgc,CODEPlot,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aint);
}


void store_Plot1(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		 const char *legend,int legend_pos, double *brect, int *aint)
{
  store_Plot_G(Xgc,CODEPlot1,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aint);
}


void store_Plot2(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		 const char *legend,int legend_pos, double *brect, int *aint)
{
  store_Plot_G(Xgc,CODEPlot2,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aint);
}


void store_Plot3(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		 const char *legend,int legend_pos, double *brect, int *aint)
{
  store_Plot_G(Xgc,CODEPlot3,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aint);
}

void store_Plot4(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		 const char *legend,int legend_pos, double *brect, int *aint)
{
  store_Plot_G(Xgc,CODEPlot4,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aint);
}


static void replay_Plot(BCG *Xgc,void * theplot)
{
  struct rec_plot2d *p = theplot;
  nsp_plot2d_1(Xgc,p->xf,p->x,p->y,&(p->n1),&(p->n2),p->style,p->strflag,p->legend,p->legend_pos, p->brect,p->aint);
}

static void replay_Plot1(BCG *Xgc,void * theplot)
{
  struct rec_plot2d *p = theplot;
  nsp_plot2d_1(Xgc,p->xf,p->x,p->y,&(p->n1),&(p->n2),p->style,p->strflag,p->legend,p->legend_pos, p->brect,p->aint);
}



static void replay_Plot2(BCG *Xgc,void * theplot)
{
  struct rec_plot2d *p = theplot;
  nsp_plot2d_2(Xgc,p->xf,p->x,p->y,&(p->n1),&(p->n2),p->style,p->strflag,p->legend,p->legend_pos, p->brect,p->aint);
}



static void replay_Plot3(BCG *Xgc,void * theplot)
{
  struct rec_plot2d *p = theplot;
  nsp_plot2d_3(Xgc,p->xf,p->x,p->y,&(p->n1),&(p->n2),p->style,p->strflag,p->legend,p->legend_pos, p->brect,p->aint);
}



static void replay_Plot4(BCG *Xgc,void * theplot)
{
  struct rec_plot2d *p = theplot;
  nsp_plot2d_4(Xgc,p->xf,p->x,p->y,&(p->n1),&(p->n2),p->style,p->strflag,p->legend,p->legend_pos, p->brect,p->aint);
}


static void clean_Plot(void *plot)
{
  struct rec_plot2d *theplot = plot;
  if ( theplot->xf[0] != 'e') FREE(theplot->x);
  FREE(theplot->xf);FREE(theplot->y);     
  FREE(theplot->style);FREE(theplot->strflag);
  FREE(theplot->legend);FREE(theplot->brect);FREE(theplot->aint);   
  FREE(theplot->strflag_kp);
  FREE(theplot->brect_kp);FREE(theplot->aint_kp);   
}



/*---------------------------------------------------------------------
 * axis  
 *---------------------------------------------------------------------------*/

void store_SciAxis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, 
		   char **str, int subtics, char *format, int fontsize, int textcolor, 
		   int ticscolor, char logflag, int seg_flag)
{
  struct rec_sciaxis *lplot = ((struct rec_sciaxis *) MALLOC(sizeof(struct rec_sciaxis)));
  if (lplot == NULL)
    {
      Scistring("\nRunning out of memory in store_ plots\n");
      return ;
    }
  lplot->pos= pos;
  lplot->xy_type = xy_type;
  lplot->nx = *nx;
  lplot->ny = *ny;
  lplot->subtics = subtics ;
  lplot->fontsize = fontsize;
  lplot->textcolor = textcolor;
  lplot->ticscolor = ticscolor;
  lplot->logflag = logflag ;
  lplot->seg_flag = seg_flag ;
  lplot->f_l = ((format == NULL) ? 0: 1);
  lplot->format = NULL;
  if ( 
      ((format == NULL) ? 1 : CopyVectC(&(lplot->format),format,((int)strlen(format))+1)) &&
      CopyVectF(&(lplot->x),x,*nx) &&
      CopyVectF(&(lplot->y),y,*ny) &&
      CopyVectS(&(lplot->str),str))
    {
      store_record(Xgc,CODESciAxis, lplot);
    }
}
  

static void replay_SciAxis(BCG *Xgc,void *theplot)
{
  struct rec_sciaxis *p = (struct rec_sciaxis *) theplot;
  sci_axis(Xgc,p->pos,p->xy_type,p->x,&p->nx,p->y,&p->ny,p->str,p->subtics,p->format,p->fontsize,p->textcolor,
	   p->ticscolor,p->logflag,p->seg_flag);

}


static void clean_SciAxis(void *plot)
{
  int count = 0;
  struct rec_sciaxis *theplot = (struct rec_sciaxis *) plot;
  if (theplot->f_l == 1) FREE(theplot->format);
  FREE(theplot->x);
  FREE(theplot->y);
  if ( theplot->str != 0)
    { 
      while ( theplot->str[count] != 0) { FREE(theplot->str[count]); count++;} 
      FREE(theplot->str);
    }
}


/*---------------------------------------------------------------------
 * xgrid 
 *---------------------------------------------------------------------------*/

void store_Grid(BCG *Xgc,int *style)
{
  struct rec_xgrid *lplot;
  lplot= ((struct rec_xgrid *) MALLOC(sizeof(struct rec_xgrid)));
  if (lplot != NULL)
    {
      lplot->style = *style;
      store_record(Xgc,CODEGrid, lplot);
      return;
    }
  Scistring("\n store_ (storegrid): No more place \n");
}


static void replay_Grid(BCG *Xgc,void *theplot)
{
  struct rec_xgrid *plch;
  plch= (struct rec_xgrid *)theplot;
  nsp_plot_grid(Xgc,&(plch->style));
}


static void clean_Grid(void *plot)
{
  struct rec_xgrid *theplot;
  theplot=(struct rec_xgrid *) plot;
}


/*---------------------------------------------------------------------
 * param3d 
 *---------------------------------------------------------------------------*/

void store_Param3D(BCG *Xgc,double *x, double *y, double *z, int *n, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  struct rec_param3d *lplot;
  lplot= ((struct rec_param3d *) MALLOC(sizeof(struct rec_param3d)));
  if (lplot != NULL)
    {
      lplot->n= *n;
      lplot->teta= *teta;
      lplot->alpha= *alpha;
      if ( 
	  CopyVectF(&(lplot->x), x,*n) &&
	  CopyVectF(&(lplot->y), y,*n) &&
	  CopyVectF(&(lplot->z), z,*n) &&
	  CopyVectC(&(lplot->legend), legend, ((int)strlen(legend))+1) && 
	  CopyVectLI(&(lplot->flag), flag,3) &&
	  CopyVectF(&(lplot->bbox), bbox,6L)
	  ) 
	{
	  store_record(Xgc,CODEParam3D, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storeparam3d): No more place \n");
}


static void replay_Param3D(BCG *Xgc,void *theplot)
{
  struct rec_param3d *pl3d;
  pl3d= (struct rec_param3d *)theplot;
  nsp_param3d(Xgc,pl3d->x,pl3d->y,pl3d->z,&pl3d->n,&pl3d->teta,
	      &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}


void store_Param3D1(BCG *Xgc, double *x, double *y, double *z, int *m, int *n, int *iflag, int *colors, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  struct rec_param3d1 *lplot;
  lplot= ((struct rec_param3d1 *) MALLOC(sizeof(struct rec_param3d1)));
  if (lplot != NULL)
    {
      lplot->n= *n;
      lplot->m= *m;
      lplot->iflag= *iflag;
      lplot->teta= *teta;
      lplot->alpha= *alpha;
      if ( 
	  CopyVectF(&(lplot->x), x,*m*(*n)) &&
	  CopyVectF(&(lplot->y), y,*m*(*n)) &&
	  CopyVectF(&(lplot->z), z,*m*(*n)) &&
	  MaybeCopyVect3dPLI(*iflag,&(lplot->colors), colors, *n) &&
	  CopyVectC(&(lplot->legend), legend, ((int)strlen(legend))+1) && 
	  CopyVectLI(&(lplot->flag), flag,3) &&
	  CopyVectF(&(lplot->bbox), bbox,6L)
	  ) 
	{
	  store_record(Xgc,CODEParam3D1, lplot);
	  return;
	}
    }
  Scistring("\n store_ Plot (storeparam3d): No more place \n");
}


static void replay_Param3D1(BCG *Xgc,void *theplot)
{
  struct rec_param3d1 *pl3d;
  pl3d= (struct rec_param3d1 *)theplot;
  nsp_param3d_1(Xgc,pl3d->x,pl3d->y,pl3d->z,&pl3d->m,&pl3d->n,&pl3d->iflag,
		pl3d->colors, &pl3d->teta,
		&pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}



static void clean_Param3D(void *plot)
{
  struct rec_param3d *theplot;
  theplot=(struct rec_param3d *) plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);FREE(theplot->legend);FREE(theplot->flag);
  FREE(theplot->bbox);
}

static void clean_Param3D1(void *plot)
{
  struct rec_param3d1 *theplot;
  theplot=(struct rec_param3d1 *) plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);FREE(theplot->legend);FREE(theplot->flag);
  if ( theplot->iflag == 1 ) 
    FREE(theplot->colors);
  FREE(theplot->bbox);
}


/*---------------------------------------------------------------------
 * plot3d 
 *---------------------------------------------------------------------------*/

static void store_Plot3D_G(BCG *Xgc,int code, double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  struct rec_plot3d *lplot;
  lplot= ((struct rec_plot3d *) MALLOC(sizeof(struct rec_plot3d)));
  if (lplot != NULL)
    {
      lplot->p= *p;
      lplot->q= *q;
      lplot->teta= *teta;
      lplot->alpha= *alpha;
      if ( 
	  CopyVectF(&(lplot->x), x,*p) &&
	  CopyVectF(&(lplot->y), y,*q) &&
	  CopyVectF(&(lplot->z), z,(*p)*(*q)) &&
	  CopyVectC(&(lplot->legend), legend, ((int)strlen(legend))+1) && 
	  CopyVectLI(&(lplot->flag), flag,3) &&
	  CopyVectF(&(lplot->bbox), bbox,6L)
	  ) 
	{
	  store_record(Xgc,code, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storeplot3d): No more place \n");
}

void store_Plot3D(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  store_Plot3D_G(Xgc,CODEPlot3D,x,y,z,p,q,teta,alpha,legend,flag,bbox);

}


static void replay_3D(BCG *Xgc,void *theplot)
{
  struct rec_plot3d *pl3d = (struct rec_plot3d *)theplot;
  nsp_plot3d(Xgc,pl3d->x,pl3d->y,pl3d->z,&pl3d->p,&pl3d->q,&pl3d->teta,
	     &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}

static void clean_3D(void *plot)
{
  struct rec_plot3d *theplot = plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);FREE(theplot->legend);FREE(theplot->flag);
  FREE(theplot->bbox);
}

void store_Plot3D1(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  store_Plot3D_G(Xgc,CODEPlot3D1,x,y,z,p,q,teta,alpha,legend,flag,bbox);
}


static void replay_3D1(BCG *Xgc,void *theplot)
{
  struct rec_plot3d *pl3d = (struct rec_plot3d *)theplot;
  nsp_plot3d_1(Xgc,pl3d->x,pl3d->y,pl3d->z,&pl3d->p,&pl3d->q,&pl3d->teta,
	       &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}

static void clean_3D1(void *plot)
{
  clean_3D(plot);
}



/*---------------------------------------------------------------------
 * 3dobjs 
 *---------------------------------------------------------------------------*/

void store_3dobj(BCG *Xgc,void *Obj,double *teta, double *alpha,const char *legend, int *flag, double *bbox,
		 int with_mesh,int with_box,int box_color,int box_style)
{
  NspList *L=Obj;
  struct rec_3dobj *lplot;
  lplot= ((struct rec_3dobj *) MALLOC(sizeof(struct rec_plot3d)));
  if (lplot != NULL)
    {
      lplot->teta= *teta;
      lplot->alpha= *alpha;
      lplot->with_mesh = with_mesh;
      lplot->with_box = with_box;
      lplot->box_color=box_color;
      lplot->box_style=box_style;
      lplot->L = (NspList *) nsp_object_copy(NSP_OBJECT(L));
      if ( 
	  ( lplot->L != NULL) && 
	  CopyVectC(&(lplot->legend), legend, ((int)strlen(legend))+1) && 
	  CopyVectLI(&(lplot->flag), flag,3) &&
	  CopyVectF(&(lplot->bbox), bbox,6L)
	  ) 
	{
	  store_record(Xgc,CODE3dobj, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storeplot3d): No more place \n");
}


static void replay_3dobj(BCG *Xgc,void *theplot)
{
  struct rec_3dobj *pl3d = (struct rec_3dobj *)theplot;
  nsp_draw_3d_obj(Xgc,pl3d->L,&pl3d->teta, &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox,
		  pl3d->with_mesh,pl3d->with_box,pl3d->box_color,pl3d->box_style);
}


static void clean_3dobj(void *plot)
{
  struct rec_3dobj *theplot = plot;
  FREE(theplot->legend);FREE(theplot->flag); FREE(theplot->bbox);
  nsp_list_destroy(theplot->L);

}

/*---------------------------------------------------------------------
 * pixbuf 
 *---------------------------------------------------------------------------*/

void store_pixbuf(BCG *Xgc,void *pix,double x, double y,double w,double h,int src_x,int src_y)
{
  NspObject *nsp_pix=pix;
  struct rec_pixbuf *lplot;
  lplot= ((struct rec_pixbuf *) MALLOC(sizeof(struct rec_pixbuf)));
  if (lplot != NULL)
    {
      lplot->x=x;
      lplot->y=y;
      lplot->w=w;
      lplot->h=h;
      lplot->src_x=src_x;
      lplot->src_y=src_y;
      lplot->nsp_pixbuf = nsp_object_copy(nsp_pix);
      if ( lplot->nsp_pixbuf  != NULL)
	{
	  store_record(Xgc,CODEpixbuf, lplot);
	  return;
	}
    }
  Scistring("\n store_ Plot (storeplot3d): No more place \n");
}

static void replay_pixbuf(BCG *Xgc,void *theplot)
{
  struct rec_pixbuf *plot = (struct rec_pixbuf *) theplot;
  Xgc->graphic_engine->scale->draw_pixbuf(Xgc,
					  plot->nsp_pixbuf, plot->x, plot->y,
					  plot->w, plot->h,plot->src_x,plot->src_y);
}


static void clean_pixbuf(void *plot)
{
  struct rec_pixbuf *theplot = plot;
  nsp_object_destroy(&theplot->nsp_pixbuf);
}


void store_pixbuf_from_file(BCG *Xgc,const char *fname,double x, double y,double w,double h,int src_x,int src_y)
{
  struct rec_pixbuf_file *lplot;
  lplot= ((struct rec_pixbuf_file *) MALLOC(sizeof(struct rec_pixbuf_file)));
  if (lplot != NULL)
    {
      lplot->x=x;
      lplot->y=y;
      lplot->w=w;
      lplot->h=h;
      lplot->src_x=src_x;
      lplot->src_y=src_y;
      if ( 
	  CopyVectC(&(lplot->pixbuf_file), fname, ((int)strlen(fname))+1)
	  )
	{
	  store_record(Xgc,CODEpixbuf_file, lplot);
	  return;
	}
    }
  Scistring("\n store_ Plot (storeplot3d): No more place \n");
}

static void replay_pixbuf_from_file(BCG *Xgc,void *theplot)
{
  struct rec_pixbuf_file *plot = (struct rec_pixbuf_file *) theplot;
  Xgc->graphic_engine->scale->draw_pixbuf_from_file(Xgc,
						    plot->pixbuf_file, plot->x, plot->y,
						    plot->w, plot->h,plot->src_x,plot->src_y);
}


static void clean_pixbuf_from_file(void *plot)
{
  struct rec_pixbuf_file *theplot = plot;
  FREE(theplot->pixbuf_file);
}


/*---------------------------------------------------------------------
 * fac3d 
 * added code by polpoth 4/5/2000 
 *---------------------------------------------------------------------------*/

static void store_Fac3D_G(BCG *Xgc,int code, double *x, double *y, double *z, int *cvect, int *p, int *q, 
			  double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  int rep ; 
  struct rec_fac3d *lplot;
  lplot= ((struct rec_fac3d *) MALLOC(sizeof(struct rec_fac3d)));
  if (lplot != NULL)
    {
      lplot->p= *p;
      lplot->q= *q;
      lplot->teta= *teta;
      lplot->alpha= *alpha;
      rep =  CopyVectF(&(lplot->x), x,(*p)*(*q)) 
	&& CopyVectF(&(lplot->y), y,(*p)*(*q)) 
	&& CopyVectF(&(lplot->z), z,(*p)*(*q)) 
	&& CopyVectC(&(lplot->legend), legend, ((int)strlen(legend))+1)
	&& CopyVectLI(&(lplot->flag), flag,3) 
	&& CopyVectF(&(lplot->bbox), bbox,6L) ; 
      if ( code == CODEFac3D3) rep = rep&& CopyVectLI(&(lplot->cvect),cvect,(*p)*(*q)); 
      else if ( code == CODEFac3D2 )  rep = rep&& CopyVectLI(&(lplot->cvect),cvect,(*q)); 
      else lplot->cvect = NULL;
      if (rep )
	{
	  store_record(Xgc,code, lplot);
	  return;
	}
    }
  Scistring("\n store_ Plot (storefac3d): No more place \n");
}

void store_Fac3D(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  store_Fac3D_G(Xgc,CODEFac3D,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
}


void store_Fac3D1(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  store_Fac3D_G(Xgc,CODEFac3D1,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
}


void store_Fac3D2(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox)
{
  store_Fac3D_G(Xgc,CODEFac3D2,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
}


void store_Fac3D3(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const
		  char *legend, int *flag, double *bbox)
{
  store_Fac3D_G(Xgc,CODEFac3D3,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
}

static void replay_Fac3D(BCG *Xgc,void *theplot)
{
  struct rec_fac3d *pl3d= (struct rec_fac3d *)theplot;
  nsp_plot_fac3d(Xgc,pl3d->x,pl3d->y,pl3d->z,pl3d->cvect,&pl3d->p,&pl3d->q,&pl3d->teta,
		 &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}

static void replay_Fac3D1(BCG *Xgc,void *theplot)
{
  struct rec_fac3d *pl3d = (struct rec_fac3d *)theplot;
  nsp_plot_fac3d_1(Xgc,pl3d->x,pl3d->y,pl3d->z,pl3d->cvect,
		   &pl3d->p,&pl3d->q,&pl3d->teta,
		   &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}

static void replay_Fac3D2(BCG *Xgc,void *theplot)
{
  struct rec_fac3d *pl3d = (struct rec_fac3d *)theplot;
  nsp_plot_fac3d_2(Xgc,pl3d->x,pl3d->y,pl3d->z,pl3d->cvect,
		   &pl3d->p,&pl3d->q,&pl3d->teta,
		   &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}


static void replay_Fac3D3(BCG *Xgc,void *theplot)
{
  struct rec_fac3d *pl3d =  (struct rec_fac3d *)theplot;
  nsp_plot_fac3d_3(Xgc,pl3d->x,pl3d->y,pl3d->z,pl3d->cvect,
		   &pl3d->p,&pl3d->q,&pl3d->teta,
		   &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox);
}



static void clean_Fac3D(void *plot)
{
  struct rec_fac3d *theplot =  plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);FREE(theplot->legend);FREE(theplot->flag);
  FREE(theplot->bbox);
}


/*---------------------------------------------------------------------
 *fec 
 *---------------------------------------------------------------------------*/

void store_Fec(BCG *Xgc, double *x, double *y, double *triangles, double *func, int *Nnode, int *Ntr, char *strflag,
	       const char *legend, double *brect, int *aaint,const double *zminmax,const int *colminmax, 
	       const int *colout,int draw)
{
  struct rec_fec *lplot;
  lplot= ((struct rec_fec *) MALLOC(sizeof(struct rec_fec)));
  if (lplot != NULL)
    {
      lplot->Nnode= *Nnode;
      lplot->Ntr= *Ntr;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      lplot->colout = NULL;
      lplot->draw = draw;
      if ( 
	  CopyVectF(&(lplot->x), x,*Nnode) &&
	  CopyVectF(&(lplot->y), y,*Nnode) &&
	  CopyVectF(&(lplot->triangles), triangles,(*Ntr)*5) &&
	  CopyVectF(&(lplot->func), func,*Nnode ) && 
	  CopyVectF(&(lplot->brect), brect,4L) &&
	  ((colminmax != NULL) ? CopyVectLI(&(lplot->colminmax),colminmax,2L) : TRUE)&&
	  ((zminmax != NULL) ? CopyVectF(&(lplot->zminmax),zminmax,2L) : TRUE)&&
	  ((colout != NULL) ? CopyVectLI(&(lplot->colout),colout,2L) : TRUE)&&
	  CopyVectF(&(lplot->brect_kp), brect,4L) &&
	  CopyVectLI(&(lplot->aaint), aaint,4) &&
	  CopyVectLI(&(lplot->aaint_kp), aaint,4) &&
	  CopyVectC(&(lplot->strflag),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->strflag_kp),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->legend),legend,((int)strlen(legend))+1)  
	  ) 
	{
	  store_record(Xgc,CODEFecN, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storefec): No more place \n");
}

static void replay_Fec(BCG *Xgc,void *theplot)
{
  struct rec_fec *plfec = (struct rec_fec *)theplot;
  nsp_fec(Xgc,plfec->x,plfec->y,plfec->triangles,plfec->func,
	  &plfec->Nnode,&plfec->Ntr,
	  plfec->strflag,plfec->legend,plfec->brect,plfec->aaint,
	  plfec->zminmax, plfec->colminmax, plfec->colout,plfec->draw );
}


static void clean_Fec(void *plot)
{
  struct rec_fec *theplot = plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->triangles);  FREE(theplot->func);
  FREE(theplot->legend);
  FREE(theplot->strflag);  FREE(theplot->strflag_kp);
  FREE(theplot->brect);   FREE(theplot->brect_kp); 
  FREE(theplot->aaint);  FREE(theplot->aaint_kp);
  FREE(theplot->zminmax); FREE(theplot->colminmax);  /* added by bruno */
  FREE(theplot->colout);
}


/*---------------------------------------------------------------------
 * contour 
 *---------------------------------------------------------------------------*/

void store_Contour(BCG *Xgc, double *x, double *y, double *z, int *n1, int *n2, int *flagnz, int *nz, double *zz, double *teta, double *alpha, char *legend, int *flag, double *bbox, double *zlev)
{
  struct rec_contour *lplot;
  lplot= ((struct rec_contour *) MALLOC(sizeof(struct rec_contour)));
  if (lplot != NULL)
    { int res=1;
    lplot->n1= *n1;
    lplot->n2= *n2;
    lplot->nz= *nz;
    lplot->flagnz= *flagnz;
    if (*flagnz != 0)
      res= CopyVectF(&(lplot->zz), zz,*nz);
    else
      lplot->zz= (double *) 0;
    lplot->teta= *teta;
    lplot->alpha= *alpha;
    lplot->zlev= *zlev;
    if ( 
	res &&
	CopyVectF(&(lplot->x), x,*n1) &&
	CopyVectF(&(lplot->y), y,*n2) &&
	CopyVectF(&(lplot->z), z,(*n1)*(*n2)) &&
	CopyVectC(&(lplot->legend), legend, ((int)strlen(legend))+1) && 
	CopyVectLI(&(lplot->flag), flag,3) &&
	CopyVectF(&(lplot->bbox), bbox,6L)
	) 
      {
	store_record(Xgc,CODEContour, lplot);
	return;}
    }
  Scistring("\n store_ Plot (storecontour): No more place \n");
}

static void replay_Contour(BCG *Xgc,void *theplot)
{
  struct rec_contour *pl3d;
  pl3d= (struct rec_contour *)theplot;
  nsp_contour(Xgc,pl3d->x,pl3d->y,pl3d->z,&pl3d->n1,&pl3d->n2,&pl3d->flagnz,&pl3d->nz,
	      pl3d->zz,&pl3d->teta,
	      &pl3d->alpha,pl3d->legend,pl3d->flag,pl3d->bbox,&pl3d->zlev,0L);
}


static void clean_Contour(void *plot)
{
  struct rec_contour *theplot = plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);FREE(theplot->zz);FREE(theplot->legend);FREE(theplot->flag);
  FREE(theplot->bbox);
}


void store_Contour2D(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2, int *flagnz, int *nz, double *zz, int *style, char *strflag, char *legend, double *brect, int *aint)
{
  struct rec_contour2d *lplot;
  int nstyle;
  if ( *n1==1) nstyle= *n1+1;else nstyle= *n1;
  lplot= ((struct rec_contour2d *) MALLOC(sizeof(struct rec_contour2d)));
  if (lplot != NULL)
    { int res=1;
    lplot->n1= *n1;
    lplot->n2= *n2;
    lplot->nz= *nz;
    lplot->flagnz= *flagnz;
    if (*flagnz != 0)
      res= CopyVectF(&(lplot->zz),zz,*nz);
    else
      lplot->zz= (double *) 0;
    if ( 
	res &&
	CopyVectF(&(lplot->x), x,*n1) &&
	CopyVectF(&(lplot->y), y,*n2) &&
	CopyVectF(&(lplot->z), z,(*n1)*(*n2)) &&
	CopyVectLI(&(lplot->style),style,nstyle) &&
	CopyVectC(&(lplot->strflag),strflag,((int)strlen(strflag))+1) &&
	CopyVectC(&(lplot->strflag_kp),strflag,((int)strlen(strflag))+1) &&
	CopyVectC(&(lplot->legend),legend,((int)strlen(legend))+1) && 
	CopyVectF(&(lplot->brect),brect,4L) &&
	CopyVectF(&(lplot->brect_kp),brect,4L) &&
	CopyVectLI(&(lplot->aint),aint,4) &&
	CopyVectLI(&(lplot->aint_kp),aint,4) 
	) 
      {
	store_record(Xgc,CODEContour2D, lplot);
	return;}
    }
  Scistring("\n store_ Plot (storecontour): No more place \n");
}

static void replay_Contour2D(BCG *Xgc,void *theplot)
{
  struct rec_contour2d *pl3d;
  pl3d= (struct rec_contour2d *)theplot;
  nsp_contour2(Xgc,pl3d->x,pl3d->y,pl3d->z,&pl3d->n1,&pl3d->n2,&pl3d->flagnz,&pl3d->nz,
	       pl3d->zz, pl3d->style,pl3d->strflag,pl3d->legend,
	       pl3d->brect,pl3d->aint);
}


static void clean_Contour2D(void *plot)
{
  struct rec_contour2d *theplot = plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);FREE(theplot->zz);
  FREE(theplot->style);FREE(theplot->strflag);
  FREE(theplot->legend);FREE(theplot->brect);FREE(theplot->aint);   
  FREE(theplot->strflag_kp);
  FREE(theplot->brect_kp);FREE(theplot->aint_kp);   
}

/*---------------------------------------------------------------------
 * grayplots Matplot 
 *---------------------------------------------------------------------------*/

void store_Gray(BCG *Xgc,double *x, double *y, double *z, int nx, int ny, char *strflag,
		double *brect, int *aaint,int remap,const int *colminmax,const double *zminmax,
		const int *colout,int shade)
{
  struct rec_gray *lplot;
  lplot= ((struct rec_gray *) MALLOC(sizeof(struct rec_gray)));
  if (lplot != NULL)
    {
      lplot->n1= nx;
      lplot->n2= ny;
      lplot->remap = remap;
      lplot->shade = shade;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      lplot->colout = NULL;
      if ( 
	  CopyVectF(&(lplot->x), x,nx) &&
	  CopyVectF(&(lplot->y), y,ny) &&
	  CopyVectF(&(lplot->z), z,(nx)*(ny)) &&
	  CopyVectC(&(lplot->strflag),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->strflag_kp),strflag,((int)strlen(strflag))+1) &&
	  CopyVectF(&(lplot->brect),brect,4L) &&
	  CopyVectF(&(lplot->brect_kp),brect,4L) &&
	  CopyVectLI(&(lplot->aaint),aaint,4)  &&
	  CopyVectLI(&(lplot->aaint_kp),aaint,4) &&
	  ((colminmax != NULL) ? CopyVectLI(&(lplot->colminmax),colminmax,2L) : TRUE)&&
	  ((zminmax != NULL) ? CopyVectF(&(lplot->zminmax),zminmax,2L) : TRUE) &&
	  ((colout != NULL) ? CopyVectLI(&(lplot->colout),colout,2L) : TRUE)
	  ) 
	{
	  store_record(Xgc,CODEGray, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storegray): No more place \n");
}

/** For matrices  z(i,j) **/

void store_Gray1(BCG *Xgc,double *z, int nr, int nc, char *strflag, double *brect, int *aaint,
		 int remap,const int *colminmax,const double *zminmax)
{
  struct rec_gray1 *lplot;
  lplot= ((struct rec_gray1 *) MALLOC(sizeof(struct rec_gray1)));
  if (lplot != NULL)
    {
      lplot->n1= nr;
      lplot->n2= nc;
      lplot->x = NULL;
      lplot->y = NULL;
      lplot->remap = remap;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      if ( 
	  CopyVectF(&(lplot->z), z,nr*nc) &&
	  CopyVectC(&(lplot->strflag),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->strflag_kp),strflag,((int)strlen(strflag))+1) &&
	  CopyVectF(&(lplot->brect),brect,4L) &&
	  CopyVectF(&(lplot->brect_kp),brect,4L) &&
	  CopyVectLI(&(lplot->aaint),aaint,4)  &&
	  CopyVectLI(&(lplot->aaint_kp),aaint,4) &&
	  ((colminmax != NULL) ? CopyVectLI(&(lplot->colminmax),colminmax,2L) : TRUE)&&
	  ((zminmax != NULL) ? CopyVectF(&(lplot->zminmax),zminmax,2L) : TRUE)
	  ) 
	{
	  store_record(Xgc,CODEGray1, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storegray): No more place \n");
}



void store_Gray2(BCG *Xgc,double *z, int nr, int nc, double *xrect,
		 int remap,const int *colminmax,const double *zminmax)
{
  struct rec_gray_2 *lplot;
  lplot= ((struct rec_gray_2 *) MALLOC(sizeof(struct rec_gray_2)));
  if (lplot != NULL)
    {
      lplot->n1= nr;
      lplot->n2= nc;
      lplot->remap = remap;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      if ( 
	  CopyVectF(&(lplot->z), z,nr*nc) &&
	  CopyVectF(&(lplot->xrect),xrect,4L) &&
	  ((colminmax != NULL) ? CopyVectLI(&(lplot->colminmax),colminmax,2L) : TRUE)&&
	  ((zminmax != NULL) ? CopyVectF(&(lplot->zminmax),zminmax,2L) : TRUE)
	  ) 
	{
	  store_record(Xgc,CODEGray2, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storegray): No more place \n");
}



static void replay_Gray(BCG *Xgc,void *theplot)
{
  struct rec_gray *pl3d;
  pl3d= (struct rec_gray *)theplot;
  nsp_draw_matrix(Xgc,pl3d->x,pl3d->y,pl3d->z,pl3d->n1,pl3d->n2,
		  pl3d->strflag,pl3d->brect,pl3d->aaint,
		  pl3d->remap,pl3d->colminmax,pl3d->zminmax,pl3d->colout,pl3d->shade);
}

static void replay_Gray1(BCG *Xgc,void *theplot)
{
  struct rec_gray1 *pl3d;
  pl3d= (struct rec_gray1 *)theplot;
  nsp_draw_matrix_1(Xgc,pl3d->z,pl3d->n1,pl3d->n2,
		    pl3d->strflag,pl3d->brect,pl3d->aaint,
		    pl3d->remap,pl3d->colminmax,pl3d->zminmax);
}

static void replay_Gray2(BCG *Xgc,void *theplot)
{
  struct rec_gray_2 *pl3d;
  pl3d= (struct rec_gray_2 *)theplot;
  nsp_draw_matrix_2(Xgc,pl3d->z,pl3d->n1,pl3d->n2, pl3d->xrect,
		    pl3d->remap,pl3d->colminmax,pl3d->zminmax);
}

/* same for Gray1 and Gray2 */

static void clean_Gray(void *plot)
{
  struct rec_gray *theplot;
  theplot=(struct rec_gray *) plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);
  FREE(theplot->strflag);
  FREE(theplot->brect);
  FREE(theplot->aaint);   
  FREE(theplot->strflag_kp);
  FREE(theplot->brect_kp);
  FREE(theplot->aaint_kp);   
}

static void clean_Gray1(void *plot)
{
  struct rec_gray1 *theplot;
  theplot=(struct rec_gray1 *) plot;
  FREE(theplot->x);FREE(theplot->y);
  FREE(theplot->z);
  FREE(theplot->strflag);
  FREE(theplot->brect);
  FREE(theplot->aaint);   
  FREE(theplot->strflag_kp);
  FREE(theplot->brect_kp);
  FREE(theplot->aaint_kp);   
  FREE(theplot->colminmax);   
  FREE(theplot->zminmax);   
}


static void clean_Gray2(void *plot)
{
  struct rec_gray_2 *theplot;
  theplot=(struct rec_gray_2 *) plot;
  FREE(theplot->z);
  FREE(theplot->xrect);
  FREE(theplot->colminmax);   
  FREE(theplot->zminmax);   
}


/*---------------------------------------------------------------------
 * champ champ1 
 *---------------------------------------------------------------------------*/

static void store_Champ_G(BCG *Xgc,int code, double *x, double *y, double *fx, double *fy, 
			  int *n1, int *n2, char *strflag, double *vrect, double *arfact)
{
  struct rec_champ *lplot;
  lplot= ((struct rec_champ *) MALLOC(sizeof(struct rec_champ)));
  if (lplot != NULL)
    {
      lplot->n1= *n1;
      lplot->n2= *n2;
      lplot->arfact= *arfact;
      if ( 
	  CopyVectF(&(lplot->x),x,(*n1)) &&
	  CopyVectF(&(lplot->y),y,(*n2)) &&
	  CopyVectF(&(lplot->fx),fx,(*n1)*(*n2)) &&
	  CopyVectF(&(lplot->fy),fy,(*n1)*(*n2)) &&
	  CopyVectC(&(lplot->strflag),strflag,((int)strlen(strflag))+1) &&
	  CopyVectC(&(lplot->strflag_kp),strflag,((int)strlen(strflag))+1) &&
	  CopyVectF(&(lplot->vrect),vrect,4L) &&
	  CopyVectF(&(lplot->vrect_kp),vrect,4L)
	  ) 
	{
	  store_record(Xgc,code, lplot);
	  return;}
    }
  Scistring("\n store_ Plot (storechamp): No more place \n");
}

void store_Champ(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, double *vrect, double *arfact)
{
  store_Champ_G(Xgc,CODEChamp, x,y, fx,fy,n1,n2,strflag, vrect,arfact);
}

void store_Champ1(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, double *vrect, double *arfact)
{
  store_Champ_G(Xgc,CODEChamp1, x,y, fx,fy,n1,n2,strflag, vrect,arfact);
}


static void replay_Champ(BCG *Xgc,void *theplot)
{
  struct rec_champ *plch = (struct rec_champ *)theplot;
  nsp_champ(Xgc,plch->x,plch->y,plch->fx,plch->fy,&(plch->n1),&(plch->n2),
	    plch->strflag,plch->vrect,&(plch->arfact),0L);
}

static void replay_Champ1(BCG *Xgc,void *theplot)
{
  struct rec_champ *plch= (struct rec_champ *)theplot;
  nsp_champ1(Xgc,plch->x,plch->y,plch->fx,plch->fy,&(plch->n1),&(plch->n2),
	     plch->strflag,plch->vrect,&(plch->arfact),0L);
}


static void clean_Champ(void *plot)
{
  struct rec_champ *theplot = plot;
  FREE(theplot->x);FREE(theplot->y);     
  FREE(theplot->fx);FREE(theplot->fy);     
  FREE(theplot->strflag);FREE(theplot->vrect);
  FREE(theplot->strflag_kp);FREE(theplot->vrect_kp);
}



/*---------------------------------------------------------------------
 * Utilities 
 *   for object copy 
 *---------------------------------------------------------------------------*/

static int CopyVectLI(int **nx,const int *x, int l)
{ 
  int i;
  if ( x != (int *) 0) 
    {
      *nx = (int *)  MALLOC(l*sizeof(int));
      if ( *nx == NULL) return(0);
      for ( i=0 ; i < l ; i++) (*nx)[i]= x[i];
    }
  return(1);
}

static int CopyVectF(double **nx,const double *x, int l)
{
  int i;
  if ( x != (double *) 0) 
    {
      *nx = (double *)  MALLOC(l*sizeof(double));
      if ( *nx == NULL) return(0);
      for ( i=0 ; i < l ; i++) (*nx)[i]= x[i];
    }
  return(1);
}

static int CopyVectC(char **nx,const char *x, int l)
{
  int i;
  if ( x != (char *) 0) 
    {
      *nx = (char *)  MALLOC(l*sizeof(char));
      if ( *nx == NULL) return(0);
      for ( i=0 ; i < l ; i++) (*nx)[i]= x[i];
    }
  return(1);
}

static int CopyVectS(char ***hstr, char **str)
{
  /** x is a null terminated string vector */
  if ( str != 0) 
    {
      char **loc;
      int count =0,i;
      while ( str[count] != NULL) count++;
      if ((loc = (char **) MALLOC( (count+1)*sizeof(char*))) == NULL) return 0;
      for ( i=0 ; i < count ; i++)
	{
	  if ( CopyVectC( &loc[i], str[i],strlen(str[i])+1) == 0) return 0;
	}
      loc[count]=0;
      *hstr = loc;
    }
  else
    {
      *hstr= NULL;
    }
  return(1);
}

static int MaybeCopyVect3dPLI(int iflag, int **nx,const int *x, int l)
{
  if (iflag == 1 ) 
    return( CopyVectLI(nx,x,l));
  else
    return(1);
}


/*-------------------------------------------------------------------------
 * Delete all recorded graphics associated to window winnumber 
 * Then if reset_flag is TRUE 
 *      [1] current graphic context is stored back in the list 
 *      [2] the scale flag is reset to zero (for preventing auto scale to use it) 
 *---------------------------------------------------------------------------*/

void tape_clean_plots(BCG *Xgc,int winnumber)
{
  int flag = FAIL;
  list_plot *list = Xgc->plots,* list1 ;
  /* if ( Xgc->record_flag == FALSE ) return ; */
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

void new_angles_plots(BCG *Xgc, int winnumber, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE ) return ;
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  if ( record_table[code].new_angles !=  NULL) 
	    record_table[code].new_angles(list->theplot,theta,alpha,iflag,flag,bbox);
	}
      list =list->next;
    }
}

static void new_angles_Plot3D(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  int i;
  struct rec_plot3d *theplot;
  theplot=(struct rec_plot3d *) plot;
  theplot->teta=*theta;
  theplot->alpha=*alpha;
  for (i=0 ; i< 3 ; i++) 
    if (iflag[i]!=0) theplot->flag[i] = flag[i];
  if ( iflag[3] != 0) 
    for ( i= 0 ; i < 6 ; i++ ) 
      theplot->bbox[i] = bbox[i];
}

static void new_angles_3dobj(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  int i;
  struct rec_3dobj *theplot;
  theplot=(struct rec_3dobj *) plot;
  theplot->teta=*theta;
  theplot->alpha=*alpha;
  for (i=0 ; i< 3 ; i++) 
    if (iflag[i]!=0) theplot->flag[i] = flag[i];
  if ( iflag[3] != 0) 
    for ( i= 0 ; i < 6 ; i++ ) 
      theplot->bbox[i] = bbox[i];
}

static void new_angles_Fac3D(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  int i;
  struct rec_fac3d *theplot;
  theplot=(struct rec_fac3d *) plot;
  theplot->teta=*theta;
  theplot->alpha=*alpha;
  for (i=0 ; i< 3 ; i++) 
    if (iflag[i]!=0) theplot->flag[i] = flag[i];
  if ( iflag[3] != 0) 
    for ( i= 0 ; i < 6 ; i++ ) 
      theplot->bbox[i] = bbox[i];
}

static void new_angles_Contour(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  int i;
  struct rec_contour *theplot;
  theplot=(struct rec_contour *) plot;
  theplot->teta=*theta;
  theplot->alpha=*alpha;
  for (i=0 ; i< 3 ; i++) 
    if (iflag[i]!=0) theplot->flag[i] = flag[i];
  if ( iflag[3] != 0) 
    for ( i= 0 ; i < 6 ; i++ ) 
      theplot->bbox[i] = bbox[i];
}

static void new_angles_Param3D(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  int i;
  struct rec_param3d *theplot;
  theplot=(struct rec_param3d *) plot;
  theplot->teta=*theta;
  theplot->alpha=*alpha;
  for (i=0 ; i< 3 ; i++) 
    if (iflag[i]!=0) theplot->flag[i] = flag[i];
  if ( iflag[3] != 0) 
    for ( i= 0 ; i < 6 ; i++ ) 
      theplot->bbox[i] = bbox[i];
}

static void new_angles_Param3D1(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox)
{
  int i;
  struct rec_param3d1 *theplot;
  theplot=(struct rec_param3d1 *) plot;
  theplot->teta=*theta;
  theplot->alpha=*alpha;
  for (i=0 ; i< 3 ; i++) 
    if (iflag[i]!=0) theplot->flag[i] = flag[i];
  if ( iflag[3] != 0) 
    for ( i= 0 ; i < 6 ; i++ ) 
      theplot->bbox[i] = bbox[i];
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

void scale_change_plots(BCG *Xgc,int winnumber, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin)
{
  list_plot *list = Xgc->plots ;
  if ( Xgc->record_flag == FALSE ) return ;
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  if ( record_table[code].scale_change != NULL) 
	    record_table[code].scale_change(Xgc,list->theplot,flag,bbox,aaint,strflag,undo,bbox1,subwin,winnumber);
	}
      list =list->next;
    }
}

/** change the plot flag in order to use bbox **/ 

static void scale_2D_change_flag(int undo, int flag, char *str,char *str_kp, char *str_new)
{
  if ( flag == 1 ) 
    {
      if ( str[2] == str_new[2] ) 
	{
	  str[1]= str_new[1];
	  if ( undo == 0) str_kp[1] =str[1];
	}
    }
}
  

static void scale_change_Plot(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_plot2d *theplot;
  theplot=(struct rec_plot2d *) plot;
  /* on passe en mode autoscale 5= on utilise bbox et on `regradue' dessus */
  scale_2D_change_flag(undo,flag[2], theplot->strflag,theplot->strflag_kp ,strflag);
  for ( i = 0 ; i < 4 ; i++)
    {
      if (flag[0]==1)
	{
	  theplot->brect[i]=bbox[i];
	  if (undo == 0)   theplot->brect_kp[i]=bbox[i];
	}
      if (flag[1]==1)  
	{
	  theplot->aint[i]=aaint[i];
	  if (undo == 0)   theplot->aint_kp[i]=aaint[i];
	}
    }
}

static void scale_change_Contour2D(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_contour2d *theplot;
  theplot=(struct rec_contour2d *) plot;
  /* on passe en mode autoscale 5= on utilise bbox et on `regradue' dessus */
  scale_2D_change_flag(undo,flag[2], theplot->strflag,theplot->strflag_kp,strflag);
  for ( i = 0 ; i < 4 ; i++)
    {
      if (flag[0]==1)
	{
	  theplot->brect[i]=bbox[i];
	  if (undo == 0)   theplot->brect_kp[i]=bbox[i];
	}
      if (flag[1]==1)  theplot->aint[i]=aaint[i];
    }
}

/* same for Gray1 and Gray2 NULL for Gray3 */

static void scale_change_Gray(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_gray *theplot;
  theplot=(struct rec_gray *) plot;
  /* on passe en mode autoscale 5= on utilise bbox et on `regradue' dessus */
  scale_2D_change_flag(undo,flag[2], theplot->strflag,theplot->strflag_kp,strflag);
  for ( i = 0 ; i < 4 ; i++)
    {
      if (flag[0]==1) 
	{
	  theplot->brect[i]=bbox[i];
	  if (undo == 0)   theplot->brect_kp[i]=bbox[i];
	}
      if (flag[1]==1)  theplot->aaint[i]=aaint[i];
    }
}

static void scale_change_Champ(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_champ *theplot;
  theplot=(struct rec_champ *) plot;
  /* on passe en mode autoscale 5= on utilise bbox et on `regradue' dessus */
  scale_2D_change_flag(undo,flag[2], theplot->strflag,theplot->strflag_kp,strflag);
  for ( i = 0 ; i < 4 ; i++)
    {
      if (flag[0]==1)
	{
	  theplot->vrect[i]=bbox[i];
	  if (undo == 0)   theplot->vrect_kp[i]=bbox[i];
	}
    }
}

static void scale_change_Fec(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_fec *theplot;
  theplot=(struct rec_fec *) plot;
  /* on passe en mode autoscale 5= on utilise bbox et on `regradue' dessus */
  scale_2D_change_flag(undo,flag[2], theplot->strflag,theplot->strflag_kp,strflag);
  for ( i = 0 ; i < 4 ; i++)
    {
      if (flag[0]==1)  
	{
	  theplot->brect[i]=bbox[i];
	  if (undo == 0)   theplot->brect_kp[i]=bbox[i];
	}	  
      if (flag[1]==1)  theplot->aaint[i]=aaint[i];
    }
}

/* here we deal with subwin changes */

static void scale_change_Ech(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_scale *theplot;
  theplot =   (struct rec_scale *) plot;
  
  if (bbox1 != NULL) 
    {
      /* we are trying to change the scale */
      int wdim[2];
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      /* check if bbox1 is inside current subwin */
      if ( theplot->Wrect[0]*wdim[0] <= bbox1[0] 
	   && (theplot->Wrect[0]+theplot->Wrect[2])*wdim[0] >= bbox1[2] 
	   && theplot->Wrect[1]*wdim[1] <= bbox1[1] 
	   && (theplot->Wrect[1]+theplot->Wrect[3])*wdim[1] >= bbox1[3] )
	{
	  /* bbox1 is inside the subwindow 
	   * we must change bbox to the subwindow scale 
	   */
	  flag[0]=1;
	  /* localy change the scale */
	  set_scale(Xgc,"tttftf",theplot->Wrect,theplot->Frect,NULL,theplot->logflag,NULL);
	  bbox[0] = XPixel2Double(bbox1[0]);
	  bbox[1] = YPixel2Double(bbox1[1]);
	  bbox[2] = XPixel2Double(bbox1[2]);
	  bbox[3] = YPixel2Double(bbox1[3]);
	}
      else 
	flag[0]=0;
    }
  else if ( subwin != NULL) 
    {
      if (    Abs(theplot->Wrect[0] - subwin[0]) < 1.e-8
	      && Abs(theplot->Wrect[1] - subwin[1]) < 1.e-8
	      && Abs(theplot->Wrect[2] - subwin[2]) < 1.e-8
	      && Abs(theplot->Wrect[3] - subwin[3]) < 1.e-8 )
	{
	  /* we are switching to the good subwindow */
	  /* sciprint("ech : je suis ds la bonne subwin [%f,%f,%f,%f]\r\n",
	     subwin[0],subwin[1],subwin[2],subwin[3]);
	  */
	  flag[0] = 1;
	}
      else 
	{
	  /* sciprint("ech : je suis pas ds la bonne subwin [%f,%f,%f,%f]\r\n",
	     subwin[0],subwin[1],subwin[2],subwin[3]);
	  */
	  flag[0] = 0;
	}
    }

  for ( i = 0 ; i < 4 ; i++)
    {
      if (flag[0]==1) 
	{
	  theplot->Frect[i]=bbox[i];
	  if (undo == 0)   theplot->Frect_kp[i]=bbox[i];
	}
    }
}

static void scale_change_NEch(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  struct rec_nscale *theplot;
  theplot =   (struct rec_nscale *) plot;
  if (bbox1 != NULL) 
    {
      /* we are trying to change the scale with a zoom rect */
      /* specified in bbox1 in pixel */
      /* 1- check if nscale contains a subwin definition */
      if ( theplot->flag[1] == 't' ) 
	{
	  int wdim[2];
	  flag[0]=0; 
	  Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
	  /* check if bbox1 is inside current subwin */
	  if (theplot->Wrect[0]*wdim[0] <= bbox1[0] && (theplot->Wrect[0]+theplot->Wrect[2])*wdim[0] >= bbox1[2] 
	      && theplot->Wrect[1]*wdim[1] <= bbox1[1] && (theplot->Wrect[1]+theplot->Wrect[3])*wdim[1] >= bbox1[3] )
	    {
	      /** we extract the associated scale **/
	      move_subwindow_scale_to_front(Xgc,theplot->Wrect);
	      /* get_window_scale(Xgc,theplot->Wrect); */
	      /* bbox1 is inside the subwindow 
	       * we must change bbox to the subwindow scale 
	       */
	      bbox[0] = XPixel2Double(bbox1[0]);
	      bbox[1] = YPixel2Double(bbox1[1]);
	      bbox[2] = XPixel2Double(bbox1[2]);
	      bbox[3] = YPixel2Double(bbox1[3]);
	      /* sciprint("je trouve un bbox ds [%f %f %f %f ] ds [%f,%f,%f,%f] log=[%c,%c]\r\n",
		 bbox[0],bbox[1],bbox[2],bbox[3],
		 theplot->Wrect[0],theplot->Wrect[1],theplot->Wrect[2],theplot->Wrect[3],
		 Xgc->scales->logflag[0],		       Xgc->scales->logflag[1]);
	      */
	      /* flag is changed ==> next recorded plots will use bbox **/
	      flag[0]=1;
	    }
	}
    }
  else if ( subwin != NULL) 
    {
      /* check if subwin is the same as theplot->Wrect */
      if ( theplot->flag[1] == 't' ) 
	{ 
	  if( Abs(theplot->Wrect[0] - subwin[0]) < 1.e-8
	      && Abs(theplot->Wrect[1] - subwin[1]) < 1.e-8
	      && Abs(theplot->Wrect[2] - subwin[2]) < 1.e-8
	      && Abs(theplot->Wrect[3] - subwin[3]) < 1.e-8 )
	    {
	      /* we are switching to the good subwindow */
	      /* sciprint("Nech : je suis ds la bonne subwin [%f,%f,%f,%f]\r\n",
		 subwin[0],subwin[1],subwin[2],subwin[3]);
	      */
	      flag[0] = 1;
	    }
	  else
	    flag[0] = 0;
	}
      else 
	{
	  flag[0] = 0;
	  /* sciprint("Nech: je suis pas ds la bonne subwin \r\n"); */
	}
    }
  if (flag[0]==1) 
    for ( i = 0 ; i < 4 ; i++)
      {
	theplot->Frect[i]=bbox[i];
	if (undo == 0)   theplot->Frect_kp[i]=bbox[i];
      }
}

/*** code added by ES 21/5/2002 ****/

static void scale_change_Contour(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag,
				 int undo, int *bbox1, double *subwin, int win_num)
{
  int i, j;
  double xmin, xmax, ymin, ymax, zmin, zmax, xp, yp, x,y,z;
  struct rec_contour *theplot;
  theplot =   (struct rec_contour *) plot;
  if (bbox1 != NULL) 
    {
      b1[0] = XPixel2Double(bbox1[0]);
      b1[1] = YPixel2Double(bbox1[1]);
      b1[2] = XPixel2Double(bbox1[2]);
      b1[3] = YPixel2Double(bbox1[3]);
      /*   sciprint("I'm trying to zoom a 3d contour \r\n"); */
      /*  sciprint(" zoom area: x={%f:%f};  y={%f:%f}\r\n",b1[0],b1[2],b1[1],b1[3]); */
      xmin=Maxi(theplot->x,theplot->n1); xmax=Mini(theplot->x,theplot->n1);
      ymin=Maxi(theplot->y,theplot->n1); ymax=Mini(theplot->y,theplot->n2);
      zmin=Maxi(theplot->z,theplot->n1*theplot->n2);
      zmax=Mini(theplot->z,theplot->n1*theplot->n2);
      for (i=0; i < theplot->n1; i++)
	for (j=0; j < theplot->n2; j++)
	  {
	    x=theplot->x[i]; y=theplot->y[j]; z=theplot->z[j*theplot->n1 + i]; 
	    xp=TRX(x,y,z); yp=TRY(x,y,z); 
	    if(xp >= b1[0] && xp <= b1[2] && yp >= b1[1] && yp <= b1[3])
	      {
		if(x < xmin) xmin=x; if(x > xmax) xmax=x;
		if(y < ymin) ymin=y; if(y > ymax) ymax=y;
		if(z < zmin) zmin=z; if(z > zmax) zmax=z;
	      }
	  }
      if (xmax > xmin) {theplot->bbox[0]=xmin; theplot->bbox[1]=xmax;}
      if (ymax > ymin) {theplot->bbox[2]=ymin; theplot->bbox[3]=ymax;}
      if (zmax > zmin) {theplot->bbox[4]=zmin; theplot->bbox[5]=zmax;}
      if (theplot->flag[1]>0) theplot->flag[1]=2*Xgc->scales->metric3d-1;
    }
}


static void scale_change_Fac3D(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag,
			       int undo, int *bbox1, double *subwin, int win_num)
{
  int i;
  double xmin, xmax, ymin, ymax, zmin, zmax, xp, yp, x,y,z;
  struct rec_fac3d *theplot;
  theplot =   (struct rec_fac3d *) plot;
  if (bbox1 != NULL) 
    {
      b1[0] = XPixel2Double(bbox1[0]);
      b1[1] = YPixel2Double(bbox1[1]);
      b1[2] = XPixel2Double(bbox1[2]);
      b1[3] = YPixel2Double(bbox1[3]);
      /*   sciprint("I'm trying to zoom a 3d fac-plot \r\n");*/
      /*  sciprint(" zoom area: x={%f:%f};  y={%f:%f}\r\n",b1[0],b1[2],b1[1],b1[3]); */
      xmin=Maxi(theplot->x,theplot->p*theplot->q);
      xmax=Mini(theplot->x,theplot->p*theplot->q);
      ymin=Maxi(theplot->y,theplot->p*theplot->q);
      ymax=Mini(theplot->y,theplot->p*theplot->q);
      zmin=Maxi(theplot->z,theplot->p*theplot->q);
      zmax=Mini(theplot->z,theplot->p*theplot->q);
      for (i=0; i < theplot->p*theplot->q; i++)
	{
	  x=theplot->x[i]; y=theplot->y[i]; z=theplot->z[i]; 
	  xp=TRX(x,y,z); yp=TRY(x,y,z); 
	  if(xp >= b1[0] && xp <= b1[2] && yp >= b1[1] && yp <= b1[3])
	    {
	      if(x < xmin) xmin=x; if(x > xmax) xmax=x;
	      if(y < ymin) ymin=y; if(y > ymax) ymax=y;
	      if(z < zmin) zmin=z; if(z > zmax) zmax=z;
	    }
	}
      if (xmax > xmin) {theplot->bbox[0]=xmin; theplot->bbox[1]=xmax;}
      if (ymax > ymin) {theplot->bbox[2]=ymin; theplot->bbox[3]=ymax;}
      if (zmax > zmin) {theplot->bbox[4]=zmin; theplot->bbox[5]=zmax;}
      if (theplot->flag[1]>0) theplot->flag[1]=2*Xgc->scales->metric3d-1; 
    }
}

static void scale_change_Param3D(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag,
				 int undo, int *bbox1, double *subwin, int win_num)
{
  int i; double xmin, xmax, ymin, ymax, zmin, zmax, xp, yp, x,y,z;
  struct rec_param3d *theplot;
  theplot =   (struct rec_param3d *) plot;
  if (bbox1 != NULL) 
    {
      b1[0] = XPixel2Double(bbox1[0]);
      b1[1] = YPixel2Double(bbox1[1]);
      b1[2] = XPixel2Double(bbox1[2]);
      b1[3] = YPixel2Double(bbox1[3]);
      /*  sciprint(" zoom area: x={%f:%f};  y={%f:%f}\r\n",b1[0],b1[2],b1[1],b1[3]); */
      xmin=Maxi(theplot->x,theplot->n); xmax=Mini(theplot->x,theplot->n);
      ymin=Maxi(theplot->y,theplot->n); ymax=Mini(theplot->y,theplot->n);
      zmin=Maxi(theplot->z,theplot->n); zmax=Mini(theplot->z,theplot->n);
      /* search the min and max x, y, z of the points which got projected into the bbox1 */
      xmin=theplot->bbox[1]; xmax=theplot->bbox[0];
      ymin=theplot->bbox[3]; ymax=theplot->bbox[2];
      zmin=theplot->bbox[5]; zmax=theplot->bbox[4];
      for (i=0; i < theplot->n; i++)
	{
	  x=theplot->x[i]; y=theplot->y[i]; z=theplot->z[i]; 
	  xp=TRX(x,y,z); yp=TRY(x,y,z); 
	  /*      sciprint("%f %f \r\n",xp,yp); */
	  if(xp >= b1[0] && xp <= b1[2] && yp >= b1[1] && yp <= b1[3])
	    {
	      /*        sciprint("**\r\n");  */
	      if(x < xmin) xmin=x; if(x > xmax) xmax=x;
	      if(y < ymin) ymin=y; if(y > ymax) ymax=y;
	      if(z < zmin) zmin=z; if(z > zmax) zmax=z;
	    }
	}
      if (xmax > xmin) {theplot->bbox[0]=xmin; theplot->bbox[1]=xmax;}
      if (ymax > ymin) {theplot->bbox[2]=ymin; theplot->bbox[3]=ymax;}
      if (zmax > zmin) {theplot->bbox[4]=zmin; theplot->bbox[5]=zmax;}
      /*   sciprint("trying to zoom a 3d param3d plot: x= [%f:%f], y=[%f:%f], z=[%f:%f]\r\n",xmin, xmax, ymin, ymax, zmin, zmax);
       */
      if (theplot->flag[1]>0) theplot->flag[1]=2*Xgc->scales->metric3d-1; 
    }
}

static void scale_change_Param3D1(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag, 
				  int undo, int *bbox1, double *subwin, int win_num)
{
  int i; 
  double xmin, xmax, ymin, ymax, zmin, zmax, xp, yp, x,y,z;
  struct rec_param3d1 *theplot;
  theplot =   (struct rec_param3d1 *) plot;
  if (bbox1 != NULL) 
    {
      b1[0] = XPixel2Double(bbox1[0]);
      b1[1] = YPixel2Double(bbox1[1]);
      b1[2] = XPixel2Double(bbox1[2]);
      b1[3] = YPixel2Double(bbox1[3]);
      /*   sciprint("I'm trying to zoom a param3d-1 plot  \r\n");*/
      /*  sciprint(" zoom area: x={%f:%f};  y={%f:%f}\r\n",b1[0],b1[2],b1[1],b1[3]); */
      xmin=Maxi(theplot->x,theplot->n*theplot->m);
      xmax=Mini(theplot->x,theplot->n*theplot->m);
      ymin=Maxi(theplot->y,theplot->n*theplot->m);
      ymax=Mini(theplot->y,theplot->n*theplot->m);
      zmin=Maxi(theplot->z,theplot->n*theplot->m);
      zmax=Mini(theplot->z,theplot->n*theplot->m);
      for (i=0; i < theplot->n*theplot->m; i++)
	{
	  x=theplot->x[i]; y=theplot->y[i]; z=theplot->z[i]; 
	  xp=TRX(x,y,z); yp=TRY(x,y,z); 
	  if(xp >= b1[0] && xp <= b1[2] && yp >= b1[1] && yp <= b1[3])
	    {
	      if(x < xmin) xmin=x; if(x > xmax) xmax=x;
	      if(y < ymin) ymin=y; if(y > ymax) ymax=y;
	      if(z < zmin) zmin=z; if(z > zmax) zmax=z;
	    }
	}
      if (xmax > xmin) {theplot->bbox[0]=xmin; theplot->bbox[1]=xmax;}
      if (ymax > ymin) {theplot->bbox[2]=ymin; theplot->bbox[3]=ymax;}
      if (zmax > zmin) {theplot->bbox[4]=zmin; theplot->bbox[5]=zmax;}
      if (theplot->flag[1]>0) theplot->flag[1]=2*Xgc->scales->metric3d-1;
      /*   sciprint("I'm trying to zoom a 3d param3d1 plot \r\n"); */
    } 
}

static void scale_change_Plot3D(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag, 
				int undo, int *bbox1, double *subwin, int win_num)
{
  int i, j;
  double xmin, xmax, ymin, ymax, zmin, zmax, xp, yp, x,y,z;
  struct rec_plot3d *theplot;
  theplot =   (struct rec_plot3d *) plot;
  if (bbox1 != NULL) 
    {
      /*   sciprint("I'm trying to zoom a 3d plot\r\n"); */
      b1[0] = XPixel2Double(bbox1[0]);
      b1[1] = YPixel2Double(bbox1[1]);
      b1[2] = XPixel2Double(bbox1[2]);
      b1[3] = YPixel2Double(bbox1[3]);
      /*  sciprint(" zoom area: x={%f:%f};  y={%f:%f}\r\n",b1[0],b1[2],b1[1],b1[3]); */
      xmin=Maxi(theplot->x,theplot->p); xmax=Mini(theplot->x,theplot->p);
      ymin=Maxi(theplot->y,theplot->q); ymax=Mini(theplot->y,theplot->q);
      zmin=Maxi(theplot->z,theplot->q*theplot->p);
      zmax=Mini(theplot->z,theplot->q*theplot->p);
      for (i=0; i < theplot->p; i++)
	for (j=0; j < theplot->q; j++)
	  {
	    x=theplot->x[i]; y=theplot->y[j]; z=theplot->z[j*theplot->p + i]; 
	    xp=TRX(x,y,z); yp=TRY(x,y,z); 
	    if(xp >= b1[0] && xp <= b1[2] && yp >= b1[1] && yp <= b1[3])
	      {
		if(x < xmin) xmin=x; if(x > xmax) xmax=x;
		if(y < ymin) ymin=y; if(y > ymax) ymax=y;
		if(z < zmin) zmin=z; if(z > zmax) zmax=z;
		/*        sciprint("%i,%i  (%f,%f,%f)-->(%f,%f)\r\n",i,j,x,y,z,xp,yp); */
	      }
	  }
      if (xmax > xmin) {theplot->bbox[0]=xmin; theplot->bbox[1]=xmax;}
      if (ymax > ymin) {theplot->bbox[2]=ymin; theplot->bbox[3]=ymax;}
      if (zmax > zmin) {theplot->bbox[4]=zmin; theplot->bbox[5]=zmax;}
      if (theplot->flag[1]>0) theplot->flag[1]=2*Xgc->scales->metric3d-1; 
      /*   sciprint("theplotflags: %i %i %i\r\n", theplot->flag[0], theplot->flag[1],
	   theplot->flag[2]);
	   sciprint("flags: %i %i %\r\n", flag[0], flag[1],flag[2]); */
      /*   sciprint(" new bbox:: x=%f:%f; y=%f:%f; z=%f:%f\r\n",
	   theplot->bbox[0],theplot->bbox[1],theplot->bbox[2],
	   theplot->bbox[3],theplot->bbox[4],theplot->bbox[5]); */ 
    }
}

static void scale_change_3dobj(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag, 
			       int undo, int *bbox1, double *subwin, int win_num)
{
  struct rec_3dobj *theplot;
  theplot =   (struct rec_3dobj *) plot;
  /* XXXX */

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

static void unscale_Plot(void *plot)
{
  int i;
  struct rec_plot2d * theplot= plot;
  strcpy( theplot->strflag,theplot->strflag_kp);
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->brect[i]=theplot->brect_kp[i];
      theplot->aint[i]=theplot->aint_kp[i];
    }
}
static void unscale_Contour2D(void *plot)
{
  int i;
  struct rec_contour2d *theplot = plot;
  strcpy( theplot->strflag,theplot->strflag_kp);
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->brect[i]=theplot->brect_kp[i];
      theplot->aint[i]=theplot->aint_kp[i];
    }
}

static void unscale_Gray(void *plot)
{
  int i;
  struct rec_gray *theplot = plot;
  strcpy( theplot->strflag,theplot->strflag_kp);
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->brect[i]=theplot->brect_kp[i];
      theplot->aaint[i]=theplot->aaint_kp[i];
    }
}

static void unscale_Champ(void *plot)
{
  int i;
  struct rec_champ *theplot = plot;
  strcpy( theplot->strflag,theplot->strflag_kp);
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->vrect[i]=theplot->vrect_kp[i];
    }
}

static void unscale_Fec(void *plot)
{
  int i;
  struct rec_fec *theplot = plot;
  strcpy( theplot->strflag,theplot->strflag_kp);
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->brect[i]=theplot->brect_kp[i];
      theplot->aaint[i]=theplot->aaint_kp[i];
    }
}

static void unscale_Ech(void *plot)
{
  int i;
  struct rec_scale *theplot = plot;
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->Frect[i]=theplot->Frect_kp[i];
    }
}

static void unscale_NEch(void *plot)
{
  int i;
  struct rec_nscale *theplot = plot;
  for ( i = 0 ; i < 4 ; i++)
    {
      theplot->Frect[i]=theplot->Frect_kp[i];
    }
}

/*** code added by ES 21/5/2002 ****/
static void unscale_Contour(void *plot)
{
  struct rec_contour *theplot = plot;
  theplot->bbox[0]=Mini(theplot->x,theplot->n1); theplot->bbox[1]=Maxi(theplot->x,theplot->n1); 
  theplot->bbox[2]=Mini(theplot->y,theplot->n2); theplot->bbox[3]=Maxi(theplot->y,theplot->n2); 
  theplot->bbox[4]=Mini(theplot->z,theplot->n1*theplot->n2); 
  theplot->bbox[5]=Maxi(theplot->z,theplot->n1*theplot->n2); 
}

static void unscale_Fac3D(void *plot)
{
  struct rec_fac3d *theplot = plot;
  theplot->bbox[0]=Mini(theplot->x,theplot->p*theplot->q); 
  theplot->bbox[1]=Maxi(theplot->x,theplot->p*theplot->q); 
  theplot->bbox[2]=Mini(theplot->y,theplot->p*theplot->q); 
  theplot->bbox[3]=Maxi(theplot->y,theplot->p*theplot->q); 
  theplot->bbox[4]=Mini(theplot->z,theplot->p*theplot->q); 
  theplot->bbox[5]=Maxi(theplot->z,theplot->p*theplot->q); 
}

static void unscale_Param3D(void *plot)
{
  struct rec_param3d *theplot = plot ;
  theplot->bbox[0]=Mini(theplot->x,theplot->n); theplot->bbox[1]=Maxi(theplot->x,theplot->n); 
  theplot->bbox[2]=Mini(theplot->y,theplot->n); theplot->bbox[3]=Maxi(theplot->y,theplot->n); 
  theplot->bbox[4]=Mini(theplot->z,theplot->n); theplot->bbox[5]=Maxi(theplot->z,theplot->n); 
}

static void unscale_Param3D1(void *plot)
{
  struct rec_param3d1 *theplot = plot;
  theplot->bbox[0]=Mini(theplot->x,theplot->n*theplot->m); 
  theplot->bbox[1]=Maxi(theplot->x,theplot->n*theplot->m); 
  theplot->bbox[2]=Mini(theplot->y,theplot->n*theplot->m); 
  theplot->bbox[3]=Maxi(theplot->y,theplot->n*theplot->m); 
  theplot->bbox[4]=Mini(theplot->z,theplot->n*theplot->m); 
  theplot->bbox[5]=Maxi(theplot->z,theplot->n*theplot->m); 
}

static void unscale_Plot3D(void *plot)
{
  struct rec_plot3d *theplot = plot;
  theplot->bbox[0]=Mini(theplot->x,theplot->p); theplot->bbox[1]=Maxi(theplot->x,theplot->p); 
  theplot->bbox[2]=Mini(theplot->y,theplot->q); theplot->bbox[3]=Maxi(theplot->y,theplot->q); 
  theplot->bbox[4]=Mini(theplot->z,theplot->p*theplot->q); 
  theplot->bbox[5]=Maxi(theplot->z,theplot->p*theplot->q); 
}

static void unscale_3dobj(void *plot)
{
  /* struct rec_3dobj *theplot = plot; */
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
	    case CODEParam3D:
	    case CODEParam3D1:
	    case CODEPlot3D:
	    case CODEPlot3D1:
	    case CODEFac3D:
	    case CODEFac3D1:
	    case CODEFac3D2:
	    case CODEFac3D3:
	    case CODE3dobj:
	    case CODEContour:
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

void tape_replay_new_scale(BCG *Xgc,int winnumber, int *flag, int *aaint,  double *bbox)
{ 
  /** get the bounding box in pixel */
  int bbox1[4];
  bbox1[0]= XDouble2Pixel(bbox[0]);
  bbox1[1]= YDouble2Pixel(bbox[1]);
  bbox1[2]= XDouble2Pixel(bbox[2]);
  bbox1[3]= YDouble2Pixel(bbox[3]);
  scale_change_plots(Xgc,winnumber,flag,bbox,aaint,NULL,1,bbox1,NULL);
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
  new_angles_plots(Xgc,winnumber,theta,alpha,iflag,flag,bbox);
  tape_replay(Xgc,winnumber);
}

/*---------------------------------------------------------------------
 * redraw stored graphics 
 * in the window (or file) described by Xgc
 *---------------------------------------------------------------------------*/

void tape_replay(BCG *Xgc,int winnumber)
{ 
  list_plot *list;
  if ( Xgc == NULL ) return ;
  if ( Xgc->record_flag == FALSE ) return ;
  Xgc->record_flag = FALSE; /* be sure not to record during replay */
  list = Xgc->plots ;
  while (list)
    {
      if ( list->theplot != NULL) 
	record_table[((plot_code *) list->theplot)->code ].replay(Xgc,list->theplot);
      list =list->next;
    }
  /* Is there a replay handler */
  scig_handler(Xgc,winnumber);
  Xgc->record_flag = TRUE; /* be sure to set back record_flg to its proper stat */
}



/*---------------------------------------------------------------------
 * Add a new graphics record in the graphic recorder list
 *---------------------------------------------------------------------------*/

int store_record(BCG *Xgc,int code ,void *plot)
{
  list_plot *list = Xgc->plots ;
  if ( code < 0 ||  code >= CODEendplots )
    {
      Scistring("Error: code is not a plot code\n");
      return 0;
    }

  if ( Xgc->record_flag == FALSE ) return 1 ;
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
	  Scistring("store_ (store-1): malloc No more Place");
	  return(0);
	}
    }
  else 
    {
      list = Xgc->last_plot;
      /*       while (list->next != NULL) list=list->next; */
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
	  Scistring("store_ (store-3):No more Place\n");
	  return(0);
	}
    }
  return(1);
}




/*---------------------------------------------------------------------
 * utilities 
 *---------------------------------------------------------------------------*/

static void store_void(BCG *Xgc,int code)
{
  struct rec_void *lplot= ((struct rec_void *) MALLOC(sizeof(struct rec_void)));
  if (lplot != NULL)
    {
      lplot->code = code;
      store_record(Xgc,code,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

static void store_int(BCG *Xgc,int code,int val)
{
  struct rec_int *lplot= ((struct rec_int *) MALLOC(sizeof(struct rec_int)));
  if (lplot != NULL)
    {
      lplot->code = code;
      lplot->val = val;
      store_record(Xgc,code,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

static void store_int2(BCG *Xgc,int code,int val, int val1)
{
  struct rec_int2 *lplot= ((struct rec_int2 *) MALLOC(sizeof(struct rec_int2)));
  if (lplot != NULL)
    {
      lplot->code = code;
      lplot->val = val;
      lplot->val1 = val1;
      store_record(Xgc,code,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

static void store_int4(BCG *Xgc,int code,int vals[])
{
  struct rec_int4 *lplot= ((struct rec_int4 *) MALLOC(sizeof(struct rec_int4)));
  if (lplot != NULL)
    {
      int i;
      lplot->code = code;
      for ( i=0; i< 4 ; i++ ) lplot->vals[i] = vals[i];
      store_record(Xgc,code,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

static void store_double4(BCG *Xgc,int code,double vals[])
{
  struct rec_double4 *lplot= ((struct rec_double4 *) MALLOC(sizeof(struct rec_double4)));
  if (lplot != NULL)
    {
      int i;
      lplot->code = code;
      for ( i=0; i< 4 ; i++ ) lplot->vals[i] = vals[i];
      store_record(Xgc,code,lplot);
      return; 
    }
  Scistring("\nstore_ Plot (xcall1): No more place \n");
}

/* Store a Graphic object  */

void store_graphic_object(BCG *Xgc,NspObject *obj)
{
  NspObject *obj_cp;
  struct rec_object *lplot= MALLOC(sizeof(struct rec_object));
  if (lplot != NULL)
    {
      if ((obj_cp = nsp_object_copy_and_name("Obj",obj)) != NULLOBJ )
	{
	  lplot->obj = obj_cp ;
	  store_record(Xgc,CODEobject, lplot);
	  return;
	}
    }
  Scistring("Out of memory in store_graphic_object \n");
  return; 
}

static void replay_graphic_object(BCG *Xgc,void  *theplot)
{
  struct rec_object *lplot= theplot ;
  NspGraphic *G = (NspGraphic *) lplot->obj;
  G->type->draw(Xgc,G);
  Scistring("replay graphic_object \n");
  /* nsp_graphic_object_replay(Xgc,obj); */
}

static void clean_graphic_object(void *plot) 
{
  NspObject *obj = ((struct rec_object *) plot)->obj;
  Scistring("clean graphic_object \n");
  nsp_object_destroy(&obj);
};





 



