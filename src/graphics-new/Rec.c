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
#include "../system/files.h" /* FSIZE */
#include <gtk/gtk.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

extern void nsp_figure_change3d_orientation(BCG *Xgc,NspGraphic *Obj,
					    double theta, double alpha,int *pt);
extern void nsp_figure_unzoom(NspGraphic *Obj);
extern void nsp_figure_zoom(BCG *Xgc,NspGraphic *Obj, int *bbox1);


void tape_store_graphic_object(BCG *Xgc,void *vobj)
{
  NspObject *obj_cp;
  if ((obj_cp = nsp_object_copy_and_name("Obj",vobj)) == NULLOBJ ) return;
  Xgc->figure = obj_cp;
  return; 
}


/**
 * tape_clean_plots:
 * @Xgc: 
 * @winnumber: 
 * 
 * 
 **/

void tape_clean_plots(BCG *Xgc,int winnumber)
{
  NspObject *Obj;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  Obj = (NspObject *) Xgc->figure ;
  nsp_object_destroy(&Obj);
  Xgc->figure = NULL;
}


/**
 * tape_new_angles_plots:
 * @Xgc: 
 * @winnumber: 
 * @theta: 
 * @alpha: 
 * @iflag: 
 * @flag: 
 * @bbox: 
 * @pt: 
 * 
 * Change les angles alpha theta dans tous les plot3d stockes 
 * change  aussi flag et box suivant la valeur de iflag.
 * iflag est de longueur [4] si iflag[i] != 0 cela veut dire qu'il faut changer le 
 * flag[i] en utilisant celui de l'argument flag.
 * iflag[3] sert a dire s'il faut ou pas changer bbox 
 * 
 **/

void tape_new_angles_plots(BCG *Xgc, int winnumber, double *theta, double *alpha, 
			   int *iflag, int *flag, double *bbox, int *pt)
{
  NspGraphic *G;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  G = (NspGraphic *) Xgc->figure ;
  nsp_figure_change3d_orientation(Xgc,G,*theta,*alpha,pt);
}


/**
 * scale_change_plots:
 * @Xgc: 
 * @winnumber: 
 * @flag: 
 * @bbox: 
 * @aaint: 
 * @strflag: 
 * @undo: 
 * @bbox1: 
 * @subwin: 
 *
 * change the scale in all the recorded graphics 
 *   if flag[0]== 1  bbox is changed 
 *   if flag[1]== 1  aaint is changed 
 *   if flag[2]== 1  then strfflag[1] is changed if (strflag[2] coincide with given one )
 *   if undo = 1 then the work can be undone (with unzoom)
 *   else undo = 1 unzoom cannot be performed 
 *   if subwin == NULL 
 *       => we must find the subwin asscoiated to bbox1
 * 
 *
 **/

static void scale_change_plots(BCG *Xgc,int winnumber, int *flag, double *bbox, int *aaint,char *strflag, int undo, int *bbox1, double *subwin)
{
  NspGraphic *G;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  G = (NspGraphic *) Xgc->figure ;
  nsp_figure_zoom(Xgc,G,bbox1);
}


/**
 * tape_check_recorded_3D:
 * @Xgc: 
 * @winnumber: 
 * 
 * 
 * checks if recorded list contains 3d graphics 
 * 
 * Returns: 
 **/

int tape_check_recorded_3D(BCG *Xgc,int winnumber)
{
  /* A revoir 
   *
   */
  return OK;
}


/**
 * tape_replay_undo_scale:
 * @Xgc: 
 * @winnumber: 
 * 
 *  restore scales (unzoom) and redraw stored graphics 
 * 
 **/

void tape_replay_undo_scale(BCG *Xgc,int winnumber)
{ 
  NspGraphic *G;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  G = (NspGraphic *) Xgc->figure ;
  nsp_figure_unzoom(G);
  G->type->draw(Xgc,G,NULL,NULL);
  nsp_gr_handler(Xgc,winnumber);
}


/**
 * tape_replay_new_scale:
 * @Xgc: 
 * @winnumber: 
 * @flag: 
 * @aaint: 
 * @bbox: 
 * @ibbox: 
 * 
 * used when zooming: replay with a new scale 
 * the new scale is coded in bbox=[xmin,ymin,xmax,ymax] 
 * the problem is a bit complex if we have many subwindows 
 * 
 **/

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
  tape_replay(Xgc,winnumber,NULL);
}



/**
 * tape_replay_new_scale_1:
 * @Xgc: 
 * @winnumber: 
 * @flag: 
 * @aaint: 
 * @bbox: 
 * @strflag: 
 * 
 * replay with a new Scale but undo is impossible 
 * used for automatic scales 
 * 
 **/
void tape_replay_new_scale_1(BCG *Xgc,int winnumber, int *flag, int *aaint, double *bbox,char *strflag)
{ 
  /* here we want to change (bbox,aaint,strfag) but only for recorded graphics 
   * which are on the same subwin as the current one 
   * and we do not want this operation to be undone ==> undo =0 
   */
  scale_change_plots(Xgc,winnumber,flag,bbox,aaint,strflag,0,NULL,Xgc->scales->subwin_rect);
  tape_replay(Xgc,winnumber,NULL);
}

/**
 * tape_replay_new_angles:
 * @Xgc: 
 * @winnumber: 
 * @iflag: 
 * @flag: 
 * @theta: 
 * @alpha: 
 * @bbox: 
 * 
 *  changes theta, alpha  flag and bbox 
 *  then redraw recorded graphics 
 * 
 **/
void tape_replay_new_angles(BCG *Xgc,int winnumber,int *iflag, int *flag,double *theta, double *alpha, double *bbox)
{ 
  NspGraphic *G;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  G = (NspGraphic *) Xgc->figure ;
  nsp_figure_change3d_orientation(Xgc,G,*theta,*alpha,NULL);
  tape_replay(Xgc,winnumber,NULL);
}

/**
 * tape_replay:
 * @Xgc: 
 * @winnumber: 
 * @rect: 
 * 
 * call the draw method on the figure contained 
 * in Xgc. Then call a graphic handler if such handler exists. 
 *
 **/

void tape_replay(BCG *Xgc,int winnumber,int *rect)
{ 
  NspGraphic *G;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  G = (NspGraphic *) Xgc->figure ;
  G->type->draw(Xgc,G, (GdkRectangle *)rect,NULL);
  /* Is there a replay handler */
  nsp_gr_handler(Xgc,winnumber);
}


/**
 * tape_replay_mix:
 * @Xgc: 
 * @Xgc1: 
 * @winnumber: 
 * 
 * Replay the contents of Xgc1 bu with the driver 
 * associated to Xgc.
 * 
 **/

void tape_replay_mix(BCG *Xgc,BCG *Xgc1, int winnumber)
{ 
  NspGraphic *G;
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  G = (NspGraphic *) Xgc1->figure ;
  G->type->draw(Xgc,G,NULL,NULL);
  /* Is there a replay handler */
  nsp_gr_handler(Xgc,winnumber);
}

/*
 * search a graphic object in the recorded objects 
 */

NspObject * tape_search_graphic_object(BCG *Xgc,int winnumber)
{ 
  if ( Xgc == NULL ) return NULL;
  return (NspObject *) Xgc->figure ; 
}


/**
 * tape_save:
 * @Xgc: 
 * @fname: 
 * @winnumber: 
 * 
 * 
 * 
 * Returns: 
 **/

int xx_tape_save(BCG *Xgc,const char *fname, int winnumber)
{
  NspFile *F;
  /* expand keys in path name result in buf */
  if ( Xgc == NULL || Xgc->figure == NULL) return FAIL;
  if (( F =nsp_file_open_xdr_w(fname)) == NULLSCIFILE) return FAIL;
  if (nsp_object_xdr_save(F->obj->xdrs,Xgc->figure)== FAIL) 
    return FAIL;
  /* flag for detecting end of obj at reload */
  nsp_xdr_save_i(F->obj->xdrs,nsp_no_type_id);
  if (nsp_file_close_xdr_w(F) == FAIL) 
    {
      nsp_file_destroy(F);
      return FAIL;
    }
  nsp_file_destroy(F);
  return OK;
}

/**
 * tape_load:
 * @Xgc: 
 * @fname: 
 * 
 * 
 * 
 * Returns: 
 **/

int xx_tape_load(BCG *Xgc,const char *fname)
{
  NspObject *O; 
  NspFile *F;
  if (( F =nsp_file_open_xdr_r(fname)) == NULLSCIFILE) 
    return FAIL;
  if ((O=nsp_object_xdr_load(F->obj->xdrs))== NULLOBJ ) 
    return FAIL;
  if ( ! IsFigure(O)) 
    return FAIL;
  if (nsp_file_close_xdr_r(F) == FAIL)
    {
      nsp_file_destroy(F);
      return FAIL;
    }
  nsp_file_destroy(F);
  return OK;
}
