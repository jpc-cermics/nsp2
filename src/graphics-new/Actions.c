/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"
#include "nsp/object.h"
#define PERI_ACTION_PRIVATE 
#include "nsp/graphics-new/actions.h"
#include "nsp/command.h"
#include "../system/files.h" /* FSIZE */
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include <nsp/axes.h> 

static void zoom_get_rectangle(BCG *Xgc,double *bbox, int *ibbox);
static int nsp_gr_buzy = 0;

/*
 * the functions in this file are called from 
 * callback ( see jpc_SGraph.c ) for the XWindow version 
 *
 */

/*
 * sets a specific draw handler for a graphic window 
 * which is called by tape_replay. 
 */ 

static int nsp_gr_handler_none(BCG *Xgc,int win_num) {return win_num;}

Scig_handler nsp_gr_handler = nsp_gr_handler_none;

Scig_handler __nsp_gr_set_handler(Scig_handler f)
{
  Scig_handler old = nsp_gr_handler;
  nsp_gr_handler = f;
  return old;
}

void __nsp_gr_reset_handler(void)
{
  nsp_gr_handler = nsp_gr_handler_none;
}

/*
 * sets a specific handler which is called on a delete event for window 
 * win_num.
 */ 

/* add handlers for delete action */

static void nsp_gr_deletegwin_handler_none (win)int win; {};

void __nsp_gr_deletegwin_handler_sci (int win)
{
  static char buf[256];
  BCG *bcg= window_list_search(win);
  if ( bcg != NULL && strlen(bcg->EventHandler)!=0) {
    sprintf(buf,"%s(%d,0,0,-1000)",bcg->EventHandler,win);
    enqueue_nsp_command(buf);
  }
}

static Scig_deletegwin_handler nsp_gr_deletegwin_handler = nsp_gr_deletegwin_handler_none;

Scig_deletegwin_handler __nsp_gr_set_deletegwin_handler(Scig_deletegwin_handler f)
{
  Scig_deletegwin_handler old = nsp_gr_deletegwin_handler;
  nsp_gr_deletegwin_handler = f;
  return old;
}

void __nsp_gr_reset_deletegwin_handler(void) 
{
  nsp_gr_deletegwin_handler = nsp_gr_deletegwin_handler_none;
}


/**
 * nsp_gr_delete:
 * @winid: graphic window number.
 * 
 * Delete graphic window @win_num. and associated data.
 * 
 **/

void nsp_gr_delete(int winid) 
{
  BCG *Xgc;
  if ( (Xgc= window_list_search(winid)) == NULL) return;
  Xgc->actions->delete(Xgc);
}

/**
 * nsp_gr_export:
 * @fname: file name;
 * @winid: graphic window number.
 * @color: b&w (0) or colour export (1),
 * @driver: driver to use for output. 
 * @option: 
 *
 * export recorded graphics to file @fname 
 * 
 **/

void nsp_gr_export(const char *fname, int winid, int color,const char *driver,char option)
{
  BCG *Xgc;
  if ( (Xgc= window_list_search(winid)) == NULL) return;
  int sc = color;
  if ( color == -1 )  getcolordef(&sc);
  Xgc->actions->tops(Xgc,sc,fname,driver,option);
}

/**
 * nsp_gr_raise: 
 * @win_num: graphic window number.
 * 
 * raises window @win_num.
 */ 

void nsp_gr_new_raise(int win_num)
{
  BCG *Xgc;
  if ((Xgc= window_list_search(win_num)) == NULL) return;
  Xgc->graphic_engine->xselgraphic(Xgc);
}

/**
 * nsp_gr_change: 
 * @win_num: graphic window number.
 * 
 * set window @win_num as the current graphic window. 
 * 
 * return value: the former current graphic window.
 */ 

int nsp_gr_new_change(int win_num)
{
  BCG *Xgc = check_graphic_window();
  if ( Xgc != NULL ) 
    {
      if ( Xgc->CurWindow != win_num) 
	Xgc->graphic_engine->xset_curwin(win_num,TRUE);
      return Xgc->CurWindow; 
    }
  else 
    return -1; 
}


/**
 * nsp_gr_set_graphic_eventhandler:
 * @win_num: 
 * @name: 
 * @ierr: 
 * 
 * FIXME: Feature to be removed: Used to set the EventHandler field of win_num properties 
 * this is to be changed one day. 
 **/

void nsp_gr_set_graphic_eventhandler(int *win_num,char *name,int *ierr)
{  
  BCG *SciGc;
  /*ButtonPressMask|PointerMotionMask|ButtonReleaseMask|KeyPressMask */
  *ierr = 0;
  SciGc = window_list_search(*win_num);
  if ( SciGc ==  NULL ) {*ierr=1;return;}
  strncpy(SciGc->EventHandler,name,NAME_MAXL);
}


/*-------------------------------------------------------
 * set of actions that can be performed on a Xgc. The 
 * next functions are static and inserted in Xgc->actions
 *
 *-------------------------------------------------------*/ 

/**
 * nsp_gc_delete:
 * @Xgc: a graphic context.
 * 
 * Delete graphic window @win_num. and associated data.
 * 
 **/

static void nsp_gc_delete(BCG *Xgc)
{
  nsp_gr_deletegwin_handler(Xgc->CurWindow);
  Xgc->graphic_engine->delete_window(Xgc,Xgc->CurWindow);
}

/**
 * nsp_gc_replay: 
 * @Xgc: a graphic context
 * 
 * redraws the recorded graphics associated to graphic window @win_num.
 */

static void nsp_gc_replay(BCG *Xgc)
{
  Xgc->graphic_engine->invalidate(Xgc,NULL);
  Xgc->graphic_engine->process_updates(Xgc);
}

/**
 * nsp_gc_expose: 
 * @Xgc: a graphic context.
 * 
 * Used to deal with an expose event. If the graphic window 
 * is in pixmap mode we can perform a wshow 
 * else we perform a sgig_replay. 
 */

static void nsp_gc_expose(BCG *Xgc)
{
  int pix = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  if ( pix == 0) 
    {
      Xgc->graphic_engine->invalidate(Xgc,NULL);
      Xgc->graphic_engine->process_updates(Xgc);
    }
  else
    {
      Xgc->graphic_engine->xset_show(Xgc);    
    }
}

/**
 * nsp_gc_resize:
 * @Xgc: a graphic context.
 * 
 * Redraws graphic window @win_num  after resizing. 
 */ 


static void nsp_gc_resize(BCG *Xgc)
{
  Xgc->graphic_engine->pixmap_resize(Xgc);
  Xgc->graphic_engine->invalidate(Xgc,NULL);
  Xgc->graphic_engine->process_updates(Xgc);
}


/**
 * nsp_gc_resize_pixmap:
 * @Xgc: a graphic context.
 * 
 * resize the pixmap associated to graphic window @win_num.
 */ 

static void nsp_gc_resize_pixmap(BCG *Xgc)
{
  Xgc->graphic_engine->pixmap_resize(Xgc);
}

/**
 * nsp_gc_erase:
 * @Xgc: a graphic context.
 * 
 * clears the graphic window @win_num and the associated 
 * recorded data. 
 */ 

static void  nsp_gc_erase(BCG *Xgc)
{
  Xgc->graphic_engine->tape_clean_plots(Xgc,Xgc->CurWindow);
  Xgc->graphic_engine->invalidate(Xgc,NULL);
  Xgc->graphic_engine->process_updates(Xgc);
}


/**
 * nsp_gc_tops:
 * @Xgc: a graphic context.
 * @colored: b&w (0) or colour export (1),
 * @bufname: file name;
 * @driver: driver to use for output. 
 *
 * Sends recorded graphics to file @bufname 
 * using syntax described by @driver ("Pos","Fig",...).
 * 
 */ 

extern BCG ScilabGCPos ; /* sans doute à changer FIXME XXX */
extern BCG ScilabGCXfig ;
extern Gengine Pos_gengine, XFig_gengine ; 

extern int nsp_cairo_export(BCG *Xgc,int win_num,int colored, const char *bufname,const char *driver,char option);



static void nsp_gc_tops(BCG *Xgc, int colored,const char *bufname,const char *driver,char option)
{
  int wdim[2],*wdim_p=NULL;
  BCG *Ggc;
  int zero=0,un=1;
  if ( strcmp(driver,"Pos")==0 ) 
    {
      Ggc = &ScilabGCPos;
      Ggc->graphic_engine = &Pos_gengine ; 
    }
  else if ( strcmp(driver,"Fig")==0 ) 
    {
      Ggc = &ScilabGCXfig;
      Ggc->graphic_engine = &XFig_gengine ; 
    }
  else 
    {
      /* Try to switch to export via cairo. 
       */
      int rep = nsp_cairo_export(Xgc,Xgc->CurWindow,colored,bufname,driver,option);
      if ( rep == OK ) 
	{
	  nsp_gr_buzy = 0;
	  return ; 
	}
      Sciprintf("Unknow driver %s using Pos\n",driver);
      Ggc = &ScilabGCPos;
      Ggc->graphic_engine = &Pos_gengine ; 
    }
  if ( option == 'k') 
    {
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      wdim_p = wdim;
    }
  Ggc->graphic_engine->initgraphic(bufname,&Xgc->CurWindow,wdim_p,NULL,NULL,NULL,option,NULL);
  if (colored==1) 
    Ggc->graphic_engine->xset_usecolor(Ggc,un);
  else
    Ggc->graphic_engine->xset_usecolor(Ggc,zero);
  Ggc->figure = Xgc->figure ; 
  xgc_reset_scales_to_default(Ggc);
  Ggc->graphic_engine->tape_replay(Ggc,NULL);
  Ggc->figure = NULL ; 
  Ggc->graphic_engine->xend(Xgc);
}


/**
 * nsp_gc_2dzoom: 
 * @Xgc: a graphic context.
 * 
 * zoom the graphics of graphic window @win_num.
 */ 

static void nsp_gc_2dzoom(BCG *Xgc)
{
  double bbox[4];
  int ibbox[4];
  static int nsp_gc_buzy_zoom = 0;
  nsp_gc_buzy_zoom =1;
  if ( Xgc == NULL) return ;
  zoom_get_rectangle(Xgc,bbox,ibbox);
  nsp_figure_zoom(Xgc,ibbox);
  Xgc->graphic_engine->process_updates(Xgc);
  nsp_gc_buzy_zoom = 0;
}

/**
 * nsp_gc_unzoom: 
 * @Xgc: a graphic context.
 * 
 * Unzoom the graphics of graphic window @win_num.
 */ 

static void  nsp_gc_unzoom(BCG *Xgc)
{
  if ( Xgc == NULL || Xgc->figure == NULL ) return ;
  /* unzoom and invalidate axes */
  nsp_figure_unzoom((NspGraphic *) Xgc->figure);
  Xgc->graphic_engine->process_updates(Xgc);
}


/**
 * nsp_gc_3drot: 
 * @Xgc: a graphic context.
 * 
 * Rotation of 3d plots of graphic window @win_num.
 */ 


static void nsp_gc_3drot(BCG *Xgc)
{
  nsp_3d_rotation(Xgc);
}


/**
 * nsp_gc_sel: 
 * @Xgc: a graphic context.
 * 
 * selects window @win_num as the current graphic window.
 */ 

static void nsp_gc_sel(BCG *Xgc)
{
  set_graphic_window(Max(Xgc->CurWindow,0)) ;
}

/**
 * nsp_gc_raise: 
 * @Xgc: a graphic context.
 * 
 * raises window @win_num.
 */ 

static void nsp_gc_raise(BCG *Xgc)
{
  Xgc->graphic_engine->xselgraphic(Xgc);
}

/**
 * nsp_gc_change: 
 * @Xgc: a graphic context.
 * 
 * set window @win_num as the current graphic window. 
 * 
 * return value: the former current graphic window.
 */ 

static int nsp_gc_change(BCG *Xgc)
{
  BCG *Cgc = check_graphic_window();
  if ( Cgc != NULL ) 
    {
      if ( Cgc->CurWindow != Xgc->CurWindow) 
	Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,TRUE);
      return Cgc->CurWindow; 
    }
  else 
    return -1; 
}

/**
 * nsp_gc_savesg: 
 * @Xgc: a graphic context.
 * @filename: a filename 
 * 
 * save graphic data from graphic window @win_num to file @filename.
 */ 

static int nsp_gc_savesg(BCG *Xgc,const char *filename )
{
  NspFile *F;
  /* expand keys in path name result in buf */
  if ( Xgc == NULL || Xgc->figure == NULL) return FAIL;
  if (( F =nsp_file_open_xdr_w(filename)) == NULLSCIFILE) return FAIL;
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
 * nsp_gr_loadsg: 
 * @Xgc: a graphic context.
 * @filename: a filename 
 * 
 * graphics reloaded from file @filename are displayed on graphic window 
 * @win_num.
 */ 

static int nsp_gc_loadsg(BCG *Xgc,const char *filename)
{
  NspFile *F;
  NspObject *O; 
  int cur;
  cur = Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,FALSE);
  if (( F =nsp_file_open_xdr_r(filename)) == NULLSCIFILE) 
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
  /*
   * use the given figure in Xgc 
   */
  Xgc->graphic_engine->xset_curwin(cur,FALSE);
  return OK;
}



/**
 * zoom_get_rectangle:
 * @Xgc: 
 * @bbox: 
 * @ibbox: 
 * 
 * A version for drivers who do not have Xor mode 
 * we have to redraw while acquiring the zoom rectangle 
 * we could also try to keep the graphic in a backing store 
 * pixmap. 
 *
 * XXXX : a revoir en regardant ce qui est fait dans 
 *        diagram pour minimiser les redessins.
 *
 * 
 **/

static void zoom_get_rectangle(BCG *Xgc,double *bbox, int *ibbox)
{
  /* Using the mouse to get the new rectangle to fix boundaries */
  int th,pixmode,alumode,color,style,fg;
  int ibutton,imask,iwait=FALSE,istr=0;
  double x0,y0,x,y,xl,yl;
  int    ix0,iy0,ix,iy,ixl,iyl;
  if ( Xgc == NULL ) return; 
  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  alumode = Xgc->graphic_engine->xget_alufunction(Xgc);
  th = Xgc->graphic_engine->xget_thickness(Xgc);
  color= Xgc->graphic_engine->xget_pattern(Xgc);
  style = Xgc->graphic_engine->xget_dash(Xgc);
  fg    = Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_thickness(Xgc,1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,fg);
  nsp_set_cursor(Xgc,GDK_TOP_LEFT_CORNER );
  Xgc->graphic_engine->xclick(Xgc,"one",&ibutton,&imask,&ix0,&iy0,iwait,FALSE,FALSE,FALSE,istr);
  scale_i2f(Xgc->scales,&x0,&y0,&ix0,&iy0,1);
  x=x0;y=y0;
  ix=ix0;iy=iy0;
  ibutton=-1;
  while ( ibutton == -1 ) 
    {
      double rect[4]= {Min(x0,x),Max(y0,y),Abs(x0-x),Abs(y0-y)};
      Xgc->graphic_engine->clearwindow(Xgc);    
      rect2d_f2i(Xgc->scales,rect,Xgc->zrect,1);
      Xgc->graphic_engine->invalidate(Xgc,NULL);
      nsp_set_cursor(Xgc,GDK_BOTTOM_RIGHT_CORNER);
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ixl, &iyl,iwait,TRUE,TRUE,FALSE);
      scale_i2f(Xgc->scales,&xl,&yl,&ixl,&iyl,1);
      x=xl;y=yl;
      ix=ixl;iy=iyl;
    }
  nsp_set_cursor(Xgc,-1);
  /* Back to the default driver which must be Rec and redraw the recorded
   * graphics with the new scales 
   */
  bbox[0]=Min(x0,x);
  bbox[1]=Min(y0,y);
  bbox[2]=Max(x0,x);
  bbox[3]=Max(y0,y);
  ibbox[0]=Min(ix0,ix);
  ibbox[1]=Min(iy0,iy);
  ibbox[2]=Max(ix0,ix);
  ibbox[3]=Max(iy0,iy);

  /* disable zrect */
  Xgc->zrect[2]=   Xgc->zrect[3]=0;
  Xgc->graphic_engine->xset_thickness(Xgc,th);
  Xgc->graphic_engine->xset_dash(Xgc,style);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  Xgc->graphic_engine->xinfo(Xgc," ");
  Xgc->graphic_engine->invalidate(Xgc,NULL);
}


