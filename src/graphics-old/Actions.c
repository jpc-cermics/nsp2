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
#include "nsp/graphics-old/Graphics.h"
#define PERI_ACTION_PRIVATE 
#include "nsp/graphics-old/actions.h"
#include "nsp/command.h"

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

static int nsp_gr_old_handler_none(BCG *Xgc,int win_num) {return win_num;}

Scig_handler nsp_gr_old_handler = nsp_gr_old_handler_none;

Scig_handler nsp_gr_old_set_handler(Scig_handler f)
{
  Scig_handler old = nsp_gr_old_handler;
  nsp_gr_old_handler = f;
  return old;
}

void nsp_gr_old_reset_handler(void)
{
  nsp_gr_old_handler = nsp_gr_old_handler_none;
}

/*
 * sets a specific handler which is called on a delete event for window 
 * win_num.
 */ 

/* add handlers for delete action */

static void nsp_gr_old_deletegwin_handler_none (win)int win; {};

void nsp_gr_old_deletegwin_handler_sci (int win)
{
  static char buf[256];
  BCG *bcg= window_list_search_old(win);
  if ( bcg != NULL && strlen(bcg->EventHandler)!=0) {
    sprintf(buf,"%s(%d,0,0,-1000)",bcg->EventHandler,win);
    enqueue_nsp_command(buf);
  }
}

static Scig_deletegwin_handler nsp_gr_old_deletegwin_handler = nsp_gr_old_deletegwin_handler_none;

Scig_deletegwin_handler nsp_gr_old_set_deletegwin_handler(Scig_deletegwin_handler f)
{
  Scig_deletegwin_handler old = nsp_gr_old_deletegwin_handler;
  nsp_gr_old_deletegwin_handler = f;
  return old;
}

void nsp_gr_old_reset_deletegwin_handler(void) 
{
  nsp_gr_old_deletegwin_handler = nsp_gr_old_deletegwin_handler_none;
}


/**
 * nsp_gr_old_delete:
 * @winid: graphic window number.
 * 
 * Delete graphic window @win_num. and associated data.
 * 
 **/

void nsp_gr_old_delete(int winid) 
{
  BCG *Xgc;
  if ( (Xgc= window_list_search_old(winid)) == NULL) return;
  nsp_gr_old_deletegwin_handler(winid);
  Xgc->graphic_engine->delete_window(Xgc,winid);
}

/**
 * nsp_gr_old_replay: 
 * @win_num: graphic window number.
 * 
 * redraws the recorded graphics associated to graphic window @win_num.
 */

void nsp_gr_old_replay(int win_num)
{
  BCG *Xgc;
  if ( nsp_gr_buzy  == 1 ) return ;
  if ( (Xgc= window_list_search_old(win_num)) == NULL) return;
  if ( Xgc->record_flag != TRUE ) return ;
  nsp_gr_buzy =1;
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_replay(Xgc,win_num);
  nsp_gr_buzy=0;
}

/**
 * nsp_gr_old_expose: 
 * @win_num: graphic window number.
 * 
 * Used to deal with an expose event. If the graphic window 
 * is in pixmap mode we can perform a wshow 
 * else we perform a sgig_replay. 
 */

void nsp_gr_old_expose(int win_num)
{
  BCG *Xgc;
  int pix;
  if ( nsp_gr_buzy  == 1 ) return ;
  if ( (Xgc= window_list_search_old(win_num)) == NULL) return;
  nsp_gr_buzy =1;
  pix = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  if ( pix == 0) 
    {
      Xgc->graphic_engine->clearwindow(Xgc);    
      Xgc->graphic_engine->tape_replay(Xgc,win_num);
    }
  else
    {
      Xgc->graphic_engine->xset_show(Xgc);    
    }
  nsp_gr_buzy = 0;
}

/**
 * nsp_gr_old_resize:
 * @win_num: graphic window number.
 * 
 * Redraws graphic window @win_num  after resizing. 
 */ 


void nsp_gr_old_resize(int win_num)
{
  BCG *Xgc;
  if ( nsp_gr_buzy  == 1 ) return ;
  if ( (Xgc = window_list_search_old(win_num)) == NULL) return;
  nsp_gr_buzy =1;
  Xgc->graphic_engine->pixmap_resize(Xgc);
  Xgc->graphic_engine->clearwindow(Xgc);    
  Xgc->graphic_engine->tape_replay(Xgc,win_num);
  nsp_gr_buzy = 0;
}


/**
 * nsp_gr_old_resize_pixmap:
 * @win_num: graphic window number.
 * 
 * resize the pixmap associated to graphic window @win_num.
 */ 

void nsp_gr_old_resize_pixmap(int win_num)
{
  BCG *Xgc;
  if ( nsp_gr_buzy  == 1 ) return ;
  if ( (Xgc= window_list_search_old(win_num)) == NULL) return;
  nsp_gr_buzy =1;
  Xgc->graphic_engine->pixmap_resize(Xgc);
  nsp_gr_buzy = 0;
}

/**
 * nsp_gr_old_erase:
 * @win_num: graphic window number.
 * 
 * clears the graphic window @win_num and the associated 
 * recorded data. 
 */ 

void  nsp_gr_old_erase(int win_num)
{
  BCG *Xgc;
  if ( nsp_gr_buzy  == 1 ) return ;
  if ( (Xgc=window_list_search_old(win_num)) == NULL) return;
  nsp_gr_buzy =1;
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_clean_plots(Xgc,win_num);
  nsp_gr_buzy = 0;
}

/**
 * nsp_gr_old_export:
 * @fname: file name;
 * @winid: graphic window number.
 * @color: b&w (0) or colour export (1),
 * @driver: driver to use for output. 
 * @option: 
 *
 * export recorded graphics to file @fname 
 * 
 **/

void nsp_gr_old_export(char *fname, int winid, int color, char *driver,char option)
{
  BCG *Xgc;
  if ( (Xgc= window_list_search_old(winid)) == NULL) return;
  int sc = color;
  if ( color == -1 )  getcolordef(&sc);
  Xgc->actions->tops(Xgc,sc,fname,driver,option);
}


/**
 * nsp_gr_old_2dzoom: 
 * @win_num: graphic window number.
 * 
 * zoom the graphics of graphic window @win_num.
 */ 


void nsp_gr_old_2dzoom(int win_num)
{
  static int nsp_gr_buzy_zoom = 0;
  BCG *Xgc;
  if ( nsp_gr_buzy_zoom == 1 ) return ;
  if ( (Xgc=window_list_search_old(win_num)) == NULL) return;
  nsp_gr_buzy_zoom =1;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Zoom works only with the Rec driver");
    }
  else 
    {
      zoom_old(Xgc);
    }
  nsp_gr_buzy_zoom = 0;
}

/**
 * nsp_gr_old_unzoom: 
 * @win_num: graphic window number.
 * 
 * Unzoom the graphics of graphic window @win_num.
 */ 

void  nsp_gr_old_unzoom(int win_num)
{
  BCG *Xgc;
  if ( nsp_gr_buzy  == 1 ) return ;
  if ( (Xgc = window_list_search_old(win_num)) == NULL) return;
  nsp_gr_buzy =1;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"UnZoom works only with the Rec driver ");
    }
  else 
    {
      unzoom_old(Xgc);
    }
  nsp_gr_buzy = 0;
}


/**
 * nsp_gr_old_3drot: 
 * @win_num: graphic window number.
 * 
 * Rotation of 3d plots of graphic window @win_num.
 */ 


void nsp_gr_old_3drot(int win_num)
{
  BCG *Xgc;
  if ((Xgc= window_list_search_old(win_num)) == NULL) return;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Rot3D works only with the Rec driver");
    }
  else 
    {
      I3dRotation(Xgc);
    }
}


/**
 * nsp_gr_old_sel: 
 * @win_num: graphic window number.
 * 
 * selects window @win_num as the current graphic window.
 */ 


void nsp_gr_old_sel(int win_num)
{
  set_graphic_window_old(Max(win_num,0)) ;
}

/**
 * nsp_gr_old_raise: 
 * @win_num: graphic window number.
 * 
 * raises window @win_num.
 */ 

void nsp_gr_old_raise(int win_num)
{
  BCG *Xgc;
  if ((Xgc= window_list_search_old(win_num)) == NULL) return;
  Xgc->graphic_engine->xselgraphic(Xgc);
}

/**
 * nsp_gr_old_change: 
 * @win_num: graphic window number.
 * 
 * set window @win_num as the current graphic window. 
 * 
 * return value: the former current graphic window.
 */ 

int nsp_gr_old_change(int win_num)
{
  BCG *Xgc = check_graphic_window_old();
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
 * nsp_gr_old_loadsg: 
 * @win_num: graphic window number.
 * @filename: a filename 
 * 
 * graphics reloaded from file @filename are displayed on graphic window 
 * @win_num.
 */ 

void nsp_gr_old_loadsg(int win_num, char *filename)
{
  BCG *Xgc;
  int cur;
  if ( nsp_gr_buzy  == 1 ) return ;
  nsp_gr_buzy =1;
  Xgc=check_graphic_window_old();
  cur = Xgc->graphic_engine->xset_curwin(win_num,FALSE);
  tape_old_load(Xgc,filename);
  Xgc->graphic_engine->xset_curwin(cur,FALSE);
  nsp_gr_buzy = 0;
}

/**
 * nsp_gr_old_savesg: 
 * @filename: a filename 
 * @win_num: graphic window number.
 * 
 * save graphic data from graphic window @win_num to file @filename.
 */ 

void nsp_gr_old_savesg(char *filename, int win_num)
{
  BCG *Xgc;
  if ( (Xgc = window_list_search_old(win_num)) == NULL) return;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"save works only with the Rec driver");
      return;
    }
  tape_old_save(Xgc,filename,win_num);
}

/**
 * nsp_gr_old_set_graphic_eventhandler:
 * @win_num: 
 * @name: 
 * @ierr: 
 * 
 * FIXME: Feature to be removed: Used to set the EventHandler field of win_num properties 
 * this is to be changed one day. 
 **/

void nsp_gr_old_set_graphic_eventhandler(int *win_num,char *name,int *ierr)
{  
  BCG *SciGc;
  /*ButtonPressMask|PointerMotionMask|ButtonReleaseMask|KeyPressMask */
  *ierr = 0;
  SciGc = window_list_search_old(*win_num);
  if ( SciGc ==  NULL ) {*ierr=1;return;}
  strncpy(SciGc->EventHandler,name,NAME_MAXL);
}

/* set of actions that can be performed on a Xgc. 
 */ 

/**
 * nsp_gc_delete:
 * @Xgc: a graphic context.
 * 
 * Delete graphic window @win_num. and associated data.
 * 
 **/

static void nsp_gc_delete(BCG *Xgc)
{
  nsp_gr_old_deletegwin_handler(Xgc->CurWindow);
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
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_replay(Xgc,Xgc->CurWindow);
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
      Xgc->graphic_engine->clearwindow(Xgc);    
      Xgc->graphic_engine->tape_replay(Xgc,Xgc->CurWindow);
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
  Xgc->graphic_engine->clearwindow(Xgc);    
  Xgc->graphic_engine->tape_replay(Xgc,Xgc->CurWindow);
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
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_clean_plots(Xgc,Xgc->CurWindow);
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
extern Gengine Pos_gengine_old, XFig_gengine_old ; 

extern int nsp_cairo_export_old(BCG *Xgc,int win_num,int colored, const char *bufname,const char *driver,char option);



static void nsp_gc_tops(BCG *Xgc, int colored,const char *bufname,const char *driver,char option)
{
  int wdim[2],*wdim_p=NULL;
  BCG *Ggc;
  int zero=0,un=1;
  if ( strcmp(driver,"Pos")==0 ) 
    {
      Ggc = &ScilabGCPos;
      Ggc->graphic_engine = &Pos_gengine_old ; 
    }
  else if ( strcmp(driver,"Fig")==0 ) 
    {
      Ggc = &ScilabGCXfig;
      Ggc->graphic_engine = &XFig_gengine_old ; 
    }
  else 
    {
      /* Try to switch to export via cairo. 
       */
      int rep = nsp_cairo_export_old(Xgc,Xgc->CurWindow,colored,bufname,driver,option);
      if ( rep == OK ) 
	{
	  nsp_gr_buzy = 0;
	  return ; 
	}
      Sciprintf("Unknow driver %s using Pos\n",driver);
      Ggc = &ScilabGCPos;
      Ggc->graphic_engine = &Pos_gengine_old ; 
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
  Ggc->record_flag = TRUE ;
  Ggc->plots = Xgc->plots ; 
  xgc_reset_scales_to_default_old(Ggc);
  Ggc->graphic_engine->tape_replay(Ggc,Xgc->CurWindow);
  Ggc->plots = NULL ; 
  Ggc->record_flag = FALSE ;
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
  static int nsp_gc_buzy_zoom = 0;
  nsp_gc_buzy_zoom =1;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Zoom works only with the Rec driver");
    }
  else 
    {
      zoom_old(Xgc);
    }
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
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"UnZoom works only with the Rec driver ");
    }
  else 
    {
      unzoom_old(Xgc);
    }
}


/**
 * nsp_gc_3drot: 
 * @Xgc: a graphic context.
 * 
 * Rotation of 3d plots of graphic window @win_num.
 */ 


static void nsp_gc_3drot(BCG *Xgc)
{
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Rot3D works only with the Rec driver");
    }
  else 
    {
      I3dRotation(Xgc);
    }
}


/**
 * nsp_gc_sel: 
 * @Xgc: a graphic context.
 * 
 * selects window @win_num as the current graphic window.
 */ 

static void nsp_gc_sel(BCG *Xgc)
{
  set_graphic_window_old(Max(Xgc->CurWindow,0)) ;
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
  BCG *Cgc = check_graphic_window_old();
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

static void nsp_gc_savesg(BCG *Xgc,const char *filename )
{
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"save works only with the Rec driver");
      return;
    }
  tape_old_save(Xgc,filename,Xgc->CurWindow);
}


/**
 * nsp_gr_loadsg: 
 * @Xgc: a graphic context.
 * @filename: a filename 
 * 
 * graphics reloaded from file @filename are displayed on graphic window 
 * @win_num.
 */ 

static void nsp_gc_loadsg(BCG *Xgc,const char *filename)
{
  int cur;
  cur = Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,FALSE);
  tape_old_load(Xgc,filename);
  Xgc->graphic_engine->xset_curwin(cur,FALSE);
}
