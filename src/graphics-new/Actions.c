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


#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/command.h"

static int scig_buzy = 0;

/*
 * the functions in this file are called from 
 * callback ( see jpc_SGraph.c ) for the XWindow version 
 *
 */

/*
 * sets a specific draw handler for a graphic window 
 * which is called by tape_replay. 
 */ 

int scig_handler_none(BCG *Xgc,int win_num) {return win_num;}

Scig_handler scig_handler = scig_handler_none;

Scig_handler set_scig_handler(Scig_handler f)
{
  Scig_handler old = scig_handler;
  scig_handler = f;
  return old;
}

void reset_scig_handler(void)
{
  scig_handler = scig_handler_none;
}

/*
 * sets a specific handler which is called on a delete event for window 
 * win_num.
 */ 

/* add handlers for delete action */

void scig_deletegwin_handler_none (win)int win; {};

void scig_deletegwin_handler_sci (int win)
{
  static char buf[256];
  BCG *bcg= window_list_search(win);
  if ( bcg != NULL && strlen(bcg->EventHandler)!=0) {
    sprintf(buf,"%s(%d,0,0,-1000)",bcg->EventHandler,win);
    enqueue_nsp_command(buf);
  }
}

static Scig_deletegwin_handler scig_deletegwin_handler = scig_deletegwin_handler_none;

Scig_deletegwin_handler set_scig_deletegwin_handler(Scig_deletegwin_handler f)
{
  Scig_deletegwin_handler old = scig_deletegwin_handler;
  scig_deletegwin_handler = f;
  return old;
}

void reset_scig_deletegwin_handler(void) 
{
  scig_deletegwin_handler = scig_deletegwin_handler_none;
}


/**
 * scig_delete:
 * @winid: graphic window number.
 * 
 * Delete graphic window @win_num. and associated data.
 * 
 **/

void scig_delete(int winid) 
{
  BCG *Xgc;
  if ( (Xgc= window_list_search(winid)) == NULL) return;
  scig_deletegwin_handler(winid);
  Xgc->graphic_engine->delete_window(Xgc,winid);
}

/**
 * scig_replay: 
 * @win_num: graphic window number.
 * 
 * redraws the recorded graphics associated to graphic window @win_num.
 */

void scig_replay(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc= window_list_search(win_num)) == NULL) return;
  if ( Xgc->record_flag != TRUE ) return ;
  scig_buzy =1;
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_replay(Xgc,win_num);
  scig_buzy=0;
}

/**
 * scig_expose: 
 * @win_num: graphic window number.
 * 
 * Used to deal with an expose event. If the graphic window 
 * is in pixmap mode we can perform a wshow 
 * else we perform a sgig_replay. 
 */

void scig_expose(int win_num)
{
  BCG *Xgc;
  int pix;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc= window_list_search(win_num)) == NULL) return;
  scig_buzy =1;
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
  scig_buzy = 0;
}

/**
 * scig_resize:
 * @win_num: graphic window number.
 * 
 * Redraws graphic window @win_num  after resizing. 
 */ 


void scig_resize(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc = window_list_search(win_num)) == NULL) return;
  scig_buzy =1;
  Xgc->graphic_engine->pixmap_resize(Xgc);
  Xgc->graphic_engine->clearwindow(Xgc);    
  Xgc->graphic_engine->tape_replay(Xgc,win_num);
  scig_buzy = 0;
}


/**
 * scig_resize_pixmap:
 * @win_num: graphic window number.
 * 
 * resize the pixmap associated to graphic window @win_num.
 */ 

void scig_resize_pixmap(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc= window_list_search(win_num)) == NULL) return;
  scig_buzy =1;
  Xgc->graphic_engine->pixmap_resize(Xgc);
  scig_buzy = 0;
}

/**
 * scig_erase:
 * @win_num: graphic window number.
 * 
 * clears the graphic window @win_num and the associated 
 * recorded data. 
 */ 

void  scig_erase(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc=window_list_search(win_num)) == NULL) return;
  scig_buzy =1;
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_clean_plots(Xgc,win_num);
  scig_buzy = 0;
}


/**
 * scig_tops:
 * @win_num: graphic window number.
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

extern int nsp_cairo_export(BCG *Xgc,int win_num,int colored, const char *bufname,char *driver,char option);

void scig_tops(int win_num, int colored, char *bufname, char *driver,char option)
{
  int wdim[2],*wdim_p=NULL;
  BCG *Xgc,*Ggc;
  int zero=0,un=1;
  if ( scig_buzy  == 1 ) return ;
  if ((Xgc= window_list_search(win_num)) == NULL) return;

  scig_buzy =1;
  
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
      int rep = nsp_cairo_export(Xgc,win_num,colored,bufname,driver,option);
      if ( rep == OK ) 
	{
	  scig_buzy = 0;
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
  Ggc->graphic_engine->initgraphic(bufname,&win_num,wdim_p,NULL,NULL,NULL,option);
  if (colored==1) 
    Ggc->graphic_engine->xset_usecolor(Xgc,un);
  else
    Ggc->graphic_engine->xset_usecolor(Xgc,zero);
  Ggc->record_flag = TRUE ;
  Ggc->plots = Xgc->plots ; 
  xgc_reset_scales_to_default(Ggc);
  Ggc->graphic_engine->tape_replay(Ggc,win_num);
  Ggc->plots = NULL ; 
  Ggc->record_flag = FALSE ;
  Ggc->graphic_engine->xend(Xgc);
  scig_buzy = 0;
}

void scig_export(char *fname, int iwin, int color, char *driver,char option)
{
  int sc;
  if ( color == -1 ) 
    getcolordef(&sc);
  else 
    sc= color;
  scig_tops(iwin,sc,fname,driver,option);
}


/**
 * scig_2dzoom: 
 * @win_num: graphic window number.
 * 
 * zoom the graphics of graphic window @win_num.
 */ 

static int scig_buzy_zoom = 0;

void scig_2dzoom(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy_zoom == 1 ) return ;
  if ( (Xgc=window_list_search(win_num)) == NULL) return;
  scig_buzy_zoom =1;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Zoom works only with the Rec driver");
    }
  else 
    {
      zoom(Xgc);
    }
  scig_buzy_zoom = 0;
}

/**
 * scig_unzoom: 
 * @win_num: graphic window number.
 * 
 * Unzoom the graphics of graphic window @win_num.
 */ 

void  scig_unzoom(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc = window_list_search(win_num)) == NULL) return;
  scig_buzy =1;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"UnZoom works only with the Rec driver ");
    }
  else 
    {
      unzoom(Xgc);
    }
  scig_buzy = 0;
}


/**
 * scig_3drot: 
 * @win_num: graphic window number.
 * 
 * Rotation of 3d plots of graphic window @win_num.
 */ 


void scig_3drot(int win_num)
{
  BCG *Xgc;
  if ((Xgc= window_list_search(win_num)) == NULL) return;
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
 * scig_sel: 
 * @win_num: graphic window number.
 * 
 * selects window @win_num as the current graphic window.
 */ 


void scig_sel(int win_num)
{
  set_graphic_window(Max(win_num,0)) ;
}

/**
 * scig_raise: 
 * @win_num: graphic window number.
 * 
 * raises window @win_num.
 */ 

void scig_raise(int win_num)
{
  BCG *Xgc;
  if ((Xgc= window_list_search(win_num)) == NULL) return;
  Xgc->graphic_engine->xselgraphic(Xgc);
}

/**
 * scig_change: 
 * @win_num: graphic window number.
 * 
 * set window @win_num as the current graphic window. 
 * 
 * return value: the former current graphic window.
 */ 

int scig_change(int win_num)
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
 * scig_loadsg: 
 * @win_num: graphic window number.
 * @filename: a filename 
 * 
 * graphics reloaded from file @filename are displayed on graphic window 
 * @win_num.
 */ 

void scig_loadsg(int win_num, char *filename)
{
  BCG *Xgc;
  int cur;
  if ( scig_buzy  == 1 ) return ;
  scig_buzy =1;
  Xgc=check_graphic_window();
  cur = Xgc->graphic_engine->xset_curwin(win_num,FALSE);
  tape_load(Xgc,filename);
  Xgc->graphic_engine->xset_curwin(cur,FALSE);
  scig_buzy = 0;
}

/**
 * scig_savesg: 
 * @filename: a filename 
 * @win_num: graphic window number.
 * 
 * save graphic data from graphic window @win_num to file @filename.
 */ 

void scig_savesg(char *filename, int win_num)
{
  BCG *Xgc;
  if ( (Xgc = window_list_search(win_num)) == NULL) return;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"save works only with the Rec driver");
      return;
    }
  tape_save(Xgc,filename,win_num);
}

/**
 * nsp_set_graphic_eventhandler:
 * @win_num: 
 * @name: 
 * @ierr: 
 * 
 * FIXME: Feature to be removed: Used to set the EventHandler field of win_num properties 
 * this is to be changed one day. 
 **/

void nsp_set_graphic_eventhandler(int *win_num,char *name,int *ierr)
{  
  BCG *SciGc;
  /*ButtonPressMask|PointerMotionMask|ButtonReleaseMask|KeyPressMask */
  *ierr = 0;
  SciGc = window_list_search(*win_num);
  if ( SciGc ==  NULL ) {*ierr=1;return;}
  strncpy(SciGc->EventHandler,name,NAME_MAXL);
}

