/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

static int scig_buzy = 0;

/*
 * the functions in this file are called from 
 * callback ( see jpc_SGraph.c ) for the XWindow version 
 */

/**
 * sets a specific draw handler for a graphic window 
 * which is called by tape_replay. 
 */ 

int scig_handler_none(int win_num) {return win_num;}

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

/**
 * sets a specific handler which is called on a delete event for window 
 * win_num.
 */ 

/* add handlers for delete action */

static void scig_deletegwin_handler_none (win)int win; {};

static void scig_deletegwin_handler_sci (int win)
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

/* delete action */

void scig_delete(int winid) 
{
  scig_erase(winid);
  scig_deletegwin_handler(winid);
  DeleteSGWin(winid);
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

extern Gengine XFig_gengine, Pos_gengine;

void scig_tops(int win_num, int colored, char *bufname, char *driver)
{
  BCG *Xgc;
  Gengine *win_graphic_engine=NULL,*graphic_engine = NULL;
  int zero=0,un=1;
  if ( scig_buzy  == 1 ) return ;
  if ((Xgc= window_list_search(win_num)) == NULL) return;

  scig_buzy =1;
  
  if ( strcmp(driver,"Pos")==0 ) 
    {
      graphic_engine = &Pos_gengine;
    }

  graphic_engine->initgraphic(bufname,&win_num);
  if (colored==1) 
    graphic_engine->xset_usecolor(Xgc,un);
  else
    graphic_engine->xset_usecolor(Xgc,zero);
  
  win_graphic_engine = Xgc->graphic_engine; 
  Xgc->graphic_engine = graphic_engine;
  graphic_engine->tape_replay(Xgc,win_num);
  Xgc->graphic_engine = win_graphic_engine;

  graphic_engine->xend(Xgc);
  scig_buzy = 0;
}

void scig_export(char *fname, int iwin, int color, char *driver)
{
  int sc;
  if ( color == -1 ) 
    getcolordef(&sc);
  else 
    sc= color;
  scig_tops(iwin,sc,fname,driver);
}


/**
 * scig_2dzoom: 
 * @win_num: graphic window number.
 * 
 * zoom the graphics of graphic window @win_num.
 */ 

void scig_2dzoom(int win_num)
{
  BCG *Xgc;
  if ( scig_buzy  == 1 ) return ;
  if ( (Xgc=window_list_search(win_num)) == NULL) return;
  scig_buzy =1;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Zoom works only with the Rec driver");
    }
  else 
    {
      zoom(Xgc);
    }
  scig_buzy = 0;
}

/**
 * scig_unzoom: 
 * @win_num: graphic window number.
 * 
 * Unzoom the graphics of graphic window @win_num.
 */ 

void   scig_unzoom(int win_num)
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
  if ( scig_buzy  == 1 ) return ;
  scig_buzy =1;
  if ((Xgc= window_list_search(win_num)) == NULL) return;
  if ( Xgc->record_flag != TRUE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"Rot3D works only with the Rec driver");
    }
  else 
    {
      I3dRotation(Xgc);
    }
  scig_buzy = 0;
}


/**
 * scig_sel: 
 * @win_num: graphic window number.
 * 
 * selects window @win_num as the current graphic window.
 */ 

extern Gengine *nsp_gengine ; /* XXXXX */

void scig_sel(int win_num)
{
  nsp_gengine->xset_curwin(win_num,TRUE);
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
  cur = Xgc->graphic_engine->xset_curwin(win_num,FALSE);
  Xgc=check_graphic_window();
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
  if ( window_list_search(win_num) == NULL) return;
  Xgc=check_graphic_window();
  tape_save(Xgc,filename,win_num);
}


