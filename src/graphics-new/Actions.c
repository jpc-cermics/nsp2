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
  int cur;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name); /* */
  if ( (nsp_gengine1.get_driver()) != 'R') nsp_gengine1.set_driver("Rec");
  cur = nsp_gengine->xset_curwin(win_num,FALSE);

  Xgc=check_graphic_window();
  nsp_gengine->clearwindow(Xgc);    
  /* XXXX scig_handler(win_num); */
  nsp_gengine->tape_replay(Xgc,win_num);
  nsp_gengine->xset_curwin(cur,FALSE);
  nsp_gengine1.set_driver(name);
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
  int cur,pix;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  cur = nsp_gengine->xset_curwin(win_num,FALSE);
  Xgc=check_graphic_window();

  pix = nsp_gengine->xget_pixmapOn(Xgc);
  if ( pix == 0) 
    {
      if ( (nsp_gengine1.get_driver()) != 'R')nsp_gengine1.set_driver("Rec");
      nsp_gengine->clearwindow(Xgc);    
      /* XXXX scig_handler(win_num); */
      nsp_gengine->tape_replay(Xgc,win_num);
      cur = nsp_gengine->xset_curwin(cur,FALSE);
      nsp_gengine1.set_driver(name);
    }
  else
    {
      nsp_gengine->xset_show(Xgc);    
      cur = nsp_gengine->xset_curwin(cur,FALSE);
      nsp_gengine1.set_driver(name);
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
  int cur;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  if ( (nsp_gengine1.get_driver()) !='R') nsp_gengine1.set_driver("Rec");
  cur = nsp_gengine->xset_curwin(win_num,FALSE);
  Xgc=check_graphic_window();
  nsp_gengine->pixmap_resize(Xgc);
  nsp_gengine->clearwindow(Xgc);    
  /* XXXX scig_handler(win_num); */
  nsp_gengine->tape_replay(Xgc,win_num);
  cur = nsp_gengine->xset_curwin(cur,FALSE);
  nsp_gengine1.set_driver(name);
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
  int cur;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  nsp_gengine1.set_driver("Int");
  cur = nsp_gengine->xset_curwin(win_num,FALSE);
  Xgc=check_graphic_window();
  nsp_gengine->pixmap_resize(Xgc);
  nsp_gengine->xset_curwin(cur,FALSE);
  nsp_gengine1.set_driver(name);
  scig_buzy = 0;
}

/**
 * scig_resize_pixmap:
 * @win_num: graphic window number.
 * 
 * clears the graphic window @win_num and the associated 
 * recorded data. 
 */ 

void  scig_erase(int win_num)
{
  BCG *Xgc;
  int cur;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  /* nothing to do if window does not exists */
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  if ( (nsp_gengine1.get_driver()) !='R') nsp_gengine1.set_driver("Rec");
  cur = nsp_gengine->xset_curwin(win_num,FALSE);
  Xgc=check_graphic_window();
  nsp_gengine->clearwindow(Xgc);
  nsp_gengine->tape_clean_plots(Xgc,win_num);
  nsp_gengine->xset_curwin(cur,FALSE);
  nsp_gengine1.set_driver(name);
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

void scig_tops(int win_num, int colored, char *bufname, char *driver)
{
  BCG *Xgc;
  char name[4];
  int zero=0,un=1;
  int cur, screenc;
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  cur = nsp_gengine->xset_curwin(win_num,FALSE);
  nsp_gengine1.set_driver(driver);
  nsp_gengine->initgraphic(bufname,&win_num);
  if (colored==1) 
    nsp_gengine->xset_usecolor(Xgc,un);
  else
    nsp_gengine->xset_usecolor(Xgc,zero);
  getcolordef(&screenc);
  /** we set the default screen color to the value of colored 
    because we don't want that recorded events such as xset("default") could 
    change the color status .
    and we set the UseColorFlag to 1 not to replay xset("use color",..) events 
    **/
  setcolordef(colored);
  UseColorFlag(1);
  /* XXXX scig_handler(win_num); */
  nsp_gengine->tape_replay(Xgc,win_num);
  /** back to default values **/
  UseColorFlag(0);
  setcolordef(screenc);
  nsp_gengine->xend(Xgc);
  nsp_gengine1.set_driver(name);
  nsp_gengine->xset_curwin(cur,FALSE);
  /* to force a reset in the graphic scales */
  SwitchWindow(&cur);
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
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  if ( (nsp_gengine1.get_driver()) !='R') 
    {
      nsp_gengine->xinfo(Xgc,"Zoom works only with the Rec driver");
    }
  else 
    {
      int cur = nsp_gengine->xset_curwin(win_num,FALSE);
      Xgc=check_graphic_window();
      zoom(Xgc);
      nsp_gengine->xset_curwin(cur,FALSE);
      nsp_gengine1.set_driver(name);
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
  int cur;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  if ( window_list_search(win_num) == NULL) return;
  scig_buzy =1;
  nsp_gengine1.get_driver_name(name);
  if ( (nsp_gengine1.get_driver()) !='R') 
    {
      nsp_gengine->xinfo(Xgc,"UnZoom works only with the Rec driver ");
    }
  else 
    {
      cur = nsp_gengine->xset_curwin(win_num,FALSE);
      Xgc=check_graphic_window();
      unzoom(Xgc);
      nsp_gengine->xset_curwin(cur,FALSE);
      nsp_gengine1.set_driver(name);
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
  int cur;
  char name[4];
  if ( scig_buzy  == 1 ) return ;
  scig_buzy =1;
  if ( window_list_search(win_num) == NULL) return;
  nsp_gengine1.get_driver_name(name);
  if ( (nsp_gengine1.get_driver()) !='R') 
    {
      nsp_gengine->xinfo(Xgc,"Rot3D works only with the Rec driver");
    }
  else 
    {
      cur = nsp_gengine->xset_curwin(win_num,FALSE);
      Xgc=check_graphic_window();
      I3dRotation(Xgc);
      nsp_gengine->xset_curwin(cur,FALSE);
      nsp_gengine1.set_driver(name);
    }
  scig_buzy = 0;
}


/**
 * scig_sel: 
 * @win_num: graphic window number.
 * 
 * selects window @win_num as the current graphic window.
 */ 

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
  nsp_gengine->xset_curwin(win_num,TRUE);
  Xgc=check_graphic_window();
  nsp_gengine->xselgraphic(Xgc);
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
  
  int cur = nsp_gengine->xget_curwin();
  if ( cur != win_num) nsp_gengine->xset_curwin(win_num,TRUE);
  return cur; 
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
  cur = nsp_gengine->xset_curwin(win_num,FALSE);
  Xgc=check_graphic_window();
  tape_load(Xgc,filename);
  nsp_gengine->xset_curwin(cur,FALSE);
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


