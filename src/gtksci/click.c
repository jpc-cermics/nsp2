/*------------------------------------------------------------------------
 *    Copyright (C) 2001-2003 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <stdio.h>			/* For the Syntax message */
#include <signal.h>
#include <string.h>
#include <gtk/gtk.h>
#include "nsp/machine.h"
#include "nsp/command.h"
#include "All-extern.h"
#include "nsp/graphics/Graphics.h" 

/*---------------------------------------------------------
 * Mouse events: Note that this code is not specific to Gtk 
 *               XXXX could be moved in graphics
 * 
 * when not inside the xclick function 
 * the default behaviour is to store mouse event in a (short) queue. 
 * An event handler can be set through the use of the 
 * set_scig_click_handler to change this default behaviour: 
 * the handler returns 1 if he takes care of the click and returns 0 if not 
 * (then the queue is used). 
 * the event queue is used when entering the xclick function. 
 *---------------------------------------------------------*/

int scig_click_handler_none (int win,int x,int y,int ibut,
			     int motion,int release) 
{return 0;}


int scig_click_handler_sci (int win,int x,int y,int ibut,int motion,int release)
{
  static char buf[256];
  BCG *SciGc;
  SciGc = window_list_search(win);
  if (strlen(SciGc->EventHandler)!=0) {
    sprintf(buf,"%s(%d,%d,%d,%d)",SciGc->EventHandler,win,x,y,ibut);
    enqueue_nsp_command(buf);
    return 1;}
  else
    return 0;
}

static Scig_click_handler scig_click_handler = scig_click_handler_sci;

Scig_click_handler set_scig_click_handler( Scig_click_handler f) 
{
  Scig_click_handler old = scig_click_handler;
  scig_click_handler = f;
  return old;
}

/* XXXX should be removed */
void reset_scig_click_handler() 
{
  scig_click_handler = scig_click_handler_none;
}


/*---------------------------------------------------------
 * The queue ....
 *---------------------------------------------------------*/

typedef struct but {
  int win,x,y,ibutton,motion,release;
} But;

#define MaxCB 50
static But ClickBuf[MaxCB];
static int lastc = 0;

int PushClickQueue(int win,int x,int y,int ibut,
		   int motion,int release) 
{
  /* first let a click_handler do the job  */
  if ( scig_click_handler(win,x,y,ibut,motion,release)== 1) return 0;
  /* do not record motion events and release button 
   * this is left for a futur release 
   */
  if ( motion == 1 || release == 1 ) return 0;

  {static int  count=0;fprintf(stderr,"In push %d\n",count++);}
  /* store click event in a queue */
  if ( lastc == MaxCB ) 
    {
      int i;
      for ( i= 1 ; i < MaxCB ; i ++ ) 
	{
	  ClickBuf[i-1]=ClickBuf[i];
	}
      ClickBuf[lastc-1].win = win;
      ClickBuf[lastc-1].x = x;
      ClickBuf[lastc-1].y = y;
      ClickBuf[lastc-1].ibutton = ibut;
      ClickBuf[lastc-1].motion = motion;
      ClickBuf[lastc-1].release = release;
    }
  else 
    {
      ClickBuf[lastc].win = win;
      ClickBuf[lastc].x = x;
      ClickBuf[lastc].y = y;
      ClickBuf[lastc].ibutton = ibut;
      ClickBuf[lastc].motion = motion;
      ClickBuf[lastc].release = release;
      lastc++;
    }
  return(0);
}

int CheckClickQueue(int *win,int *x,int *y,int *ibut)
{
  int i;
  for ( i = 0 ; i < lastc ; i++ )
    {
      int j ;
      if ( ClickBuf[i].win == *win || *win == -1 ) 
	{
	  *win = ClickBuf[i].win;
	  *x= ClickBuf[i].x ;
	  *y= ClickBuf[i].y ;
	  *ibut= ClickBuf[i].ibutton; 
	  for ( j = i+1 ; j < lastc ; j++ ) 
	    {
	      ClickBuf[j-1].win = ClickBuf[j].win ;
	      ClickBuf[j-1].x   = ClickBuf[j].x ;
	      ClickBuf[j-1].y =  ClickBuf[j].y ;
	      ClickBuf[j-1].ibutton = ClickBuf[j].ibutton ;
	      ClickBuf[j-1].motion =  ClickBuf[j].motion ;
	      ClickBuf[j-1].release = ClickBuf[j].release ;
	    }
	  lastc--;
	  return(1);
	}
    }
  return(0);
}

int ClearClickQueue(int win)
{
  int i;
  if ( win == -1 ) 
    {
      lastc = 0;
      return 0;
    }
  for ( i = 0 ; i < lastc ; i++ )
    {
      int j ;
      if ( ClickBuf[i].win == win  ) 
	{
	  for ( j = i+1 ; j < lastc ; j++ ) 
	    {
	      ClickBuf[j-1].win = ClickBuf[j].win ;
	      ClickBuf[j-1].x   = ClickBuf[j].x ;
	      ClickBuf[j-1].y =  ClickBuf[j].y ;
	      ClickBuf[j-1].ibutton = ClickBuf[j].ibutton ;
	      ClickBuf[j-1].motion =  ClickBuf[j].motion ;
	      ClickBuf[j-1].release = ClickBuf[j].release ;
	    }
	  lastc--;
	}
    }
  lastc=0;
  return(0);
}



