/* Nsp
 * Copyright (C) 2001-2006 Jean-Philippe Chancelier Enpc/Cermics
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
 * mouse events 
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <stdio.h>			/* For the Syntax message */
#include <signal.h>
#include <string.h>
#include <gtk/gtk.h>
#include "nsp/machine.h"
#include "nsp/command.h"
#include "nsp/gtksci.h"
#include "nsp/graphics/Graphics.h" 

/**
 * scig_click_handler_none:
 * @win: 
 * @x: 
 * @y: 
 * @ibut: 
 * @motion: 
 * @release: 
 * 
 * empty event recorder.
 * 
 * Return value: 0
 **/

int scig_click_handler_none (int win,int x,int y,int ibut,
			     int motion,int release) 
{return 0;}


/**
 * scig_click_handler_sci:
 * @win: 
 * @x: 
 * @y: 
 * @ibut: 
 * @motion: 
 * @release: 
 * 
 * when not inside the xclick() function 
 * the default behaviour is to store mouse event in a queue. 
 * An event handler can be set through the use of
 * set_scig_click_handler() to change this default behaviour: 
 * the handler returns 1 if he takes care of the click and returns 0 if not 
 * (then the queue is used). 
 * The event queue is used when entering the xclick function. 
 * 
 * 
 * Return value: 0 or 1 
 **/

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

/**
 * set_scig_click_handler:
 * @f: 
 * 
 * changes the default xclick handler
 * 
 * Return value: the previous xclick handler
 **/

static Scig_click_handler scig_click_handler = scig_click_handler_sci;

Scig_click_handler set_scig_click_handler( Scig_click_handler f) 
{
  Scig_click_handler old = scig_click_handler;
  scig_click_handler = f;
  return old;
}

/* should be removed */

void reset_scig_click_handler(void)
{
  scig_click_handler = scig_click_handler_none;
}

/*
 * a queue for click/motion events.
 * each graphic window has its own queue. 
 * see graphics/perigtk/events.c 
 */

int nsp_enqueue(nsp_event_queue *q, nsp_gwin_event *ev)
{
  /* first let a click_handler do the job  */
  if ( scig_click_handler(ev->win,ev->x,ev->y,ev->ibutton,ev->motion,ev->release)== 1) return 0;
  

  /* XXX: do not record motion events and release button 
   *      this is left for a futur release 
   *      gives a pb in scicos to be solved 
   */
  if ( ev->motion == 1 || ev->release == 1 ) return 0; 

  if ( q->in == q->out -1 )  
    {
      /* the queue is full */
      /* Sciprintf("queue is full event will get lost\n"); */
      return 0;
    }
  /* Sciprintf("status in=%d out=%d size=%d\n",q->in,q->out,q->size);*/
  q->elems[q->in++] = *ev;
  if (q->in == q->size) 
    {
      if ( q->out == 0) 
	{
	  /* Sciprintf("queue is full event will get lost\n");*/
	  q->in--;
	}
      else 
	q->in=0;
    }
  return 0;
}

/* to be used when queue is not empty */

nsp_gwin_event nsp_dequeue(nsp_event_queue *q)
{
  nsp_gwin_event ev = q->elems[q->out++];
  if (q->out == q->size) q->out = 0;
  return ev;
}

/* to be used when queue is not empty */

nsp_gwin_event nsp_peekqueue(nsp_event_queue *q)
{
  return  q->elems[q->out];
}

int nsp_queue_empty(nsp_event_queue *q)
{
  return (q->in == q->out ) ? TRUE : FALSE ;
}

void nsp_clear_queue(nsp_event_queue *q)
{
  q->in = q->out = 0;
}





