/* Nsp
 * Copyright (C) 2001-2010 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/graphics-new/Graphics.h" 
#include <nsp/object.h>
#include <nsp/matrix.h>
#include <nsp/eval.h>

/**
 * scig_click_handler_none:
 * @win: 
 * @x: 
 * @y: 
 * @ibut: 
 * @imask: 
 * @motion: 
 * @release: 
 * 
 * empty event recorder.
 * 
 * Return value: 0
 **/

int scig_click_handler_none (int win,int x,int y,int ibut,
			     int imask, int motion,int release) 
{return 0;}


/**
 * scig_click_handler_sci:
 * @win: 
 * @x: 
 * @y: 
 * @ibut: 
 * @imask:
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

int scig_click_handler_sci (int win,int x,int y,int ibut,int imask,int motion,int release)
{
  BCG *SciGc;
  SciGc = window_list_search_new(win);
  if ( strlen(SciGc->EventHandler)!=0) 
    {
      /* In the second part of the if the command is executed in the standard 
       * eval loop in which presence of pending commands are checked. 
       * Thus it can only work if the standard eval loop is active and it 
       * gives a very slow execution when using nsp -tv. Thus we use here 
       * a direct evaluation through  nsp_gtk_eval_function_by_name. 
       * It could be usefull to have the possibility to errcatch or not 
       * the execution of the given handler.
       */
#if 1 
      const char *names[]={ "win","x","y","ibut","imask"};
      static NspObject *args[5] = {0};
      static int initialized=0;
      int n_args = 5, i, n_ret=0;
      if ( initialized == 0) 
	{
	  for ( i = 0 ; i < n_args ; i++)
	    args[i]= (NspObject *) nsp_matrix_create(names[i],'r',1,1);
	  initialized = 1;
	}
      ((NspMatrix *) args[0])->R[0] = win; 
      ((NspMatrix *) args[1])->R[0] = x; 
      ((NspMatrix *) args[2])->R[0] = y; 
      ((NspMatrix *) args[3])->R[0] = ibut; 
      ((NspMatrix *) args[4])->R[0] = imask; 
      /* Maybe it could be usefull to make errcatch here XXX 
       */
      if ( nsp_gtk_eval_function_by_name(SciGc->EventHandler,args,n_args,NULL,&n_ret) == FAIL) 
	{
	  Sciprintf("Error: %s failed\n",SciGc->EventHandler);
	}
#else 
      char buf[256];
      sprintf(buf,"%s(%d,%d,%d,%d,%d)",SciGc->EventHandler,win,x,y,ibut,imask);
      enqueue_nsp_command(buf);
#endif
      return 1;
    }
  else
    {
      return 0;
    }
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

/**
 * nsp_enqueue:
 * @q: an event queue attached to a graphic window
 * @ev: an event 
 * 
 * This function is called to store in a queue click/motion events which 
 * occur in a graphic window. Each graphic window has its own queue. 
 * see graphics/perigtk/events.c 
 * The events are queued when outise the xclick() function. The events 
 * which are stored in the queue can be used by the xclick function. 
 * If an event handler is attached to the graphic window then this 
 * event handler is directly called. 
 * 
 * Returns: 0 
 **/

int nsp_enqueue(nsp_event_queue *q, nsp_gwin_event *ev)
{
  /* first let a click_handler do the job  */
  if ( scig_click_handler(ev->win,ev->x,ev->y,ev->ibutton,ev->mask,ev->motion,ev->release)== 1) return 0;

  /* Sciprintf("Put event in the queue [%d,%d,%d]\n",ev->x,ev->y,ev->ibutton); */

  if ( q->in == q->out -1 )  
    {
      /* the queue is full */
      /* Sciprintf("queue is full event will get lost\n"); */
      return 0;
    }
  q->elems[q->in++] = *ev;
  if (q->in == q->size) 
    {
      q->in=0;
      if ( q->out == 0) 
	{
	  q->out++;
	}
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





