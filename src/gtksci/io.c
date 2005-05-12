/*------------------------------------------------------------------------
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <string.h> 
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/time.h>
#include <stdarg.h>
#include <stdlib.h>

#ifdef aix
#include <sys/select.h>
#endif

#include <gdk/gdkx.h>
#include <gtk/gtk.h>

#include "nsp/math.h"
#include "../system/Sun.h"
#include "nsp/gtksci.h"
#include "nsp/sciio.h"

/* WITH_TK is eventually defined in nsp/machine.h 
 * include by nsp/Math.h 
 */ 

#define WITH_GTK_MAIN 1

#ifdef WITH_TK
#include "../tksci/tksci.h"
#endif

/* FIXME */
extern void C2F(diary) (char *str, int *n, int nn);
extern int checkqueue_nsp_command(void) ;

/*
 * Input output functions 
 */

/*
 * Functions to set or to get the nsp status 
 * i.e is it a nsp or a nsp -nw 
 */

static int nsp_in_widget = FALSE;
static int nsp_with_events = FALSE ;

/**
 * nsp_in_gtk_window:
 * @void: 
 * 
 * nsp is a gtk application 
 * 
 **/

void nsp_in_gtk_window(void)
{
#ifdef WITH_TK
  inittk();
#endif
  nsp_in_widget = TRUE ;
  nsp_with_events = TRUE ;
}

int nsp_is_gtk_window(void)
{
  return  nsp_in_widget;
}

void nsp_activate_gtk_events_check(void)
{
  nsp_with_events = TRUE;
}

int nsp_check_events_activated(void)
{ 
  return nsp_with_events ;
}


#define IBSIZE 1024
char sci_input_char_buffer[IBSIZE];
int  sci_input_char_buffer_count = 0;

#define SELECT_DEBUG(x) 

/* send string s as if it was typed in scilab window */ 

void write_scilab(char *s)
{
  while ( *s != '\0' && sci_input_char_buffer_count < IBSIZE) 
    {
      sci_input_char_buffer[sci_input_char_buffer_count++]= *s;
      s++;
    }
  sci_input_char_buffer[sci_input_char_buffer_count++]='\n';
}

/* 
 *  Xorgetchar : function used while in the scilab -nw mode 
 *      by zzledt to get the next typed char (in stdin)
 *      but also checks for X11 events if we are using an X Window 
 *      ( ex a graphic window) with scilab -nw )
 *      
 *      stdin is supposed to be changed 
 *      so as not to wait for <cr> this is done inside zzledt 
 *      (in the following code  the key function is select )
 * wait for a character and check for pending events 
 */

int Xorgetchar(void)
{
  int i;
  static int c_count = -1;
  SELECT_DEBUG(static int counter=0) ;
  static int GtkXsocket,fd_in,fd_out,fd_err;
  static int first = 0,max_plus1;
  fd_set select_mask,write_mask;
  static struct timeval select_timeout;
  SELECT_DEBUG(fprintf(stderr,"New Xorgetchar %d\n",counter++);)

  if ( nsp_check_events_activated()== FALSE) return(getchar());

  if ( first == 0) 
    {
      first++;
      GtkXsocket = ConnectionNumber(GDK_DISPLAY());
      fd_in  = fileno(stdin) ;
      fd_out = fileno(stdout);
      fd_err = fileno(stderr);
      max_plus1 = Max(fd_in,GtkXsocket);      
      max_plus1 = Max(fd_out,max_plus1);
      max_plus1 = Max(fd_err,max_plus1);
#ifdef WITH_TK 
      max_plus1 = Max(XTKsocket,max_plus1);
#endif 
      max_plus1++;
    }

  for( ; ; ) {
    SELECT_DEBUG(fprintf(stderr,"looping %d\n",counter++);)
    /* always flush writes before waiting */
    gdk_flush();
    fflush(stdout); 
    fflush(stderr);
    /* Update the masks and, unless X events are already in the queue,
     * wait for I/O to be possible. 
     */
    FD_ZERO(&select_mask);
    FD_SET(fd_in , &select_mask);
    FD_SET(GtkXsocket, &select_mask);
#ifdef WITH_TK 
    FD_SET(XTKsocket, &select_mask);
#endif 
    FD_ZERO(&write_mask);
    /* XXXX : the two next FD_SET causes select not to wait 
     * and since they do not seam necessary they are commented out  
     */
    /* FD_SET(fd_out,&write_mask);
       FD_SET(fd_err,&write_mask); */
    /* 
     * here we must reset select_timeout at each iteration
     * since select can change it.
     * 
     * we put a timeout to give a chance to gtk_timeout and gtk_idle 
     * (they do not unblock select) to be executed when they are used without a gtk_main call 
     * I do not exactly know which time choice is good ....
     * if timeout is too small nsp will work too much when idle 
     * if too large gtk_timeout and gtk_idle won't work fine without gtk_main.
     */
    select_timeout.tv_sec =  0; /* could be more */
    select_timeout.tv_usec = 5;
#ifdef WITH_GTK_MAIN 
    while ( gtk_events_pending()) 
      {
	SELECT_DEBUG(fprintf(stderr,"une iteration %d\n",counter++);)
	gtk_main_iteration(); 
      }
#endif
    /* maybe a new string to execute */
    if ( sci_input_char_buffer_count > 0) 
      {
	sci_input_char_buffer_count--;
	return sci_input_char_buffer[++c_count];
      }
    else
      {
	c_count = -1;
      }
    /* maybe a command in the command queue */
    if ( checkqueue_nsp_command() == TRUE) return 0;
    SELECT_DEBUG(fprintf(stderr,"enter select %d\n",counter++);)
    i = select(max_plus1, &select_mask,&write_mask, (fd_set *)NULL, &select_timeout);
    SELECT_DEBUG(fprintf(stderr,"after select %d\n",counter++);)
    if (i < 0) {
      if (errno != EINTR)
	{ 
	  SELECT_DEBUG(fprintf(stderr,"error in select\n");)
	  exit(0);
	  continue;
	} 
      SELECT_DEBUG(fprintf(stderr,"error %s\n",strerror(errno)));
      continue;
    }
    if ( i == 0 ) 
      {
	continue ;
      }
    /* if there's something to output */
    if ( FD_ISSET(fd_out,&write_mask)) { 
      fflush(stdout); 
    }
    if ( FD_ISSET(fd_err,&write_mask)) { 
      fflush(stderr); 
    }
    /* if there's something to read */
    if ( FD_ISSET(fd_in,&select_mask )) { 
      return getchar();
      break;
    } 
#ifdef WITH_TK 
    if ( FD_ISSET(XTKsocket,&select_mask )) { 
      flushTKEvents();
    }
#endif 
    if ( FD_ISSET(GtkXsocket,&select_mask)) { 
      /* if there are X events in our queue, it
       * counts as being readable 
       */
#ifdef WITH_GTK_MAIN 
      while ( gtk_events_pending()) 
	{ 
	  SELECT_DEBUG(fprintf(stderr,"une iteration after select %d\n",counter++);)
	  gtk_main_iteration(); 
	} 
#endif 
    }
  }
}

/*
 * This routine can be called to checks for all events 
 * except typed text (but testing Ctrl) and to deal with them 
 * This function is unused in the gtk version. 
 */

void  nsp_check_gtk_events(void)
{
  /* check the TK case */ 
  if ( nsp_check_events_activated()== FALSE) return ;
#ifdef WITH_TK
  flushTKEvents();
#endif
  while ( gtk_events_pending())  gtk_main_iteration(); 
}


/*
 * winch signal : window size changed 
 * FIXME: 
 * not usefull since we call sci_get_screen_size each time we need it
 */

void sci_winch_signal(int n) 
{
  int rows,cols;
  sci_get_screen_size (&rows,&cols);
#ifdef DEBUG
  fprintf(stderr,"windows size changed %d %d\r\n",rows,cols);
#endif 
}

