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
 * input from user 
 * jpc@cermics.enpc.fr 
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

#ifndef WIN32 
#include <gdk/gdkx.h>
#else 
#include <winsock.h>
#endif 

#include <gtk/gtk.h>
#include "nsp/math.h"
#include "nsp/gtksci.h"
#include "nsp/sciio.h"


/* #define NSP_ENTRY_INPUT_TEST  */

#define WITH_GTK_MAIN 1

/* WITH_TK is eventually defined in nsp/machine.h 
 * include by nsp/Math.h 
 */ 

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
char nsp_input_char_buffer[IBSIZE];
int  nsp_input_char_buffer_count = 0;

#define SELECT_DEBUG(x) 

/* send string s as if it was typed in scilab window */ 

void nsp_input_feed(char *s)
{
  while ( *s != '\0' && nsp_input_char_buffer_count < IBSIZE) 
    {
      nsp_input_char_buffer[nsp_input_char_buffer_count++]= *s;
      s++;
    }
  nsp_input_char_buffer[nsp_input_char_buffer_count++]='\n';
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

#ifndef WIN32 
int Xorgetchar_select(void)
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
#ifdef WITH_TK 
    flushTKEvents();
#endif
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
#ifdef __APPLE__
    select_timeout.tv_usec = 1000;
#else 
    select_timeout.tv_usec = 5;
#endif 

#ifdef WITH_GTK_MAIN 
    while ( gtk_events_pending()) 
      {
	SELECT_DEBUG(fprintf(stderr,"une iteration %d\n",counter++);)
	gtk_main_iteration(); 
      }
#endif
    /* maybe a new string to execute */
    if ( nsp_input_char_buffer_count > 0) 
      {
	nsp_input_char_buffer_count--;
	return nsp_input_char_buffer[++c_count];
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

#endif /*  WIN32  */


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




/* 
 * Xorgetchar
 * this function is used to get next char and manage X11 events during 
 * the wait for next char. Note that Xorgetchar is transmited to 
 * readline. This version should be much more portable than the 
 * previous one which uses select. It should be checked on windows.
 * must protect the gtk_main of a Ctrl-C 
 * 
 */

static void nsp_create_stdin_channel(int fd,int *val);

int Xorgetchar_channel(void)
{
  static int fd_in,first=0;
  int val;
  SELECT_DEBUG(static int counter=0) ;
  SELECT_DEBUG(fprintf(stderr,"New Xorgetchar %d\n",counter++););
  if ( nsp_check_events_activated()== FALSE) return(getchar());

  if ( first == 0) 
    {
      first++;
      fd_in  = fileno(stdin) ;
      nsp_create_stdin_channel(fd_in,&val);
    }
  gtk_main();
  return val;
}


static gboolean stdin_read( GIOChannel *source, GIOCondition condition, gpointer data );

static void nsp_create_stdin_channel(int fd,int *val)
{
  static GIOChannel *channel;
  guint stdout_tag;
  channel = g_io_channel_unix_new( fd );
  g_io_channel_set_encoding( channel, NULL, NULL );
  g_io_channel_set_flags(channel,G_IO_FLAG_NONBLOCK,NULL);
  stdout_tag = g_io_add_watch( channel,
			       ( G_IO_IN | G_IO_HUP | G_IO_ERR ),
			       stdin_read,
			       val );
}

/* Note that bytes_read can be longer that 1 here 
 * if buf is larger than 1 and a set of characters are pasted
 * This could be used to detect that we have more than 
 * one char typed i.e that we are in a cut-and-paste operation 
 * when the user is interactive chars are accepted one by one.
 *
 */

static gboolean stdin_read( GIOChannel *source, GIOCondition condition, gpointer data )
{
  const int buf_size=1;
  char buf[1];
  GIOStatus status;
  gsize bytes_read;
  status = g_io_channel_read_chars( source, buf,buf_size,&bytes_read,NULL);
  if ( status != G_IO_STATUS_NORMAL )
    {
      g_print( "STDIN: %s\n"," stdin finished !!!!");
    }
  else 
    {
      int *val = data;
      buf[bytes_read]='\0';
      /* Sciprintf("just seen: [%s]\n",buf); */
      *val = buf[0];
      gtk_main_quit();
    }
  return TRUE;
}


/* 
 * Xorgetchar
 *  simple version for threaded gtk
 */

int Xorgetchar_thread(void)
{
  return getchar();
}

/*  used to switch the default interactive getchar 
 *
 */

#ifdef WIN32 
Get_char Xorgetchar = Xorgetchar_thread ;
#else 
Get_char Xorgetchar = Xorgetchar_select ;
#endif 

Get_char nsp_set_getchar_fun(Get_char F)
{
  Get_char g = Xorgetchar;
  Xorgetchar = F;
  return g;
}

/* used to switch the default interactive readline function 
 *
 */


SciReadFunction DefSciReadLine = nsp_defscireadline_rl;

SciReadFunction nsp_set_readline_fun(SciReadFunction F)
{
  SciReadFunction g = DefSciReadLine;
  DefSciReadLine = F;
  return g;
}




/* test version with vte as output and an entry as input 
 * for windows native port.
 */

#ifdef NSP_ENTRY_INPUT_TEST 
#include "term.c"
#endif
