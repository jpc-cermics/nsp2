/* Nsp
 * 
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * main 
 *--------------------------------------------------------------------------*/

#include <ctype.h>
#if !defined(__MSC__) && ! defined(__MINGW32__)
#include <sys/ioctl.h>
#endif 
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include "nsp/version.h"
#include "nsp/machine.h"
#ifdef WITH_GTKGLEXT
#include <gtk/gtkgl.h>
#endif
#include <gtk/gtk.h>
#include "nsp/sciio.h"
#include "../system/files.h"
#include "nsp/gtksci.h"
#include "nsp/nsptcl.h"

extern GtkWidget *create_main_menu( GtkWidget  *window);
extern void nsp_create_main_text_view(void);



/* #define STATUS_BAR 1  */

#ifdef STATUS_BAR 
static void create_scilab_status(void);
#endif 

static void nsp_gtk_gl_init (int *argc,char ***argv);
static void nsp_create_gtk_toplevel(gint argc, gchar *argv[]);
static void nsp_set_emacs_key_theme(void) ;


/**
 * nsp_gtk_init:
 * @argc: 
 * @argv: 
 * @no_window: 
 * 
 * Initialize gtk 
 **/

void nsp_gtk_init(int argc, char **argv,int no_window,int use_textview)
{
  if ( no_window == FALSE ) 
    {
      char *shmid= nsp_getenv("SHMID");
      /* we are using a gtk widget app */
      nsp_in_gtk_window();
      /* initialise gtk */
      gtk_set_locale();
      gtk_init(&argc,&argv);
      /* initialise opengl */
      nsp_gtk_gl_init (&argc, &argv);
      /* we are in window mode */
      if ( shmid != NULL )
	{
	  /* we build a toplevel widget and 
	   * zterm will be inserted through socket/plug mechanism 
	   */
	  nsp_create_gtk_toplevel(argc,argv);
	}
      else 
	{
	  /* we just create a menu which will be inserted 
	   * in the calling zterm through socket/plug mechanism 
	   */
	  create_plugged_main_menu() ;
	}
      /* interaction with a textview 
       *
       */
      if ( use_textview == TRUE) 
	nsp_create_main_text_view();

      /* create a status bar 
       * FIXME: unsused up to now 
       * 
       */
#ifdef STATUS_BAR 
      create_scilab_status();
#endif 
      nsp_set_emacs_key_theme() ;

    }
  /* signals */
  signal(SIGSEGV,sci_clear_and_exit);
#if !defined(__MSC__) && ! defined(__MINGW32__)
  signal(SIGINT,sci_clear_and_exit); 
  signal(SIGBUS,sci_clear_and_exit);
  signal(SIGQUIT,sci_clear_and_exit);
  signal(SIGHUP,sci_clear_and_exit);
  signal(SIGUSR1,sci_usr1_signal);
  signal(SIGWINCH, sci_winch_signal);
#endif
  /* initialize scilab interp  */
  /* C2F(inisci)(&ini, &memory, &ierr); */
  /* set up terminal size */
  sci_winch_signal(0);
}

/**
 * start_sci_gtk:
 * @void: 
 * 
 * used for starting gtk when 
 * scilab has been called with scilab -nw 
 * and a menu or graphic window is activated 
 *
 **/

void start_sci_gtk(void)
{
  int argc=0;
  char **argv = NULL; 
  if ( nsp_check_events_activated() == TRUE ) return;
  /* initialise gtk */
  gtk_set_locale();
  gtk_init(&argc,&argv);
  nsp_gtk_gl_init (&argc, &argv);
  nsp_activate_gtk_events_check();
  nsp_set_emacs_key_theme();
}

static void nsp_gtk_gl_init (int *argc,char ***argv)
{
#ifdef WITH_GTKGLEXT 
  gtk_gl_init(argc,argv);
#endif
}

/*-------------------------------------------------------
 * color status 
 *-------------------------------------------------------*/

static int screencolor = 1 ; /* default screen color status */

/* return the current screencolor */

void getcolordef(integer *screenc)
{
  *screenc= screencolor;
}

void setcolordef( int screenc)
{
  screencolor = screenc;
}

/*-------------------------------------------------------
 * status bar 
 *-------------------------------------------------------*/

#ifdef STATUS_BAR 

static GtkWidget *status = NULL; 

static void create_scilab_status()
{
  GtkWidget *Plug; 
  char * plug_info = nsp_getenv("SCIINFO");
  if ( plug_info == NULL) return ; 

  Plug = gtk_plug_new(atoi(nsp_getenv("SCIINFO")));
  status  = gtk_statusbar_new ();
  gtk_container_add(GTK_CONTAINER(Plug), status);
  gtk_widget_show_all(Plug);
}

void scilab_status_show(char * message)
{
  gtk_statusbar_pop (GTK_STATUSBAR(status), 1);
  gtk_statusbar_push(GTK_STATUSBAR(status), 1, message);
}

#endif 

/*
 * scilab toplevel widget when zterm widget is plugged 
 */

#if !defined(__MSC__) && ! defined(__MINGW32__)
#include <gdk/gdkx.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkkeysyms.h>
#endif 

static GtkWidget  *window = NULL;

/**
 * we use  shared memory to send back to the calling process 
 * the id of a socket button 
 */ 

#if !defined(__MSC__) && ! defined(__MINGW32__)
#include <sys/types.h>
#include <sys/shm.h>

static char *get_shared(void)
{
  int shmid;
  char *shm;
  char *s= nsp_getenv("SHMID");
  
  if ( s == NULL) 
    {
      perror("cannot getenv SHMID");
      exit(1);
    }
  shmid = atoi(s);

  /*
   * Now we attach the segment to our data space.
   */
  if ((shm = shmat(shmid, NULL, 0)) == (char *) -1) {
    perror("shmat");
    exit(1);
  }
  return shm;
}
#endif 


/*
 *  main routine
 *  Does setup, initialises windows, forks child.
 */

static void nsp_create_gtk_toplevel(gint argc, gchar *argv[])
{
#if !defined(__MSC__) && ! defined(__MINGW32__)
  guint32 *xid; 
  char * shm = get_shared() ;
#endif 
  GtkWidget *vbox,*menubar, *socket_button;
  gtk_set_locale();
  gtk_init(&argc, &argv);
  nsp_gtk_gl_init (&argc, &argv);
  /*
   */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Nsp");
  gtk_window_set_wmclass (GTK_WINDOW (window), "toplevel", "Scilab");
  gtk_widget_set_size_request (window,600,400);
  /* create vbox */
  vbox = gtk_vbox_new (FALSE, 0);
  gtk_box_set_spacing (GTK_BOX (vbox), 2);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  /* create the menu bar */

  menubar= create_main_menu( window);
  gtk_box_pack_start(GTK_BOX(vbox),menubar,FALSE,TRUE,0);

  /* a socket in which I will redirect interaction */ 
  socket_button = gtk_socket_new();
  /*   gtk_widget_set_usize(socket_button,300,100); */
  gtk_box_pack_start(GTK_BOX(vbox), socket_button,TRUE,TRUE,0);

  /* show them all! */
  gtk_widget_show_all(window);
  gtk_widget_grab_focus(socket_button);
#if !defined(__MSC__) && ! defined(__MINGW32__)
  /* I transmit the socket Id via shared memory  */ 
  xid = (guint32 *) (shm+1); 
#ifdef  GDK_WINDOW_XWINDOW
  *xid = GDK_WINDOW_XWINDOW(socket_button->window); 
#else 
  *xid = socket_button->window;
#endif
  *shm = '*' ; /* just to tell that there's something to read */
#endif 
} 

/* emacs mode for edition 
 *
 */

void nsp_set_emacs_key_theme(void) 
{
  GtkSettings *settings;
  gchar *default_key_theme = NULL;
  /*
   * check settings for edition 
   */
  settings = gtk_settings_get_default ();
  if ( settings == NULL) return ;
  g_object_get (settings, "gtk-key-theme-name", &default_key_theme, NULL);
  if ( default_key_theme == NULL || strcmp(default_key_theme,"Emacs") != 0) 
    gtk_settings_set_string_property (settings,
				      "gtk-key-theme-name", "Emacs",NULL);
  if ( default_key_theme != NULL)  free(default_key_theme);
}


