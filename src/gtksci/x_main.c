/* Nsp
 *
 * Copyright (C) 2005-2019 Jean-Philippe Chancelier Enpc/Cermics
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
#include <nsp/nsp.h>
#include <locale.h>
#include <gtk/gtk.h>

#include <nsp/sciio.h>
#include <nsp/system.h>
#include <nsp/gtksci.h>
#include <nsp/nsptcl.h>
#include <nsp/config.h>

#if !defined(WITH_GTKOSX) && !defined(WIN32) 
#define WITH_GTKXH
#if GTK_CHECK_VERSION (3,0,0)
#include <gtk/gtkx.h>
#endif
#endif

#ifdef WITH_GTKGLEXT
#include <gtk/gtkgl.h>
#endif

extern GtkWidget *create_main_menu( GtkWidget  *window);
extern void nsp_create_main_text_view(void);

/* #define STATUS_BAR 1  */

#ifdef STATUS_BAR
static void create_nsp_status(void);
#endif

static void nsp_gtk_gl_init (int *argc,char ***argv);
static void nsp_create_gtk_toplevel(gint argc, gchar *argv[]);
static void nsp_set_emacs_key_theme(void) ;
static void nsp_unset_gtk_error(int no_log);

/**
 * nsp_gtk_init:
 * @argc:
 * @argv:
 * @no_window:
 *
 * Initialize gtk
 **/

#ifdef MY_LOG_HANDLER
static void my_log_handler(const gchar *log_domain,
			   GLogLevelFlags log_level,
			   const gchar *message,
			   gpointer user_data);
#endif

void nsp_gtk_init(int argc, char **argv,int no_window,int use_textview)
{
  if ( no_window == FALSE )
    {
      const char *shmid= nsp_getenv("SHMID");
      /* we are using a gtk widget app */
      nsp_in_gtk_window();
      /* initialise gtk */
#ifdef __APPLE__
      /* avoid a gtk warning about locale */
      gtk_disable_setlocale();
      setlocale(LC_ALL,"");
#else
#if GTK_CHECK_VERSION (3,0,0)
      setlocale(LC_ALL,"");
#else
      gtk_set_locale();
#endif
#endif
      gtk_init(&argc,&argv);
      /* TRUE: means no log, this is for production */
      nsp_unset_gtk_error(TRUE);
      /*
	g_object_set (gtk_settings_get_default (),
		    "gtk-enable-mnemonics", TRUE,
		    "gtk-auto-mnemonics", FALSE,
		    "gtk-enable-accels", TRUE, NULL);
      */
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
	  create_plugged_main_menu();
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
      create_nsp_status();
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
  /* initialize nsp interp  */
  /* C2F(inisci)(&ini, &memory, &ierr); */
  /* set up terminal size */
  sci_winch_signal(0);
}

/**
 * start_sci_gtk:
 * @void:
 *
 * used for starting gtk when
 * nsp has been called with nsp -nw
 * and a menu or graphic window is activated
 *
 **/

void start_sci_gtk(void)
{
  int argc=0;
  char **argv = NULL;
  if ( nsp_check_events_activated() == TRUE ) return;
  /* initialise gtk */
#ifdef __APPLE__
  /* avoid a gtk warning about locale */
  gtk_disable_setlocale();
  setlocale(LC_ALL,"");
#else
#if GTK_CHECK_VERSION (3,0,0)
  setlocale(LC_ALL,"");
#else
  gtk_set_locale();
#endif
#endif
  gtk_init(&argc,&argv);
  nsp_gtk_gl_init (&argc, &argv);
  nsp_activate_gtk_events_check();
  nsp_set_emacs_key_theme();
  /* locale for LC_NUMERIC must be english type
   * and starting gtk can change this.
   */
  setlocale(LC_NUMERIC,"C");
}

static void nsp_gtk_gl_init (int *argc,char ***argv)
{
#ifdef WITH_GTKGLEXT
#if ! GTK_CHECK_VERSION(3,0,0)
  gtk_gl_init(argc,argv);
#endif
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

static void create_nsp_status()
{
  GtkWidget *Plug;
  char * plug_info = nsp_getenv("SCIINFO");
  if ( plug_info == NULL) return ;

  Plug = gtk_plug_new(atoi(nsp_getenv("SCIINFO")));
  status  = gtk_statusbar_new ();
  gtk_container_add(GTK_CONTAINER(Plug), status);
  gtk_widget_show_all(Plug);
}

void nsp_status_show(char * message)
{
  gtk_statusbar_pop (GTK_STATUSBAR(status), 1);
  gtk_statusbar_push(GTK_STATUSBAR(status), 1, message);
}

#endif

/*
 * nsp toplevel widget when zterm widget is plugged
 */

#if !defined(__MSC__) && ! defined(__MINGW32__) && !defined(WITH_GTKOSX)
#include <gdk/gdkx.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkkeysyms.h>
#endif

static GtkWidget  *window = NULL;

/**
 * we use  shared memory to send back to the calling process
 * the id of a socket button
 */

#ifdef WITH_GTKXH
#include <sys/types.h>
#include <sys/shm.h>

static char *get_shared(void)
{
  int shmid;
  char *shm;
  const char *s= nsp_getenv("SHMID");

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
  GtkWidget *vbox,*menubar;
#ifdef WITH_GTKXH
  gulong *xid;
  char * shm = get_shared() ;
  GtkWidget *socket_button;
#endif
  
#ifdef __APPLE__
  /* avoid a gtk warning about locale */
  gtk_disable_setlocale();
  setlocale(LC_ALL,"");
#else
  /* deprecated gtk_set_locale(); */
  setlocale(LC_ALL,"");
#endif

  gtk_init(&argc, &argv);
  nsp_gtk_gl_init (&argc, &argv);
  /*
   */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Nsp");
  gtk_window_set_wmclass (GTK_WINDOW (window), "toplevel", "Scilab");
  gtk_widget_set_size_request (window,600,400);
  /* create vbox */
#if GTK_CHECK_VERSION (3,0,0)
  vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new (FALSE, 0);
#endif
  gtk_box_set_spacing (GTK_BOX (vbox), 2);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  /* create the menu bar */
  menubar= create_main_menu( window);
  gtk_box_pack_start(GTK_BOX(vbox),menubar,FALSE,TRUE,0);

#ifdef WITH_GTKXH
  /* a socket in which I will redirect interaction */
  socket_button = gtk_socket_new();
  /*   gtk_widget_set_usize(socket_button,300,100); */
  gtk_box_pack_start(GTK_BOX(vbox), socket_button,TRUE,TRUE,0);
#endif
  
  /* show them all! */
  gtk_widget_show_all(window);

#ifdef WITH_GTKXH
  gtk_widget_grab_focus(socket_button);
  
#if !defined(__MSC__) && ! defined(__MINGW32__)
  /* I transmit the socket Id via shared memory  */
  xid = (gulong *) (shm+1);
#ifdef GDK_WINDOW_XWINDOW
#ifdef GSEAL_ENABLE
  *xid = (gulong) GDK_WINDOW_XWINDOW(gtk_socket_get_plug_window(GTK_SOCKET(socket_button)));
#else
  *xid = GDK_WINDOW_XWINDOW(socket_button->window);
#endif
#else
#if GTK_CHECK_VERSION (3,0,0)
  /* *xid= gtk_socket_get_plug_window(GTK_SOCKET(socket_button)); */
  *xid= (gulong) gtk_socket_get_id(GTK_SOCKET(socket_button));
#else
  *xid = (gulong) socket_button->window;
#endif
#endif
  *shm = '*' ; /* just to tell that there's something to read */
#endif
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
				      "gtk-key-theme-name", "Emacs","");
  if ( default_key_theme != NULL)  free(default_key_theme);
}

/* gtk warnings:
 *
 */

static void _dummy(const gchar *log_domain,
		   GLogLevelFlags log_level,
		   const gchar *message,
		   gpointer user_data )
{
  /* Dummy does nothing */ 
  return ;      
}

#if GLIB_CHECK_VERSION (2, 50, 0)
static GLogWriterOutput
null_log_writer (GLogLevelFlags   log_level,
                 const GLogField *fields,
                 gsize            n_fields,
                 gpointer         user_data)
{
  return G_LOG_WRITER_HANDLED;
}
#endif

static void nsp_unset_gtk_error(int no_log)
{
  if (no_log)
    {
      /* Set dummy for all levels */
      g_log_set_default_handler(  _dummy, NULL);
#if GLIB_CHECK_VERSION (2, 50, 0)
      g_log_set_writer_func (null_log_writer, NULL, NULL);
#endif
    }
  else
    {
      /* Set default handler based on argument for appropriate log level */
      g_log_set_default_handler( g_log_default_handler, NULL);
#if GLIB_CHECK_VERSION (2, 50, 0)
      g_log_set_writer_func (g_log_writer_default, NULL, NULL);
#endif
    }
  /* test: if the following if is active 
   * warnings will be emited if no_log is FALSE 
   * and not if no_log is TRUE 
   */
  if ( 0 )
    {
      g_warning("This is warning\n");
      g_message("This is message\n");
      g_debug("This is debug\n");
      g_critical("This is critical\n");
      g_log(NULL, G_LOG_LEVEL_INFO , "This is info\n");
    }
}
