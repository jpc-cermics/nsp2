/*---------------------------------------------------------- 
 * mainsci.f directly call this function 
 * thus this is the real main for scilab 
 * Copyright 2001 Inria/Enpc 
 *----------------------------------------------------------*/

#include <pwd.h>
#include <ctype.h>
#include <sys/ioctl.h>
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
#include "menus.h"

#if 0 
static void create_scilab_status(void);
#endif 

static void nsp_gtk_gl_init (int *argc,char ***argv);
static void nsp_create_gtk_toplevel(gint argc, gchar *argv[]);

/*---------------------------------------------------------- 
 * mainsci.f directly call this function 
 * thus this is the real main for scilab 
 * Copyright Inria/Enpc 
 *----------------------------------------------------------*/

/* global var */

void nsp_gtk_init(int argc, char **argv,int no_window)
{
  if ( no_window == FALSE ) 
    {
      char *shmid= getenv("SHMID");
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
      /* create a status bar 
       * FIXME: unsused up to now 
       * create_scilab_status();
       */
    }
  /* signals */
  signal(SIGINT,sci_clear_and_exit);
  signal(SIGBUS,sci_clear_and_exit);
  signal(SIGSEGV,sci_clear_and_exit);
  signal(SIGQUIT,sci_clear_and_exit);
  signal(SIGHUP,sci_clear_and_exit);
  signal(SIGUSR1,sci_usr1_signal);
  signal(SIGWINCH, sci_winch_signal);

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

#if 0 

static GtkWidget *status = NULL; 

static void create_scilab_status()
{
  GtkWidget *Plug; 
  char * plug_info = getenv("SCIINFO");
  if ( plug_info == NULL) return ; 

  Plug = gtk_plug_new(atoi(getenv("SCIINFO")));
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

#include <gdk/gdkx.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkkeysyms.h>

static GtkWidget  *window = NULL;

/**
 * we use  shared memory to send back to the calling process 
 * the id of a socket button 
 */ 

#include <sys/ipc.h>
#include <sys/shm.h>

static char *get_shared(void)
{
  int shmid;
  char *shm;
  char *s= getenv("SHMID");
  
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

/*
 *  main routine
 *  Does setup, initialises windows, forks child.
 */

static void nsp_create_gtk_toplevel(gint argc, gchar *argv[])
{
  guint32 *xid; 
  char * shm = get_shared() ;
  GtkWidget *vbox,*menubar, *socket_button;
  gtk_set_locale();
  gtk_init(&argc, &argv);
  nsp_gtk_gl_init (&argc, &argv);
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
  /* I transmit the socket Id via shared memory  */ 
  xid = (guint32 *) (shm+1); 
  *xid = GDK_WINDOW_XWINDOW(socket_button->window); 
  *shm = '*' ; /* just to tell that there's something to read */
} 




