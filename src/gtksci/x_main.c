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
#include <gtk/gtk.h>

#include "../version.h"
#include "nsp/machine.h"
#include "nsp/sciio.h"
#include "../system/files.h"
#include "All-extern.h"

char *ProgramName = NULL;

#if 0 
static void Syntax  (char *badOption);  
static void Help  (void);  
static void create_scilab_status(void);
#endif 

static void set_sci_env (void);

/*---------------------------------------------------------- 
 * mainsci.f directly call this function 
 * thus this is the real main for scilab 
 * Copyright Inria/Enpc 
 *----------------------------------------------------------*/

#define MIN_STACKSIZE 180000

static int  no_startup_flag=0;
static int  memory = MIN_STACKSIZE;
static int  no_window = 0;
static char * initial_script = NULL;
static int  initial_script_type = 0; /* 0 means filename 1 means code */

/* extern int C2F(initcom)(int *,int*); */

extern int scilab_main (int argc,char **argv,char *pname,int no_window,int no_startup, char *display);
extern int C2F(nofpex)(void);
extern int C2F(getarg)(int *,char *,long int l);
extern int C2F(iargc)(void);
extern void C2F(settmpdir)(void);
extern char *get_sci_data_strings(int n);

extern void SciGtkReadLine(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof);

static char ** create_argv(int *argc);
static void strip_blank(char *source);

/* global var */

int  sci_show_banner=1;

int real_main(int argc, char **argv)
{
  int ierr, i;
  char startup[128];
  char *display = NULL;

  /* floating point exceptions */
  /* XXXX C2F(nofpex)();  */
  /* create argv */
  /* if (( argv = create_argv(&argc))== NULL) exit(1); */
  ProgramName = argv[0];
  /* scanning options */
  for ( i=0 ; i < argc ; i++) 
    {
      if ( strcmp(argv[i],"-nw") == 0) { no_window = 1; } 
      else if ( strcmp(argv[i],"-display") == 0) { display = argv[++i];} 
      else if ( strcmp(argv[i],"-ns") == 0) { no_startup_flag = 1; }
      else if ( strcmp(argv[i],"-nb") == 0) { sci_show_banner = 0; }
      else if (strcmp(argv[i],"-mem") == 0) { memory = Max(atoi(argv[++i]),MIN_STACKSIZE );} 
      else if (strcmp(argv[i],"-f") == 0) { initial_script = argv[++i];} 
      else if ( strcmp(argv[i],"-e") == 0) 
	{
	  initial_script = argv[++i];
	  initial_script_type = 1;
	} 
      else if ( strcmp(argv[i],"-pipes") == 0) 
	{
	  /* old stuff used by geci 
	  int p1,p2;
  	  p1 = atoi(argv[++i]);
	  p2 = atoi(argv[++i]); 
	  C2F(initcom)(&p1, &p2); */
	}
    }
  /* reload history */

  nsp_read_history();
  
  /* provide a default SCI  */
  set_sci_env();

  /* change the default input for dealing with 
   * gtk events  
   */

  SetSciReadFunction( SciGtkReadLine) ;

  /* create temp directory */
  /* C2F(settmpdir)(); XXXX */

  if ( no_window == 0 ) 
    {
      char *shmid= getenv("SHMID");
      /* Not Always initialise gtk */
      gtk_init(&argc,&argv);
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
      /* create a status bar */ 
      /* XXX en attente est-ce utile ? 
	 create_scilab_status();
      */
      SetXsciOn();
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
  if (ierr > 0) return 1;
  /*  execute startup 
   *  and enter main loop 
   */

  if ( no_startup_flag == 0) 
    {
      /* execute a startup */
      strcpy(startup,get_sci_data_strings(1));
      strcat(startup,";quit");
      /* XXXXX C2F(scirun)(startup,strlen(startup)); */
    }
  /* now fill startup with the initial_script if necessary */
  if ( initial_script != NULL ) 
    {
      switch ( initial_script_type ) 
	{
	case 0 : 
	  sprintf(startup,"exec('%s',-1)",initial_script);
	  break;
	case 1 : 
	  sprintf(startup,"%s;",initial_script);
	  break;
	}
    }
  else 
    strcpy(startup," ");
  /* message */  
  /* scilab_status_show("Scilab (C) Inria/Enpc"); */ 
  /* execute the initial script and enter scilab */ 
  /* C2F(scirun)(startup,strlen(startup)); XXXX */
  /* cleaning */
  /* XXXX C2F(sciquit)();*/

  return 0;
}

/* This is to be used for starting gtk when 
 * scilab has been called with scilab -nw 
 * and a menu or graphic window is activated 
 */

void start_sci_gtk() {
  int i;
  C2F(xscion)(&i); 
  if ( i== 0 && GetBasic() == 1) 
    {
      int argc;
      char **argv; 
      if (( argv = create_argv(&argc))== NULL) 
	exit(1);
      /* initialise gtk */
      gtk_init(&argc,&argv);
      SetNotBasic();
    }
}

/* utility */

#define BSIZE 128 

static char ** create_argv(int *argc)
{
  int i;
  char **argv;
  *argc = C2F(iargc)() + 1;
  if ( ( argv = malloc((*argc)*sizeof(char *))) == NULL) return NULL;
  for ( i=0 ; i < *argc ; i++) 
    {
      char buf[BSIZE];
      C2F(getarg)(&i,buf,BSIZE);
      buf[BSIZE-1]='\0';
      strip_blank(buf);
      argv[i] = malloc((strlen(buf)+1)*sizeof(char));
      if ( argv[i] == NULL) return NULL;
      strcpy(argv[i],buf);
#ifdef DEBUG
      fprintf(stderr,"arg[%d] %s\n",i,argv[i]);
#endif 
    }
  return argv;
}

/* utility */

static void strip_blank(char *source)
{
  char *p;
  p = source;
  /* look for end of string */
  while(*p != '\0') p++;
  while(p != source) {
    p--;
    if(*p != ' ') break;
    *p = '\0';
  }
}

/*-------------------------------------------------------
 * Exit function called by some 
 * X11 functions 
 * call sciquit which call clearexit
 *-------------------------------------------------------*/

void sci_clear_and_exit(int n)
{
  if ( no_startup_flag == 0) 
    {
      /* char *quit_script =  get_sci_data_strings(5);
       * XXXX C2F(scirun)(quit_script,strlen(quit_script)); 
       */
    }
  /** save history **/
  nsp_write_history();
  /** clean tmpfiles **/
  clean_tmpdir();
  /** clean ieee **/
#ifdef sun 
#ifndef SYSV
#include <sys/ieeefp.h>
  {
    char *mode, **out, *in;
    ieee_flags("clearall","exeption","all", &out);
  }
#endif 
#endif 
  /* really exit */
  exit(n);
}


/*-------------------------------------------------------
 * usr1 signal : used to transmit a Control C to 
 * scilab 
 *-------------------------------------------------------*/

void sci_usr1_signal(int n) 
{
  controlC_handler(n);
}



/*-------------------------------------------------------
 * Ctrl-Z : stops the current computation 
 *          or the current interface call 
 *-------------------------------------------------------*/

void  sci_sig_tstp(int n)
{
  Scierror("SIGSTP: aborting current computation\r\n");
}

/*-------------------------------------------------------
 * Utility function to try to hide system differences from
 * everybody who used to call killpg() 
 *-------------------------------------------------------*/

int kill_process_group(pid, sig)
    int pid;
    int sig;
{
    return kill (-pid, sig);
}

/*-------------------------------------------------------
 * Syntax 
 *-------------------------------------------------------*/

#if 0

static struct _options {
  char *opt;
  char *desc;
} options[] = {
{ "-help",                 "print out this message" },
{ "-ns",                   "no startup mode " },
{ "-nw",                   "no window mode " },
{ "-display displayname",  "X server to contact" },
{ "-name string",          "client instance, icon, and title strings" },
{ "-xrm resourcestring",   "additional resource specifications" },
{ "-tm string",            "terminal mode keywords and characters" },
{ NULL, NULL }};

static void Syntax (badOption)
    char *badOption;
{
  struct _options *opt;
  int col;

  fprintf (stderr, "%s:  bad command line option \"%s\"\r\n\n",
	   ProgramName, badOption);

  fprintf (stderr, "usage:  %s", ProgramName);
  col = 8 + strlen(ProgramName);
  for (opt = options; opt->opt; opt++) {
    int len = 3 + strlen(opt->opt);	 /* space [ string ] */
    if (col + len > 79) {
      fprintf (stderr, "\r\n   ");  /* 3 spaces */
      col = 3;
    }
    fprintf (stderr, " [%s]", opt->opt);
    col += len;
  }

  fprintf (stderr, "\r\n\nType %s -help for a full description.\r\n\n",
	   ProgramName);
  exit (1);
}

#endif 
/*-------------------------------------------------------
 * Help utility function 
 *-------------------------------------------------------*/

#if 0 

static char *message[] = {
  "Options that start with a plus sign (+) restore the default.",
  NULL
};

static void Help ()
{
  struct _options *opt;
  char **cpp;

  fprintf (stderr, "usage:\n        %s [-options ...] \n\n",
	   ProgramName);
  fprintf (stderr, "where options include:\n");
  for (opt = options; opt->opt; opt++) {
    fprintf (stderr, "    %-28s %s\n", opt->opt, opt->desc);
  }
  putc ('\n', stderr);
  for (cpp = message; *cpp; cpp++) {
    fputs (*cpp, stderr);
    putc ('\n', stderr);
  }
  putc ('\n', stderr);
  exit (0);
}

#endif 

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
 * try to build SCI and MANCHAPTERS if not provided 
 *-------------------------------------------------------*/

static char *sci_env;

extern char * nsp_get_curdir(void);

extern int C2F(scigetcwd)( char **path, int *lpath, int *err);

static void set_sci_env ()
{
  char *p1; 
  if ((p1 = getenv ("SCI")) == (char *) 0)
    {
      sci_env = malloc((strlen(ProgramName)+1+4)*sizeof(char));
      if ( sci_env != NULL) 
	{
	  int i;
	  sprintf (sci_env, "SCI=%s",ProgramName);
	  /* removing the trailing /bin/scilex  */
	  for ( i = strlen(sci_env) ; i >= 0 ; i-- ) 
	    {
	      if ( sci_env[i]== '/' ) 
		{
		  if ( i >= 4 ) sci_env[i-4]= '\0';
		  else { free(sci_env) ; return ;}
		  break;
		}
	    }
	  if ( strcmp(sci_env,"SCI")==0 )  
	    {
	      /* special case when ProgramName = bin/scilex */
	      char *cwd =  nsp_get_curdir();
	      if ( cwd  != NULL  ) 
		{
		  free(sci_env);
		  sci_env = malloc((strlen(cwd)+1+4)*sizeof(char));
		  if ( sci_env != NULL) 
		    {
		      strcpy(sci_env,"SCI=");
		      strcat(sci_env,cwd);
		      putenv(sci_env);
		    }
		  else 
		    putenv("SCI=./");
		}
	      else 
		{
		  putenv("SCI=./");
		}
	    }
	  else 
	    {
	      putenv(sci_env);
	    }
	}
    }    
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

static char *get_shared() 
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

void nsp_create_gtk_toplevel(gint argc, gchar *argv[])
{
  guint32 *xid; 
  char * shm = get_shared() ;
  GtkWidget *vbox,*menubar, *socket_button;

  gtk_init(&argc, &argv);
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




