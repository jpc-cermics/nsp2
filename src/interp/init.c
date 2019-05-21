/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <locale.h>
#include <nsp/nsp.h>
#include <nsp/plist.h>
#include <nsp/plistc.h>
#include <nsp/parse.h>
#include <nsp/seval.h>
#include <nsp/libstab.h>

#include <nsp/version.h>
#include <nsp/machine.h>
#include <gtk/gtk.h>
#include <nsp/gtksci.h>
#include <nsp/nsptcl.h>
#include <nsp/system.h>
#include <nsp/nspthreads.h>

/* FIXME: these is to be in a .h file */
extern void nsp_init_gtk_stack(void);
extern void primitive_types_register(void);
extern void nsp_textview_destroy(void);
extern void controlC_handler (int sig);

/**/
static int  no_startup= FALSE;
static int  no_window = FALSE;
static int  show_banner=TRUE;
static int  init_echo = FALSE;
static int  init_disp = FALSE;
static int  init_errcatch = FALSE;
static int  use_texmacs = FALSE;
static int  use_textview = FALSE;

static char *ProgramName = NULL;

/**/

static void set_nsp_env (void);
static void nsp_syntax (char *badOption) ;
static void set_nsp_home_env(char *nsp_abs_path);
static int nsp_execute_command_for_extension(const char *command_fmt,const char *fname,const char *error_fmt,int *ret);

/* needed when wekbit-gtk is used (in fact just in recent versions) */
#if defined(HAVE_GTHREAD) && defined(HAVE_WEBKIT)
#define ACTIVATE_THREAD
#endif

/* this is for a threaded main in nsp: still experimental */

#ifdef NSP_WITH_MAIN_GTK_THREAD
static void *nsp_top_level_loop_thread(void *args);
GThread *thread1=NULL,*thread2= NULL,*thmain=NULL;
#endif

typedef struct _nsp_thread_data nsp_thread_data;
struct _nsp_thread_data {
  int argc;
  char **argv;
};

int nsp_init_and_loop(int argc, char **argv,int loop)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  GError *error = NULL;
  nsp_thread_data data={argc,argv};
#endif
  int use_stdlib = TRUE;
  char *initial_script = NULL;
  char *initial_code = NULL;
  int initial_dir = FALSE;
  char *dir_name=NULL, *dir_pos = NULL;
  char startup[128];
  /* char *display = NULL; */
  int i;
  ProgramName = (argc > 0 ) ? argv[0]: "unknown";

  /* Scierror is a printf */
  nsp_set_error_io_basic();
  /* Initialize reader */
  nsp_intialize_reader();
  /* Initialize gtk evaluation stack */
  nsp_init_gtk_stack();
  /* initialize data types */
  primitive_types_register();
  /* initialize primitive table */
  nsp_init_function_table();
  /* Initialize acceleration op tables */
  nsp_init_accelerated_tabs();
  /* Initialize macro hash table */
  nsp_init_macro_table();
  /* Initialize evaluation stack */
#ifdef NSP_WITH_MAIN_GTK_THREAD
  thmain = g_thread_self();
  nsp_new_interp(thmain,argc,argv);
#else
  nsp_new_interp(NULL,argc,argv);
#endif
  /* set default error function */
  SetScilabErrorIO(NULL);
  /* MPI */
  /* MPI_Init(&argc,&argv); */
  /* MPI_Init(NULL,NULL); */
  /* provide a default SCI and HOME on win32  */
  set_nsp_env();
  /* reload history: take care that this must be
   * called after set_nsp_env
   */
  nsp_read_history();
  /* ignore error message produced during intialization
   * note that they are just warnings
   */
  nsp_error_message_clear();
  /* scanning options */
  for ( i=0 ; i < argc ; i++)
    {
      if ( strcmp(argv[i],"-nw") == 0) { no_window = TRUE; } /* no window mode */
      else if (strcmp(argv[i],"-display") == 0)
	{
	  /* X11 display */
	  if ( i+1 < argc )
	    {
	      /* display = argv[++i];*/ ++i;

	    }
	  else
	    {
	      Sciprintf("missing argument after %s\n",argv[i]);
	    }
	}
      else if (strcmp(argv[i],"-ns") == 0) { no_startup = TRUE ; } /* no start_up */
      else if (strcmp(argv[i],"-nb") == 0) { show_banner = FALSE; } /* no banner */
      else if (strcmp(argv[i],"-tv") == 0)
	{
	  use_textview = TRUE;
	}
      else if (strcmp(argv[i],"-f") == 0)
	{ /* execute file */
	  if ( i+1 < argc )
	    initial_script = argv[++i];
	  else
	    {
	      Sciprintf("missing argument after %s\n",argv[i]);
	    }
	}
      else if (strcmp(argv[i],"-e") == 0)
	{
	  if ( i+1 < argc )
	    initial_code = argv[++i];  /* execute exp */
	  else
	    {
	      Sciprintf("missing argument after %s\n",argv[i]);
	    }
	}
      else if (strcmp(argv[i],"-dir") == 0)   /* macros files .sci -> .bin */
	{
	  if ( i+1 < argc )
	    {
	      initial_dir = TRUE;
	      dir_name = argv[++i];
	      if ( i+1 < argc ) dir_pos = argv[++i] ;
	    }
	  else
	    {
	      Sciprintf("missing argument after %s\n",argv[i]);
	    }
	  use_stdlib = FALSE;
	}
      else if (strcmp(argv[i],"-echo") == 0) {init_echo = TRUE ; } /* echo mode for exec() */
      else if (strcmp(argv[i],"-show") == 0) {init_disp = TRUE ; } /* display mode for exec() */
      else if (strcmp(argv[i],"-errcatch") == 0) {init_errcatch = TRUE ;  } /* errcatch mode */
      else if (strcmp(argv[i],"-help") == 0) { nsp_syntax(NULL); exit(1); } /* help */
      else if (strcmp(argv[i],"-texmacs") == 0) {use_texmacs = TRUE ;} /* run from texmacs */
      else if (strcmp(argv[i],"-nostdlib") == 0) {use_stdlib = FALSE ;} /* do not add macros */
    }

#ifdef WIN32
  if ( no_window == FALSE )
    {
      /* force textview on windows */
      use_textview = TRUE;
    }
#endif

  nsp_set_in_text_view(use_textview);

#if (defined(NSP_WITH_MAIN_GTK_THREAD) || defined(ACTIVATE_THREAD))
  /* init threads but useless after version 2.32
   * causes errors on win32 versions
   */
#if !GLIB_CHECK_VERSION (2,32,0)
    {
      g_thread_init(NULL);
      gdk_threads_init();
    }
#endif
#endif

  /* FIXME: should be moved here  */
  nsp_gtk_init(argc,argv,no_window,use_textview);

  /* #define NSP_ENTRY_INPUT_TEST   */
#ifdef NSP_ENTRY_INPUT_TEST
  if ( no_window == FALSE )
    term_output(argc, argv);
#endif

  /* Load initial macros */
  if ( use_stdlib == TRUE ) nsp_enter_macros("SCI/macros",TRUE,FALSE);

  /* locale for LC_NUMERIC must be english type */
  setlocale(LC_NUMERIC,"C");

  if ( use_texmacs == TRUE )
    {
      fprintf(stdout,"nsp session in Texmacs");
      fflush (stdout);
    }

  if ( no_startup == FALSE)
    {
      char fname_expanded[FSIZE+1];
      FILE *input;
      /* try to execute a user startup */
      strcpy(startup,get_sci_data_strings(1));
      nsp_path_expand(startup,fname_expanded,FSIZE);
      if ((input = fopen(fname_expanded,"r")) != NULL)
	{
	  int rep;
	  fclose(input);
	  rep=nsp_parse_eval_file(fname_expanded,init_disp,init_echo,init_errcatch,TRUE,FALSE );
	  if  ( rep < 0 )
	    {
	      Sciprintf("Warning: error during execution of the startup script %s\n",startup);
	      nsp_error_message_show();
	    }
	}
    }

  if ( initial_dir  == TRUE  )
    {
      /* -dir dirname filename
       * or
       * -dir dirname filename
       * FIXME: dirname must be an absolute file name
       * but can contain SCI, NSP etc.....
       */
      int rep;
      if (dir_pos == NULL)
	rep =nsp_parse_eval_dir_full(dir_name);
      else
	rep =nsp_parse_eval_dir(dir_name,dir_pos);
      if ( rep >= 0 || rep == RET_QUIT )
	{
	  sci_clear_and_exit(0);
	  return 0;
	}
      else
	{
	  sci_clear_and_exit(1);
	  return 1;
	}
    }

  if ( initial_script != NULL )
    {
      const char *extension;
      char fname_expanded[FSIZE+1];
      nsp_path_expand(initial_script,fname_expanded,FSIZE);
      /* execute initial script if given 
       * try to detect cos files 
       */
      extension = nsp_get_extension(initial_script);
      if ( strcmp(extension,".cos") == 0  || strcmp(extension,".cosf") == 0) 
	{
	  /* try to start scicos */
	  int ret;
	  if ( nsp_execute_command_for_extension("load_toolbox('scicos-4.4');scicos('%s');",
						 initial_script,
						 "Error: while loading scicos file %s\n",&ret) 
	       == FAIL)
	    return ret;
	}
      else if ( strcmp(extension,".mdl")== 0 )
	{
	  /* try to start scicos with a mdl file */
	  int ret;
	  const char *cmd="load_toolbox('scicos-4.4');scs_m = do_load_mdl(fname='%s',check_companion=%%f);if isempty(scs_m) then quit;end;scicos(scs_m);";
	  if ( nsp_execute_command_for_extension(cmd,
						 initial_script,
						 "Error: while loading scicos file %s\n",&ret) 
	       == FAIL)
	    return ret;
	}
      else if ( strcmp(extension,".mat")== 0 )
	{
	  /* try to load a matfile */
	  int ret;
	  if ( nsp_execute_command_for_extension("loadmatfile('%s');",
						 initial_script,
						 "Error: while loading scicos file %s\n",&ret) 
	       == FAIL)
	    return ret;
	}
      else if ( strcmp(extension,".bin")== 0 )
	{
	  /* try to load a file */
	  int ret;
	  if ( nsp_execute_command_for_extension("load('%s');",
						 initial_script,
						 "Error: while loading scicos file %s\n",&ret) 
	       == FAIL)
	    return ret;
	}
      else
	{
	  int rep =nsp_parse_eval_file(fname_expanded,init_disp,init_echo,init_errcatch,TRUE,FALSE );
	  if ( rep == RET_QUIT )
	    {
	      sci_clear_and_exit(0);
	      return 0;
	    }
	  else if  ( rep < 0 )
	    {
	      if ( init_errcatch== FALSE )
		{
		  Scierror("Error: during evaluation of %s\n",initial_script);
		  nsp_error_message_show();
		}
	      sci_clear_and_exit(1);
	      return 1;
	    }
	}
    }
  
  if ( initial_code != NULL )
    {
      /* then execute initial given code
       * we quit with status equal to 1 when an error occurs
       */
      int rep =nsp_parse_eval_from_string(initial_code,init_disp,init_echo,init_errcatch,TRUE );
      if ( rep == RET_QUIT )
	{
	  sci_clear_and_exit(0);
	  return 0;
	}
      else if  ( rep < 0 )
	{
	  if ( init_errcatch== FALSE )
	    {
	      Scierror("Error: during evaluation of %s\n",initial_code);
	      nsp_error_message_show();
	    }
	  sci_clear_and_exit(1);
	  return 1;
	}
    }

  /* stop after initialization if requested */
  if ( loop == FALSE ) return 0;

  /* enter normal loop
   * but we only want to stop on RET_QUIT and RET_EOF
   * i.e RET_ABORT should not stop.
   */

#ifdef NSP_WITH_MAIN_GTK_THREAD
#if GLIB_CHECK_VERSION (2,32,0)
  if (! (thread1= g_thread_try_new("thread1",nsp_top_level_loop_thread,&data, &error)))
    {
      g_printerr ("Failed to create thread: %s\n", error->message);
      return 1;
    }
  if (!(thread2=g_thread_try_new("thread2",nsp_top_level_loop_thread,&data, &error)))
    {
      g_printerr ("Failed to create thread: %s\n", error->message);
      return 1;
    }
#else
  if (! (thread1= g_thread_create(nsp_top_level_loop_thread,&data, FALSE, &error)))
    {
      g_printerr ("Failed to create thread: %s\n", error->message);
      return 1;
    }
  if (!(thread2=g_thread_create(nsp_top_level_loop_thread,&data, FALSE, &error)))
    {
      g_printerr ("Failed to create thread: %s\n", error->message);
      return 1;
    }
#endif
  /* enter the GTK main loop */
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  gtk_main();
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave();
#endif
#else
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  while (1)
    {
      if (  nsp_parse_eval_from_std(TRUE) == 0) break;
    }
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave();
#endif
#endif
  sci_clear_and_exit(0);
  return 0;
}

#ifdef NSP_WITH_MAIN_GTK_THREAD
static void *nsp_top_level_loop_thread(void *args)
{
  nsp_thread_data *data= args;
  int rep;
  nsp_new_interp(g_thread_self(),data->argc,data->argv);
  while (1)
    {
      /* FIXME : this is too much but we have to protect gtk_calls
       * gdk_threads_enter() etc.... should be moved in other places
       */
      rep = nsp_parse_eval_from_std(TRUE);
      if (  rep == 0) break;
    }
  sci_clear_and_exit(0);
  return NULL;
}
#endif

/*
 * nsp is called from texmacs ?
 */

int nsp_from_texmacs(void)
{
  return use_texmacs ;
}


/* utility to execute commands associated to file name extension 
 */

static int nsp_execute_command_for_extension(const char *command_fmt,const char *fname,const char *error_fmt,int *ret)
{
  char fname_expanded[FSIZE+1];
  char initial_code[FSIZE+1+128];
  int rep;
  nsp_path_expand(fname,fname_expanded,FSIZE);
  sprintf(initial_code,command_fmt,fname_expanded);
  rep =nsp_parse_eval_from_string(initial_code,init_disp,init_echo,init_errcatch,TRUE );
  if ( rep == RET_QUIT )
    {
      sci_clear_and_exit(0);
      *ret=0; 
      return FAIL;
    }
  else if  ( rep < 0 )
    {
      if ( init_errcatch== FALSE )
	{
	  Scierror(error_fmt,fname);
	  nsp_error_message_show();
	}
      sci_clear_and_exit(1);
      *ret=1;
      return FAIL;
    }
  return OK;
}

/*-------------------------------------------------------
 * Exit function called by some
 * X11 functions
 * call sciquit which call clearexit
 *-------------------------------------------------------*/

void sci_clear_and_exit(int n)
{
  if ( no_startup == FALSE)
    {
      /* char *quit_script =  get_sci_data_strings(5);
       * XXXX C2F(scirun)(quit_script,strlen(quit_script));
       */
    }
  /* save history **/
  nsp_write_history();
  /* clean tmpfiles **/
  clean_tmpdir();
  /* clean ieee **/
#ifdef sun
#ifndef SYSV
#include <sys/ieeefp.h>
  {
    char *mode, **out, *in;
    ieee_flags("clearall","exeption","all", &out);
  }
#endif
#endif
  /* clear the texview if we have one */
  nsp_textview_destroy();
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
 * Syntax
 *-------------------------------------------------------*/

static struct _options {
  char *opt;
  char *desc;
} options[] = {
  { "-help",                 "print out this message" },
  { "-ns",                   "no startup mode " },
  { "-nw",                   "no window mode " },
  { "-display displayname",  "X server to contact" },
  { "-f file",               "execute script given by file"},
  { "-e string",             "execute code given by string (-e is executed after -f) "},
  { "-name string",          "client instance, icon, and title strings" },
  { "-errcatch",             "execute -f file and -e script in errcatch mode" },
  { "-echo",                 "execute -f file and -e script in echo mode" },
  { "-show",                 "execute -f file and -e script in show mode" },
  { "-dir",                  "load and save of sci files in a directory" },
  { NULL, NULL }};

static void nsp_syntax (char *badOption)
{
  struct _options *opt;
  int col;
  if ( badOption != NULL )
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


/*
 * Build environment variables
 * SCI, NSP and TMPDIR
 *
 */

static void set_nsp_env (void)
{
  int i;
  const char *p1;
  char *nsp_abs_path=NULL, *pname =ProgramName ;
  /* TMPDIR */
  set_nsp_tmpdir();
  /* SCI  */
  if ((p1 = nsp_getenv ("SCI")) != (char *) 0)
    {
      nsp_setenv("NSP",p1);
      return;
    }
  /* we should check here if path is absolute or not
   * with nsp_get_path_type
   * fprintf(stderr,"No SCI trying with %s\n",ProgramName);
   */
  /* fprintf(stderr,"No SCI trying with %s\n",pname); */
  if ( pname[0]== '.' && pname[1] == '/') pname += 2;
  nsp_abs_path  = nsp_absolute_file_name(pname);
  /* fprintf(stderr,"Expanded to %s\n",nsp_abs_path);  */
  if ( nsp_abs_path == NULL) return;
  for ( i=0 ; i < strlen(nsp_abs_path) ;i++)
    {
      if ( nsp_abs_path[i]=='\\') nsp_abs_path[i] ='/';
    }
  /* going upward and try removing the trailing /bin/program-name  */
  for ( i = strlen(nsp_abs_path) ; i >= 0 ; i-- )
    {
      if ( nsp_abs_path[i]== '/'  )
	{
	  nsp_abs_path[i]= '\0';
	  if (strcmp(nsp_abs_path+i+1,"bin")== 0 )
	    {
	      break;
	    }
	}
    }
  /* fprintf(stderr,"Changed to %s\n",nsp_abs_path);  */
  nsp_setenv("SCI",nsp_abs_path);
  nsp_setenv("NSP",nsp_abs_path);
  /* be sure that home exists */
  set_nsp_home_env(nsp_abs_path);
  free(nsp_abs_path);
}

/* this is for WIN32 */

static void set_nsp_home_env(char *nsp_abs_path)
{
#ifdef WIN32
  int k;
  char HOME[FSIZE+1];
  const char *hd, *hp;
  if ( nsp_getenv("HOME") != NULL)  return;
  hd = nsp_getenv ("HOMEDRIVE");
  hp = nsp_getenv ("HOMEPATH");
  if ( hp == NULL || hd == NULL)
    sprintf(HOME,"%s",nsp_abs_path);
  else
    sprintf(HOME,"%s%s",hd,hp);
  for (k=0 ; k < strlen(HOME) ;k++) if ( HOME[k]=='\\') HOME[k]='/';
  nsp_setenv("HOME",HOME);
#endif
}


#ifdef FORTRAN_MAIN

/* utility */

#define BSIZE 128

/**
 * create_argv:
 * @argc:
 *
 * A utility which rebuild @argv and @argc when the
 * main program was a Fortran main
 *
 * Return value:
 **/

/* two functions coming from Fortran */

extern int C2F(getarg)(int *,char *,long int l);
extern int C2F(iargc)(void);
static void strip_blank(char *source);

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


/**
 * strip_blank:
 * @source:
 *
 * removes trailing white spaces.
 **/

static void strip_blank(char *source)
{
  char *p = source;
  /* look for end of string */
  while( *p != '\0') p++;
  while( p != source) {
    p--;
    if(*p != ' ') break;
    *p = '\0';
  }
}


#endif /*  FORTRAN_MAIN  */
