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

#include <stdio.h>
#include <math.h>
#include <locale.h>

#include "nsp/plistc.h"
#include "nsp/object.h"
#include "nsp/interf.h"
#include "nsp/parse.h"
#include "nsp/seval.h"
#include "Functions.h" 

#include "nsp/version.h"
#include "nsp/machine.h"
#ifdef WITH_GTKGLEXT
#include <gtk/gtkgl.h>
#endif
#include <gtk/gtk.h>
#include "nsp/gtksci.h"
#include <nsp/system.h>

/* FIXME: these is to be in a .h file */
extern void sci_clear_and_exit(int);
extern void primitive_types_register(void);
extern int  nsp_new_frame(const char *);
extern void InitFunctionTable(void);
extern void controlC_handler (int sig);


/**/
static int  no_startup= FALSE;
static int  no_window = FALSE;
static int  show_banner=TRUE;
static int  init_echo = FALSE;
static int  init_disp = FALSE;
static int  init_errcatch = FALSE;
static int  use_texmacs = FALSE;

char *ProgramName = NULL;

static void set_nsp_env (void);
static void nsp_syntax (char *badOption) ;

int poo()
{ 
  double H[]={1,0,0,0,1,0,0,0,1};
  double P[]={1,2,3};
  double C[]={0.6040239,0.0079647, 0.6643966, 0.9832111,0.5321420,0.4138784};
  double D[]={1,1};
  double x[3],lambda[2],war[1000],*ci=NULL,*cs=NULL,f;
  int iwar[1000],ira,mi,md,iter,modo,imp,io=6,info,m;
  
  ira = 0;
  m=3;
  mi=2;
  md=0;
  iter=0; /* => nbre max 14*(N+MI+MD)*/
  modo = 1; 
  imp=30;
  quapro_plcbas (H,P, C,D,ci,cs,&ira,&mi,&md,
		 x,&f, war,iwar, lambda ,&imp,&io,&m,
		 &modo, &info, &iter);
  return 0;
}


int main(int argc, char **argv)
{
  int use_stdlib = TRUE;
  char *initial_script = NULL;
  char *initial_code = NULL;
  int initial_dir = FALSE; 
  char *dir_name=NULL, *dir_pos = NULL;
  char startup[128];
  char *display = NULL;
  int i;
  ProgramName = argv[0];

  /* Initialize reader */
  nsp_intialize_reader();
  /* Initialize evaluation stack **/
  InitStack();
  /* initialize data types */
  primitive_types_register();
  /* initialize primitive table */
  InitFunctionTable();
  /* Initialize macro hash table **/
  nsp_init_macro_table();
  /* Initialize data frame **/  
  if ( nsp_init_frames(argc,argv) == FAIL ) return 1;
  /* reload history */
  nsp_read_history();
  /* provide a default SCI  */
  set_nsp_env();
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
	    display = argv[++i];
	  else 
	    {
	      Sciprintf("missing argument after %s\n",argv[i]);
	    }
	}
      else if (strcmp(argv[i],"-ns") == 0) { no_startup = TRUE ; } /* no start_up */
      else if (strcmp(argv[i],"-nb") == 0) { show_banner = FALSE; } /* no banner */
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
  
  /* FIXME: should be moved here  */
  nsp_gtk_init(argc,argv,no_window);

  /* Load initial macros */
  if ( use_stdlib == TRUE )nsp_enter_macros("SCI/macros",TRUE,FALSE);

  /* locale for LC_NUMERIC must be english type */
  setlocale(LC_NUMERIC,"C");

  if ( use_texmacs == TRUE )
    {
      fprintf(stdout,"nsp session in Texmacs");
      fflush (stdout);
    }

  if ( no_startup == FALSE) 
    {
      /* execute a startup */
      strcpy(startup,get_sci_data_strings(1));
      strcat(startup,";quit");
      /* FIXME C2F(scirun)(startup,strlen(startup)); */
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
	  sci_clear_and_exit(0);
	  return 1;
	}
    }

  if ( initial_script != NULL ) 
    {
      /* execute initial script if given */
      int rep =nsp_parse_eval_file(initial_script,init_disp,init_echo,init_errcatch,TRUE );
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

  poo();
}





/*
 * nsp is called from texmacs ? 
 */

int nsp_from_texmacs(void)
{
  return use_texmacs ;
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

/* FIXME: should be in a .h */
extern char * nsp_get_curdir(void);
extern void set_nsp_tmpdir(void);

static void set_nsp_env (void)
{
  char *sci_env, *p1, *nsp_env; 
  /* TMPDIR */
  set_nsp_tmpdir();
  /* SCI  */
  if ((p1 = nsp_getenv ("SCI")) == (char *) 0)
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
  /* NSP (copy of SCI  */
  if (( p1 = nsp_getenv ("SCI")) != NULL )
    {
      nsp_env = malloc((strlen(p1)+1+4)*sizeof(char));
      if ( nsp_env != NULL) 
	{
	  sprintf (nsp_env, "NSP=%s",p1);
	  putenv(nsp_env);
	}
    }
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

