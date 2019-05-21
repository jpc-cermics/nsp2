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
 * This file provides a way to use readline in nsp and dealing with 
 * gtk events while waiting for characters. 
 * The function my_getc is passed to readline as through the rl_getc_function;
 * while in  my_getc we check for events, enqueued commands and typed characters. 
 * 
 *--------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h> 
#include <readline/readline.h>
#include <readline/history.h>
#include <setjmp.h>
#include <unistd.h> /* for isatty */
#include <signal.h>
#include <glib.h>

#include "nsp/machine.h" 
#include "nsp/sciio.h" 
#include "nsp/gtksci.h" 
#include <nsp/system.h> /* FSIZE */
#include <nsp/nsptcl.h> /* FSIZE */

#ifdef WIN32
#define sigsetjmp(x,y) setjmp(x)
#define siglongjmp(x,y) longjmp(x,y)
#endif 

/* FIXME: should be moved */
extern int checkqueue_nsp_command(void) ;
extern int dequeue_nsp_command(char *buf,int buf_len);
extern void controlC_handler (int sig);
extern void controlC_handler_void (int sig);

static int fd=0;              /* file number for standard in */
static int use_prompt=1;
static int hist = 1; /* flag to add to history */
static jmp_buf my_env;

/* #define NSP_READLINE_COMPLETION */

#ifdef NSP_READLINE_COMPLETION
static char *command_generator (const char *, int);
static char **nsp_completion (const char *, int, int);
#endif 

static char *nsp_fgets(char *str, int n, FILE *stream);

/**
 * nsp_intialize_reader:
 * @void: 
 * 
 * initialize readline explicitely since we may use rl_get_screen_size 
 * before calling readline which is supposed 
 * to call rl_initialize(); 
 *
 **/

void nsp_intialize_reader(void)
{
  rl_initialize(); 
  /* local initialization */
#ifdef NSP_READLINE_COMPLETION
  rl_attempted_completion_function = nsp_completion;
#endif 
}

/**
 * my_getc:
 * @dummy: 
 * 
 * This function is used by readline as rl_getc_function.
 * 
 * Returns: a character in an integer. 
 **/

static int my_getc (FILE *dummy) 
{ 
  int  i= Xorgetchar();
  if ( checkqueue_nsp_command() == TRUE) 
    {
      /* abort current line aquisition */
      siglongjmp(my_env,1);
      return 0;
    }
  return i;
}

/**
 * using_readline:
 * @void: 
 * 
 * 
 * 
 * Returns:  %TRUE or %FALSE 
 **/

int using_readline(void) { return 1;}

/**
 * nsp_defscireadline_rl:
 * @T: 
 * @prompt: 
 * @buffer: 
 * @buf_size: 
 * @len_line: 
 * @eof: 
 * 
 * Readline with gtk events 
 * Note that readline is not supposed to be reentrant 
 * and nsp can lead to a reentrant call of readline 
 * (if a callback is activated while in readline and 
 *  this callback contains a pause)
 * 
 **/

void nsp_defscireadline_rl(Tokenizer *T,char *prompt, char *buffer, int *buf_size, int *len_line, int *eof)
{
  static int tty =0, init_flag = TRUE, enter=0;
  char * line=NULL ; 
  if(init_flag) {
    /* the next line is useful for cut and paste 
     * ehrlich juin 2001 
     */
    setvbuf(stdin, NULL, _IONBF, 0); 
    fd=fileno(stdin);
    tty = isatty(fileno(stdin));
    init_flag = FALSE;
#if (defined(__MINGW32__) || defined(__APPLE__))
    /* 2017: apple added since the result is wrong when starting a bundle */
    tty=1;
#endif
  }
  
  set_is_reading(TRUE);
  
  /* reentrant counter */
  enter++;
  rl_getc_function = my_getc;

  if( !tty) 
    {
      /* if not an interactive terminal use fgets */
      static int first = 0;
      if ( nsp_from_texmacs() == TRUE )
	{
	  if ( first == 0 ) 
	    {
	      fputs("\002verbatim:",stdout);
	      first++;
	    }
	  /* FIXME: prompt should take care of pause */
	  fputs("\002channel:prompt\005-nsp->\005",stdout);
	  fflush (stdout);
	}
      else 
	{
	  fputs(prompt,stdout);
	  fflush (stdout);
	}
      *eof = (nsp_fgets(buffer, *buf_size -1, stdin) == NULL);
      *len_line = strlen(buffer);
      if ( nsp_from_texmacs() == TRUE )  fputs("\002verbatim: ",stdout);
      /* fprintf(stdout,"[[%s]]\n",buffer);  */
      return;
    }

  if ( sigsetjmp(my_env,1)) 
    {
      /* return from longjmp: we get here if there's a menu command 
       * to be executed by nsp 
       */
      if ( dequeue_nsp_command(buffer,*buf_size) == FAIL) 
	{
	  *eof = -1;
	  use_prompt=0;
	  *len_line=0;
	  goto end;
	}
      else
	{
	  *eof = FALSE;
	  use_prompt=0;
	  *len_line = strlen(buffer);
	  add_history (buffer);
	  goto end;
	}
    } 
  else 
    {
      {
	signal (SIGINT, controlC_handler_void);
	line = readline((use_prompt) ? prompt : "" );
	use_prompt=1;
	signal (SIGINT, controlC_handler);
      }
    }
  if (hist && line && *line != '\0') 
    {
      add_history (line);
    }
  
  if ( line == NULL) 
    {
      *len_line= 1;
      /* strncpy(buffer,"\n",1); */
      buffer[0]= '\n';
      *eof = FALSE;
    }
  else 
    {
      *len_line= strlen(line);
      strncpy(buffer,line,*buf_size);
      /* Do not free line on reentrant calls  */
      if ( enter == 1 ) 
	{
	  free(line);
	  line = NULL;
	}
      *eof = FALSE;
    }
  if(get_echo_mode()==0)  set_echo_mode(TRUE);
  set_is_reading(FALSE);
 end: 
  enter--;
  return;
}

/* 
 * similar to fgets but events are checked inside my_getc 
 */

static char *nsp_fgets(char *str, int n, FILE *stream)
{
  int count = 0, c;
  while (1)
    {
      if (count == n-1 )
	{
	  str[count]='\0';
	  return str;
	}
      if ((c= my_getc(stream))== 0 ) return NULL;
      str[count]=c;
      if ( c == '\n')
	{
	  str[count]='\0';
	  if ( count >=1 && str[count-1]  == '\r') str[count-1]='\0';
	  return str;
	}
      count++;
    }
}

/*
 * used to clear line after a pause 
 * (only usefull when readline calls are 
 * reentrant).
 */

void nsp_readline_clear_line(void) 
{
  rl_delete_text(0,rl_end);
}

/**
 * SciReadClean:
 * @void: 
 * 
 * used when reading is interupted by Ctrl-C 
 * 
 **/

void nsp_read_clean_after_ctrc(void) {}

/**
 * nsp_read_history:
 * @void: 
 * 
 * Returns: 
 **/

#define NSP_HISTORY "$HOME/.nsp_history"

int nsp_read_history(void)
{
  char history[FSIZE+1];
  nsp_path_expand(NSP_HISTORY,history,FSIZE);
  return read_history (history);
}

/**
 * nsp_write_history:
 * @void: 
 * 
 * Returns: 
 **/

#ifdef WIN32

int nsp_write_history(void)
{
  int rep;
  char *S[]={NULL,NULL};
  char history[FSIZE+1];
  nsp_path_expand(NSP_HISTORY,history,FSIZE);
  S[0]=history;
  nsp_file_delete_cmd(1,S,1);
  if ((rep= write_history (history)) != 0) 
    {
      Sciprintf("Error: unable to write history file %s\n",NSP_HISTORY);
    }
  return rep;
}
#else
int nsp_write_history(void)
{
  int rep;
  char history[FSIZE+1];
  nsp_path_expand(NSP_HISTORY,history,FSIZE);
  if ((rep= write_history (history)) != 0) 
    {
      Sciprintf("Error: unable to write history file %s\n",NSP_HISTORY);
    }
  return rep;
}
#endif

/**
 * nsp_clear_history:
 * @void: 
 * 
 * 
 * 
 * Returns: 
 **/

void nsp_clear_history(void) 
{
  /* */
  clear_history ();
}

/*----------------------------------------------------------------------
 * initialise the io sequences
 *----------------------------------------------------------------------*/

extern void  nsp_text_view_screen_size(int *rows,int *cols);
static int in_text_view = FALSE;

void nsp_set_in_text_view(int value)
{
  in_text_view = value;
}

int nsp_get_in_text_view(void)
{
  return   in_text_view ;
}



void sci_get_screen_size (int *rows,int *cols)
{
  if (   in_text_view  == TRUE )
    {
      nsp_text_view_screen_size(rows,cols);
      /* Sciprintf("Using TRUE return r=%d c=%d\n",*rows,*cols); */
    }
  else
    {
      rl_get_screen_size(rows,cols);
    }
}

/* A structure which contains information on the commands this program
 * can understand. 
 */

#ifdef NSP_READLINE_COMPLETION

static char *commands_pool[] = {
#include "reader.txt"
  (char *)NULL,
};



/**
 * nsp_completion:
 * @text: 
 * @start: 
 * @end: 
 * 
 * 
 * 
 * Returns: an array of strings 
 **/

static char **nsp_completion (const char * text,int start,int end)
{
  char **matches ; 
  matches = rl_completion_matches (text, command_generator);
  return (matches);
}

/**
 * command_generator:
 * @text: 
 * @state: 
 * 
 * 
 * Returns: %NULL or a possible completion.
 **/

static char *command_generator (const char *text, int state)
{
  static int list_index, len;
  char *name;
  /* If this is a new word to complete, initialize now.  This includes
   * saving the length of TEXT for efficiency, and initializing the index
   * variable to 0. 
   */
  if (!state)
    {
      list_index = 0;
      len = strlen (text);
    }
  /* Return the next name which partially matches from the command list. */
  while ( (name = commands_pool[list_index]))
    {
      list_index++;
      if (strncmp (name, text, len) == 0)
        return ( g_strdup(name));
    }
  /* If no names matched, then return NULL. */
  return ((char *)NULL);
}

#endif 
