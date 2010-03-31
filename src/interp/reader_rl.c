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

#include "nsp/machine.h" 
#include "nsp/sciio.h" 
#include "nsp/gtksci.h" 

static int fd=0;              /* file number for standard in */
static int use_prompt=1;
static int hist = 1; /* flag to add to history */
static void initialize_readline(void);

/* FIXME: should be moved */
extern int checkqueue_nsp_command(void) ;
extern int dequeue_nsp_command(char *buf,int buf_len);

/*-------------------------------------------------------------------
 * line editor: using readline 
 *-------------------------------------------------------------------*/

/* my_getc just try to get one interactive character typed 
 * by scilab user while dealing with gtk/tcltk events 
 * if a soft menu is activated current edited line is aborted and we 
 * jump to quit readline 
 */


void nsp_intialize_reader(void)
{
  /* initialize readline explicitely since 
   * we may use rl_get_screen_size 
   * before calling readline which is supposed 
   * to call rl_initialize(); 
   */
  rl_initialize(); 
  /* local initialization */
  initialize_readline();
}



#ifdef WIN32
/* XXXXXX */
#define sigsetjmp(x,y) setjmp(x)
#define siglongjmp(x,y) longjmp(x,y)
#endif 

static jmp_buf my_env;

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

int using_readline(void) { return 1;}

/* Readline with gtk events 
 * Note that readline is not supposed to be reentrant 
 * and nsp can lead to a reentrant call of readline 
 * (if a callback is activated while in readline and 
 *  this callback contains a pause)
 */

extern void controlC_handler (int sig);
extern void controlC_handler_void (int sig);


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
#ifdef __MINGW32__
    tty=1;
#endif
  }
  
  set_is_reading(TRUE);
  
  if( !tty) 
    { 
      static int first = 0;
      /* if not an interactive terminal use fgets 
       * should be changed for gtk events FIXME
       */ 
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
	  fputs("-nsp->",stdout);
	  fflush (stdout);
	}
      *eof = (fgets(buffer, *buf_size, stdin) == NULL);
      *len_line = strlen(buffer);
      /* remove newline character if there */
      if(*len_line >= 2)
	{
	  if ( buffer[*len_line - 2] == '\r' && buffer[*len_line - 1] == '\n' )
	    *len_line -= 2;
	  else if ( buffer[*len_line - 1] == '\n') (*len_line)--;
	}
      else if( *len_line >= 1) 
	{
	  if ( buffer[*len_line - 1] == '\n') (*len_line)--;
	}
      if ( nsp_from_texmacs() == TRUE )  fputs("\002verbatim: ",stdout);
      return;
    }
  /* reentrant counter */
  enter++;
  rl_getc_function = my_getc;
  
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
	  goto end;
	}
    } 
  else 
    {
      signal (SIGINT, controlC_handler_void);
      line = readline((use_prompt) ? prompt : "" );
      use_prompt=1;
      signal (SIGINT, controlC_handler);
    }
  if (hist && line && *line != '\0') 
    add_history (line);
  
  if ( line == NULL) 
    {
      *len_line= 1;
      strncpy(buffer,"\n",1);
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
 * used to clear line after a pause 
 * (only usefull when readline calls are 
 * reentrant).
 */

void nsp_readline_clear_line(void) 
{
  rl_delete_text(0,rl_end);
}

/*
 * used when reading is interupted by Ctrl-C 
 */

void SciReadClean(void) {}

/*-------------------------------------------------
 *  history 
 *------------------------------------------------*/

int nsp_read_history(void)
{
  return read_history (".nsp_history");
}

int nsp_write_history(void)
{
  int rep = write_history (".nsp_history");
  if ( rep != 0) 
    {
      perror("Pb with ~/.nsp_history ");
    }
  return rep;
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

/*-----------------------------------------------------
 * interface to Readline Completion 
 *-----------------------------------------------------*/

/* A structure which contains information on the commands this program
 * can understand. 
 */

typedef char *hchar ; 
hchar commands[] = {
#include "reader.txt"
  (char *)NULL,
};

#if complete_commands  
static char *command_generator (const char *, int);
static char **scilab_completion (const char *, int, int);
#endif 
/* Tell the GNU Readline library how to complete. */ 

static void initialize_readline (void)
{
  /* Tell the completer that we want a crack first. */
  /* rl_attempted_completion_function = scilab_completion; FIXME: removed temporarily */
}

#if complete_commands 

static char **scilab_completion (const char * text,int start,int end)
{
  char **matches ; 
  matches = rl_completion_matches (text, command_generator);
  return (matches);
}

static char * dupstr (char *s)
{
  char *r;
  r = malloc (strlen (s) + 1);
  strcpy (r, s);
  return (r);
}


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
  while ( (name = commands[list_index]))
    {
      list_index++;
      if (strncmp (name, text, len) == 0)
        return (dupstr(name));
    }
  /* If no names matched, then return NULL. */
  return ((char *)NULL);
}
#endif 
