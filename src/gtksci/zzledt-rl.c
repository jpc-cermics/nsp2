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

#include "nsp/machine.h" 
#include "nsp/sciio.h" 
#include "nsp/gtksci.h" 

static int fd=0;              /* file number for standard in */
static int  use_prompt=1;
static int hist = 1; /* flag to add to history */

extern int checkqueue_nsp_command(void) ;
extern int dequeue_nsp_command(char *buf,int buf_len);



/***********************************************************************
 * line editor: using readline 
 * 
 **********************************************************************/

/* my_getc just try to get one interactive character typed 
 * by scilab user while dealing with gtk/tcltk events 
 * if a soft menu is activated current edited line is aborted and we 
 * jump to quit readline 
 */

static jmp_buf my_env;

static int my_getc (FILE *dummy) 
{ 
  int  i= Xorgetchar();
  if ( checkqueue_nsp_command() == TRUE) 
    {
      /* abort current line aquisition*/
      longjmp(my_env,1);
      return 0;
    }
  return i;
}

int using_readline(void) { return 1;}

static void initialize_readline(void);

static char * dupstr (char *s)
{
  char *r;
  r = malloc (strlen (s) + 1);
  strcpy (r, s);
  return (r);
}

/* this is used but the More statement */

int get_one_char(char *prompt) {
  char buffer[2];
  int buf_size=2, len_line, eof;
  rl_num_chars_to_read = 1;  
  hist = 0; /* not to be added to history */
  SciReadLine1(prompt,buffer,&buf_size,&len_line,&eof);
  rl_num_chars_to_read = 0;
  hist = 1;
  return buffer[0];
}

/* Readline with gtk events */

void SciGtkReadLine(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof)
{
  static int init_flag = TRUE;
  char * line ; 
  static int tty =0;

   if(init_flag) {
     initialize_readline();
     /* the next line is useful for cut and paste 
      * ehrlich juin 2001 
      */
     setvbuf(stdin, NULL, _IONBF, 0); 
     fd=fileno(stdin);
     tty = isatty(fileno(stdin));
     init_flag = FALSE;
   }

   set_is_reading(TRUE);

   if(!tty) {
     /* if not an interactive terminal 
      * use fgets 
      */ 
     fputs("-->",stdout);
     *eof = (fgets(buffer, *buf_size, stdin) == NULL);
     *len_line = strlen(buffer);
     /* remove newline character if there */
     if(buffer[*len_line - 1] == '\n')
       (*len_line)--;
     return;
   }

   rl_getc_function = my_getc;
   
   if ( setjmp(my_env)) 
     {
       /* return from longjmp: we get here if there's a menu command 
	* to be executed by nsp 
	*/
       if ( dequeue_nsp_command(buffer,*buf_size) == FAIL) 
	 {
	   *eof = -1;
	   use_prompt=0;
	   *len_line=0;
	   return;
	 }
       else
	 {
	   *eof = FALSE;
	   use_prompt=0;
	   *len_line = strlen(buffer);
	   return;
	 }
     } 
   else 
     {
       line = readline((use_prompt) ? prompt : "" );
       use_prompt=1;
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
       free(line);
       *eof = FALSE;
     }
   if(get_echo_mode()==0)  set_echo_mode(TRUE);
   set_is_reading(FALSE);
   return;
}

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

void sci_get_screen_size (int *rows,int *cols)
{
  rl_get_screen_size(rows,cols);
}

/* **************************************************************** */
/*                                                                  */
/*                  Interface to Readline Completion                */
/*                                                                  */
/* **************************************************************** */

/* A structure which contains information on the commands this program
   can understand. */

typedef char *hchar ; 
hchar commands[] = {
#include "zzledt.txt"
   (char *)NULL,
};

static char *command_generator (const char *, int);
static char **scilab_completion (const char *, int, int);

/* Tell the GNU Readline library how to complete. */ 

static void initialize_readline (void)
{
  /* Tell the completer that we want a crack first. */
  rl_attempted_completion_function = scilab_completion;
}

static char **scilab_completion (const char * text,int start,int end)
{
  char **matches ; 
  matches = rl_completion_matches (text, command_generator);
  return (matches);
}

static char *command_generator (const char *text, int state)
{
  static int list_index, len;
  char *name;

  /* If this is a new word to complete, initialize now.  This includes
     saving the length of TEXT for efficiency, and initializing the index
     variable to 0. */
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
