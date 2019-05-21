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
 * call the parser 
 *--------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <glib.h>

#include <nsp/nsp.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/bhash.h> 
#include <nsp/cells.h> 
#include <nsp/smatrix.h> 
#include <nsp/list.h> 
#include <nsp/file.h> 
#include <nsp/ast.h> 
#include <nsp/datas.h>
#include <nsp/parse.h>
#include <nsp/system.h> /* FSIZE */
#include <nsp/seval.h>
#include <nsp/frame.h>
#include <nsp/nspdatas.h>

#include <signal.h>
#include <setjmp.h>

#ifdef WIN32 
/* no siglongjmp or sigsetjmp  */
#define  siglongjmp(x,y) longjmp(x,y)
#define  sigsetjmp(x,y) setjmp(x)
#endif 

#ifdef STANDALONE 
token Token ;
int debug = 0;
int debugI =0 ;
#endif

static int ParseEvalLoop(Tokenizer *T, int display,int errcatch,int pause);
static int DirParseAndXdrSave (Tokenizer *T,const char *Dir);

/*
 * Set Ctrl_C action while parsing 
 */

static jmp_buf vtjmpbuf ;

void controlC_handler (int sig)
{
  siglongjmp(vtjmpbuf, 1);
}

void controlC_handler_void (int sig)
{
  
}

/**
 * nsp_parse_eval_file:
 * @Str: pathname as string of the file to be executed 
 * @display: %TRUE or %FALSE 
 * @echo: %TRUE or %FALSE 
 * @errcatch: %TRUE or %FALSE 
 * @pause: %TRUE or %FALSE 
 * @mtlb:  %TRUE or %FALSE 
 * 
 * parses and evaluates the contents of the file given by @Str 
 * the return value is strictly negative if an error was found 
 * during execution or if the execution was interrupted by 
 * control-C. Optional values are used to control execution 
 * @display controls the display of instructions output. 
 * @echo can be set to true to echo instructions while they 
 * are executed. @errcatch controls the error catch. 
 * @pause can be used to disable pauses during execution
 *
 * Return value: an integer which gives the status of the execution.
 **/

int nsp_parse_eval_file(char *Str, int display,int echo, int errcatch, int pause,int mtlb)
{
  FILE *input;
  Tokenizer T;
  int rep;
  int cur_echo= nsp_set_echo_input_line(echo);
  Stack *stack = nsp_get_stack();
  char *file_name = NspFileName1(stack);
  if ((input = fopen(Str,"r")) == NULL) 
    {
      /* Only when strerror exists XXXXXXX */
      Scierror("Error: Cannot open file %s for reading\n %s\n"
	       ,Str,strerror(errno));
      return RET_BUG;
    }
  nsp_tokenizer_init(&T);
  T.mtlb = mtlb;
  /* set tokenizer input */
  nsp_tokenizer_file(&T,input);
  /* reset the line counter */
  /* Calling the evaluator */
  if ( stack != NULL) stack->file_name =  Str;
  rep = ParseEvalLoop(&T,display,errcatch,pause);
  if ( rep == RET_EOF || rep == RET_QUIT ) rep = 0;
  if ( rep == RET_CTRLC ) 
    {
      Scierror("execstr: Execution interupted by Ctrl-C\n");
    }
  else if ( rep < 0 ) 
    {
      Scierror("Error: at line %d of file %s\n",T.tokenv.Line,Str);
    }
  if ( errcatch == FALSE ) 
    nsp_error_message_show();
  else 
    nsp_error_message_to_lasterror();
  /* restore current input function */
  nsp_set_echo_input_line(cur_echo);
  if ( stack != NULL) stack->file_name = file_name;
  fclose(input);
  return rep;
}

/**
 * nsp_parse_eval_from_string:
 * @Str: a string
 * @display: %TRUE or %FALSE 
 * @echo: %TRUE or %FALSE 
 * @errcatch: %TRUE or %FALSE 
 * @pause: %TRUE or %FALSE 
 * 
 * parses and evaluates the contents of the string given by @Str 
 * the return value is strictly negative if an error was found 
 * during execution or if the execution was interrupted by 
 * control-C. Optional values are used to control execution 
 * @display controls the display of instructions output. 
 * @echo can be set to true to echo instructions while they 
 * are executed. @errcatch controls the error catch. 
 * @pause can be used to disable pauses during execution
 *
 * Return value: an integer which gives the status of the execution.
 **/

int nsp_parse_eval_from_string(const char *Str,int display,int echo, int errcatch,int pause)
{
  char *file_name;
  Tokenizer T;
  int rep ;
  int cur_echo= nsp_set_echo_input_line(echo);
  nsp_tokenizer_init(&T);
  /* set tokenizer input */
  nsp_tokenizer_string(&T,Str);
  Stack *stack = nsp_get_stack();
  file_name = NspFileName1(stack);
  if ( stack != NULL ) stack->file_name = NULL;
  /* Calling the evaluator */
  rep = ParseEvalLoop(&T,display,errcatch,pause);
  if ( rep == RET_EOF ) 
    {
      rep = 0;
    }
  else if ( rep == RET_CTRLC ) 
    {
      Scierror("execstr: Execution interupted by Ctrl-C\n");
    }
  else if ( rep < 0 && rep != RET_QUIT ) 
    {
      Scierror("Error: Bug detected during execstr\n");
    }
  if ( errcatch == FALSE ) 
    nsp_error_message_show();
  else 
    nsp_error_message_to_lasterror();
  /* restore current input function */
  if ( stack != NULL ) stack->file_name = file_name;
  nsp_set_echo_input_line(cur_echo);
  return rep ;
}

/**
 * nsp_parse_eval_from_smat:
 * @M: a #NspSMatrix
 * @display: %TRUE or %FALSE 
 * @echo: %TRUE or %FALSE 
 * @errcatch: %TRUE or %FALSE 
 * @pause: %TRUE or %FALSE 
 * 
 * parses and evaluates the contents of the string matrix given by @M. 
 * The strings contained in matrix @M are explored in a column order. 
 * The return value is strictly negative if an error was found 
 * during execution or if the execution was interrupted by 
 * control-C. Optional values are used to control execution 
 * @display controls the display of instructions output. 
 * @echo can be set to true to echo instructions while they 
 * are executed. @errcatch controls the error catch. 
 * @pause can be used to disable pauses during execution
 *
 * Return value: an integer which gives the status of the execution.
 **/

int nsp_parse_eval_from_smat(NspSMatrix *M,int display,int echo, int errcatch,int pause)
{
  char *file_name;
  Tokenizer T;
  int rep ;
  int cur_echo= nsp_set_echo_input_line(echo);
  nsp_tokenizer_init(&T);
  nsp_tokenizer_strings(&T,M->S);
  Stack *stack = nsp_get_stack();
  file_name = NspFileName1(stack);
  if ( stack != NULL ) stack->file_name = NULL;
  /* Calling the evaluator */
  rep = ParseEvalLoop(&T,display,errcatch,pause);
  /* normal return  */
  if ( rep == RET_EOF || rep == RET_QUIT ) rep = 0;
  if ( rep == RET_CTRLC ) 
    {
      Scierror("execstr: Execution interupted by Ctrl-C\n");
    }
  else if ( rep < 0 ) 
    {
      int i = T.strings.ind ; 
      if ( i >= 0 && i < M->mn ) 
	{
	  Scierror("Error: execstr error during evaluation of string matrix element %d\n",i+1);
          Scierror("\t%s\n",M->S[i]);
	}
      else 
	{
	  Scierror("Error: Bug detected during execstr\n");
	}
    }

  if ( errcatch == FALSE ) 
    {
      /* nothing to do, we propagate the error messages */
      /* nsp_error_message_show(); */
    }
  else 
    {
      /* fill the contents of lasterror */
      nsp_error_message_to_lasterror();
    }
  /* restore current input function */
  if ( stack != NULL ) stack->file_name = file_name;
  nsp_set_echo_input_line(cur_echo);
  return rep ;
}



/**
 * nsp_parse_eval_from_multistring:
 * @Str: a string
 * @display: %TRUE or %FALSE 
 * @echo: %TRUE or %FALSE 
 * @errcatch: %TRUE or %FALSE 
 * @pause: %TRUE or %FALSE 
 * 
 * parses and evaluates the contents of the string given by @Str. 
 * Since the string potentially contains '\n' the control is 
 * passed to nsp_parse_eval_from_smat(). The return value is thus 
 * similar to the one given by nsp_parse_eval_from_smat().

 * Return value: an integer which gives the status of the execution.
 **/

int nsp_parse_eval_from_multistring(const char *Str,int display,int echo, int errcatch,int pause)
{
  int rep ;
  NspSMatrix *S = nsp_smatrix_split_string(Str,"\n",1);
  rep = nsp_parse_eval_from_smat(S,display,echo,errcatch,pause);
  nsp_smatrix_destroy(S);
  return rep;
}

/**
 * nsp_parse_eval_from_std:
 * @display: %TRUE or %FALSE 
 * 
 * parses and evaluates from standard input stream.
 * the return value is strictly negative if an error was found 
 * during execution or if the execution was interrupted by 
 * control-C. @display controls the display of instructions output. 
 *
 * Return value: an integer which gives the status of the execution.
 **/

int nsp_parse_eval_from_std(int display)
{
  Tokenizer T;
  nsp_tokenizer_init(&T);
  while (1)
    {
      int rep = ParseEvalLoop(&T,TRUE,FALSE,TRUE);
      switch (rep ) 
	{
	case RET_QUIT :
	case RET_EOF: 
	  /* normal stop */
	  return 0;
	case RET_ABORT : 
	  /* evaluation aborted */
	  return RET_ABORT;
	}
    }
  return 0;
}


/**
 * nsp_parse_eval_dir:
 * @Dir: 
 * @Fname: 
 * 
 * parses all the files whose name are found in the file 
 * @Fname which is directory @Dir.
 * For each parsed file, parsed objects are saved 
 * in file in directory @Dir and object named x is saved in a file 
 * named x.bin
 * 
 * Return value: an error status.
 * 
 **/

int nsp_parse_eval_dir(const char *Dir, char *Fname)
{
  int rep=RET_OK;
  char F[2*FSIZE+1], F1[2*FSIZE+1], F2[2*FSIZE+1], dirname[FSIZE+1];
  FILE *f, *SciInput = NULL;
  Stack *stack = nsp_get_stack();
  char *file_name = NspFileName1(stack);
  /** Open the file Dir/Fname  **/
  nsp_path_expand(Dir,dirname,FSIZE);
  sprintf(F,"%s/%s",dirname,Fname);
  if (( f= fopen(F,"r") ) == (FILE *)0 )
    {
      Sciprintf("Error: Cannot open file %s\n",F);
      return RET_BUG;
    }
  while (1) 
    {
      Tokenizer T;
      struct stat buf1,buf2;
      int rep1;
      char name[NAME_MAXL];
      rep1 = fscanf(f,"%s",name);
      if ( rep1 == 0 || rep1 == EOF ) 
	{
	  /* we have reached end-of-file we stop **/ 
	  rep =RET_OK;
	  break;
	}
      /* Parse each macro file **/
      sprintf(F1,"%s/%s",dirname,name);
      if ((SciInput = fopen(F1,"r")) == NULL) 
	{
	  /* Only when strerror exists XXXXXXX **/
	  Sciprintf("Error: Cannot open file %s for reading\n %s\n"
		   ,F1,strerror(errno));
	  rep = RET_BUG;
	  goto end;
	}
      strcpy(F2,F1);
      strcpy(F2+strlen(F2)-3,"bin");
      if ( stat(F2,&buf2)==0) 
	{
	  /* checks if file really need to be binary saved */
	  stat(F1,&buf1);
	  if (buf2.st_mtime > buf1.st_mtime ) 
	    continue ;
	}
      Sciprintf("Processing file: %s\n",F1);
      /* set current file name  **/
      if ( stack != NULL ) stack->file_name = F1;
      /* reset the line counter **/
      nsp_tokenizer_init(&T);
      nsp_tokenizer_file(&T,SciInput);
      /* Calling the evaluator  
       * the macro is sabed with its non expanded name
       */
      rep= DirParseAndXdrSave(&T,dirname);
      fclose(SciInput);
      /* restore current input function **/
      if ( stack != NULL ) stack->file_name = file_name;
      if ( rep < 0 ) 
	{
	  Sciprintf("Error at line %d while processing file: %s\n",T.tokenv.Line,F1);
	  break;
	}
    }
 end: 
  fclose(f);
  return rep;
}


/**
 * nsp_parse_eval_dir_full:
 * @Dir:  
 *
 * parses all the *.sci files found in directory @Dir.
 * For each parsed file, parsed objects are saved 
 * in file in directory @Dir and object named x is saved in a file 
 * named x.bin
 * 
 * Return value: an error status.
 * 
 **/


int nsp_parse_eval_dir_full(const char *Dir)
{
  int rep=RET_OK,flen;
  char F1[FSIZE+1], F2[FSIZE+1], dirname[FSIZE+1];
  FILE *SciInput = NULL;
  Stack *stack = nsp_get_stack();
  char *file_name = NspFileName1(stack);
  /** Open the file Dir/Fname  **/
  nsp_path_expand(Dir,dirname,FSIZE);
  GDir *dir =  g_dir_open(dirname,0,NULL);
  if ( dir == NULL) 
    {
      Sciprintf("Error: Cannot open directory %s\n",Dir);
      return RET_BUG;
    }
  strcpy(F1,dirname);
  flen=strlen(F1);
  while (1) 
    {
      const gchar *fname=  g_dir_read_name(dir);
      if (fname == NULL) break;
      F1[flen]='/'; 
      F1[flen+1]='\0'; 
      strcat(F1,fname);
      if (g_file_test (F1, G_FILE_TEST_IS_REGULAR))
	{
	  Tokenizer T;
	  struct stat buf1,buf2;
	  if ( strlen(fname) >= 4 && strncmp(".sci",fname + strlen(fname)-4,4)==0)
	    {
	      /* Parse macro file */
	      if ((SciInput = fopen(F1,"r")) == NULL) 
		{
		  /* Only when strerror exists XXXXXXX */
		  Sciprintf("Error: Cannot open file %s for reading\n %s\n"
			   ,F1,strerror(errno));
		  rep = RET_BUG;
		  goto end;
		}
	      strcpy(F2,F1);
	      strcpy(F2+strlen(F2)-3,"bin");
	      if ( stat(F2,&buf2)==0) 
		{
		  /* checks if file really need to be binary saved */
		  stat(F1,&buf1);
		  if (buf2.st_mtime > buf1.st_mtime ) 
		    {
		      fclose(SciInput);
		      continue ;
		    }
		}
	      Sciprintf("Processing file: %s\n",F1);
	      /* set current file name  */
	      if ( stack != NULL ) stack->file_name = F1;
	      /* reset the line counter */
	      nsp_tokenizer_init(&T);
	      nsp_tokenizer_file(&T,SciInput);
	      /* Calling the evaluator  
	       */
	      rep= DirParseAndXdrSave(&T,dirname);
	      fclose(SciInput);
	      /* restore current input function */
	      if ( stack != NULL ) stack->file_name = file_name;
	      if ( rep < 0 ) 
		{
		  Sciprintf("Error at line %d while processing file: %s\n",T.tokenv.Line,F1);
		  break;
		}
	    }
	}
    }
 end: 
  g_dir_close (dir);
  return rep;
}


static int DirParseAndXdrSave(Tokenizer *T,const char *Dir)
{
  nsp_datas *data = nsp_get_datas();
  int rep=OK;
  NspFile *F;
  Cell *C;
  NspList *L;
  if (nsp_new_frame("datas") == FAIL ) return FAIL;
  if (( rep = ParseEvalLoop(T,0,FALSE,TRUE)) < 0 && rep != RET_EOF ) 
    {
      nsp_frame_delete() ;
      return rep;
    }
  rep = RET_OK;
  /* explore current frame and store objects in files */
  L= (NspList *) ((NspFrame *) data->L->first->O)->vars;
  C= L->first;
  while ( C != NULLCELL)
    {
      if ( C->O != NULLOBJ )
        {
          char Name[FSIZE+1]; 
          sprintf(Name,"%s/%s.bin",Dir,nsp_object_get_name(C->O));
          if (( F =nsp_file_open_xdr_w(Name)) == NULLSCIFILE) 
	    { rep = RET_BUG; break;} 
          if (nsp_object_xdr_save(F->obj->xdrs,C->O)== FAIL) 
	    { rep = RET_BUG;nsp_file_close_xdr_w(F); break;} 
	  nsp_xdr_save_i(F->obj->xdrs,nsp_no_type_id); /** flag for detecting end of obj at reload **/
          if (nsp_file_close_xdr_w(F) == FAIL) 
	    { 
	      rep = RET_BUG; 
	      nsp_file_destroy(F);
	      break;
	    } 
	  nsp_file_destroy(F);
        }
      C = C->next ;
    }
  nsp_frame_delete() ;
  return rep;
}

/*
 * ParseEvalLoop:
 * @T:  a #Tokenizer
 * @display: an integer 
 * @errcatch: and integer
 * @pause: an integer 
 * 
 * 
 * Parse Eval Loop until quit or EOF or CtrlC reached 
 * Or an error occurs 
 * 
 * Return value: 
 */

static int ParseEvalLoop(Tokenizer *T, int display,int errcatch,int pause)
{
  NspObject **Ob;
  static int count = 0;
  int err,rep,  first =0;
  PList plist = NULLPLIST ;
  Stack *stack = nsp_get_stack();
  int errcatch_cur = stack->val->errcatch;
  int pause_cur = stack->val->pause;
  count++;
  /* must be reset at the end */
  stack->val->errcatch = errcatch; 
  stack->val->pause = pause; 
  if ( count != 1 ) 
    {
      /* Preserve the already stored objects 
       * we assume here that the stack is properly filled
       */
      NspObject **O;
      O = stack->val->S ;
      while ( *O != NULLOBJ) { first++; O++;}
      if (debug) fprintf(stderr,"Recursive parse_eval: protect %d arguments \n",first);
    }
  
  /* just fix a long jump on a top level ParseEval */
  rep = (count == 1) ? sigsetjmp(vtjmpbuf,1) : 0;
  
  if (rep ==0)
    {
      signal (SIGINT, controlC_handler);
      while (1) 
	{
	  /** clean previous parsed expression **/
	  nsp_plist_destroy(&plist);
	  if ((err=nsp_parse(T,NULLBHASH,&plist)) < 0 ) 
	    {
	      /* parse error */
	      count--;
	      if ( errcatch == FALSE ) nsp_error_message_show();
	      nsp_plist_destroy(&plist);
	      goto ret_err;
	    }
	  if (plist != NULLPLIST ) 
	    {
	      /*nsp_plist_print_int(plist); */ /* lisp syntax */
	      /*nsp_plist_pretty_print(plist,0,TRUE,4,FALSE,90);*/ /* pretty print */
	      /*nsp_plist_print(plist,0);*/ /* fully parenthesized */
	      if (debug) nsp_plist_pretty_print(plist,0,TRUE,4,FALSE,90);
	      if (debug) Sciprintf("====Eval===\n");
	      if (Sciprint_diary_on()) nsp_diary_print_plist(plist);
	      if ((err =nsp_eval(plist,*stack,first,0,0,display)) < 0) 
		{
		  /* evaluation error or quit ? */
		  count--;
		  if ( errcatch == FALSE ) nsp_error_message_show();
		  nsp_plist_destroy(&plist);
		  goto ret_err;
		}
	    }
	}
    }
  /* we get here after a Ctrl-C :
   * thus we are back to top level 
   */
  first = 0;
  count=0;
  zero_pause_prompt();
  /* FIXME: reset the pause level to 0 
   */
  signal (SIGINT, controlC_handler_void);
  nsp_read_clean_after_ctrc();
  Sciprintf("\nCtrl C\n");
  /* clean the stack after control-C: 
   * it is dangerous here to try to dealloc non null 
   * object. Thus we just set them to NULL
   */
#ifdef UPDATE_EXEC_DIR  
  stack->val->current_exec_dir[0]='\0';
#endif
  stack->fname = NULL;
  stack->file_name = NULL;
  Ob = stack->val->S + first; 
  while ( *Ob != NULL ) 
    {
      *Ob = NULL;  Ob++;
    }
  stack->val->errcatch= errcatch_cur; 
  stack->val->pause= pause_cur;
  return RET_CTRLC;
 ret_err:
  stack->val->errcatch= errcatch_cur; 
  stack->val->pause= pause_cur;
  return err;
}


/**
 * nsp_parse_expr:
 * @M: a string matrix
 * 
 * parses an expression contained in string @str
 *
 * Return value: a new #PList or %NULLPLIST
 **/

PList nsp_parse_expr(NspSMatrix *M)
{
  char *file_name;
  PList plist = NULLPLIST ;
  Tokenizer T;
  int rep, cur_echo= nsp_set_echo_input_line(FALSE);
  nsp_tokenizer_init(&T);
  nsp_tokenizer_strings(&T,M->S);
  Stack *stack = nsp_get_stack();
  file_name = NspFileName1(stack);
  /* call the parser */
  if ((rep=nsp_parse(&T,NULLBHASH,&plist)) < 0 ) 
    {
      /* parse error */
      nsp_error_message_show();
      nsp_plist_destroy(&plist);
      plist = NULLPLIST;
    }
  /* restore current input function */
  if ( stack != NULL ) stack->file_name = file_name;
  nsp_set_echo_input_line(cur_echo);
  return plist;
}

/***************************************************
 * Parses one expression returned in PList 
 * err is set to 1 2 3 if an error is detected 
 ***************************************************/

int nsp_parse(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  T->tokenv.id = RETURN_OP;
  T->tokenv.FlagEqu = 0;
  if ( T->Getlin(T,nsp_prompt()) == TRUE ) 
    {
      /** Sciprintf("Eof : Quitting ...\n"); **/
      return RET_EOF;
    }
  /* Instruction Scilab */
  if ( T->NextToken(T) == FAIL) return RET_BUG;
  if ( T->tokenv.id != RETURN_OP && T->tokenv.id != COMMA_OP && T->tokenv.id != SEMICOLON_OP
       && T->tokenv.id != COMMA_RET_OP && T->tokenv.id != SEMICOLON_RET_OP )
    {
      if (nsp_parse_top(T,symb_table,plist) == FAIL) return RET_BUG;
    }
  return RET_OK;
}



/***************************************************
 * Evaluates a given function 
 * if first is >= 0 it is used as stack position 
 * to store the function arguments 
 * else a correct position is evaluated and returned in first 
 * 
 ***************************************************/

NspObject *nsp_eval_macro_code(NspPList *PL,NspObject **O,NspList *args,int *first) 
{
  Stack *stack = nsp_get_stack();
  int rhs = 0,lhs = 1,opt = 0,rep;
  NspObject **Ob,*Rep;
  if ( *first < 0 ) 
    {
      *first = 0;
      /* Preserve the already stored objects */ 
      Ob = stack->val->S ;
      while ( *Ob != NULLOBJ) { (*first)++; Ob++;}
    }
  else 
    {
      Ob = stack->val->S + (*first);
    }
  while ( *O != NULLOBJ) 
    {
      *Ob++= *O++;
      rhs++;
    }
  if ( args != NULLLIST ) 
    {
      Cell *cell = args->first; 
      while ( cell != NULLCELL) 
	{
	  if ( cell->O != NULLOBJ) 
	    {
	      *Ob++ = cell->O ; 
	      rhs++;
	    }
	  cell = cell->next;
	}
    }
  rep=nsp_eval_macro((NspObject *) PL,*stack,*first,rhs,opt,lhs);
  if ( rep > 1 ) 
    {
      int i ; 
      /* to many argument returned we ignore the last ones */
      for ( i = 2 ; i <= rep ; i++)nsp_void_object_destroy(&stack->val->S[*first+i]);
    }
  if ( rep <= 0 ) 
    return NULLOBJ ;
  else 
    {
      Rep = stack->val->S[*first];
      stack->val->S[*first] = NULLOBJ;
    }
  return Rep;
}
    

/**
 * nsp_parse_file:
 * @Str: pathname as string of the file to be executed 
 * 
 * parses the contents of the file given by @Str and returns 
 * an ast.
 *
 * Return value: a #NspAst or %NULL
 **/

static NspAst *nsp_parse_full(Tokenizer *T);

NspAst* nsp_parse_file(char *Str) 
{
  NspAst *ast;
  FILE *input;
  Tokenizer T;
  Stack *stack = nsp_get_stack();
  char *file_name = NspFileName1(stack);
  if ((input = fopen(Str,"r")) == NULL) 
    {
      /* Only when strerror exists XXXXXXX */
      Scierror("Error: file %s cannot be opened for reading\n %s\n"
	       ,Str,strerror(errno));
      return NULL;
    }
  nsp_tokenizer_init(&T);
  /* set tokenizer input */
  nsp_tokenizer_file(&T,input);
  /* reset the line counter */
  /* Calling the evaluator */
  if ( stack != NULL ) stack->file_name = Str;
  ast = nsp_parse_full(&T);
  if ( ast == NULL ) 
    {
      Scierror("Error: failed to parse at line %d of file %s\n",T.tokenv.Line,Str);
    }
  if ( stack != NULL ) stack->file_name = file_name;
  fclose(input);
  return ast;
}


/**
 * nsp_parse_from_smat:
 * @M: a #NspSMatrix
 * 
 * parses the contents of the string matrix given by @M. 
 * The strings contained in matrix @M are explored in a column order. 
 * A #NspAst is returned on success and %NULL on error.
 *
 * Return value: a #NspAst or %NULL
 **/

NspAst * nsp_parse_from_smat(NspSMatrix *M)
{
  NspAst *ast;
  char *file_name;
  Tokenizer T;
  nsp_tokenizer_init(&T);
  nsp_tokenizer_strings(&T,M->S);
  Stack *stack = nsp_get_stack();
  file_name = NspFileName1(stack);
  if ( stack != NULL ) stack->file_name = NULL;
  /* Calling the evaluator */
  ast = nsp_parse_full(&T);
  if ( ast == NULL ) 
    {
      int i = T.strings.ind ; 
      if ( i >= 0 && i < M->mn ) 
	{
	  Scierror("Error: parsing of string matrix element %d failed\n",i+1);
          Scierror("\t%s\n",M->S[i]);
	}
      else 
	{
	  Scierror("Error: while parsing a string matrix\n");
	}
    }
  /* restore current input function */
  if ( stack != NULL ) stack->file_name = file_name;
  return ast;
}

/*
 * nsp_parse_full:
 * @T: a #Tokenizer
 * 
 * reads from @T and return an ast 
 * 
 * Return value: a #NspAst or %NULL.
 */

static NspAst *nsp_parse_full(Tokenizer *T)
{
  Stack *stack = nsp_get_stack();
  int errcatch = stack->val->errcatch;
  int rep; 
  NspAst *ast;
  PList plist = NULLPLIST ;
  NspList *args =  NULLLIST;
  if ((args= nsp_list_create("args"))== NULLLIST) return NULLAST;
  while (1) 
    {
      /* clean previous parsed expression */
      nsp_plist_destroy(&plist);
      if ((rep=nsp_parse(T,NULLBHASH,&plist)) < 0 ) 
	{
	  /* parse error */
	  if ( errcatch == FALSE ) nsp_error_message_show();
	  nsp_plist_destroy(&plist);
	  break;
	}
      if (plist != NULLPLIST ) 
	{
	  if ((ast= nsp_plist_to_ast("ast",plist))== NULLAST ) goto err;
	  if ( nsp_list_end_insert(args,NSP_OBJECT(ast))== FAIL) goto err;
	}
    }
  if ( rep == RET_OK || rep == RET_EOF ) 
    {
      /* then create an ast with tolevel object */
      if ((ast= nsp_ast_create(NVOID,STATEMENTS,nsp_list_length(args),NULL,NULL,NULL,NULL,-1,NULL))== NULL)
	goto err;
      nsp_list_destroy(ast->args);
      ast->args = args;
      return ast;
    }
  else
    {
      nsp_list_destroy(args);
      return NULL;
    }
 err:
  nsp_list_destroy(args);
  return NULL;
}
