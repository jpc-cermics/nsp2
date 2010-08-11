/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nsp/math.h"
#include "nsp/object.h"
#include "nsp/sciio.h"
#include "nsp/stack.h"
#include "nsp/smatrix.h"

/**
 * DefScivprintf:
 * @fmt: a string 
 * @ap: variable list of arguments 
 * 
 * Default function used for printing. This function
 * should not be used directly but through Scivprintf
 * 
 * Returns: an integer
 **/

static int  DefScivprintf(const char *fmt, va_list ap)
{
  return  vprintf(fmt, ap );
}

/*
 * Scivprintf: 
 *
 * a variable set to the default function used for 
 * printing. This variable can be changed using SetScilabIO().
 */

static IOVFun Scivprintf = DefScivprintf;

/**
 * Sciprintf: 
 * @fmt: The  format  string
 * @...: format arguments 
 *
 * This function is similar to function printf(format,args,....) and 
 * should be used for nsp output. Sciprintf() redirect output to
 * the function Scivprintf(). Scivprintf is by default set to 
 * DefScivprintf() but using the function SetScilabIO() this default value can 
 * be changed. 
 */

int Sciprintf (const char *fmt, ...)
{
  int n;
  va_list ap;
  va_start(ap,fmt);
  n= Scivprintf(fmt, ap );
  va_end(ap);
  return n;
}

/**
 * Sciprintf1:
 * @indent: integer giving level of indentation
 * @fmt: The  format  string
 * @...: format arguments 
 *
 * Similar to function Sciprintf, with an extra 
 * parameter giving a level of indentation. 
 *   
 */

int  Sciprintf1 (int indent,const char *fmt,...)
{
  int i,n;
  va_list ap;
  va_start(ap,fmt);
  for (i=0 ; i < indent ; i++) Sciprintf(" ");
  n=  Scivprintf( fmt, ap );
  va_end(ap);
  return n+indent;
}

/**
 * do_printf_stdout:
 * @ignore: 
 * @fmt: 
 * @: 
 * 
 * used in do_printf for stream stdout when 
 * we are using a texview widget as terminal. 
 * In a standard xterm this is not necessary.
 * 
 * Returns: 
 **/

int do_printf_stdout(const FILE *ignore,const char *fmt, ...)
{
  int n;
  va_list ap;
  va_start(ap,fmt);
  n= Scivprintf(fmt, ap );
  va_end(ap);
  return n;
}


/**
 * scidebug: 
 * @indent: integer giving level of indentation
 * @fmt: The  format  string
 * @...: format arguments 
 *
 * used for printing debug messages during parsing. 
 *   
 */

int scidebug(int indent,char *fmt,...)
{
  int j,n;
  va_list ap;
  va_start(ap,fmt);
  Sciprintf("\n%d",indent);
  for ( j=0; j < indent; j++) Sciprintf("|");
  n= Scivprintf( fmt, ap );
  va_end(ap);
  return n;
}


/**
 * SetScilabIO:
 * @F: a function used for printing. 
 * 
 * This function is used to change the default function used 
 * for printing. When called with NULL argument default value is restored. 
 * 
 * Returns: previously used function.
 **/

IOVFun SetScilabIO(IOVFun F)
{
  IOVFun g = Scivprintf;
  if ( F == NULL) 
    Scivprintf = DefScivprintf;
  else 
    Scivprintf = F;
  return g;
}

/**
 * DefScifprintf:
 * @f: a file descriptor
 * @fmt: The  format  string
 * @...: format arguments 
 *
 * similar to function fprintf(file,format,args,....) 
 * for nsp output. 
 */

static int  DefScifprintf (FILE *f,const char *fmt,...)
{
  int n;
  va_list ap;
  va_start(ap,fmt);
  n = vfprintf(f, fmt, ap );
  va_end(ap);
  return n;
}

/**
 * SetScifprintf:
 * @F: a function used for printing in a file.  
 * 
 * This function is used to change the current function used 
 * for printing in a file. 
 * 
 * Returns: previously used function
 **/

IOFun2 Scifprintf = DefScifprintf;

IOFun2 SetScifprintf(IOFun2 F)
{
  IOFun2 g = Scifprintf;
  if ( F == NULL) 
    Scifprintf = DefScifprintf;
  else
    Scifprintf = F;
  return g;
}

/* 
 * redirection of nsp print routine in order to 
 * print in a row string matrix 
 * the string matrix is created if nsp_print_string 
 * is NULL;
 *
 */

static int count=0;
static char buf[1024];/* rendre buf dynamique XXX */

/**
 * Sciprint2string:
 * @fmt: a string 
 * @ap: variable list of arguments 
 *
 * This function can be given as argument to  SetScilabIO() 
 * in order to redirect printing to a NspSMatrix  nsp_print_string.
 * 
 * 
 * Returns: an integer.
 **/

static NspSMatrix *nsp_print_string = NULL;

int  Sciprint2string(const char *fmt, va_list ap)
{
  char *line;
  int n,i;
  n= vsnprintf(buf+count,1024-count , fmt, ap );
  if ((line = strchr(buf+count,'\n')) != NULL)
    {
      *line = '\0'; 
      if ( nsp_print_string == NULL) 
	nsp_print_string = nsp_smatrix_create_with_length(NVOID,0,0,-1);
      if ( nsp_print_string != NULL)  nsp_row_smatrix_append_string(nsp_print_string,buf);
      for ( i = 0 ; i < n -( line-(buf+count))-1 ; i++) buf[i]= *(line+1+i); 
      count=  n -( line-(buf+count))-1;
      buf[count]='\0';
    }
  else 
    {
      count+=n;
    }
  return n;
}

/**
 * Sciprint2string_reset:
 * 
 * returns the string matrix containing the output redirection. 
 * And reset the variable nsp_print_string to NULL.
 * 
 * Returns: the value of nsp_print_string
 **/

NspObject *Sciprint2string_reset(void)
{
  NspSMatrix *res;
  if ( count != 0 ) Sciprintf("\n");
  res = nsp_print_string; 
  nsp_print_string = NULL;
  return (NspObject *) res;
}

/**
 * Sciprint_file:
 * @f: a FILE
 * 
 * sets the file that will be used as output file by
 * Sciprint2file() function.
 * 
 * Returns: previous value of nsp_print_file
 **/

static FILE *nsp_print_file = NULL;

FILE * Sciprint_file(FILE *f)
{
  FILE * ret = nsp_print_file;
  nsp_print_file = f;
  return ret;
}

/**
 * Sciprint2file:
 * @fmt: a string 
 * @ap: variable list of arguments 
 *
 * This function can be given as argument to  SetScilabIO() 
 * in order to redirect printing to nsp_print_file.
 * 
 * 
 * Returns: an integer.
 **/

int Sciprint2file(const char *fmt, va_list ap)
{
  return (nsp_print_file != NULL) ? vfprintf(nsp_print_file, fmt, ap ) : 0;
}

/* 
 * a diary redirection 
 *
 */

static FILE *nsp_diary_file = NULL;
static int nsp_diary_echo = TRUE; /* normal out when diary is on ? */

FILE * Sciprint_set_diary(FILE *f,int diary_echo )
{
  FILE *f1 =   nsp_diary_file;
  nsp_diary_file = f;
  nsp_diary_echo = diary_echo; 
  return f1;
}

/**
 * Sciprint_diary:
 * @fmt: a string 
 * @ap: variable list of arguments 
 *
 * This function can be given as argument to  SetScilabIO() 
 * in order to add a diary redirection for nsp output.
 * 
 * 
 * Returns: an integer.
 **/

int Sciprint_diary(const char *fmt, va_list ap)
{
  int n=0;
  /* to standard output */
  if ( nsp_diary_echo ) n=DefScivprintf(fmt,ap); 
  /* to diary file */
  if ( nsp_diary_file)  vfprintf(nsp_diary_file, fmt, ap );
  return n;
}

/**
 * Sciprint_diary:
 * @fmt: a string 
 * @ap: variable list of arguments 
 *
 * This function can be given as argument to  SetScilabIO() 
 * in order to redirect output to diary file.
 * 
 * 
 * Returns: an integer.
 **/

int Sciprint_diary_only (const char *fmt,...)
{
  int n=0;
  va_list ap;
  va_start(ap,fmt);
  if ( nsp_diary_file)  n=vfprintf(nsp_diary_file, fmt, ap );
  va_end(ap);
  return n;
}

/**
 * Sciprint_diary_on:
 * 
 * check if a diary redirection is active.
 * 
 * Returns: %TRUE or %FALSE.
 **/

int Sciprint_diary_on(void) 
{
  return (nsp_diary_file != NULL);
}

/*
 * Scierror :  Default value DefSciprintf 
 *   similar to function printf(format,args,....) 
 *   for Scilab error output 
 *   SetScilabError : function which can be used to 
 *               change scilab Scierror function
 */

static NspSMatrix *lasterror= NULLSMAT;

int  def_nsp_error_vprintf(const char *fmt, va_list ap)
{
  const int size=256;
  char buf[size];
  int n;
  n=  vsnprintf(buf,size, fmt, ap );
  if ( n > -1 ) 
    {
      nsp_row_smatrix_append_string((NspSMatrix *) SciStack.val->error_msg,buf);
    }
  return 0;
}

IOVFun  nsp_error_vprintf = def_nsp_error_vprintf ;


/**
 * SetScilabErrorIO:
 * @F: a function used for error printing. 
 * 
 * This function is used to change the default function used 
 * for error printing. When called with NULL argument default value is restored. 
 * 
 * Returns: previously used function.
 **/

IOVFun SetScilabErrorIO(IOVFun F)
{
  IOVFun g = nsp_error_vprintf;
  if ( F == NULL) 
    nsp_error_vprintf = def_nsp_error_vprintf ;
  else
    nsp_error_vprintf = F;
  return g;
}

/**
 * Scierror: 
 * @fmt: The  format  string
 * @...: format arguments 
 *
 * This function is similar to function printf(format,args,....) and 
 * should be used for nsp error output. Sciprintf() redirect output to
 * the function nsp_error_vprintf(). nsp_error_vprintf is by default set to 
 * def_nsp_error_vprintf() but using the function SetScilabErrorIO() this default value can 
 * be changed. 
 */

int Scierror(const char *fmt,...)
{
  va_list ap;
  va_start(ap,fmt); 
  nsp_error_vprintf(fmt,ap);
  va_end(ap);
  return 0;
}

/**
 * nsp_error_message_show:
 * @void: 
 * 
 * Print and then clear error message 
 **/

void nsp_error_message_show(void)
{
  if ( ((NspSMatrix *)SciStack.val->error_msg)->mn != 0 ) 
    {
      int i;
      for ( i = 0 ; i < ((NspSMatrix *)SciStack.val->error_msg)->mn; i++) 
	Sciprintf("%s",((NspSMatrix *)SciStack.val->error_msg)->S[i]);
      nsp_smatrix_resize((NspSMatrix *) SciStack.val->error_msg,0,0);
    }
}

/**
 * nsp_error_message_clear:
 * @void: 
 * 
 * clear an error message if any.
 **/

void nsp_error_message_clear(void)
{
  if ( ((NspSMatrix *)SciStack.val->error_msg)->mn != 0 ) 
    {
      nsp_smatrix_resize((NspSMatrix *) SciStack.val->error_msg,0,0);
    }
}


/**
 * nsp_error_message_to_lasterror:
 * @void: 
 * 
 * copy recorded error messages to lasterror and clear 
 * error message. 
 *
 **/

void nsp_error_message_to_lasterror(void)
{
  int i;
  if ( lasterror== NULLSMAT ) 
    {
      lasterror =  nsp_smatrix_create(NVOID,0,0,NULL,0);
    }
  else 
    {
      /* keep previous messages */
      /* nsp_smatrix_resize(lasterror,0,0); */
    }
  if ( lasterror != NULLSMAT ) 
    for ( i = 0 ; i < ((NspSMatrix *)SciStack.val->error_msg)->mn; i++) 
      nsp_row_smatrix_append_string(lasterror,((NspSMatrix *)SciStack.val->error_msg)->S[i]);
  nsp_smatrix_resize((NspSMatrix *) SciStack.val->error_msg,0,0);
}


/**
 * nsp_lasterror_get:
 * 
 * returns the value of lasterror
 * 
 * Returns: a NspSMatrix.
 **/

NspSMatrix *nsp_lasterror_get(void) 
{
  return lasterror;
}

/**
 * nsp_lasterror_clear:
 * 
 * reset lasterror to a 0x0 string matrix.
 * 
 **/

void nsp_lasterror_clear(void) 
{
  nsp_smatrix_resize(lasterror,0,0);
}

/*
 * functions to handle echo of typed scilab commands 
 */

static int echo_mode = TRUE;
static int reading = FALSE;

void set_echo_mode(int mode)
{
  echo_mode = mode;
}

int get_echo_mode(void)
{
  return(echo_mode);
}

/*
 * functions to handle echo of typed scilab commands 
 */

void set_is_reading(int mode)
{
  reading = mode;
}

int get_is_reading(void)
{
  return(reading);
}

/*
 * functions to handle echo of typed scilab commands 
 * 
 */

static int nsp_echo_input_line = FALSE;

int nsp_set_echo_input_line(int val)
{
  int val1 =  nsp_echo_input_line ;
  nsp_echo_input_line = val;
  return val1;
}

int nsp_get_echo_input_line(void)
{
  return nsp_echo_input_line;
}
