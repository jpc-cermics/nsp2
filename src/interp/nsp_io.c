/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
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
 * Sciprintf: 
 * @fmt: The  format  string
 * @...: format arguments 
 *
 * similar to function printf(format,args,....) 
 * for nsp output. SetScilabIO: function which can be used to 
 * change scilab Sciprintf function 
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

int  DefScivprintf(const char *fmt, va_list ap)
{
  return  vprintf(fmt, ap );
}

IOVFun Scivprintf = DefScivprintf;

IOVFun SetScilabIO(IOVFun F)
{
  IOVFun g = Scivprintf;
  Scivprintf = F;
  return g;
}

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


/**
 * DefScifprintf:
 * @f: a file descriptor
 * @fmt: The  format  string
 * @...: format arguments 

 * similar to function fprintf(file,format,args,....) 
 * for nso output 
 */

int  DefScifprintf (FILE *f,const char *fmt,...)
{
  int n;
  va_list ap;
  va_start(ap,fmt);
  n = vfprintf(f, fmt, ap );
  va_end(ap);
  return n;
}

IOFun2 Scifprintf = DefScifprintf;

IOFun2 SetScifprintf(IOFun2 F)
{
  IOFun2 g = Scifprintf;
  Scifprintf = F;
  return g;
}

/**
 * Sciprintf1:
 * @indent: integer giving level of indentation
 * @fmt: The  format  string
 * @...: format arguments 
 * Similar to function Sciprintf, with an extra 
 * parameter giving a level of indentation 
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

/* 
 * redirection of nsp print routine in order to 
 * print in a row string matrix 
 * the string matrix is created if nsp_print_string 
 * is NULL;
 *
 */

static int count=0;
static char buf[1024];/* rendre buf dynamique XXX */
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

NspObject *Sciprint2string_reset()
{
  NspSMatrix *res;
  if ( count != 0 ) Sciprintf("\n");
  res = nsp_print_string; 
  nsp_print_string = NULL;
  return (NspObject *) res;
}


/* 
 * redirection of nsp print routine in order to 
 * print in a file 
 *
 */

static FILE *nsp_print_file = NULL;

FILE * Sciprint_file(FILE *f)
{
  FILE * ret = nsp_print_file;
  nsp_print_file = f;
  return ret;
}

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

int Sciprint_diary(const char *fmt, va_list ap)
{
  int n=0;
  /* to standard output */
  if ( nsp_diary_echo ) n=DefScivprintf(fmt,ap); 
  /* to diary file */
  if ( nsp_diary_file)  vfprintf(nsp_diary_file, fmt, ap );
  return n;
}

int Sciprint_diary_only (const char *fmt,...)
{
  int n=0;
  va_list ap;
  va_start(ap,fmt);
  if ( nsp_diary_file)  n=vfprintf(nsp_diary_file, fmt, ap );
  va_end(ap);
  return n;
}

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

IOVFun SetScilabErrorIO(IOVFun F)
{
  IOVFun g = nsp_error_vprintf;
  nsp_error_vprintf = F;
  return g;
}


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
 * clear an error message 
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
    lasterror =  nsp_smatrix_create(NVOID,0,0,NULL,0);
  else 
    nsp_smatrix_resize(lasterror,0,0);
  if ( lasterror != NULLSMAT ) 
    for ( i = 0 ; i < ((NspSMatrix *)SciStack.val->error_msg)->mn; i++) 
      nsp_row_smatrix_append_string(lasterror,((NspSMatrix *)SciStack.val->error_msg)->S[i]);
  nsp_smatrix_resize((NspSMatrix *) SciStack.val->error_msg,0,0);
}


NspSMatrix *nsp_lasterror_get() 
{
  return lasterror;
}

void nsp_lasterror_clear() 
{
  nsp_smatrix_resize(lasterror,0,0);
}


/*
 * scidebug :  Default value DefScidebug
 *            debug messages for parser 
 */

int scidebug(int i,char *fmt,...)
{
  int j,n;
  va_list ap;
  va_start(ap,fmt);
  fprintf(stdout,"\n%d",i);
  for ( j=0; j < i; j++) Sciprintf("|");
  n= Scivprintf( fmt, ap );
  va_end(ap);
  return n;
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

