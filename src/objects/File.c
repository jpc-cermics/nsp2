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

/* See also the Copyright at the end of the file for the swapxxx 
 * functions. 
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

/*
 * do_printf: code extraced from RLab and adapted for nsp 
 * Jean-Philippe Chancelier 1998.  
 *
 *   Copyright (C) 1995  Ian R. Searle 
 *   This program is free software; you can redistribute it and/or modify 
 *   it under the terms of the GNU General Public License as published by 
 *   the Free Software Foundation; either version 2 of the License, or 
 *   (at your option) any later version. 
 *
 *   This program is distributed in the hope that it will be useful, 
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of 
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 *   GNU General Public License for more details. 
 *   You should have received a copy of the GNU General Public License 
 *   along with this program; if not, write to the Free Software 
 *
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
 */


#include <math.h>
#include <stdio.h>
#include <string.h> 
#include <string.h>
#include <ctype.h>  /* isdigit */

#define SciFile_xdr_save_string /* locally change the header */
#include "nsp/object.h"
#include "nsp/interf.h"
#include "../system/files.h" /* FSIZE */

/**
 *nsp_file_open:
 * @fname: a path name 
 * @mode: 
 * @xdr_on: 
 * @swap_on: 
 * 
 * Opens a file given by fname and in case of success returns 
 * a  #Nspfile Object. 
 * 
 * Return value: a #Nspfile or %NULLSCIFILE
 **/

NspFile *nsp_file_open(char *fname, char *mode,int xdr_on,int swap_on)
{
  FILE *f;
  NspFile *F;
  char buf[FSIZE+1];
  /* expand env variables */
  nsp_path_expand(fname,buf,FSIZE);
  if ( strcmp("stdin",fname)==0) 
    {
      f = stdin;
    }
  else if ( strcmp("stdout",fname)==0)
    {
      f = stdout;
    }
  else if ( strcmp("stderr",fname)==0)
    {
      f = stderr;
    }
  else if((f=fopen(buf,mode)) == NULL)
    {
      Scierror("Error: fopen failed for file %s\n",fname) ;
      return(NULLSCIFILE);
    }
  if ((F =nsp_file_create(NVOID,buf,mode,0,f)) == NULLSCIFILE )
    {
      if ( f != stdin && f != stdout && f != stderr) fclose(f);
      return(NULLSCIFILE);
    }
  if ( xdr_on == TRUE) XDR_ON(F->obj->flag);
  if ( swap_on == TRUE) SWAP_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  return F;
}

/**
 *nsp_file_close:
 * @F: a  #Nspfile Object
 * 
 * Close the file described by F. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_file_close(NspFile  *F)
{
  if ( !IS_OPENED(F->obj->flag))
    {
      Sciprintf("Warning: file %s is already closed\n",F->obj->fname);
      return OK;
    }
  if ( F->obj->file != stdin && F->obj->file != stdout && F->obj->file != stderr)
    {
      if ( fclose(F->obj->file) == 0) 
	{
	  OPEN_OFF(F->obj->flag);
	  return OK;
	}
      else 
	{
	  Scierror("Error: close failed on file %s\n",F->obj->fname);
	  return FAIL;
	}
    }
  return OK;
}

#define assertR(ex) {if (!(ex)){ Scierror("Error: xdr read failed\n");return(FAIL);}}
#define assertW(ex) {if (!(ex)){ Scierror("Error: xdr write failed\n");return(FAIL);}}

static u_int count ;
static u_int szof ;

/*
 * Open a File for xdr reading 
 */

#define SCIF_V 256

/**
 *nsp_file_open_xdr_r:
 * @fname: file name 
 * 
 * Opens a file given its name for xdr reading 
 * 
 * Return value: a #NspFile object 
 **/

NspFile *nsp_file_open_xdr_r(char *fname)
{
#if defined(__STDC__) ||  defined(_MSC_VER)
  static char openf[]="rb";
#else
  static char openf[]="r";
#endif
  char SciF_version[SCIF_V];
  FILE *f;
  NspFile *F;
  if((f=fopen(fname,openf)) == NULL)
    {
      Scierror("Error: fopen failed for file %s\n",fname) ;
      return(NULLSCIFILE);
    }
  if ((F =nsp_file_create(NVOID,fname,openf,0,f)) == NULLSCIFILE )
    {
      fclose(f);
      return(NULLSCIFILE);
    }
  XDR_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  xdrstdio_create(F->obj->xdrs,F->obj->file, XDR_DECODE) ;

  if (nsp_xdr_load_string(F->obj->xdrs,SciF_version,SCIF_V) == FAIL ) 
    {
      /* clear the xdr message */
      nsp_error_message_clear();
      Scierror("Error: Wrong xdr header in file: %s expecting %s\n",fname,"NspXdr_1.0");
      return NULLSCIFILE;
    }
  if  (strcmp(SciF_version,"NspXdr_1.0") != 0 && strcmp(SciF_version,"SciXdr1.0") != 0 )
    {
      Scierror("Error: File %s with Wrong header %s, expecting  %s or %s.\n",
		fname,SciF_version,"NspXdr_1.0","SciXdr1.0");
      return NULLSCIFILE;
    }
  return F;
}

/*
 * Close a File opened for xdr read 
 */

#define TYPE_S 256

/**
 *nsp_file_close_xdr_r:
 * @F: a  #Nspfile Object
 * 
 * Close the file described by F. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_file_close_xdr_r(NspFile  *F)
{
  int rep =OK;
  static char type[TYPE_S];
  if ( !IS_XDR(F->obj->flag))
    {
      Scierror("Error: file %s is not an xdr file\n",F->obj->fname);
    }
  if ( !IS_OPENED(F->obj->flag))
    {
      Sciprintf("Warning: file %s is already closed\n",F->obj->fname);
      return OK;
    }
  nsp_xdr_load_string(F->obj->xdrs,type,TYPE_S) ;
  if ( strcmp(type,"endsave") != 0)
    {
      Scierror("Warning: Closing xdr file %s while not at end of file\n",F->obj->fname);
      rep = FAIL;
    }
  /* FIXME : here assertR does not work on macOSX 
     assertR(fflush((FILE *) F->obj->xdrs->x_private) != EOF) ; 
     xdr_destroy(F->obj->xdrs);
     assertR(fclose(F->obj->file) != EOF) ;
  */
  fflush((FILE *) F->obj->xdrs->x_private);
  xdr_destroy(F->obj->xdrs);
  fclose(F->obj->file);
  OPEN_OFF(F->obj->flag);
  return rep;
}

/**
 *nsp_file_open_xdr_w:
 * @fname: file name 
 * 
 * Opens a file given its name for xdr writing 
 * 
 * Return value: a #NspFile object 
 **/

NspFile *nsp_file_open_xdr_w(char *fname)
{
  char scis[]={"NspXdr_1.0"};
  FILE *f;
  NspFile *F;
#if defined(__STDC__) ||  defined(_MSC_VER)
  static char openf[]="wb";
#else
  static char openf[]="w";
#endif
  if((f=fopen(fname,openf)) == NULL)
    {
      Scierror("Error: fopen failed for file %s\n",fname) ;
      return(NULLSCIFILE);
    }
  if ((F =nsp_file_create(NVOID,fname,openf,0,f)) == NULLSCIFILE )
    {
      fclose(f);
      return(NULLSCIFILE);
    }
  XDR_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  xdrstdio_create(F->obj->xdrs, F->obj->file, XDR_ENCODE) ;
  nsp_xdr_save_string(F->obj->xdrs,scis);
  return F;
}

/**
 *nsp_file_close_xdr_w:
 * @F: a  #Nspfile Object
 * 
 * Close the file described by @F. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_file_close_xdr_w(NspFile  *F)
{
  if ( !IS_XDR(F->obj->flag))
    {
      Scierror("Error: close: file %s was not opened for xdr writting\n",
	       F->obj->fname);
      return FAIL;
    }
  if ( !IS_OPENED(F->obj->flag))
    {
      Sciprintf("Warning: file %s is already closed\n",F->obj->fname);
      return OK;
    }
  nsp_xdr_save_string(F->obj->xdrs,"endsave");
  assertW(fflush((FILE *) F->obj->xdrs->x_private) != EOF) ; 
  xdr_destroy(F->obj->xdrs);
  assertW(fclose(F->obj->file) != EOF) ;
  OPEN_OFF(F->obj->flag);
  return OK;
}


/**
 *nsp_xdr_save_d:
 * @xdrs: a  #XDR structure
 * @x: a double precision number to be saved in F.
 * 
 * Saves a double in a xdr stream with xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_d(XDR *xdrs, double x)
{
  szof = sizeof(double) ;
  count = (u_int) 1;
  assertW(xdr_vector(xdrs,(char *) &x,count,szof,(xdrproc_t) xdr_double)) ;
  return OK;
}

/**
 *nsp_xdr_load_d:
 * @xdrs: a  #XDR structure
 * @x: a pointer to a double precision number to be read in F.
 * 
 * Reads a double in a  xdr stream assuming xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_load_d(XDR *xdrs, double *x)
{
  szof = sizeof(double) ;
  count = (u_int) 1;
  assertR(xdr_vector(xdrs, (char *)x, count, szof ,
		     (xdrproc_t) xdr_double)) ;
  return OK;
}

/**
 *nsp_xdr_save_i:
 * @xdrs: a  #XDR structure
 * @ix: an integer 
 * 
 * Saves an int in a xdr stream with xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_i(XDR *xdrs, int ix)
{
  szof = sizeof(int) ;
  count = 1;
  assertW( xdr_vector(xdrs, (char *)&ix, count, szof,(xdrproc_t) xdr_int)) ;
  return OK;
}

/**
 *nsp_xdr_load_i:
 * @xdrs: a  #XDR structure
 * @ix: an int pointer 
 *
 * Reads an integer in a  xdr stream assuming xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/
int nsp_xdr_load_i(XDR *xdrs, int *ix)
{
  szof = sizeof(int) ;
  count = (u_int)1;
  assertR( xdr_vector(xdrs, (char *)ix, count,szof,(xdrproc_t) xdr_int)) ;
  return OK;
}

/**
 *nsp_xdr_save_c:
 * @xdrs: a  #XDR structure
 * @c: a character 
 * 
 * Saves a character in a xdr stream with xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_c(XDR *xdrs, char c)
{
  szof = sizeof(char);
  assertW( xdr_opaque(xdrs,&c,szof));
  return OK;
}

/**
 *nsp_xdr_load_c:
 * @xdrs: a  #XDR structure
 * @c: a char pointer
 * 
 * Reads a character in a  xdr stream assuming xdr coding.  
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_load_c(XDR *xdrs, char *c)
{
  szof = sizeof(char);
  assertR( xdr_opaque(xdrs,c,szof));
  return OK;
}

/**
 *nsp_xdr_save_array_i:
 * @xdrs: a  #XDR structure
 * @nx: pointer to an array of integers
 * @l: size of array 
 * 
 * Saves an int array @nx of size @l in a xdr stream with xdr coding. 
 *
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_array_i(XDR *xdrs, int *nx, int l)
{ 
  szof = sizeof(int) ;
  count = (int) l;
  assertW( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  assertW( xdr_vector(xdrs, (char *)nx, count, szof,(xdrproc_t) xdr_int)) ;
  return OK;
}

/**
 *nsp_xdr_load_array_i:
 * @xdrs: a  #XDR structure
 * @nx: pointer to an array of integers
 * @l: an integer
 *
 * fills the array pointed by @nx whith @l integers 
 * which are read from the xdr stream @xdr.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_load_array_i(XDR *xdrs, int *nx, int l)
{ 
  szof = sizeof(int) ;
  assertR( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  if ( count != (u_int) l ) return(FAIL);
  assertR( xdr_vector(xdrs, (char *)nx, count, szof,(xdrproc_t) xdr_int)) ;
  return OK;
}

/**
 *nsp_xdr_save_array_c:
 * @xdrs: a  #XDR structure
 * @nx: pointer to an array of characters 
 * @l: an integer 
 * 
 * Saves an array of characters @nx of size @l in a xdr stream with xdr coding. 
 *
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_array_c(XDR *xdrs,char *nx, int l)
{ 
  szof = sizeof(char) ;
  count = (int) l;
  assertW( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  assertW( xdr_vector(xdrs, (char *)nx, count, szof,(xdrproc_t) xdr_char)) ;
  return OK;
}

/**
 *nsp_xdr_load_array_c:
 * @xdrs: a  #XDR structure
 * @nx:  pointer to an array of characters 
 * @l: an integer 
 *
 * fills the array pointed by @nx whith @l characters
 * which are read from the xdr stream @xdr.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_load_array_c(XDR *xdrs,char *nx, int l)
{ 
  szof = sizeof(char) ;
  assertR( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  if ( count != (u_int) l ) return(FAIL);
  assertR( xdr_vector(xdrs, (char *)nx, count, szof,(xdrproc_t) xdr_char)) ;
  return OK;
}


/**
 *nsp_xdr_save_array_d:
 * @xdrs: a  #XDR structure
 * @nx: an array of double 
 * @l: an integer 
 *
 * Saves an array of double in a xdr stream with xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_array_d(XDR *xdrs, double *nx, int l)
{
  szof = sizeof(double) ;
  count = (int) l;
  assertW( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  assertW( xdr_vector(xdrs,(char *)nx, count, szof,
		      (xdrproc_t) xdr_double)) ;
  return OK;
}

/**
 *nsp_xdr_load_array_d:
 * @xdrs: a  #XDR structure
 * @nx: an array of double 
 * @mn: an integer
 * 
 * fills the array pointed by @nx whith @l doubles
 * which are read from the xdr stream @xdr.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_load_array_d(XDR *xdrs, double *nx, int mn)
{
  szof = sizeof(double) ;
  assertR( xdr_vector(xdrs,(char *) &count,(u_int)1,(u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  if ( count != (u_int) mn ) return(FAIL);
  assertR( xdr_vector(xdrs, (char *) nx, count, szof,(xdrproc_t) xdr_double)) ;
  return OK;
}

/*
 * Xdr Save and Load a  string 
 * XXX : coud use an xdr_primitive for that 
 */

/**
 *nsp_xdr_save_string:
 * @xdrs: a  #XDR structure
 * @str: string to be saved 
 *
 * Saves string @str in a xdr stream with xdr coding. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_string(XDR *xdrs, char *str)
{
  szof = (strlen(str)+1)*sizeof(char);
  assertW( xdr_vector(xdrs,(char *) &szof,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  assertW( xdr_opaque(xdrs,str,szof));
  return OK;
}

/**
 *nsp_xdr_load_string:
 * @xdrs: a  #XDR structure
 * @buf: a char pointer as buffer 
 * @buf_len: buffer length 
 *
 * fills the array pointed by @buf with a string 
 * read from the xdr stream @xdr. If the string is larger 
 * than the buffer length @buf_len an error is raised and 
 * %FAIL is returned.
 * 
 * Return value: %OK and %FAIL.
 **/

int nsp_xdr_load_string(XDR *xdrs, char *buf, int buf_len)
{
  assertR( xdr_vector(xdrs,(char *) &szof,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  if ( szof > (u_int) buf_len ) 
    {
      Scierror("Error: Buffer too small (%d> %d) for reading a string\n",szof,buf_len);
      return FAIL;
    }
  assertR( xdr_opaque(xdrs, buf ,szof));
  return OK;
}

/**
 *nsp_xdr_load_new_string:
 * @xdrs: a  #XDR structure
 * @str: pointer to a string to be set
 *
 * read a string from the xdr stream @xdr. a new string is 
 * allocated and filled. An error is raised and 
 * %FAIL is returned is allocation fails or if a string cannot be read.
 * 
 * Return value: %OK and %FAIL.
 **/

int nsp_xdr_load_new_string(XDR *xdrs, char **str)
{
  assertR( xdr_vector(xdrs,(char *) &szof,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  if (( *str =new_nsp_string_n(szof)) == (nsp_string) 0) 
    {
      Scierror("Error: running out of memory for reading a string\n");
      return FAIL;
    }
  assertR( xdr_opaque(xdrs, *str ,szof));
  return OK;
}

/**
 *nsp_xdr_save_array_ixx:
 * @xdrs: a  #XDR structure
 * @nx: pointer to an array of integers
 * @itype: integer describing the type of integer
 * @l: size of array 
 * 
 * Saves an int array @nx of size @l in a xdr stream with xdr coding. 
 *
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_save_array_ixx(XDR *xdrs,void *nx,nsp_itype itype, int l)
{ 
  count = (int) l;
  assertW( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  switch (itype ) 
    {
    case nsp_gint: 
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gint),(xdrproc_t) xdr_int)) ;
      break;
    case nsp_guint: 
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(guint),(xdrproc_t) xdr_u_int)) ;
      break;
    case nsp_gshort:
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gshort),(xdrproc_t) xdr_short)) ;
      break;
    case nsp_gushort:
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gushort),(xdrproc_t) xdr_u_short)) ;
      break;
    case nsp_glong : 
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(glong ),(xdrproc_t) xdr_long)) ;
      break;
    case nsp_gulong: 
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gulong),(xdrproc_t) xdr_u_long)) ;
      break;
    case nsp_gint8: 
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gint8),(xdrproc_t) xdr_int8_t)) ;
      break;
    case nsp_guint8:
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(guint8),(xdrproc_t) xdr_uint8_t)) ;
      break;
    case nsp_gint16:
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gint16),(xdrproc_t) xdr_int16_t)) ;
      break;
    case nsp_guint16:
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(guint16),(xdrproc_t) xdr_uint16_t)) ;
      break;
    case nsp_gint32: 
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gint32),(xdrproc_t) xdr_int32_t)) ;
      break;
    case nsp_guint32:
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(guint32),(xdrproc_t) xdr_uint32_t)) ;
      break;
    case nsp_gint64 :
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(gint64 ),(xdrproc_t) xdr_int64_t)) ;
      break;
    case nsp_guint64 :
      assertW( xdr_vector(xdrs, (char *)nx, count, sizeof(guint64 ),(xdrproc_t) xdr_uint64_t)) ;
      break;
    }
  return OK;
}

/**
 *nsp_xdr_load_array_ixx:
 * @xdrs: a  #XDR structure
 * @nx: pointer to an array of integers
 * @itype: integer describing the type of integer
 * @l: an integer
 *
 * fills the array pointed by @nx whith @l integers 
 * which are read from the xdr stream @xdr.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_xdr_load_array_ixx(XDR *xdrs, void *nx,nsp_itype itype, int l)
{ 
  assertR( xdr_vector(xdrs,(char *) &count,(u_int)1,
		      (u_int) sizeof(u_int),(xdrproc_t) xdr_u_int)) ;
  if ( count != (u_int) l ) return(FAIL);
  switch (itype ) 
    {
    case nsp_gint: 
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gint),(xdrproc_t) xdr_int)) ;
      break;
    case nsp_guint: 
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(guint),(xdrproc_t) xdr_u_int)) ;
      break;
    case nsp_gshort:
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gshort),(xdrproc_t) xdr_short)) ;
      break;
    case nsp_gushort:
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gushort),(xdrproc_t) xdr_u_short)) ;
      break;
    case nsp_glong : 
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(glong ),(xdrproc_t) xdr_long)) ;
      break;
    case nsp_gulong: 
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gulong),(xdrproc_t) xdr_u_long)) ;
      break;
    case nsp_gint8: 
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gint8),(xdrproc_t) xdr_int8_t)) ;
      break;
    case nsp_guint8:
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(guint8),(xdrproc_t) xdr_uint8_t)) ;
      break;
    case nsp_gint16:
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gint16),(xdrproc_t) xdr_int16_t)) ;
      break;
    case nsp_guint16:
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(guint16),(xdrproc_t) xdr_uint16_t)) ;
      break;
    case nsp_gint32: 
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gint32),(xdrproc_t) xdr_int32_t)) ;
      break;
    case nsp_guint32:
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(guint32),(xdrproc_t) xdr_uint32_t)) ;
      break;
    case nsp_gint64 :
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(gint64 ),(xdrproc_t) xdr_int64_t)) ;
      break;
    case nsp_guint64 :
      assertR( xdr_vector(xdrs, (char *)nx, count, sizeof(guint64 ),(xdrproc_t) xdr_uint64_t)) ;
      break;
    }


  return OK;
}




/*-------------------------------------------------------
 * A set of functions for reading and writing binary 
 * datas in a machine independant way 
 * using functions from a sound library 
 *-------------------------------------------------------*/

/* generic swap routine */

static void swapb(char *l, char *f, int n);
static unsigned short swapw(unsigned short us);
static unsigned long swapl(long unsigned int ul);
static unsigned int swapi(unsigned int ul);
static float swapf(float uf);
static double swapd(double df);
char *strerror(int errcode);

/* A corriger XXXXX */
static int swap = 0;

/**
 * is_little_endian:
 * 
 * check machine status (little or big endian)
 * 
 * Return value: %TRUE or %FALSE
 **/

int is_little_endian(void)
{
  int	littlendian = 1;
  char	*endptr =  (char *) &littlendian;
  return (*endptr) ? TRUE : FALSE ;
}

/**
 * nsp_feof:
 * @f: a #NspFile
 * 
 * checks if eof was reached in #NspFile @f.
 *  
 * Return value: %TRUE or %FALSE
 **/

int nsp_feof(NspFile *f)
{       
  if ( !IS_OPENED(f->obj->flag)) return TRUE;
  return (feof(f->obj->file)) ? TRUE : FALSE;
}

/**
 * nsp_ferror:
 * @f: a #NspFile
 * 
 * checks if an error was raised in #NspFile @f.
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_ferror(NspFile *f)
{       
  if ( !IS_OPENED(f->obj->flag)) return FALSE;
  return ferror(f->obj->file) ? TRUE : FALSE ; 
}

/**
 * nsp_clearerr:
 * @f: a #NspFile
 * 
 * clears raised error if any in #NspFile @f
 * 
 **/

void nsp_clearerr(NspFile *f)
{       
  if ( !IS_OPENED(f->obj->flag)) return ; 
  clearerr(f->obj->file);
}

#if (defined(sun) && !defined(SYSV)) || defined(sgi)
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif 

/**
 * nsp_fseek:
 * @F: a #NspFile
 * @offset: a long int 
 * @flag: a string from "set",or "cur" ,or "end"
 * 
 * call the fseek function. 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_fseek(NspFile *F,long int offset,const char *flag)
{     
  int iflag;
#if (defined(sun) && !defined(SYSV)) || defined(sgi)
  int irep;
#endif
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  if ( strncmp(flag,"set",3)==0 ) 
    iflag = SEEK_SET;
  else if ( strncmp(flag,"cur",3)==0 ) 
    iflag = SEEK_CUR;
  else if ( strncmp(flag,"end",3)==0 ) 
    iflag = SEEK_END;
  else 
    {
      Scierror("fseek : flag = %s not recognized\n",flag);
      return FAIL;
    }
#if (defined(sun) && !defined(SYSV)) || defined(sgi)
  irep = fseek(F->obj->file,iflag) ;
  if ( irep != 0 ) 
    {
      Scierror(strerror(irep));
      return FAIL;
    }
#else
  if (fseek(F->obj->file,offset,iflag) == -1 ) 
    {
      Scierror("fseek: error\n");
      return FAIL;
    }
#endif
  return OK;
}

/**
 * nsp_ftell:
 * @F: a #NspFile
 * @offset: a pointer to a long int 
 * 
 * calls the tell function 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_ftell(NspFile *F,long int *offset)
{     
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  *offset = ftell(F->obj->file) ;
  return OK;
}


/*
 * mput 
 *
 */

#define MPUT_CHARS_(x,n,file,Type) fwrite((Type *) x,sizeof(Type),n,file)
		
#define MPUT_(x,n,file,swap,Type,Fswap) \
{ \
   Type *val = (Type *) x ; \
   Type vali; \
   for ( i=0; i < n; i++)  \
     { \
       vali = *val++; \
       if (swap) vali = Fswap(vali); \
       fwrite(&vali,sizeof(Type),1,file); \
     } \
}

/**
 * nsp_mput:
 * @F: a #NspFile
 * @x: a pointer to an array 
 * @n: size of array as an integer 
 * @type: type of @x coded as string.
 *
 * writes array @x of size @n in the stream described by @F 
 * using type described by @type and controlling the byte order.
 * This routine assume that the array @x is type compatible wih @type
 * i.e Type *val = (Type *) x and *val++ works for iterating on x 
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_mput(NspFile *F,void *x,int n, char *type)
{  
  char swap_c,c1;
  int i,swap;
  if ( strlen(type) == 0) 
    {
      Scierror("mput: format is of length 0\n");
      return FAIL;
    }
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  
  swap = ( USE_SWAP(F->obj->flag) ) ? (( is_little_endian() ) ? FALSE :  TRUE) : FALSE; 
  /* check if swap was given in type */
  swap_c = type[strlen(type)-1]; 
  if (swap_c == 'b' ) 
    swap = ( is_little_endian() ) ? TRUE : FALSE ;
  else if (swap_c == 'l' ) 
    swap = ( is_little_endian() ) ? FALSE : TRUE ;

  switch ( type[0] )
    {
    case 'i' : MPUT_(x,n,F->obj->file,swap,int,swapi);       break;
    case 'l' : MPUT_(x,n,F->obj->file,swap,long,swapl);      break;
    case 's' : MPUT_(x,n,F->obj->file,swap,short,swapw);     break;
    case 'c' : MPUT_CHARS_(x,n,F->obj->file,char) ;                 break;
    case 'd' : MPUT_(x,n,F->obj->file,swap,double,swapd);    break;
    case 'f' : MPUT_(x,n,F->obj->file,swap,float,swapf);     break;
    case 'u' :
      c1=( strlen(type) < 1) ? ' ': type[1];
      switch ( c1)
	{
	case 'i' :  MPUT_(x,n,F->obj->file,swap,unsigned int,swapi); break;
	case 'l' :  MPUT_(x,n,F->obj->file,swap,unsigned long,swapl); break;
	case 's' :  MPUT_(x,n,F->obj->file,swap,unsigned short,swapw); break;
	case ' ' :  MPUT_(x,n,F->obj->file,swap,unsigned int,swapi); break;
	case 'c' :  MPUT_CHARS_(x,n,F->obj->file,unsigned char); break;
	default : 
	  Scierror("mput '%c' unrecognized conversion character\n",c1);
	  return FAIL;
	}
      break; 
    default : 
      Scierror("mput '%c' unrecognized conversion character\n",type[0]);
      return FAIL;
      break;
    }
  return OK;
}


/*
 * reads data and store them without type conversion 
 */

#define MGET_CHARS_(x,n,file,Type) items=fread((Type *) x,sizeof(Type),n,file)

#define MGET_(x,n,file,swap,Type,Fswap) \
{\
   Type *val = (Type *) x ;\
   items=fread(val,sizeof(Type),n,file);\
   if (swap) for (i=0;i<items;i++) val[i]=Fswap(val[i]);\
}

/**
 * nsp_mget:
 * @F: a #NspFile 
 * @x: an array pointer 
 * @n: an integer 
 * @type: a string coding type of @x.
 * @items_read: an int pointer 
 * 
 * reads data from #NspFile the byte order being controled by 
 * the way @F was opened. This function can be used to read data coded 
 * in little or big endian 
 * if read fails @items_read will contain the number of properly read items. 
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_mget(NspFile *F,void *x,int n,const char *type,int *items_read)
{  
  char c1,swap_c;
  int i,items=n; 
  int nc=strlen(type);
  if ( nc == 0) 
    {
      Scierror("mget: format is of length 0\r\n");
      return FAIL;
    }
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  
  swap = ( USE_SWAP(F->obj->flag) ) ? (( is_little_endian() ) ? FALSE :  TRUE) : FALSE; 
  /* check if swap was given in type */
  swap_c = type[strlen(type)-1]; 
  if (swap_c == 'b' ) 
    swap = ( is_little_endian() ) ? TRUE : FALSE ;
  else if (swap_c == 'l' ) 
    swap = ( is_little_endian() ) ? FALSE : TRUE ;

  switch ( type[0] )
    {
    case 'i' : MGET_(x,n,F->obj->file,swap,int,swapi);break;
    case 'l' : MGET_(x,n,F->obj->file,swap,long,swapl);break;
    case 's' : MGET_(x,n,F->obj->file,swap,short,swapw);break;
    case 'c' : MGET_CHARS_(x,n,F->obj->file,char) ; break;
    case 'd' : MGET_(x,n,F->obj->file,swap,double,swapd);break;
    case 'f' : MGET_(x,n,F->obj->file,swap,float,swapf);break;
    case 'u' :
      c1=( strlen(type) < 1) ? ' ': type[1];
      switch ( c1)
	{
	case 'i' :  MGET_(x,n,F->obj->file,swap,unsigned int,swapi); break;
	case 'l' :  MGET_(x,n,F->obj->file,swap,unsigned long,swapl); break;
	case 's' :  MGET_(x,n,F->obj->file,swap,unsigned short,swapw); break;
	case ' ' :  MGET_(x,n,F->obj->file,swap,unsigned int,swapi); break;
	case 'c' :  MGET_CHARS_(x,n,F->obj->file,unsigned char); break;
	default :  
	  Scierror("mput '%c' unrecognized conversion character\n",c1);
	  return FAIL;
	}
      break;
    default :
      Scierror("mput '%c' unrecognized conversion character\n",type[0]);
      return FAIL;
      break;
    }
  *items_read = items;
  return OK;
}

/**
 * nsp_mgetstr:
 * @F: a #NspFile 
 * @start: pointer to string to be returned 
 * @n: an integer 
 * 
 * allocate and read a string of size @n which is 
 * returned in @start.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_mgetstr(NspFile *F, char **start, int n)
{ 
  int count;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  *start= (char *) malloc((n+1)*sizeof(char));
  if ( *start == (char *) 0)
    {       
      Scierror("No more memory\n");
      return FAIL;
    }
  count=fread(*start,sizeof(char),n,F->obj->file);
  (*start)[n]='\0';
  if ( count != n ) 
    {
      Scierror("mgetstr: all the requested chars were not read\n");  
      return FAIL;
    }
  return OK;
}


int nsp_mgetstr1 (NspFile *F, char *start, int n,int *n_read)
{ 
  int count;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  count=fread(start,sizeof(char),n,F->obj->file);
  start[n]='\0';
  *n_read = count;
  return OK;
}

/**
 * nsp_putstr:
 * @F: a #NspFile
 * @str: a char pointer 
 * 
 * writes string @str in stream @F.
 * 
 * Return value: 
 **/

int nsp_putstr(NspFile *F, char *str)
{   
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  if ( fprintf(F->obj->file,"%s",str) <= 0 ) 
    {
      Scierror("putstr: failed for string '%s' \n",str);
      return FAIL;
    }
  return OK;
}


/*
 * nsp_fscan_matrix 
 */ 

#define INFOSIZE 1024

static int  Info_size = 0;
static char *Info= NULL;
static int nsp_read_line(FILE *fd,int *mem);
static int count_tokens(char *string);

/**
 * nsp_fscanf_matrix:
 * @F: a #NspFile 
 * @format: a string 
 * @M: A #NspMatrix 
 * @flag: an integer 
 * @S: a #NspSMatrix
 * 
 * reads a scalar matrix in the file given by @F.
 * The first non numeric values found in file @F are stored in a string matrix @S 
 * (only if @flag is %TRUE). The matrix is returned in @M. 
 * @format is unused.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_fscanf_matrix(NspFile *F,char *format,NspMatrix **M,int flag,NspSMatrix **S)
{
  long int offset;
  int mem=0;
  double x;
  int i,j,rows,cols,n,c;
  int vl=-1;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }


  if ( Info == NULL && (Info =new_nsp_string_n(INFOSIZE)) == NULLSTRING) return FAIL;
  Info_size = INFOSIZE;

  /* mark position */
  
  offset = ftell(F->obj->file) ;

  /* first pass to get colums and rows ***/
  strcpy(Info,"--------");
  n =0; 
  while ( sscanf(Info,"%lf",&x) <= 0 && n != EOF ) 
    { 
      n=nsp_read_line(F->obj->file,&mem); 
      if ( mem == 1) return FAIL;
      vl++;
    }
  if ( n == EOF )
    {
      Scierror("fscanfMat: cannot find matrix data (file %s)\n",F->obj->fname);
      return FAIL;
    }
  cols = count_tokens(Info);
  rows = 1;
  while (1) 
    { 
      int cols1;
      n=nsp_read_line(F->obj->file,&mem);
      if ( mem == 1) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      if ( sscanf(Info,"%lf",&x) <= 0) break;
      cols1 = count_tokens(Info);
      if ( cols1 != cols ) 
	{
	  break; 
	  /* Scierror("fscanfMat: Expecting %d columns and %d found at line %d \n",cols,cols1,rows);*/ 
	}
      rows++;
    }
  if ( cols == 0 || rows == 0) rows=cols=0;
  
  if ((*M = nsp_matrix_create(NVOID,'r',rows,cols))== NULLMAT) return FAIL;

  /* second pass to read data **/
  
  nsp_fseek(F,offset,"set");
  
  /* non numeric lines in S **/
  if ( flag == TRUE ) 
    {
      if (( *S=nsp_smatrix_create_with_length(NVOID,vl,1,-1))== NULLSMAT) return FAIL;
    }

  for ( i = 0 ; i < vl ; i++) 
    {
      nsp_read_line(F->obj->file,&mem);
      if ( mem == 1) return FAIL;
      if ( flag == TRUE ) 
	{
	  if (((*S)->S[i]=new_nsp_string(Info))==NULL) return FAIL;
	}
      
    }

  /* numeric data in M */
  
  for (i=0; i < rows ;i++)
    {
      for (j=0 ; j < cols;j++)
	{ 
	  double xloc;
	  fscanf(F->obj->file,"%lf",&xloc);
	  (*M)->R[i+rows*j]=xloc;
	}
    }
  while ( (c = getc(F->obj->file)) != '\n' && c != EOF) {};

  /* just in case Info is too Big */ 

  if ( Info_size > INFOSIZE ) 
    {
      Info_size = INFOSIZE;
      Info = realloc(Info,(Info_size+1)*sizeof(char));
    }
  return OK;
}  


static int nsp_read_line(FILE *fd,int *mem)
{
  int n=0;
  while (1)
    {
      char c = (char) getc(fd);
      if ( n > Info_size ) 
	{
	  Info_size += INFOSIZE;
	  if (( Info = realloc(Info,(Info_size+1)*sizeof(char)))==NULL) {
	    Scierror("Error: no more memory\n");
	    *mem=1;
	    return EOF;
	  }
	}
      Info[n]= c ; 
      if ( c == '\n') 
	{ 
	  Info[n] = '\0' ; 
	  if ( n > 0 && Info[n-1] == '\r')  Info[n-1] = '\0' ; 
	  return 1;
	}
      else if ( c == (char)EOF ) return EOF;  
      n++;
    }
}

#if 0 
static int count_tokens_bug(char *string)
{
  char prev, *copy = string;
  int lnchar=0,ntok=-1;
  int length = strlen(string)+1;
  if (string != 0)
    { 
      while (*copy==' ' || *copy=='\t' || *copy=='\n')
        {
          copy++;
          lnchar++;
        }
      if(lnchar==0) 
	{
	  /* line begins with a number */
	  prev='0'; ntok++; 
	}
      else
	{
	  /* line begins with [ \t]*  */
	  prev=*(copy-1);
	}
      while (lnchar <= length)
        {
          if((*copy!=' ' && *copy!='\t' && *copy!='\n') &&
             (prev==' ' || prev=='\t' || prev=='\n'))
            ntok++;
          prev=*copy;
          copy++;
          lnchar++;
        }
      return(ntok);
    }
  return(FAIL);
}
#endif 

static int count_tokens(char *string)
{
  char *copy = string; 
  int ntok=0;
  /* gobble spaces */
  while ( *copy==' ' || *copy=='\t' || *copy=='\n') copy++;
  if ( *copy == '\0') return 0;
  while (1) 
    {
      /* gobble token */
      while ( *copy != '\0' && *copy !=' ' && *copy !='\t' && *copy !='\n') copy++;
      ntok++;
      /* gobble spaces */
      while ( *copy==' ' || *copy=='\t' || *copy=='\n') copy++;
      if ( *copy == '\0' ) 
	return ntok;
    }
  return -1;
}


/**
 * nsp_read_lines:
 * @F: a #NspFile 
 * @S: a #NspSMatrix
 * @nlines: an integer 
 * 
 * Fills a #NspSMatrix object with lines read from file @F. 
 * @nlines gives the maximum number of lines to read. 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_read_lines(NspFile *F,NspSMatrix **S,int nlines)
{
  int mem=0,i,n;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  if ( Info == NULL && (Info =new_nsp_string_n(INFOSIZE)) == NULLSTRING) return FAIL;
  Info_size = INFOSIZE;
  if ((*S =nsp_smatrix_create_with_length(NVOID,nlines,1,-1))== NULLSMAT) return FAIL;
  for ( i= 0 ; i < nlines ; i++)
    {
      n = nsp_read_line(F->obj->file,&mem);
      if ( mem == 1) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      if (((*S)->S[i]=new_nsp_string(Info))==NULL) return FAIL;
    }
  if ( i < nlines ) 
    {
      /* we have read less than nlines */
      NspSMatrix *S1; 
      int j;
      if ((S1 =nsp_smatrix_create_with_length(NVOID,i,1,-1))== NULLSMAT) return FAIL;
      for ( j= 0 ; j < i ; j++) 
	{
	  S1->S[j] = (*S)->S[j];
	  (*S)->S[j]= NULL;
	}
      for ( j = i ; j < nlines ; j++) (*S)->S[j]=NULL;
      nsp_smatrix_destroy(*S);
      *S = S1;
    }

  /* just in case Info is too Big */ 
  if ( Info_size > INFOSIZE ) 
    {
      Info_size = INFOSIZE;
      Info = realloc(Info,(Info_size+1)*sizeof(char));
    }
  return OK;
}

/**
 * nsp_fscanf_smatrix:
 * @F: a #NspFile
 * @S: a #NspSMatrix
 * 
 * Creates and fills a #NspSMatrix object with file contents.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_fscanf_smatrix(NspFile *F,NspSMatrix **S)
{
  long int offset;
  int mem=0;
  int rows=0,n;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }

  if ( Info == NULL && (Info =new_nsp_string_n(INFOSIZE)) == NULLSTRING) return FAIL;
  Info_size = INFOSIZE;

  /* mark position */
  
  offset = ftell(F->obj->file) ;

  /* first pass to get number of rows ***/

  while (1) 
    { 
      n=nsp_read_line(F->obj->file,&mem);
      if ( mem == 1) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      rows++;
    }
  
  if ((*S =nsp_smatrix_create_with_length(NVOID,rows,1,-1))== NULLSMAT) return FAIL;

  /* second pass to read data **/
  
  nsp_fseek(F,offset,"set");
  rows=0;
  while (1)
    {
      n = nsp_read_line(F->obj->file,&mem);
      if ( mem == 1) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      if (((*S)->S[rows++]=new_nsp_string(Info))==NULL) return FAIL;
    }

  /* just in case Info is too Big */ 

  if ( Info_size > INFOSIZE ) 
    {
      Info_size = INFOSIZE;
      Info = realloc(Info,(Info_size+1)*sizeof(char));
    }
  return OK;
}  

/**
 * nsp_fprintf_matrix:
 * @F: a #NspFile 
 * @format: a string.
 * @sep: a string 
 * @M: a #NspMatrix 
 * @S: a #NspSMatrix
 * 
 * prints matrix @M in file @F using @format for formating numbers and 
 * @sep as separator. if @S is non null it is writen first in the file 
 * and stands as comments.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_fprintf_matrix(NspFile *F,char *format,char *sep,NspMatrix *M,NspSMatrix *S)
{
  int i,j;
  const char *fmt = "%f"; 
  const char *separator = " ";
  if ( format != NULL) fmt= format; 
  if ( sep != NULL) separator = sep;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  if ( S != NULL) for ( i=0 ; i < S->mn ; i++) fprintf(F->obj->file,"%s\n",S->S[i]);

  for (i = 0 ; i < M->m ; i++ ) 
    {
      for ( j = 0 ; j < M->n ; j++) 
	{
	  fprintf(F->obj->file,fmt,M->R[i+M->m*j]);
	  fprintf(F->obj->file,"%s",separator);
	}
      fprintf(F->obj->file,"\n");
    }
  return OK;
}  

/**
 * nsp_fprintf_smatrix:
 * @F: a #NspFile 
 * @S:  a #NspSMatrix
 * 
 * prints the contents of @S in @F.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_fprintf_smatrix(NspFile *F,NspSMatrix *S)
{
  int i;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("File %s is not opened\n",F->obj->fname);
      return FAIL;
    }
  for ( i=0 ; i < S->mn ; i++) fprintf(F->obj->file,"%s\n",S->S[i]);
  return OK;
}  

/*
 * Emulation of Ansi C XXscanf functions 
 * The number of scaned object is hardwired (MAXSCAN) 
 * and scaned strings (%s,%c %[) are limited to MAX_STR characters
 *
 * XXXX Could be changed to eliminate the MAXSCAN limitation 
 * 
 */

#define MAX_STR 1024
/* MAXSCAN and SCAN_ARGS must be compatible */
#define MAXSCAN 20
#define SCAN_ARGS ptrtab[0],ptrtab[1],ptrtab[2],ptrtab[3],ptrtab[4],ptrtab[5], \
                  ptrtab[6],ptrtab[7],ptrtab[8],ptrtab[9],ptrtab[10],ptrtab[11],\
                  ptrtab[12],ptrtab[13],ptrtab[14],ptrtab[15],ptrtab[16],ptrtab[17],\
	          ptrtab[18],ptrtab[19]

typedef enum {SF_C,SF_S,SF_LUI,SF_SUI,SF_UI,SF_LI,SF_SI,SF_I,SF_LF,SF_F} sfdir;

typedef int (*PRINTER) (FILE *,const char *,...);

/**
 * do_scanf:
 * @command: string added for error message  which gives the function name
 * @fp: the stream to use for reading
 * @format:  the format to use 
 * @stack: nsp stack 
 * @iline:  is the items row to be filled 
 *         when iline == 0 items are to be created 
 *         when iline != 0 we assume that we just have to fill object 
 * @nargs: a return value, the number of items detected in the format string 
 * @strv:  a string
 * @retval: the return value of the scanf or fscanf function 
 * 
 * Return value: %OK or %FAIL.
 **/

int do_scanf (char *command, FILE *fp, char *format, Stack stack,int iline, int *nargs, char *strv, int *retval)
{
  double dval;
  int i;
  char sformat[MAX_STR];
  char char_buf[MAXSCAN][MAX_STR];
  long unsigned int buf_lui[MAXSCAN];
  short unsigned int buf_sui[MAXSCAN];
  unsigned int buf_ui[MAXSCAN];
  long int buf_li[MAXSCAN];
  short int buf_si[MAXSCAN];
  int buf_i[MAXSCAN];
  double buf_lf[MAXSCAN];
  float buf_f[MAXSCAN];
  void *ptrtab[MAXSCAN];
  sfdir  type[MAXSCAN];
  int nc[MAXSCAN];
  char save,directive;
  char *p,*p1;
  register char *q;
  void * target;
  int l_flag=0, h_flag=0,width_flag,width_val,ign_flag,str_width_flag=0;
  int num_conversion = -1;	/* for error messages and counting arguments*/
  PRINTER printer;		/* pts at fprintf() or sprintf() */

  q = format;
  *retval = 0;
  *nargs =1;
  if (fp == (FILE *) 0)		
    {
      /* doing sscanf */
      target = strv;
      printer = (PRINTER) sscanf;
    }
  else
    {
      /* doing fscanf */
      target =  fp;
      printer = (PRINTER) fscanf;
    }

  /* Traverse format string, doing scanf(). */
  while (1)
    {
      /* scanf */
      p=q;
      while (*q != '%' && *q != '\0' ) q++;
      if ( *q == '%' && *(q+1) == '%' ) 
	{
	  q=q+2;
	  while (*q != '%' && *q != '\0' ) q++;
	}
      if (*q == 0) 
	{
	  break ;
	}

      q++; /* q point to character following % **/
      
      /* 
       * We have found a conversion specifier, figure it out,
       * then scan the data asociated with it.
       */

      num_conversion++;
      if ( num_conversion > MAXSCAN ) 
	{
	  Scierror("Error: scanf too many (%d > %d) conversion required\n",
		   num_conversion,MAXSCAN);
	  return FAIL;
	}

      /* mark the '%' with p1 */
      
      p1 = q - 1; 

      /* check for width field */

      while ( isdigit(((int)*q)) ) q++;
      width_flag =0;

      if ( p1+1 != q ) 
	{	  
	  char w= *q;
	  *q='\0';
	  width_flag = 1;
	  sscanf(p1+1,"%d",&width_val);
	  *q=w;
	}

      /* check for ignore argument */

      ign_flag=0;

      if (*q == '*')
	{
	  /* Ignore the argument in the input args */
	  num_conversion = Max(num_conversion-1,0);
	  ign_flag = 1;
	  q++;
	}
      /* check for %l or %h */
      l_flag = h_flag = 0;

      if (*q == 'l')
	{
	  q++;
	  l_flag = 1;
	}
      else if (*q == 'h')
	{
	  q++;
	  h_flag = 1;
	}

      /* directive points to the scan directive  */
      
      directive = *q++;

      if ( directive == '[' )
	{
	  char *q1=q--;
	  /* we must find the closing bracket **/
	  while ( *q1 != '\0' && *q1 != ']') q1++;
	  if ( *q1 == '\0') 
	    {
	      Scierror("Error: scanf, unclosed [ directive\n");
	      return FAIL;
	    }
	  if ( q1 == q +1 || strncmp(q,"[^]",3)==0 ) 
	    {
	      q1++;
	      while ( *q1 != '\0' && *q1 != ']') q1++;  
	      if ( *q1 == '\0') 
		{
		  Scierror("Error: scanf unclosed [ directive\n");
		  return FAIL;
		}
	    }
	  directive = *q1++;
	  q=q1;
	}

      save = *q;
      *q = 0;
      
      /* if (debug) Sciprintf("Now directive [%s],%c\n",p,directive); **/
      
      switch (directive )
	{
	case ']':
	  if (width_flag == 0 ) str_width_flag = 1;
	  if (width_flag == 1 && width_val > MAX_STR-1 )
	    {
	      Scierror("Error: scanf, width field %d is too long (> %d) for %%[ directive\n",
		       width_val,MAX_STR-1);
	      return FAIL;
	    }
	  ptrtab[num_conversion] =  char_buf[num_conversion];
	  type[num_conversion] = SF_S;
	  break;
	case 's':
	  if (l_flag + h_flag)
	    Scierror("Error: scanf: bad conversion\n");
	  if (width_flag == 0 ) str_width_flag = 1;
	  if (width_flag == 1 && width_val > MAX_STR-1 )
	    {
	      Scierror("Error: scanf, width field %d is too long (< %d) for %%s directive\n",
		       width_val,MAX_STR-1);
	      return FAIL;
	    }
	  ptrtab[num_conversion] =  char_buf[num_conversion];
	  type[num_conversion] = SF_S;
	  break;
	case 'c':
	  if (l_flag + h_flag)
	    Scierror("Error: scanf: bad conversion\n");
	  if ( width_flag == 1 ) 
	    nc[num_conversion ] = width_val;
	  else
	    nc[num_conversion ] = 1;
	  if (width_flag == 1 && width_val > MAX_STR-1 )
	    {
	      Scierror("Error: scanf width field %d is too long (< %d) for %%c directive\n",
		       width_val,MAX_STR-1);
	      return FAIL;
	    }
	  ptrtab[num_conversion] =  char_buf[num_conversion];
	  type[num_conversion] = SF_C;
	  break;
	case 'o':
	case 'u':
	case 'x':
	case 'X':
	  if ( l_flag ) 
	    {
	      ptrtab[num_conversion] =  &buf_lui[num_conversion];
	      type[num_conversion] = SF_LUI;
	    }
	  else if ( h_flag) 
	    {
	      ptrtab[num_conversion] =  &buf_sui[num_conversion];
	      type[num_conversion] = SF_SUI;
	    }
	  else 
	    {
	      ptrtab[num_conversion] =  &buf_ui[num_conversion];
	      type[num_conversion] = SF_UI;
	    }
	  break;
	case 'D':
	  ptrtab[num_conversion] =  &buf_li[num_conversion];
	  type[num_conversion] = SF_LI;
	  break;
	case 'n':
	case 'i':
	case 'd':
	  if ( l_flag ) 
	    {
	      ptrtab[num_conversion] =  &buf_li[num_conversion];
	      type[num_conversion] = SF_LI;
	    }
	  else if ( h_flag) 
	    {
	      ptrtab[num_conversion] =  &buf_si[num_conversion];
	      type[num_conversion] = SF_SI;
	    }
	  else 
	    {
	      ptrtab[num_conversion] =  &buf_i[num_conversion];
	      type[num_conversion] = SF_I;
	    }
	  break;
	case 'e':
	case 'f':
	case 'g':
	case 'E':
	case 'G':
	  if (h_flag)
	    {
	      Scierror("Error: scanf: bad conversion\n");
	      return FAIL;
	    }
	  else if (l_flag) 
	    {
	      ptrtab[num_conversion] =  &buf_lf[num_conversion];
	      type[num_conversion] = SF_LF;
	    }
	  else
	    {
	      ptrtab[num_conversion] =  &buf_f[num_conversion];
	      type[num_conversion] = SF_F;
	    }
	  break;
	default:
	  Scierror("Error: scanf: bad conversion\n");
	  return FAIL;
	}
      *q = save;
    }

  /* we replace %s and %[ directive with a max length field **/

  if ( str_width_flag == 1) 
    {
      char *f1=format;
      char *f2=sformat;
      char *slast = sformat + MAX_STR-1 -4;
      while ( *f1 != '\0'  ) 
	{
	  int n;
	  *f2++ = *f1++;
	  if ( *(f1-1) == '%' && ( *(f1) == 's'  || *(f1) == '['))
	    {
	      n=sprintf(f2,"%d",MAX_STR-1);
	      f2 += n;
      	      *f2++ = *f1++;
	    }
	  if ( f2 == slast )
	    {
	      Scierror("Error: scanf, format is too long (> %d) \n",MAX_STR-1);
	      return FAIL;
	    }
	}
      format = sformat;
    }

  
  /* Calling scanf function : 
   *  Only  num_conversion +1 argument are used 
   *  the last arguments transmited points to nothing 
   *  but this is not a problem since they won't be used 
   */

  *retval = (*printer) ( target,format,SCAN_ARGS);

  *nargs = num_conversion+1;

  if ( *retval == EOF) 
    {
      Scierror("Warning: eof found while executing %s\n",command);
      return FAIL;
    }
  if ( *retval != *nargs )
    {
      Scierror("Warning: %d items not assigned while executing %s\n",*nargs-*retval,command);
      return FAIL;
    }
  for ( i=1 ; i <= *nargs ; i++) 
    {
      switch ( type[i-1] )
	{
	case SF_C:
	  char_buf[i-1][nc[i-1]]='\0';
	  NthObj(i)=nsp_create_object_from_str(NVOID,char_buf[i-1]);
	  /* Sciprintf("read [%s]\n",char_buf[i-1]); **/
	  break;
	case SF_S:
	  NthObj(i)=nsp_create_object_from_str(NVOID,char_buf[i-1]);
	  break;
	case SF_LUI:
	  dval = *((unsigned long int*) ptrtab[i-1]);
	  NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  break;
	case SF_SUI:
	  dval = *((unsigned short int*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	case SF_UI:
	  dval = *((unsigned int*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	case SF_LI:
	  dval = *((long int*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	case SF_SI:
	  dval = *((short int*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	case SF_I:
	  dval = *((int*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	case SF_LF:
	  dval = *((double*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	case SF_F:
	  dval = *((float*) ptrtab[i-1]);
	  if ( iline == 0) 
	    NthObj(i)=nsp_create_object_from_double(NVOID,dval);
	  else
	    ((NspMatrix *) NthObj(i))->R[iline]= dval;
	  break;
	}
    }
  return OK;
}


/*--------------------------------------------------------------
 *   do_printf: code extraced from RLab and hacked for Scilab  
 *               by Jean-Philippe Chancelier 1998.  
 *
 *     Copyright (C) 1995  Ian R. Searle 
 *     This program is free software; you can redistribute it and/or modify 
 *     it under the terms of the GNU General Public License as published by 
 *     the Free Software Foundation; either version 2 of the License, or 
 *     (at your option) any later version. 
 *
 *     This program is distributed in the hope that it will be useful, 
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of 
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 *     GNU General Public License for more details. 
 *     You should have received a copy of the GNU General Public License 
 *     along with this program; if not, write to the Free Software 
 *
 *     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
 *--------------------------------------------------------------*/

/*---------- types and defs for doing printf ------------*/

typedef enum {PF_C,PF_S,PF_D, PF_LD,PF_F, PF_UD, PF_LUD} printf_cv;

/* for switch on number of '*' and type */

#define  AST(num,type)  (7*(num)+(type))

/* Buffer for printf **/

#define MAX_SPRINTF_SIZE  4096
static char sprintf_buff[MAX_SPRINTF_SIZE];
static char *sprintf_limit = sprintf_buff + MAX_SPRINTF_SIZE;

static int P_count_columns(Stack stack,int first_arg,int n_args)
{
  NspObject *obj=NULL; 
  int i, cols=0;

  for ( i= first_arg ; i <= n_args ; i++) 
    {
      if ((obj =nsp_get_object(stack,i))== NULL) return FAIL;
      cols +=nsp_object_get_size(obj,2); /* number of columns of obj */
    }
  return cols;
}

/* 
 * first_arg: position of the first argument of printf in the stack
 * n_args: number of arguments 
 * arg_cnt: requested column 
 * line: requested line 
 */

static int P_GetScalarDouble(Stack stack,int first_arg,int n_args,int col,int line,double *dval)
{
  int i, cols=0,obj_cols,obj_col=-1;
  NspObject *obj=NULL; 
  NspObject *M;
  for ( i= first_arg ; i <= n_args ; i++) 
    {
      if ((obj =nsp_get_object(stack,i))== NULL) return FAIL;
      obj_cols =nsp_object_get_size(obj,2); /* number of columns of obj */
      if ( cols <= col && col < cols + obj_cols ) 
	{
	  obj_col = col-cols ;
	  break;
	}
      else cols += obj_cols;
    }
  if (obj_col == -1)
    {
      Scierror("%s: not enough arguments expecting at least %d columns\n",
	       NspFname(stack),col+1);
      return FAIL;
    }
  /* we must check that object at position i is a Scalar Matrix 
   * and we get the element (line,obj_col)
   */
  if ( IsMat(obj)) 
    {
      if ((M =(NspObject*) GetRealMat(stack,i))== NULL) return FAIL;
      *dval = ((NspMatrix *)M)->R[line+ ((NspMatrix *)M)->m*obj_col];
    }
  else if ( IsBMat(obj))
    {
      if ((M =(NspObject*) GetBMat(stack,i))== NULL) return FAIL;
      *dval = ((NspBMatrix *)M)->B[line+ ((NspBMatrix *)M)->m*obj_col];
    }
  else 
    {
      Scierror("%s: item %d should be a scalar or boolean matrix\n",NspFname(stack),i);
      return FAIL;
    }
  return OK;
}

static int P_GetScalarInt(Stack stack,int first_arg,int n_args,int col,int line,int *ival)
{
  int i, cols=0,obj_cols,obj_col=-1;
  NspObject *obj=NULL; 
  NspObject *M;
  for ( i= first_arg ; i <= n_args ; i++) 
    {
      if ((obj =nsp_get_object(stack,i))== NULL) return FAIL;
      obj_cols =nsp_object_get_size(obj,2); /* number of columns of obj */
      if ( cols <= col && col < cols + obj_cols ) 
	{
	  obj_col = col-cols ;
	  break;
	}
      else cols += obj_cols;
    }
  if (obj_col == -1)
    {
      Scierror("%s: not enough arguments expecting at least %d columns\n",
	       NspFname(stack),col+1);
      return FAIL;
    }
  /* we must check that object at position i is a Scalar Matrix 
   * and we get the element (line,obj_col)
   */
  if ((M =(NspObject*) GetRealMat(stack,i))== NULL) return FAIL;
  *ival = ((NspMatrix *)M)->R[line+ ((NspMatrix *)M)->m*obj_col];
  return OK;
}


static char * P_GetString(Stack stack,int first_arg,int n_args,int col,int line)
{
  int i, cols=0,obj_cols,obj_col=-1;
  NspObject *obj=NULL; 
  NspSMatrix *S;
  for ( i= first_arg ; i <= n_args ; i++) 
    {
      if ((obj =nsp_get_object(stack,i))== NULL) return NULL;
      obj_cols =nsp_object_get_size(obj,2); /* number of columns of obj */
      if ( cols <= col && col < cols + obj_cols ) 
	{
	  obj_col = col-cols ;
	  break;
	}
      else cols += obj_cols;
    }
  if (obj_col == -1)
    {
      Scierror("%s: not enough arguments expecting at least %d columns\n",
	       NspFname(stack),col+1);
      return NULL;
    }
  /* we must check that object at position i is a Scalar Matrix 
   * and we get the element (line,obj_col)
   */
  if ((S = GetSMat(stack,i))== NULLSMAT) return NULL;
  return S->S[line+ S->m*obj_col];
}



/**
 * do_printf:
 * @fname: file name 
 * @fp: stream to use 
 * @format: format for printing 
 * @stack: a  #Stack.
 * @nargs: number of arguments 
 * @arg_cnt: position of first argument 
 * @line:  current line 
 * @strv: if present printing is performed in a string buffer
 * 
 * emulates the printf (or sprintf()) function using #NspObjects as arguments 
 * 
 * Returns: %OK or %FAIL 
 **/

int do_printf (char *fname, FILE *fp, char *format, Stack stack, int nargs, int arg_cnt, int line, char **strv)
{
  int m1;
  char save;
  char *p;
  register char *q;
  void *target;
  char *s_target=NULL;
  int l_flag, h_flag;		/* seen %ld or %hd  */
  int ast_cnt;
  int ast[2];
  double dval = 0.0;
  char *sval= "bug";
  int num_conversion = 0;	/* for error messages */
  int pf_type = 0;		/* conversion type */
  PRINTER printer;		/* pts at fprintf() or sprintf() */
  int argcnt;
  int first_arg = arg_cnt+1;
  int retval;			/* Attempt to return C-printf() return-val */
  q = format;
  retval = 0;
  arg_cnt=-1;                   /* count the columns */

  argcnt = P_count_columns(stack,first_arg,nargs); /*To count backward */

  if (fp == (FILE *) 0)		
    {
      /* doing sprintf */
      target = s_target =  sprintf_buff;
      printer = (PRINTER) sprintf;
    }
  else if ( fp == stdout ) 
    {
      printer = do_printf_stdout;
      target = stdout;
    }
  else
    {
      /* doing printf */
      target =  fp;	/* will never change */
      printer = (PRINTER) fprintf;
    }

  /* Traverse format string, doing printf(). */
  while (1)
    {
      if (fp)			/* printf */
	{
	  while (*q != '%')
	    {
	      if (*q == 0)
		{
		  fflush ((FILE *) target);
		  return (retval);
		}
	      else
		{
		  (*printer) (target, "%c",*q);
		  /* putc (*q, fp);*/
		  q++;
		  retval++;
		}
	    }
	}
      else
	{
	  /* sprintf() */
	  while (*q != '%')
	    {
	      if (*q == 0)
		{
		  if ( (s_target) > sprintf_limit)	/* damaged */
		    {
		      Scierror("Error: sprintf problem, buffer too small");
		      return RET_BUG;
		    }
		  else
		    {
		      /* really done */
		      *(s_target) = '\0';
		      *strv = sprintf_buff;
		      return (retval);
		    }
		}
	      else
		{
		  *(s_target)++ = *q++;
		  retval++;
		}
	    }
	}

      num_conversion++;
      
      if (*++q == '%')		/* %% */
	{
	  if (fp)
	    {
	      (*printer) (target, "%c",*q);
	      /* putc (*q, fp);*/
	      retval++;
	    }
	  else
	    {
	      *(s_target)++ = *q;
	    }
	  q++;
	  continue;
	}

      /* 
       * We have found a conversion specifier, figure it out,
       * then print the data asociated with it.
       */

      if (argcnt <= 0)
	{
	  if (fp) 
	    {
	      (*printer) (target, "\n");
	      fflush ((FILE *) target);
	    }
	  else 
	    {
	      target = s_target ;
	      (*printer) (target, "\n");
	    }
	  Scierror("Error: printf: not enough arguments\n");
	  return RET_BUG;
	}
      else
	{
 	  /* Get The data object from the arg-list */
	  ++arg_cnt;
	  argcnt--;
	}

      /* mark the '%' with p */
      p = q - 1;

      /* eat the flags */
      while (*q == '-' || *q == '+' || *q == ' ' ||
	     *q == '#' || *q == '0')
	q++;

      ast_cnt = 0;		/* asterisk count */
      if (*q == '*')
	{
	  /* Use current arg as field width spec */
	  if (P_GetScalarInt(stack,first_arg,nargs,arg_cnt,line,&m1) == FAIL) return -1;
	  ast[ast_cnt++] = m1;
	  q++;

	  if (argcnt <= 0)
	    {
	      if (fp) 
		{
		  (*printer) ( target, "\n");
		  fflush ((FILE *) target);
		}
	      else 
		{
		  target = s_target ;
		  (*printer) ( target, "\n");
		}
	      Scierror("Error: printf: not enough arguments\n");
	      return RET_BUG;
	    }
	  else
	    {
	      /* Get next arg */
	      ++arg_cnt;
	      argcnt--;
	    }
	}
      else
	while ( isdigit(((int)*q)))  q++;
      /* width is done */

      if (*q == '.')		/* have precision */
	{
	  q++;
	  if (*q == '*')
	    {
	      /* Use current arg as precision spec */
	      if (P_GetScalarInt(stack,first_arg,nargs,arg_cnt,line,&m1) == FAIL) return RET_BUG;
	      ast[ast_cnt++] = m1;
	      q++;

	      if (argcnt <= 0)
		{
		  if (fp) 
		    {
		      (*printer) ( target, "\n");
		      fflush ((FILE *) target);
		    }
		  else 
		    {
		      target = s_target ;
		      (*printer) ( target, "\n");
		    }
		  Scierror("Error: printf: not enough arguments\n");
		  return RET_BUG;
		}
	      else
		{
		  /* Get next arg */
		  ++arg_cnt;
		  argcnt--;
		}
	    }
	  else
	    while ( isdigit(((int)*q)) ) q++;
	}

      if (argcnt < 0)
	{
	  if (fp) 
	    {
	      (*printer) ( target, "\n");
	      fflush ((FILE *) target);
	    }
	  else 
	    {
	      target = s_target ;
	      (*printer) ( target, "\n");
	    }
	  Scierror("Error: printf: not enough arguments\n");
	  return RET_BUG;
	}

      l_flag = h_flag = 0;

      if (*q == 'l')
	{
	  q++;
	  l_flag = 1;
	}
      else if (*q == 'h')
	{
	  q++;
	  h_flag = 1;
	}
      
      /* Set pf_type and load val */
      switch (*q++)
	{
	case 's':
	  if (l_flag + h_flag)
	    Scierror("Warning: printf: bad conversion l or h flag mixed with s directive\n");
	  if ((sval = P_GetString(stack,first_arg,nargs,arg_cnt,line)) == NULL) return RET_BUG;
	  pf_type = PF_S;
	  break;
	case 'c':
	  if (l_flag + h_flag)
	    Scierror("Warning: printf: bad conversion l or h flag mixed with c directive\n");
	  if ((sval = P_GetString(stack,first_arg,nargs,arg_cnt,line)) == NULL) return RET_BUG;
	  pf_type = PF_C;
	  break;
	case 'd':
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = PF_D;
	  break;

	case 'o':
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = PF_UD;
	  break;

	case 'x':
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = PF_UD;
	  break;

	case 'X':
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = PF_UD;
	  break;

	case 'i':
	  /* use strod() here */
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = l_flag ? PF_LD : PF_D;
	  break;
	case 'u':
	  /* use strod() here */
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = l_flag ? PF_LUD : PF_UD;
	  break;

	case 'e':
	case 'g':
	case 'f':
	case 'E':
	case 'G':
	  if (h_flag ) /* + l_flag) */
	    {
	      Scierror("Error: printf: bad conversion\n");
	      return RET_BUG;
	    }
	  /* use strod() here */
	  if (P_GetScalarDouble(stack,first_arg,nargs,arg_cnt,line,&dval) == FAIL) return RET_BUG;
	  pf_type = PF_F;
	  break;

	default:
	  Scierror("Error: printf: bad conversion\n");
	  return RET_BUG;
	}

      save = *q;
      *q = 0;

      /* ready to call printf() */
      /* 
       * target:   The output file (or variable for sprintf())
       * p:        the beginning of the format
       * ast:      array with asterisk values
       */

      if (! fp) target = s_target ;

      switch (AST (ast_cnt, pf_type))
	{
	case AST (0, PF_C):
	  retval += (*printer) ( target, p, sval[0]);
	  break;

	case AST (1, PF_C):
	  retval += (*printer) ( target, p, ast[0], sval[0]);
	  break;

	case AST (2, PF_C):
	  retval += (*printer) ( target, p, ast[0], ast[1],sval[0]);
	  break;

	case AST (0, PF_S):
	  retval += (*printer) ( target, p, sval);
	  break;

	case AST (1, PF_S):
	  retval += (*printer) ( target, p, ast[0], sval);
	  break;

	case AST (2, PF_S):
	  retval += (*printer) ( target, p, ast[0], ast[1], sval);
	  break;

	case AST (0, PF_D):
	  retval += (*printer) ( target, p, (int) dval);
	  break;

	case AST (1, PF_D):
	  retval += (*printer) ( target, p, ast[0], (int) dval);
	  break;

	case AST (2, PF_D):
	  retval += (*printer) ( target, p, ast[0], ast[1], (int) dval);
	  break;

	case AST (0, PF_UD):
	  retval += (*printer) ( target, p, (unsigned int) dval);
	  break;

	case AST (1, PF_UD):
	  retval += (*printer) ( target, p, ast[0], (unsigned int) dval);
	  break;

	case AST (2, PF_UD):
	  retval += (*printer) ( target, p, ast[0], ast[1], (unsigned int) dval);
	  break;

	case AST (0, PF_LD):
	  retval += (*printer) ( target, p, (long int) dval);
	  break;

	case AST (1, PF_LD):
	  retval += (*printer) ( target, p, ast[0], (long int) dval);
	  break;

	case AST (2, PF_LD):
	  retval += (*printer) ( target, p, ast[0], ast[1], (long int) dval);
	  break;

	case AST (0, PF_LUD):
	  retval += (*printer) ( target, p, (long unsigned int) dval);
	  break;

	case AST (1, PF_LUD):
	  retval += (*printer) ( target, p, ast[0], (long unsigned int) dval);
	  break;

	case AST (2, PF_LUD):
	  retval += (*printer) ( target, p, ast[0], ast[1], (long unsigned int) dval);
	  break;

	case AST (0, PF_F):
	  retval += (*printer) ( target, p, dval);
	  break;

	case AST (1, PF_F):
	  retval += (*printer) ( target, p, ast[0], dval);
	  break;

	case AST (2, PF_F):
	  retval += (*printer) ( target, p, ast[0], ast[1], dval);
	  break;
	}
      if (fp == (FILE *) 0)
	{
	  while (*s_target)  s_target++;
	}
      *q = save;
    }
}





/*----------------------------------------------------------------
 * Utilities: 
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 *-------------------------------------------------------------*/

static const char readerr[] = "Premature EOF while reading sample file.";
static const char writerr[] = "Error writing sample file.  You are probably out of disk space.";

/* generic swap routine */

static void swapb(char *l, char *f, int n)
{    
  int i;
  for (i= 0; i< n; i++) f[i]= l[n-i-1];
}

/* Byte swappers */

static unsigned short swapw(unsigned short us)
{
  return ((us >> 8) | (us << 8)) & 0xffff;
}

/* swapl : swap a long : note that a long size is machine dependant **/

static unsigned long swapl(long unsigned int ul)
{
  unsigned long  sdf;
  swapb((char *) &ul,(char *) &sdf, sizeof(unsigned long));
  return (sdf);
}

/* swap an int assumed to be on 4 bytes **/

static unsigned int swapi(unsigned int ul)
{
  return (ul >> 24) | ((ul >> 8) & 0xff00) | ((ul << 8) & 0xff0000) | (ul << 24);
}

/* return swapped 32-bit float */

static float swapf(float uf)
{
  if (sizeof(long) == sizeof(float)){
    union {
      unsigned long l;  /* we assume here long is 4 bytes **/
      float f;
    } u;
    u.f= uf;
    u.l= (u.l>>24) | ((u.l>>8)&0xff00) | ((u.l<<8)&0xff0000) | (u.l<<24);
    return u.f;
  }
  else {
    union {
      unsigned int l;  /* we assume here int is 4 bytes **/
      float f;
    } u;
    u.f= uf;
    u.l= (u.l>>24) | ((u.l>>8)&0xff00) | ((u.l<<8)&0xff0000) | (u.l<<24);
    return u.f;
  }
}

static double swapd(double df)
{
  double sdf;
  swapb((char *) &df,(char *) &sdf, sizeof(double));
  return (sdf);
}

#ifndef HAVE_STRERROR
/* strerror function */
char *strerror(int errcode)
{
  static char  nomesg[30];
  extern int sys_nerr;
  extern char *sys_errlist[];
  if (errcode < sys_nerr)
    return (sys_errlist[errcode]);
  else
    {
      sprintf (nomesg, "Undocumented error %d", errcode);
      return (nomesg);
    }
}
#endif


