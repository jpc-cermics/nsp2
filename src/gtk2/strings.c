/* -*-   Encoding: utf-8  -*-  */
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
 * utilities for string matrices using the glib 
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h> 
#include <gdk/gdk.h> 

#include <nsp/nsp.h>
#include <nsp/matrix.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/mpmatrix.h>
#include <nsp/hash.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>

static void nsp_swap_double_array(double *R,int n);
/*
 * string matrix conversion
 * FIXME: should be freed with g_free 
 *        
 */

/**
 * nsp_smatrix_convert:
 * @name: a string 
 * @A: a #NspSMatrix 
 * @to_codeset: name of character set into which to convert @A
 * @from_codeset: character set of @A.
 * 
 * returns a new #NspMatrix named @name which is a copy of @A 
 * where characters are converted from @from_codeset to @to_codeset
 * 
 * Return value: %NULLSMAT or the newly created #NspSMatrix
 **/

NspSMatrix* nsp_smatrix_convert(const char *name,NspSMatrix *A,const char *to_codeset,
				const char *from_codeset)
{
  NspSMatrix *Loc;
  int i=0;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(name,A->m,A->n,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < A->mn ; i++ )
    {
      Loc->S[i] = g_convert(A->S[i],-1,to_codeset,from_codeset, NULL, NULL, NULL);
      if (Loc->S[ i] == NULL ) return(NULLSMAT);
    }
  return(Loc);
}

#if 1
#define DEBUG_STR(x) 
#else 
#define DEBUG_STR(x) Sciprintf(x) 
#endif

/**
 * nsp_smatrix_to_utf8:
 * @A: a #NspSMatrix 
 * 
 * converts @A to Utf8 using #nsp_string_to_utf8.
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_smatrix_to_utf8(NspSMatrix *A)
{ 
  int i,ret=OK;
  for ( i= 0 ; i < A->mn ; i++)
    {
      char *loc= nsp_string_to_utf8(A->S[i]);
      if ( loc != A->S[i] && loc != NULL) 
	{
	  nsp_string_destroy(&(A->S[i]));
	  A->S[i]= loc;
	}
      else if ( loc == NULL) 
	{
	  ret= FAIL;
	}
    }
  return ret;
}

/**
 * nsp_string_to_utf8:
 * @str: string to be converted to Utf8
 * 
 * converts a string to utf8 with the following assumptions.
 * If the string is an utf8 string then do nothing and return @str. 
 * If locale is UTF8 and the string is not utf8 then assume that the string is ISO-8859-15. 
 * If locale is not UTF8 then assume that the string is given with locale coding.
 *
 * Return value: a pointer to the new allocated string or %NULL in case of allocation failure 
 * or a pointer to @str if the string was not converted.
 **/

nsp_string nsp_string_to_utf8(nsp_string str)
{ 
  char *str_utf8;
  if ( g_utf8_validate(str,-1,NULL) == TRUE ) 
    {
      DEBUG_STR("xname: str is utf8\r\n");
      str_utf8 = str;
    }
  else
    {
      const char *charset_cl=NULL;
      if (g_get_charset (&charset_cl)) 
	{
	  DEBUG_STR("xname: gtk_window_set_title is used with a non utf8 string and your locale is UTF8\r\n");
	  DEBUG_STR("       assuming that your string is ISO-8859-15\r\n");
	  str_utf8 = g_convert (str, -1,"UTF8","ISO-8859-15", NULL, NULL, NULL);
	  if ( str_utf8 == NULL)  DEBUG_STR("xname: convertion to UTF-8 failed\r\n");
	}
      else 
	{
	  DEBUG_STR("xnam: the locale charset : ");DEBUG_STR(charset_cl);DEBUG_STR("\r\n");
	  str_utf8 =g_locale_to_utf8 (str, -1, NULL, NULL, NULL);
	  DEBUG_STR("xname: from locale to UTF8\r\n");
	  if ( str_utf8 == NULL) DEBUG_STR("xname: convertion to UTF-8 failed\r\n");
	}
    }
  return str_utf8;
}



/**
 * nsp_smatrix_utf8_validate:
 * @A: a #NspSMatrix 
 * 
 * %TRUE if all the elements of @A are Utf8.
 * 
 * Return value: %TRUE or %FALSE 
 **/

int nsp_smatrix_utf8_validate(NspSMatrix *A)
{
  int i;
  for ( i= 0 ; i < A->mn ; i++)
    if ( g_utf8_validate(A->S[i],-1,NULL) == FALSE ) 
      return FALSE;
  return TRUE;
}

/**
 * nsp_string_length:
 * @str: string 
 * 
 * Compute the length of a string checking if string is utf8 or not.
 * 
 * Return value: an integer 
 **/

int nsp_string_length(nsp_const_string str ) 
{
  if ( g_utf8_validate(str,-1,NULL))
    {
      return g_utf8_strlen (str,-1);
    }
  else
    {
      return strlen(str);
    }
}

/**
 * nsp_string_utf8_pos:
 * @str: string 
 * @i: integer
 * 
 * If @str is an utf8 string, returns the index of the @i-th byte of @str
 * in terms of utf8 characters. @i must be a byte which starts an utf8 character.
 * In case of errors, -1 is returned.
 *
 * Return value: an integer 
 **/

int nsp_string_utf8_pos(nsp_const_string str, int i) 
{
  /* retourne un offset en terme de nbre de characteres utf8 
     int io=g_utf8_pointer_to_offset (str,&str[8]);
     gchar *res= g_utf8_offset_to_pointer (str,3);
     inversement donne le charactere utf8 situé a un offset donné 
     Sciprintf("character obtenue apres offset de 3 %c qui est a pos %d \n",*res,(int) (res-str));
     Sciprintf("le character %c est a un offset de %d \n",str[8],io);
     char *p = g_utf8_find_prev_char (str,&str[i]);
     Sciprintf("Previous utf8 char is %c at position %d\n",*p,(int) (p-str));
     if (g_utf8_find_prev_char (str,&str[i]) != &str[i]) 
     return g_utf8_strlen (str, Min(i,strlen(str) ));
  */
  if (  0 <= i && i < strlen(str) && g_utf8_validate(str,-1,NULL) )
    {
      return g_utf8_pointer_to_offset (str,&str[i]);
    }
  else
    {
      return -1;
    }
}



/**
 * nsp_smatrix_utf8_from_unichar:
 * @A: a #NspMatrix with unichar codes 
 * 
 * converts the scalar matrix @A to an utf8 string matrix.
 * Each entry of @A is assumed to be an unichar code (ISO10646).
 * 
 * Return value: a new #NspSMatrix or %NULLSMAT
 **/

NspSMatrix *nsp_smatrix_utf8_from_unichar(NspMatrix *A) 
{
  int i,j;
  char buf[7]; /* 6 bytes long */
  NspSMatrix *Loc;
  if ( A->mn == 0) return nsp_smatrix_create(NVOID,0,0,"v",0);
  if ((Loc =nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1))== NULLSMAT)
    return(NULLSMAT);
  for ( i = 0 ; i < A->mn ; i++)
    {
      int n;
      n= g_unichar_to_utf8((unsigned int) A->R[i],buf);
      buf[n]='\0';
      if ((Loc->S[i] = nsp_string_copy(buf)) == (nsp_string) 0 ) 
	{
	  for ( j = i ; j < A->mn ; j++) Loc->S[i]=NULL;
	  nsp_smatrix_destroy(Loc);
	  return(NULLSMAT);
	}
    }
  return(Loc);
}

/**
 * nsp_smatrix_to_latin1:
 * @A: a #NspSMatrix 
 * 
 * converts @A to latin1 using #nsp_string_to_latin1.
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_smatrix_to_latin1(NspSMatrix *A)
{ 
  int i,ret=OK;
  for ( i= 0 ; i < A->mn ; i++)
    {
      char *loc= nsp_string_to_latin1(A->S[i]);
      if ( loc != A->S[i] && loc != NULL) 
	{
	  nsp_string_destroy(&(A->S[i]));
	  A->S[i]= loc;
	}
      else if ( loc == NULL) 
	{
	  ret= FAIL;
	}
    }
  return ret;
}

/**
 * nsp_string_to_latin1:
 * @str: string to be converted to latin1
 * 
 * converts an utf8 string to latin1 
 * if string is not utf8 do nothing and return @str. 
 *
 * Return value: a pointer to the new allocated string or %NULL in case of allocation failure 
 * or a pointer to @str if the string was not converted.
 **/

nsp_string nsp_string_to_latin1(nsp_string str)
{ 
  char *str_latin1= NULL;
  if ( g_utf8_validate(str,-1,NULL) == FALSE ) 
    {
      str_latin1 = str;
    }
  else
    {
      str_latin1 = g_convert (str, -1,"LATIN1", "UTF8", NULL, NULL, NULL);
      if ( str_latin1 == NULL) 
	{
	  DEBUG_STR("xname: convertion to latin1 failed\r\n");
	}
    }
  return str_latin1;
}


/**
 * nsp_mat_to_base64string:
 * @A: a #NspMatrix
 * 
 * allocate a #nsp_string filled with the base64 code 
 * of the data contained in A;
 * 
 * Returns: a new #nsp_string
 **/

nsp_string nsp_mat_to_base64string(NspMatrix *A) 
{
  /* */
  nsp_string out;
  const guchar *data = (const guchar *) A->R;
  gint state = 0, save = 0;
  int dlen =A->mn*((A->rc_type == 'c') + 1);
  int len = dlen*sizeof(double);
  int outlen;
  out = new_nsp_string_n(len * 4 / 3 + 4);
  if ( out == NULL ) return NULL;
  nsp_swap_double_array(A->R,dlen);
  outlen = g_base64_encode_step (data, len, FALSE, out, &state, &save);
  outlen += g_base64_encode_close (FALSE, out + outlen, &state, &save);
  out[outlen] = '\0';
  nsp_swap_double_array(A->R,dlen);
  return out;
}


/**
 * nsp_base64string_to_doubles:
 * @text: a #nsp_string 
 * @out_len: an int pointer 
 * 
 * Decode the base 64 string into an array of doubles.
 * The array of double is allocated.
 * 
 * Returns: an allocated and filled array of doubles.
 **/

double *nsp_base64string_to_doubles(nsp_string text, int *out_len) 
{
  guchar *ret;
  gint input_length, state = 0;
  guint save = 0;
  input_length = strlen (text);
  ret = malloc((input_length * 3 / 4)*sizeof(char));
  *out_len = g_base64_decode_step (text, input_length, ret, &state, &save);
  if ( (*out_len % sizeof(double) ) != 0 ) 
    {
      Scierror("The base64 string cannot be decoded to an array of double\n");
      return NULL;
    }
  nsp_swap_double_array((double *)ret, (*out_len)/sizeof(double) );
  return (double *) ret; 
}

/**
 * nsp_string_to_base64string:
 * @str: a nsp_string
 * 
 * allocate a #nsp_string filled with the base64 code 
 * of the data contained in @str. Since @str can contain 
 * null characters, the length to be copied is given in len.
 * 
 * Returns: a new #nsp_string
 **/

nsp_string nsp_string_to_base64string(nsp_const_string str,unsigned int len)
{
  const guchar *data = (const guchar *) str;
  gint state = 0, save = 0;
  int outlen;
  nsp_string out = new_nsp_string_n(len * 4 / 3 + 4);
  if ( out == NULL ) return NULL;
  outlen = g_base64_encode_step (data, len, FALSE, out, &state, &save);
  outlen += g_base64_encode_close (FALSE, out + outlen, &state, &save);
  out[outlen] = '\0';
  return out;
}

/**
 * nsp_base64string_to_nsp_string:
 * @text: a #nsp_string 
 * @out_len: an int pointer 
 * 
 * Decode the base 64 string into an array of doubles.
 * The array of double is allocated.
 * 
 * Returns: an allocated and filled array of doubles.
 **/

nsp_string nsp_base64string_to_nsp_string(nsp_const_string text, int *out_len) 
{
  gint input_length= strlen (text), state = 0;
  guint save = 0;
  guchar *ret = malloc((input_length * 3 / 4)*sizeof(char));
  if (ret == NULL ) return NULL;
  *out_len = g_base64_decode_step (text, input_length, ret, &state, &save);
  return (nsp_string) ret; 
}

void nsp_test_base64(NspMatrix *A)
{
  int i;
  nsp_string str;
  int outlen;
  double *d;
  str = nsp_mat_to_base64string(A);
  d = nsp_base64string_to_doubles(str,&outlen);
  if (outlen/sizeof(double) != A->mn) 
    {
      Sciprintf("nsp_test_base64 failed\n");
      return;
    }
  for ( i = 0 ; i < A->mn ; i++)
    if (A->R[i] != d[i] ) 
      Sciprintf("nsp_test_base64 failed\n");
}

/* swap an array of double if the machine 
 * is little_endian. 
 */

static void nsp_swap_double_array(double *R,int n)
{
  double dest;
  int len =sizeof(double);
  int i,j, littlendian = 1;
  char	*endptr =  (char *) &littlendian;
  if (*endptr)
    {
      /* machine is little endian */
      for ( i = 0 ; i < n ; i++)
	{
	  char *c_orig =(char *) &R[i];
	  char *c_dest =(char *) &dest ;
	  for (j= 0; j < len ; j++)	c_dest[j]= c_orig[len-j-1];
	  R[i]=dest;
	}
    }
}

