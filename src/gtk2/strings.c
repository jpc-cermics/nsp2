/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include <gtk/gtk.h> 
#include "nsp/object.h"

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

#define DEBUG_STR(x) 
/* #define DEBUG_STR(x) sciprint(x) */
  
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
 * converts a string to Utf8 with the following assumptions.
 * if string is utf8 do nothing and return @str. If locale 
 * is UTF8 and the string is not utf8 then assume that the string is 
 * ISO-8859-15. If locale is not UTF8 then assume that the string is 
 * given with locale coding.
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
      if (g_get_charset (NULL)) 
	{
	  DEBUG_STR("xname: gtk_window_set_title is used with a non utf8 string and your locale is UTF8\r\n");
	  DEBUG_STR("       assuming that your string is ISO-8859-15\r\n");
	  str_utf8 = g_convert (str, -1,"UTF8","ISO-8859-15", NULL, NULL, NULL);
	  if ( str_utf8 == NULL)  DEBUG_STR("xname: convertion to UTF-8 failed\r\n");
	}
      else 
	{
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
