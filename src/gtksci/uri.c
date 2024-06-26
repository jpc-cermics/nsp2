/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * The GIMP Help Browser - URI functions
 * Copyright (C) 2001  Jacob Schroeder  <jacob@convergence.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/* this file is used when helpbrowser.c is used */

#include <string.h>
#include <glib.h>
#include "uri.h"

/*  #define URI_DEBUG 1  */

typedef enum
{
  URI_UNKNOWN,
  URI_ABSURI,
  URI_NETPATH,
  URI_ABSPATH,
  URI_RELPATH,
  URI_QUERY,
  URI_EMPTY,
  URI_FRAGMENT,
  URI_INVALID
} UriType;


static UriType
uri_get_type (const gchar *uri)
{
  gchar        c;
  const gchar *cptr;
  UriType      type = URI_UNKNOWN;
  
  if (!uri)
    return type;

  cptr = uri;
  c = *cptr++;

  if (g_ascii_isalpha (c))
    {
      type = URI_RELPATH;  /* assume relative path */
      
      while ((c = *cptr++))
        {
          if (g_ascii_isalnum (c) || c == '+' || c == '-' || c == '.')
            continue;
          
          if (c == ':')
            {
              /* it was a scheme */
              type = URI_ABSURI;
            }
          break;
        }
    }
  else
    {
      switch (c)
        {
        case '/':
          if (*cptr == '/')
            {
              cptr++;
              type = URI_NETPATH;
            }
          else
            {
              type = URI_ABSPATH;
            }
          break;
        case '?':
          type = URI_QUERY;
          break;
        case '#':
          type = URI_FRAGMENT;
          break;
        case '\0':
          type = URI_EMPTY;
          break;
        default:
          type = URI_RELPATH;
          break;
        }
    }

#ifdef URI_DEBUG
  g_print ("uri_get_type (\"%s\") -> ", uri);
  switch (type)
    {
    case URI_UNKNOWN:  g_print ("unknown");  break;
    case URI_ABSURI:   g_print ("absuri");   break;
    case URI_NETPATH:  g_print ("netpath");  break;
    case URI_ABSPATH:  g_print ("abspath");  break;
    case URI_RELPATH:  g_print ("relpath");  break;
    case URI_QUERY:    g_print ("query");    break;
    case URI_EMPTY:    g_print ("empty");    break;
    case URI_FRAGMENT: g_print ("fragment"); break;
    case URI_INVALID:  g_print ("invalid");  break;
    }
  g_print ("\n");
#endif

  return type;
}

gchar *
uri_to_abs (const gchar *uri,
            const gchar *base_uri)
{
  gchar        c;
  const gchar *cptr;
  gchar       *retval    = NULL;
  UriType      uri_type  = URI_UNKNOWN;
  UriType      base_type = URI_UNKNOWN;
  
  gint base_cnt    =  0;  /* no of chars to be copied from base URI  */
  gint uri_cnt     =  0;  /* no of chars to be copied from URI       */
  gint sep_cnt     =  0;  /* no of chars to be inserted between them */
  gint path_offset = -1;
  
  const gchar *sep_str = ""; /* string to insert between base and uri */
  const gchar *part;
  const gchar *last_segment = NULL;
  
#ifdef URI_DEBUG
  g_print ("uri_to_abs (\"%s\", \"%s\")\n", uri, base_uri);
#endif

  /* this function does not use the algorithm that is being proposed
   * in RFC 2396. Instead it analyses the first characters of each
   * URI to determine its kind (abs, net, path, ...).
   * After that it locates the missing parts in the base URI and then
   * concats everything into a newly allocated string.
   */
  
  /* determine the kind of the URIs */
  uri_type = uri_get_type (uri);
  
  if (uri_type != URI_ABSURI)
    {
      base_type = uri_get_type (base_uri);
    
      if (base_type != URI_ABSURI)
        {
          g_warning ("base uri is not absolute: '%s'\n", base_uri);
          return NULL;
        }
    }

  /* find missing parts in base URI */
  switch (uri_type)
    {
    case URI_ABSURI:
      /* base uri not needed */
      break;

    case URI_QUERY:
      /* ??? last segment? */
      uri_type = URI_RELPATH;
    case URI_NETPATH:  /* base scheme */
    case URI_ABSPATH:  /* base scheme and authority */
    case URI_RELPATH:  /* base scheme, authority and path */
      cptr = base_uri;

      /* skip scheme */
      while ((c = *cptr++) && c != ':')
        ; /* nada */
      
      base_cnt = cptr - base_uri; /* incl : */

      if (*cptr != '/')
        {
          /* completion not possible */
          return NULL;
        }

      if (uri_type == URI_NETPATH)
        break;
      
      /* skip authority */
      if (cptr[0] == '/' && cptr[1] == '/')
        {
          part = cptr;
          cptr += 2;

          while ((c = *cptr++) && c != '/' && c != '?' && c != '#')
            ; /* nada */

          cptr--;
          base_cnt += cptr - part;
        }

      path_offset = base_cnt; 

      if (uri_type == URI_ABSPATH)
        break;
      
      /* skip path */
      if (*cptr != '/')
        {
          sep_cnt = 1;
          sep_str = "/";
          break;
        }

      part = cptr;
      
      g_assert (*cptr == '/');
      
      while ((c = *cptr++) && c != '?' && c != '#')
        {
          if (c == '/')
            last_segment = cptr - 1;
        };
      
      g_assert (last_segment);
      
      cptr = last_segment;
      
      while ((c = *uri) && c == '.' && cptr > part)
        {
          gint shift_segment = 0;

          c = uri[1];
          
          if (c == '.' )
            {
              c = uri[2];
              shift_segment = 1;
            }

          if (c == '/')
            {
              uri += 2;
            } 
          else if (c == 0 || c == '?' || c == '#')
            {
              uri += 1;
            } 
          else 
            {
              break;
            }

          g_assert (*cptr == '/');
          
          if (shift_segment)
            {
              uri += 1;
              while (cptr > part && *--cptr != '/')
                ; /* nada */
            }
        }
      
      base_cnt += cptr - part + 1;
      break;
      
    case URI_EMPTY:
    case URI_FRAGMENT:
      /* use whole base uri */
      base_cnt = strlen (base_uri);
      break;

    case URI_UNKNOWN:
    case URI_INVALID:
      return NULL;
    }
  
  /* do not include fragment part from the URI reference */
  for (cptr = uri; (c = *cptr) && c != '#'; cptr++)
    ; /* nada */

  uri_cnt = cptr - uri;
  
  /* allocate string and copy characters */
  
  retval = g_new (gchar, base_cnt + sep_cnt + uri_cnt + 1);
  
  if (base_cnt)
    strncpy (retval, base_uri, base_cnt);

  if (sep_cnt)
    strncpy (retval + base_cnt, sep_str, sep_cnt);

  if (uri_cnt)
    strncpy (retval + base_cnt + sep_cnt, uri, uri_cnt);

  retval[base_cnt + sep_cnt + uri_cnt] = '\0';

#ifdef URI_DEBUG
  g_print ("  ->  \"%s\"\n", retval);
#endif

  return retval;
}

