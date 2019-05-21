#ifndef NSP_INC_INTS
#define NSP_INC_INTS 

/*
 * Copyright (C) 2009-2019 Jean-Philippe Chancelier Enpc/Cermics
 *                         Bruno Pincon Esial/Iecn
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

#include <nsp/intsf.h>  /* forward declarations */

#if ( defined(__MSC__) && defined(_MSC_VER) ) || defined(NOGLIBH)
typedef int gint    ;
typedef unsigned int guint   ;
typedef short gshort  ;
typedef unsigned short gushort ;
typedef long int glong   ;
typedef unsigned long int gulong  ;
typedef signed char gint8   ;
typedef unsigned char guint8  ;
typedef signed short gint16  ;
typedef unsigned short guint16 ;
typedef int gint32  ;
typedef unsigned int guint32 ;
#ifdef _WIN64 
typedef __int64 gint64;
typedef unsigned __int64 guint64;
#else 
typedef long long int gint64;
typedef unsigned long long int guint64;
#endif 
#else /* __MSC__ */
#include <glib.h> 
#endif 

/* all the integer types */

union nsp_int_union_ { 
  gint     Gint;
  guint     Guint;
  gshort     Gshort;
  gushort     Gushort;
  glong     Glong;
  gulong     Gulong;
  gint8     Gint8;
  guint8     Guint8;
  gint16     Gint16;
  guint16     Guint16;
  gint32     Gint32;
  guint32     Guint32;
  gint64     Gint64;
  guint64     Guint64;
};

union  nsp_int_union_ptr_  { 
  void *    Iv;
  gint     *Gint;
  guint     *Guint;
  gshort     *Gshort;
  gushort     *Gushort;
  glong     *Glong;
  gulong     *Gulong;
  gint8     *Gint8;
  guint8     *Guint8;
  gint16     *Gint16;
  guint16     *Guint16;
  gint32     *Gint32;
  guint32     *Guint32;
  gint64     *Gint64;
  guint64     *Guint64;
};

#endif 

