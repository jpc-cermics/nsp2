/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * Uses the qsort by Author: B. Pincon (trying to accelerate the initial scilab dsort.f)
 * (i) n must be less than 2**(25) ! due to lengh of work space (ileft, iright)
 * (ii) quicksort is used with Sedgewick tricks
 *
 */

#include <stdlib.h>
#include <string.h>
#include <glib.h> /* for int64 */

#include "nsp/machine.h" 
#include "nsp/gsort-p.h"

#define ELT_TYPE int
#include "qsort2-gen.c"
#undef  ELT_TYPE

/*
 * double arrays or matrices 
 */

#define ELT_TYPE double
#define DOUBLE_ONLY
#include "qsort2-gen.c"
#undef DOUBLE_ONLY
#undef  ELT_TYPE


/*
 * all the imatrix sub-types 
 */
  
#define ELT_TYPE gint
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gshort
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gushort
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE glong
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gulong
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint8
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint8
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint16
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint16
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint32
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint32
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint64
#include "qsort2-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint64
#include "qsort2-gen.c"
#undef  ELT_TYPE
