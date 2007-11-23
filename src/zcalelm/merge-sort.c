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
 *
 * A mergsort specialized to int, double  by include 
 *
 */

#include <stdlib.h>
#include "nsp/machine.h" 
#include "nsp/gsort-p.h"

#define ELT_TYPE int
#include "merge-sort-gen.c"
#undef  ELT_TYPE

/*
 * double arrays or matrices 
 */

#define DOUBLE_ONLY
#define ELT_TYPE double
#include "merge-sort-gen.c"
#undef  ELT_TYPE
  
