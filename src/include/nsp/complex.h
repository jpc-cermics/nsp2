#ifndef NSP_INC_COMPLEX
#define NSP_INC_COMPLEX 

/*
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
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

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 
#include "nsp/objectf.h"  /* forward declarations */

/**
 * doubleC: 
 * @r: real part 
 * @i: imaginary part
 * 
 * structure used to store complex values i.e two doubles.
 */

/* typedef struct _doubleC doubleC; */

struct _doubleC { 
  double r;
  double i; 
};

#endif 
