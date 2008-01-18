/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 * Graphic library
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/


/*------------------------------------------------------------------------
 * Allocation routines for working arrays 
 ------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

/*--------------------------------------------
 * void graphic_alloc_info() 
 * void graphic_alloc_free()
 * void * graphic_alloc(indice,n,size)
 *
 * maintains a set of S_alloc_max working arrays 
 * Usage : 
 *   int *x = graphic_alloc(0,10,sizeof(int)) 
 *   double *y = graphic_alloc(1,100,sizeof(double))
 --------------------------------------------*/

#define NBPOINTS 256 

typedef struct s_alloc { 
  int init;          /* used for first allocation check */
  unsigned int size; /* currently allocated space in bytes */
  void *storage;     /* pointer to allocated space */
} S_alloc;

#define S_alloc_max 9 

static S_alloc Storage[S_alloc_max] = { {0,0,NULL},{0,0,NULL},{0,0,NULL},
					{0,0,NULL},{0,0,NULL},{0,0,NULL},
					{0,0,NULL},{0,0,NULL},{0,0,NULL}};

void * graphic_alloc(int indice, int n, unsigned int size)
{
  int *p; 
  unsigned int size_needed = n*size;
  unsigned int block_size = Storage[indice].size;
  /* check indice */
  if ( indice < 0 || indice >= S_alloc_max ) return 0;
  if ( n == 0 ) return 0;
  if (size_needed <= block_size  ) 
    /* no need to alloc or realloc */
    return Storage[indice].storage;
  /* compute size to be dynamically allocated */
  while ( size_needed > block_size ) block_size += NBPOINTS ;
  if ( Storage[indice].init == 0) 
    /** Allocation **/
    { p = (int*) MALLOC( block_size); }
  else
    /** Reallocation **/
    { p = (int *)  REALLOC( Storage[indice].storage,block_size ) ; }
  if ( p == NULL) 
    return 0;
  Storage[indice].storage= p ;
  Storage[indice].init = 1;
  Storage[indice].size = block_size;
  return Storage[indice].storage;
}

void graphic_alloc_info(void)
{
  unsigned int gsize=0;
  int i;
  for (i = 0 ; i < S_alloc_max ; i++) 
    if ( Storage[i].init == 1 ) gsize += Storage[i].size;
  sciprint("Graphic allocated dynamic memory: %ud bytes\n",gsize);
}

void graphic_alloc_free(void)
{
  int i;
  for (i = 0 ; i < S_alloc_max ; i++) 
    if ( Storage[i].init == 1 ) 
      {
	free(Storage[i].storage);
	Storage[i].init = 0;
      }
}

