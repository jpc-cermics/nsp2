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
 */

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/interf.h>

static void f99(NspMatrix *M);

int int_first(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  f99(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static void f99(NspMatrix *M)
{
  int i;
  if ( M->rc_type == 'r' ) 
    for ( i= 0 ; i < M->mn ; i++) M->R[i] *= 2.0;
  else 
    for ( i= 0 ; i < M->mn ; i++) { M->C[i].r *= 2.0;M->C[i].i *= 3.0;}
}





