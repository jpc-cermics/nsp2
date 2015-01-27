/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

/*
 * Interface demo file 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

static void not(NspBMatrix *B) ;

int int_second(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* since not modify its argument we must use a copy */ 
  if ((A = GetBMatCopy(stack,1)) == NULLBMAT)  return RET_BUG;
  not(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1; 
}

static void not(NspBMatrix *B) 
{
  int i;
  for ( i=0 ; i < B->mn ; i++)  B->B[i] = !  B->B[i];
}

static OpTab libaddinter_func[]={
  {"first",int_first},
  {"second",int_second},
  {(char *) 0, NULL}
};

int libaddinter_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(libaddinter_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void libaddinter_Interf_Info(int i, char **fname, function (**f))
{
  *fname = libaddinter_func[i].name;
  *f = libaddinter_func[i].fonc;
}



