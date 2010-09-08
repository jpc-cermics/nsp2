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

/*
 * Interface demo file 
 */

#include <nsp/nsp.h> 
#include <nsp/objects.h> 
#include <nsp/interf.h> 


static void plus(NspMatrix *M);

int int_first(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  plus(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
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

static OpTab tutorial_func[]={
  {"first",int_first},
  {"second",int_second},
  {(char *) 0, NULL}
};

int tutorial_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(tutorial_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void tutorial_Interf_Info(int i, char **fname, function (**f))
{
  *fname = tutorial_func[i].name;
  *f = tutorial_func[i].fonc;
}



static void not(NspBMatrix *B) 
{
  int i;
  for ( i=0 ; i < B->mn ; i++)  B->B[i] = !  B->B[i];
}


static void plus(NspMatrix *A) 
{
  int i;
  for ( i=0 ; i < A->mn ; i++)  A->R[i] += 2.0;
}



