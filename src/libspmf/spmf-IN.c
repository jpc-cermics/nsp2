/* Nsp
 * Copyright (C) 2007 Bruno Pinçon Esial/Iecn
 * Copyright (C) 2007 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#include <math.h>
#include <string.h>
#include <nsp/machine.h>
#include <nsp/matrix-in.h>
#include <nsp/bmatrix-in.h>
#include <nsp/spmf.h>


static int int_nsp_log1p(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_log1p(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_sinpi(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_sinpi(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_gammabr(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_gamma(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_lngamma(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_lngamma(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_kcdf(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *P;
  int i, n;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (x = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( GetScalarInt(stack,2,&n) == FAIL ) return RET_BUG;      

  if ( (P = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT) return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    if ( nsp_kcdf(x->R[i], P->R + i, n) == FAIL )
      {
	nsp_matrix_destroy(P); return RET_BUG;
      }

  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

static int int_nsp_kcdflim(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *P;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( (P = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT) return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    P->R[i] = nsp_kcdflim(x->R[i]);

  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

static int int_nsp_kcdfbis(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *P;
  int i, n;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (x = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( GetScalarInt(stack,2,&n) == FAIL ) return RET_BUG;      

  if ( (P = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT) return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    P->R[i] = marsaglia_K(x->R[i], n);

  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

static OpTab Spmf_func[]={
  {"log1p", int_nsp_log1p},
  {"sinpi", int_nsp_sinpi},
  {"gammabr", int_nsp_gammabr},
  {"lngamma", int_nsp_lngamma},
  {"kcdf", int_nsp_kcdf},
  {"kcdflim", int_nsp_kcdflim},
  {"kcdfbis", int_nsp_kcdfbis},
  {(char *) 0, NULL}
};

int Spmf_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Spmf_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Spmf_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Spmf_func[i].name;
  *f = Spmf_func[i].fonc;
}
