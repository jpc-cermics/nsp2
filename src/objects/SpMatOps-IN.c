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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/spmatops-in.h"
#include "nsp/matutil.h" /* urand */

/*
 *  A=op(A) 
 */

typedef int (*M11) (NspSpMatrix *A);
typedef void (*VM11) (NspSpMatrix *A);

/* generic function for ones,rand,eyes **/
 
static int int_sp_gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static int int_sp_genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * SpMinus : A=-(A)
 * A is changed  
 * return 0 if error 
 */

int int_spminus(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_gen11(stack,rhs,opt,lhs,SpMinus);
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

static int int_spabs(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_gen11(stack,rhs,opt,lhs,SpAbs);
}

/*
 * A=Erf(A)
 */

/*
 * A=Erfc(A),  * A is changed 
 */

/*
 * A=Arg(A),  * A is changed 
 */

static int int_sparg(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_gen11(stack,rhs,opt,lhs,SpArg);
}

/*
 * SpCos : A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

typedef NspMatrix* (*SpM) (NspSpMatrix *A);

static int int_sp_m_gen11(Stack stack, int rhs, int opt, int lhs, SpM F)
{
  NspMatrix *Loc;
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ((Loc = (*F)(A)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Loc);
  return 1;
}

static int int_spcos(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_m_gen11(stack,rhs,opt,lhs,SpCos);
}

/*
 * SpCosh : A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */
static int int_spcosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_m_gen11(stack,rhs,opt,lhs,SpCosh);
}

/*
 * SpExpl : Exponentiation terme a term 
 * A is changed 
 */

static int int_spexpel(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_m_gen11(stack,rhs,opt,lhs,SpExpEl);
}

/*
 * SpLog : A=LogEl(A) 
 */

static int int_splogel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  if ( SpLogEl(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * SpSin : A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spsin(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpSin);
}

/*
 * SpSinh : A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spsinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpSinh);
}

/*
 * SpSqrtEl : A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

static int int_spsqrtel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  if ( SpSqrtEl(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * SpAcos : A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spacos(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_m_gen11(stack,rhs,opt,lhs,SpAcos);
}

/*
 * SpAcosh : A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spacosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_m_gen11(stack,rhs,opt,lhs,SpAcosh);
}

/*
 * SpAsin : A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spasin(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpAsin);
}

/*
 * SpAsinh : A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


static int int_spasinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpAsinh);
}

/*
 * SpATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spatan(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpAtan);
}

/*
 * SpArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spatanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpAtanh);
}

/*
 * SpCeil : A=Ceil(A)
 * A is changed  
 */

static int int_spceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpCeil);
}

/*
 * SpInt : A=Int(A)
 * A is changed  
 */

static int int_spint(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpInt);
}

/*
 * SpFloor : A=Floor(A)
 * A is changed  
 */
 
static int int_spfloor(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpFloor);
}

/*
 * SpRound : A=Round(A)
 * A is changed  
 */
 
static int int_spround(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpRound);
}

/*
 * SpSign : A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spsign(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_gen11(stack,rhs,opt,lhs,SpSign);
}

/*
 * SpTan : A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_sptan(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpTan);
}

/*
 * SpTanh : A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_sptanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sp_genv11(stack,rhs,opt,lhs,SpTanh);
}

/*
 * A=Polar(A,B),  * A is changed 
 */

/*
 * SpConj : A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

static int int_spconj(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0 || A->rc_type == 'r' )
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  SpConj(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * A=nsp_mat_pow_el(A,B), A.^ B 
 * with special cases Mat.^[]  and Mat.^scalar
 *                    [].^Mat ans scalar.^Mat
 */
/*
 * A=DivEl(A,B),  A ./ B 
 */
/*
 * A=BackDivEl(A,B),  A .\ B 
 */
/*
 * A=MultEl(A,B),  A .* B 
 */

/*
 * Matrix multiplication  Res= A*B  
 * with special cases Mat * [] and Mat * scalar
 * very similar to mopscal but MatMult returns a new matrix 
 */

/*
 * returns in a Matrix the indices for which the Matrix is true 
 */

static int int_spfind(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetSp(stack,1)) == NULLSP)  return RET_BUG;
  if ( SpFind(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Rr);
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NthObj(2)->ret_pos = 2;
      return 2;
    }
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * The Interface for basic numerical matrices operation 
 */

static OpTab SpMatOps_func[]={
  {"abs_sp",int_spabs},
  {"arg_sp",int_sparg},
  {"sin_sp",int_spsin},
  {"sinh_sp",int_spsinh},
  {"asin_sp",int_spasin},
  {"asinh_sp",int_spasinh},
  {"cos_sp",int_spcos},
  {"cosh_sp",int_spcosh},
  {"acos_sp",int_spacos},
  {"acosh_sp",int_spacosh},
  {"atan_sp",int_spatan},
  {"atanh_sp",int_spatanh},
  {"ceil_sp",int_spceil},
  {"int_sp",int_spint},
  {"floor_sp",int_spfloor},
  {"round_sp",int_spround},
  {"sign_sp",int_spsign},
  {"tan_sp",int_sptan},
  {"tanh_sp",int_sptanh},
  {"conj_sp",int_spconj},
  {"find_sp", int_spfind},
  {"sqrt_sp",int_spsqrtel},
  {"log_sp",int_splogel},
  {"exp_sp",int_spexpel},
  {(char *) 0, NULL}
};

int SpMatOps_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SpMatOps_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void SpMatOps_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SpMatOps_func[i].name;
  *f = SpMatOps_func[i].fonc;
}
