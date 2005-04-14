/* 
 * interface of lapack for Nsp 
 * Copyright (C) 2005 Jean-Philippe Chancelier
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

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <strings.h>
#include <nsp/machine.h>
#include <nsp/math.h>
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/approx.h"
#include "nsp/nsp_lapack.h"


static int int_qr( Stack stack, int rhs, int opt, int lhs)
{ 
  char *mode = NULL,cmode ;
  double tol = 0, *Tol=NULL;
  NspMatrix *A;
  NspMatrix *Q=NULL, *R=NULL, *rank=NULL, *E=NULL;
  NspMatrix **hrank=NULL,**hE=NULL;
  int_types T[] = {mat,new_opts,t_end} ;
  nsp_option opts[] ={{ "tol",s_double,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&tol,&mode) == FAIL) return RET_BUG;
  
  Tol = ( opts[0].obj == NULLOBJ) ? NULL : &tol; 
  cmode = ( opts[1].obj == NULLOBJ) ? 'x' : mode[0]; 
  if ( cmode != 'x' && cmode != 'e' ) 
    {
      Scierror("%s: mode should be 'x' or 'e' \n",stack.fname);
      return RET_BUG;
    }
  CheckLhs(1,4);
  if ( lhs >= 3) { hE= &E;}
  if ( lhs >= 4) { hrank= &rank;}
  if ( nsp_qr(A,&Q,&R,hrank,hE,Tol,cmode)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Q));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(R));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(E));
  if ( lhs >= 4 ) MoveObj(stack,4,NSP_OBJECT(rank));
  return Max(lhs,1);
}

/* flag = 'n' for minimize Norm */ 
/* flag = 'z' for zero setting  */


static int int_lsq( Stack stack, int rhs, int opt, int lhs)
{ 
  char *mode = NULL,cmode ;
  NspMatrix *A,*B;
  NspMatrix *Res;
  int_types T[] = {mat,mat,new_opts,t_end} ;
  nsp_option opts[] ={{ "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&B,&opts,&mode) == FAIL) return RET_BUG;

  cmode = ( opts[1].obj == NULLOBJ) ? 'n' : mode[0]; 
  if ( cmode != 'n' && cmode != 'z' ) 
    {
      Scierror("%s: mode should be 'n' or 'z' \n",stack.fname);
      return RET_BUG;
    }
  CheckLhs(1,1);
  if ((Res= nsp_lsq(A,B,cmode))== NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

/* XXXXX for test */

extern double C2F(dlamch)(char *,int );
extern double cdf_dlamch(char *,int );

static int int_dlamch( Stack stack, int rhs, int opt, int lhs)
{
  double x,y,z;
  char *mode;
  int_types T[] = {string,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&mode) == FAIL) return RET_BUG;
  x = nsp_dlamch(mode);
  y = C2F(dlamch)(mode,strlen(mode));
  z = cdf_dlamch(mode,strlen(mode));
  fprintf(stderr," %e %e %e %d\n",x,y,z,x==y);
  return 0;
  
}


/*
 * svd: 
 *    S is always computed 
 *    if U != NULL then 
 *       U and V are computed. 
 *       if flag is set to "S" then Min(m,n) columns 
 *       are computed for U and V (rows of Vt) else 
 *      U and V are fully computed 
 *    rank is computed if non null (using tol if tol != NULL)
 */

static int int_svd( Stack stack, int rhs, int opt, int lhs)
{
  char *mode = NULL,cmode ;
  double tol = 0, *Tol=NULL;
  NspMatrix *A;
  NspMatrix *S=NULL, *U=NULL,*V=NULL, *rank=NULL;
  NspMatrix **hrank=NULL,**hU=NULL;
  int_types T[] = {mat,new_opts,t_end} ;
  nsp_option opts[] ={{ "tol",s_double,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&tol,&mode) == FAIL) return RET_BUG;
  
  Tol = ( opts[0].obj == NULLOBJ) ? NULL : &tol; 
  cmode = ( opts[1].obj == NULLOBJ) ? '\0' : mode[0]; 
  if ( cmode != '\0' && cmode != 'S' ) 
    {
      Scierror("%s: mode should be '' or  'S' \n",stack.fname);
      return RET_BUG;
    }
  CheckLhs(1,4);
  if ( lhs >= 2) { hU= &U;}
  if ( lhs >= 4) { hrank= &rank;}
  if ( nsp_svd(A,&S,hU,&V,cmode,hrank,Tol)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(U));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(V));
  if ( lhs >= 4 ) MoveObj(stack,4,NSP_OBJECT(rank));
  /* XXX clean V if not requested and computed */
  return Max(lhs,1);
}



static int int_det( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *res,*A;
  char *mode = NULL,cmode ;
  int_types T[] = {mat,new_opts,t_end} ;
  nsp_option opts[] ={{ "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&mode) == FAIL) return RET_BUG;
  cmode = ( opts[0].obj == NULLOBJ) ? 'd' : mode[0]; 
  if ( cmode != '\0' && cmode != 'd' ) 
    {
      Scierror("%s: mode should be '' or  'd' \n",stack.fname);
      return RET_BUG;
    }
  if (( res = nsp_det(A,cmode))== NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(res));
  return Max(lhs,1);
}


static int int_spec( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A,*d,*v;
  NspMatrix **hv=NULL;
  int_types T[] = {mat,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,2);
  if (lhs == 2) hv = &v;
  if ( nsp_spec(A,&d,hv)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(d));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(v));
  return Max(lhs,1);
}

static int int_inv( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_inv(A)== FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}


static int int_rcond( Stack stack, int rhs, int opt, int lhs)
{
  double rcond;
  NspMatrix *A;
  int_types T[] = {mat,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_rcond(A,&rcond)== FAIL) return RET_BUG;
  if ( nsp_move_double(stack,1,(double)rcond )== FAIL) return RET_BUG;
  return Max(lhs,1);
}


static int int_cholewsky( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_cholewsky(A)== FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}


static int int_lu( Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A;
  NspMatrix *L=NULL, *U=NULL, *E=NULL;
  NspMatrix **hE=NULL;
  int_types T[] = {mat,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(1,3);
  if ( lhs >= 3) { hE= &E;}
  if ( nsp_lu(A,&L,&U,hE)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(U));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(E));
  return Max(lhs,1);
}

/*
 * The Interface for basic matrices operation 
 */

static OpTab Lapack_func[] = {
  {"qr",int_qr},
  {"lsq",int_lsq},
  {"dlamch",int_dlamch},
  {"svd",int_svd},
  {"det",int_det},
  {"spec",int_spec},
  {"inv",int_inv},
  {"cholewsky",int_cholewsky},
  {"rcond",int_rcond},
  {"lu",int_lu},
  {(char *) 0, NULL}
};

int
Lapack_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Lapack_func[i].fonc)) (stack, rhs, opt, lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) **/

void
Lapack_Interf_Info (int i, char **fname, function (**f))
{
  *fname = Lapack_func[i].name;
  *f = Lapack_func[i].fonc;
}

