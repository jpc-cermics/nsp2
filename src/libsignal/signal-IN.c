/* Nsp
 * Copyright (C) 2016-2016 Jean-Philippe Chancelier Enpc/Cermics
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
#include <nsp/object.h>
#include <nsp/matrix.h>
#include <nsp/smatrix.h>
#include <nsp/pmatrix.h>
#include <nsp/blas.h>
#include <nsp/nsp_lapack.h>
#include <nsp/interf.h>
#include <nsp/matint.h>

#include "signal.h"

typedef enum 
  {
    low_pass = 1,
    high_pass = 2,
    band_pass = 3,
    stop_band = 4
  } filter_type;

typedef enum 
  {
    butterworth = 1,
    elliptic = 2,
    chebytchev1 = 3,
    chebytchev2 = 4
  } design_type;

static double maximum (double *Val, int m)
{
  int i;
  double max = 0;
  if (m < 1) return max;
  max = Val[0];
  for ( i = 1 ; i < m; i++)
    {
      if ( Val[i] > max ) max = Val[i];
    }
  return max;
}

static double minimum (double *Val, int m)
{
  int i;
  double min = 0;
  if (m < 1) return min;
  min = Val[0];
  for ( i = 1 ; i < m; i++)
    {
      if ( Val[i] < min ) min = Val[i];
    }
  return min;
}

static int is_ascending(double *Val, int m)
{
  int i;
  for (i = 1 ; i < m; i++)
    {
      if ( Val[i-1] > Val[i]) return FALSE;
    }
  return TRUE;
}

NspMatrix *reshapeFilters (const double *InR, const double *InI, int m)
{
  int i,j;
  NspMatrix *loc;
  if (( loc = nsp_matrix_create(NVOID,'c',1, m)) == NULL)
    return NULL;
  for (i = 0, j = 0; j < m ; i++, j++)
    {
      if ( InI[i] == 0)
	{
	  loc->C[j].r = InR[i];	  loc->C[j].i = 0.0;
	}
      else
	{
	  loc->C[j].r = InR[i];	  loc->C[j].i = InI[i];
	  j++;
	  loc->C[j].r = InR[i];	  loc->C[j].i = -InI[i];
	}
    }
  return loc;
}

#define OUT_COUNT 18

int int_syredi (Stack stack, int rhs, int opt, int lhs)
{
  int i;
  int iMaxDeg = 64;
  int Type;
  int Appro;
  NspMatrix *CutOff = NULL;
  double DeltaP = 0;
  double DeltaS = 0;
  NspMatrix *Out[OUT_COUNT];
  int OutSize[OUT_COUNT] =
    { 32, 32, 32, 32, 32, 64, 64, 64, 64, 64, 64, 64, 64, 256, 256, 16, 64, 64 };
  int iErr = 0;
  int iZeroCount = 0;
  int iDegCount = 0;
  double Fact = 0;
  
  CheckRhs(5,5);
  CheckLhs(0,8);

  for (i = 0; i < OUT_COUNT; i++) Out[i] = NULL;
  
  if (GetScalarInt (stack, 1, &Type) == FAIL) return RET_BUG;
  if (GetScalarInt (stack, 2, &Appro) == FAIL) return RET_BUG;
  /* cuttof frequencies ( 4-row vector )*/
  if ((CutOff = GetRealMatCopy (stack, 3)) == NULLMAT) return RET_BUG;
  if ( CutOff->mn != 4 )
    {
      Scierror ("Error: third argument should be of size 4\n");
      return RET_BUG;
    }
  if ( minimum (CutOff->R,CutOff->mn) < 0 || maximum (CutOff->R,CutOff->mn) > M_PI)
    {
      Scierror ("Error: values in third arguments must be in the interval [0,%%pi]\n");
      return RET_BUG;
    }
  if ((Type == low_pass || Type == high_pass)
      && is_ascending (CutOff->R, 2) == FALSE)
    {
      Scierror ("Error: CutOff elements must be in increasing order.\n");
      return RET_BUG;
    }
  if ((Type == band_pass || Type == stop_band)
      && is_ascending (CutOff->R, 4) == FALSE)
    {
      Scierror ("Error: CutOff elements must be in increasing order.\n");
      return RET_BUG;
    }
  /*  ripple in passband ( 0 < deltap < 1 ) */
  if (GetScalarDouble (stack, 4, &DeltaP) == FAIL) return RET_BUG;
  
  /*  ripple in stopband ( 0 < deltas < 1 )*/
  if (GetScalarDouble (stack, 5, &DeltaS) == FAIL) return RET_BUG;
  
  /* alloc temporary variables */
  for (i = 0; i < OUT_COUNT; i++)
    {
      if (( Out[i] = nsp_matrix_create(NVOID,'r',1, OutSize[i]))== NULL) goto err;
    }
  
  signal_syredi (&iMaxDeg, &Type, &Appro, CutOff->R, &DeltaP, &DeltaS, &iZeroCount, &iDegCount, &Fact,
		 Out[ 0]->R, Out[ 1]->R, Out[ 2]->R,
		 Out[ 3]->R, Out[ 4]->R, Out[ 5]->R,
		 Out[ 6]->R, Out[ 7]->R, Out[ 8]->R,
		 &iErr,
		 Out[ 9]->R, Out[10]->R, Out[11]->R,
		 Out[12]->R, Out[13]->R, Out[14]->R,
		 Out[15]->R, Out[16]->I, Out[17]->I);
  if (iErr)
    {
      switch ( iErr)
	{
	case -7: Scierror ("Error: specs => invalid order filter.\n"); goto err;
	case -9: Scierror ("Error: specs => too high order filter.\n");goto err;
	default: Scierror ("Error: error in function syredi.\n");goto err;
	}
    }
  
  if ((nsp_move_double(stack,1,Fact))== FAIL) goto err;
  
  for ( i = 0 ; i < 5 ; i++)
    {
      if ( lhs >= i+2 )
	{
	  if ( nsp_matrix_resize(Out[i], 1 , iDegCount) == FAIL) goto err;
	  MoveObj(stack,i+2,NSP_OBJECT(Out[i]));
	}
      else
	{
	  nsp_matrix_destroy(Out[i]);
	}
    }
  if ( lhs >= 7 )
    {
      /*  zeros */
      NspMatrix *Res = reshapeFilters (Out[5]->R, Out[6]->R, iZeroCount);
      if ( Res == NULL) goto err;
      MoveObj(stack,7,NSP_OBJECT(Res));
    }
  if ( lhs >= 8 )
    {
      /* poles */
      NspMatrix *Res = reshapeFilters (Out[7]->R, Out[8]->R, iZeroCount);
      if ( Res == NULL) goto err;
      MoveObj(stack,8,NSP_OBJECT(Res));
    }

  /* free temporary variables */
  for (i = Max(lhs,0) -1 ; i < OUT_COUNT; i++)
    {
      nsp_matrix_destroy(Out[i]);
    }

  return Max(lhs,1);
 err:
  for (i = 0 ; i < OUT_COUNT; i++)
    {
      if ( Out[i] != NULL) nsp_matrix_destroy(Out[i]);
    }
  return RET_BUG;
}

int int_delip (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *Out=NULL;
  double ck;
  int complex = FALSE, i;
  //check input parameters
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((A = GetRealMatCopy (stack,1)) == NULLMAT) return RET_BUG;
  for ( i = 0; i < A->mn; i++)
    {
      if ( A->R[i] < 0)
	{
	  Scierror ("Error: first argument should be non negative \n");
	  return RET_BUG;
	}
      else if ( A->R[i] > 1)
	{
	  complex = TRUE;
	}
    }
  if (GetScalarDouble (stack, 2, &ck) == FAIL) return RET_BUG;
  if ( ck < -1 || ck > 1)
    {
      Scierror ("Error: second argument should be in [-1,1]\n");
      return RET_BUG;
    }
  if (( Out = nsp_matrix_create(NVOID,'c', A->m,A->n)) ==  NULLMAT) return RET_BUG;
  signal_delip(&A->mn, Out->R, Out->R+Out->mn, A->R, &ck);
  if ( complex == FALSE)
    {
      Out->rc_type = 'r';
    }
  else
    {
      nsp_double2complex(Out->R,Out->mn);
    }
  MoveObj(stack,1,NSP_OBJECT(Out));
  return Max(lhs,1);
}

int int_amell (Stack stack, int rhs, int opt, int lhs)
{
  double dk;
  NspMatrix *U = NULL, *Out = NULL;
  CheckStdRhs (2, 2);
  CheckLhs (1, 5);
  if ((U = GetRealMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarDouble (stack, 2, &dk) == FAIL)
    return RET_BUG;
  if ((Out = nsp_matrix_create (NVOID, 'r', U->m, U->n)) == NULLMAT)
    return RET_BUG;
  signal_amell(U->R, &dk, Out->R, &Out->mn);
  MoveObj (stack, 1, NSP_OBJECT (Out));
  return Max (lhs, 1);
}

int int_remez (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *fg, *ds, *wt,*iext, *Work=NULL;
  int nc;
  CheckRhs (4, 4);
  CheckLhs (0, 1);

  if ((iext = GetRealMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  nc = iext->mn - 2;
      
  iext = Mat2int (iext);
  if ((fg = GetRealMatCopy (stack, 2)) == NULLMAT)
    return RET_BUG;
  if ((ds = GetRealMatCopy (stack, 3)) == NULLMAT)
    return RET_BUG;
  CheckSameDims (NspFname (stack), 2, 3, fg, ds);
  if ((wt = GetRealMatCopy (stack, 4)) == NULLMAT)
    return RET_BUG;
  CheckSameDims (NspFname (stack), 2, 4, fg, wt);
  if ((Work = nsp_matrix_create (NVOID, 'r', nc + 2, 7)) == NULLMAT)
    return RET_BUG;
  signal_remez (&fg->mn, &nc, iext->I,
		Work->R + Work->m, Work->R + Work->m * 2,
		Work->R + Work->m * 3, fg->R, ds->R, wt->R,
		Work->R + Work->m * 4, Work->R + Work->m * 5,
		Work->R + Work->m * 6, Work->R);
  nsp_matrix_resize (Work, 1, nc + 1);
  MoveObj (stack, 1, NSP_OBJECT (Work));
  return Max (lhs, 1);
}

/* [cov,mean]=corr('fft',xmacro,[ymacro],n,sect) */

int int_corr_fft (Stack stack, int rhs, int opt, int lhs)
{
#if 0
  int iErr = 0, iSect = 0, iOutSize = 0, iTotalSize = 0, iSize = 0, iMode = 0;
  double *xa = NULL, *xi = NULL, *xr = NULL, *zr = NULL, *zi = NULL;
  bool bOK = false;
  
  CheckRhs(4,5);
  //get parameter sect
  int
    iPos = (int) (in.size () - 1);
  if (in[iPos]->isDouble () == false
      || in[iPos]->getAs < types::Double > ()->isScalar () == false)
    {
      Scierror (999,
		_
		("%s: Wrong type for input argument #%d: A scalar expected.\n"),
		"corr", iPos + 1);
      return RET_BUG;
    }

  iOutSize = (int) in[iPos]->getAs < types::Double > ()->get (0);
  iSect = iOutSize * 2;
	  
  //get parameter n
  iPos--;
  if (in[iPos]->isDouble () == false
      || in[iPos]->getAs < types::Double > ()->isScalar () == false)
    {
      Scierror (999,
		_
		("%s: Wrong type for input argument #%d: A scalar expected.\n"),
		"corr", iPos + 1);
      return RET_BUG;
    }

  iTotalSize = (int) in[iPos]->getAs < types::Double > ()->get (0);

  Signalprocessingfunctions *
    spFunctionsManager = new Signalprocessingfunctions (L"corr");
  Signalprocessing::addSignalprocessingfunctions (spFunctionsManager);

  //get xmacro
  if (in[1]->isCallable ())
    {
      pXFunction = in[1]->getAs < types::Callable > ();
      spFunctionsManager->setDgetx (in[1]->getAs < types::Callable >
				    ());
    }
  else if (in[1]->isString ())
    {
      pXFunction = in[1]->getAs < types::String > ();
      spFunctionsManager->setDgetx (in[1]->getAs < types::String >
				    ());
    }
  else
    {
      Scierror (999,
		_
		("%s: Wrong type for input argument #%d: A scalar expected.\n"),
		"corr", iPos + 1);
      return RET_BUG;
    }

  iMode = 2;

  if (in.size () == 5)
    {
      //get ymacro
      if (in[2]->isCallable ())
	{
	  pYFunction = in[2]->getAs < types::Callable > ();
	  spFunctionsManager->setDgety (in[2]->getAs <
					types::Callable > ());
	}
      else if (in[2]->isString ())
	{
	  pYFunction = in[2]->getAs < types::String > ();
	  spFunctionsManager->setDgety (in[2]->getAs < types::String >
					());
	}
      else
	{
	  Scierror (999,
		    _
		    ("%s: Wrong type for input argument #%d: A scalar expected.\n"),
		    "corr", iPos + 2);
	  return RET_BUG;
	}

      iMode = 3;
    }

  xa = new double[iSect];
  xr = new double[iSect];
  xi = new double[iSect];
  zr = new double[iSect / 2 + 1];
  zi = new double[iSect / 2 + 1];
  C2F (cmpse2) (&iSect, &iTotalSize, &iMode, (void *) dgetx_f,
		(void *) dgety_f, xa, xr, xi, zr, zi, &iErr);

  delete[]xi;
  delete[]zr;
  delete[]zi;

  if (iErr > 0)
    {
      delete[]xa;
      delete[]xr;
      Scierror (999, _("fft call : needs power of two!"));
      return RET_BUG;
    }

  types::Double * pDblOut1 = new types::Double (1, iOutSize);
  pDblOut1->set (xa);
  delete[]xa;
  out.push_back (pDblOut1);

  types::Double * pDblOut2 = new types::Double (1, iMode - 1);
  pDblOut2->set (xr);
  delete[]xr;
  out.push_back (pDblOut2);
#endif
  
  return Max(lhs,1);

}

int int_corr_updt(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *X = NULL, *Y = NULL, *Xu = NULL, *W = NULL;
  int err = 0, ichaud = 0, iMode = 0, nbx=0;
  
  if ((X = GetRealMatCopy (stack, 2)) == NULLMAT) return RET_BUG;
  if ((Y = GetMatCopy (stack, 3)) == NULLMAT) return RET_BUG;
  if (X->mn == Y->mn) iMode = 1;
  
  if (iMode == 0)
    {
      /* auto-correlation [w,xu]=corr('updt',x1,w0 [,xu]) */
      W= Y;
      /* Y not given, we create the matrix */
      if (( Y = nsp_matrix_create(NVOID,'c', 1, X->mn)) == NULLMAT) return RET_BUG;
      /* switch w to complex */
      if (nsp_mat_complexify(W,0.0) == FAIL ) return RET_BUG;
      nsp_complex2double(W->R,W->mn);
      if ( rhs == 4)
	{
	  if ((Xu = GetMatCopy (stack, 4)) == NULLMAT) return RET_BUG;
	  if (nsp_mat_complexify(Xu,0.0) == FAIL ) return RET_BUG;
	  nsp_complex2double(Xu->R,Xu->mn);
	  ichaud = 1;
	}
      else
	{
	  if ((Xu = nsp_matrix_create(NVOID,'c', 1, W->mn * 2)) == NULLMAT) return RET_BUG;
	  ichaud = 0;
	}
      nbx = W->mn;
      signal_cmpse3 (&W->mn, &X->mn, &iMode, X->R, Y->R, Xu->R, Xu->R+Xu->mn,
		     W->R, W->R+W->mn, &err, &ichaud, &nbx);
      if (err > 0)
	{
	  Scierror ("fft call : needs power of two!");
	  return RET_BUG;
	}
    }
  else
    {
      /* [w,xu]=corr('updt',x1,y1,w0,xu) */
      if ( Y->rc_type != 'r')
	{
	  Scierror("Error: second argument should be real\n");
	  return RET_BUG;
	}
      if ((W = GetMatCopy (stack, 4)) == NULLMAT) return RET_BUG;
      if (nsp_mat_complexify(W,0.0) == FAIL ) return RET_BUG;
      nsp_complex2double(W->R,W->mn);
      if ( rhs  == 5)
	{
	  if ((Xu = GetMatCopy (stack, 5)) == NULLMAT) return RET_BUG;
	  if (nsp_mat_complexify(Xu,0.0) == FAIL ) return RET_BUG;
	  nsp_complex2double(Xu->R,Xu->mn);
	  ichaud = 1;
	}
      else
	{
	  if ((Xu = nsp_matrix_create(NVOID,'c', 1, W->mn * 2)) == NULLMAT) return RET_BUG;
	}
      nbx = W->mn;
      signal_cmpse3(&W->mn, &X->mn, &iMode, X->R, Y->R, Xu->R, Xu->R+Xu->mn,
		    W->R, W->R+W->mn, &err, &ichaud, &nbx);
      if (err > 0)
	{
	  Scierror ("fft call : needs power of two!");
	  return RET_BUG;
	}
    }
  nsp_double2complex(W->R,W->mn);
  NSP_OBJECT(W)->ret_pos = 1;
  /* MoveObj(stack,1,NSP_OBJECT(W)); */
  if ( lhs >= 2)
    {
      int i;
      NspMatrix *Out2 = NULL;
      int m=  W->mn / 2;
      if ((Out2 = nsp_matrix_create(NVOID,'r', 1, W->mn)) == NULLMAT)
	return RET_BUG;
      for ( i = 0; i < m; i++)        Out2->R[i] = X->R[X->mn - m + i];
      for ( i = m+1 ; i < W->mn; i++) Out2->R[i] = 0.0;
      MoveObj(stack,1,NSP_OBJECT(Out2));
      NSP_OBJECT(Out2)->ret_pos = 2;
    }
  return Max(lhs,1);
}

int int_corr_std (Stack stack, int rhs, int opt, int lhs)
{
  /* [cov,mean]=corr(x,[y],nlags) */
  int err = 0, nlags = 0;
  NspMatrix *X=NULL, *Y=NULL, *Correlation=NULL;
  double mean[2];
  CheckRhs(2,3);
  CheckLhs(0,2);
  if ((X = GetRealMat (stack, 1)) == NULLMAT) return RET_BUG;
  if ( rhs == 3 )
    {
      if ((Y = GetRealMat (stack, 2)) == NULLMAT) return RET_BUG;
      CheckSameDims (NspFname (stack), 1, 2, X, Y);
      if (GetScalarInt (stack, 3, &nlags) == FAIL) return RET_BUG;
    }
  else
    {
      if (GetScalarInt (stack, 2, &nlags) == FAIL) return RET_BUG;
    }
  if (( Correlation = nsp_matrix_create(NVOID,'r',1,nlags)) == NULL)
    return RET_BUG;
  if ( X->mn == 0)
    {
      MoveObj(stack,1,NSP_OBJECT(X));
      return Max(lhs,1);
    }
  
  if ( nlags <= 0 || nlags > X->mn ) 
    {
      Scierror("Error: nlags should be in [%d,%d]\n",0,X->mn);
      return RET_BUG;
    }
  
  signal_tscccf(X->R, (rhs==3) ? Y->R: X->R, &X->mn,
		Correlation->R, mean, &nlags,&err);
  if (err == -1)
    {
      Scierror("Error: should not be there !\n");
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Correlation));
  if ( lhs >= 2)
    {
      NspMatrix *Res;
      int n= ( rhs == 3)? 2:1, i;
      if ((Res = nsp_matrix_create(NVOID,'r',1,n)) == NULL)
	return RET_BUG;
      for (i = 0 ; i < n ; i++) Res->R[i]= mean[i];
      MoveObj(stack,2,NSP_OBJECT(Res));
    }
  return Max(lhs,1);
}

int int_corr (Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(2,5);
  if ( IsSMatObj(stack,1)) 
    {
      char *mode;
      if ((mode = GetString(stack,1)) == NULLSTRING) return RET_BUG; 
      if ( strcmp(mode,"fft") == 0 )
	{
	  return int_corr_fft (stack, rhs, opt, lhs);
	}
      else if ( strcmp(mode,"updt") == 0 )
	{
	  return int_corr_updt (stack, rhs, opt, lhs);
	}
      else
	{
	  Scierror("Error: first argument should be \"corr\" or \"fft\" or \"updt\"\n");
	  return RET_BUG;
	}
    }
  else
    {
      return int_corr_std (stack, rhs, opt, lhs);
    }
}

int int_simp (Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *N,*D;
  NspMatrix *Work;
  int Nd=0, Dd=0, err= 0, Work_size=0, i;
  
  CheckRhs(2,2);
  CheckLhs(0,2);

  if ((N = GetPMatCopy(stack,1))== NULL) return RET_BUG;
  if ((D = GetPMatCopy(stack,2))== NULL) return RET_BUG;
  /* 
  if ( ! ( (N->mn == 1 && D->mn >0) || (N->mn > 0 && D->mn == 1) || N->mn == D->mn))
    {
      Scierror("Error: arguments should have compatible sizes\n");
      return RET_BUG;
    }
  */
  CheckSameDims (NspFname (stack), 1, 2, N, D);
  if ( N->mn == 0 )
    {
      goto ret;
    }
  
  for ( i = 0; i < N->mn; i++)
    {
      int nd = Max(0,((NspMatrix *) N->S[i])->mn -1);
      int dd = Max(0,((NspMatrix *) D->S[i])->mn -1);
      Nd = Max(Nd, nd);
      Dd = Max(Dd, dd);
    }
  int iMax = Max(Nd, Dd) + 1;
  Work_size = 2 * (Nd + Dd) + Min(Nd, Dd) + 10 * iMax + 3 * iMax * iMax + 4 +1;
    
  if (( Work = nsp_matrix_create(NVOID,'r', 1, Work_size)) == NULLMAT) return RET_BUG;
  
  for ( i = 0; i < Max(N->mn,D->mn); i++)
    {
      int Nout=0, Dout=0;
      NspMatrix *Nm= (NspMatrix *) N->S[i];
      NspMatrix *Dm= (NspMatrix *) D->S[i];
      if ( Nm->mn != 0 && Nm->rc_type == 'r' && Dm->rc_type == 'r')
	{
	  int nd = Max(0, Nm->mn -1);
	  int dd = Max(0, Dm->mn -1);
	  err = Work_size;
	  signal_dpsimp(Nm->R, &nd, Dm->R, &dd, Nm->R, &Nout, Dm->R , &Dout,Work->R, &err);
	  if (err)
	    {
	      Scierror("Error: work size too small in sim\n");
	      return RET_BUG;
	    }
	  nsp_matrix_resize (Nm, 1, Nout);
	  nsp_matrix_resize (Dm, 1, Dout);
	}
    }
 ret: 
  NSP_OBJECT(N)->ret_pos=1;
  if ( lhs >= 2)
    {
      NSP_OBJECT(D)->ret_pos=2;
    }
  return Max(lhs,1);
}

int int_conv2 (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Res;
  char *mode = "full";
  int iopt = 1, rep,mRes = 0, nRes = 0, edgM = 0, edgN = 0;
  const char *modes_s[]={"full", "same", "valid",NULL};
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs (2, 3);
  CheckLhs(0,1);

  if ( get_optional_args(stack, rhs, opt, opts, &mode) == FAIL )
    return RET_BUG;
  if ( (rep= is_string_in_array(mode,modes_s,0)) == -1 )
    {
      string_not_in_array(stack, mode, modes_s, "optional argument mode");
      return RET_BUG;
    }
  else
    {
      iopt = rep +1;
    }
  
  if (rhs -opt == 3)
    {
      NspMatrix *C,*R,*A,*T;
      if ((C = GetMatCopy (stack, 1)) == NULLMAT) return RET_BUG;
      if ((R = GetMatCopy (stack, 2)) == NULLMAT) return RET_BUG;
      if ((A = GetMatCopy (stack, 3)) == NULLMAT) return RET_BUG;
      switch (iopt)
	{
	case 1 : 
	  mRes = (C->mn == 0 || R->mn == 0) ?  A->m : A->m + C->mn - 1;
	  nRes = (C->mn == 0 || R->mn == 0) ?  A->n : A->n + C->mn - 1;
	  edgM = C->mn - 1;
	  edgN = R->mn - 1;
	  break;
	case 2:
	  mRes = A->m;
	  nRes = A->n;
	  edgM = (C->mn - 1) / 2;
	  edgN = (R->mn - 1) / 2;
	  break;
	case 3:
	  mRes = (C->mn == 0 || R->mn == 0) ? A->m : Max (0, A->m - C->mn + 1);
	  nRes = (C->mn == 0 || R->mn == 0) ? A->n : Max (0, A->n - C->mn + 1);
	  edgM = edgN = 0;
	}
	
      if ( R->rc_type == 'r' && C->rc_type == 'r' && A->rc_type == 'r') 
	{
	  if ((Res = nsp_matrix_create(NVOID,'r', mRes, nRes))== NULL) return RET_BUG;
	  if ((T = nsp_matrix_create(NVOID,'r', 1, A->n ))== NULL) return RET_BUG;
	  signal_conv2_separable_R (R->R, R->mn, C->R, C->mn, A->R, A->m, A->n, Res->R, Res->m,
				    Res->n, edgM, edgN, T->R);
	}
      else
	{
	  if ((Res = nsp_matrix_create(NVOID,'c', mRes, nRes))== NULL) return RET_BUG;
	  if ((T = nsp_matrix_create(NVOID,'c', 1, A->n ))== NULL) return RET_BUG;
	  nsp_complex2double(A->R,A->mn);
	  nsp_complex2double(C->R,C->mn);
	  nsp_complex2double(R->R,R->mn);
	  signal_conv2_separable_C (R->R, R->R+R->mn, R->mn, C->R, C->R+C->mn , C->mn, A->R, A->R+A->mn, A->m, A->n,
				    Res->R, Res->R+Res->mn, Res->m, Res->n, edgM, edgN, T->R, T->R+T->mn);
	  nsp_double2complex(Res->R,Res->mn);
	  nsp_matrix_destroy(T);
	}
    }
  else
    {
      NspMatrix *A,*B;
      if ((A = GetMatCopy (stack, 1)) == NULLMAT) return RET_BUG;
      if ((B = GetMatCopy (stack, 2)) == NULLMAT) return RET_BUG;
      switch ( iopt )
	{
	case 1:
	  mRes = (A->m == 0) ? B->m : ((B->m == 0) ? A->m : A->m + Max(0,B->m - 1));
	  nRes = (A->n == 0) ? B->n : ((B->n == 0) ? A->n : A->n + Max(0,B->n - 1));
	  edgM = B->m - 1;  edgN = B->n - 1;
	  break;
	case 2:
	  mRes = A->m;
	  nRes = A->n;
	  edgM = (B->m - 1) / 2;
	  edgN = (B->n - 1) / 2;
	  break;
	case 3:
	  mRes = (B->m == 0) ? A->m : Max (0, A->m - B->m + 1);
	  nRes = (B->n == 0) ? A->n : Max (0, A->n - B->n + 1);
	  edgM = edgN = 0;
	}
      if (A->rc_type == 'r' && B->rc_type == 'r') 
	{
	  if ((Res = nsp_matrix_create(NVOID,'r', mRes, nRes))== NULL) return RET_BUG;
	  signal_conv2_R (A->R, A->m, A->n, B->R, B->m, B->n , Res->R, Res->m, Res->n, edgM, edgN);
	}
      else
	{
	  if (nsp_mat_complexify(A,0.0) == FAIL ) return RET_BUG;
	  if (nsp_mat_complexify(B,0.0) == FAIL ) return RET_BUG;
	  nsp_complex2double(A->R,A->mn);
	  nsp_complex2double(B->R,B->mn);
	  if ((Res = nsp_matrix_create(NVOID,'c', mRes, nRes))== NULL) return RET_BUG;
	  signal_conv2_C (A->R, A->R+A->mn , A->m, A->n, B->R, B->R+B->mn,  B->m, B->n ,
			  Res->R, Res->R+Res->mn, Res->m, Res->n,  edgM, edgN);
	  nsp_double2complex(Res->R,Res->mn);
	}
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return Max(lhs,1);
}

int int_sfact (Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *N;
  NspMatrix *Work;
  int degree,deg2, max_iteration= 100, err= 0, one= 1, i;

  CheckRhs(1,1);
  CheckLhs(0,1);

  if ((N = GetPMatCopy(stack,1))== NULL) return RET_BUG;
        
  if ( N->mn == 1 ) 
    {
      /* one polynomial */
      NspMatrix *Nmat = (NspMatrix *) N->S[0];
      if (Nmat->rc_type == 'c' )
	{
	  Scierror("Error: polynom should be real\n");
	  return RET_BUG;
	}
      degree = Max( Nmat->mn - 1,0);
      deg2 = degree/2;
      if (2 * deg2 != degree)
	{
	  Scierror("Error: A symmetric polynom is expected.\n");return RET_BUG;
	}
      for ( i = 0; i < Nmat->mn; i++)
	{
	  if ( Nmat->R[i] != Nmat->R[degree - i])
	    {
	      Scierror("Error: A symmetric polynom is expected.\n");return RET_BUG;
	    }
	}
      if (( Work = nsp_matrix_create(NVOID,'r',1, 7 * Nmat->mn))== NULL) return RET_BUG;
      signal_sfact1( Nmat->R, &deg2, Work->R, &max_iteration, &err);
      nsp_matrix_destroy(Work);
      nsp_matrix_resize(Nmat,1,deg2+1);
      if ( err < 0)
	{
	  Sciprintf("warning: Convergence at 10^%d near.\n",err);
	}
      switch ( err )
	{
	case 2 :
	  Scierror("Error: Non negative value expected at degree %d.\n", degree);
	  return RET_BUG;
	case 1 :
	  Scierror("Error: Convergence problem.\n");
	  return RET_BUG;
	default :
	  break;
	}
      NSP_OBJECT(N)->ret_pos=1;
    }
  else
    {
      NspMatrix *Out;
      NspPMatrix *Pout;
      int i, max_mn=0,  max_degree ,max_dege2,n;
      CheckSquare(NspFname(stack),1,N);
      for (i=0 ; i < N->mn ; i++)
	{
	  max_mn = Max(max_mn, ((NspMatrix *) N->S[i])->mn);
	}
      max_degree = max_mn-1;
      max_dege2  = max_degree/2;
      n = max_dege2 + 1;
      
      if ( ( Out = nsp_matrix_create(NVOID,'r',1, N->mn*n)) == NULL) return RET_BUG;
      memset(Out->R, 0x00, N->mn * n * sizeof(double));
	
      if ( ( Work = nsp_matrix_create(NVOID,'r',1,(n + 1) * N->m * ((n + 1)*N->m) + 1 ))== NULL) return RET_BUG;
      for ( i = 0; i < N->mn; i++)
	{
	  NspMatrix *Elt = (NspMatrix *)  N->S[i];
	  int nc = 2 + Elt->mn - 1 - n;
	  if ( nc > 0)
	    {
	      C2F(dcopy)(&nc, Elt->R + n - 1, &one, Out->R + i, &N->mn);
	    }
	}
      int nm1 = n - 1;
      max_iteration += n;
      signal_sfact2(Out->R, &N->m, &nm1, Work->R, &max_iteration, &err);
      if (err < 0)
	{
	  Scierror("Error: convergence problem.\n");
	  return RET_BUG;
	}
      else if (err > 0)
	{
	  Scierror("Erro: singular or asymmetric problem.\n");
	  return RET_BUG;
	}
      if ((Pout =nsp_pmatrix_create(NVOID,N->m,N->n,NULL,-1, N->var))== NULLPMAT)
	return RET_BUG;
	
      for ( i = 0; i < N->mn ; i++)
	{
	  NspMatrix *Mout;
	  if (( Mout = nsp_matrix_create("pe",'r',1,n))==NULL) return RET_BUG;
	  Pout->S[i] = Mout;
	  C2F(dcopy)(&n, Out->R + i, &N->mn, Mout->R, &one);
	}
      nsp_matrix_destroy(Work);
      nsp_matrix_destroy(Out);
      MoveObj(stack,1,NSP_OBJECT(Pout));
    }
  return Max(lhs,1);
}


