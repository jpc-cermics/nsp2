/* corth.f -- translated by f2c (version 19961017). */

/* 
 *    given a complex general matrix, this subroutine 
 *    reduces a submatrix situated in rows and columns 
 *    low through igh to upper hessenberg form by 
 *    unitary similarity transformations. 
 *
 *    subroutine corth(nm,n,low,igh,ar,ai,ortr,orti) 
 * 
 *    int i,j,m,n,ii,jj,la,mp,nm,igh,kp1,low 
 *    double precision ar(nm,n),ai(nm,n),ortr(igh),orti(igh) 
 *    double precision f,g,h,fi,fr,scale 
 * 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrix; 
 *       low and igh are ints determined by the balancing 
 *         subroutine  cbal.  if  cbal  has not been used, 
 *         set low=1, igh=n; 
 *       ar and ai contain the real and imaginary parts, 
 *         respectively, of the complex input matrix. 
 *    on output: 
 *       ar and ai contain the real and imaginary parts, 
 *         respectively, of the hessenberg matrix.  information 
 *         about the unitary transformations used in the reduction 
 *         is stored in the remaining triangles under the 
 *         hessenberg matrix; 
 *       ortr and orti contain further information about the 
 *         transformations.  only elements low through igh are used. 
 * 
 *    this subroutine is a translation of a complex analogue of 
 *    the algol procedure orthes, num. math. 12, 349-368(1968) 
 *    by martin and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_corth (int *nm, int *n, int *low, int *igh, double *ar,
		    double *ai, double *ortr, double *orti)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  double f, g, h__;
  int i__, j, m;
  double scale;
  int la;
  double fi;
  int ii, jj;
  double fr;
  int mp, kp1;

  /* Parameter adjustments */
  ai_dim1 = *nm;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *nm;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  --orti;
  --ortr;

  /* Function Body */
  la = *igh - 1;
  kp1 = *low + 1;
  if (la < kp1)
    {
      goto L200;
    }
  /* 
   */
  i__1 = la;
  for (m = kp1; m <= i__1; ++m)
    {
      h__ = 0.;
      ortr[m] = 0.;
      orti[m] = 0.;
      scale = 0.;
      /*    :::::::::: scale column (algol tol then not needed) :::::::::: 
       */
      i__2 = *igh;
      for (i__ = m; i__ <= i__2; ++i__)
	{
	  /* L90: */
	  scale = scale + (d__1 =
			   ar[i__ + (m - 1) * ar_dim1], Abs (d__1)) + (d__2 =
								       ai[i__
									  +
									  (m -
									   1)
									  *
									  ai_dim1],
								       Abs
								       (d__2));
	}
      /* 
       */
      if (scale == 0.)
	{
	  goto L180;
	}
      mp = m + *igh;
      /*    :::::::::: for i=igh step -1 until m do -- :::::::::: 
       */
      i__2 = *igh;
      for (ii = m; ii <= i__2; ++ii)
	{
	  i__ = mp - ii;
	  ortr[i__] = ar[i__ + (m - 1) * ar_dim1] / scale;
	  orti[i__] = ai[i__ + (m - 1) * ai_dim1] / scale;
	  h__ = h__ + ortr[i__] * ortr[i__] + orti[i__] * orti[i__];
	  /* L100: */
	}
      /* 
       */
      g = sqrt (h__);
      f = sqrt (ortr[m] * ortr[m] + orti[m] * orti[m]);
      if (f == 0.)
	{
	  goto L103;
	}
      h__ += f * g;
      g /= f;
      ortr[m] = (g + 1.) * ortr[m];
      orti[m] = (g + 1.) * orti[m];
      goto L105;
      /* 
       */
    L103:
      ortr[m] = g;
      ar[m + (m - 1) * ar_dim1] = scale;
      /*    :::::::::: form (i-(u*ut)/h) * a :::::::::: 
       */
    L105:
      i__2 = *n;
      for (j = m; j <= i__2; ++j)
	{
	  fr = 0.;
	  fi = 0.;
	  /*    :::::::::: for i=igh step -1 until m do -- :::::::::: 
	   */
	  i__3 = *igh;
	  for (ii = m; ii <= i__3; ++ii)
	    {
	      i__ = mp - ii;
	      fr =
		fr + ortr[i__] * ar[i__ + j * ar_dim1] + orti[i__] * ai[i__ +
									j *
									ai_dim1];
	      fi =
		fi + ortr[i__] * ai[i__ + j * ai_dim1] - orti[i__] * ar[i__ +
									j *
									ar_dim1];
	      /* L110: */
	    }
	  /* 
	   */
	  fr /= h__;
	  fi /= h__;
	  /* 
	   */
	  i__3 = *igh;
	  for (i__ = m; i__ <= i__3; ++i__)
	    {
	      ar[i__ + j * ar_dim1] =
		ar[i__ + j * ar_dim1] - fr * ortr[i__] + fi * orti[i__];
	      ai[i__ + j * ai_dim1] =
		ai[i__ + j * ai_dim1] - fr * orti[i__] - fi * ortr[i__];
	      /* L120: */
	    }
	  /* 
	   */
	  /* L130: */
	}
      /*    :::::::::: form (i-(u*ut)/h)*a*(i-(u*ut)/h) :::::::::: 
       */
      i__2 = *igh;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  fr = 0.;
	  fi = 0.;
	  /*    :::::::::: for j=igh step -1 until m do -- :::::::::: 
	   */
	  i__3 = *igh;
	  for (jj = m; jj <= i__3; ++jj)
	    {
	      j = mp - jj;
	      fr =
		fr + ortr[j] * ar[i__ + j * ar_dim1] - orti[j] * ai[i__ +
								    j *
								    ai_dim1];
	      fi =
		fi + ortr[j] * ai[i__ + j * ai_dim1] + orti[j] * ar[i__ +
								    j *
								    ar_dim1];
	      /* L140: */
	    }
	  /* 
	   */
	  fr /= h__;
	  fi /= h__;
	  /* 
	   */
	  i__3 = *igh;
	  for (j = m; j <= i__3; ++j)
	    {
	      ar[i__ + j * ar_dim1] =
		ar[i__ + j * ar_dim1] - fr * ortr[j] - fi * orti[j];
	      ai[i__ + j * ai_dim1] =
		ai[i__ + j * ai_dim1] + fr * orti[j] - fi * ortr[j];
	      /* L150: */
	    }
	  /* 
	   */
	  /* L160: */
	}
      /* 
       */
      ortr[m] = scale * ortr[m];
      orti[m] = scale * orti[m];
      ar[m + (m - 1) * ar_dim1] = -g * ar[m + (m - 1) * ar_dim1];
      ai[m + (m - 1) * ai_dim1] = -g * ai[m + (m - 1) * ai_dim1];
    L180:
      ;
    }
  /* 
   */
 L200:
  return 0;
  /*    :::::::::: last card of corth :::::::::: 
   */
}				/* corth_ */
