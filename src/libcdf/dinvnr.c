#include "cdf.h"

/* ********************************************************************** */
/*     DOUBLE PRECISION FUNCTION DINVNR(P,Q) */
/*     Double precision NoRmal distribution INVerse */
/*                              Function */
/*     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from - */
/*     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P */
/*                              Arguments */
/*     P --> The probability whose normal deviate is sought. */
/*                    P is DOUBLE PRECISION */
/*     Q --> 1-P */
/*                    P is DOUBLE PRECISION */
/*                              Method */
/*     The  rational   function   on  page 95    of Kennedy  and  Gentle, */
/*     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start */
/*     value for the Newton method of finding roots. */
/*                              Note */
/*     If P or Q .lt. machine EPS returns +/- DINVNR(EPS) */


double cdf_dinvnr (double *p, double *q)
{
  const int maxit=100;
  const double eps=1.0E-13, r2pi=0.3989422804014326E0, nhalf=-0.5E0;
  int i__1;
  double ret_val, d__1;
  double ccum, xcur;
  int i__;
  int qporq;
  double strtx, dx, pp;
  double cum;


/*     FIND MINIMUM OF P AND Q */

  qporq = *p <= *q;
  if (!qporq)
    {
      goto L10;
    }
  pp = *p;
  goto L20;
L10:
  pp = *q;

/*     INITIALIZATION STEP */

L20:
  strtx = cdf_stvaln (&pp);
  xcur = strtx;

/*     NEWTON INTERATIONS */

  i__1 = maxit;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      cdf_cumnor (&xcur, &cum, &ccum);
      dx = (cum - pp) / (r2pi * exp (nhalf * xcur * xcur));
      xcur -= dx;
      if ((d__1 = dx / xcur, Abs (d__1)) < eps)
	{
	  goto L40;
	}
/* L30: */
    }
  ret_val = strtx;

/*     IF WE GET HERE, NEWTON HAS FAILED */

  if (!qporq)
    {
      ret_val = -ret_val;
    }
  return ret_val;

/*     IF WE GET HERE, NEWTON HAS SUCCEDED */

L40:
  ret_val = xcur;
  if (!qporq)
    {
      ret_val = -ret_val;
    }
  return ret_val;
}				/* dinvnr_ */
