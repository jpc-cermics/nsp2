#include "cdf.h"

/**
 * cdf_dinvnr:
 * @p: The probability whose normal deviate is sought. 
 * @q: 1-P
 * 
 * returns x  such that cumnor(x)  =   p,  i.e., the  integral from 
 * - infinity to x of (1/sqrt(2*pi)) exp(-u*u/2) du is p 
 * note that if p or q .lt. machine eps returns +/- dinvnr(eps) 
 * 
 *     The  rational   function   on  page 95    of Kennedy  and  Gentle, 
 *     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start 
 *     value for the Newton method of finding roots. 
 * 
 * Returns: a double 
 **/

double cdf_dinvnr (double *p, double *q)
{
  const int maxit=100;
  const double eps=1.0E-13, r2pi=0.3989422804014326E0, nhalf=-0.5E0;
  double ccum, xcur, cum, d1, strtx, dx, pp;
  int i1, i,  qporq;

  /* add the particular case p=q=0.5 to get exactly 0 */
  if ( *p == 0.5 || *q == 0.5 )
    return 0.0;

  /*     FIND MINIMUM OF P AND Q */
  qporq = *p <= *q;
  pp = (!qporq) ? *q : *p;
  /*     initialization step */
  strtx = cdf_stvaln (&pp);
  xcur = strtx;
  /*     newton iterations */
  i1 = maxit;
  for (i = 1; i <= i1; ++i)
    {
      cdf_cumnor (&xcur, &cum, &ccum);
      dx = (cum - pp) / (r2pi * exp (nhalf * xcur * xcur));
      xcur -= dx;
      if ((d1 = dx / xcur, abs (d1)) < eps)
	{
	  /*     if we get here, newton has succeded */
	  return (!qporq) ? - xcur : xcur ;
	}
    }
  /*     if we get here, newton has failed */
  return (!qporq) ?  -strtx : strtx;
}	

