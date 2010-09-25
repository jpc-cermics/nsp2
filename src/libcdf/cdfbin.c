#include "cdf.h"

/*********************************************************************** 
 *     SUBROUTINE CDFBIN ( WHICH, P, Q, S, XN, PR, OMPR, STATUS, BOUND ) 
 *               Cumulative Distribution Function 
 *                         BINomial distribution 
 *                              Function 
 *     Calculates any one parameter of the binomial 
 *     distribution given values for the others. 
 *                              Arguments 
 *     WHICH --> Int indicating which of the next four argument 
 *               values is to be calculated from the others. 
 *               Legal range: 1..4 
 *               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR 
 *               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR 
 *               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR 
 *               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN 
 *                    INT WHICH 
 *     P <--> The cumulation from 0 to S of the binomial distribution. 
 *            (Probablility of S or fewer successes in XN trials each 
 *            with probability of success PR.) 
 *            Input range: [0,1]. 
 *                    DOUBLE PRECISION P 
 *     Q <--> 1-P. 
 *            Input range: [0, 1]. 
 *            P + Q = 1.0. 
 *                    DOUBLE PRECISION Q 
 *     S <--> The number of successes observed. 
 *            Input range: [0, XN] extended to [-infinity , + infinity ] for which == 1
 *            Search range: [0, XN] 
 *            
 *                    DOUBLE PRECISION S 
 *     XN  <--> The number of binomial trials. 
 *              Input range: (0, +infinity). 
 *              Search range: [1E-300, 1E300] 
 *                    DOUBLE PRECISION XN 
 *     PR  <--> The probability of success in each binomial trial. 
 *              Input range: [0,1]. 
 *              Search range: [0,1] 
 *                    DOUBLE PRECISION PR 
 *     OMPR  <--> 1-PR 
 *              Input range: [0,1]. 
 *              Search range: [0,1] 
 *              PR + OMPR = 1.0 
 *                    DOUBLE PRECISION OMPR 
 *     STATUS <-- 0 if calculation completed correctly 
 *               -I if input parameter number I is out of range 
 *                1 if answer appears to be lower than lowest 
 *                  search bound 
 *                2 if answer appears to be higher than greatest 
 *                  search bound 
 *                3 if P + Q .ne. 1 
 *                4 if PR + OMPR .ne. 1 
 *                    INT STATUS 
 *     BOUND <-- Undefined if STATUS is 0 
 *               Bound exceeded by parameter number I if STATUS 
 *               is negative. 
 *               Lower search bound if STATUS is 1. 
 *               Upper search bound if STATUS is 2. 
 *                              Method 
 *
 *     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of 
 *     Mathematical   Functions (1966) is   used  to reduce the  binomial 
 *     distribution  to  the  cumulative incomplete    beta distribution. 
 *     Computation of other parameters involve a seach for a value that 
 *     produces  the desired  value  of P.   The search relies  on  the 
 *     monotinicity of P with the other parameter. 
 *     Note that :  
 *     
 *     F_[B(n,p)](k) = 1 - F_[beta(k+1,n-(k+1)+1)](p) 
 *     thus this function compute the cumulative distribution using this 
 *     formula i.e for k in R 
 *     
 ********************************************************************** */


int
cdf_cdfbin (int *which, double *p, double *q, double *s, double *xn,
	    double *pr, double *ompr, int *status, double *bound, double *boundbis)
{
  static int c__1 = 1;
  static double c_b37 = 0.;
  static double c_b38 = .5;
  static double c_b40 = 5.;
  static double c_b59 = 1.;
  const  double atol=1.0E-50, zero=1.0E-300, inf=1.0E300,one=1.0E0,tol=1.0E-14;

  double d__1;
  double ccum;
  int qleft;
  int qporq;
  double fx, pq;
  double prompr;
  int qhi;
  double cum, xhi, xlo;
  /*     jpc : changed the next line which was strangeley */
  /*     not working at run time on win32 gcc with optimize option !!! */
  /*      IF (.NOT ((which.LT.1).AND. (which.GT.4))) GO TO 30 */
  if (*which >= 1 && *which <= 4)
    {
      goto L30;
    }
  if (!(*which < 1))
    {
      goto L10;
    }
  *bound = 1.;
  goto L20;
 L10:
  *bound = 4.;
 L20:
  *status = -1;
  return 0;
 L30:
  if (*which == 1)
    {
      goto L70;
    }
  /*     P */
  if (!(*p < 0. || *p > 1.))
    {
      goto L60;
    }
  if (!(*p < 0.))
    {
      goto L40;
    }
  *bound = 0.;
  goto L50;
 L40:
  *bound = 1.;
 L50:
  *status = -2;
  return 0;
 L60:
 L70:
  if (*which == 1)
    {
      goto L110;
    }
  /*     Q */
  if (!(*q < 0. || *q > 1.))
    {
      goto L100;
    }
  if (!(*q < 0.))
    {
      goto L80;
    }
  *bound = 0.;
  goto L90;
 L80:
  *bound = 1.;
 L90:
  *status = -3;
  return 0;
 L100:
 L110:
  if (*which == 3)
    {
      goto L130;
    }

  /*     XN */

  if (!(*xn <= 0.))
    {
      goto L120;
    }
  *bound = 0.;
  *status = -5;
  return 0;
 L120:
 L130:
  if (*which == 2)
    {
      goto L170;
    }
  /* S : jpc range for *s is extended for *which == 1 */
  if (!(*s < 0. || (*which != 3 && *s > *xn)) || *which == 1)
    {
      goto L160;
    }
  if (!(*s < 0.))
    {
      goto L140;
    }
  *bound = 0.;
  goto L150;
 L140:
  *bound = *xn;
 L150:
  *status = -4;
  return 0;
 L160:
 L170:
  if (*which == 4)
    {
      goto L210;
    }
  /*     PR */
  if (!(*pr < 0. || *pr > 1.))
    {
      goto L200;
    }
  if (!(*pr < 0.))
    {
      goto L180;
    }
  *bound = 0.;
  goto L190;
 L180:
  *bound = 1.;
 L190:
  *status = -6;
  return 0;
 L200:
 L210:
  if (*which == 4)
    {
      goto L250;
    }
  /*     OMPR */
  if (!(*ompr < 0. || *ompr > 1.))
    {
      goto L240;
    }
  if (!(*ompr < 0.))
    {
      goto L220;
    }
  *bound = 0.;
  goto L230;
 L220:
  *bound = 1.;
 L230:
  *status = -7;
  return 0;
 L240:
 L250:
  if (*which == 1)
    {
      goto L290;
    }
  /*     P + Q */
  pq = *p + *q;
  if (!((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L280;
    }
  if (!(pq < 0.))
    {
      goto L260;
    }
  *bound = 0.;
  goto L270;
 L260:
  *bound = 1.;
 L270:
  *status = 3;
  return 0;
 L280:
 L290:
  if (*which == 4)
    {
      goto L330;
    }
  /*     PR + OMPR */
  prompr = *pr + *ompr;
  if (!((d__1 = prompr - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L320;
    }
  if (!(prompr < 0.))
    {
      goto L300;
    }
  *bound = 0.;
  goto L310;
 L300:
  *bound = 1.;
 L310:
  *status = 4;
  return 0;
 L320:
 L330:
  if (!(*which == 1))
    {
      qporq = *p <= *q;
    }
  /*     Select the minimum of P or Q */
  /*     Calculate ANSWERS */
  if (1 == *which)
    {
      /*     Calculating P */
      if ( *s < 0 ) 
	{ 
	  *p=0.0; *q=1.0; *status = 0;
	}
      else if ( *s > *xn ) 
	{ 
	  *p=1.0; *q=0.0; *status = 0;
	}
      else 
	{
	  double sf = floor(*s); /* add floor to compute the real cdfbin (bruno march,22,2010)) */
	  cdf_cumbin (&sf, xn, pr, ompr, p, q);
	  *status = 0;
	}
    }
  else if (2 == *which)
    {
      if ( *p <= pow(*ompr,*xn) )
	{
	  /* jpc fev 2005 */
	  *status = 0; 
	  *s =0.0;
	  return 0;
	}
      /*     Calculating S */
      *s = *pr * *xn;  // start from the mean in place of 5. ! (bruno, april 2010)
      cdf_dstinv (&c_b37, xn, &c_b38, &c_b38, &c_b40, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, s, &fx, &qleft, &qhi);
    L340:
      if (!(*status == 1))
	{
	  goto L370;
	}
      cdf_cumbin (s, xn, pr, ompr, &cum, &ccum);
      if (!qporq)
	{
	  goto L350;
	}
      fx = cum - *p;
      goto L360;
    L350:
      fx = ccum - *q;
    L360:
      cdf_dinvr (status, s, &fx, &qleft, &qhi);
      goto L340;
    L370:
      if (!(*status == -1))
	{
	  goto L400;
	}
      if (!qleft)
	{
	  goto L380;
	}
      *status = 1;
      *bound = 0.;
      goto L390;
    L380:
      *status = 2;
      *bound = *xn;
    L390:
    L400:
      ;
    }
  else if (3 == *which)
    {

      /*     Calculating XN */

      *xn = 5.;
      cdf_dstinv (&zero, &inf, &c_b38, &c_b38, &c_b40, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, xn, &fx, &qleft, &qhi);
    L410:
      if (!(*status == 1))
	{
	  goto L440;
	}
      cdf_cumbin (s, xn, pr, ompr, &cum, &ccum);
      if (!qporq)
	{
	  goto L420;
	}
      fx = cum - *p;
      goto L430;
    L420:
      fx = ccum - *q;
    L430:
      cdf_dinvr (status, xn, &fx, &qleft, &qhi);
      goto L410;
    L440:
      if (!(*status == -1))
	{
	  goto L470;
	}
      if (!qleft)
	{
	  goto L450;
	}
      *status = 1;
      *bound = zero;
      goto L460;
    L450:
      *status = 2;
      *bound = inf;
    L460:
    L470:
      ;
    }
  else if (4 == *which)
    {
      /*     Calculating PR and OMPR */
      cdf_dstzr (&c_b37, &c_b59, &atol, &tol);
      if (!qporq)
	{
	  goto L500;
	}
      *status = 0;
      cdf_dzror (status, pr, &fx, &xlo, &xhi, &qleft, &qhi);
      *ompr = one - *pr;
    L480:
      if (!(*status == 1))
	{
	  goto L490;
	}
      cdf_cumbin (s, xn, pr, ompr, &cum, &ccum);
      fx = cum - *p;
      cdf_dzror (status, pr, &fx, &xlo, &xhi, &qleft, &qhi);
      *ompr = one - *pr;
      goto L480;
    L490:
      goto L530;
    L500:
      *status = 0;
      cdf_dzror (status, ompr, &fx, &xlo, &xhi, &qleft, &qhi);
      *pr = one - *ompr;
    L510:
      if (!(*status == 1))
	{
	  goto L520;
	}
      cdf_cumbin (s, xn, pr, ompr, &cum, &ccum);
      fx = ccum - *q;
      cdf_dzror (status, ompr, &fx, &xlo, &xhi, &qleft, &qhi);
      *pr = one - *ompr;
      goto L510;
    L520:
    L530:
      if (!(*status == -1))
	{
	  goto L560;
	}
      if (!qleft)
	{
	  goto L540;
	}
      *status = 1;
      *bound = 0.;
      goto L550;
    L540:
      *status = 2;
      *bound = 1.;
    L550:
    L560:
      ;
    }
  return 0;
}				/* cdfbin_ */

