#include "nsp/math.h"
#include "fromquadpack.h"

/* Table of constant values */

static double c_b20 = 0.;
static double c_b21 = 1.;
static const double c_b49 = 1.5;

static int nsp_quadpack_dqelg (int *n, double *epstab, double *result,
			       double *abserr, double *res3la, int *nres);
static int nsp_quadpack_dqpsrt (int *limit, int *last, int *maxerr,
				double *ermax, double *elist, int *iord,
				int *nrmax);
static int nsp_quadpack_dqk21 (intg_f fvect, double *a, double *b, double *result,
			       double *abserr, double *resabs, double *resasc,
			       int *vectflag, int *stat);
static int nsp_quadpack_dqk15i (intg_f fvect, double *boun, int *inf, double *a,
				double *b, double *result, double *abserr,
				double *resabs, double *resasc, int *vectflag,
				int *stat);

static int nsp_quadpack_dqk21b (intg_f fvect, double *a, double *c__, double *result1,
				double *abserr1, double *resasc1, double *result2,
				double *abserr2, double *resasc2, int *vectflag,
				int *stat);

#define nsp_quadpack_dqagie nspdqagie_
#define nsp_quadpack_dqagse nspdqagse_ 

/* 
 * this file contains 6 routines from the netlib quapack distribution: 
 * - dqagse: routine for integration on [a,b] 
 * - dqagie: routine for integration on (-oo,a], [a,+oo) or (-oo,+oo) 
 * - dqelg: epsilon algorithm code used both by dqagse and dqagie 
 * - dqpsrt: used both by dqagse and dqagie 
 * - dqk21: base formula used by dqagse 
 * - dqk15i: base formula used by dqagie 
 * 
 * These routines have been slightly modified by Bruno Pincon 
 * for inclusion in nsp, in particular dqk21 and dqk15i 
 * (see comments). Also calls to d1mach have been replaced by 
 * equivalent call to the lapack dlamch and the fortran code 
 * have been cleaned a little. 
 * All names are prefixed by nsp to avoid possible future 
 * name clash. 
 * 
 * 
 */

int 
nsp_quadpack_dqagse (intg_f f, double *a, double *b, double *epsabs,
		     double *epsrel, int *limit, double *result,
		     double *abserr,  int *neval, int *ier, double *alist__, 
		     double *blist,  double *rlist, double *elist, int *iord,
		     int *last, int *vectflag, int *stat)
{
  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double area, dres;
  int ksgn, nres;
  double area1, area2;
  double area12;
  int k;
  double small, erro12;
  int ierro;
  double a1, a2, b1, b2, defab1, defab2, oflow;
  int ktmin, nrmax;
  double uflow;
  int noext;
  int iroff1, iroff2, iroff3;
  double res3la[3], error1, error2;
  int id;
  double rlist2[52];
  int numrl2;
  double defabs;
  double epmach, erlarg, abseps, correc, errbnd, resabs;
  int jupbnd;
  double erlast, errmax;
  int maxerr;
  double reseps;
  int extrap;
  double ertest, errsum;

  /****begin prologue  dqagse 
   ****date written   800101   (yymmdd) 
   ****revision date  830518   (yymmdd) 
   ****category no.  h2a1a1 
   ****keywords  automatic integrator, general-purpose, 
   *            (end point) singularities, extrapolation, 
   *            globally adaptive 
   ****author  piessens,robert,appl. math. & progr. div. - k.u.leuven 
   *          de doncker,elise,appl. math. & progr. div. - k.u.leuven 
   * 
   ****slightly modified by Bruno Pincon for nsp 
   * 
   ****purpose  the routine calculates an approximation result to a given 
   *           definite integral i = integral of f over (a,b), 
   *           hopefully satisfying following claim for accuracy 
   *           Abs(i-result).le.max(epsabs,epsrel*abs(i)). 
   ****description 
   * 
   *       computation of a definite integral 
   *       standard fortran subroutine 
   *       double precision version 
   * 
   *       parameters 
   *        on entry 
   * 
   *           f      - function subprogram defining the integrand 
   *                    function f(x). the actual name for f needs to be 
   *                    declared e x t e r n a l in the driver program. 
   *                    f has the form of a function returning an int: 
   *                     stat = f(x,y,n) 
   *                    which must compute y(k)=f(x(k)) 1<=k<=n 
   *                    the returned value (stat) is used to communicate 
   *                    problems in the evaluation of the function by 
   *                    the nsp interpretor 
   * 
   *           a      - double precision 
   *                    lower limit of integration 
   * 
   *           b      - double precision 
   *                    upper limit of integration 
   * 
   *           epsabs - double precision 
   *                    absolute accuracy requested 
   *           epsrel - double precision 
   *                    relative accuracy requested 
   *                    if  epsabs.le.0 
   *                    and epsrel.lt.max(50*rel.mach.acc.,0.5d-28), 
   *                    the routine will end with ier = 6. 
   * 
   *           limit  - int 
   *                    gives an upperbound on the number of subintervals 
   *                    in the partition of (a,b) 
   * 
   *        on return 
   *           result - double precision 
   *                    approximation to the integral 
   * 
   *           abserr - double precision 
   *                    estimate of the modulus of the absolute error, 
   *                    which should equal or exceed Abs(i-result) 
   * 
   *           neval  - int 
   *                    number of integrand evaluations 
   * 
   *           ier    - int 
   *                    ier = 0 normal and reliable termination of the 
   *                            routine. it is assumed that the requested 
   *                            accuracy has been achieved. 
   *                    ier.gt.0 abnormal termination of the routine 
   *                            the estimates for integral and error are 
   *                            less reliable. it is assumed that the 
   *                            requested accuracy has not been achieved. 
   *           error messages 
   *                        = 1 maximum number of subdivisions allowed 
   *                            has been achieved. one can allow more sub- 
   *                            divisions by increasing the value of limit 
   *                            (and taking the according dimension 
   *                            adjustments into account). however, if 
   *                            this yields no improvement it is advised 
   *                            to analyze the integrand in order to 
   *                            determine the integration difficulties. if 
   *                            the position of a local difficulty can be 
   *                            determined (e.g. singularity, 
   *                            discontinuity within the interval) one 
   *                            will probably gain from splitting up the 
   *                            interval at this point and calling the 
   *                            integrator on the subranges. if possible, 
   *                            an appropriate special-purpose integrator 
   *                            should be used, which is designed for 
   *                            handling the type of difficulty involved. 
   *                        = 2 the occurrence of roundoff error is detec- 
   *                            ted, which prevents the requested 
   *                            tolerance from being achieved. 
   *                            the error may be under-estimated. 
   *                        = 3 extremely bad integrand behaviour 
   *                            occurs at some points of the integration 
   *                            interval. 
   *                        = 4 the algorithm does not converge. 
   *                            roundoff error is detected in the 
   *                            extrapolation table. 
   *                            it is presumed that the requested 
   *                            tolerance cannot be achieved, and that the 
   *                            returned result is the best which can be 
   *                            obtained. 
   *                        = 5 the integral is probably divergent, or 
   *                            slowly convergent. it must be noted that 
   *                            divergence can occur with any other value 
   *                            of ier. 
   *                        = 6 the input is invalid, because 
   *                            epsabs.le.0 and 
   *                            epsrel.lt.max(50*rel.mach.acc.,0.5d-28). 
   *                            result, abserr, neval, last, rlist(1), 
   *                            iord(1) and elist(1) are set to zero. 
   *                            alist(1) and blist(1) are set to a and b 
   *                            respectively. 
   * 
   *           alist  - double precision 
   *                    vector of dimension at least limit, the first 
   *                     last  elements of which are the left end points 
   *                    of the subintervals in the partition of the 
   *                    given integration range (a,b) 
   * 
   *           blist  - double precision 
   *                    vector of dimension at least limit, the first 
   *                     last  elements of which are the right end points 
   *                    of the subintervals in the partition of the given 
   *                    integration range (a,b) 
   * 
   *           rlist  - double precision 
   *                    vector of dimension at least limit, the first 
   *                     last  elements of which are the integral 
   *                    approximations on the subintervals 
   * 
   *           elist  - double precision 
   *                    vector of dimension at least limit, the first 
   *                     last  elements of which are the moduli of the 
   *                    absolute error estimates on the subintervals 
   * 
   *           iord   - int 
   *                    vector of dimension at least limit, the first k 
   *                    elements of which are pointers to the 
   *                    error estimates over the subintervals, 
   *                    such that elist(iord(1)), ..., elist(iord(k)) 
   *                    form a decreasing sequence, with k = last 
   *                    if last.le.(limit/2+2), and k = limit+1-last 
   *                    otherwise 
   * 
   *           last   - int 
   *                    number of subintervals actually produced in the 
   *                    subdivision process 
   * 
   ****references  (none) 
   ****routines called  d1mach,dqelg,dqk21,dqpsrt 
   ****end prologue  dqagse 
   * 
   *   local vars 
   * 
   *           the dimension of rlist2 is determined by the value of 
   *           limexp in subroutine dqelg (rlist2 should be of dimension 
   *           (limexp+2) at least). 
   * 
   *           list of major variables 
   *           ----------------------- 
   * 
   *          alist     - list of left end points of all subintervals 
   *                      considered up to now 
   *          blist     - list of right end points of all subintervals 
   *                      considered up to now 
   *          rlist(i)  - approximation to the integral over 
   *                      (alist(i),blist(i)) 
   *          rlist2    - array of dimension at least limexp+2 containing 
   *                      the part of the epsilon table which is still 
   *                      needed for further computations 
   *          elist(i)  - error estimate applying to rlist(i) 
   *          maxerr    - pointer to the interval with largest error 
   *                      estimate 
   *          errmax    - elist(maxerr) 
   *          erlast    - error on the interval currently subdivided 
   *                      (before that subdivision has taken place) 
   *          area      - sum of the integrals over the subintervals 
   *          errsum    - sum of the errors over the subintervals 
   *          errbnd    - requested accuracy Max(epsabs,epsrel* 
   *                      Abs(result)) 
   *          *****1    - variable for the left interval 
   *          *****2    - variable for the right interval 
   *          last      - index for subdivision 
   *          nres      - number of calls to the extrapolation routine 
   *          numrl2    - number of elements currently in rlist2. if an 
   *                      appropriate approximation to the compounded 
   *                      integral has been obtained it is put in 
   *                      rlist2(numrl2) after numrl2 has been increased 
   *                      by one. 
   *          small     - length of the smallest interval considered up 
   *                      to now, multiplied by 1.5 
   *          erlarg    - sum of the errors over the intervals larger 
   *                      than the smallest interval considered up to now 
   *          extrap    - int variable denoting that the routine is 
   *                      attempting to perform extrapolation i.e. before 
   *                      subdividing the smallest interval we try to 
   *                      decrease the value of erlarg. 
   *          noext     - int variable denoting that extrapolation 
   *                      is no longer allowed (true value) 
   * 
   *           machine dependent constants 
   *           --------------------------- 
   * 
   *          epmach is the largest relative spacing. 
   *          uflow is the smallest positive magnitude. 
   *          oflow is the largest positive magnitude. 
   * 
   ****first executable statement  dqagse 
   */
  /* Parameter adjustments */
  --iord;
  --elist;
  --rlist;
  --blist;
  --alist__;

  /* Function Body */
  epmach = nsp_dlamch ("p");
  /* 
   *           test on validity of parameters 
   *           ------------------------------ 
   */
  *ier = 0;
  *neval = 0;
  *last = 0;
  *result = 0.;
  *abserr = 0.;
  alist__[1] = *a;
  blist[1] = *b;
  rlist[1] = 0.;
  elist[1] = 0.;
  /*Computing MAX 
   */
  d__1 = epmach * 50.;
  if (*epsabs <= 0. && *epsrel < Max (d__1, 5e-29))
    {
      *ier = 6;
      return 0;
    }
  /* 
   *          first approximation to the integral 
   *          ----------------------------------- 
   * 
   */
  uflow = nsp_dlamch ("u");
  oflow = nsp_dlamch ("o");
  ierro = 0;
  nsp_quadpack_dqk21 ((intg_f) f, a, b, result, abserr, &defabs, &resabs, 
		      vectflag,   stat);
  if (*stat != 0)
    {
      return 0;
    }
  /* 
   *          test on accuracy. 
   * 
   */
  dres = Abs (*result);
  /*Computing MAX 
   */
  d__1 = *epsabs, d__2 = *epsrel * dres;
  errbnd = Max (d__1, d__2);
  *last = 1;
  rlist[1] = *result;
  elist[1] = *abserr;
  iord[1] = 1;
  if (*abserr <= epmach * 100. * defabs && *abserr > errbnd)
    {
      *ier = 2;
    }
  if (*limit == 1)
    {
      *ier = 1;
    }
  if (*ier != 0 || (*abserr <= errbnd && *abserr != resabs) || *abserr == 0.)
    {
      goto L140;
    }
  /* 
   *          initialization 
   *          -------------- 
   * 
   */
  rlist2[0] = *result;
  errmax = *abserr;
  maxerr = 1;
  area = *result;
  errsum = *abserr;
  *abserr = oflow;
  nrmax = 1;
  nres = 0;
  numrl2 = 2;
  ktmin = 0;
  extrap = FALSE;
  noext = FALSE;
  iroff1 = 0;
  iroff2 = 0;
  iroff3 = 0;
  ksgn = -1;
  if (dres >= (1. - epmach * 50.) * defabs)
    {
      ksgn = 1;
    }
  /* 
   *          main do-loop 
   *          ------------ 
   * 
   */
  i__1 = *limit;
  for (*last = 2; *last <= i__1; ++(*last))
    {
      /* 
       *          bisect the subinterval with the nrmax-th largest error 
       *          estimate. 
       * 
       */
      a1 = alist__[maxerr];
      b1 = (alist__[maxerr] + blist[maxerr]) * .5;
      a2 = b1;
      b2 = blist[maxerr];
      erlast = errmax;
      /************************************************************************ 
       * 
       *  use new nspdqk21b to integrate on the 2 subintervals improving the 
       *  (possible) vectorization for nsp function. (bruno on 12 july 2009) 
       * 
       */
      nsp_quadpack_dqk21b ((intg_f) f, &a1, &b2, &area1, &error1, &defab1,
			   &area2, &error2, &defab2, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      /*$$$         call nspdqk21(f,a1,b1,area1,error1,resabs,defab1,vectflag,stat) 
       *$$$         if ( stat .ne. 0 ) return 
       *$$$         call nspdqk21(f,a2,b2,area2,error2,resabs,defab2,vectflag,stat) 
       *$$$         if ( stat .ne. 0 ) return 
       * 
       ************************************************************************ 
       * 
       *          improve previous approximations to integral 
       *          and error and test for accuracy. 
       * 
       */
      area12 = area1 + area2;
      erro12 = error1 + error2;
      errsum = errsum + erro12 - errmax;
      area = area + area12 - rlist[maxerr];
      if (defab1 == error1 || defab2 == error2)
	{
	  goto L15;
	}
      if ((d__1 = rlist[maxerr] - area12, Abs (d__1)) > Abs (area12) * 1e-5
	  || erro12 < errmax * .99)
	{
	  goto L10;
	}
      if (extrap)
	{
	  ++iroff2;
	}
      if (!extrap)
	{
	  ++iroff1;
	}
    L10:
      if (*last > 10 && erro12 > errmax)
	{
	  ++iroff3;
	}
    L15:
      rlist[maxerr] = area1;
      rlist[*last] = area2;
      /*Computing MAX 
       */
      d__1 = *epsabs, d__2 = *epsrel * Abs (area);
      errbnd = Max (d__1, d__2);
      /* 
       *          test for roundoff error and eventually set error flag. 
       * 
       */
      if (iroff1 + iroff2 >= 10 || iroff3 >= 20)
	{
	  *ier = 2;
	}
      if (iroff2 >= 5)
	{
	  ierro = 3;
	}
      /* 
       *          set error flag in the case that the number of subintervals 
       *          equals limit. 
       * 
       */
      if (*last == *limit)
	{
	  *ier = 1;
	}
      /* 
       *          set error flag in the case of bad integrand behaviour 
       *          at a point of the integration range. 
       * 
       *Computing MAX 
       */
      d__1 = Abs (a1), d__2 = Abs (b2);
      if (Max (d__1, d__2) <= (epmach * 100. + 1.) * (Abs (a2) + uflow * 1e3))
	{
	  *ier = 4;
	}
      /* 
       *          append the newly-created intervals to the list. 
       * 
       */
      if (error2 <= error1)
	{
	  alist__[*last] = a2;
	  blist[maxerr] = b1;
	  blist[*last] = b2;
	  elist[maxerr] = error1;
	  elist[*last] = error2;
	}
      else
	{
	  alist__[maxerr] = a2;
	  alist__[*last] = a1;
	  blist[*last] = b1;
	  rlist[maxerr] = area2;
	  rlist[*last] = area1;
	  elist[maxerr] = error2;
	  elist[*last] = error1;
	}
      /* 
       *          call subroutine dqpsrt to maintain the descending ordering 
       *          in the list of error estimates and select the subinterval 
       *          with nrmax-th largest error estimate (to be bisected next). 
       * 
       */
      nsp_quadpack_dqpsrt (limit, last, &maxerr, &errmax, &elist[1], &iord[1],
			   &nrmax);
      /*    ***jump out of do-loop 
       */
      if (errsum <= errbnd)
	{
	  goto L115;
	}
      /*    ***jump out of do-loop 
       */
      if (*ier != 0)
	{
	  goto L100;
	}
      if (*last == 2)
	{
	  goto L80;
	}
      if (noext)
	{
	  goto L90;
	}
      erlarg -= erlast;
      if ((d__1 = b1 - a1, Abs (d__1)) > small)
	{
	  erlarg += erro12;
	}
      if (extrap)
	{
	  goto L40;
	}
      /* 
       *          test whether the interval to be bisected next is the 
       *          smallest interval. 
       * 
       */
      if ((d__1 = blist[maxerr] - alist__[maxerr], Abs (d__1)) > small)
	{
	  goto L90;
	}
      extrap = TRUE;
      nrmax = 2;
    L40:
      if (ierro == 3 || erlarg <= ertest)
	{
	  goto L60;
	}
      /* 
       *          the smallest interval has the largest error. 
       *          before bisecting decrease the sum of the errors over the 
       *          larger intervals (erlarg) and perform extrapolation. 
       * 
       */
      id = nrmax;
      jupbnd = *last;
      if (*last > *limit / 2 + 2)
	{
	  jupbnd = *limit + 3 - *last;
	}
      i__2 = jupbnd;
      for (k = id; k <= i__2; ++k)
	{
	  maxerr = iord[nrmax];
	  errmax = elist[maxerr];
	  /*          ***jump out of do-loop 
	   */
	  if ((d__1 = blist[maxerr] - alist__[maxerr], Abs (d__1)) > small)
	    {
	      goto L90;
	    }
	  ++nrmax;
	}
      /* 
       *          perform extrapolation. 
       * 
       */
    L60:
      ++numrl2;
      rlist2[numrl2 - 1] = area;
      nsp_quadpack_dqelg (&numrl2, rlist2, &reseps, &abseps, res3la, &nres);
      ++ktmin;
      if (ktmin > 5 && *abserr < errsum * .001)
	{
	  *ier = 5;
	}
      if (abseps >= *abserr)
	{
	  goto L70;
	}
      ktmin = 0;
      *abserr = abseps;
      *result = reseps;
      correc = erlarg;
      /*Computing MAX 
       */
      d__1 = *epsabs, d__2 = *epsrel * Abs (reseps);
      ertest = Max (d__1, d__2);
      /*    ***jump out of do-loop 
       */
      if (*abserr <= ertest)
	{
	  goto L100;
	}
      /* 
       *          prepare bisection of the smallest interval. 
       * 
       */
    L70:
      if (numrl2 == 1)
	{
	  noext = TRUE;
	}
      if (*ier == 5)
	{
	  goto L100;
	}
      maxerr = iord[1];
      errmax = elist[maxerr];
      nrmax = 1;
      extrap = FALSE;
      small *= .5;
      erlarg = errsum;
      goto L90;
    L80:
      small = (d__1 = *b - *a, Abs (d__1)) * .375;
      erlarg = errsum;
      ertest = errbnd;
      rlist2[1] = area;
    L90:
      ;
    }
  /* 
   *          set final result and error estimate. 
   *          ------------------------------------ 
   * 
   */
 L100:
  if (*abserr == oflow)
    {
      goto L115;
    }
  if (*ier + ierro == 0)
    {
      goto L110;
    }
  if (ierro == 3)
    {
      *abserr += correc;
    }
  if (*ier == 0)
    {
      *ier = 3;
    }
  if (*result != 0. && area != 0.)
    {
      goto L105;
    }
  if (*abserr > errsum)
    {
      goto L115;
    }
  if (area == 0.)
    {
      goto L130;
    }
  goto L110;
 L105:
  if (*abserr / Abs (*result) > errsum / Abs (area))
    {
      goto L115;
    }
  /* 
   *          test on divergence. 
   * 
   */
 L110:
  /*Computing MAX 
   */
  d__1 = Abs (*result), d__2 = Abs (area);
  if (ksgn == -1 && Max (d__1, d__2) <= defabs * .01)
    {
      goto L130;
    }
  if (.01 > *result / area || *result / area > 100. || errsum > Abs (area))
    {
      *ier = 6;
    }
  goto L130;
  /* 
   *          compute global integral sum. 
   * 
   */
 L115:
  *result = 0.;
  i__1 = *last;
  for (k = 1; k <= i__1; ++k)
    {
      *result += rlist[k];
    }
  *abserr = errsum;
 L130:
  if (*ier > 2)
    {
      --(*ier);
    }
 L140:
  *neval = *last * 42 - 21;
  return 0;
}				/* nspdqagse_ */

int
nsp_quadpack_dqagie (intg_f f, double *bound, int *inf, double *epsabs,
		     double *epsrel, int *limit, double *result, 
		     double *abserr,  int *neval, int *ier, double *alist__,
		     double *blist, double *rlist, double *elist, int *iord, 
		     int *last, int *vectflag, int *stat)
{
  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double area, dres;
  int ksgn;
  double boun;
  int nres;
  double area1, area2;
  double area12;
  int k;
  double small, erro12;
  int ierro;
  double a1, a2, b1, b2, defab1, defab2, oflow;
  int ktmin, nrmax;
  double uflow;
  int noext;
  int iroff1, iroff2, iroff3;
  double res3la[3], error1, error2;
  int id;
  double rlist2[52];
  int numrl2;
  double defabs;
  double epmach, erlarg, abseps, correc, errbnd, resabs;
  int jupbnd;
  double erlast, errmax;
  int maxerr;
  double reseps;
  int extrap;
  double ertest, errsum;

  /* 
****begin prologue  dqagie 
****date written   800101   (yymmdd) 
*                            will probably gain from splitting up the 
*                            interval at this point and calling the 
*                            integrator on the subranges. if possible, 
*                            an appropriate special-purpose integrator 
*                            should be used, which is designed for 
*                            handling the type of difficulty involved. 
*                        = 2 the occurrence of roundoff error is 
*                            detected, which prevents the requested 
*                            tolerance from being achieved. 
*                            the error may be under-estimated. 
*                        = 3 extremely bad integrand behaviour occurs 
*                            at some points of the integration 
*                            interval. 
*                        = 4 the algorithm does not converge. 
*                            roundoff error is detected in the 
*                            extrapolation table. 
*                            it is assumed that the requested tolerance 
*                            cannot be achieved, and that the returned 
*                            result is the best which can be obtained. 
*                        = 5 the integral is probably divergent, or 
*                            slowly convergent. it must be noted that 
*                            divergence can occur with any other value 
*                            of ier. 
*                        = 6 the input is invalid, because 
*                            (epsabs.le.0 and 
*                             epsrel.lt.max(50*rel.mach.acc.,0.5d-28), 
*                            result, abserr, neval, last, rlist(1), 
*                            elist(1) and iord(1) are set to zero. 
*                            alist(1) and blist(1) are set to 0 
*                            and 1 respectively. 
* 
*           alist  - double precision 
*                    vector of dimension at least limit, the first 
*                     last  elements of which are the left 
*                    end points of the subintervals in the partition 
*                    of the transformed integration range (0,1). 
* 
*           blist  - double precision 
*                    vector of dimension at least limit, the first 
*                     last  elements of which are the right 
*                    end points of the subintervals in the partition 
*                    of the transformed integration range (0,1). 
* 
*           rlist  - double precision 
*                    vector of dimension at least limit, the first 
*                     last  elements of which are the integral 
*                    approximations on the subintervals 
* 
*           elist  - double precision 
*                    vector of dimension at least limit,  the first 
*                    last elements of which are the moduli of the 
*                    absolute error estimates on the subintervals 
* 
*           iord   - int 
*                    vector of dimension limit, the first k 
*                    elements of which are pointers to the 
*                    error estimates over the subintervals, 
*                    such that elist(iord(1)), ..., elist(iord(k)) 
*                    form a decreasing sequence, with k = last 
*                    if last.le.(limit/2+2), and k = limit+1-last 
*                    otherwise 
* 
*           last   - int 
*                    number of subintervals actually produced 
*                    in the subdivision process 
* 
****references  (none) 
****routines called  dlamch,dqelg,dqk15i,dqpsrt 
****end prologue  dqagie 
* 
*   local vars 
* 
*           the dimension of rlist2 is determined by the value of 
*           limexp in subroutine dqelg. 
* 
* 
*           list of major variables 
*           ----------------------- 
* 
*          alist     - list of left end points of all subintervals 
*                      considered up to now 
*          blist     - list of right end points of all subintervals 
*                      considered up to now 
*          rlist(i)  - approximation to the integral over 
*                      (alist(i),blist(i)) 
*          rlist2    - array of dimension at least (limexp+2), 
*                      containing the part of the epsilon table 
*                      wich is still needed for further computations 
*          elist(i)  - error estimate applying to rlist(i) 
*          maxerr    - pointer to the interval with largest error 
*                      estimate 
*          errmax    - elist(maxerr) 
*          erlast    - error on the interval currently subdivided 
*                      (before that subdivision has taken place) 
*          area      - sum of the integrals over the subintervals 
*          errsum    - sum of the errors over the subintervals 
*          errbnd    - requested accuracy Max(epsabs,epsrel* 
*                      Abs(result)) 
*          *****1    - variable for the left subinterval 
*          *****2    - variable for the right subinterval 
*          last      - index for subdivision 
*          nres      - number of calls to the extrapolation routine 
*          numrl2    - number of elements currently in rlist2. if an 
*                      appropriate approximation to the compounded 
*                      integral has been obtained, it is put in 
*                      rlist2(numrl2) after numrl2 has been increased 
*                      by one. 
*          small     - length of the smallest interval considered up 
*                      to now, multiplied by 1.5 
*          erlarg    - sum of the errors over the intervals larger 
*                      than the smallest interval considered up to now 
*          extrap    - int variable denoting that the routine 
*                      is attempting to perform extrapolation. i.e. 
*                      before subdividing the smallest interval we 
*                      try to decrease the value of erlarg. 
*          noext     - int variable denoting that extrapolation 
*                      is no longer allowed (true-value) 
* 
*           machine dependent constants 
*           --------------------------- 
* 
*          epmach is the largest relative spacing. 
*          uflow is the smallest positive magnitude. 
*          oflow is the largest positive magnitude. 
* 
****first executable statement  dqagie 
*/
  /* Parameter adjustments */
  --iord;
  --elist;
  --rlist;
  --blist;
  --alist__;

  /* Function Body */
  epmach = nsp_dlamch ("p");
  /* 
   *          test on validity of parameters 
   *          ----------------------------- 
   * 
   */
  *ier = 0;
  *neval = 0;
  *last = 0;
  *result = 0.;
  *abserr = 0.;
  alist__[1] = 0.;
  blist[1] = 1.;
  rlist[1] = 0.;
  elist[1] = 0.;
  iord[1] = 0;
  /*Computing MAX 
   */
  d__1 = epmach * 50.;
  if (*epsabs <= 0. && *epsrel < Max (d__1, 5e-29))
    {
      *ier = 6;
      return 0;
    }
  /* 
   * 
   *          first approximation to the integral 
   *          ----------------------------------- 
   * 
   *          determine the interval to be mapped onto (0,1). 
   *          if inf = 2 the integral is computed as i = i1+i2, where 
   *          i1 = integral of f over (-infinity,0), 
   *          i2 = integral of f over (0,+infinity). 
   * 
   */
  boun = *bound;
  if (*inf == 2)
    {
      boun = 0.;
    }
  nsp_quadpack_dqk15i ((intg_f) f, &boun, inf, &c_b20, &c_b21, result, abserr,
		       &defabs, &resabs, vectflag, stat);
  if (*stat != 0)
    {
      return 0;
    }
  /* 
   *          test on accuracy 
   * 
   */
  *last = 1;
  rlist[1] = *result;
  elist[1] = *abserr;
  iord[1] = 1;
  dres = Abs (*result);
  /*Computing MAX 
   */
  d__1 = *epsabs, d__2 = *epsrel * dres;
  errbnd = Max (d__1, d__2);
  if (*abserr <= epmach * 100. * defabs && *abserr > errbnd)
    {
      *ier = 2;
    }
  if (*limit == 1)
    {
      *ier = 1;
    }
  if (*ier != 0 || (*abserr <= errbnd && *abserr != resabs) || *abserr == 0.)
    {
      goto L130;
    }
  /* 
   *          initialization 
   *          -------------- 
   * 
   */
  uflow = nsp_dlamch ("u");
  oflow = nsp_dlamch ("o");
  rlist2[0] = *result;
  errmax = *abserr;
  maxerr = 1;
  area = *result;
  errsum = *abserr;
  *abserr = oflow;
  nrmax = 1;
  nres = 0;
  ktmin = 0;
  numrl2 = 2;
  extrap = FALSE;
  noext = FALSE;
  ierro = 0;
  iroff1 = 0;
  iroff2 = 0;
  iroff3 = 0;
  ksgn = -1;
  if (dres >= (1. - epmach * 50.) * defabs)
    {
      ksgn = 1;
    }
  /* 
   *          main do-loop 
   *          ------------ 
   * 
   */
  i__1 = *limit;
  for (*last = 2; *last <= i__1; ++(*last))
    {
      /* 
       *          bisect the subinterval with nrmax-th largest error estimate. 
       * 
       */
      a1 = alist__[maxerr];
      b1 = (alist__[maxerr] + blist[maxerr]) * .5;
      a2 = b1;
      b2 = blist[maxerr];
      erlast = errmax;
      nsp_quadpack_dqk15i ((intg_f) f, &boun, inf, &a1, &b1, &area1, &error1,
			   &resabs, &defab1, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      nsp_quadpack_dqk15i ((intg_f) f, &boun, inf, &a2, &b2, &area2, &error2,
			   &resabs, &defab2, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      /* 
       *          improve previous approximations to integral 
       *          and error and test for accuracy. 
       * 
       */
      area12 = area1 + area2;
      erro12 = error1 + error2;
      errsum = errsum + erro12 - errmax;
      area = area + area12 - rlist[maxerr];
      if (defab1 == error1 || defab2 == error2)
	{
	  goto L15;
	}
      if ((d__1 = rlist[maxerr] - area12, Abs (d__1)) > Abs (area12) * 1e-5
	  || erro12 < errmax * .99)
	{
	  goto L10;
	}
      if (extrap)
	{
	  ++iroff2;
	}
      if (!extrap)
	{
	  ++iroff1;
	}
    L10:
      if (*last > 10 && erro12 > errmax)
	{
	  ++iroff3;
	}
    L15:
      rlist[maxerr] = area1;
      rlist[*last] = area2;
      /*Computing MAX 
       */
      d__1 = *epsabs, d__2 = *epsrel * Abs (area);
      errbnd = Max (d__1, d__2);
      /* 
       *          test for roundoff error and eventually set error flag. 
       * 
       */
      if (iroff1 + iroff2 >= 10 || iroff3 >= 20)
	{
	  *ier = 2;
	}
      if (iroff2 >= 5)
	{
	  ierro = 3;
	}
      /* 
       *          set error flag in the case that the number of 
       *          subintervals equals limit. 
       * 
       */
      if (*last == *limit)
	{
	  *ier = 1;
	}
      /* 
       *          set error flag in the case of bad integrand behaviour 
       *          at some points of the integration range. 
       * 
       *Computing MAX 
       */
      d__1 = Abs (a1), d__2 = Abs (b2);
      if (Max (d__1, d__2) <= (epmach * 100. + 1.) * (Abs (a2) + uflow * 1e3))
	{
	  *ier = 4;
	}
      /* 
       *          append the newly-created intervals to the list. 
       * 
       */
      if (error2 <= error1)
	{
	  alist__[*last] = a2;
	  blist[maxerr] = b1;
	  blist[*last] = b2;
	  elist[maxerr] = error1;
	  elist[*last] = error2;
	}
      else
	{
	  alist__[maxerr] = a2;
	  alist__[*last] = a1;
	  blist[*last] = b1;
	  rlist[maxerr] = area2;
	  rlist[*last] = area1;
	  elist[maxerr] = error2;
	  elist[*last] = error1;
	}
      /* 
       *          call subroutine dqpsrt to maintain the descending ordering 
       *          in the list of error estimates and select the subinterval 
       *          with nrmax-th largest error estimate (to be bisected next). 
       * 
       */
      nsp_quadpack_dqpsrt (limit, last, &maxerr, &errmax, &elist[1], &iord[1],
			   &nrmax);
      if (errsum <= errbnd)
	{
	  goto L115;
	}
      if (*ier != 0)
	{
	  goto L100;
	}
      if (*last == 2)
	{
	  goto L80;
	}
      if (noext)
	{
	  goto L90;
	}
      erlarg -= erlast;
      if ((d__1 = b1 - a1, Abs (d__1)) > small)
	{
	  erlarg += erro12;
	}
      if (extrap)
	{
	  goto L40;
	}
      /* 
       *          test whether the interval to be bisected next is the 
       *          smallest interval. 
       * 
       */
      if ((d__1 = blist[maxerr] - alist__[maxerr], Abs (d__1)) > small)
	{
	  goto L90;
	}
      extrap = TRUE;
      nrmax = 2;
    L40:
      if (ierro == 3 || erlarg <= ertest)
	{
	  goto L60;
	}
      /* 
       *          the smallest interval has the largest error. 
       *          before bisecting decrease the sum of the errors over the 
       *          larger intervals (erlarg) and perform extrapolation. 
       * 
       */
      id = nrmax;
      jupbnd = *last;
      if (*last > *limit / 2 + 2)
	{
	  jupbnd = *limit + 3 - *last;
	}
      i__2 = jupbnd;
      for (k = id; k <= i__2; ++k)
	{
	  maxerr = iord[nrmax];
	  errmax = elist[maxerr];
	  if ((d__1 = blist[maxerr] - alist__[maxerr], Abs (d__1)) > small)
	    {
	      goto L90;
	    }
	  ++nrmax;
	}
      /* 
       *          perform extrapolation. 
       * 
       */
    L60:
      ++numrl2;
      rlist2[numrl2 - 1] = area;
      nsp_quadpack_dqelg (&numrl2, rlist2, &reseps, &abseps, res3la, &nres);
      ++ktmin;
      if (ktmin > 5 && *abserr < errsum * .001)
	{
	  *ier = 5;
	}
      if (abseps >= *abserr)
	{
	  goto L70;
	}
      ktmin = 0;
      *abserr = abseps;
      *result = reseps;
      correc = erlarg;
      /*Computing MAX 
       */
      d__1 = *epsabs, d__2 = *epsrel * Abs (reseps);
      ertest = Max (d__1, d__2);
      if (*abserr <= ertest)
	{
	  goto L100;
	}
      /* 
       *           prepare bisection of the smallest interval. 
       * 
       */
    L70:
      if (numrl2 == 1)
	{
	  noext = TRUE;
	}
      if (*ier == 5)
	{
	  goto L100;
	}
      maxerr = iord[1];
      errmax = elist[maxerr];
      nrmax = 1;
      extrap = FALSE;
      small *= .5;
      erlarg = errsum;
      goto L90;
    L80:
      small = .375;
      erlarg = errsum;
      ertest = errbnd;
      rlist2[1] = area;
    L90:
      ;
    }
  /* 
   *          set final result and error estimate. 
   *          ------------------------------------ 
   * 
   */
 L100:
  if (*abserr == oflow)
    {
      goto L115;
    }
  if (*ier + ierro == 0)
    {
      goto L110;
    }
  if (ierro == 3)
    {
      *abserr += correc;
    }
  if (*ier == 0)
    {
      *ier = 3;
    }
  if (*result != 0. && area != 0.)
    {
      goto L105;
    }
  if (*abserr > errsum)
    {
      goto L115;
    }
  if (area == 0.)
    {
      goto L130;
    }
  goto L110;
 L105:
  if (*abserr / Abs (*result) > errsum / Abs (area))
    {
      goto L115;
    }
  /* 
   *          test on divergence 
   * 
   */
 L110:
  /*Computing MAX 
   */
  d__1 = Abs (*result), d__2 = Abs (area);
  if (ksgn == -1 && Max (d__1, d__2) <= defabs * .01)
    {
      goto L130;
    }
  if (.01 > *result / area || *result / area > 100. || errsum > Abs (area))
    {
      *ier = 6;
    }
  goto L130;
  /* 
   *          compute global integral sum. 
   * 
   */
 L115:
  *result = 0.;
  i__1 = *last;
  for (k = 1; k <= i__1; ++k)
    {
      *result += rlist[k];
      /* L120: */
    }
  *abserr = errsum;
 L130:
  *neval = *last * 30 - 15;
  if (*inf == 2)
    {
      *neval <<= 1;
    }
  if (*ier > 2)
    {
      --(*ier);
    }
  return 0;
}				/* nspdqagie_ */

static int
nsp_quadpack_dqelg (int *n, double *epstab, double *result, double *abserr,
		    double *res3la, int *nres)
{
  /* System generated locals */
  int i__1;
  double d__1, d__2, d__3;

  /* Local variables */
  int indx;
  double e1abs;
  int i__;
  double e0, e1, e2, e3, error, oflow;
  int k1, k2, k3;
  double delta1, delta2, delta3;
  int ib, ie;
  double epmach, ss, epsinf;
  int newelm, ib2, limexp;
  double res;
  int num;
  double err1, err2, err3, tol1, tol2, tol3;

  /****begin prologue  dqelg 
   ****refer to  dqagie,dqagoe,dqagpe,dqagse 
   ****routines called  d1mach 
   ****revision date  830518   (yymmdd) 
   ****keywords  epsilon algorithm, convergence acceleration, 
   *            extrapolation 
   ****author  piessens,robert,appl. math. & progr. div. - k.u.leuven 
   *          de doncker,elise,appl. math & progr. div. - k.u.leuven 
   * 
   ****slightly modified by bruno pincon for nsp: transformation of the 
   *          toward a cleaner fortran 
   * 
   ****purpose  the routine determines the limit of a given sequence of 
   *           approximations, by means of the epsilon algorithm of 
   *           p.wynn. an estimate of the absolute error is also given. 
   *           the condensed epsilon table is computed. only those 
   *           elements needed for the computation of the next diagonal 
   *           are preserved. 
   ****description 
   * 
   *          epsilon algorithm 
   *          standard fortran subroutine 
   *          double precision version 
   * 
   *          parameters 
   *             n      - int 
   *                      epstab(n) contains the new element in the 
   *                      first column of the epsilon table. 
   * 
   *             epstab - double precision 
   *                      vector of dimension 52 containing the elements 
   *                      of the two lower diagonals of the triangular 
   *                      epsilon table. the elements are numbered 
   *                      starting at the right-hand corner of the 
   *                      triangle. 
   * 
   *             result - double precision 
   *                      resulting approximation to the integral 
   * 
   *             abserr - double precision 
   *                      estimate of the absolute error computed from 
   *                      result and the 3 previous results 
   * 
   *             res3la - double precision 
   *                      vector of dimension 3 containing the last 3 
   *                      results 
   * 
   *             nres   - int 
   *                      number of calls to the routine 
   *                      (should be zero at first call) 
   * 
   ****end prologue  dqelg 
   * 
   * 
   *          list of major variables 
   *          ----------------------- 
   * 
   *          e0     - the 4 elements on which the computation of a new 
   *          e1       element in the epsilon table is based 
   *          e2 
   *          e3                 e0 
   *                       e3    e1    new 
   *                             e2 
   *          newelm - number of elements to be computed in the new 
   *                   diagonal 
   *          error  - error = Abs(e1-e0)+abs(e2-e1)+abs(new-e2) 
   *          result - the element in the new diagonal with least value 
   *                   of error 
   * 
   *          machine dependent constants 
   *          --------------------------- 
   * 
   *          epmach is the largest relative spacing. 
   *          oflow is the largest positive magnitude. 
   *          limexp is the maximum number of elements the epsilon 
   *          table can contain. if this number is reached, the upper 
   *          diagonal of the epsilon table is deleted. 
   * 
   ****first executable statement  dqelg 
   */
  /* Parameter adjustments */
  --res3la;
  --epstab;

  /* Function Body */
  epmach = nsp_dlamch ("p");
  oflow = nsp_dlamch ("o");
  ++(*nres);
  *abserr = oflow;
  *result = epstab[*n];
  if (*n < 3)
    {
      goto L100;
    }
  limexp = 50;
  epstab[*n + 2] = epstab[*n];
  newelm = (*n - 1) / 2;
  epstab[*n] = oflow;
  num = *n;
  k1 = *n;
  i__1 = newelm;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      k2 = k1 - 1;
      k3 = k1 - 2;
      res = epstab[k1 + 2];
      e0 = epstab[k3];
      e1 = epstab[k2];
      e2 = res;
      e1abs = Abs (e1);
      delta2 = e2 - e1;
      err2 = Abs (delta2);
      /*Computing MAX 
       */
      d__1 = Abs (e2);
      tol2 = Max (d__1, e1abs) * epmach;
      delta3 = e1 - e0;
      err3 = Abs (delta3);
      /*Computing MAX 
       */
      d__1 = e1abs, d__2 = Abs (e0);
      tol3 = Max (d__1, d__2) * epmach;
      if (err2 <= tol2 && err3 <= tol3)
	{
	  /* 
	   *          if e0, e1 and e2 are equal to within machine 
	   *          accuracy, convergence is assumed. 
	   *          result = e2 
	   *          abserr = Abs(e1-e0)+abs(e2-e1) 
	   * 
	   */
	  *result = res;
	  *abserr = err2 + err3;
	  /*          ***jump out of do-loop 
	   */
	  goto L100;
	}
      e3 = epstab[k1];
      epstab[k1] = e1;
      delta1 = e1 - e3;
      err1 = Abs (delta1);
      /*Computing MAX 
       */
      d__1 = e1abs, d__2 = Abs (e3);
      tol1 = Max (d__1, d__2) * epmach;
      /* 
       *          if two elements are very close to each other, omit 
       *          a part of the table by adjusting the value of n 
       * 
       */
      if (err1 <= tol1 || err2 <= tol2 || err3 <= tol3)
	{
	  *n = i__ + i__ - 1;
	  /*          ***jump out of do-loop 
	   */
	  goto L50;
	}
      ss = 1. / delta1 + 1. / delta2 - 1. / delta3;
      epsinf = (d__1 = ss * e1, Abs (d__1));
      /* 
       *          test to detect irregular behaviour in the table, and 
       *          eventually omit a part of the table adjusting the value 
       *          of n. 
       * 
       */
      if (epsinf <= 1e-4)
	{
	  *n = i__ + i__ - 1;
	  /*          ***jump out of do-loop 
	   */
	  goto L50;
	}
      /* 
       *          compute a new element and eventually adjust 
       *          the value of result. 
       * 
       */
      res = e1 + 1. / ss;
      epstab[k1] = res;
      k1 += -2;
      error = err2 + (d__1 = res - e2, Abs (d__1)) + err3;
      if (error <= *abserr)
	{
	  *abserr = error;
	  *result = res;
	}
    }
  /* 
   *          shift the table. 
   * 
   */
 L50:
  if (*n == limexp)
    {
      *n = (limexp / 2 << 1) - 1;
    }
  ib = 1;
  if (num / 2 << 1 == num)
    {
      ib = 2;
    }
  ie = newelm + 1;
  i__1 = ie;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ib2 = ib + 2;
      epstab[ib] = epstab[ib2];
      ib = ib2;
    }
  if (num != *n)
    {
      indx = num - *n + 1;
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  epstab[i__] = epstab[indx];
	  ++indx;
	}
    }
  if (*nres < 4)
    {
      res3la[*nres] = *result;
      *abserr = oflow;
    }
  else
    {
      /*       compute error estimate 
       */
      *abserr = (d__1 = *result - res3la[3], Abs (d__1)) + (d__2 =
							    *result -
							    res3la[2],
							    Abs (d__2)) +
	(d__3 = *result - res3la[1], Abs (d__3));
      res3la[1] = res3la[2];
      res3la[2] = res3la[3];
      res3la[3] = *result;
    }
 L100:
  /*Computing MAX 
   */
  d__1 = *abserr, d__2 = epmach * 5. * Abs (*result);
  *abserr = Max (d__1, d__2);
  return 0;
}				/* nspdqelg_ */

static int
nsp_quadpack_dqpsrt (int *limit, int *last, int *maxerr, double *ermax,
		     double *elist, int *iord, int *nrmax)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int ibeg, jbnd, i__, j, k, isucc, jupbn;
  double errmin, errmax;
  int ido;

  /****begin prologue  dqpsrt 
   ****refer to  dqage,dqagie,dqagpe,dqawse 
   ****routines called  (none) 
   ****revision date  810101   (yymmdd) 
   ****keywords  sequential sorting 
   ****author  piessens,robert,appl. math. & progr. div. - k.u.leuven 
   *          de doncker,elise,appl. math. & progr. div. - k.u.leuven 
   * 
   ****slightly modified by bruno pincon for nsp: transformation of the 
   *          toward a cleaner fortran 
   ****purpose  this routine maintains the descending ordering in the 
   *           list of the local error estimated resulting from the 
   *           interval subdivision process. at each call two error 
   *           estimates are inserted using the sequential search 
   *           method, top-down for the largest error estimate and 
   *           bottom-up for the smallest error estimate. 
   ****description 
   * 
   *          ordering routine 
   *          standard fortran subroutine 
   *          double precision version 
   * 
   *          parameters (meaning at output) 
   *             limit  - int 
   *                      maximum number of error estimates the list 
   *                      can contain 
   * 
   *             last   - int 
   *                      number of error estimates currently in the list 
   * 
   *             maxerr - int 
   *                      maxerr points to the nrmax-th largest error 
   *                      estimate currently in the list 
   * 
   *             ermax  - double precision 
   *                      nrmax-th largest error estimate 
   *                      ermax = elist(maxerr) 
   * 
   *             elist  - double precision 
   *                      vector of dimension last containing 
   *                      the error estimates 
   * 
   *             iord   - int 
   *                      vector of dimension last, the first k elements 
   *                      of which contain pointers to the error 
   *                      estimates, such that 
   *                      elist(iord(1)),...,  elist(iord(k)) 
   *                      form a decreasing sequence, with 
   *                      k = last if last.le.(limit/2+2), and 
   *                      k = limit+1-last otherwise 
   * 
   *             nrmax  - int 
   *                      maxerr = iord(nrmax) 
   * 
   ****end prologue  dqpsrt 
   * 
   * local var 
   * 
   *          check whether the list contains more than 
   *          two error estimates. 
   * 
   ****first executable statement  dqpsrt 
   */
  /* Parameter adjustments */
  --iord;
  --elist;

  /* Function Body */
  if (*last <= 2)
    {
      iord[1] = 1;
      iord[2] = 2;
      *maxerr = iord[*nrmax];
      *ermax = elist[*maxerr];
      return 0;
    }
  /* 
   *          this part of the routine is only executed if, due to a 
   *          difficult integrand, subdivision increased the error 
   *          estimate. in the normal case the insert procedure should 
   *          start after the nrmax-th largest error estimate. 
   * 
   */
  errmax = elist[*maxerr];
  if (*nrmax != 1)
    {
      ido = *nrmax - 1;
      i__1 = ido;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  isucc = iord[*nrmax - 1];
	  /*          ***jump out of do-loop 
	   */
	  if (errmax <= elist[isucc])
	    {
	      goto L30;
	    }
	  iord[*nrmax] = isucc;
	  --(*nrmax);
	}
    }
  /* 
   *          compute the number of elements in the list to be maintained 
   *          in descending order. this number depends on the number of 
   *          subdivisions still allowed. 
   * 
   */
 L30:
  jupbn = *last;
  if (*last > *limit / 2 + 2)
    {
      jupbn = *limit + 3 - *last;
    }
  errmin = elist[*last];
  /* 
   *          insert errmax by traversing the list top-down, 
   *          starting comparison from the element elist(iord(nrmax+1)). 
   * 
   */
  jbnd = jupbn - 1;
  ibeg = *nrmax + 1;
  if (ibeg > jbnd)
    {
      iord[jbnd] = *maxerr;
      iord[jupbn] = *last;
      goto L90;
    }
  i__1 = jbnd;
  for (i__ = ibeg; i__ <= i__1; ++i__)
    {
      isucc = iord[i__];
      /*       ***jump out of do-loop 
       */
      if (errmax >= elist[isucc])
	{
	  goto L60;
	}
      iord[i__ - 1] = isucc;
    }
  /* 
   *          insert errmin by traversing the list bottom-up. 
   * 
   */
 L60:
  iord[i__ - 1] = *maxerr;
  k = jbnd;
  i__1 = jbnd;
  for (j = i__; j <= i__1; ++j)
    {
      isucc = iord[k];
      /*       ***jump out of do-loop 
       */
      if (errmin < elist[isucc])
	{
	  iord[k + 1] = *last;
	  goto L90;
	}
      iord[k + 1] = isucc;
      --k;
    }
  iord[i__] = *last;
  /* 
   *          set maxerr and ermax. 
   * 
   */
 L90:
  *maxerr = iord[*nrmax];
  *ermax = elist[*maxerr];
  return 0;
}				/* nspdqpsrt_ */

static int
nsp_quadpack_dqk21 (intg_f fvect, double *a, double *b, double *result,
		    double *abserr, double *resabs, double *resasc,
		    int *vectflag, int *stat)
{
  /* Initialized data */

  static double wg[5] =
    { .066671344308688137593568809893332, .149451349150580593145776339657697,
      .219086362515982043995534934228163, .269266719309996355091226921569469,
      .295524224714752870173892994651338 };
  static double xgk[11] =
    { .995657163025808080735527280689003, .973906528517171720077964012084452,
      .930157491355708226001207180059508, .865063366688984510732096688423493,
      .780817726586416897063717578345042, .679409568299024406234327365114874,
      .562757134668604683339000099272694, .433395394129247190799265943165784,
      .294392862701460198131126603103866, .14887433898163121088482600112972, 0. };
  static double wgk[11] =
    { .011694638867371874278064396062192, .03255816230796472747881897245939,
      .05475589657435199603138130024458, .07503967481091995276704314091619,
      .093125454583697605535065465083366, .109387158802297641899210590325805,
      .123491976262065851077958109831074, .134709217311473325928054001771707,
      .142775938577060080797094273138717, .147739104901338491374841515972068,
      .149445554002916905664936468389821 };

  /* System generated locals */
  double d__1, d__2, d__3;

  /* Builtin functions */

  /* Local variables */
  double allf[21], resg, allx[21], resk, fsum, fval1, fval2;
  int j;
  double hlgth, centr, reskh, uflow;
  int n1, jg, n21;
  double dx, epmach, dhlgth;

  /****begin prologue  dqk21 
   ****date written   800101   (yymmdd) 
   ****revision date  830518   (yymmdd) 
   ****category no.  h2a1a2 
   ****keywords  21-point gauss-kronrod rules 
   ****author  piessens,robert,appl. math. & progr. div. - k.u.leuven 
   *          de doncker,elise,appl. math. & progr. div. - k.u.leuven 
   * 
   ****modified by bruno pincon for nsp. The call to the integrand function 
   *           take a different form and when vectflag is TRUE only one call 
   *           is done over all the integration points. The last added var 
   *           stat lets to manage problem when the evaluation of fvect 
   *           by the nsp interpretor failed. 
   * 
   ****purpose  to compute i = integral of f over (a,b), with error 
   *                          estimate 
   *                      j = integral of Abs(f) over (a,b) 
   ****description 
   * 
   *          integration rules 
   *          standard fortran subroutine 
   *          double precision version 
   * 
   *          parameters 
   *           on entry 
   *             fvect    function subprogram defining the integrand 
   *                      function. Have the form: 
   *                        stat = fvect(allx, allf,n) 
   *                      and return an int (stat) which is 0 
   *                      if the evaluation of fvect (generally by the 
   *                      nsp interpretor) is OK. Other value imply an 
   *                      immediate return to the caller. allx is a 
   *                      vector of length n and the function must 
   *                      compute: 
   *                        allf(i) = f(allx(i)) for  1 <= i <= n. 
   * 
   *             a      - double precision 
   *                      lower limit of integration 
   * 
   *             b      - double precision 
   *                      upper limit of integration 
   * 
   *          vectflag  - boolean true if fvect could be called on a 
   *                      vector x. This is useful when the fvect is 
   *                      coded in nsp language to speed up the computation. 
   * 
   * 
   *           on return 
   *             result - double precision 
   *                      approximation to the integral i 
   *                      result is computed by applying the 21-point 
   *                      kronrod rule (resk) obtained by optimal addition 
   *                      of abscissae to the 10-point gauss rule (resg). 
   * 
   *             abserr - double precision 
   *                      estimate of the modulus of the absolute error, 
   *                      which should not exceed Abs(i-result) 
   * 
   *             resabs - double precision 
   *                      approximation to the integral j 
   * 
   *             resasc - double precision 
   *                      approximation to the integral of Abs(f-i/(b-a)) 
   *                      over (a,b) 
   * 
   *               stat - error control for fvect evaluation. Is set 
   *                      by the fvect code which must return 0 if the evaluation 
   *                      of fvect is OK (any other values implies immediate 
   *                      return and stopping of the integration procedure) 
   * 
   ****references  (none) 
   ****routines called  dlamch 
   ****end prologue  dqk21 
   * 
   * 
   *          the abscissae and weights are given for the interval (-1,1). 
   *          because of symmetry only the positive abscissae and their 
   *          corresponding weights are given. 
   * 
   *          xgk    - abscissae of the 21-point kronrod rule 
   *                   xgk(2), xgk(4), ...  abscissae of the 10-point 
   *                   gauss rule 
   *                   xgk(1), xgk(3), ...  abscissae which are optimally 
   *                   added to the 10-point gauss rule 
   * 
   *          wgk    - weights of the 21-point kronrod rule 
   * 
   *          wg     - weights of the 10-point gauss rule 
   * 
   * 
   *gauss quadrature weights and kronrod quadrature abscissae and weights 
   *as evaluated with 80 decimal digit arithmetic by l. w. fullerton, 
   *bell labs, nov. 1981. 
   * 
   */
  /* 
   * 
   * 
   * 
   *          list of major variables 
   *          ----------------------- 
   * 
   *          centr  - mid point of the interval 
   *          hlgth  - half-length of the interval 
   *          fval*  - function value 
   *          resg   - result of the 10-point gauss formula 
   *          resk   - result of the 21-point kronrod formula 
   *          reskh  - approximation to the mean value of f over (a,b), 
   *                   i.e. to i/(b-a) 
   * 
   * 
   *          machine dependent constants 
   *          --------------------------- 
   * 
   *          epmach is the largest relative spacing. 
   *          uflow is the smallest positive magnitude. 
   * 
   ****first executable statement  dqk21 
   */
  epmach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  n1 = 1;
  n21 = 21;
  /* 
   */
  centr = (*a + *b) * .5;
  hlgth = (*b - *a) * .5;
  dhlgth = Abs (hlgth);
  /* 
   *          compute the 21-point kronrod approximation to 
   *          the integral, and estimate the absolute error. 
   * 
   */
  for (j = 1; j <= 10; ++j)
    {
      dx = dhlgth * xgk[j - 1];
      allx[j - 1] = centr - dx;
      allx[22 - j - 1] = centr + dx;
    }
  allx[10] = centr;
  if (*vectflag)
    {
      /*vector evaluation 
       */
      *stat = (*fvect) (allx, allf, &n21);
      if (*stat != 0)
	{
	  return 0;
	}
    }
  else
    {
      /*scalar evaluation 
       */
      for (j = 1; j <= 21; ++j)
	{
	  *stat = (*fvect) (&allx[j - 1], &allf[j - 1], &n1);
	  if (*stat != 0)
	    {
	      return 0;
	    }
	}
    }
  resg = 0.;
  resk = wgk[10] * allf[10];
  *resabs = Abs (resk);
  jg = 1;
  for (j = 1; j <= 9; j += 2)
    {
      fval1 = allf[j - 1];
      fval2 = allf[22 - j - 1];
      resk += wgk[j - 1] * (fval1 + fval2);
      *resabs += wgk[j - 1] * (Abs (fval1) + Abs (fval2));
      fval1 = allf[j];
      fval2 = allf[21 - j - 1];
      fsum = fval1 + fval2;
      resk += wgk[j] * fsum;
      *resabs += wgk[j] * (Abs (fval1) + Abs (fval2));
      resg += wg[jg - 1] * fsum;
      ++jg;
    }
  reskh = resk * .5;
  *resasc = wgk[10] * (d__1 = allf[10] - reskh, Abs (d__1));
  for (j = 1; j <= 10; ++j)
    {
      *resasc +=
	wgk[j - 1] * ((d__1 = allf[j - 1] - reskh, Abs (d__1)) +
		      (d__2 = allf[22 - j - 1] - reskh, Abs (d__2)));
    }
  *result = resk * hlgth;
  *resabs *= dhlgth;
  *resasc *= dhlgth;
  *abserr = (d__1 = (resk - resg) * hlgth, Abs (d__1));
  if (*resasc != 0. && *abserr != 0.)
    {
      /*Computing MIN 
       */
      d__3 = *abserr * 200. / *resasc;
      d__1 = 1., d__2 = pow (d__3, c_b49);
      *abserr = *resasc * Min (d__1, d__2);
    }
  if (*resabs > uflow / (epmach * 50.))
    {
      /*Computing MAX 
       */
      d__1 = epmach * 50. * *resabs;
      *abserr = Max (d__1, *abserr);
    }
  return 0;
}				/* nspdqk21_ */


static int
nsp_quadpack_dqk21b (intg_f fvect, double *a, double *c__, double *result1,
		     double *abserr1, double *resasc1, double *result2,
		     double *abserr2, double *resasc2, int *vectflag,
		     int *stat)
{
  /* Initialized data */

  static double wg[5] =
    { .066671344308688137593568809893332, .149451349150580593145776339657697,
      .219086362515982043995534934228163, .269266719309996355091226921569469,
      .295524224714752870173892994651338 };
  static double xgk[11] =
    { .995657163025808080735527280689003, .973906528517171720077964012084452,
      .930157491355708226001207180059508, .865063366688984510732096688423493,
      .780817726586416897063717578345042, .679409568299024406234327365114874,
      .562757134668604683339000099272694, .433395394129247190799265943165784,
      .294392862701460198131126603103866, .14887433898163121088482600112972, 0. };
  static double wgk[11] =
    { .011694638867371874278064396062192, .03255816230796472747881897245939,
      .05475589657435199603138130024458, .07503967481091995276704314091619,
      .093125454583697605535065465083366, .109387158802297641899210590325805,
      .123491976262065851077958109831074, .134709217311473325928054001771707,
      .142775938577060080797094273138717, .147739104901338491374841515972068,
      .149445554002916905664936468389821 };

  /* System generated locals */
  double d__1, d__2, d__3;
  /* Local variables */
  double allf[42], allx[42], resg1, resg2, resk1, resk2, b;
  int j;
  double hlgth, uflow;
  int n1;
  double centr1, centr2, reskh1, reskh2;
  int jg, n42;
  double dx, epmach, dhlgth, resabs1, resabs2;

  /* same than nspdqk21 but operate on the 2 intervals: 
   *     [a,(a+b)/2] and [(a+b)/2,c] 
   * thus improving the vectorization in case it is used with a 
   * (vectorized) nsp function (the nsp function could be called with 42 
   * points instead of 21 with nspdqk21) 
   * 
   * added by Bruno Pincon 
   * 
   * 
   *          the abscissae and weights are given for the interval (-1,1). 
   *          because of symmetry only the positive abscissae and their 
   *          corresponding weights are given. 
   * 
   *          xgk    - abscissae of the 21-point kronrod rule 
   *                   xgk(2), xgk(4), ...  abscissae of the 10-point 
   *                   gauss rule 
   *                   xgk(1), xgk(3), ...  abscissae which are optimally 
   *                   added to the 10-point gauss rule 
   * 
   *          wgk    - weights of the 21-point kronrod rule 
   * 
   *          wg     - weights of the 10-point gauss rule 
   * 
   * 
   *gauss quadrature weights and kronrod quadrature abscissae and weights 
   *as evaluated with 80 decimal digit arithmetic by l. w. fullerton, 
   *bell labs, nov. 1981. 
   * 
   */
  /* 
   * 
   * 
   */
  epmach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  n1 = 1;
  n42 = 42;
  /* 
   */
  b = (*a + *c__) * .5;
  centr1 = (*a + b) * .5;
  hlgth = (b - *a) * .5;
  dhlgth = Abs (hlgth);
  centr2 = (b + *c__) * .5;
  /* 
   *          compute the 21-point kronrod approximation to 
   *          the integral, and estimate the absolute error. 
   * 
   */
  for (j = 1; j <= 10; ++j)
    {
      dx = dhlgth * xgk[j - 1];
      allx[j - 1] = centr1 - dx;
      allx[22 - j - 1] = centr1 + dx;
      allx[j + 20] = centr2 + dx;
      allx[43 - j - 1] = centr2 - dx;
    }
  allx[10] = centr1;
  allx[31] = centr2;
  if (*vectflag)
    {
      /*vector evaluation 
       */
      *stat = (*fvect) (allx, allf, &n42);
      if (*stat != 0)
	{
	  return 0;
	}
    }
  else
    {
      /*scalar evaluation 
       */
      for (j = 1; j <= 42; ++j)
	{
	  *stat = (*fvect) (&allx[j - 1], &allf[j - 1], &n1);
	  if (*stat != 0)
	    {
	      return 0;
	    }
	}
    }
  resg1 = 0.;
  resg2 = 0.;
  resk1 = wgk[10] * allf[10];
  resk2 = wgk[10] * allf[31];
  resabs1 = Abs (resk1);
  resabs2 = Abs (resk2);
  jg = 1;
  for (j = 1; j <= 9; j += 2)
    {
      resk1 += wgk[j - 1] * (allf[j - 1] + allf[22 - j - 1]);
      resabs1 +=
	wgk[j - 1] * ((d__1 = allf[j - 1], Abs (d__1)) +
		      (d__2 = allf[22 - j - 1], Abs (d__2)));
      resk1 += wgk[j] * (allf[j] + allf[21 - j - 1]);
      resabs1 +=
	wgk[j] * ((d__1 = allf[j], Abs (d__1)) +
		  (d__2 = allf[21 - j - 1], Abs (d__2)));
      resg1 += wg[jg - 1] * (allf[j] + allf[21 - j - 1]);
      resk2 += wgk[j - 1] * (allf[j + 20] + allf[43 - j - 1]);
      resabs2 +=
	wgk[j - 1] * ((d__1 = allf[j + 20], Abs (d__1)) +
		      (d__2 = allf[43 - j - 1], Abs (d__2)));
      resk2 += wgk[j] * (allf[j + 21] + allf[42 - j - 1]);
      resabs2 +=
	wgk[j] * ((d__1 = allf[j + 21], Abs (d__1)) +
		  (d__2 = allf[42 - j - 1], Abs (d__2)));
      resg2 += wg[jg - 1] * (allf[j + 21] + allf[42 - j - 1]);
      ++jg;
    }
  reskh1 = resk1 * .5;
  reskh2 = resk2 * .5;
  *resasc1 = wgk[10] * (d__1 = allf[10] - reskh1, Abs (d__1));
  *resasc2 = wgk[10] * (d__1 = allf[31] - reskh2, Abs (d__1));
  for (j = 1; j <= 10; ++j)
    {
      *resasc1 +=
	wgk[j - 1] * ((d__1 = allf[j - 1] - reskh1, Abs (d__1)) +
		      (d__2 = allf[22 - j - 1] - reskh1, Abs (d__2)));
      *resasc2 +=
	wgk[j - 1] * ((d__1 = allf[j + 20] - reskh2, Abs (d__1)) +
		      (d__2 = allf[43 - j - 1] - reskh2, Abs (d__2)));
    }
  *result1 = resk1 * hlgth;
  resabs1 *= dhlgth;
  *resasc1 *= dhlgth;
  *abserr1 = (d__1 = (resk1 - resg1) * hlgth, Abs (d__1));
  if (*resasc1 != 0. && *abserr1 != 0.)
    {
      /*Computing MIN 
       */
      d__3 = *abserr1 * 200. / *resasc1;
      d__1 = 1., d__2 = pow (d__3, c_b49);
      *abserr1 = *resasc1 * Min (d__1, d__2);
    }
  if (resabs1 > uflow / (epmach * 50.))
    {
      /*Computing MAX 
       */
      d__1 = epmach * 50. * resabs1;
      *abserr1 = Max (d__1, *abserr1);
    }
  *result2 = resk2 * hlgth;
  resabs2 *= dhlgth;
  *resasc2 *= dhlgth;
  *abserr2 = (d__1 = (resk2 - resg2) * hlgth, Abs (d__1));
  if (*resasc2 != 0. && *abserr2 != 0.)
    {
      /*Computing MIN 
       */
      d__3 = *abserr2 * 200. / *resasc2;
      d__1 = 1., d__2 = pow (d__3, c_b49);
      *abserr2 = *resasc2 * Min (d__1, d__2);
    }
  if (resabs2 > uflow / (epmach * 50.))
    {
      /*Computing MAX 
       */
      d__1 = epmach * 50. * resabs2;
      *abserr2 = Max (d__1, *abserr2);
    }
  return 0;
}				/* nspdqk21b_ */




static int
nsp_quadpack_dqk15i (intg_f fvect, double *boun, int *inf, double *a, 
		     double *b, double *result, double *abserr, 
		     double *resabs, double *resasc, int *vectflag, int *stat)
{
  /* Initialized data */

  static double wg[8] =
    { 0., .129484966168869693270611432679082, 0.,
      .27970539148927666790146777142378, 0., .381830050505118944950369775488975, 0.,
      .417959183673469387755102040816327 };
  static double xgk[8] =
    { .991455371120812639206854697526329, .949107912342758524526189684047851,
      .864864423359769072789712788640926, .741531185599394439863864773280788,
      .58608723546769113029414483825873, .405845151377397166906606412076961,
      .207784955007898467600689403773245, 0. };
  static double wgk[8] =
    { .02293532201052922496373200805897, .063092092629978553290700663189204,
      .104790010322250183839876322541518, .140653259715525918745189590510238,
      .16900472663926790282658342659855, .190350578064785409913256402421014,
      .204432940075298892414161999234649, .209482141084727828012999174891714 };

  /* System generated locals */
  int i__1;
  double d__1, d__2, d__3;

  /* Builtin functions */

  /* Local variables */
  double absc, allf[30], dinf, resg, allx[30], resk, fsum, absc1[7], absc2[7];
  int j;
  double hlgth, centr, reskh, uflow;
  int n1, nb;
  double epmach;

  /****begin prologue  dqk15i 
   ****date written   800101   (yymmdd) 
   ****revision date  830518   (yymmdd) 
   ****category no.  h2a3a2,h2a4a2 
   ****keywords  15-point transformed gauss-kronrod rules 
   ****author  piessens,robert,appl. math. & progr. div. - k.u.leuven 
   *          de doncker,elise,appl. math. & progr. div. - k.u.leuven 
   * 
   ****modified by bruno pincon for nsp. The call to the integrand function 
   *           take a different form and when vectflag is TRUE only one call 
   *           is done over all the integration points. The last added var 
   *           stat lets to manage problem when the evaluation of fvect 
   *           by the nsp interpretor failed. 
   * 
   *c***purpose  the original (infinite integration range is mapped 
   *           onto the interval (0,1) and (a,b) is a part of (0,1). 
   *           it is the purpose to compute 
   *           i = integral of transformed integrand over (a,b), 
   *           j = integral of Abs(transformed integrand) over (a,b). 
   ****description 
   * 
   *          integration rule 
   *          standard fortran subroutine 
   *          double precision version 
   * 
   *          parameters 
   *           on entry 
   *             fvect    function subprogram defining the integrand 
   *                      function. Have the form: 
   *                        stat = fvect(allx, allf,n) 
   *                      and return an int (stat) which is 0 
   *                      if the evaluation of fvect (generally by the 
   *                      nsp interpretor) is OK. Other value imply an 
   *                      immediate return to the caller. allx is a 
   *                      vector of length n and the function must 
   *                      compute: 
   *                        allf(i) = f(allx(i)) for  1 <= i <= n. 
   * 
   *             boun   - double precision 
   *                      finite bound of original integration 
   *                      range (set to zero if inf = +2) 
   * 
   *             inf    - int 
   *                      if inf = -1, the original interval is 
   *                                  (-infinity,bound), 
   *                      if inf = +1, the original interval is 
   *                                  (bound,+infinity), 
   *                      if inf = +2, the original interval is 
   *                                  (-infinity,+infinity) and 
   *                      the integral is computed as the sum of two 
   *                      integrals, one over (-infinity,0) and one over 
   *                      (0,+infinity). 
   * 
   *             a      - double precision 
   *                      lower limit for integration over subrange 
   *                      of (0,1) 
   * 
   *             b      - double precision 
   *                      upper limit for integration over subrange 
   *                      of (0,1) 
   * 
   *          vectflag  - boolean true if fvect could be called on a 
   *                      vector x. This is useful when the fvect is 
   *                      coded in nsp language to speed up the computation. 
   * 
   * 
   * 
   *           on return 
   *             result - double precision 
   *                      approximation to the integral i 
   *                      result is computed by applying the 15-point 
   *                      kronrod rule(resk) obtained by optimal addition 
   *                      of abscissae to the 7-point gauss rule(resg). 
   * 
   *             abserr - double precision 
   *                      estimate of the modulus of the absolute error, 
   *                      which should equal or exceed Abs(i-result) 
   * 
   *             resabs - double precision 
   *                      approximation to the integral j 
   * 
   *             resasc - double precision 
   *                      approximation to the integral of 
   *                      Abs((transformed integrand)-i/(b-a)) over (a,b) 
   * 
   *               stat - error control for fvect evaluation. Is set 
   *                      by the fvect code which must return 0 if the evaluation 
   *                      of fvect is OK (any other values implies immediate 
   *                      return and stopping of the integration procedure) 
   * 
   ****references  (none) 
   ****routines called  dlamch 
   ****end prologue  dqk15i 
   * 
   * 
   * 
   *          the abscissae and weights are supplied for the interval 
   *          (-1,1).  because of symmetry only the positive abscissae and 
   *          their corresponding weights are given. 
   * 
   *          xgk    - abscissae of the 15-point kronrod rule 
   *                   xgk(2), xgk(4), ... abscissae of the 7-point 
   *                   gauss rule 
   *                   xgk(1), xgk(3), ...  abscissae which are optimally 
   *                   added to the 7-point gauss rule 
   * 
   *          wgk    - weights of the 15-point kronrod rule 
   * 
   *          wg     - weights of the 7-point gauss rule, corresponding 
   *                   to the abscissae xgk(2), xgk(4), ... 
   *                   wg(1), wg(3), ... are set to zero. 
   * 
   */
  /* 
   * 
   * 
   * 
   *          list of major variables 
   *          ----------------------- 
   * 
   *          centr  - mid point of the interval 
   *          hlgth  - half-length of the interval 
   *          absc*  - abscissa 
   *          tabsc* - transformed abscissa 
   *          fval*  - function value 
   *          resg   - result of the 7-point gauss formula 
   *          resk   - result of the 15-point kronrod formula 
   *          reskh  - approximation to the mean value of the transformed 
   *                   integrand over (a,b), i.e. to i/(b-a) 
   * 
   *          machine dependent constants 
   *          --------------------------- 
   * 
   *          epmach is the largest relative spacing. 
   *          uflow is the smallest positive magnitude. 
   * 
   ****first executable statement  dqk15i 
   */
  epmach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  dinf = (double) Min (1, *inf);
  if (*inf == 2)
    {
      nb = 30;
    }
  else
    {
      nb = 15;
    }
  n1 = 1;
  /* 
   *    compute all integration points 
   * 
   */
  centr = (*a + *b) * .5;
  hlgth = (*b - *a) * .5;
  allx[7] = *boun + dinf * (1. - centr) / centr;
  for (j = 1; j <= 7; ++j)
    {
      absc = hlgth * xgk[j - 1];
      absc1[j - 1] = centr - absc;
      absc2[j - 1] = centr + absc;
      allx[j - 1] = *boun + dinf * (1. - absc1[j - 1]) / absc1[j - 1];
      allx[16 - j - 1] = *boun + dinf * (1. - absc2[j - 1]) / absc2[j - 1];
    }
  if (*inf == 2)
    {
      for (j = 1; j <= 15; ++j)
	{
	  allx[j + 14] = -allx[j - 1];
	}
    }
  /* 
   *    compute f on the integration points 
   * 
   */
  if (*vectflag)
    {
      *stat = (*fvect) (allx, allf, &nb);
      if (*stat != 0)
	{
	  return 0;
	}
    }
  else
    {
      i__1 = nb;
      for (j = 1; j <= i__1; ++j)
	{
	  *stat = (*fvect) (&allx[j - 1], &allf[j - 1], &n1);
	  if (*stat != 0)
	    {
	      return 0;
	    }
	}
    }
  if (*inf == 2)
    {
      for (j = 1; j <= 15; ++j)
	{
	  allf[j - 1] += allf[j + 14];
	}
    }
  /* 
   *    compute the integral approximation and estimates 
   * 
   */
  allf[7] = allf[7] / centr / centr;
  resg = wg[7] * allf[7];
  resk = wgk[7] * allf[7];
  *resabs = Abs (resk);
  for (j = 1; j <= 7; ++j)
    {
      allf[j - 1] = allf[j - 1] / absc1[j - 1] / absc1[j - 1];
      allf[16 - j - 1] = allf[16 - j - 1] / absc2[j - 1] / absc2[j - 1];
      fsum = allf[j - 1] + allf[16 - j - 1];
      resg += wg[j - 1] * fsum;
      resk += wgk[j - 1] * fsum;
      *resabs +=
	wgk[j - 1] * ((d__1 = allf[j - 1], Abs (d__1)) +
		      (d__2 = allf[16 - j - 1], Abs (d__2)));
    }
  reskh = resk * .5;
  *resasc = wgk[7] * (d__1 = allf[7] - reskh, Abs (d__1));
  for (j = 1; j <= 7; ++j)
    {
      *resasc +=
	wgk[j - 1] * ((d__1 = allf[j - 1] - reskh, Abs (d__1)) +
		      (d__2 = allf[16 - j - 1] - reskh, Abs (d__2)));
    }
  *result = resk * hlgth;
  *resasc *= hlgth;
  *resabs *= hlgth;
  *abserr = (d__1 = (resk - resg) * hlgth, Abs (d__1));
  if (*resasc != 0. && *abserr != 0.)
    {
      /*Computing MIN 
       */
      d__3 = *abserr * 200. / *resasc;
      d__1 = 1., d__2 = pow (d__3, c_b49);
      *abserr = *resasc * Min (d__1, d__2);
    }
  if (*resabs > uflow / (epmach * 50.))
    {
      /*Computing MAX 
       */
      d__1 = epmach * 50. * *resabs;
      *abserr = Max (d__1, *abserr);
    }
  return 0;
}				/* nspdqk15i_ */
