#include "nsp/math.h"


/* 
 * based on quadpack routine dqags (formerly qags).
 * The routine dqags calculates an approximation of a given definite integral
 * I = integral of /f/ over (a,b), hopefully satisfying following claim for accuracy:
 *          Abs(i-result) .le. Max(epsabs,epsrel*abs(i)). 
 * 
 * Authors:  Piessens, Robert. Applied Mathematics and Programming Division 
 *            K. U. Leuven 
 *          de Doncker, Elise. Applied Mathematics and Programming Division 
 *            K. U. Leuven 
 * 
 * Modified by Bruno Pincon for nsp: the expected prototype for the external f 
 * is of the form: int  f(double *x, double *y, int *n). A specific external 
 * named intg_func (See integ-IN.c) is used to redirect the evaluation 
 * on a nsp function which is evaluated by the nsp interpretor. 
 * 
 * The return value is 0 if no error arises in the evaluation of f (otherwise -1). 
 * dqk21 the basic integrator on an interval has been modified too 
 * (and renamed as dqk21vect) in order to make it possible to call 
 * f with a vector x with the 21 evaluation points (the result being stored in the vector y). 
 * This is the case when vectflag is TRUE (otherwise a loop with 21 iteration is used to fill 
 * the vector y). When an error occurs in the evaluation of the external, the variable 
 * stat is set to -1 and dqk21vect then dqags return immediatly, this last one setting ier to 6. 
 * 
 * calling sequence: 
 *          call dqags (f,a,b,epsabs,epsrel,alist,blist,elist,rlist,limit,iord,liord,result,abserr,ier) 
 * 
 *       parameters 
 *           f      - function subprogram defining the integrand function f(x). 
 *                    the actual name for f needs to be declared external in the driver program 
 *           a      - lower limit of integration 
 *           b      - upper limit of integration 
 *           epsabs - absolute accuracy requested 
 *           epsrel - relative accuracy requested 
 *           alist,blist,elist,rlist 
 *                  - work arrays (functions described below) 
 *           limit  - upper bound for number of subintervals 
 *           iord   - work array 
 *           liord  - length of iord (at least limit/2 + 2) 
 *           result - approximation to the integral 
 *           abserr - estimate of the modulus of the absolute error, 
 *                    which should equal or exceed Abs(i-result) 
 *           ier    - ier   = 0 normal and reliable termination of the routine. 
 *                            it is assumed that the requested  accuracy has been 
 *                            achieved. 
 *                  - ier   .ne. 0 abnormal termination of the routine. the estimates 
 *                            for integral and error are less reliable. it is assumed 
 *                            that the  requested accuracy has not been achieved. 
 *                        = 1 maximum number of subdivisions allowed has been achieved. the user can 
 *                            allow more sub divisions by increasing the dimensions of the 
 *                            work arrays work and iwork. However, this may yield no  improvement, and it 
 *                            is rather advised to have a close look at the integrand, 
 *                            in order to determine the integration  difficulties. if 
 *                            the position of a local difficulty can be determined 
 *                            (i.e.  singularity, discontinuity within the interval) one will probably 
 *                            gain from  splitting up the interval at this point and 
 *                            calling the integrator on the sub-ranges. if possible, an 
 *                            appropriate special-purpose integrator should be used 
 *                            which is designed for handling the type  of difficulty involved. 
 *                        = 2 the occurrence of roundoff error is detected which 
 *                            prevents the requested tolerance  from being 
 *                            achieved. the error may be under-estimated. 
 *                        = 3 extremely bad integrand behaviour occurs at some interior points of the 
 *                            integration interval. 
 *                        = 4 it is presumed that the requested tolerance cannot be achieved, 
 *                            and that the returned result is the best which can be obtained. 
 *                        = 5 the integral is probably divergent, or slowly convergent. it must be noted 
 *                            that divergency can occur with any other value of ier. 
 *                        = 6 an error occurs during the evaluation of f 
 *           vectflag - TRUE if the external could evaluated on a vector 
 * 
 *           the dimension of /rlist2/ is determined by 
 *           data /limexp/ in subroutine epsalg (/rlist2/ 
 *           should be of dimension (limexp+2) at least). 
 */

typedef int (*intg_f)(const double *t, double *y, int *n);

static int intg_epsalg (int *n, double *epstab, double *result, double *abserr,
			double *res3la, int *nres);

static int intg_dqk21vect (intg_f fvect, double *a, double *b, double *result,
			   double *abserr, double *resabs, double *resasc,
			   int *vectflag, int *stat);

static int intg_order (int *limit, int *last, int *maxerr, double *ermax,
		       double *elist, int *iord, int *liord, int *nrmax, int * dqa001_jupbnd);


int C2F(dqags)(intg_f f, double *a, double *b, double *epsabs, double *epsrel,
	       double *alist__, double *blist, double *elist, double *rlist,
	       int *limit, int *iord, int *liord, double *result,
	       double *abserr, int *ier, int *vectflag)
{
  int dqa001_jupbnd;
  int i__1, i__2;
  double d__1, d__2;
  double area, dres;
  int ksgn, last, nres, stat;
  double area1, area2;
  int last1;
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
  int id, numrl2;
  double rlist2[52], defabs;
  double epmach;
  double erlarg, abseps, correc, errbnd, resabs, erlast, errmax;
  int maxerr;
  double reseps;
  int extrap;
  double ertest, errsum;

  /* Parameter adjustments */
  --rlist;
  --elist;
  --blist;
  --alist__;
  --iord;

  /* Function Body */
  epmach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  oflow = nsp_dlamch ("o");
  /* 
   *           list of major variables 
   *           ----------------------- 
   * 
   *          alist     - list of left end-points of all subintervals 
   *                      considered up to now 
   * 
   *          blist     - list of right end-points of all subintervals 
   *                      considered up to now 
   * 
   *          rlist(i)  - approximation to the integral over 
   *                      (alist(i),blist(i)) 
   * 
   *          rlist2    - array of dimension at least limexp+2 
   *                      containing the part of the epsilon table 
   *                      which is still needed for further 
   *                      computations 
   * 
   *          elist(i)  - error estimate applying to rlist(i) 
   * 
   *          maxerr    - pointer to the interval with largest error 
   *                      estimate 
   * 
   *          errmax    - elist(maxerr) 
   * 
   *          erlast    - error on the interval currently subdivided 
   *                      (before that subdivision has taken place) 
   * 
   *          area      - sum of the integrals over the subintervals 
   * 
   *          errsum    - sum of the errors over the subintervals 
   * 
   *          errbnd    - requested accuracy Max(epsabs,epsrel* 
   *                      Abs(result)) 
   * 
   *          *****1    - variable for the left interval 
   * 
   *          *****2    - variable for the right interval 
   * 
   *          last      - index for subdivision 
   * 
   *          nres      - number of calls to the extrapolation routine 
   * 
   *          numrl2    - number of elements currently  in 
   *                      rlist2. if an appropriate 
   *                      approximation to the compounded 
   *                      integral has been obtained it is 
   *                      put in  rlist2(numrl2) after numrl2 
   *                      has been increased by one. 
   * 
   *          small     - length of the smallest interval considered 
   *                      up to now, multiplied by 1.5 
   * 
   *          erlarg    - sum of the errors over the intervals larger 
   *                      than the smallest interval 
   *                      considered up to now 
   *          extrap    - int variable denoting that the 
   *                      routine is attempting to perform 
   *                      extrapolation.  i.e. before 
   *                      subdividing the smallest interval 
   *                      we try to decrease the value of 
   *                      erlarg 
   *          noext     - int variable denoting that extrapolation 
   *                      is no longer allowed(/true/ value) 
   *          stat      - tell if the function evaluation f in dqk21vect 
   *                      is successful (0) or not (-1) 
   * 
   *          first approximation to the integral 
   *          ----------------------------------- 
   * 
   */
  last1 = 1;
  *ier = 0;
  ierro = 0;
  intg_dqk21vect ( f, a, b, result, abserr, &defabs, &resabs,
		   vectflag, &stat);
  if (stat != 0)
    {
      *ier = 6;
      return 0;
    }
  /* 
   *          test on accuracy 
   * 
   */
  dres = Abs (*result);
  /*Computing MAX 
   */
  d__1 = *epsabs, d__2 = *epsrel * dres;
  errbnd = Max (d__1, d__2);
  if (*abserr <= epmach * 100. * defabs && *abserr > errbnd)
    {
      *ier = 2;
    }
  if (*limit < 2 && *abserr > errbnd)
    {
      *ier = 1;
    }
  if (*ier != 0 || *abserr <= errbnd)
    {
      goto L320;
    }
  /* 
   *          initialization 
   *          -------------- 
   * 
   */
  alist__[1] = *a;
  blist[1] = *b;
  rlist[1] = *result;
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
  if (*limit < 2)
    {
      goto L220;
    }
  i__1 = *limit;
  for (last = 2; last <= i__1; ++last)
    {
      /* 
       *          bisect the subinterval with the nrmax-th largest 
       *          error estimate 
       * 
       */
      last1 = last;
      a1 = alist__[maxerr];
      b1 = (alist__[maxerr] + blist[maxerr]) * .5;
      a2 = b1;
      b2 = blist[maxerr];
      erlast = errmax;
      intg_dqk21vect ( f, &a1, &b1, &area1, &error1, &resabs, &defab1,
		       vectflag, &stat);
      if (stat != 0)
	{
	  *ier = 6;
	  return 0;
	}
      intg_dqk21vect ( f, &a2, &b2, &area2, &error2, &resabs, &defab2,
		       vectflag, &stat);
      if (stat != 0)
	{
	  *ier = 6;
	  return 0;
	}
      /* 
       *          improve previous approximation of integral 
       *          and error and test for accuracy 
       * 
       */
      area12 = area1 + area2;
      erro12 = error1 + error2;
      errsum = errsum + erro12 - errmax;
      area = area + area12 - rlist[maxerr];
      if (defab1 == error1 || defab2 == error2)
	{
	  goto L40;
	}
      if ((d__1 = rlist[maxerr] - area12, Abs (d__1)) > Abs (area12) * 1e-5
	  || erro12 < errmax * .99)
	{
	  goto L20;
	}
      if (extrap)
	{
	  ++iroff2;
	}
      if (!extrap)
	{
	  ++iroff1;
	}
    L20:
      if (last > 10 && erro12 > errmax)
	{
	  ++iroff3;
	}
    L40:
      rlist[maxerr] = area1;
      rlist[last] = area2;
      /*Computing MAX 
       */
      d__1 = *epsabs, d__2 = *epsrel * Abs (area);
      errbnd = Max (d__1, d__2);
      if (errsum <= errbnd)
	{
	  goto L280;
	}
      /* 
       *          test for roundoff error and eventually 
       *          set error flag 
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
       *          set error flag in the case that the number of interval 
       *           bisections exceeds /limit/ 
       * 
       */
      if (last == *limit)
	{
	  *ier = 1;
	}
      /* 
       *          set error flag in the case of bad integrand behaviour 
       *          at interior points of integration range 
       * 
       *Computing MAX 
       */
      d__1 = Abs (a1), d__2 = Abs (b2);
      if (Max (d__1, d__2) <= (epmach * 100. + 1.) * (Abs (a2) + uflow * 1e3))
	{
	  *ier = 4;
	}
      if (*ier != 0)
	{
	  goto L220;
	}
      /* 
       *          append the newly-created intervals to the list 
       * 
       */
      if (error2 > error1)
	{
	  goto L60;
	}
      alist__[last] = a2;
      blist[maxerr] = b1;
      blist[last] = b2;
      elist[maxerr] = error1;
      elist[last] = error2;
      goto L80;
    L60:
      alist__[maxerr] = a2;
      alist__[last] = a1;
      blist[last] = b1;
      rlist[maxerr] = area2;
      rlist[last] = area1;
      elist[maxerr] = error2;
      elist[last] = error1;
      /* 
       *          call subroutine order to maintain the 
       *          descending ordering in the list of error 
       *          estimates and select the subinterval with 
       *          nrmax-th largest error estimate (to be bisected 
       *          next) 
       * 
       */
    L80:
      intg_order (limit, &last, &maxerr, &errmax, &elist[1], &iord[1],
		  liord, &nrmax, &dqa001_jupbnd);
      if (last == 2)
	{
	  goto L180;
	}
      if (noext)
	{
	  goto L200;
	}
      erlarg -= erlast;
      if ((d__1 = b1 - a1, Abs (d__1)) > small)
	{
	  erlarg += erro12;
	}
      if (extrap)
	{
	  goto L100;
	}
      /* 
       *          test whether the interval to be bisected next is the 
       *          smallest interval 
       * 
       */
      if ((d__1 = blist[maxerr] - alist__[maxerr], Abs (d__1)) > small)
	{
	  goto L200;
	}
      extrap = TRUE;
      nrmax = 2;
    L100:
      if (ierro == 3 || erlarg <= ertest)
	{
	  goto L140;
	}
      /* 
       *          the smallest interval has the largest error. 
       *          before bisecting decrease the sum of the errors 
       *          over the larger intervals(erlarg) and perform 
       *          extrapolation 
       * 
       */
      id = nrmax;
      i__2 = dqa001_jupbnd;
      for (k = id; k <= i__2; ++k)
	{
	  maxerr = iord[nrmax];
	  errmax = elist[maxerr];
	  if ((d__1 = blist[maxerr] - alist__[maxerr], Abs (d__1)) > small)
	    {
	      goto L200;
	    }
	  ++nrmax;
	  /* L120: */
	}
      /* 
       *          perform extrapolation 
       * 
       */
    L140:
      ++numrl2;
      rlist2[numrl2 - 1] = area;
      intg_epsalg (&numrl2, rlist2, &reseps, &abseps, res3la, &nres);
      ++ktmin;
      if (ktmin > 5 && *abserr < errsum * .001)
	{
	  *ier = 5;
	}
      if (abseps >= *abserr)
	{
	  goto L160;
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
	  goto L220;
	}
      /* 
       *          prepare  bisection of the smallest interval 
       * 
       */
    L160:
      if (numrl2 == 1)
	{
	  noext = TRUE;
	}
      if (*ier == 5)
	{
	  goto L220;
	}
      maxerr = iord[1];
      errmax = elist[maxerr];
      nrmax = 1;
      extrap = FALSE ;
      small *= .5;
      erlarg = errsum;
      goto L200;
    L180:
      small = (d__1 = *b - *a, Abs (d__1)) * .375;
      erlarg = errsum;
      ertest = errbnd;
      rlist2[1] = area;
    L200:
      ;
    }
  /* 
   *          set  final result and error estimate 
   *          ------------------------------------ 
   * 
   */
 L220:
  if (*abserr == oflow)
    {
      goto L280;
    }
  if (*ier + ierro == 0)
    {
      goto L260;
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
      goto L240;
    }
  if (*abserr > errsum)
    {
      goto L280;
    }
  if (area == 0.)
    {
      goto L320;
    }
  goto L260;
 L240:
  if (*abserr / Abs (*result) > errsum / Abs (area))
    {
      goto L280;
    }
  /* 
   *          test on divergency 
   * 
   */
 L260:
  /*Computing MAX 
   */
  d__1 = Abs (*result), d__2 = Abs (area);
  if (ksgn == -1 && Max (d__1, d__2) <= defabs * .01)
    {
      goto L320;
    }
  if (.01 > *result / area || *result / area > 100. || errsum > Abs (area))
    {
      *ier = 6;
    }
  goto L320;
  /* 
   *          compute global integral sum 
   * 
   */
 L280:
  *result = 0.;
  i__1 = last;
  for (k = 1; k <= i__1; ++k)
    {
      *result += rlist[k];
      /* L300: */
    }
  *abserr = errsum;
 L320:
  if (*ier > 2)
    {
      --(*ier);
    }
  iord[1] = last1 << 2;
  return 0;
}	


/****begin prologue  dqk21vect 
 ****date written   800101   (yymmdd) 
 ****revision date  830518   (yymmdd) 
 ****category no.  h2a1a2 
 ****keywords  21-point gauss-kronrod rules 
 ****author  piessens,robert,appl. math. & progr. div. - k.u.leuven 
 *          de doncker,elise,appl. math. & progr. div. - k.u.leuven 
 ****modified by bruno pincon to call the integrand only one time 
 *           over the 21 integration points 
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
 *             fvect    subroutine subprogram defining the integrand 
 *                      function fvect(x, allf). the actual name for 
 *                      fvect needs to be declared e x t e r n a l 
 *                      in the driver program. 
 * 
 *             a      - double precision 
 *                      lower limit of integration 
 * 
 *             b      - double precision 
 *                      upper limit of integration 
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
 ****references  (none) 
 ****routines called  d1mach ** replaced by a dlamch call 
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

static double c_b4 = 1.5;

static int intg_dqk21vect (intg_f fvect, double *a, double *b, double *result,
			   double *abserr, double *resabs, double *resasc,
			   int *vectflag, int *stat)
{
  static const double wg[5] =
    { .066671344308688137593568809893332, .149451349150580593145776339657697,
      .219086362515982043995534934228163, .269266719309996355091226921569469,
      .295524224714752870173892994651338 };
  static const double xgk[11] =
    { .995657163025808080735527280689003, .973906528517171720077964012084452,
      .930157491355708226001207180059508, .865063366688984510732096688423493,
      .780817726586416897063717578345042, .679409568299024406234327365114874,
      .562757134668604683339000099272694, .433395394129247190799265943165784,
      .294392862701460198131126603103866, .14887433898163121088482600112972, 0. };
  static const double wgk[11] =
    { .011694638867371874278064396062192, .03255816230796472747881897245939,
      .05475589657435199603138130024458, .07503967481091995276704314091619,
      .093125454583697605535065465083366, .109387158802297641899210590325805,
      .123491976262065851077958109831074, .134709217311473325928054001771707,
      .142775938577060080797094273138717, .147739104901338491374841515972068,
      .149445554002916905664936468389821 };

  double d__1, d__2, d__3;
  double allf[21], resg, allx[21], resk, fsum, fval1, fval2;
  int j;
  double hlgth, centr, reskh, uflow;
  int n1, jg, n21;
  double dx, epmach, dhlgth;

  /* 
   *          list of major variables 
   *          centr  - mid point of the interval 
   *          hlgth  - half-length of the interval 
   *          fval*  - function value 
   *          resg   - result of the 10-point gauss formula 
   *          resk   - result of the 21-point kronrod formula 
   *          reskh  - approximation to the mean value of f over (a,b), 
   *                   i.e. to i/(b-a) 
   *          machine dependent constants 
   *          --------------------------- 
   *          epmach is the largest relative spacing. 
   *          uflow is the smallest positive magnitude. 
   * 
   */
  epmach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  *stat = 0;
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
      d__1 = 1., d__2 = pow (d__3,c_b4);
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
}


/* 
 *    based on quadpack routine epsalg 
 * 
 *          purpose 
 *             the routine transforms a given sequence of 
 *             approximations, by means of the epsilon 
 *             algorithm of P. Wynn. 
 * 
 *             an estimate of the absolute error is also given. 
 *             the condensed epsilon table is computed. only those 
 *             elements needed for the computation of the 
 *             next diagonal are preserved. 
 * 
 *          calling sequence 
 *             call epsalg (n,epstab,result,abserr,res3la,nres) 
 * 
 *          parameters 
 *             n      - epstab(n) contains the new element in the 
 *                      first column of the epsilon table. 
 * 
 *             epstab - one dimensional array containing the 
 *                      elements of the two lower diagonals of 
 *                      the triangular epsilon table. 
 *                      the elements are numbered starting at the 
 *                      right-hand corner of the triangle. 
 *                      the dimension should be at least n+2. 
 * 
 *             result - resulting approximation to the integral 
 * 
 *             abserr - estimate of the absolute error computed from 
 *                      result and the 3 previous /results/ 
 * 
 *             res3la - array containing the last 3 /results/ 
 * 
 *             nres   - number of calls to the routine 
 *                      (should be zero at first call) 
 * 
 *           machine dependent constants 
 *            ------------------------- 
 *           /limexp/ is the maximum number of elements the epsilon 
 *           table can contain. if this number is reached, the upper 
 *           diagonal of the epsilon table is deleted. 
 * 
 */

static int intg_epsalg (int *n, double *epstab, double *result, double *abserr,
			double *res3la, int *nres)
{
  const int limexp = 50;
  int i__1;
  double d__1, d__2, d__3;
  double e1abs;
  int i__;
  double e0, e1, e2, e3, error, oflow;
  int k1, k2, k3;
  double delta1, delta2, delta3;
  int ib, ie;
  double epmach, ss, epsinf;
  int newelm, ib2, ind;
  double res;
  int num;
  double err1, err2, err3, tol1, tol2, tol3;

  /* Parameter adjustments */
  --res3la;
  --epstab;

  /* Function Body */
  epmach = nsp_dlamch ("p");
  oflow = nsp_dlamch ("o");
  /* 
   *          list of major variables 
   *          ----------------------- 
   *          e0     - the 4 elements on which the 
   *          e1       computation of a new element in 
   *          e2       the epsilon table is based 
   *          e3                 e0 
   *                       e3    e1    new 
   *                             e2 
   *          newelm - number of elements to be computed in the new 
   *                   diagonal 
   *          error  - error = Abs(e1-e0)+abs(e2-e1)+abs(new-e2) 
   *          result - the element in the new diagonal with least 
   *                   error 
   * 
   */
  ++(*nres);
  *abserr = oflow;
  *result = epstab[*n];
  if (*n < 3)
    {
      goto L200;
    }
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
      if (err2 > tol2 || err3 > tol3)
	{
	  goto L20;
	}
      /* 
       *          if e0, e1 and e2 are equal to within machine 
       *          accuracy, convergence is assumed 
       *          result = e2 
       *          abserr = Abs(e1-e0)+abs(e2-e1) 
       * 
       */
      *result = res;
      *abserr = err2 + err3;
      goto L200;
    L20:
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
      if (err1 < tol1 || err2 < tol2 || err3 < tol3)
	{
	  goto L40;
	}
      ss = 1. / delta1 + 1. / delta2 - 1. / delta3;
      epsinf = (d__1 = ss * e1, Abs (d__1));
      /* 
       *          test to detect irregular behaviour in the table, and 
       *          eventually omit a part of the table adjusting the value 
       *          of n 
       * 
       */
      if (epsinf > 1e-4)
	{
	  goto L60;
	}
    L40:
      *n = i__ + i__ - 1;
      goto L100;
      /* 
       *          compute a new element and eventually adjust 
       *          the value of result 
       * 
       */
    L60:
      res = e1 + 1. / ss;
      epstab[k1] = res;
      k1 += -2;
      error = err2 + (d__1 = res - e2, Abs (d__1)) + err3;
      if (error > *abserr)
	{
	  goto L80;
	}
      *abserr = error;
      *result = res;
    L80:
      ;
    }
  /* 
   *          shift the table 
   * 
   */
 L100:
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
      /* L120: */
    }
  if (num == *n)
    {
      goto L160;
    }
  ind = num - *n + 1;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      epstab[i__] = epstab[ind];
      ++ind;
      /* L140: */
    }
 L160:
  if (*nres >= 4)
    {
      goto L180;
    }
  res3la[*nres] = *result;
  *abserr = oflow;
  goto L200;
  /* 
   *          compute error estimate 
   * 
   */
 L180:
  *abserr = (d__1 = *result - res3la[3], Abs (d__1)) 
    +  (d__2 = *result - res3la[2],Abs (d__2)) 
    +  (d__3 = *result - res3la[1],Abs(d__3));
  res3la[1] = res3la[2];
  res3la[2] = res3la[3];
  res3la[3] = *result;
 L200:
  /*Computing MAX 
   */
  d__1 = *abserr, d__2 = epmach * 5. * Abs (*result);
  *abserr = Max (d__1, d__2);
  return 0;
}	


/* 
 *    based on quadpack routine order 
 * 
 *    purpose 
 *             this routine maintains the descending ordering 
 *             in the list of the local error estimates 
 *             resulting from the interval subdivision 
 *             process. at each call two error estimates 
 *             are inserted using the sequential search 
 *             method . top-down for the largest error 
 *             estimate,  bottom-up for the smallest error 
 *             estimate. 
 * 
 *    calling sequence 
 *             call order(limit,last,maxerr,ermax,elist,iord,liord,nrmax) 
 * 
 *            parameters (meaning at output) 
 *             limit  - maximum number of error estimates the list can contain 
 *             last   - number of error estimates currently 
 *                      in the list. elist(last) contains the smallest error estimate. 
 *             maxerr - maxerr points to the nrmax-th largest error 
 *                      estimate currently in the list. 
 *             ermax  - nrmax-th largest error estimate ermax = elist(maxerr) 
 *             elist  - array of dimension last containing the error estimates 
 *             iord   - array containing pointers to elist so that iord(1) points to the largest 
 *                      error estimate,...,iord(last) to the smallest error estimate 
 *             liord  - dimension of iord 
 *             nrmax  - maxerr = iord(nrmax) 
 * 
 *           check whether the list contains more than 
 *           two error estimates 
 * 
 */

static int intg_order (int *limit, int *last, int *maxerr, double *ermax,
		       double *elist, int *iord, int *liord, int *nrmax, int * dqa001_jupbnd)
{
  int i__1;
  int ibeg, jbnd, i__, j, k, isucc;
  double errmin, errmax;
  int ido;

  /* Parameter adjustments */
  --elist;
  --iord;

  /* Function Body */
  if (*last > 2)
    {
      goto L20;
    }
  iord[1] = 1;
  iord[2] = 2;
  goto L180;
  /* 
   *          this part of the routine is only executed 
   *          if, due to a difficult integrand, subdivision 
   *          increased the error estimate. in the normal case 
   *          the insert procedure should start after the 
   *          nrmax-th largest error estimate. 
   * 
   */
 L20:
  errmax = elist[*maxerr];
  if (*nrmax == 1)
    {
      goto L60;
    }
  ido = *nrmax - 1;
  i__1 = ido;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      isucc = iord[*nrmax - 1];
      if (errmax <= elist[isucc])
	{
	  goto L60;
	}
      iord[*nrmax] = isucc;
      --(*nrmax);
      /* L40: */
    }
  /* 
   *          compute the number of elements in the list to 
   *          be maintained in descending order. this number 
   *          depends on the number of subdivisions still 
   *          allowed 
   * 
   */
 L60:
  *dqa001_jupbnd = *last;
  if (*last > *limit / 2 + 2)
    {
      *dqa001_jupbnd = *limit + 3 - *last;
    }
  errmin = elist[*last];
  /* 
   *          insert errmax by traversing the list top-down 
   *          starting comparison from the element 
   *          elist(iord(nrmax+1)) 
   * 
   */
  jbnd = *dqa001_jupbnd - 1;
  ibeg = *nrmax + 1;
  if (ibeg > jbnd)
    {
      goto L100;
    }
  i__1 = jbnd;
  for (i__ = ibeg; i__ <= i__1; ++i__)
    {
      isucc = iord[i__];
      if (errmax >= elist[isucc])
	{
	  goto L120;
	}
      iord[i__ - 1] = isucc;
      /* L80: */
    }
 L100:
  iord[jbnd] = *maxerr;
  iord[*dqa001_jupbnd] = *last;
  goto L180;
  /* 
   *          insert errmin by traversing the list bottom-up 
   * 
   */
 L120:
  iord[i__ - 1] = *maxerr;
  k = jbnd;
  i__1 = jbnd;
  for (j = i__; j <= i__1; ++j)
    {
      isucc = iord[k];
      if (errmin < elist[isucc])
	{
	  goto L160;
	}
      iord[k + 1] = isucc;
      --k;
      /* L140: */
    }
  iord[i__] = *last;
  goto L180;
 L160:
  iord[k + 1] = *last;
  /* 
   *          set maxerr and ermax 
   * 
   */
 L180:
  *maxerr = iord[*nrmax];
  *ermax = elist[*maxerr];
  return 0;
}

