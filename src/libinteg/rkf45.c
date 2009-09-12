#include <nsp/math.h>
#include "integ.h" 
#include "rk.h"

static const double c_b17 = .2;
static const int c__2 = 2;

static int rk_rkfs (ode_f fydot, int *neqn, double *y, double *t, double *tout,
		    double *rerr, double *aerr__, int *iflag, double *yp, double *h__,
		    double *f1, double *f2, double *f3, double *f4, double *f5,
		    double *savre, double *savae, double *savey, int *nfe, int *kop,
		    int *init, int *jflag, int *kflag,void *param);
static int rk_fehl (ode_f fydot, int *neqn, double *y, double *t, double *h__, double *yp,
		    double *f1, double *f2, double *f3, double *f4, double *f5,
		    double *s, double *savey, void *param);

/* 
 *    fehlberg fourth-fifth order runge-kutta method 
 * 
 *    written by h.a.watts and l.f.shampine 
 *                  sandia laboratories 
 *                 albuquerque,new mexico 
 * 
 *   rkf45 is primarily designed to solve non-stiff and mildly stiff 
 *   differential equations when derivative evaluations are inexpensive. 
 *   rkf45 should generally not be used when the user is demanding 
 *   high accuracy. 
 * 
 *abstract 
 * 
 *   subroutine  rkf45  integrates a system of neqn first order 
 *   ordinary differential equations of the form 
 *            dy(i)/dt = fydot(t,y(1),y(2),...,y(neqn)) 
 *             where the y(i) are given at t . 
 *   typically the subroutine is used to integrate from t to tout but it 
 *   can be used as a one-step integrator to advance the solution a 
 *   single step in the direction of tout.  on return the parameters in 
 *   the call list are set for continuing the integration. the user has 
 *   only to call rkf45 again (and perhaps define a new value for tout). 
 *   actually, rkf45 is an interfacing routine which calls subroutine 
 *   rkfs for the solution.  rkfs in turn calls subroutine  fehl which 
 *   computes an approximate solution over one step. 
 * 
 *   rkf45  uses the runge-kutta-fehlberg (4,5)  method described 
 *   in the reference 
 *   e.fehlberg , low-order classical runge-kutta formulas with stepsize 
 *                control , nasa tr r-315 
 * 
 *   the performance of rkf45 is illustrated in the reference 
 *   l.f.shampine,h.a.watts,s.davenport, solving non-stiff ordinary 
 *                differential equations-the state of the art , 
 *                sandia laboratories report sand75-0182 , 
 *                to appear in siam review. 
 * 
 * 
 *   the parameters represent- 
 *     fydot -- subroutine fydot(neqn,t,y,yp) to evaluate derivatives 
 *            yp(i)=dy(i)/dt 
 *     neqn -- number of equations to be integrated 
 *     y(*) -- solution vector at t 
 *     t -- independent variable 
 *     tout -- output point at which solution is desired 
 *     rerr,aerr -- relative and absolute error tolerances for local 
 *           error test. at each step the code requires that 
 *                Abs(local error) .le. rerr*abs(y) + aerr 
 *           for each component of the local error and solution vectors 
 *     iflag -- indicator for status of integration 
 *     work(*) -- array to hold information internal to rkf45 which is 
 *           necessary for subsequent calls. must be dimensioned 
 *           at least  3+6*neqn 
 *     iwork(*) -- int array used to hold information internal to 
 *           rkf45 which is necessary for subsequent calls. must be 
 *           dimensioned at least  5 
 * 
 * 
 * first call to rkf45 
 * 
 *   the user must provide storage in his calling program for the arrays 
 *   in the call list  -      y(neqn) , work(3+6*neqn) , iwork(5)  , 
 *   declare fydot in an external statement, supply 
 *   subroutine fydot(neqn,t,y,yp) 
 *   and initialize the following parameters- 
 * 
 *     neqn -- number of equations to be integrated.  (neqn .ge. 1) 
 *     y(*) -- vector of initial conditions 
 *     t -- starting point of integration , must be a variable 
 *     tout -- output point at which solution is desired. 
 *           t=tout is allowed on the first call only, in which case 
 *           rkf45 returns with iflag=2 if continuation is possible. 
 *     rerr,aerr -- relative and absolute local error tolerances 
 *           which must be non-negative. rerr must be a variable while 
 *           aerr may be a constant. the code should normally not be 
 *           used with relative error control smaller than about 1.e-8 . 
 *           to avoid limiting precision difficulties the code requires 
 *           rerr to be larger than an internally computed relative 
 *           error parameter which is machine dependent. in particular, 
 *           pure absolute error is not permitted. if a smaller than 
 *           allowable value of rerr is attempted, rkf45 increases 
 *           rerr appropriately and returns control to the user before 
 *           continuing the integration. 
 *     iflag -- +1,-1  indicator to initialize the code for each new 
 *           problem. normal input is +1. the user should set iflag=-1 
 *           only when one-step integrator control is essential. in this 
 *           case, rkf45 attempts to advance the solution a single step 
 *           in the direction of tout each time it is called. since this 
 *           mode of operation results in extra computing overhead, it 
 *           should be avoided unless needed. 
 * 
 * 
 * output from rkf45 
 * 
 *     y(*) -- solution at t 
 *     t -- last point reached in integration. 
 *     iflag = 2 -- integration reached tout. indicates successful retur 
 *                  and is the normal mode for continuing integration. 
 *           =-2 -- a single successful step in the direction of tout 
 *                  has been taken. normal mode for continuing 
 *                  integration one step at a time. 
 *           = 3 -- integration was not completed because relative error 
 *                  tolerance was too small. rerr has been increased 
 *                  appropriately for continuing. 
 *           = 4 -- integration was not completed because more than 
 *                  3000 derivative evaluations were needed. this 
 *                  is approximately 500 steps. 
 *           = 5 -- integration was not completed because solution 
 *                  vanished making a pure relative error test 
 *                  impossible. must use non-zero aerr to continue. 
 *                  using the one-step integration mode for one step 
 *                  is a good way to proceed. 
 *           = 6 -- integration was not completed because requested 
 *                  accuracy could not be achieved using smallest 
 *                  allowable stepsize. user must increase the error 
 *                  tolerance before continued integration can be 
 *                  attempted. 
 *           = 7 -- it is likely that rkf45 is inefficient for solving 
 *                  this problem. too much output is restricting the 
 *                  natural stepsize choice. use the one-step integrator 
 *                  mode. 
 *           = 8 -- invalid input parameters 
 *                  this indicator occurs if any of the following is 
 *                  satisfied -   neqn .le. 0 
 *                                t=tout  and  iflag .ne. +1 or -1 
 *                                rerr or aerr .lt. 0. 
 *                                iflag .eq. 0  or  .lt. -2  or  .gt. 8 
 *     work(*),iwork(*) -- information which is usually of no interest 
 *                  to the user but necessary for subsequent calls. 
 *                  work(1),...,work(neqn) contain the first derivatives 
 *                  of the solution vector y at t. work(neqn+1) contains 
 *                  the stepsize h to be attempted on the next step. 
 *                  iwork(1) contains the derivative evaluation counter. 
 * 
 * 
 * subsequent calls to rkf45 
 * 
 *   subroutine rkf45 returns with all information needed to continue 
 *   the integration. if the integration reached tout, the user need onl 
 *   define a new tout and call rkf45 again. in the one-step integrator 
 *   mode (iflag=-2) the user must keep in mind that each step taken is 
 *   in the direction of the current tout. upon reaching tout (indicated 
 *   by changing iflag to 2),the user must then define a new tout and 
 *   reset iflag to -2 to continue in the one-step integrator mode. 
 * 
 *   if the integration was not completed but the user still wants to 
 *   continue (iflag=3,4 cases), he just calls rkf45 again. with iflag=3 
 *   the rerr parameter has been adjusted appropriately for continuing 
 *   the integration. in the case of iflag=4 the function counter will 
 *   be reset to 0 and another 3000 function evaluations are allowed. 
 * 
 *   however,in the case iflag=5, the user must first alter the error 
 *   criterion to use a positive value of aerr before integration can 
 *   proceed. if he does not,execution is terminated. 
 * 
 *   also,in the case iflag=6, it is necessary for the user to reset 
 *   iflag to 2 (or -2 when the one-step integration mode is being used) 
 *   as well as increasing either aerr,rerr or both before the 
 *   integration can be continued. if this is not done, execution will 
 *   be terminated. the occurrence of iflag=6 indicates a trouble spot 
 *   (solution is changing rapidly,singularity may be present) and it 
 *   often is inadvisable to continue. 
 * 
 *   if iflag=7 is encountered, the user should use the one-step 
 *   integration mode with the stepsize determined by the code or 
 *   consider switching to the adams codes de/step,intrp. if the user 
 *   insists upon continuing the integration with rkf45, he must reset 
 *   iflag to 2 before calling rkf45 again. otherwise,execution will be 
 *   terminated. 
 * 
 *   if iflag=8 is obtained, integration can not be continued unless 
 *   the invalid input parameters are corrected. 
 * 
 *   it should be noted that the arrays work,iwork contain information 
 *   required for subsequent integration. accordingly, work and iwork 
 *   should not be altered. 
 * 
 * 
 * 
 * 
 */

int rk_rkf45 (ode_f fydot, int *neqn, double *y, double *t, double *tout,
	      int *itol, double *rerr, double *aerr__, int *itask, int *iflag,
	      int *iopt, double *work, int *lrw, int *iwork, int *liw,
	      double *bjac, int *mf, void *param)
{
  double theniflag;
  int k1, k2, k3, k4, k5, k6, k7, k1m;

  /* Parameter adjustments */
  --y;
  --work;
  --iwork;

  /* Function Body */
  if (*itask == 1)
    {
      theniflag = 1.;
    }
  if (*itask == 2)
    {
      theniflag = -1.;
    }
  /* 
   * 
   *    compute indices for the splitting of the work array 
   * 
   */
  k1m = *neqn + 1;
  k1 = k1m + 1;
  k2 = k1 + *neqn;
  k3 = k2 + *neqn;
  k4 = k3 + *neqn;
  k5 = k4 + *neqn;
  k6 = k5 + *neqn;
  k7 = k6 + *neqn;
  /* 
   *    this interfacing routine merely relieves the user of a long 
   *    calling list via the splitting apart of two working storage 
   *    arrays. if this is not compatible with the users compiler, 
   *    he must use rkfs directly. 
   * 
   */
  rk_rkfs ( fydot, neqn, &y[1], t, tout, rerr, aerr__, iflag, &work[1],
	    &work[k1m], &work[k1], &work[k2], &work[k3], &work[k4], &work[k5],
	    &work[k6], &work[k6 + 1], &work[k7], &iwork[1], &iwork[2],
	    &iwork[3], &iwork[4], &iwork[5],param);
  /* 
   */
  return 0;
}		

/* 
 *    fehlberg fourth-fifth order runge-kutta method 
 * 
 * 
 *    rkfs integrates a system of first order ordinary differential 
 *    equations as described in the comments for rkf45 . 
 *    the arrays yp,f1,f2,f3,f4,and f5 (of dimension at least neqn) and 
 *    the variables h,savre,savae,nfe,kop,init,jflag,and kflag are used 
 *    internally by the code and appear in the call list to eliminate 
 *    local retention of variables between calls. accordingly, they 
 *    should not be altered. items of possible interest are 
 *        yp - derivative of solution vector at t 
 *        h  - an appropriate stepsize to be used for the next step 
 *        nfe- counter on the number of derivative function evaluations 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * remin is the minimum acceptable value of rerr.  attempts 
 * to obtain higher accuracy with this subroutine are usually 
 * very expensive and often unsuccessful. 
 * 
 */

static int rk_rkfs (ode_f fydot, int *neqn, double *y, double *t, double *tout,
		    double *rerr, double *aerr__, int *iflag, double *yp, double *h__,
		    double *f1, double *f2, double *f3, double *f4, double *f5,
		    double *savre, double *savae, double *savey, int *nfe, int *kop,
		    int *init, int *jflag, int *kflag, void *param)
{
  static double remin = 1e-12;
  static int maxnfe = 3000;

  int i__1;
  double d__1, d__2, d__3, d__4;
  double hmin, toln, a;
  int k;
  double s, scale;
  int mflag;
  double eeoet, ae, ee;
  int hfaild;
  double dt, et;
  double u26, esttol, twoeps;
  int output;
  double rer, tol, ypk;

  /* Parameter adjustments */
  --f5;
  --f4;
  --f3;
  --f2;
  --f1;
  --yp;
  --y;
  --savey;

  /* 
   * 
   *    the expense is controlled by restricting the number 
   *    of function evaluations to be approximately maxnfe. 
   *    as set, this corresponds to about 500 steps. 
   * 
   * 
   *  here two constants emboding the machine epsilon is present 
   *  twoesp is set to twice the machine epsilon while u26 is set 
   *  to 26 times the machine epsilon 
   * 
   *    data twoeps, u26/4.4d-16, 5.72d-15/ 
   */
  twoeps = nsp_dlamch ("p") * 2.;
  u26 = twoeps * 13.;
  /* 
   * 
   *    check input parameters 
   * 
   * 
   */
  if (*neqn < 1)
    {
      goto L10;
    }
  if (*rerr < 0. || *aerr__ < 0.)
    {
      goto L10;
    }
  mflag = Abs (*iflag);
  if (mflag >= 1 && mflag <= 8)
    {
      goto L20;
    }
  /* 
   *    invalid input 
   */
 L10:
  *iflag = 8;
  return 0;
  /* 
   *    is this the first call 
   */
 L20:
  if (mflag == 1)
    {
      goto L50;
    }
  /* 
   *    check continuation possibilities 
   * 
   */
  if (*t == *tout && *kflag != 3)
    {
      goto L10;
    }
  if (mflag != 2)
    {
      goto L25;
    }
  /* 
   *    iflag = +2 or -2 
   */
  if (*kflag == 3)
    {
      goto L45;
    }
  if (*init == 0)
    {
      goto L45;
    }
  if (*kflag == 4)
    {
      goto L40;
    }
  if (*kflag == 5 && *aerr__ == 0.)
    {
      goto L30;
    }
  if (*kflag == 6 && *rerr <= *savre && *aerr__ <= *savae)
    {
      goto L30;
    }
  goto L50;
  /* 
   *    iflag = 3,4,5,6,7 or 8 
   */
 L25:
  if (*iflag == 3)
    {
      goto L45;
    }
  if (*iflag == 4)
    {
      goto L40;
    }
  if (*iflag == 5 && *aerr__ > 0.)
    {
      goto L45;
    }
  /* 
   *    integration cannot be continued since user did not respond to 
   *    the instructions pertaining to iflag=5,6,7 or 8 
   */
 L30:
  /* s_stop ("", 0L); */
  return 0;
  /* 
   *    reset function evaluation counter 
   */
 L40:
  *nfe = 0;
  if (mflag == 2)
    {
      goto L50;
    }
  /* 
   *    reset flag value from previous call 
   */
 L45:
  *iflag = *jflag;
  if (*kflag == 3)
    {
      mflag = Abs (*iflag);
    }
  /* 
   *    save input iflag and set continuation flag value for subsequent 
   *    input checking 
   */
 L50:
  *jflag = *iflag;
  *kflag = 0;
  /* 
   *    save rerr and aerr for checking input on subsequent calls 
   */
  *savre = *rerr;
  *savae = *aerr__;
  /* 
   *    restrict relative error tolerance to be at least as large as 
   *    2*eps+remin to avoid limiting precision difficulties arising 
   *    from impossible accuracy requests 
   * 
   */
  rer = twoeps + remin;
  if (*rerr >= rer)
    {
      goto L55;
    }
  /* 
   *    relative error tolerance too small 
   */
  *rerr = rer;
  *iflag = 3;
  *kflag = 3;
  return 0;
  /* 
   */
 L55:
  dt = *tout - *t;
  /* 
   */
  if (mflag == 1)
    {
      goto L60;
    }
  if (*init == 0)
    {
      goto L65;
    }
  goto L80;
  /* 
   *    initialization -- 
   *                      set initialization completion indicator,init 
   *                      set indicator for too many output points,kop 
   *                      evaluate initial derivatives 
   *                      set counter for function evaluations,nfe 
   *                      evaluate initial derivatives 
   *                      set counter for function evaluations,nfe 
   *                      estimate starting stepsize 
   * 
   */
 L60:
  *init = 0;
  *kop = 0;
  /* 
   */
  a = *t;
  (*fydot) (neqn, &a, &y[1], &yp[1],param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  *nfe = 1;
  if (*t != *tout)
    {
      goto L65;
    }
  *iflag = 2;
  return 0;
  /* 
   * 
   */
 L65:
  *init = 1;
  *h__ = Abs (dt);
  toln = 0.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      tol = *rerr * (d__1 = y[k], Abs (d__1)) + *aerr__;
      if (tol <= 0.)
	{
	  goto L70;
	}
      toln = tol;
      ypk = (d__1 = yp[k], Abs (d__1));
      /*Computing 5th power 
       */
      d__1 = *h__, d__2 = d__1, d__1 *= d__1;
      if (ypk * (d__2 * (d__1 * d__1)) > tol)
	{
	  d__3 = tol / ypk;
	  *h__ = pow (d__3,c_b17);
	}
    L70:
      ;
    }
  if (toln <= 0.)
    {
      *h__ = 0.;
    }
  /*Computing MAX 
   *Computing MAX 
   */
  d__3 = Abs (*t), d__4 = Abs (dt);
  d__1 = *h__, d__2 = u26 * Max (d__3, d__4);
  *h__ = Max (d__1, d__2);
  *jflag = i_sign (&c__2, iflag);
  /* 
   * 
   *    set stepsize for integration in the direction from t to tout 
   * 
   */
 L80:
  *h__ = d_sign (h__, &dt);
  /* 
   *    test to see if rkf45 is being severely impacted by too many 
   *    output points 
   * 
   */
  if (Abs (*h__) >= Abs (dt) * 2.)
    {
      ++(*kop);
    }
  if (*kop != 100)
    {
      goto L85;
    }
  /* 
   *    unnecessary frequency of output 
   */
  *kop = 0;
  *iflag = 7;
  return 0;
  /* 
   */
 L85:
  if (Abs (dt) > u26 * Abs (*t))
    {
      goto L95;
    }
  /* 
   *    if too close to output point,extrapolate and return 
   * 
   */
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L90: */
      y[k] += dt * yp[k];
    }
  a = *tout;
  (*fydot) (neqn, &a, &y[1], &yp[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++(*nfe);
  goto L300;
  /* 
   * 
   *    initialize output point indicator 
   * 
   */
 L95:
  output = FALSE;
  /* 
   *    to avoid premature underflow in the error tolerance function, 
   *    scale the error tolerances 
   * 
   */
  scale = 2. / *rerr;
  ae = scale * *aerr__;
  /* 
   * 
   *    step by step integration 
   * 
   */
 L100:
  hfaild = FALSE;
  /* 
   *    set smallest allowable stepsize 
   * 
   */
  hmin = u26 * Abs (*t);
  /* 
   *    adjust stepsize if necessary to hit the output point. 
   *    look ahead two steps to avoid drastic changes in the stepsize and 
   *    thus lessen the impact of output points on the code. 
   * 
   */
  dt = *tout - *t;
  if (Abs (dt) >= Abs (*h__) * 2.)
    {
      goto L200;
    }
  if (Abs (dt) > Abs (*h__))
    {
      goto L150;
    }
  /* 
   *    the next successful step will complete the integration to the 
   *    output point 
   * 
   */
  output = TRUE;
  *h__ = dt;
  goto L200;
  /* 
   */
 L150:
  *h__ = dt * .5;
  /* 
   * 
   * 
   *    core integrator for taking a single step 
   * 
   *    the tolerances have been scaled to avoid premature underflow in 
   *    computing the error tolerance function et. 
   *    to avoid problems with zero crossings,relative error is measured 
   *    using the average of the magnitudes of the solution at the 
   *    beginning and end of a step. 
   *    the error estimate formula has been grouped to control loss of 
   *    significance. 
   *    to distinguish the various arguments, h is not permitted 
   *    to become smaller than 26 units of roundoff in t. 
   *    practical limits on the change in the stepsize are enforced to 
   *    smooth the stepsize selection process and to avoid excessive 
   *    chattering on problems having discontinuities. 
   *    to prevent unnecessary failures, the code uses 9/10 the stepsize 
   *    it estimates will succeed. 
   *    after a step failure, the stepsize is not allowed to increase for 
   *    the next attempted step. this makes the code more efficient on 
   *    problems having discontinuities and more effective in general 
   *    since local extrapolation is being used and extra caution seems 
   *    warranted. 
   * 
   * 
   *    test number of derivative function evaluations. 
   *    if okay,try to advance the integration from t to t+h 
   * 
   */
 L200:
  if (*nfe <= maxnfe)
    {
      goto L220;
    }
  /* 
   *    too much work 
   */
  *iflag = 4;
  *kflag = 4;
  return 0;
  /* 
   *    advance an approximate solution over one step of length h 
   * 
   */
 L220:
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L33: */
      savey[k] = y[k];
    }
  rk_fehl ( fydot, neqn, &y[1], t, h__, &yp[1], &f1[1], &f2[1], &f3[1],
	    &f4[1], &f5[1], &f1[1], &savey[1],param);
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L34: */
      y[k] = savey[k];
    }
  *nfe += 5;
  /* 
   *    compute and test allowable tolerances versus local error estimates 
   *    and remove scaling of tolerances. note that relative error is 
   *    measured with respect to the average of the magnitudes of the 
   *    solution at the beginning and end of the step. 
   * 
   */
  eeoet = 0.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      et = (d__1 = savey[k], Abs (d__1)) + (d__2 = f1[k], Abs (d__2)) + ae;
      if (et > 0.)
	{
	  goto L240;
	}
      /* 
       *      inappropriate error tolerance 
       */
      *iflag = 5;
      return 0;
      /* 
       */
    L240:
      ee = (d__1 =
	    yp[k] * -2090. + (f3[k] * 21970. - f4[k] * 15048.) +
	    (f2[k] * 22528. - f5[k] * 27360.), Abs (d__1));
      /* L250: */
      /*Computing MAX 
       */
      d__1 = eeoet, d__2 = ee / et;
      eeoet = Max (d__1, d__2);
    }
  /* 
   */
  esttol = Abs (*h__) * eeoet * scale / 752400.;
  /* 
   */
  if (esttol <= 1.)
    {
      goto L260;
    }
  /* 
   * 
   *    unsuccessful step 
   *                      reduce the stepsize , try again 
   *                      the decrease is limited to a factor of 1/10 
   * 
   */
  hfaild = TRUE;
  output = FALSE;
  s = .1;
  if (esttol < 59049.)
    {
      s = .9 / pow(esttol, c_b17);
    }
  *h__ = s * *h__;
  if (Abs (*h__) > hmin)
    {
      goto L200;
    }
  /* 
   *    requested error unattainable at smallest allowable stepsize 
   */
  *iflag = 6;
  *kflag = 6;
  return 0;
  /* 
   * 
   *    successful step 
   *                       store solution at t+h 
   *                       and evaluate derivatives there 
   * 
   */
 L260:
  *t += *h__;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L270: */
      y[k] = f1[k];
    }
  a = *t;
  (*fydot) (neqn, &a, &y[1], &yp[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++(*nfe);
  /* 
   * 
   *                      choose next stepsize 
   *                      the increase is limited to a factor of 5 
   *                      if step failure has just occurred, next 
   *                         stepsize is not allowed to increase 
   * 
   */
  s = 5.;
  if (esttol > 1.889568e-4)
    {
      s = .9 / pow(esttol,c_b17);
    }
  if (hfaild)
    {
      s = Min (s, 1.);
    }
  /*Computing MAX 
   */
  d__2 = s * Abs (*h__);
  d__1 = Max (d__2, hmin);
  *h__ = d_sign (&d__1, h__);
  /* 
   *    end of core integrator 
   * 
   * 
   *    should we take another step 
   * 
   */
  if (output)
    {
      goto L300;
    }
  if (*iflag > 0)
    {
      goto L100;
    }
  /* 
   * 
   *    integration successfully completed 
   * 
   *    one-step mode 
   */
  *iflag = -2;
  return 0;
  /* 
   *    interval mode 
   */
 L300:
  *t = *tout;
  *iflag = 2;
  return 0;
  /* 
   */
}				/* rkfs_ */


/*     subroutine fehl(fydot,neqn,y,t,h,yp,f1,f2,f3,f4,f5,s) 
 * 
 *    fehlberg fourth-fifth order runge-kutta method 
 * 
 *   fehl integrates a system of neqn first order 
 *   ordinary differential equations of the form 
 *            dy(i)/dt=fydot(t,y(1),---,y(neqn)) 
 *   where the initial values y(i) and the initial derivatives 
 *   yp(i) are specified at the starting point t. fehl advances 
 *   the solution over the fixed step h and returns 
 *   the fifth order (sixth order accurate locally) solution 
 *   approximation at t+h in array s(i). 
 *   f1,---,f5 are arrays of dimension neqn which are needed 
 *   for internal storage. 
 *   the formulas have been grouped to control loss of significance. 
 *   fehl should be called with an h not smaller than 13 units of 
 *   roundoff in t so that the various independent arguments can be 
 *   distinguished. 
 * 
 * 
 * 
 * 
 */

static int rk_fehl (ode_f fydot, int *neqn, double *y, double *t, double *h__, double *yp,
		    double *f1, double *f2, double *f3, double *f4, double *f5,
		    double *s, double *savey, void *param)
{
  int i__1;
  double d__1;
  int k;
  double ch;
  --savey;
  --s;
  --f5;
  --f4;
  --f3;
  --f2;
  --f1;
  --yp;
  --y;

  /* Function Body */
  ch = *h__ / 4.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L221: */
      y[k] = savey[k] + ch * yp[k];
    }
  d__1 = *t + ch;
  (*fydot) (neqn, &d__1, &y[1], &f1[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ * 3. / 32.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L222: */
      y[k] = savey[k] + ch * (yp[k] + f1[k] * 3.);
    }
  d__1 = *t + *h__ * 3. / 8.;
  (*fydot) (neqn, &d__1, &y[1], &f2[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ / 2197.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L223: */
      y[k] =
	savey[k] + ch * (yp[k] * 1932. + (f2[k] * 7296. - f1[k] * 7200.));
    }
  d__1 = *t + *h__ * 12. / 13.;
  (*fydot) (neqn, &d__1, &y[1], &f3[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ / 4104.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L224: */
      y[k] =
	savey[k] + ch * (yp[k] * 8341. - f3[k] * 845. +
			 (f2[k] * 29440. - f1[k] * 32832.));
    }
  d__1 = *t + *h__;
  (*fydot) (neqn, &d__1, &y[1], &f4[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ / 20520.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L225: */
      y[k] =
	savey[k] + ch * (yp[k] * -6080. + (f3[k] * 9295. - f4[k] * 5643.) +
			 (f1[k] * 41040. - f2[k] * 28352.));
    }
  d__1 = *t + *h__ / 2.;
  (*fydot) (neqn, &d__1, &y[1], &f5[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /* 
   *    compute approximate solution at t+h 
   * 
   */
  ch = *h__ / 7618050.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L230: */
      s[k] =
	savey[k] + ch * (yp[k] * 902880. +
			 (f3[k] * 3855735. - f4[k] * 1371249.) +
			 (f2[k] * 3953664. + f5[k] * 277020.));
    }
  return 0;
}






