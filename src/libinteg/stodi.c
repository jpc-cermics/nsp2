#include "integ.h"

#define ls0001_1 ls0001_._2

int
nsp_ode_stodi (int *neq, double *y, double *yh, int *nyh, double *yh1,
	       double *ewt, double *savf, double *savr, double *acor,
	       double *wm, int *iwm, lsodi_res res, ode_jac adda, Dgbydy jac,
	       Pjac pjac, Slvs slvs, void *param)
{
  /* System generated locals */
  int yh_dim1, yh_offset, i__1, i__2;
  double d__1, d__2, d__3;


  /* Local variables */
  double dcon, delp, eljh, rhdn, exdn,  dsm=0, dup, el1h,  exsm,
    rhup, exup,  r__,  rh=0, del, ddn,  told, rhsm;
  int i__, j, m,  iredo=0, i1, ires, iret;
  int jb , ncf, kgo,  newq;

  /*lll. optimize 
   *----------------------------------------------------------------------- 
   *%purpose 
   *stodi performs one step of the integration of an initial value 
   *problem for a system of ordinary differential equations. 
   *note.. stodi is independent of the value of the iteration method 
   *indicator miter, and hence is independent 
   *of the type of chord method used, or the jacobian structure. 
   *%calling sequence 
   *communication with stodi is done with the following variables.. 
   * 
   *neq    = int array containing problem size in neq(1), and 
   *         passed as the neq argument in all calls to res, adda, 
   *         and jac. 
   *y      = an array of length .ge. n used as the y argument in 
   *         all calls to res, jac, and adda. 
   *neq    = int array containing problem size in neq(1), and 
   *         passed as the neq argument in all calls to res, g, adda, 
   *         and jac 
   *yh     = an nyh by lmax array containing the dependent variables 
   *         and their approximate scaled derivatives, where 
   *         lmax = maxord + 1.  yh(i,j+1) contains the approximate 
   *         j-th derivative of y(i), scaled by h**j/factorial(j) 
   *         (j = 0,1,...,nq).  on entry for the first step, the first 
   *         two columns of yh must be set from the initial values. 
   *nyh    = a constant int .ge. n, the first dimension of yh. 
   *yh1    = a one-dimensional array occupying the same space as yh. 
   *ewt    = an array of length n containing multiplicative weights 
   *         for local error measurements.  local errors in y(i) are 
   *         compared to 1.0/ewt(i) in various error tests. 
   *savf   = an array of working storage, of length n. also used for 
   *         input of yh(*,maxord+2) when jstart = -1 and maxord is less 
   *         than the current order nq. 
   *         same as ydoti in lsodi. 
   *savr   = an array of working storage, of length n. 
   *acor   = a work array of length n used for the accumulated 
   *         corrections. on a succesful return, acor(i) contains 
   *         the estimated one-step local error in y(i). 
   *wm,iwm = real and int work arrays associated with matrix 
   *         operations in chord iteration. 
   *pjac   = name of routine to evaluate and preprocess jacobian matrix. 
   *slvs   = name of routine to solve linear system in chord iteration. 
   *ccmax  = maximum relative change in h*el0 before pjac is called. 
   *h      = the step size to be attempted on the next step. 
   *         h is altered by the error control algorithm during the 
   *         problem.  h can be either positive or negative, but its 
   *         sign must remain constant throughout the problem. 
   *hmin   = the minimum absolute value of the step size h to be used. 
   *hmxi   = inverse of the maximum absolute value of h to be used. 
   *         hmxi = 0.0 is allowed and corresponds to an infinite hmax. 
   *         hmin and hmxi may be changed at any time, but will not 
   *         take effect until the next change of h is considered. 
   *tn     = the independent variable. tn is updated on each step taken. 
   *jstart = an int used for input only, with the following 
   *         values and meanings.. 
   *              0  perform the first step. 
   *          .gt.0  take a new step continuing from the last. 
   *             -1  take the next step with a new value of h, maxord, 
   *                   n, meth, miter, and/or matrix parameters. 
   *             -2  take the next step with a new value of h, 
   *                   but with other inputs unchanged. 
   *         on return, jstart is set to 1 to facilitate continuation. 
   *kflag  = a completion code with the following meanings.. 
   *              0  the step was succesful. 
   *             -1  the requested error could not be achieved. 
   *             -2  corrector convergence could not be achieved. 
   *             -3  res ordered immediate return. 
   *             -4  error condition from res could not be avoided. 
   *             -5  fatal error in pjac or slvs. 
   *         a return with kflag = -1, -2, or -4 means either 
   *         Abs(h) = hmin or 10 consecutive failures occurred. 
   *         on a return with kflag negative, the values of tn and 
   *         the yh array are as of the beginning of the last 
   *         step, and h is the last step size attempted. 
   *maxord = the maximum order of integration method to be allowed. 
   *maxcor = the maximum number of corrector iterations allowed. 
   *msbp   = maximum number of steps between pjac calls. 
   *mxncf  = maximum number of convergence failures allowed. 
   *meth/miter = the method flags.  see description in driver. 
   *n      = the number of first-order differential equations. 
   *! 
   *----------------------------------------------------------------------- 
   */
  /* Parameter adjustments */
  --neq;
  --y;
  yh_dim1 = *nyh;
  yh_offset = yh_dim1 + 1;
  yh -= yh_offset;
  --yh1;
  --ewt;
  --savf;
  --savr;
  --acor;
  --wm;
  --iwm;

  /* Function Body */
  ls0001_1.kflag = 0;
  told = ls0001_1.tn;
  ncf = 0;
  ls0001_1.ierpj = 0;
  ls0001_1.iersl = 0;
  ls0001_1.jcur = 0;
  ls0001_1.icf = 0;
  delp = 0.;
  if (ls0001_1.jstart > 0)
    {
      goto L200;
    }
  if (ls0001_1.jstart == -1)
    {
      goto L100;
    }
  if (ls0001_1.jstart == -2)
    {
      goto L160;
    }
  /*----------------------------------------------------------------------- 
   *on the first call, the order is set to 1, and other variables are 
   *initialized.  rmax is the maximum ratio by which h can be increased 
   *in a single step.  it is initially 1.e4 to compensate for the small 
   *initial h, but then is normally equal to 10.  if a failure 
   *occurs (in corrector convergence or error test), rmax is set at 2 
   *for the next increase. 
   *----------------------------------------------------------------------- 
   */
  ls0001_1.lmax = ls0001_1.maxord + 1;
  ls0001_1.nq = 1;
  ls0001_1.l = 2;
  ls0001_1.ialth = 2;
  ls0001_1.rmax = 1e4;
  ls0001_1.rc = 0.;
  ls0001_1.el0 = 1.;
  ls0001_1.crate = .7;
  ls0001_1.hold = ls0001_1.h__;
  ls0001_1.meo = ls0001_1.meth;
  ls0001_1.nslp = 0;
  ls0001_1.ipup = ls0001_1.miter;
  iret = 3;
  goto L140;
  /*----------------------------------------------------------------------- 
   *the following block handles preliminaries needed when jstart = -1. 
   *ipup is set to miter to force a matrix update. 
   *if an order increase is about to be considered (ialth = 1), 
   *ialth is reset to 2 to postpone consideration one more step. 
   *if the caller has changed meth, cfode is called to reset 
   *the coefficients of the method. 
   *if the caller has changed maxord to a value less than the current 
   *order nq, nq is reduced to maxord, and a new h chosen accordingly. 
   *if h is to be changed, yh must be rescaled. 
   *if h or meth is being changed, ialth is reset to l = nq + 1 
   *to prevent further changes in h for that many steps. 
   *----------------------------------------------------------------------- 
   */
 L100:
  ls0001_1.ipup = ls0001_1.miter;
  ls0001_1.lmax = ls0001_1.maxord + 1;
  if (ls0001_1.ialth == 1)
    {
      ls0001_1.ialth = 2;
    }
  if (ls0001_1.meth == ls0001_1.meo)
    {
      goto L110;
    }
  nsp_ode_cfode (&ls0001_1.meth, ls0001_1.elco, ls0001_1.tesco);
  ls0001_1.meo = ls0001_1.meth;
  if (ls0001_1.nq > ls0001_1.maxord)
    {
      goto L120;
    }
  ls0001_1.ialth = ls0001_1.l;
  iret = 1;
  goto L150;
 L110:
  if (ls0001_1.nq <= ls0001_1.maxord)
    {
      goto L160;
    }
 L120:
  ls0001_1.nq = ls0001_1.maxord;
  ls0001_1.l = ls0001_1.lmax;
  i__1 = ls0001_1.l;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L125: */
      ls0001_1.el[i__ - 1] = ls0001_1.elco[i__ + ls0001_1.nq * 13 - 14];
    }
  ls0001_1.nqnyh = ls0001_1.nq * *nyh;
  ls0001_1.rc = ls0001_1.rc * ls0001_1.el[0] / ls0001_1.el0;
  ls0001_1.el0 = ls0001_1.el[0];
  ls0001_1.conit = .5 / (double) (ls0001_1.nq + 2);
  ddn =
    nsp_ode_vnorm (&ls0001_1.n, &savf[1],
		      &ewt[1]) / ls0001_1.tesco[ls0001_1.l * 3 - 3];
  exdn = 1. / (double) ls0001_1.l;
  rhdn = 1. / (pow (ddn, exdn) * 1.3 + 1.3e-6);
  rh = Min (rhdn, 1.);
  iredo = 3;
  if (ls0001_1.h__ == ls0001_1.hold)
    {
      goto L170;
    }
  /*Computing MIN 
   */
  d__2 = rh, d__3 = (d__1 = ls0001_1.h__ / ls0001_1.hold, Abs (d__1));
  rh = Min (d__2, d__3);
  ls0001_1.h__ = ls0001_1.hold;
  goto L175;
  /*----------------------------------------------------------------------- 
   *cfode is called to get all the integration coefficients for the 
   *current meth.  then the el vector and related constants are reset 
   *whenever the order nq is changed, or at the start of the problem. 
   *----------------------------------------------------------------------- 
   */
 L140:
  nsp_ode_cfode (&ls0001_1.meth, ls0001_1.elco, ls0001_1.tesco);
 L150:
  i__1 = ls0001_1.l;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L155: */
      ls0001_1.el[i__ - 1] = ls0001_1.elco[i__ + ls0001_1.nq * 13 - 14];
    }
  ls0001_1.nqnyh = ls0001_1.nq * *nyh;
  ls0001_1.rc = ls0001_1.rc * ls0001_1.el[0] / ls0001_1.el0;
  ls0001_1.el0 = ls0001_1.el[0];
  ls0001_1.conit = .5 / (double) (ls0001_1.nq + 2);
  switch (iret)
    {
    case 1:
      goto L160;
    case 2:
      goto L170;
    case 3:
      goto L200;
    }
  /*----------------------------------------------------------------------- 
   *if h is being changed, the h ratio rh is checked against 
   *rmax, hmin, and hmxi, and the yh array rescaled.  ialth is set to 
   *l = nq + 1 to prevent a change of h for that many steps, unless 
   *forced by a convergence or error test failure. 
   *----------------------------------------------------------------------- 
   */
 L160:
  if (ls0001_1.h__ == ls0001_1.hold)
    {
      goto L200;
    }
  rh = ls0001_1.h__ / ls0001_1.hold;
  ls0001_1.h__ = ls0001_1.hold;
  iredo = 3;
  goto L175;
 L170:
  /*Computing MAX 
   */
  d__1 = rh, d__2 = ls0001_1.hmin / Abs (ls0001_1.h__);
  rh = Max (d__1, d__2);
 L175:
  rh = Min (rh, ls0001_1.rmax);
  /*Computing MAX 
   */
  d__1 = 1., d__2 = Abs (ls0001_1.h__) * ls0001_1.hmxi * rh;
  rh /= Max (d__1, d__2);
  r__ = 1.;
  i__1 = ls0001_1.l;
  for (j = 2; j <= i__1; ++j)
    {
      r__ *= rh;
      i__2 = ls0001_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L180: */
	  yh[i__ + j * yh_dim1] *= r__;
	}
    }
  ls0001_1.h__ *= rh;
  ls0001_1.rc *= rh;
  ls0001_1.ialth = ls0001_1.l;
  if (iredo == 0)
    {
      goto L690;
    }
  /*----------------------------------------------------------------------- 
   *this section computes the predicted values by effectively 
   *multiplying the yh array by the pascal triangle matrix. 
   *rc is the ratio of new to old values of the coefficient  h*el(1). 
   *when rc differs from 1 by more than ccmax, ipup is set to miter 
   *to force pjac to be called. 
   *in any case, pjac is called at least every msbp steps. 
   *----------------------------------------------------------------------- 
   */
 L200:
  if ((d__1 = ls0001_1.rc - 1., Abs (d__1)) > ls0001_1.ccmax)
    {
      ls0001_1.ipup = ls0001_1.miter;
    }
  if (ls0001_1.nst >= ls0001_1.nslp + ls0001_1.msbp)
    {
      ls0001_1.ipup = ls0001_1.miter;
    }
  ls0001_1.tn += ls0001_1.h__;
  i1 = ls0001_1.nqnyh + 1;
  i__2 = ls0001_1.nq;
  for (jb = 1; jb <= i__2; ++jb)
    {
      i1 -= *nyh;
      i__1 = ls0001_1.nqnyh;
      for (i__ = i1; i__ <= i__1; ++i__)
	{
	  /* L210: */
	  yh1[i__] += yh1[i__ + *nyh];
	}
      /* L215: */
    }
  /*----------------------------------------------------------------------- 
   *up to maxcor corrector iterations are taken.  a convergence test is 
   *made on the r.m.s. norm of each correction, weighted by h and the 
   *error weight vector ewt.  the sum of the corrections is accumulated 
   *in acor(i).  the yh array is not altered in the corrector loop. 
   *----------------------------------------------------------------------- 
   */
 L220:
  m = 0;
  i__2 = ls0001_1.n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      savf[i__] = yh[i__ + (yh_dim1 << 1)] / ls0001_1.h__;
      /* L230: */
      y[i__] = yh[i__ + yh_dim1];
    }
  if (ls0001_1.ipup <= 0)
    {
      goto L240;
    }
  /*----------------------------------------------------------------------- 
   *if indicated, the matrix p = a - h*el(1)*dr/dy is reevaluated and 
   *preprocessed before starting the corrector iteration.  ipup is set 
   *to 0 as an indicator that this has been done. 
   *----------------------------------------------------------------------- 
   */
  ls0001_1.ipup = 0;
  ls0001_1.rc = 1.;
  ls0001_1.nslp = ls0001_1.nst;
  ls0001_1.crate = .7;
  (*pjac) (&neq[1], &y[1], &yh[yh_offset], nyh, &ewt[1], &acor[1], &savr[1],
	   &savf[1], &wm[1], &iwm[1], res, jac, adda, param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  if (ls0001_1.ierpj == 0)
    {
      goto L250;
    }
  ires = ls0001_1.ierpj;
  switch (ires)
    {
    case 1:
      goto L430;
    case 2:
      goto L435;
    case 3:
      goto L430;
    }
  /*get residual at predicted values, if not already done in pjac. ------- 
   */
 L240:
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &savf[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  kgo = Abs (ires);
  switch (kgo)
    {
    case 1:
      goto L250;
    case 2:
      goto L435;
    case 3:
      goto L430;
    }
 L250:
  i__2 = ls0001_1.n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* L260: */
      acor[i__] = 0.;
    }
  /*----------------------------------------------------------------------- 
   *solve the linear system with the current residual as 
   *right-hand side and p as coefficient matrix. 
   *----------------------------------------------------------------------- 
   */
 L270:
  (*slvs) (&wm[1], &iwm[1], &savr[1], &savf[1]);
  if (ls0001_1.iersl < 0)
    {
      goto L430;
    }
  if (ls0001_1.iersl > 0)
    {
      goto L410;
    }
  el1h = ls0001_1.el[0] * ls0001_1.h__;
  del =
    nsp_ode_vnorm (&ls0001_1.n, &savr[1], &ewt[1]) * Abs (ls0001_1.h__);
  i__2 = ls0001_1.n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      acor[i__] += savr[i__];
      savf[i__] = acor[i__] + yh[i__ + (yh_dim1 << 1)] / ls0001_1.h__;
      /* L380: */
      y[i__] = yh[i__ + yh_dim1] + el1h * acor[i__];
    }
  /*----------------------------------------------------------------------- 
   *test for convergence.  if m.gt.0, an estimate of the convergence 
   *rate constant is stored in crate, and this is used in the test. 
   *----------------------------------------------------------------------- 
   */
  if (m != 0)
    {
      /*Computing MAX 
       */
      d__1 = ls0001_1.crate * .2, d__2 = del / delp;
      ls0001_1.crate = Max (d__1, d__2);
    }
  /*Computing MIN 
   */
  d__1 = 1., d__2 = ls0001_1.crate * 1.5;
  dcon =
    del * Min (d__1,
	       d__2) / (ls0001_1.tesco[ls0001_1.nq * 3 - 2] * ls0001_1.conit);
  if (dcon <= 1.)
    {
      goto L460;
    }
  ++m;
  if (m == ls0001_1.maxcor)
    {
      goto L410;
    }
  if (m >= 2 && del > delp * 2.)
    {
      goto L410;
    }
  delp = del;
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &savf[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  kgo = Abs (ires);
  switch (kgo)
    {
    case 1:
      goto L270;
    case 2:
      goto L435;
    case 3:
      goto L410;
    }
  /*----------------------------------------------------------------------- 
   *the correctors failed to converge, or res has returned abnormally. 
   *on a convergence failure, if the jacobian is out of date, pjac is 
   *called for the next try.  otherwise the yh array is retracted to its 
   *values before prediction, and h is reduced, if possible. 
   *take an error exit if ires = 2, or h cannot be reduced, or mxncf 
   *failures have occurred, or a fatal error occurred in pjac or slvs. 
   *----------------------------------------------------------------------- 
   */
 L410:
  ls0001_1.icf = 1;
  if (ls0001_1.jcur == 1)
    {
      goto L430;
    }
  ls0001_1.ipup = ls0001_1.miter;
  goto L220;
 L430:
  ls0001_1.icf = 2;
  ++ncf;
  ls0001_1.rmax = 2.;
 L435:
  ls0001_1.tn = told;
  i1 = ls0001_1.nqnyh + 1;
  i__2 = ls0001_1.nq;
  for (jb = 1; jb <= i__2; ++jb)
    {
      i1 -= *nyh;
      i__1 = ls0001_1.nqnyh;
      for (i__ = i1; i__ <= i__1; ++i__)
	{
	  /* L440: */
	  yh1[i__] -= yh1[i__ + *nyh];
	}
      /* L445: */
    }
  if (ires == 2)
    {
      goto L680;
    }
  if (ls0001_1.ierpj < 0 || ls0001_1.iersl < 0)
    {
      goto L685;
    }
  if (Abs (ls0001_1.h__) <= ls0001_1.hmin * 1.00001)
    {
      goto L450;
    }
  if (ncf == ls0001_1.mxncf)
    {
      goto L450;
    }
  rh = .25;
  ls0001_1.ipup = ls0001_1.miter;
  iredo = 1;
  goto L170;
 L450:
  if (ires == 3)
    {
      goto L680;
    }
  goto L670;
  /*----------------------------------------------------------------------- 
   *the corrector has converged.  jcur is set to 0 
   *to signal that the jacobian involved may need updating later. 
   *the local error test is made and control passes to statement 500 
   *if it fails. 
   *----------------------------------------------------------------------- 
   */
 L460:
  ls0001_1.jcur = 0;
  if (m == 0)
    {
      dsm = del / ls0001_1.tesco[ls0001_1.nq * 3 - 2];
    }
  if (m > 0)
    {
      dsm =
	Abs (ls0001_1.h__) * nsp_ode_vnorm (&ls0001_1.n, &acor[1],
					       &ewt[1]) /
	ls0001_1.tesco[ls0001_1.nq * 3 - 2];
    }
  if (dsm > 1.)
    {
      goto L500;
    }
  /*----------------------------------------------------------------------- 
   *after a successful step, update the yh array. 
   *consider changing h if ialth = 1.  otherwise decrease ialth by 1. 
   *if ialth is then 1 and nq .lt. maxord, then acor is saved for 
   *use in a possible order increase on the next step. 
   *if a change in h is considered, an increase or decrease in order 
   *by one is considered also.  a change in h is made only if it is by a 
   *factor of at least 1.1.  if not, ialth is set to 3 to prevent 
   *testing for that many steps. 
   *----------------------------------------------------------------------- 
   */
  ls0001_1.kflag = 0;
  iredo = 0;
  ++ls0001_1.nst;
  ls0001_1.hu = ls0001_1.h__;
  ls0001_1.nqu = ls0001_1.nq;
  i__2 = ls0001_1.l;
  for (j = 1; j <= i__2; ++j)
    {
      eljh = ls0001_1.el[j - 1] * ls0001_1.h__;
      i__1 = ls0001_1.n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  /* L470: */
	  yh[i__ + j * yh_dim1] += eljh * acor[i__];
	}
    }
  --ls0001_1.ialth;
  if (ls0001_1.ialth == 0)
    {
      goto L520;
    }
  if (ls0001_1.ialth > 1)
    {
      goto L700;
    }
  if (ls0001_1.l == ls0001_1.lmax)
    {
      goto L700;
    }
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L490: */
      yh[i__ + ls0001_1.lmax * yh_dim1] = acor[i__];
    }
  goto L700;
  /*----------------------------------------------------------------------- 
   *the error test failed.  kflag keeps track of multiple failures. 
   *restore tn and the yh array to their previous values, and prepare 
   *to try the step again.  compute the optimum step size for this or 
   *one lower order.  after 2 or more failures, h is forced to decrease 
   *by a factor of 0.1 or less. 
   *----------------------------------------------------------------------- 
   */
 L500:
  --ls0001_1.kflag;
  ls0001_1.tn = told;
  i1 = ls0001_1.nqnyh + 1;
  i__1 = ls0001_1.nq;
  for (jb = 1; jb <= i__1; ++jb)
    {
      i1 -= *nyh;
      i__2 = ls0001_1.nqnyh;
      for (i__ = i1; i__ <= i__2; ++i__)
	{
	  /* L510: */
	  yh1[i__] -= yh1[i__ + *nyh];
	}
      /* L515: */
    }
  ls0001_1.rmax = 2.;
  if (Abs (ls0001_1.h__) <= ls0001_1.hmin * 1.00001)
    {
      goto L660;
    }
  if (ls0001_1.kflag <= -7)
    {
      goto L660;
    }
  iredo = 2;
  rhup = 0.;
  goto L540;
  /*----------------------------------------------------------------------- 
   *regardless of the success or failure of the step, factors 
   *rhdn, rhsm, and rhup are computed, by which h could be multiplied 
   *at order nq - 1, order nq, or order nq + 1, respectively. 
   *in the case of failure, rhup = 0.0 to avoid an order increase. 
   *the largest of these is determined and the new order chosen 
   *accordingly.  if the order is to be increased, we compute one 
   *additional scaled derivative. 
   *----------------------------------------------------------------------- 
   */
 L520:
  rhup = 0.;
  if (ls0001_1.l == ls0001_1.lmax)
    {
      goto L540;
    }
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L530: */
      savf[i__] = acor[i__] - yh[i__ + ls0001_1.lmax * yh_dim1];
    }
  dup =
    Abs (ls0001_1.h__) * nsp_ode_vnorm (&ls0001_1.n, &savf[1],
					   &ewt[1]) /
    ls0001_1.tesco[ls0001_1.nq * 3 - 1];
  exup = 1. / (double) (ls0001_1.l + 1);
  rhup = 1. / (pow (dup, exup) * 1.4 + 1.4e-6);
 L540:
  exsm = 1. / (double) ls0001_1.l;
  rhsm = 1. / (pow (dsm, exsm) * 1.2 + 1.2e-6);
  rhdn = 0.;
  if (ls0001_1.nq == 1)
    {
      goto L560;
    }
  ddn =
    nsp_ode_vnorm (&ls0001_1.n, &yh[ls0001_1.l * yh_dim1 + 1],
		      &ewt[1]) / ls0001_1.tesco[ls0001_1.nq * 3 - 3];
  exdn = 1. / (double) ls0001_1.nq;
  rhdn = 1. / (pow (ddn, exdn) * 1.3 + 1.3e-6);
 L560:
  if (rhsm >= rhup)
    {
      goto L570;
    }
  if (rhup > rhdn)
    {
      goto L590;
    }
  goto L580;
 L570:
  if (rhsm < rhdn)
    {
      goto L580;
    }
  newq = ls0001_1.nq;
  rh = rhsm;
  goto L620;
 L580:
  newq = ls0001_1.nq - 1;
  rh = rhdn;
  if (ls0001_1.kflag < 0 && rh > 1.)
    {
      rh = 1.;
    }
  goto L620;
 L590:
  newq = ls0001_1.l;
  rh = rhup;
  if (rh < 1.1)
    {
      goto L610;
    }
  r__ = ls0001_1.h__ * ls0001_1.el[ls0001_1.l - 1] / (double) ls0001_1.l;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L600: */
      yh[i__ + (newq + 1) * yh_dim1] = acor[i__] * r__;
    }
  goto L630;
 L610:
  ls0001_1.ialth = 3;
  goto L700;
 L620:
  if (ls0001_1.kflag == 0 && rh < 1.1)
    {
      goto L610;
    }
  if (ls0001_1.kflag <= -2)
    {
      rh = Min (rh, .1);
    }
  /*----------------------------------------------------------------------- 
   *if there is a change of order, reset nq, l, and the coefficients. 
   *in any case h is reset according to rh and the yh array is rescaled. 
   *then exit from 690 if the step was ok, or redo the step otherwise. 
   *----------------------------------------------------------------------- 
   */
  if (newq == ls0001_1.nq)
    {
      goto L170;
    }
 L630:
  ls0001_1.nq = newq;
  ls0001_1.l = ls0001_1.nq + 1;
  iret = 2;
  goto L150;
  /*----------------------------------------------------------------------- 
   *all returns are made through this section.  h is saved in hold 
   *to allow the caller to change h on the next step. 
   *----------------------------------------------------------------------- 
   */
 L660:
  ls0001_1.kflag = -1;
  goto L720;
 L670:
  ls0001_1.kflag = -2;
  goto L720;
 L680:
  ls0001_1.kflag = -1 - ires;
  goto L720;
 L685:
  ls0001_1.kflag = -5;
  goto L720;
 L690:
  ls0001_1.rmax = 10.;
 L700:
  r__ = ls0001_1.h__ / ls0001_1.tesco[ls0001_1.nqu * 3 - 2];
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L710: */
      acor[i__] *= r__;
    }
 L720:
  ls0001_1.hold = ls0001_1.h__;
  ls0001_1.jstart = 1;
  return 0;
}

