#include "integ.h"



#define ls0001_1 ls0001_._1

/* Table of constant values */

static int c__0 = 0;
static int c__50 = 50;
static int c__101 = 101;
static int c__1 = 1;
static double c_b84 = 0.;
static int c__60 = 60;
static int c__2 = 2;
static int c__102 = 102;
static int c__301 = 301;
static int c__201 = 201;
static int c__202 = 202;
static int c__203 = 203;
static int c__204 = 204;
static int c__205 = 205;
static int c__30 = 30;
static int c__206 = 206;
static int c__207 = 207;
static int c__208 = 208;
static int c__20 = 20;
static int c__210 = 210;
static int c__3 = 3;
static int c__4 = 4;
static int c__5 = 5;
static int c__6 = 6;
static int c__7 = 7;
static int c__8 = 8;
static int c__9 = 9;
static int c__10 = 10;
static int c__11 = 11;
static int c__12 = 12;
static int c__13 = 13;
static int c__40 = 40;
static int c__14 = 14;
static int c__15 = 15;
static int c__16 = 16;
static int c__17 = 17;
static int c__18 = 18;
static int c__19 = 19;
static int c__21 = 21;
static int c__22 = 22;
static int c__23 = 23;
static int c__24 = 24;
static int c__25 = 25;
static int c__26 = 26;
static int c__27 = 27;
static int c__302 = 302;
static int c__303 = 303;


int
nsp_ode_lsodi (lsodi_res res, ode_jac adda, Dgbydy jac, int *neq, double *y,
		  double *ydoti, double *t, double *tout, int *itol,
		  double *rtol, double *atol, int *itask, int *istate,
		  int *iopt, double *rwork, int *lrw, int *iwork, int *liw,
		  int *mf,void *param)
{
  /* Initialized data */

  static int mord[2] = { 12, 5 };
  static int mxstp0 = 500;
  static int mxhnl0 = 10;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;


  /* Local variables */
  double hmax;
  int ihit;
  int ires;
  double ewti, size;
  int i__, iflag;
  double atoli;
  int leniw, lenwm;
  int imxer;
  double tcrit;
  int i1, i2, lenrw;
  double h0, rtoli, tdist, tnext, tolsf;
  double w0;
  int ml;
  double rh;
  int lp, mu;
  double tp;
  double big;
  int ier, kgo;
  double ayi, hmx, tol, sum;
  int lyd0;

  /* 
   *!purpose 
   * livermore solver for ordinary differential equations (implicit form). 
   *lsodi solves the initial value problem for linearly implicit 
   *systems of first order ode-s, 
   *    a(t,y) * dy/dt = g(t,y) ,  where a(t,y) is a square matrix, 
   *or, in component form, 
   *    ( a   * ( dy / dt ))  + ... +  ( a     * ( dy   / dt ))  = 
   *       i,1      1                     i,neq      neq 
   * 
   *     =   g ( t, y , y ,..., y    )   ( i = 1,...,neq ) 
   *          i      1   2       neq 
   * 
   *if a is singular, this is a differential-algebraic system. 
   * 
   *lsodi is a variant version of the lsode package. 
   * 
   *!summary of usage. 
   * 
   *communication between the user and the lsodi package, for normal 
   *situations, is summarized here.  this summary describes only a subset 
   *of the full set of options available.  see the full description for 
   *details, including optional communication, nonstandard options, 
   *and instructions for special situations.  see also the example 
   *problem (with program and output) following this summary. 
   * 
   *a. first, provide a subroutine of the form.. 
   *              subroutine res (neq, t, y, s, r, ires) 
   *              dimension y(neq), s(neq), r(neq) 
   *which computes the residual function 
   *    r = g(t,y)  -  a(t,y) * s , 
   *as a function of t and the vectors y and s.  (s is an internally 
   *generated approximation to dy/dt.)  the arrays y and s are inputs 
   *to the res routine and should not be altered.  the residual 
   *vector is to be stored in the array r.  the argument ires should be 
   *ignored for casual use of lsodi.  (for uses of ires, see the 
   *paragraph on res in the full description below.) 
   * 
   *b. next, decide whether full or banded form is more economical 
   *for the storage of matrices.  lsodi must deal internally with the 
   *matrices a and dr/dy, where r is the residual function defined above. 
   *lsodi generates a linear combination of these two matrices, and 
   *this is treated in either full or banded form. 
   *    the matrix structure is communicated by a method flag mf, 
   *which is 21 or 22 for the full case, and 24 or 25 in the band case. 
   *    in the banded case, lsodi requires two half-bandwidth 
   *parameters ml and mu.  these are, respectively, the widths of the 
   *lower and upper parts of the band, excluding the main diagonal. 
   *thus the band consists of the locations (i,j) with 
   *i-ml .le. j .le. i+mu, and the full bandwidth is ml+mu+1. 
   *note that the band must accommodate the nonzero elements of 
   *a(t,y), dg/dy, and d(a*s)/dy (s fixed).  alternatively, one 
   *can define a band that encloses only the elements that are relatively 
   *large in magnitude, and gain some economy in storage and possibly 
   *also efficiency, although the appropriate threshhold for 
   *retaining matrix elements is highly problem-dependent. 
   * 
   *c. you must also provide a subroutine of the form.. 
   *              subroutine adda (neq, t, y, ml, mu, p, nrowp) 
   *              dimension y(neq), p(nrowp,neq) 
   *which adds the matrix a = a(t,y) to the contents of the array p. 
   *t and the y array are input and should not be altered. 
   *    in the full matrix case, this routine should add elements of 
   *to p in the usual order.  i.e., add a(i,j) to p(i,j).  (ignore the 
   *ml and mu arguments in this case.) 
   *    in the band matrix case, this routine should add element a(i,j) 
   *to p(i-j+mu+1,j).  i.e., add the diagonal lines of a to the rows of 
   *p from the top down (the top line of a added to the first row of p). 
   * 
   *d. for the sake of efficiency, you are encouraged to supply the 
   *jacobian matrix dr/dy in closed form, where r = g(t,y) - a(t,y)*s 
   *(s = a fixed vector) as above.  if dr/dy is being supplied, 
   *use mf = 21 or 24, and provide a subroutine of the form.. 
   *              subroutine jac (neq, t, y, s, ml, mu, p, nrowp) 
   *              dimension y(neq), s(neq), p(nrowp,neq) 
   *which computes dr/dy as a function of t, y, and s.  here t, y, and 
   *s are inputs, and the routine is to load dr/dy into p as follows.. 
   *    in the full matrix case (mf = 21), load p(i,j) with dr(i)/dy(j), 
   *the partial derivative of r(i) with respect to y(j).  (ignore the 
   *ml and mu arguments in this case.) 
   *    in the band matrix case (mf = 24), load p(i-j+mu+1,j) with 
   *dr(i)/dy(j), i.e. load the diagonal lines of dr/dy into the rows of 
   *p from the top down. 
   *    in either case, only nonzero elements need be loaded, and the 
   *indexing of p is the same as in the adda routine. 
   *    note that if a is independent of y (or this dependence 
   *is weak enough to be ignored) then jac is to compute dg/dy. 
   *    if it is not feasible to provide a jac routine, use 
   *mf = 22 or 25, and lsodi will compute an approximate jacobian 
   *internally by difference quotients. 
   * 
   *e. next decide whether or not to provide the initial value of the 
   *derivative vector dy/dt.  if the initial value of a(t,y) is 
   *nonsingular (and not too ill-conditioned), you may let lsodi compute 
   *this vector (istate = 0).  (lsodi will solve the system a*s = g for 
   *s, with initial values of a and g.)  if a(t,y) is initially 
   *singular, then the system is a differential-algebraic system, and 
   *you must make use of the particular form of the system to compute the 
   *initial values of y and dy/dt.  in that case, use istate = 1 and 
   *load the initial value of dy/dt into the array ydoti. 
   *the input array ydoti and the initial y array must be consistent with 
   *the equations a*dy/dt = g.  this implies that the initial residual 
   *r = g(t,y) - a(t,y)*ydoti   must be approximately zero. 
   * 
   *f. write a main program which calls subroutine lsodi once for 
   *each point at which answers are desired.  this should also provide 
   *for possible use of int unit 6 for output of error messages 
   *by lsodi.  on the first call to lsodi, supply arguments as follows.. 
   *res    = name of user subroutine for residual function r. 
   *adda   = name of user subroutine for computing and adding a(t,y). 
   *jac    = name of user subroutine for jacobian matrix dr/dy 
   *         (mf = 21 or 24).  if not used, pass a dummy name. 
   *note.. the names for the res and adda routines and (if used) the 
   *       jac routine must be declared external in the calling program. 
   *neq    = number of scalar equations in the system. 
   *y      = array of initial values, of length neq. 
   *ydoti  = array of length neq (containing initial dy/dt if istate = 1). 
   *t      = the initial value of the independent variable. 
   *tout   = first point where output is desired (.ne. t). 
   *itol   = 1 or 2 according as atol (below) is a scalar or array. 
   *rtol   = relative tolerance parameter (scalar). 
   *atol   = absolute tolerance parameter (scalar or array). 
   *         the estimated local error in y(i) will be controlled so as 
   *         to be roughly less (in magnitude) than 
   *            ewt(i) = rtol*abs(y(i)) + atol     if itol = 1, or 
   *            ewt(i) = rtol*abs(y(i)) + atol(i)  if itol = 2. 
   *         thus the local error test passes if, in each component, 
   *         either the absolute error is less than atol (or atol(i)), 
   *         or the relative error is less than rtol. 
   *         use rtol = 0.0 for pure absolute error control, and 
   *         use atol = 0.0 (or atol(i) = 0.0) for pure relative error 
   *         control.  caution.. actual (global) errors may exceed these 
   *         local tolerances, so choose them conservatively. 
   *itask  = 1 for normal computation of output values of y at t = tout. 
   *istate = int flag (input and output).  set istate = 1 if the 
   *         initial dy/dt is supplied, and 0 otherwise. 
   *iopt   = 0 to indicate no optional inputs used. 
   *rwork  = real work array of length at least.. 
   *            22 +  9*neq + neq**2           for mf = 21 or 22, 
   *            22 + 10*neq + (2*ml + mu)*neq  for mf = 24 or 25. 
   *lrw    = declared length of rwork (in user-s dimension). 
   *iwork  = int work array of length at least 20 + neq. 
   *         if mf = 24 or 25, input in iwork(1),iwork(2) the lower 
   *         and upper half-bandwidths ml,mu. 
   *liw    = declared length of iwork (in user-s dimension). 
   *mf     = method flag.  standard values are.. 
   *         21 for a user-supplied full jacobian. 
   *         22 for an internally generated full jacobian. 
   *         24 for a user-supplied banded jacobian. 
   *         25 for an internally generated banded jacobian. 
   *         for other choices of mf, see the paragraph on mf in 
   *         the full description below. 
   *note that the main program must declare arrays y, ydoti, rwork, iwork, 
   *and possibly atol. 
   * 
   *g. the output from the first call (or any call) is.. 
   *     y = array of computed values of y(t) vector. 
   *     t = corresponding value of independent variable (normally tout). 
   *istate = 2  if lsodi was successful, negative otherwise. 
   *         -1 means excess work done on this call (check all inputs). 
   *         -2 means excess accuracy requested (tolerances too small). 
   *         -3 means illegal input detected (see printed message). 
   *         -4 means repeated error test failures (check all inputs). 
   *         -5 means repeated convergence failures (perhaps bad jacobian 
   *            supplied or wrong choice of tolerances). 
   *         -6 means error weight became zero during problem. (solution 
   *            component i vanished, and atol or atol(i) = 0.) 
   *         -7 cannot occur in casual use. 
   *         -8 means lsodi was unable to compute the initial dy/dt. 
   *            in casual use, this means a(t,y) is initially singular. 
   *            supply ydoti and use istate = 1 on the first call. 
   * 
   * if lsodi returns istate = -1, -4, or -5, then the output of 
   * lsodi also includes ydoti = array containing residual vector 
   * r = g - a * dy/dt  evaluated at the current t, y, and dy/dt. 
   * 
   *h. to continue the integration after a successful return, simply 
   *reset tout and call lsodi again.  no other parameters need be reset. 
   * 
   * 
   * 
   *!example problem. 
   * 
   *the following is a simple example problem, with the coding 
   *needed for its solution by lsodi.  the problem is from chemical 
   *kinetics, and consists of the following three equations.. 
   *    dy1/dt = -.04*y1 + 1.e4*y2*y3 
   *    dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2 
   *      0.   = y1 + y2 + y3 - 1. 
   *on the interval from t = 0.0 to t = 4.e10, with initial conditions 
   *y1 = 1.0, y2 = y3 = 0. 
   * 
   *the following coding solves this problem with lsodi, using mf = 21 
   *and printing results at t = .4, 4., ..., 4.e10.  it uses 
   *itol = 2 and atol much smaller for y2 than y1 or y3 because 
   *y2 has much smaller values.  dy/dt is supplied in ydoti. we had 
   *obtained the initial value of dy3/dt by differentiating the 
   *third equation and evaluating the first two at t=0. 
   *at the end of the run, statistical quantities of interest are 
   *printed (see optional outputs in the full description below). 
   * 
   *    external resid, aplusp, dgbydy 
   *    double precision atol, rwork, rtol, t, tout, y, ydoti 
   *    dimension y(3), ydoti(3), atol(3), rwork(58), iwork(23) 
   *    neq = 3 
   *    y(1) = 1.0d+0 
   *    y(2) = 0.0d+0 
   *    y(3) = 0.0d+0 
   *    ydoti(1) = -.040d+0 
   *    ydoti(2) =  .040d+0 
   *    ydoti(3) =  0.0d+0 
   *    t = 0.0d+0 
   *    tout = .40d+0 
   *    itol = 2 
   *    rtol = 1.0d-4 
   *    atol(1) = 1.0d-6 
   *    atol(2) = 1.0d-10 
   *    atol(3) = 1.0d-6 
   *    itask = 1 
   *    istate = 1 
   *    iopt = 0 
   *    lrw = 58 
   *    liw = 23 
   *    mf = 21 
   *    do 40  iout = 1,12 
   *       call lsodi(resid, aplusp, dgbydy, neq, y, ydoti, t, tout, itol, 
   *   1      rtol, atol, itask, istate, iopt, rwork, lrw, iwork, liw, mf) 
   *       write (6,20)  t, y(1), y(2), y(3) 
   * 20    format(7h at t =,e12.4,6h   y =,3e14.6) 
   *       if (istate .lt. 0 )  go to 80 
   * 40    tout = tout*10.0d+0 
   *    write (6,60)  iwork(11), iwork(12), iwork(13) 
   * 60 format(/12h no. steps =,i4,11h  no. r-s =,i4, 
   *   1         11h  no. j-s =,i4) 
   *    stop 
   * 80 write (6,90)  istate 
   * 90 format(///22h error halt.. istate =,i3) 
   *    stop 
   *    end 
   * 
   *    subroutine resid(neq, t, y, s, r, ires) 
   *    double precision r, s, t, y 
   *    dimension y(3), s(3), r(3) 
   *    r(1) = -0.040d+0*y(1) + 1.0d+4*y(2)*y(3) - s(1) 
   *    r(2) =  0.040d+0*y(1) - 1.0d+4*y(2)*y(3) - 3.0d+7*y(2)*y(2) - s(2) 
   *    r(3) = y(1) + y(2) + y(3) - 1.0d+0 
   *    return 
   *    end 
   * 
   *    subroutine aplusp(neq, t, y, ml, mu, p, nrowp) 
   *    double precision p, t, y 
   *    dimension y(3), p(nrowp,3) 
   *    p(1,1) = p(1,1) + 1.0d+0 
   *    p(2,2) = p(2,2) + 1.0d+0 
   *    return 
   *    end 
   * 
   *    subroutine dgbydy(neq, t, y, s, ml, mu, p, nrowp) 
   *    double precision s, t, p, y 
   *    dimension y(3), s(3), p(nrowp,3) 
   *    p(1,1) = -0.040d+0 
   *    p(1,2) =  1.0d+4*y(3) 
   *    p(1,3) =  1.0d+4*y(2) 
   *    p(2,1) =  0.040d+0 
   *    p(2,2) = -1.0d+4*y(3) - 6.0d+7*y(2) 
   *    p(2,3) = -1.0d+4*y(2) 
   *    p(3,1) =  1.0d+0 
   *    p(3,2) =  1.0d+0 
   *    p(3,3) =  1.0d+0 
   *    return 
   *    end 
   * 
   *the output of this program (on a cdc-7600 in single precision) 
   *is as follows.. 
   * 
   *  at t =  4.0000e-01   y =  9.851726e-01  3.386406e-05  1.479357e-02 
   *  at t =  4.0000e+00   y =  9.055142e-01  2.240418e-05  9.446344e-02 
   *  at t =  4.0000e+01   y =  7.158050e-01  9.184616e-06  2.841858e-01 
   *  at t =  4.0000e+02   y =  4.504846e-01  3.222434e-06  5.495122e-01 
   *  at t =  4.0000e+03   y =  1.831701e-01  8.940379e-07  8.168290e-01 
   *  at t =  4.0000e+04   y =  3.897016e-02  1.621193e-07  9.610297e-01 
   *  at t =  4.0000e+05   y =  4.935213e-03  1.983756e-08  9.950648e-01 
   *  at t =  4.0000e+06   y =  5.159269e-04  2.064759e-09  9.994841e-01 
   *  at t =  4.0000e+07   y =  5.306413e-05  2.122677e-10  9.999469e-01 
   *  at t =  4.0000e+08   y =  5.494532e-06  2.197826e-11  9.999945e-01 
   *  at t =  4.0000e+09   y =  5.129457e-07  2.051784e-12  9.999995e-01 
   *  at t =  4.0000e+10   y = -7.170472e-08 -2.868188e-13  1.000000e+00 
   * 
   *  no. steps = 330  no. r-s = 404  no. j-s =  69 
   * 
   *!full description of user interface to lsodi. 
   * 
   *the user interface to lsodi consists of the following parts. 
   * 
   *i.   the call sequence to subroutine lsodi, which is a driver 
   *     routine for the solver.  this includes descriptions of both 
   *     the call sequence arguments and of user-supplied routines. 
   *     following these descriptions is a description of 
   *     optional inputs available through the call sequence, and then 
   *     a description of optional outputs (in the work arrays). 
   * 
   *ii.  descriptions of other routines in the lsodi package that may be 
   *     (optionally) called by the user.  these provide the ability to 
   *     alter error message handling, save and restore the internal 
   *     common, and obtain specified derivatives of the solution y(t). 
   * 
   *iii. descriptions of common blocks to be declared in overlay 
   *     or similar environments, or to be saved when doing an interrupt 
   *     of the problem and continued solution later. 
   * 
   *iv.  description of two subroutines in the lsodi package, either of 
   *     which the user may replace with his own version, if desired. 
   *     these relate to the measurement of errors. 
   * 
   * 
   *part i.  call sequence. 
   * 
   *the call sequence parameters used for input only are 
   *    res, adda, jac, neq, tout, itol, rtol, atol, itask, 
   *    iopt, lrw, liw, mf, 
   *and those used for both input and output are 
   *    y, t, istate, ydoti. 
   *the work arrays rwork and iwork are also used for conditional and 
   *optional inputs and optional outputs.  (the term output here refers 
   *to the return from subroutine lsodi to the user-s calling program.) 
   * 
   *the legality of input parameters will be thoroughly checked on the 
   *initial call for the problem, but not checked thereafter unless a 
   *change in input parameters is flagged by istate = 3 on input. 
   * 
   *the descriptions of the call arguments are as follows. 
   * 
   *res    = the name of the user-supplied subroutine which supplies 
   *         the residual vector for the ode system, defined by 
   *           r = g(t,y) - a(t,y) * s 
   *         as a function of the scalar t and the vectors 
   *         s and y ( s approximates dy/dt ). this 
   *         subroutine is to have the form 
   *             subroutine res ( neq, t, y, s, r, ires ) 
   *             dimension y(*), s(*), r(*) 
   *         where neq, t, y, s, and ires are input, and r and 
   *         ires are output. y, s, and r are arrays of length neq. 
   *         in dimension statements such as that above, 1 is a 
   *         dummy dimension. it can be replaced by any value. 
   *            on input, ires indicates how lsodi will use the 
   *         returned array r, as follows.. 
   *            ires = 1  means that lsodi needs the full residual, 
   *                      r = g - a*s, exactly. 
   *            ires = -1 means that lsodi is using r only to compute 
   *                      the jacobian dr/dy by difference quotients. 
   *         the res routine can ignore ires, or it can omit some terms 
   *         if ires = -1.  if a does not depend on y, then res can 
   *         just return r = g when ires = -1.  if g - a*s contains other 
   *         additive terms that are independent of y, these can also be 
   *         dropped, if done consistently, when ires = -1. 
   *            the subroutine should set the flag ires if it 
   *         encounters a halt condition or illegal input. 
   *         otherwise, it should not reset ires.  on output, 
   *            ires = 1 or -1 represents a normal return, and 
   *         lsodi continues integrating the ode.  leave ires 
   *         unchanged from its input value. 
   *            ires = 2 tells lsodi to immediately return control 
   *         to the calling program, with istate = 3.  this lets 
   *         the calling program change parameters of the prob- 
   *         lem if necessary. 
   *            ires = 3 represents an error condition (for example, an 
   *         illegal value of y). lsodi tries to integrate the ode without 
   *         getting ires = 3 from res.  if it cannot, lsodi returns 
   *         with istate = -7 or -1. 
   *           on an lsodi return with istate = 3, -1, or -7, the values 
   *         of t and y returned correspond to the last point reached 
   *         successfully without getting the flag ires = 2 or 3. 
   *           the flag values ires = 2 and 3 should not be used to 
   *         handle switches or root-stop conditions.  this is better 
   *         done by calling lsodi in a one-step mode and checking the 
   *         stopping function for a sign change at each step. 
   *           res must be declared external in the calling 
   *         program. see note below for more about res. 
   * 
   *adda   = the name of the user-supplied subroutine which adds 
   *         the matrix a = a(t,y) to another matrix stored in the same 
   *         form as a. the storage form is determined by miter (see 
   *         mf).  this subroutine is to have the form 
   *              subroutine adda ( neq, t, y, ml, mu, p, nrowp ) 
   *              dimension y(*), p(nrowp,*) 
   *         where neq, t, y, ml, mu, and nrowp are input and p is 
   *         output. y is an array of length neq, and the matrix p is 
   *         stored in an nrowp by neq array. 
   *            in the full matrix case ( miter =  1 or 2 ) adda should 
   *         add  a    to p(i,j). ml and mu are ignored. 
   *               i,j 
   *            in the band matrix case ( miter = 4 or 5 ) adda should 
   *         add  a    to  p(i-j+mu+1,j). 
   *               i,j 
   *         see jac for details on this band storage form. 
   *            adda must be declared external in the calling program. 
   *         see note below for more information about adda. 
   * 
   *jac    = the name of the user-supplied subroutine which supplies 
   *         the jacobian matrix, dr/dy, where r = g-a*s. the form of the 
   *         jacobian matrix is determined by miter. jac is required 
   *         if miter = 1 or 4 -- otherwise a dummy name can be 
   *         passed. this subroutine is to have the form 
   *              subroutine jac ( neq, t, y, s, ml, mu, p, nrowp ) 
   *              dimension y(*), s(*), p(nrowp,*) 
   *         where neq, t, y, s, ml, mu, and nrowp are input and p 
   *         is output. y and s are arrays of length neq, and the 
   *         matrix p is stored in an nrowp by neq array. 
   *         p is to be loaded with partial derivatives ( elements 
   *         of the jacobian matrix ) on output. 
   *            in the full matrix case ( miter = 1 ), ml and mu 
   *         are ignored and the jacobian is to be loaded into p 
   *         by columns- i.e., dr(i)/dy(j) is loaded into p(i,j). 
   *            in the band matrix case ( miter = 4 ), the ele- 
   *         ments within the band are to be loaded into p by 
   *         by columns, with diagonal lines of dr/dy loaded into 
   *         the rows of p.  thus dr(i)/dy(j) is to be loaded 
   *         into p(i-j+mu+1,j). the locations in p in the two 
   *         triangular areas which correspond to nonexistent matrix 
   *         elements can be ignored or loaded arbitrarily, as they 
   *         they are overwritten by lsodi. ml and mu are the half- 
   *         bandwidth parameters ( see iwork ). 
   *              in either case, p is preset to zero by the solver, 
   *         so that only the nonzero elements need be loaded by jac. 
   *         each call to jac is preceded by a call to res with the same 
   *         arguments neq, t, y, and s.  thus to gain some efficiency, 
   *         intermediate quantities shared by both calculations may be 
   *         saved in a user common block by res and not recomputed by jac 
   *         if desired.  also, jac may alter the y array, if desired. 
   *              jac need not provide dr/dy exactly.  a crude 
   *         approximation (possibly with a smaller bandwidth) will do. 
   *              jac must be declared external in the calling program. 
   *              see note below for more about jac. 
   * 
   *      note on res, adda, and jac--  these 
   *         subroutines may access user-defined quantities in 
   *         neq(2),... and y(neq(1)+1),... if neq is an array 
   *         (dimensioned in the subroutines) and y has length exceeding 
   *         neq(1). however, these subroutines should not alter 
   *         neq(1), y(1),...,y(neq) or any other input variables. 
   *         see the descriptions of neq and y below. 
   * 
   *neq    = the size of the system (number of first order ordinary 
   *         differential equations or scalar algebraic equations). 
   *         used only for input. 
   *         neq may be decreased, but not increased, during the problem. 
   *         if neq is decreased (with istate = 3 on input), the 
   *         remaining components of y should be left undisturbed, if 
   *         these are to be accessed in res, adda, or jac. 
   * 
   *         normally, neq is a scalar, and it is generally referred to 
   *         as a scalar in this user interface description.  however, 
   *         neq may be an array, with neq(1) set to the system size. 
   *         (the lsodi package accesses only neq(1).)  in either case, 
   *         this parameter is passed as the neq argument in all calls 
   *         to res, adda, and jac.  hence, if it is an array, 
   *         locations neq(2),... may be used to store other int data 
   *         and pass it to  res, adda, or jac.  each such subroutine 
   *         must include neq in a dimension statement in that case. 
   * 
   *y      = a real array for the vector of dependent variables, of 
   *         length neq or more.  used for both input and output on the 
   *         first call (istate = 0 or 1), and only for output on other 
   *         calls.  on the first call, y must contain the vector of 
   *         initial values.  on output, y contains the computed solution 
   *         vector, evaluated at t.  if desired, the y array may be used 
   *         for other purposes between calls to the solver. 
   * 
   *         this array is passed as the y argument in all calls to res, 
   *         adda, and jac.  hence its length may exceed neq, 
   *         and locations y(neq+1),... may be used to store other real 
   *         data and pass it to res, adda, or jac.  (the lsodi 
   *         package accesses only y(1),...,y(neq). ) 
   * 
   *ydoti  = a real array for the initial value of the vector 
   *         dy/dt and for work space, of dimension at least neq. 
   * 
   *         on input... 
   *           if istate = 0 then lsodi will compute the initial value 
   *         of dy/dt, if a is nonsingular.  thus ydoti will 
   *         serve only as work space and may have any value. 
   *           if istate = 1 then ydoti must contain the initial value 
   *         of dy/dt. 
   *           if istate = 2 or 3 (continuation calls) then ydoti 
   *         may have any value. 
   *           n.b.- if the initial value of a is singular, then 
   *         lsodi cannot compute the initial value of dy/dt, so 
   *         it must be provided in ydoti, with istate=1. 
   * 
   *         on output, when lsodi terminates abnormally with istate = 
   *         -1, -4, or -5, ydoti will contain the residual 
   *         r = g(t,y) - a(t,y)*(dy/dt).  if r is large, t is near 
   *         its initial value, and ydoti is supplied with istate=1, 
   *         there may have been an incorrect input value of 
   *         ydoti = dy/dt or the problem ( as given to lsodi ) 
   *         may not have a solution. 
   * 
   *         if desired, the ydoti array may be used for other 
   *         purposes between calls to the solver. 
   * 
   *t      = the independent variable.  on input, t is used only on the 
   *         first call, as the initial point of the integration. 
   *         on output, after each call, t is the value at which a 
   *         computed solution y is evaluated (usually the same as tout). 
   *         on an error return, t is the farthest point reached. 
   * 
   *tout   = the next value of t at which a computed solution is desired. 
   *         used only for input. 
   * 
   *         when starting the problem (istate = 0 or 1), tout may be 
   *         equal to t for one call, then should .ne. t for the next 
   *         call.  for the initial t, an input value of tout .ne. t is 
   *         used in order to determine the direction of the integration 
   *         (i.e. the algebraic sign of the step sizes) and the rough 
   *         scale of the problem.  integration in either direction 
   *         (forward or backward in t) is permitted. 
   * 
   *         if itask = 2 or 5 (one-step modes), tout is ignored after 
   *         the first call (i.e. the first call with tout .ne. t). 
   *         otherwise, tout is required on every call. 
   * 
   *         if itask = 1, 3, or 4, the values of tout need not be 
   *         monotone, but a value of tout which backs up is limited 
   *         to the current internal t interval, whose endpoints are 
   *         tcur - hu and tcur (see optional outputs, below, for 
   *         tcur and hu). 
   * 
   *itol   = an indicator for the type of error control.  see 
   *         description below under atol.  used only for input. 
   * 
   *rtol   = a relative error tolerance parameter, either a scalar or 
   *         an array of length neq.  see description below under atol. 
   *         input only. 
   * 
   *atol   = an absolute error tolerance parameter, either a scalar or 
   *         an array of length neq.  input only. 
   * 
   *            the input parameters itol, rtol, and atol determine 
   *         the error control performed by the solver.  the solver will 
   *         control the vector e = (e(i)) of estimated local errors 
   *         in y, according to an inequality of the form 
   *                     rms-norm of ( e(i)/ewt(i) )   .le.   1, 
   *         where       ewt(i) = rtol(i)*abs(y(i)) + atol(i), 
   *         and the rms-norm (root-mean-square norm) here is 
   *         rms-norm(v) = sqrt(sum v(i)**2 / neq).  here ewt = (ewt(i)) 
   *         is a vector of weights which must always be positive, and 
   *         the values of rtol and atol should all be non-negative. 
   *         the following table gives the types (scalar/array) of 
   *         rtol and atol, and the corresponding form of ewt(i). 
   * 
   *            itol    rtol       atol          ewt(i) 
   *             1     scalar     scalar     rtol*abs(y(i)) + atol 
   *             2     scalar     array      rtol*abs(y(i)) + atol(i) 
   *             3     array      scalar     rtol(i)*abs(y(i)) + atol 
   *             4     array      scalar     rtol(i)*abs(y(i)) + atol(i) 
   * 
   *         when either of these parameters is a scalar, it need not 
   *         be dimensioned in the user-s calling program. 
   * 
   *         if none of the above choices (with itol, rtol, and atol 
   *         fixed throughout the problem) is suitable, more general 
   *         error controls can be obtained by substituting 
   *         user-supplied routines for the setting of ewt and/or for 
   *         the norm calculation.  see part iv below. 
   * 
   *         if global errors are to be estimated by making a repeated 
   *         run on the same problem with smaller tolerances, then all 
   *         components of rtol and atol (i.e. of ewt) should be scaled 
   *         down uniformly 
   * 
   *itask  = an index specifying the task to be performed. 
   *         input only.  itask has the following values and meanings. 
   *         1  means normal computation of output values of y(t) at 
   *            t = tout (by overshooting and interpolating). 
   *         2  means take one step only and return. 
   *         3  means stop at the first internal mesh point at or 
   *            beyond t = tout and return. 
   *         4  means normal computation of output values of y(t) at 
   *            t = tout but without overshooting t = tcrit. 
   *            tcrit must be input as rwork(1).  tcrit may be equal to 
   *            or beyond tout, but not behind it in the direction of 
   *            integration.  this option is useful if the problem 
   *            has a singularity at or beyond t = tcrit. 
   *         5  means take one step, without passing tcrit, and return. 
   *            tcrit must be input as rwork(1). 
   * 
   *         note..  if itask = 4 or 5 and the solver reaches tcrit 
   *         (within roundoff), it will return t = tcrit (exactly) to 
   *         indicate this (unless itask = 4 and tout comes before tcrit, 
   *         in which case answers at t = tout are returned first). 
   * 
   *istate = an index used for input and output to specify the 
   *         state of the calculation. 
   * 
   *         on input, the values of istate are as follows. 
   *         0  means this is the first call for the problem, and 
   *            lsodi is to compute the initial value of dy/dt 
   *            (while doing other initializations).  see note below. 
   *         1  means this is the first call for the problem, and 
   *            the initial value of dy/dt has been supplied in 
   *            ydoti (lsodi will do other initializations). see note 
   *            below. 
   *         2  means this is not the first call, and the calculation 
   *            is to continue normally, with no change in any input 
   *            parameters except possibly tout and itask. 
   *            (if itol, rtol, and/or atol are changed between calls 
   *            with istate = 2, the new values will be used but not 
   *            tested for legality.) 
   *         3  means this is not the first call, and the 
   *            calculation is to continue normally, but with 
   *            a change in input parameters other than 
   *            tout and itask.  changes are allowed in 
   *            neq, itol, rtol, atol, iopt, lrw, liw, mf, ml, mu, 
   *            and any of the optional inputs except h0. 
   *            (see iwork description for ml and mu.) 
   *         note..  a preliminary call with tout = t is not counted 
   *         as a first call here, as no initialization or checking of 
   *         input is done.  (such a call is sometimes useful for the 
   *         purpose of outputting the initial conditions.) 
   *         thus the first call for which tout .ne. t requires 
   *         istate = 0 or 1 on input. 
   * 
   *         on output, istate has the following values and meanings. 
   *          0 or 1  means nothing was done, as tout was equal to t with 
   *             istate = 0 or 1 on input.  (however, an internal counter 
   *             was set to detect and prevent repeated calls of this 
   *             type. ) 
   *          2  means that the integration was performed successfully. 
   *          3  means that the user-supplied subroutine res signalled 
   *             lsodi to halt the integration and return (ires=2). 
   *             integration as far as t was achieved with no occurrence 
   *             of ires=2, but this flag was set on attempting the next 
   *             step. 
   *         -1  means an excessive amount of work (more than mxstep 
   *             steps) was done on this call, before completing the 
   *             requested task, but the integration was otherwise 
   *             successful as far as t.  (mxstep is an optional input 
   *             and is normally 500.)  to continue, the user may 
   *             simply reset istate to a value .gt. 1 and call again 
   *             (the excess work step counter will be reset to 0). 
   *             in addition, the user may increase mxstep to avoid 
   *             this error return (see below on optional inputs). 
   *         -2  means too much accuracy was requested for the precision 
   *             of the machine being used.  this was detected before 
   *             completing the requested task, but the integration 
   *             was successful as far as t.  to continue, the tolerance 
   *             parameters must be reset, and istate must be set 
   *             to 3.  the optional output tolsf may be used for this 
   *             purpose.  (note.. if this condition is detected before 
   *             taking any steps, then an illegal input return 
   *             (istate = -3) occurs instead.) 
   *         -3  means illegal input was detected, before taking any 
   *             integration steps.  see written message for details. 
   *             note..  if the solver detects an infinite loop of calls 
   *             to the solver with illegal input, it will cause 
   *             the run to stop. 
   *         -4  means there were repeated error test failures on 
   *             one attempted step, before completing the requested 
   *             task, but the integration was successful as far as t. 
   *             the problem may have a singularity, or the input 
   *             may be inappropriate. 
   *         -5  means there were repeated convergence test failures on 
   *             one attempted step, before completing the requested 
   *             task, but the integration was successful as far as t. 
   *             this may be caused by an inaccurate jacobian matrix. 
   *         -6  means ewt(i) became zero for some i during the 
   *             integration.  pure relative error control (atol(i)=0.0) 
   *             was requested on a variable which has now vanished. 
   *             the integration was successful as far as t. 
   *         -7  means that the user-supplied subroutine res set 
   *             its error flag (ires = 3) despite repeated tries by 
   *             lsodi to avoid that condition. 
   *         -8  means that istate was 0 on input but lsodi was unable 
   *             to compute the initial value of dy/dt.  see the 
   *             printed message for details. 
   * 
   *         note..  since the normal output value of istate is 2, 
   *         it does not need to be reset for normal continuation. 
   *         similarly, istate need not be reset if res told lsodi 
   *         to return because the calling program must change 
   *         the parameters of the problem. 
   *         also, since a negative input value of istate will be 
   *         regarded as illegal, a negative output value requires the 
   *         user to change it, and possibly other inputs, before 
   *         calling the solver again. 
   * 
   *iopt   = an int flag to specify whether or not any optional 
   *         inputs are being used on this call.  input only. 
   *         the optional inputs are listed separately below. 
   *         iopt = 0 means no optional inputs are being used. 
   *                  default values will be used in all cases. 
   *         iopt = 1 means one or more optional inputs are being used. 
   * 
   *rwork  = a real working array (double precision). 
   *         the length of rwork must be at least 
   *            20 + nyh*(maxord + 1) + 3*neq + lenwm    where 
   *         nyh    = the initial value of neq, 
   *         maxord = 12 (if meth = 1) or 5 (if meth = 2) (unless a 
   *                  smaller value is given as an optional input), 
   *         lenwm   = neq**2 + 2    if miter is 1 or 2, and 
   *         lenwm   = (2*ml+mu+1)*neq + 2 if miter is 4 or 5. 
   *         (see mf description for the definition of meth and miter.) 
   *         thus if maxord has its default value and neq is constant, 
   *         this length is 
   *            22 + 16*neq + neq**2         for mf = 11 or 12, 
   *            22 + 17*neq + (2*ml+mu)*neq  for mf = 14 or 15, 
   *            22 +  9*neq + neq**2         for mf = 21 or 22, 
   *            22 + 10*neq + (2*ml+mu)*neq  for mf = 24 or 25. 
   *         the first 20 words of rwork are reserved for conditional 
   *         and optional inputs and optional outputs. 
   * 
   *         the following word in rwork is a conditional input.. 
   *           rwork(1) = tcrit = critical value of t which the solver 
   *                      is not to overshoot.  required if itask is 
   *                      4 or 5, and ignored otherwise.  (see itask.) 
   * 
   *lrw    = the length of the array rwork, as declared by the user. 
   *         (this will be checked by the solver.) 
   * 
   *iwork  = an int work array.  the length of iwork must be at least 
   *         20 + neq .  the first few words of iwork are used for 
   *         conditional and optional inputs and optional outputs. 
   * 
   *         the following 2 words in iwork are conditional inputs.. 
   *           iwork(1) = ml     these are the lower and upper 
   *           iwork(2) = mu     half-bandwidths, respectively, of the 
   *                      matrices in the problem-- the jacobian dr/dy 
   *                      and the left-hand side matrix a. these half- 
   *                      bandwidths exclude the main diagonal, so 
   *                      the total bandwidth is ml + mu + 1 . 
   *                      the band is defined by the matrix locations 
   *                      (i,j) with i-ml .le. j .le. i+mu.  ml and mu 
   *                      must satisfy  0 .le.  ml,mu  .le. neq-1. 
   *                      these are required if miter is 4 or 5, and 
   *                      ignored otherwise. 
   *                      ml and mu may in fact be the band parameters for 
   *                      matrices to which dr/dy and a are only 
   *                      approximately equal. 
   * 
   *liw    = the length of the array iwork, as declared by the user. 
   *         (this will be checked by the solver.) 
   * 
   *note..  the work arrays must not be altered between calls to lsodi 
   *for the same problem, except possibly for the conditional and 
   *optional inputs, and except for the last 3*neq words of rwork. 
   *the latter space is used for internal scratch space, and so is 
   *available for use by the user outside lsodi between calls, if 
   *desired (but not for use by res, adda, or jac). 
   * 
   *mf     = the method flag.  used only for input.  the legal values of 
   *         mf are 11, 12, 14, 15, 21, 22, 24, and 25. 
   *         mf has decimal digits meth and miter.. mf = 10*meth + miter. 
   *           meth indicates the basic linear multistep method.. 
   *             meth = 1 means the implicit adams method. 
   *             meth = 2 means the method based on backward 
   *                      differentiation formulas (bdf-s). 
   *               the bdf method is strongly preferred for stiff prob- 
   *             lems, while the adams method is preferred when the prob- 
   *             lem is not stiff. if the matrix a(t,y) is nonsingular, 
   *             stiffness here can be taken to mean that of the explicit 
   *             ode system dy/dt = a**(-1) * g.  if a is singular, the 
   *             concept of stiffness is not well defined. 
   *               if you do not know whether the problem is stiff, we 
   *             recommend using meth = 2.  if it is stiff, the advan- 
   *             tage of meth = 2 over 1 will be great, while if it is 
   *             not stiff, the advantage of meth = 1 will be slight. 
   *             if maximum efficiency is important, some experimentation 
   *             with meth may be necessary. 
   *           miter indicates the corrector iteration method.. 
   *             miter = 1 means chord iteration with a user-supplied 
   *                       full (neq by neq) jacobian. 
   *             miter = 2 means chord iteration with an internally 
   *                       generated (difference quotient) full jacobian. 
   *                       this uses neq+1 extra calls to res per dr/dy 
   *                       evaluation. 
   *             miter = 4 means chord iteration with a user-supplied 
   *                       banded jacobian. 
   *             miter = 5 means chord iteration with an internally 
   *                       generated banded jacobian (using ml+mu+2 
   *                       extra calls to res per dr/dy evaluation). 
   *             if miter = 1 or 4, the user must supply a subroutine jac 
   *             (the name is arbitrary) as described above under jac. 
   *             for other values of miter, a dummy argument can be used. 
   * 
   *!optional inputs. 
   * 
   *the following is a list of the optional inputs provided for in the 
   *call sequence.  (see also part ii.)  for each such input variable, 
   *this table lists its name as used in this documentation, its 
   *location in the call sequence, its meaning, and the default value. 
   *the use of any of these inputs requires iopt = 1, and in that 
   *case all of these inputs are examined.  a value of zero for any 
   *of these optional inputs will cause the default value to be used. 
   *thus to use a subset of the optional inputs, simply preload 
   *locations 5 to 10 in rwork and iwork to 0.0 and 0 respectively, and 
   *then set those of interest to nonzero values. 
   * 
   *name    location      meaning and default value 
   * 
   *h0      rwork(5)  the step size to be attempted on the first step. 
   *                  the default value is determined by the solver. 
   * 
   *hmax    rwork(6)  the maximum absolute step size allowed. 
   *                  the default value is infinite. 
   * 
   *hmin    rwork(7)  the minimum absolute step size allowed. 
   *                  the default value is 0.  (this lower bound is not 
   *                  enforced on the final step before reaching tcrit 
   *                  when itask = 4 or 5.) 
   * 
   *maxord  iwork(5)  the maximum order to be allowed.  the default 
   *                  value is 12 if meth = 1, and 5 if meth = 2. 
   *                  if maxord exceeds the default value, it will 
   *                  be reduced to the default value. 
   *                  if maxord is changed during the problem, it may 
   *                  cause the current order to be reduced. 
   * 
   *mxstep  iwork(6)  maximum number of (internally defined) steps 
   *                  allowed during one call to the solver. 
   *                  the default value is 500. 
   * 
   *mxhnil  iwork(7)  maximum number of messages printed (per problem) 
   *                  warning that t + h = t on a step (h = step size). 
   *                  this must be positive to result in a non-default 
   *                  value.  the default value is 10. 
   * 
   *!optional outputs. 
   * 
   *as optional additional output from lsodi, the variables listed 
   *below are quantities related to the performance of lsodi 
   *which are available to the user.  these are communicated by way of 
   *the work arrays, but also have internal mnemonic names as shown. 
   *except where stated otherwise, all of these outputs are defined 
   *on any successful return from lsodi, and on any return with 
   *istate = -1, -2, -4, -5, -6, or -7. on a return with -3 (illegal 
   *input) or -8, they will be unchanged from their existing values 
   *(if any), except possibly for tolsf, lenrw, and leniw. 
   *on any error return, outputs relevant to the error will be defined, 
   *as noted below. 
   * 
   *name    location      meaning 
   * 
   *hu      rwork(11) the step size in t last used (successfully). 
   * 
   *hcur    rwork(12) the step size to be attempted on the next step. 
   * 
   *tcur    rwork(13) the current value of the independent variable 
   *                  which the solver has actually reached, i.e. the 
   *                  current internal mesh point in t.  on output, tcur 
   *                  will always be at least as far as the argument 
   *                  t, but may be farther (if interpolation was done). 
   * 
   *tolsf   rwork(14) a tolerance scale factor, greater than 1.0, 
   *                  computed when a request for too much accuracy was 
   *                  detected (istate = -3 if detected at the start of 
   *                  the problem, istate = -2 otherwise).  if itol is 
   *                  left unaltered but rtol and atol are uniformly 
   *                  scaled up by a factor of tolsf for the next call, 
   *                  then the solver is deemed likely to succeed. 
   *                  (the user may also ignore tolsf and alter the 
   *                  tolerance parameters in any other way appropriate.) 
   * 
   *nst     iwork(11) the number of steps taken for the problem so far. 
   * 
   *nfe     iwork(12) the number of residual evaluations (res calls) 
   *                  for the problem so far. 
   * 
   *nje     iwork(13) the number of jacobian evaluations (each involving 
   *                  an evaluation of a and dr/dy) for the problem so 
   *                  far.  this equals the number of calls to adda and 
   *                  (if miter = 1 or 4) jac, and the number of matrix 
   *                  l-u decompositions. 
   * 
   *nqu     iwork(14) the method order last used (successfully). 
   * 
   *nqcur   iwork(15) the order to be attempted on the next step. 
   * 
   *imxer   iwork(16) the index of the component of largest magnitude in 
   *                  the weighted local error vector ( e(i)/ewt(i) ), 
   *                  on an error return with istate = -4 or -5. 
   * 
   *lenrw   iwork(17) the length of rwork actually required. 
   *                  this is defined on normal returns and on an illegal 
   *                  input return for insufficient storage. 
   * 
   *leniw   iwork(18) the length of iwork actually required. 
   *                  this is defined on normal returns and on an illegal 
   *                  input return for insufficient storage. 
   * 
   * 
   *the following two arrays are segments of the rwork array which 
   *may also be of interest to the user as optional outputs. 
   *for each array, the table below gives its internal name, 
   *its base address in rwork, and its description. 
   * 
   *name    base address      description 
   * 
   *yh      21             the nordsieck history array, of size nyh by 
   *                       (nqcur + 1), where nyh is the initial value 
   *                       of neq.  for j = 0,1,...,nqcur, column j+1 
   *                       of yh contains hcur**j/factorial(j) times 
   *                       the j-th derivative of the interpolating 
   *                       polynomial currently representing the solution, 
   *                       evaluated at t = tcur. 
   * 
   *acor     lenrw-neq+1   array of size neq used for the accumulated 
   *                       corrections on each step, scaled on output to 
   *                       represent the estimated local error in y on the 
   *                       last step. this is the vector e in the descrip- 
   *                       tion of the error control.  it is defined only 
   *                       on a return from lsodi with istate = 2. 
   * 
   * 
   *!part ii.  other routines callable. 
   * 
   *the following are optional calls which the user may make to 
   *gain additional capabilities in conjunction with lsodi. 
   *(the routines xsetun and xsetf are designed to conform to the 
   *slatec error handling package.) 
   * 
   *    form of call                  function 
   *  call xsetun(lun)          set the int unit number, lun, for 
   *                            output of messages from lsodi, if 
   *                            the default is not desired. 
   *                            the default value of lun is 6. 
   * 
   *  call xsetf(mflag)         set a flag to control the printing of 
   *                            messages by lsodi. 
   *                            mflag = 0 means do not print. (danger.. 
   *                            this risks losing valuable information.) 
   *                            mflag = 1 means print (the default). 
   * 
   *                            either of the above calls may be made at 
   *                            any time and will take effect immediately. 
   * 
   *  call svcom (rsav, isav)   store in rsav and isav the contents 
   *                            of the internal common blocks used by 
   *                            lsodi (see part iii below). 
   *                            rsav must be a real array of length 219 
   *                            or more, and isav must be an int 
   *                            array of length 41 or more. 
   * 
   *  call rscom (rsav, isav)   restore, from rsav and isav, the contents 
   *                            of the internal common blocks used by 
   *                            lsodi.  presumes a prior call to svcom 
   *                            with the same arguments. 
   * 
   *                            svcom and rscom are useful if 
   *                            interrupting a run and restarting 
   *                            later, or alternating between two or 
   *                            more problems solved with lsodi. 
   * 
   *  call intdy(,,,,,)         provide derivatives of y, of various 
   *       (see below)          orders, at a specified point t, if 
   *                            desired.  it may be called only after 
   *                            a successful return from lsodi. 
   * 
   *the detailed instructions for using intdy are as follows. 
   *the form of the call is.. 
   * 
   *  call intdy (t, k, rwork(21), nyh, dky, iflag) 
   * 
   *the input parameters are.. 
   * 
   *t         = value of independent variable where answers are desired 
   *            (normally the same as the t last returned by lsodi). 
   *            for valid results, t must lie between tcur - hu and tcur. 
   *            (see optional outputs for tcur and hu.) 
   *k         = int order of the derivative desired.  k must satisfy 
   *            0 .le. k .le. nqcur, where nqcur is the current order 
   *            (see optional outputs).  the capability corresponding 
   *            to k = 0, i.e. computing y(t), is already provided 
   *            by lsodi directly.  since nqcur .ge. 1, the first 
   *            derivative dy/dt is always available with intdy. 
   *rwork(21) = the base address of the history array yh. 
   *nyh       = column length of yh, equal to the initial value of neq. 
   * 
   *the output parameters are.. 
   * 
   *dky       = a real array of length neq containing the computed value 
   *            of the k-th derivative of y(t). 
   *iflag     = int flag, returned as 0 if k and t were legal, 
   *            -1 if k was illegal, and -2 if t was illegal. 
   *            on an error return, a message is also written. 
   * 
   *!part iii.  common blocks. 
   * 
   *if lsodi is to be used in an overlay situation, the user 
   *must declare, in the primary overlay, the variables in.. 
   *  (1) the call sequence to lsodi, 
   *  (2) the two internal common blocks 
   *        /ls0001/  of length  258  (219 double precision words 
   *                        followed by 39 int words), 
   *        /eh0001/  of length  2 (int words). 
   * 
   *if lsodi is used on a system in which the contents of internal 
   *common blocks are not preserved between calls, the user should 
   *declare the above two common blocks in his main program to insure 
   *that their contents are preserved. 
   * 
   *if the solution of a given problem by lsodi is to be interrupted 
   *and then later continued, such as when restarting an interrupted run 
   *or alternating between two or more problems, the user should save, 
   *following the return from the last lsodi call prior to the 
   *interruption, the contents of the call sequence variables and the 
   *internal common blocks, and later restore these values before the 
   *next lsodi call for that problem.  to save and restore the common 
   *blocks, use subroutines svcom and rscom (see part ii above). 
   * 
   * 
   *!part iv.  optionally replaceable solver routines. 
   * 
   *below are descriptions of two routines in the lsodi package which 
   *relate to the measurement of errors.  either routine can be 
   *replaced by a user-supplied version, if desired.  however, since such 
   *a replacement may have a major impact on performance, it should be 
   *done only when absolutely necessary, and only with great caution. 
   *(note.. the means by which the package version of a routine is 
   *superseded by the user-s version may be system-dependent.) 
   * 
   *(a) ewset. 
   *the following subroutine is called just before each internal 
   *integration step, and sets the array of error weights, ewt, as 
   *described under itol/rtol/atol above.. 
   *    subroutine ewset (neq, itol, rtol, atol, ycur, ewt) 
   *where neq, itol, rtol, and atol are as in the lsodi call sequence, 
   *ycur contains the current dependent variable vector, and 
   *ewt is the array of weights set by ewset. 
   * 
   *if the user supplies this subroutine, it must return in ewt(i) 
   *(i = 1,...,neq) a positive quantity suitable for comparing errors 
   *in y(i) to.  the ewt array returned by ewset is passed to the 
   *vnorm routine (see below), and also used by lsodi in the computation 
   *of the optional output imxer, the diagonal jacobian approximation, 
   *and the increments for difference quotient jacobians. 
   * 
   *in the user-supplied version of ewset, it may be desirable to use 
   *the current values of derivatives of y.  derivatives up to order nq 
   *are available from the history array yh, described above under 
   *optional outputs.  in ewset, yh is identical to the ycur array, 
   *extended to nq + 1 columns with a column length of nyh and scale 
   *factors of h**j/factorial(j).  on the first call for the problem, 
   *given by nst = 0, nq is 1 and h is temporarily set to 1.0. 
   *the quantities nq, nyh, h, and nst can be obtained by including 
   *in ewset the statements.. 
   *    double precision h, rls 
   *    common /ls0001/ rls(219),ils(39) 
   *    nq = ils(35) 
   *    nyh = ils(14) 
   *    nst = ils(36) 
   *    h = rls(213) 
   *thus, for example, the current value of dy/dt can be obtained as 
   *ycur(nyh+i)/h  (i=1,...,neq)  (and the division by h is 
   *unnecessary when nst = 0). 
   * 
   *(b) vnorm. 
   *the following is a real function routine which computes the weighted 
   *root-mean-square norm of a vector v.. 
   *    d = vnorm (n, v, w) 
   *where.. 
   *  n = the length of the vector, 
   *  v = real array of length n containing the vector, 
   *  w = real array of length n containing weights, 
   *  d = sqrt( (1/n) * sum(v(i)*w(i))**2 ). 
   *vnorm is called with n = neq and with w(i) = 1.0/ewt(i), where 
   *ewt is as set by subroutine ewset. 
   * 
   *if the user supplies this function, it should return a non-negative 
   *value of vnorm suitable for use in the error control in lsodi. 
   *none of the arguments should be altered by vnorm. 
   *for example, a user-supplied vnorm routine might.. 
   *  -substitute a max-norm of (v(i)*w(i)) for the rms-norm, or 
   *  -ignore some components of v in the norm, with the effect of 
   *   suppressing the error control on those components of y. 
   * 
   * 
   *!other routines in the lsodi package. 
   * 
   *in addition to subroutine lsodi, the lsodi package includes the 
   *following subroutines and function routines.. 
   * ainvg    computes the initial value of the vector 
   *            dy/dt = inverse(a) * g 
   * intdy    computes an interpolated value of the y vector at t = tout. 
   * stodi    is the core integrator, which does one step of the 
   *          integration and the associated error control. 
   * cfode    sets all method coefficients and test constants. 
   * prepji   computes and preprocesses the jacobian matrix j = df/dy 
   *          and the newton iteration matrix p = i - h*l0*j. 
   * solsy    manages solution of linear system in chord iteration. 
   * ewset    sets the error weight vector ewt before each step. 
   * vnorm    computes the weighted r.m.s. norm of a vector. 
   * svcom and rscom   are user-callable routines to save and restore, 
   *          respectively, the contents of the internal common blocks. 
   * dgefa and dgesl   are routines from linpack for solving full 
   *          systems of linear algebraic equations. 
   * dgbfa and dgbsl   are routines from linpack for solving banded 
   *          linear systems. 
   * daxpy, dscal, idamax, and ddot   are basic linear algebra modules 
   *          (blas) used by the above linpack routines. 
   * dlamch   computes the unit roundoff in a machine-independent manner. 
   * xerrwv, xsetun, and xsetf   handle the printing of all error 
   *          messages and warnings.  xerrwv is machine-dependent. 
   *note..  vnorm, idamax, ddot, and dlamch are function routines. 
   *all the others are subroutines. 
   * 
   *the intrinsic and external routines used by lsodi are..  abs, 
   *max, min, dble, abs, max, min, mod, sign, sqrt, and write. 
   * 
   *a block data subprogram is also included with the package, 
   *for loading some of the variables in internal common. 
   * 
   *!authors 
   *            jeffrey f. painter  and 
   *            alan c. hindmarsh 
   *            mathematics and statistics division, l-316 
   *            lawrence livermore national laboratory 
   *            livermore, ca 94550. 
   * 
   *!reference.. 
   *    alan c. hindmarsh,  lsode and lsodi, two new initial value 
   *    ordinary differential equation solvers, 
   *    acm-signum newsletter, vol. 15, no. 4 (1980), pp. 10-11. 
   * 
   *! 
   *this is the may 9, 1983 version of lsodi. 
   *this version is in double precision. 
   *----------------------------------------------------------------------- 
   *the following card is for optimized compilation on llnl compilers. 
   *lll. optimize 
   *----------------------------------------------------------------------- 
   *----------------------------------------------------------------------- 
   *the following internal common block contains 
   *(a) variables which are local to any subroutine but whose values must 
   *    be preserved between calls to the routine (own variables), and 
   *(b) variables which are communicated between subroutines. 
   *common block ls0001 is shared by the lsodi and lsode packages. 
   *the structure of ls0001 is as follows..  all real variables are 
   *listed first, followed by all ints.  within each type, the 
   *variables are grouped with those local to subroutine lsodi first, 
   *then those local to subroutine stodi, and finally those used 
   *for communication.  the block is declared in subroutines 
   *lsodi, intdy, stodi, prepji, and solsy.  groups of variables are 
   *replaced by dummy arrays in the common declarations in routines 
   *where those variables are not used. 
   *----------------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --neq;
  --y;
  --ydoti;
  --rtol;
  --atol;
  --rwork;
  --iwork;

  /* Function Body */
  /*----------------------------------------------------------------------- 
   *block a. 
   *this code block is executed on every call. 
   *it tests istate and itask for legality and branches appropiately. 
   *if istate .gt. 1 but the flag init shows that initialization has 
   *not yet been done, an error return occurs. 
   *if istate = 0 or 1 and tout = t, jump to block g and return 
   *immediately. 
   *----------------------------------------------------------------------- 
   */
  ierode_1.iero = 0;
  if (*istate < 0 || *istate > 3)
    {
      goto L601;
    }
  if (*itask < 1 || *itask > 5)
    {
      goto L602;
    }
  if (*istate <= 1)
    {
      goto L10;
    }
  if (ls0001_1.init == 0)
    {
      goto L603;
    }
  if (*istate == 2)
    {
      goto L200;
    }
  goto L20;
 L10:
  ls0001_1.init = 0;
  if (*tout == *t)
    {
      goto L430;
    }
 L20:
  ls0001_1.ntrep = 0;
  /*----------------------------------------------------------------------- 
   *block b. 
   *the next code block is executed for the initial call (istate = 0 or 1) 
   *or for a continuation call with parameter changes (istate = 3). 
   *it contains checking of all inputs and various initializations. 
   * 
   *first check legality of the non-optional inputs neq, itol, iopt, 
   *mf, ml, and mu. 
   *----------------------------------------------------------------------- 
   */
  if (neq[1] <= 0)
    {
      goto L604;
    }
  if (*istate <= 1)
    {
      goto L25;
    }
  if (neq[1] > ls0001_1.n)
    {
      goto L605;
    }
 L25:
  ls0001_1.n = neq[1];
  if (*itol < 1 || *itol > 4)
    {
      goto L606;
    }
  if (*iopt < 0 || *iopt > 1)
    {
      goto L607;
    }
  ls0001_1.meth = *mf / 10;
  ls0001_1.miter = *mf - ls0001_1.meth * 10;
  if (ls0001_1.meth < 1 || ls0001_1.meth > 2)
    {
      goto L608;
    }
  if (ls0001_1.miter <= 0 || ls0001_1.miter > 5)
    {
      goto L608;
    }
  if (ls0001_1.miter == 3)
    {
      goto L608;
    }
  if (ls0001_1.miter < 3)
    {
      goto L30;
    }
  ml = iwork[1];
  mu = iwork[2];
  if (ml < 0 || ml >= ls0001_1.n)
    {
      goto L609;
    }
  if (mu < 0 || mu >= ls0001_1.n)
    {
      goto L610;
    }
 L30:
  /*next process and check the optional inputs. -------------------------- 
   */
  if (*iopt == 1)
    {
      goto L40;
    }
  ls0001_1.maxord = mord[ls0001_1.meth - 1];
  ls0001_1.mxstep = mxstp0;
  ls0001_1.mxhnil = mxhnl0;
  if (*istate <= 1)
    {
      h0 = 0.;
    }
  ls0001_1.hmxi = 0.;
  ls0001_1.hmin = 0.;
  goto L60;
 L40:
  ls0001_1.maxord = iwork[5];
  if (ls0001_1.maxord < 0)
    {
      goto L611;
    }
  if (ls0001_1.maxord == 0)
    {
      ls0001_1.maxord = 100;
    }
  /*Computing MIN 
   */
  i__1 = ls0001_1.maxord, i__2 = mord[ls0001_1.meth - 1];
  ls0001_1.maxord = Min (i__1, i__2);
  ls0001_1.mxstep = iwork[6];
  if (ls0001_1.mxstep < 0)
    {
      goto L612;
    }
  if (ls0001_1.mxstep == 0)
    {
      ls0001_1.mxstep = mxstp0;
    }
  ls0001_1.mxhnil = iwork[7];
  if (ls0001_1.mxhnil < 0)
    {
      goto L613;
    }
  if (ls0001_1.mxhnil == 0)
    {
      ls0001_1.mxhnil = mxhnl0;
    }
  if (*istate > 1)
    {
      goto L50;
    }
  h0 = rwork[5];
  if ((*tout - *t) * h0 < 0.)
    {
      goto L614;
    }
 L50:
  hmax = rwork[6];
  if (hmax < 0.)
    {
      goto L615;
    }
  ls0001_1.hmxi = 0.;
  if (hmax > 0.)
    {
      ls0001_1.hmxi = 1. / hmax;
    }
  ls0001_1.hmin = rwork[7];
  if (ls0001_1.hmin < 0.)
    {
      goto L616;
    }
  /*----------------------------------------------------------------------- 
   *set work array pointers and check lengths lrw and liw. 
   *pointers to segments of rwork and iwork are named by prefixing l to 
   *the name of the segment.  e.g., the segment yh starts at rwork(lyh). 
   *segments of rwork (in order) are denoted yh, wm, ewt, savr, acor. 
   *----------------------------------------------------------------------- 
   */
 L60:
  ls0001_1.lyh = 21;
  if (*istate <= 1)
    {
      ls0001_1.nyh = ls0001_1.n;
    }
  ls0001_1.lwm = ls0001_1.lyh + (ls0001_1.maxord + 1) * ls0001_1.nyh;
  if (ls0001_1.miter <= 2)
    {
      lenwm = ls0001_1.n * ls0001_1.n + 2;
    }
  if (ls0001_1.miter >= 4)
    {
      lenwm = ((ml << 1) + mu + 1) * ls0001_1.n + 2;
    }
  ls0001_1.lewt = ls0001_1.lwm + lenwm;
  ls0001_1.lsavf = ls0001_1.lewt + ls0001_1.n;
  ls0001_1.lacor = ls0001_1.lsavf + ls0001_1.n;
  lenrw = ls0001_1.lacor + ls0001_1.n - 1;
  iwork[17] = lenrw;
  ls0001_1.liwm = 1;
  leniw = ls0001_1.n + 20;
  iwork[18] = leniw;
  if (lenrw > *lrw)
    {
      goto L617;
    }
  if (leniw > *liw)
    {
      goto L618;
    }
  /*check rtol and atol for legality. ------------------------------------ 
   */
  rtoli = rtol[1];
  atoli = atol[1];
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (*itol >= 3)
	{
	  rtoli = rtol[i__];
	}
      if (*itol == 2 || *itol == 4)
	{
	  atoli = atol[i__];
	}
      if (rtoli < 0.)
	{
	  goto L619;
	}
      if (atoli < 0.)
	{
	  goto L620;
	}
      /* L70: */
    }
  if (*istate <= 1)
    {
      goto L100;
    }
  /*if istate = 3, set flag to signal parameter changes to stodi. -------- 
   */
  ls0001_1.jstart = -1;
  if (ls0001_1.nq <= ls0001_1.maxord)
    {
      goto L90;
    }
  /*maxord was reduced below nq.  copy yh(*,maxord+2) into ydoti.--------- 
   */
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L80: */
      ydoti[i__] = rwork[i__ + ls0001_1.lwm - 1];
    }
  /*reload wm(1) = rwork(lwm), since lwm may have changed. --------------- 
   */
 L90:
  rwork[ls0001_1.lwm] = sqrt (ls0001_1.uround);
  if (ls0001_1.n == ls0001_1.nyh)
    {
      goto L200;
    }
  /*neq was reduced.  zero part of yh to avoid undefined references. ----- 
   */
  i1 = ls0001_1.lyh + ls0001_1.l * ls0001_1.nyh;
  i2 = ls0001_1.lyh + (ls0001_1.maxord + 1) * ls0001_1.nyh - 1;
  if (i1 > i2)
    {
      goto L200;
    }
  i__1 = i2;
  for (i__ = i1; i__ <= i__1; ++i__)
    {
      /* L95: */
      rwork[i__] = 0.;
    }
  goto L200;
  /*----------------------------------------------------------------------- 
   *block c. 
   *the next block is for the initial call only (istate = 0 or 1). 
   *it contains all remaining initializations, the call to ainvg 
   *(if istate = 1), and the calculation of the initial step size. 
   *the error weights in ewt are inverted after being loaded. 
   *----------------------------------------------------------------------- 
   */
 L100:
  ls0001_1.uround = nsp_dlamch ("p");
  ls0001_1.tn = *t;
  if (*itask != 4 && *itask != 5)
    {
      goto L105;
    }
  tcrit = rwork[1];
  if ((tcrit - *tout) * (*tout - *t) < 0.)
    {
      goto L625;
    }
  if (h0 != 0. && (*t + h0 - tcrit) * h0 > 0.)
    {
      h0 = tcrit - *t;
    }
 L105:
  ls0001_1.jstart = 0;
  rwork[ls0001_1.lwm] = sqrt (ls0001_1.uround);
  ls0001_1.nhnil = 0;
  ls0001_1.nst = 0;
  ls0001_1.nfe = 0;
  ls0001_1.nje = 0;
  ls0001_1.nslast = 0;
  ls0001_1.hu = 0.;
  ls0001_1.nqu = 0;
  ls0001_1.ccmax = .3;
  ls0001_1.maxcor = 3;
  ls0001_1.msbp = 20;
  ls0001_1.mxncf = 10;
  /*compute initial dy/dt, if necessary, and load it and initial y into yh 
   */
  lyd0 = ls0001_1.lyh + ls0001_1.nyh;
  lp = ls0001_1.lwm + 1;
  if (*istate == 1)
    {
      goto L120;
    }
  /*lsodi must compute initial dy/dt (lyd0 points to yh(*,2)). ----------- 
   */
  nsp_ode_ainvg (res, adda, &neq[1], t, &y[1], &rwork[lyd0],
		    &ls0001_1.miter, &ml, &mu, &rwork[lp], &iwork[21], &ier, param);
  ++ls0001_1.nfe;
  /*     if (ier)  560,110,565 
   */
  if (ier < 0)
    {
      goto L560;
    }
  else if (ier == 0)
    {
      goto L110;
    }
  else
    {
      goto L565;
    }
 L110:
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      rwork[i__ + ls0001_1.lyh - 1] = y[i__];
      /* L115: */
    }
  goto L130;
  /*    initial dy/dt has been supplied. ------------------------------------- 
   */
 L120:
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      rwork[i__ + ls0001_1.lyh - 1] = y[i__];
      rwork[i__ + lyd0 - 1] = ydoti[i__];
      /* L125: */
    }
  /*    load and invert the ewt array.  (h is temporarily set to 1.0.) ------- 
   */
 L130:
  ls0001_1.nq = 1;
  ls0001_1.h__ = 1.;
  nsp_ode_ewset (&ls0001_1.n, itol, &rtol[1], &atol[1],
		    &rwork[ls0001_1.lyh], &rwork[ls0001_1.lewt]);
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (rwork[i__ + ls0001_1.lewt - 1] <= 0.)
	{
	  goto L621;
	}
      /* L135: */
      rwork[i__ + ls0001_1.lewt - 1] = 1. / rwork[i__ + ls0001_1.lewt - 1];
    }
  /*----------------------------------------------------------------------- 
   *the coding below computes the step size, h0, to be attempted on the 
   *first step, unless the user has supplied a value for this. 
   *first check that tout - t differs significantly from zero. 
   *a scalar tolerance quantity tol is computed, as Max(rtol(i)) 
   *if this is positive, or Max(atol(i)/abs(y(i))) otherwise, adjusted 
   *so as to be between 100*uround and 1.0e-3. 
   *then the computed value h0 is given by.. 
   *                                     neq 
   *  h0**2 = tol / ( w0**-2 + (1/neq) * sum ( ydot(i)/ywt(i) )**2  ) 
   *                                      1 
   *where   w0      = Max( Abs(t), Abs(tout) ), 
   *        ydot(i) = i-th component of initial value of dy/dt, 
   *        ywt(i)  = ewt(i)/tol  (a weight for y(i)). 
   *the sign of h0 is inferred from the initial values of tout and t. 
   *----------------------------------------------------------------------- 
   */
  if (h0 != 0.)
    {
      goto L180;
    }
  tdist = (d__1 = *tout - *t, Abs (d__1));
  /*Computing MAX 
   */
  d__1 = Abs (*t), d__2 = Abs (*tout);
  w0 = Max (d__1, d__2);
  if (tdist < ls0001_1.uround * 2. * w0)
    {
      goto L622;
    }
  tol = rtol[1];
  if (*itol <= 2)
    {
      goto L145;
    }
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L140: */
      /*Computing MAX 
       */
      d__1 = tol, d__2 = rtol[i__];
      tol = Max (d__1, d__2);
    }
 L145:
  if (tol > 0.)
    {
      goto L160;
    }
  atoli = atol[1];
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (*itol == 2 || *itol == 4)
	{
	  atoli = atol[i__];
	}
      ayi = (d__1 = y[i__], Abs (d__1));
      if (ayi != 0.)
	{
	  /*Computing MAX 
	   */
	  d__1 = tol, d__2 = atoli / ayi;
	  tol = Max (d__1, d__2);
	}
      /* L150: */
    }
 L160:
  /*Computing MAX 
   */
  d__1 = tol, d__2 = ls0001_1.uround * 100.;
  tol = Max (d__1, d__2);
  tol = Min (tol, .001);
  sum = nsp_ode_vnorm (&ls0001_1.n, &rwork[lyd0], &rwork[ls0001_1.lewt]);
  /*Computing 2nd power 
   */
  d__1 = sum;
  sum = 1. / (tol * w0 * w0) + tol * (d__1 * d__1);
  h0 = 1. / sqrt (sum);
  h0 = Min (h0, tdist);
  d__1 = *tout - *t;
  h0 = d_sign (&h0, &d__1);
  /*adjust h0 if necessary to meet hmax bound. --------------------------- 
   */
 L180:
  rh = Abs (h0) * ls0001_1.hmxi;
  if (rh > 1.)
    {
      h0 /= rh;
    }
  /*load h with h0 and scale yh(*,2) by h0. ------------------------------ 
   */
  ls0001_1.h__ = h0;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L190: */
      rwork[i__ + lyd0 - 1] = h0 * rwork[i__ + lyd0 - 1];
    }
  goto L270;
  /*----------------------------------------------------------------------- 
   *block d. 
   *the next code block is for continuation calls only (istate = 2 or 3) 
   *and is to check stop conditions before taking a step. 
   *----------------------------------------------------------------------- 
   */
 L200:
  ls0001_1.nslast = ls0001_1.nst;
  switch (*itask)
    {
    case 1:
      goto L210;
    case 2:
      goto L250;
    case 3:
      goto L220;
    case 4:
      goto L230;
    case 5:
      goto L240;
    }
 L210:
  if ((ls0001_1.tn - *tout) * ls0001_1.h__ < 0.)
    {
      goto L250;
    }
  nsp_ode_intdy (tout, &c__0, &rwork[ls0001_1.lyh], &ls0001_1.nyh, &y[1],
		    &iflag);
  if (iflag != 0)
    {
      goto L627;
    }
  *t = *tout;
  goto L420;
 L220:
  tp = ls0001_1.tn - ls0001_1.hu * (ls0001_1.uround * 100. + 1.);
  if ((tp - *tout) * ls0001_1.h__ > 0.)
    {
      goto L623;
    }
  if ((ls0001_1.tn - *tout) * ls0001_1.h__ < 0.)
    {
      goto L250;
    }
  goto L400;
 L230:
  tcrit = rwork[1];
  if ((ls0001_1.tn - tcrit) * ls0001_1.h__ > 0.)
    {
      goto L624;
    }
  if ((tcrit - *tout) * ls0001_1.h__ < 0.)
    {
      goto L625;
    }
  if ((ls0001_1.tn - *tout) * ls0001_1.h__ < 0.)
    {
      goto L245;
    }
  nsp_ode_intdy (tout, &c__0, &rwork[ls0001_1.lyh], &ls0001_1.nyh, &y[1],
		    &iflag);
  if (iflag != 0)
    {
      goto L627;
    }
  *t = *tout;
  goto L420;
 L240:
  tcrit = rwork[1];
  if ((ls0001_1.tn - tcrit) * ls0001_1.h__ > 0.)
    {
      goto L624;
    }
 L245:
  hmx = Abs (ls0001_1.tn) + Abs (ls0001_1.h__);
  ihit = (d__1 =
	  ls0001_1.tn - tcrit, Abs (d__1)) <= ls0001_1.uround * 100. * hmx;
  if (ihit)
    {
      goto L400;
    }
  tnext = ls0001_1.tn + ls0001_1.h__ * (ls0001_1.uround * 4. + 1.);
  if ((tnext - tcrit) * ls0001_1.h__ <= 0.)
    {
      goto L250;
    }
  ls0001_1.h__ = (tcrit - ls0001_1.tn) * (1. - ls0001_1.uround * 4.);
  if (*istate == 2)
    {
      ls0001_1.jstart = -2;
    }
  /*----------------------------------------------------------------------- 
   *block e. 
   *the next block is normally executed for all calls and contains 
   *the call to the one-step core integrator stodi. 
   * 
   *this is a looping point for the integration steps. 
   * 
   *first check for too many steps being taken, update ewt (if not at 
   *start of problem), check for too much accuracy being requested, and 
   *check for h below the roundoff level in t. 
   *----------------------------------------------------------------------- 
   */
 L250:
  if (ls0001_1.nst - ls0001_1.nslast >= ls0001_1.mxstep)
    {
      goto L500;
    }
  nsp_ode_ewset (&ls0001_1.n, itol, &rtol[1], &atol[1],
		    &rwork[ls0001_1.lyh], &rwork[ls0001_1.lewt]);
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (rwork[i__ + ls0001_1.lewt - 1] <= 0.)
	{
	  goto L510;
	}
      /* L260: */
      rwork[i__ + ls0001_1.lewt - 1] = 1. / rwork[i__ + ls0001_1.lewt - 1];
    }
 L270:
  tolsf =
    ls0001_1.uround * nsp_ode_vnorm (&ls0001_1.n, &rwork[ls0001_1.lyh],
					&rwork[ls0001_1.lewt]);
  if (tolsf <= 1.)
    {
      goto L280;
    }
  tolsf *= 2.;
  if (ls0001_1.nst == 0)
    {
      goto L626;
    }
  goto L520;
 L280:
  if (ls0001_1.tn + ls0001_1.h__ != ls0001_1.tn)
    {
      goto L290;
    }
  ++ls0001_1.nhnil;
  if (ls0001_1.nhnil > ls0001_1.mxhnil)
    {
      goto L290;
    }
  C2F (xerrwv) ("lsodi--  attention.. t (=r1) and h (=r2) are", &c__50,
		&c__101, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b84,
		&c_b84, 44L);
  C2F (xerrwv) ("    such that  t + h = t at next step", &c__60, &c__101,
		&c__1, &c__0, &c__0, &c__0, &c__0, &c_b84, &c_b84, 37L);
  C2F (xerrwv) ("      (h = pas). integration continues", &c__50,
		&c__101, &c__1, &c__0, &c__0, &c__0, &c__2, &ls0001_1.tn,
		&ls0001_1.h__, 38L);
  if (ls0001_1.nhnil < ls0001_1.mxhnil)
    {
      goto L290;
    }
  C2F (xerrwv) ("lsodi--  previous message has been given i1 times",
		&c__50, &c__102, &c__1, &c__0, &c__0, &c__0, &c__0,
		&c_b84, &c_b84, 49L);
  C2F (xerrwv) ("     it will not be repeated", &c__50, &c__102, &c__1,
		&c__1, &ls0001_1.mxhnil, &c__0, &c__0, &c_b84, &c_b84, 28L);
 L290:
  /*----------------------------------------------------------------------- 
   *    call stodi(neq,y,yh,nyh,yh1,ewt,savf,savr,acor,wm,iwm,res, 
   *               adda,jac,prepji,solsy) 
   *note... savf in stodi occupies the same space as ydoti in lsodi. 
   *----------------------------------------------------------------------- 
   */
  nsp_ode_stodi (&neq[1], &y[1], &rwork[ls0001_1.lyh], &ls0001_1.nyh,
		    &rwork[ls0001_1.lyh], &rwork[ls0001_1.lewt], &ydoti[1],
		    &rwork[ls0001_1.lsavf], &rwork[ls0001_1.lacor],
		    &rwork[ls0001_1.lwm], &iwork[ls0001_1.liwm], res,
		    adda, jac, nsp_ode_prepji, nsp_ode_solsy, param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  kgo = 1 - ls0001_1.kflag;
  switch (kgo)
    {
    case 1:
      goto L300;
    case 2:
      goto L530;
    case 3:
      goto L540;
    case 4:
      goto L400;
    case 5:
      goto L550;
    }
  /* 
   *kgo = 1,success. 2,error test failure. 3,convergence failure. 
   *      4,res ordered return. 5,res returned error. 
   *----------------------------------------------------------------------- 
   *block f. 
   *the following block handles the case of a successful return from the 
   *core integrator (kflag = 0).  test for stop conditions. 
   *----------------------------------------------------------------------- 
   */
 L300:
  ls0001_1.init = 1;
  switch (*itask)
    {
    case 1:
      goto L310;
    case 2:
      goto L400;
    case 3:
      goto L330;
    case 4:
      goto L340;
    case 5:
      goto L350;
    }
  /*itask = 1.  if tout has been reached, interpolate. ------------------- 
   */
 L310:
  if ((ls0001_1.tn - *tout) * ls0001_1.h__ < 0.)
    {
      goto L250;
    }
  nsp_ode_intdy (tout, &c__0, &rwork[ls0001_1.lyh], &ls0001_1.nyh, &y[1],
		    &iflag);
  *t = *tout;
  goto L420;
  /*itask = 3.  jump to exit if tout was reached. ------------------------ 
   */
 L330:
  if ((ls0001_1.tn - *tout) * ls0001_1.h__ >= 0.)
    {
      goto L400;
    }
  goto L250;
  /*itask = 4.  see if tout or tcrit was reached.  adjust h if necessary. 
   */
 L340:
  if ((ls0001_1.tn - *tout) * ls0001_1.h__ < 0.)
    {
      goto L345;
    }
  nsp_ode_intdy (tout, &c__0, &rwork[ls0001_1.lyh], &ls0001_1.nyh, &y[1],
		    &iflag);
  *t = *tout;
  goto L420;
 L345:
  hmx = Abs (ls0001_1.tn) + Abs (ls0001_1.h__);
  ihit = (d__1 =
	  ls0001_1.tn - tcrit, Abs (d__1)) <= ls0001_1.uround * 100. * hmx;
  if (ihit)
    {
      goto L400;
    }
  tnext = ls0001_1.tn + ls0001_1.h__ * (ls0001_1.uround * 4. + 1.);
  if ((tnext - tcrit) * ls0001_1.h__ <= 0.)
    {
      goto L250;
    }
  ls0001_1.h__ = (tcrit - ls0001_1.tn) * (1. - ls0001_1.uround * 4.);
  ls0001_1.jstart = -2;
  goto L250;
  /*itask = 5.  see if tcrit was reached and jump to exit. --------------- 
   */
 L350:
  hmx = Abs (ls0001_1.tn) + Abs (ls0001_1.h__);
  ihit = (d__1 =
	  ls0001_1.tn - tcrit, Abs (d__1)) <= ls0001_1.uround * 100. * hmx;
  /*----------------------------------------------------------------------- 
   *block g. 
   *the following block handles all successful returns from lsodi. 
   *if itask .ne. 1, y is loaded from yh and t is set accordingly. 
   *istate is set to 2, the illegal input counter is zeroed, and the 
   *optional outputs are loaded into the work arrays before returning.  if 
   *istate = 0 or 1 and tout = t, there is a return with no action taken, 
   *except that if this has happened repeatedly, the run is terminated. 
   *----------------------------------------------------------------------- 
   */
 L400:
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L410: */
      y[i__] = rwork[i__ + ls0001_1.lyh - 1];
    }
  *t = ls0001_1.tn;
  if (*itask != 4 && *itask != 5)
    {
      goto L420;
    }
  if (ihit)
    {
      *t = tcrit;
    }
 L420:
  *istate = 2;
  if (ls0001_1.kflag == -3)
    {
      *istate = 3;
    }
  ls0001_1.illin = 0;
  rwork[11] = ls0001_1.hu;
  rwork[12] = ls0001_1.h__;
  rwork[13] = ls0001_1.tn;
  iwork[11] = ls0001_1.nst;
  iwork[12] = ls0001_1.nfe;
  iwork[13] = ls0001_1.nje;
  iwork[14] = ls0001_1.nqu;
  iwork[15] = ls0001_1.nq;
  return 0;
  /* 
   */
 L430:
  ++ls0001_1.ntrep;
  if (ls0001_1.ntrep < 5)
    {
      return 0;
    }
  C2F (xerrwv)
    ("lsodi--  repeated calls with istate=0 or 1 and tout=t (r1)  ", &c__60,
     &c__301, &c__1, &c__0, &c__0, &c__0, &c__1, t, &c_b84, 60L);
  goto L800;
  /*----------------------------------------------------------------------- 
   *block h. 
   *the following block handles all unsuccessful returns other than 
   *those for illegal input.  first the error message routine is called. 
   *if there was an error test or convergence test failure, imxer is set. 
   *then y is loaded from yh, t is set to tn, and the illegal input 
   *counter illin is set to 0.  the optional outputs are loaded into 
   *the work arrays before returning. 
   *----------------------------------------------------------------------- 
   *the maximum number of steps was taken before reaching tout. ---------- 
   */
 L500:
  C2F (xerrwv) ("lsodi--  at t (=r1), mxstep (=i1) steps   ", &c__50,
		&c__201, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b84,
		&c_b84, 42L);
  C2F (xerrwv) ("necessary before reaching tout", &c__50, &c__201, &c__1,
		&c__1, &ls0001_1.mxstep, &c__0, &c__1, &ls0001_1.tn,
		&c_b84, 30L);
  *istate = -1;
  goto L580;
  /*ewt(i) .le. 0.0 for some i (not at start of problem). ---------------- 
   */
 L510:
  ewti = rwork[ls0001_1.lewt + i__ - 1];
  C2F (xerrwv) ("lsodi--  at t (=r1), ewt(i1) (r2) is .le. 0", &c__50,
		&c__202, &c__1, &c__1, &i__, &c__0, &c__2, &ls0001_1.tn,
		&ewti, 43L);
  *istate = -6;
  goto L590;
 L520:
  C2F (xerrwv) ("lsodi--  at t (=r1),  too much precision required",
		&c__50, &c__203, &c__1, &c__0, &c__0, &c__0, &c__0,
		&c_b84, &c_b84, 49L);
  /*too much accuracy requested for machine precision. ------------------- 
   */
  C2F (xerrwv) (" w.r.t. machine precision  tolsf (=r2) ", &c__50,
		&c__203, &c__1, &c__0, &c__0, &c__0, &c__2, &ls0001_1.tn,
		&tolsf, 39L);
  rwork[14] = tolsf;
  *istate = -2;
  goto L590;
  /*kflag = -1.  error test failed repeatedly or with Abs(h) = hmin. ----- 
   */
 L530:
  C2F (xerrwv) ("lsodi--  at t(=r1) anf for h(=r2), error", &c__50,
		&c__204, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b84,
		&c_b84, 40L);
  C2F (xerrwv) ("      test failed with Abs(h) = hmin", &c__50, &c__204,
		&c__1, &c__0, &c__0, &c__0, &c__2, &ls0001_1.tn,
		&ls0001_1.h__, 36L);
  *istate = -4;
  goto L570;
  /*kflag = -2.  convergence failed repeatedly or with Abs(h) = hmin. ---- 
   */
 L540:
  C2F (xerrwv) ("lsodi--  at t (=r1) for step h (=r2), le", &c__50,
		&c__205, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b84,
		&c_b84, 40L);
  C2F (xerrwv) ("    corrector does not converge ", &c__50, &c__205,
		&c__1, &c__0, &c__0, &c__0, &c__0, &c_b84, &c_b84, 32L);
  C2F (xerrwv) ("      with Abs(h) = hmin   ", &c__30, &c__205, &c__1,
		&c__0, &c__0, &c__0, &c__2, &ls0001_1.tn, &ls0001_1.h__, 27L);
  *istate = -5;
  goto L570;
  /*ires = 3 returned by res, despite retries by stodi. ------------------ 
   */
 L550:
  C2F (xerrwv) ("lsodi--  at t (=r1) repeated error (ires=3) due to ",
		&c__50, &c__206, &c__1, &c__0, &c__0, &c__0, &c__0,
		&c_b84, &c_b84, 51L);
  C2F (xerrwv) ("routine which evaluates the residue", &c__30, &c__206,
		&c__1, &c__0, &c__0, &c__0, &c__1, &ls0001_1.tn, &c_b84, 35L);
  *istate = -7;
  goto L590;
  /*ainvg failed because a-matrix was singular. -------------------------- 
   */
 L560:
  ier = -ier;
  C2F (xerrwv) ("lsodi-- initialization failed dy/dt: singular matrix",
		&c__60, &c__207, &c__1, &c__0, &c__0, &c__0, &c__0,
		&c_b84, &c_b84, 52L);
  C2F (xerrwv) ("dgefa or dgbfa return info=(i1)", &c__50, &c__207,
		&c__1, &c__1, &ier, &c__0, &c__0, &c_b84, &c_b84, 31L);
  *istate = -8;
  return 0;
  /*ainvg failed because res set ires to 2 or 3. ------------------------- 
   */
 L565:
  C2F (xerrwv) ("lsodi--  initialisation failed dy/dt:  routine", &c__50,
		&c__208, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b84,
		&c_b84, 46L);
  C2F (xerrwv) ("      of residue evaluation returns:", &c__50, &c__208,
		&c__1, &c__0, &c__0, &c__0, &c__0, &c_b84, &c_b84, 36L);
  C2F (xerrwv) ("       ires = (i1)", &c__20, &c__208, &c__1, &c__1,
		&ier, &c__0, &c__0, &c_b84, &c_b84, 18L);
  *istate = -8;
  return 0;
  /*compute imxer if relevant. ------------------------------------------- 
   */
 L570:
  big = 0.;
  imxer = 1;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      size = (d__1 =
	      rwork[i__ + ls0001_1.lacor - 1] * rwork[i__ + ls0001_1.lewt -
						      1], Abs (d__1));
      if (big >= size)
	{
	  goto L575;
	}
      big = size;
      imxer = i__;
    L575:
      ;
    }
  iwork[16] = imxer;
  /*compute residual if relevant. ---------------------------------------- 
   */
 L580:
  lyd0 = ls0001_1.lyh + ls0001_1.nyh;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      rwork[i__ + ls0001_1.lsavf - 1] = rwork[i__ + lyd0 - 1] / ls0001_1.h__;
      /* L585: */
      y[i__] = rwork[i__ + ls0001_1.lyh - 1];
    }
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &rwork[ls0001_1.lsavf], &ydoti[1],
	  &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires <= 1)
    {
      goto L595;
    }
  C2F (xerrwv) ("lsodi--  routine for evaluation od residue returns",
		&c__50, &c__210, &c__1, &c__0, &c__0, &c__0, &c__0,
		&c_b84, &c_b84, 50L);
  C2F (xerrwv) ("    ires=i1 ", &c__50, &c__210, &c__1, &c__1, &ires,
		&c__0, &c__0, &c_b84, &c_b84, 12L);
  goto L595;
  /*set y vector, t, illin, and optional outputs. ------------------------ 
   */
 L590:
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L592: */
      y[i__] = rwork[i__ + ls0001_1.lyh - 1];
    }
 L595:
  *t = ls0001_1.tn;
  ls0001_1.illin = 0;
  rwork[11] = ls0001_1.hu;
  rwork[12] = ls0001_1.h__;
  rwork[13] = ls0001_1.tn;
  iwork[11] = ls0001_1.nst;
  iwork[12] = ls0001_1.nfe;
  iwork[13] = ls0001_1.nje;
  iwork[14] = ls0001_1.nqu;
  iwork[15] = ls0001_1.nq;
  return 0;
  /*----------------------------------------------------------------------- 
   *block i. 
   *the following block handles all error returns due to illegal input 
   *(istate = -3), as detected before calling the core integrator. 
   *first the error message routine is called.  then if there have been 
   *5 consecutive such returns just before this call to the solver, 
   *the run is halted. 
   *----------------------------------------------------------------------- 
   */
 L601:
  C2F (xerrwv) ("lsodi--  istate (=i1) illegal ", &c__30, &c__1, &c__1,
		&c__1, istate, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L602:
  C2F (xerrwv) ("lsodi--  itask (=i1) illegal  ", &c__30, &c__2, &c__1,
		&c__1, itask, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L603:
  C2F (xerrwv) ("lsodi--  istate .gt. 1 ", &c__50, &c__3, &c__1, &c__0,
		&c__0, &c__0, &c__0, &c_b84, &c_b84, 23L);
  goto L700;
 L604:
  C2F (xerrwv) ("lsodi--  neq (=i1) .lt. 1     ", &c__30, &c__4, &c__1,
		&c__1, &neq[1], &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L605:
  C2F (xerrwv) ("lsodi--  istate = 3 et neq jumps from i1 to i2", &c__50,
		&c__5, &c__1, &c__2, &ls0001_1.n, &neq[1], &c__0, &c_b84,
		&c_b84, 46L);
  goto L700;
 L606:
  C2F (xerrwv) ("lsodi--  itol (=i1) illegal   ", &c__30, &c__6, &c__1,
		&c__1, itol, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L607:
  C2F (xerrwv) ("lsodi--  iopt (=i1) illegal   ", &c__30, &c__7, &c__1,
		&c__1, iopt, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L608:
  C2F (xerrwv) ("lsodi--  mf (=i1) illegal     ", &c__30, &c__8, &c__1,
		&c__1, mf, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L609:
  C2F (xerrwv) ("lsodi--  ml (=i1) illegal.. .lt.0 or .ge.neq (=i2)",
		&c__50, &c__9, &c__1, &c__2, &ml, &neq[1], &c__0, &c_b84,
		&c_b84, 50L);
  goto L700;
 L610:
  C2F (xerrwv) ("lsodi--  mu (=i1) illegal.. .lt.0 or .ge.neq (=i2)",
		&c__50, &c__10, &c__1, &c__2, &mu, &neq[1], &c__0,
		&c_b84, &c_b84, 50L);
  goto L700;
 L611:
  C2F (xerrwv) ("lsodi--  maxord (=i1) .lt. 0  ", &c__30, &c__11, &c__1,
		&c__1, &ls0001_1.maxord, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L612:
  C2F (xerrwv) ("lsodi--  mxstep (=i1) .lt. 0  ", &c__30, &c__12, &c__1,
		&c__1, &ls0001_1.mxstep, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L613:
  C2F (xerrwv) ("lsodi--  mxhnil (=i1) .lt. 0  ", &c__30, &c__13, &c__1,
		&c__1, &ls0001_1.mxhnil, &c__0, &c__0, &c_b84, &c_b84, 30L);
  goto L700;
 L614:
  C2F (xerrwv) ("lsodi--  tout (=r1)  .gt.  t (=r2)      ", &c__40,
		&c__14, &c__1, &c__0, &c__0, &c__0, &c__2, tout, t, 40L);
  C2F (xerrwv) ("      h0 (=r1) gives integration direction", &c__50,
		&c__14, &c__1, &c__0, &c__0, &c__0, &c__1, &h0, &c_b84, 42L);
  goto L700;
 L615:
  C2F (xerrwv) ("lsodi--  hmax (=r1) .lt. 0.0  ", &c__30, &c__15, &c__1,
		&c__0, &c__0, &c__0, &c__1, &hmax, &c_b84, 30L);
  goto L700;
 L616:
  C2F (xerrwv) ("lsodi--  hmin (=r1) .lt. 0.0  ", &c__30, &c__16, &c__1,
		&c__0, &c__0, &c__0, &c__1, &ls0001_1.hmin, &c_b84, 30L);
  goto L700;
 L617:
  C2F (xerrwv) ("lsodi-- necessary size for  rwork (i1) larger than i2",
		&c__60, &c__17, &c__1, &c__2, &lenrw, lrw, &c__0, &c_b84,
		&c_b84, 53L);
  goto L700;
 L618:
  C2F (xerrwv) ("lsodi-- necessary size for  iwork (i1) larger than i2",
		&c__60, &c__18, &c__1, &c__2, &leniw, liw, &c__0, &c_b84,
		&c_b84, 53L);
  goto L700;
 L619:
  C2F (xerrwv) ("lsodi--  rtol(i1) is r1 .lt. 0.0        ", &c__40,
		&c__19, &c__1, &c__1, &i__, &c__0, &c__1, &rtoli, &c_b84,
		40L);
  goto L700;
 L620:
  C2F (xerrwv) ("lsodi--  atol(i1) is r1 .lt. 0.0        ", &c__40,
		&c__20, &c__1, &c__1, &i__, &c__0, &c__1, &atoli, &c_b84,
		40L);
  goto L700;
 L621:
  ewti = rwork[ls0001_1.lewt + i__ - 1];
  C2F (xerrwv) ("lsodi--  ewt(i1) (=r1) is  .le. 0.0         ", &c__40,
		&c__21, &c__1, &c__1, &i__, &c__0, &c__1, &ewti, &c_b84, 44L);
  goto L700;
 L622:
  C2F (xerrwv) ("lsodi--  tout (=r1) too close to t(=r2) ", &c__60,
		&c__22, &c__1, &c__0, &c__0, &c__0, &c__2, tout, t, 40L);
  goto L700;
 L623:
  C2F (xerrwv)
    ("lsodi--  itask = i1 and tout (=r1) .gt. tcur - hu (= r2)  ", &c__60,
     &c__23, &c__1, &c__1, itask, &c__0, &c__2, tout, &tp, 58L);
  goto L700;
 L624:
  C2F (xerrwv)
    ("lsodi--  itask = 4 or 5 and tcrit (=r1) .gt. tcur (=r2)   ", &c__60,
     &c__24, &c__1, &c__0, &c__0, &c__0, &c__2, &tcrit, &ls0001_1.tn, 58L);
  goto L700;
 L625:
  C2F (xerrwv)
    ("lsodi--  itask = 4 or 5 and tcrit (=r1)  .gt.  tout (=r2)", &c__60,
     &c__25, &c__1, &c__0, &c__0, &c__0, &c__2, &tcrit, tout, 57L);
  goto L700;
 L626:
  C2F (xerrwv) ("lsodi-- too much accuracy required", &c__50, &c__26,
		&c__1, &c__0, &c__0, &c__0, &c__0, &c_b84, &c_b84, 34L);
  C2F (xerrwv) ("w.r.t machine precision tolsf (=r1)", &c__60, &c__26,
		&c__1, &c__0, &c__0, &c__0, &c__1, &tolsf, &c_b84, 35L);
  rwork[14] = tolsf;
  goto L700;
 L627:
  C2F (xerrwv) ("lsodi--  problems due to intdy. itask=i1,tout=r1",
		&c__50, &c__27, &c__1, &c__1, itask, &c__0, &c__1, tout,
		&c_b84, 48L);
  /* 
   */
 L700:
  if (ls0001_1.illin == 5)
    {
      goto L710;
    }
  ++ls0001_1.illin;
  *istate = -3;
  return 0;
 L710:
  C2F (xerrwv) ("lsodi--  incorrect inputs", &c__50, &c__302, &c__1,
		&c__0, &c__0, &c__0, &c__0, &c_b84, &c_b84, 25L);
  /* 
   */
 L800:
  C2F (xerrwv) ("lsodi-- infinite loop", &c__50, &c__303, &c__2, &c__0,
		&c__0, &c__0, &c__0, &c_b84, &c_b84, 21L);
  return 0;
  /*----------------------- end of subroutine lsodi ----------------------- 
   */
}				/* lsodi_ */
