#include "integ.h"


#define ls0001_1 ls0001_._1

ode_ls0001 ls0001_;
ode_lsa lsa001_;
ode_lsr lsr001_;
ode_err ierode_;

/* Table of constant values */

static int c__0 = 0;
static int c__6 = 6;
static int c__1101 = 1101;
static int c__1 = 1;
static int c__2 = 2;
static int c__60 = 60;
static int c__301 = 301;
static double c_b98 = 0.;
static int c__201 = 201;
static int c__202 = 202;
static int c__203 = 203;
static int c__50 = 50;
static int c__204 = 204;
static int c__205 = 205;
static int c__30 = 30;
static int c__3 = 3;
static int c__4 = 4;
static int c__5 = 5;
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
static int c__20 = 20;
static int c__21 = 21;
static int c__22 = 22;
static int c__23 = 23;
static int c__24 = 24;
static int c__25 = 25;
static int c__26 = 26;
static int c__27 = 27;
static int c__302 = 302;
static int c__303 = 303;
static double c_d0 = 0.0;

int
C2F(lsode) (ode_f f, int *neq, double *y, double *t, double *tout,
	    int *itol, double *rtol,const double *atol, int *itask,
	    int *istate, int *iopt, double *rwork, int *lrw, int *iwork,
	    int *liw, ode_jac jac, int *mf, void *param)
{
  /* Initialized data */

  static int mord[2] = { 12, 5 };
  static int mxstp0 = 10000;
  static int mxhnl0 = 10;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double atoli, ayi, hmx, tol, sum,  big,  ewti, size,  h0,  hmax,  rh,  rtoli, tdist, tolsf,  tcrit,  tnext,  tp,  w0;
  int i1, i2;
  int i__, iflag;
  int ihit=0;
  int imxer;
  int kgo;
  int leniw;
  int lenrw;
  int lenwm;
  int lf0;
  int ml;
  int mu;

  /* 
   *!purpose 
   *livermore solver for ordinary differential equations. 
   *this version is in double precision. 
   * 
   *lsode solves the initial value problem for stiff or nonstiff 
   *systems of first order ode-s, 
   *    dy/dt = f(t,y) ,  or, in component form, 
   *    dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq). 
   *lsode is a package based on the gear and gearb packages, and on the 
   *october 23, 1978 version of the tentative odepack user interface 
   *standard, with minor modifications. 
   * 
   *!summary of usage. 
   * 
   *communication between the user and the lsode package, for normal 
   *situations, is summarized here.  this summary describes only a subset 
   *of the full set of options available.  see the full description for 
   *details, including optional communication, nonstandard options, 
   *and instructions for special situations.  see also the example 
   *problem (with program and output) following this summary. 
   * 
   *a. first provide a subroutine of the form.. 
   *              subroutine f (neq, t, y, ydot,param) 
   *              dimension y(neq), ydot(neq) 
   *              double precision param   ! use as a pointer to datas for external written in C 
   *which supplies the vector function f by loading ydot(i) with f(i). 
   * 
   *b. next determine (or guess) whether or not the problem is stiff. 
   *stiffness occurs when the jacobian matrix df/dy has an eigenvalue 
   *whose real part is negative and large in magnitude, compared to the 
   *reciprocal of the t span of interest.  if the problem is nonstiff, 
   *use a method flag mf = 10.  if it is stiff, there are four standard 
   *choices for mf, and lsode requires the jacobian matrix in some form. 
   *this matrix is regarded either as full (mf = 21 or 22), 
   *or banded (mf = 24 or 25).  in the banded case, lsode requires two 
   *half-bandwidth parameters ml and mu.  these are, respectively, the 
   *widths of the lower and upper parts of the band, excluding the main 
   *diagonal.  thus the band consists of the locations (i,j) with 
   *i-ml .le. j .le. i+mu, and the full bandwidth is ml+mu+1. 
   * 
   *c. if the problem is stiff, you are encouraged to supply the jacobian 
   *directly (mf = 21 or 24), but if this is not feasible, lsode will 
   *compute it internally by difference quotients (mf = 22 or 25). 
   *if you are supplying the jacobian, provide a subroutine of the form.. 
   *              subroutine jac (neq, t, y, ml, mu, pd, nrowpd) 
   *              dimension y(neq), pd(nrowpd,neq) 
   *which supplies df/dy by loading pd as follows.. 
   *    for a full jacobian (mf = 21), load pd(i,j) with df(i)/dy(j), 
   *the partial derivative of f(i) with respect to y(j).  (ignore the 
   *ml and mu arguments in this case.) 
   *    for a banded jacobian (mf = 24), load pd(i-j+mu+1,j) with 
   *df(i)/dy(j), i.e. load the diagonal lines of df/dy into the rows of 
   *pd from the top down. 
   *    in either case, only nonzero elements need be loaded. 
   * 
   *d. write a main program which calls subroutine lsode once for 
   *each point at which answers are desired.  this should also provide 
   *for possible use of int unit 6 for output of error messages 
   *by lsode.  on the first call to lsode, supply arguments as follows.. 
   *f      = name of subroutine for right-hand side vector f. 
   *         this name must be declared external in calling program. 
   *neq    = number of first order ode-s. 
   *y      = array of initial values, of length neq. 
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
   *istate = int flag (input and output).  set istate = 1. 
   *iopt   = 0 to indicate no optional inputs used. 
   *rwork  = real work array of length at least.. 
   *            20 + 16*neq                    for mf = 10, 
   *            22 +  9*neq + neq**2           for mf = 21 or 22, 
   *            22 + 10*neq + (2*ml + mu)*neq  for mf = 24 or 25. 
   *lrw    = declared length of rwork (in user-s dimension). 
   *iwork  = int work array of length at least.. 
   *            20        for mf = 10, 
   *            20 + neq  for mf = 21, 22, 24, or 25. 
   *         if mf = 24 or 25, input in iwork(1),iwork(2) the lower 
   *         and upper half-bandwidths ml,mu. 
   *liw    = declared length of iwork (in user-s dimension). 
   *jac    = name of subroutine for jacobian matrix (mf = 21 or 24). 
   *         if used, this name must be declared external in calling 
   *         program.  if not used, pass a dummy name. 
   *mf     = method flag.  standard values are.. 
   *         10 for nonstiff (adams) method, no jacobian used. 
   *         21 for stiff (bdf) method, user-supplied full jacobian. 
   *         22 for stiff method, internally generated full jacobian. 
   *         24 for stiff method, user-supplied banded jacobian. 
   *         25 for stiff method, internally generated banded jacobian. 
   *note that the main program must declare arrays y, rwork, iwork, 
   *and possibly atol. 
   * 
   *e. the output from the first call (or any call) is.. 
   *     y = array of computed values of y(t) vector. 
   *     t = corresponding value of independent variable (normally tout). 
   *istate = 2  if lsode was successful, negative otherwise. 
   *         -1 means excess work done on this call (perhaps wrong mf). 
   *         -2 means excess accuracy requested (tolerances too small). 
   *         -3 means illegal input detected (see printed message). 
   *         -4 means repeated error test failures (check all inputs). 
   *         -5 means repeated convergence failures (perhaps bad jacobian 
   *            supplied or wrong choice of mf or tolerances). 
   *         -6 means error weight became zero during problem. (solution 
   *            component i vanished, and atol or atol(i) = 0.) 
   *         -8 means time step becomes too small (t+h=t) new error 
   *            message (previously the solver take enough such step 
   *            until mxstep was reached) 
   * 
   *f. to continue the integration after a successful return, simply 
   *reset tout and call lsode again.  no other parameters need be reset. 
   * 
   * 
   *!example problem. 
   * 
   *the following is a simple example problem, with the coding 
   *needed for its solution by lsode.  the problem is from chemical 
   *kinetics, and consists of the following three rate equations.. 
   *    dy1/dt = -.04*y1 + 1.e4*y2*y3 
   *    dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2 
   *    dy3/dt = 3.e7*y2**2 
   *on the interval from t = 0.0 to t = 4.e10, with initial conditions 
   *y1 = 1.0, y2 = y3 = 0.  the problem is stiff. 
   * 
   *the following coding solves this problem with lsode, using mf = 21 
   *and printing results at t = .4, 4., ..., 4.e10.  it uses 
   *itol = 2 and atol much smaller for y2 than y1 or y3 because 
   *y2 has much smaller values. 
   *at the end of the run, statistical quantities of interest are 
   *printed (see optional outputs in the full description below). 
   * 
   *    external fex, jex 
   *    double precision atol, rwork, rtol, t, tout, y 
   *    dimension y(3), atol(3), rwork(58), iwork(23) 
   *    neq = 3 
   *    y(1) = 1.0d+0 
   *    y(2) = 0.0d+0 
   *    y(3) = 0.0d+0 
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
   *    do 40 iout = 1,12 
   *      call lsode(fex,neq,y,t,tout,itol,rtol,atol,itask,istate, 
   *   1     iopt,rwork,lrw,iwork,liw,jex,mf) 
   *      write(6,20)t,y(1),y(2),y(3) 
   *20    format(7h at t =,e12.4,6h   y =,3e14.6) 
   *      if (istate .lt. 0) go to 80 
   *40    tout = tout*10.0d+0 
   *    write(6,60)iwork(11),iwork(12),iwork(13) 
   *60  format(/12h no. steps =,i4,11h  no. f-s =,i4,11h  no. j-s =,i4) 
   *    stop 
   *80  write(6,90)istate 
   *90  format(///22h error halt.. istate =,i3) 
   *    stop 
   *    end 
   * 
   *    subroutine fex (neq, t, y, ydot) 
   *    double precision t, y, ydot 
   *    dimension y(3), ydot(3) 
   *    ydot(1) = -.040d+0*y(1) + 1.0d+4*y(2)*y(3) 
   *    ydot(3) = 3.0d+7*y(2)*y(2) 
   *    ydot(2) = -ydot(1) - ydot(3) 
   *    return 
   *    end 
   * 
   *    subroutine jex (neq, t, y, ml, mu, pd, nrpd) 
   *    double precision pd, t, y 
   *    dimension y(3), pd(nrpd,3) 
   *    pd(1,1) = -0.040d+0 
   *    pd(1,2) =  1.0d+4*y(3) 
   *    pd(1,3) =  1.0d+4*y(2) 
   *    pd(2,1) =  0.040d+0 
   *    pd(2,3) = -pd(1,3) 
   *    pd(3,2) =  6.0d+7*y(2) 
   *    pd(2,2) = -pd(1,2) - pd(3,2) 
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
   *  at t =  4.0000e+08   y =  5.494529e-06  2.197824e-11  9.999945e-01 
   *  at t =  4.0000e+09   y =  5.129458e-07  2.051784e-12  9.999995e-01 
   *  at t =  4.0000e+10   y = -7.170586e-08 -2.868234e-13  1.000000e+00 
   * 
   *  no. steps = 330  no. f-s = 405  no. j-s =  69 
   * 
   *!full description of user interface to lsode. 
   * 
   *the user interface to lsode consists of the following parts. 
   * 
   *i.   the call sequence to subroutine lsode, which is a driver 
   *     routine for the solver.  this includes descriptions of both 
   *     the call sequence arguments and of user-supplied routines. 
   *     following these descriptions is a description of 
   *     optional inputs available through the call sequence, and then 
   *     a description of optional outputs (in the work arrays). 
   * 
   *ii.  descriptions of other routines in the lsode package that may be 
   *     (optionally) called by the user.  these provide the ability to 
   *     alter error message handling, save and restore the internal 
   *     common, and obtain specified derivatives of the solution y(t). 
   * 
   *iii. descriptions of common blocks to be declared in overlay 
   *     or similar environments, or to be saved when doing an interrupt 
   *     of the problem and continued solution later. 
   * 
   *iv.  description of two subroutines in the lsode package, either of 
   *     which the user may replace with his own version, if desired. 
   *     these relate to the measurement of errors. 
   * 
   * 
   *part i.  call sequence. 
   * 
   *the call sequence parameters used for input only are 
   *    f, neq, tout, itol, rtol, atol, itask, iopt, lrw, liw, jac, mf, 
   *and those used for both input and output are 
   *    y, t, istate. 
   *the work arrays rwork and iwork are also used for conditional and 
   *optional inputs and optional outputs.  (the term output here refers 
   *to the return from subroutine lsode to the user-s calling program.) 
   * 
   *the legality of input parameters will be thoroughly checked on the 
   *initial call for the problem, but not checked thereafter unless a 
   *change in input parameters is flagged by istate = 3 on input. 
   * 
   *the descriptions of the call arguments are as follows. 
   * 
   *f      = the name of the user-supplied subroutine defining the 
   *         ode system.  the system must be put in the first-order 
   *         form dy/dt = f(t,y), where f is a vector-valued function 
   *         of the scalar t and the vector y.  subroutine f is to 
   *         compute the function f.  it is to have the form 
   *              subroutine f (neq, t, y, ydot) 
   *              dimension y(1), ydot(1) 
   *         where neq, t, and y are input, and the array ydot = f(t,y) 
   *         is output.  y and ydot are arrays of length neq. 
   *         (in the dimension statement above, 1 is a dummy 
   *         dimension.. it can be replaced by any value.) 
   *         subroutine f should not alter y(1),...,y(neq). 
   *         f must be declared external in the calling program. 
   * 
   *         subroutine f may access user-defined quantities in 
   *         neq(2),... and y(neq(1)+1),... if neq is an array 
   *         (dimensioned in f) and y has length exceeding neq(1). 
   *         see the descriptions of neq and y below. 
   * 
   *neq    = the size of the ode system (number of first order 
   *         ordinary differential equations).  used only for input. 
   *         neq may be decreased, but not increased, during the problem. 
   *         if neq is decreased (with istate = 3 on input), the 
   *         remaining components of y should be left undisturbed, if 
   *         these are to be accessed in f and/or jac. 
   * 
   *         normally, neq is a scalar, and it is generally referred to 
   *         as a scalar in this user interface description.  however, 
   *         neq may be an array, with neq(1) set to the system size. 
   *         (the lsode package accesses only neq(1).)  in either case, 
   *         this parameter is passed as the neq argument in all calls 
   *         to f and jac.  hence, if it is an array, locations 
   *         neq(2),... may be used to store other int data and pass 
   *         it to f and/or jac.  subroutines f and/or jac must include 
   *         neq in a dimension statement in that case. 
   * 
   *y      = a real array for the vector of dependent variables, of 
   *         length neq or more.  used for both input and output on the 
   *         first call (istate = 1), and only for output on other calls. 
   *         on the first call, y must contain the vector of initial 
   *         values.  on output, y contains the computed solution vector, 
   *         evaluated at t.  if desired, the y array may be used 
   *         for other purposes between calls to the solver. 
   * 
   *         this array is passed as the y argument in all calls to 
   *         f and jac.  hence its length may exceed neq, and locations 
   *         y(neq+1),... may be used to store other real data and 
   *         pass it to f and/or jac.  (the lsode package accesses only 
   *         y(1),...,y(neq).) 
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
   *         when starting the problem (istate = 1), tout may be equal 
   *         to t for one call, then should .ne. t for the next call. 
   *         for the initial t, an input value of tout .ne. t is used 
   *         in order to determine the direction of the integration 
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
   *             4     array      array      rtol(i)*abs(y(i)) + atol(i) 
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
   *         down uniformly. 
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
   *         the state of the calculation. 
   * 
   *         on input, the values of istate are as follows. 
   *         1  means this is the first call for the problem 
   *            (initializations will be done).  see note below. 
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
   *         istate = 1 on input. 
   * 
   *         on output, istate has the following values and meanings. 
   *          1  means nothing was done, as tout was equal to t with 
   *             istate = 1 on input.  (however, an internal counter was 
   *             set to detect and prevent repeated calls of this type.) 
   *          2  means the integration was performed successfully. 
   *         -1  means an excessive amount of work (more than mxstep 
   *             steps) was done on this call, before completing the 
   *             requested task, but the integration was otherwise 
   *             successful as far as t.  (mxstep is an optional input 
   *             and is normally 500.(now 10000 (bruno))  to continue, the user may 
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
   *             this may be caused by an inaccurate jacobian matrix, 
   *             if one is being used. 
   *         -6  means ewt(i) became zero for some i during the 
   *             integration.  pure relative error control (atol(i)=0.0) 
   *             was requested on a variable which has now vanished. 
   *             the integration was successful as far as t. 
   * 
   *         note..  since the normal output value of istate is 2, 
   *         it does not need to be reset for normal continuation. 
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
   *            20 + nyh*(maxord + 1) + 3*neq + lwm    where 
   *         nyh    = the initial value of neq, 
   *         maxord = 12 (if meth = 1) or 5 (if meth = 2) (unless a 
   *                  smaller value is given as an optional input), 
   *         lwm   = 0             if miter = 0, 
   *         lwm   = neq**2 + 2    if miter is 1 or 2, 
   *         lwm   = neq + 2       if miter = 3, and 
   *         lwm   = (2*ml+mu+1)*neq + 2 if miter is 4 or 5. 
   *         (see the mf description for meth and miter.) 
   *         thus if maxord has its default value and neq is constant, 
   *         this length is.. 
   *            20 + 16*neq                  for mf = 10, 
   *            22 + 16*neq + neq**2         for mf = 11 or 12, 
   *            22 + 17*neq                  for mf = 13, 
   *            22 + 17*neq + (2*ml+mu)*neq  for mf = 14 or 15, 
   *            20 +  9*neq                  for mf = 20, 
   *            22 +  9*neq + neq**2         for mf = 21 or 22, 
   *            22 + 10*neq                  for mf = 23, 
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
   *            20        if miter = 0 or 3 (mf = 10, 13, 20, 23), or 
   *            20 + neq  otherwise (mf = 11, 12, 14, 15, 21, 22, 24, 25). 
   *         the first few words of iwork are used for conditional and 
   *         optional inputs and optional outputs. 
   * 
   *         the following 2 words in iwork are conditional inputs.. 
   *           iwork(1) = ml     these are the lower and upper 
   *           iwork(2) = mu     half-bandwidths, respectively, of the 
   *                      banded jacobian, excluding the main diagonal. 
   *                      the band is defined by the matrix locations 
   *                      (i,j) with i-ml .le. j .le. i+mu.  ml and mu 
   *                      must satisfy  0 .le.  ml,mu  .le. neq-1. 
   *                      these are required if miter is 4 or 5, and 
   *                      ignored otherwise.  ml and mu may in fact be 
   *                      the band parameters for a matrix to which 
   *                      df/dy is only approximately equal. 
   * 
   *liw    = the length of the array iwork, as declared by the user. 
   *         (this will be checked by the solver.) 
   * 
   *note..  the work arrays must not be altered between calls to lsode 
   *for the same problem, except possibly for the conditional and 
   *optional inputs, and except for the last 3*neq words of rwork. 
   *the latter space is used for internal scratch space, and so is 
   *available for use by the user outside lsode between calls, if 
   *desired (but not for use by f or jac). 
   * 
   *jac    = the name of the user-supplied routine (miter = 1 or 4) to 
   *         compute the jacobian matrix, df/dy, as a function of 
   *         the scalar t and the vector y.  it is to have the form 
   *              subroutine jac (neq, t, y, ml, mu, pd, nrowpd) 
   *              dimension y(*), pd(nrowpd,*) 
   *         where neq, t, y, ml, mu, and nrowpd are input and the array 
   *         pd is to be loaded with partial derivatives (elements of 
   *         the jacobian matrix) on output.  pd must be given a first 
   *         dimension of nrowpd.  t and y have the same meaning as in 
   *         subroutine f.  (in the dimension statement above, 1 is a 
   *         dummy dimension.. it can be replaced by any value.) 
   *              in the full matrix case (miter = 1), ml and mu are 
   *         ignored, and the jacobian is to be loaded into pd in 
   *         columnwise manner, with df(i)/dy(j) loaded into pd(i,j). 
   *              in the band matrix case (miter = 4), the elements 
   *         within the band are to be loaded into pd in columnwise 
   *         manner, with diagonal lines of df/dy loaded into the rows 
   *         of pd.  thus df(i)/dy(j) is to be loaded into pd(i-j+mu+1,j). 
   *         ml and mu are the half-bandwidth parameters (see iwork). 
   *         the locations in pd in the two triangular areas which 
   *         correspond to nonexistent matrix elements can be ignored 
   *         or loaded arbitrarily, as they are overwritten by lsode. 
   *              jac need not provide df/dy exactly.  a crude 
   *         approximation (possibly with a smaller bandwidth) will do. 
   *              in either case, pd is preset to zero by the solver, 
   *         so that only the nonzero elements need be loaded by jac. 
   *         each call to jac is preceded by a call to f with the same 
   *         arguments neq, t, and y.  thus to gain some efficiency, 
   *         intermediate quantities shared by both calculations may be 
   *         saved in a user common block by f and not recomputed by jac, 
   *         if desired.  also, jac may alter the y array, if desired. 
   *         jac must be declared external in the calling program. 
   *              subroutine jac may access user-defined quantities in 
   *         neq(2),... and y(neq(1)+1),... if neq is an array 
   *         (dimensioned in jac) and y has length exceeding neq(1). 
   *         see the descriptions of neq and y above. 
   * 
   *mf     = the method flag.  used only for input.  the legal values of 
   *         mf are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, and 25. 
   *         mf has decimal digits meth and miter.. mf = 10*meth + miter. 
   *         meth indicates the basic linear multistep method.. 
   *           meth = 1 means the implicit adams method. 
   *           meth = 2 means the method based on backward 
   *                    differentiation formulas (bdf-s). 
   *         miter indicates the corrector iteration method.. 
   *           miter = 0 means functional iteration (no jacobian matrix 
   *                     is involved). 
   *           miter = 1 means chord iteration with a user-supplied 
   *                     full (neq by neq) jacobian. 
   *           miter = 2 means chord iteration with an internally 
   *                     generated (difference quotient) full jacobian 
   *                     (using neq extra calls to f per df/dy value). 
   *           miter = 3 means chord iteration with an internally 
   *                     generated diagonal jacobian approximation. 
   *                     (using 1 extra call to f per df/dy evaluation). 
   *           miter = 4 means chord iteration with a user-supplied 
   *                     banded jacobian. 
   *           miter = 5 means chord iteration with an internally 
   *                     generated banded jacobian (using ml+mu+1 extra 
   *                     calls to f per df/dy evaluation). 
   *         if miter = 1 or 4, the user must supply a subroutine jac 
   *         (the name is arbitrary) as described above under jac. 
   *         for other values of miter, a dummy argument can be used. 
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
   *                  the default value is 500 (now 10000). 
   * 
   *mxhnil  iwork(7)  maximum number of messages printed (per problem) 
   *                  warning that t + h = t on a step (h = step size). 
   *                  this must be positive to result in a non-default 
   *                  value.  the default value is 10. 
   * 
   *!optional outputs. 
   * 
   *as optional additional output from lsode, the variables listed 
   *below are quantities related to the performance of lsode 
   *which are available to the user.  these are communicated by way of 
   *the work arrays, but also have internal mnemonic names as shown. 
   *except where stated otherwise, all of these outputs are defined 
   *on any successful return from lsode, and on any return with 
   *istate = -1, -2, -4, -5, or -6.  on an illegal input return 
   *(istate = -3), they will be unchanged from their existing values 
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
   *nfe     iwork(12) the number of f evaluations for the problem so far. 
   * 
   *nje     iwork(13) the number of jacobian evaluations (and of matrix 
   *                  lu decompositions) for the problem so far. 
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
   *                       corrections on each step, scaled on output 
   *                       to represent the estimated local error in y 
   *                       on the last step.  this is the vector e in 
   *                       the description of the error control.  it is 
   *                       defined only on a successful return from lsode. 
   * 
   * 
   *!part ii.  other routines callable. 
   * 
   *the following are optional calls which the user may make to 
   *gain additional capabilities in conjunction with lsode. 
   *(the routines xsetun and xsetf are designed to conform to the 
   *slatec error handling package.) 
   * 
   *    form of call                  function 
   *  call xsetun(lun)          set the int unit number, lun, for 
   *                            output of messages from lsode, if 
   *                            the default is not desired. 
   *                            the default value of lun is 6. 
   * 
   *  call xsetf(mflag)         set a flag to control the printing of 
   *                            messages by lsode. 
   *                            mflag = 0 means do not print. (danger.. 
   *                            this risks losing valuable information.) 
   *                            mflag = 1 means print (the default). 
   * 
   *                            either of the above calls may be made at 
   *                            any time and will take effect immediately. 
   * 
   *  call svcom (rsav, isav)   store in rsav and isav the contents 
   *                            of the internal common blocks used by 
   *                            lsode (see part iii below). 
   *                            rsav must be a real array of length 219 
   *                            or more, and isav must be an int 
   *                            array of length 41 or more. 
   * 
   *  call rscom (rsav, isav)   restore, from rsav and isav, the contents 
   *                            of the internal common blocks used by 
   *                            lsode.  presumes a prior call to svcom 
   *                            with the same arguments. 
   * 
   *                            svcom and rscom are useful if 
   *                            interrupting a run and restarting 
   *                            later, or alternating between two or 
   *                            more problems solved with lsode. 
   * 
   *  call intdy(,,,,,)         provide derivatives of y, of various 
   *       (see below)          orders, at a specified point t, if 
   *                            desired.  it may be called only after 
   *                            a successful return from lsode. 
   * 
   *the detailed instructions for using intdy are as follows. 
   *the form of the call is.. 
   * 
   *  call intdy (t, k, rwork(21), nyh, dky, iflag) 
   * 
   *the input parameters are.. 
   * 
   *t         = value of independent variable where answers are desired 
   *            (normally the same as the t last returned by lsode). 
   *            for valid results, t must lie between tcur - hu and tcur. 
   *            (see optional outputs for tcur and hu.) 
   *k         = int order of the derivative desired.  k must satisfy 
   *            0 .le. k .le. nqcur, where nqcur is the current order 
   *            (see optional outputs).  the capability corresponding 
   *            to k = 0, i.e. computing y(t), is already provided 
   *            by lsode directly.  since nqcur .ge. 1, the first 
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
   *if lsode is to be used in an overlay situation, the user 
   *must declare, in the primary overlay, the variables in.. 
   *  (1) the call sequence to lsode, 
   *  (2) the two internal common blocks 
   *        /ls0001/  of length  258  (219 double precision words 
   *                        followed by 39 int words), 
   *        /eh0001/  of length  2 (int words). 
   * 
   *if lsode is used on a system in which the contents of internal 
   *common blocks are not preserved between calls, the user should 
   *declare the above two common blocks in his main program to insure 
   *that their contents are preserved. 
   * 
   *if the solution of a given problem by lsode is to be interrupted 
   *and then later continued, such as when restarting an interrupted run 
   *or alternating between two or more problems, the user should save, 
   *following the return from the last lsode call prior to the 
   *interruption, the contents of the call sequence variables and the 
   *internal common blocks, and later restore these values before the 
   *next lsode call for that problem.  to save and restore the common 
   *blocks, use subroutines svcom and rscom (see part ii above). 
   * 
   * 
   *!part iv.  optionally replaceable solver routines. 
   * 
   *below are descriptions of two routines in the lsode package which 
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
   *where neq, itol, rtol, and atol are as in the lsode call sequence, 
   *ycur contains the current dependent variable vector, and 
   *ewt is the array of weights set by ewset. 
   * 
   *if the user supplies this subroutine, it must return in ewt(i) 
   *(i = 1,...,neq) a positive quantity suitable for comparing errors 
   *in y(i) to.  the ewt array returned by ewset is passed to the 
   *vnorm routine (see below), and also used by lsode in the computation 
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
   *value of vnorm suitable for use in the error control in lsode. 
   *none of the arguments should be altered by vnorm. 
   *for example, a user-supplied vnorm routine might.. 
   *  -substitute a max-norm of (v(i)*w(i)) for the rms-norm, or 
   *  -ignore some components of v in the norm, with the effect of 
   *   suppressing the error control on those components of y. 
   * 
   * 
   *!other routines in the lsode package. 
   * 
   *in addition to subroutine lsode, the lsode package includes the 
   *following subroutines and function routines.. 
   * intdy    computes an interpolated value of the y vector at t = tout. 
   * stode    is the core integrator, which does one step of the 
   *          integration and the associated error control. 
   * cfode    sets all method coefficients and test constants. 
   * prepj    computes and preprocesses the jacobian matrix j = df/dy 
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
   *the intrinsic and external routines used by lsode are.. 
   *abs, max, min, dble, max, min, mod, sign, sqrt, and write. 
   * 
   *a block data subprogram is also included with the package, 
   *for loading some of the variables in internal common. 
   * 
   *!author and contact 
   *                     alan c. hindmarsh, 
   *                     mathematics and statistics division, l-316 
   *                     lawrence livermore national laboratory 
   *                     livermore, ca 94550. 
   * 
   *!reference. 
   *    alan c. hindmarsh,  lsode and lsodi, two new initial value 
   *    ordinary differential equation solvers, 
   *    acm-signum newsletter, vol. 15, no. 4 (1980), pp. 10-11. 
   * 
   *! 
   *this is the august 13, 1981 version of lsode. 
   *----------------------------------------------------------------------- 
   *the following card is for optimized compilation on llnl compilers. 
   *lll. optimize 
   *----------------------------------------------------------------------- 
   *use as a pointer to datas for external 
   *----------------------------------------------------------------------- 
   *the following internal common block contains 
   *(a) variables which are local to any subroutine but whose values must 
   *    be preserved between calls to the routine (own variables), and 
   *(b) variables which are communicated between subroutines. 
   *the structure of the block is as follows..  all real variables are 
   *listed first, followed by all ints.  within each type, the 
   *variables are grouped with those local to subroutine lsode first, 
   *then those local to subroutine stode, and finally those used 
   *for communication.  the block is declared in subroutines 
   *lsode, intdy, stode, prepj, and solsy.  groups of variables are 
   *replaced by dummy arrays in the common declarations in routines 
   *where those variables are not used. 
   *----------------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --neq;
  --y;
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
   *if istate = 1 and tout = t, jump to block g and return immediately. 
   *----------------------------------------------------------------------- 
   */
  ierode_1.iero = 0;
  if (*istate < 1 || *istate > 3)
    {
      goto L601;
    }
  if (*itask < 1 || *itask > 5)
    {
      goto L602;
    }
  if (*istate == 1)
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
   *the next code block is executed for the initial call (istate = 1), 
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
  if (*istate == 1)
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
  if (ls0001_1.miter < 0 || ls0001_1.miter > 5)
    {
      goto L608;
    }
  if (ls0001_1.miter <= 3)
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
  if (*istate == 1)
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
  if (*istate != 1)
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
   *segments of rwork (in order) are denoted  yh, wm, ewt, savf, acor. 
   *----------------------------------------------------------------------- 
   */
 L60:
  ls0001_1.lyh = 21;
  if (*istate == 1)
    {
      ls0001_1.nyh = ls0001_1.n;
    }
  ls0001_1.lwm = ls0001_1.lyh + (ls0001_1.maxord + 1) * ls0001_1.nyh;
  if (ls0001_1.miter == 0)
    {
      lenwm = 0;
    }
  if (ls0001_1.miter == 1 || ls0001_1.miter == 2)
    {
      lenwm = ls0001_1.n * ls0001_1.n + 2;
    }
  if (ls0001_1.miter == 3)
    {
      lenwm = ls0001_1.n + 2;
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
  if (ls0001_1.miter == 0 || ls0001_1.miter == 3)
    {
      leniw = 20;
    }
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
  if (*istate == 1)
    {
      goto L100;
    }
  /*if istate = 3, set flag to signal parameter changes to stode. -------- 
   */
  ls0001_1.jstart = -1;
  if (ls0001_1.nq <= ls0001_1.maxord)
    {
      goto L90;
    }
  /*maxord was reduced below nq.  copy yh(*,maxord+2) into savf. --------- 
   */
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L80: */
      rwork[i__ + ls0001_1.lsavf - 1] = rwork[i__ + ls0001_1.lwm - 1];
    }
  /*reload wm(1) = rwork(lwm), since lwm may have changed. --------------- 
   */
 L90:
  if (ls0001_1.miter > 0)
    {
      rwork[ls0001_1.lwm] = sqrt (ls0001_1.uround);
    }
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
   *the next block is for the initial call only (istate = 1). 
   *it contains all remaining initializations, the initial call to f, 
   *and the calculation of the initial step size. 
   *the error weights in ewt are inverted after being loaded. 
   *----------------------------------------------------------------------- 
   */
 L100:
  ls0001_1.uround = nsp_dlamch ("p");
  ls0001_1.tn = *t;
  if (*itask != 4 && *itask != 5)
    {
      goto L110;
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
 L110:
  ls0001_1.jstart = 0;
  if (ls0001_1.miter > 0)
    {
      rwork[ls0001_1.lwm] = sqrt (ls0001_1.uround);
    }
  ls0001_1.nhnil = 0;
  ls0001_1.nst = 0;
  ls0001_1.nje = 0;
  ls0001_1.nslast = 0;
  ls0001_1.hu = 0.;
  ls0001_1.nqu = 0;
  ls0001_1.ccmax = .3;
  ls0001_1.maxcor = 3;
  ls0001_1.msbp = 20;
  ls0001_1.mxncf = 10;
  /*initial call to f.  (lf0 points to yh(*,2).) ------------------------- 
   */
  lf0 = ls0001_1.lyh + ls0001_1.nyh;
  (*f) (&neq[1], t, &y[1], &rwork[lf0], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ls0001_1.nfe = 1;
  /*load the initial value vector in yh. --------------------------------- 
   */
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L115: */
      rwork[i__ + ls0001_1.lyh - 1] = y[i__];
    }
  /*load and invert the ewt array.  (h is temporarily set to 1.0.) ------- 
   */
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
      /* L120: */
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
   *  h0**2 = tol / ( w0**-2 + (1/neq) * sum ( f(i)/ywt(i) )**2  ) 
   *                                      1 
   *where   w0     = Max( Abs(t), Abs(tout) ), 
   *        f(i)   = i-th component of initial value of f, 
   *        ywt(i) = ewt(i)/tol  (a weight for y(i)). 
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
      goto L140;
    }
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L130: */
      /*Computing MAX 
       */
      d__1 = tol, d__2 = rtol[i__];
      tol = Max (d__1, d__2);
    }
 L140:
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
  sum = nsp_ode_vnorm (&ls0001_1.n, &rwork[lf0], &rwork[ls0001_1.lewt]);
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
      rwork[i__ + lf0 - 1] = h0 * rwork[i__ + lf0 - 1];
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
   *the call to the one-step core integrator stode. 
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
  /*    (change by bp): new behavior: we return istate = -8 t=t+h occurs 
   */
  C2F (xerrwvb) ("lsode:", &c__6, &c__1101, &c__1, &c__0, &c__0, &c__0,
		 &c__2, &ls0001_1.tn, &ls0001_1.h__, 6L);
  *istate = -8;
  goto L580;
  /*     nhnil = nhnil + 1 
   *     if (nhnil .gt. mxhnil) go to 290 
   *$$$      call xerrwv('lsode--  caution... t (=r1) and h (=r2) are', 
   *$$$     1   50, 101, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv( 
   *$$$     1 '         such that t + h = t at next step', 
   *$$$     1   60, 101, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv('         integration continues', 
   *$$$     1   50, 101, 1, 0, 0, 0, 2, tn, h) 
   *     call xerrwvb('lsode:', 6, 101, 1, 0, 0, 0, 2, tn, h) 
   *     if (nhnil .lt. mxhnil) go to 290 
   *$$$      call xerrwv('lsode--  preceding message given i1 times', 
   *$$$     1   50, 102, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv('         will not be repeated', 
   *$$$     1   50, 102, 1, 1, mxhnil, 0, 0, 0.0d+0, 0.0d+0) 
   *     call xerrwvb('lsode:', 6, 102, 1, 1, mxhnil, 0, 0, 0.0, 0.0) 
   *ou return 
   */
 L290:
  /*----------------------------------------------------------------------- 
   *    call stode(neq,y,yh,nyh,yh,ewt,savf,acor,wm,iwm,f,jac,prepj,solsy,param) 
   *----------------------------------------------------------------------- 
   */
  nsp_ode_stode (&neq[1], &y[1], &rwork[ls0001_1.lyh], &ls0001_1.nyh,
		    &rwork[ls0001_1.lyh], &rwork[ls0001_1.lewt],
		    &rwork[ls0001_1.lsavf], &rwork[ls0001_1.lacor],
		    &rwork[ls0001_1.lwm], &iwork[ls0001_1.liwm],  f,
		    jac, nsp_ode_prepj, nsp_ode_solsy, param);
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
    }
  /*----------------------------------------------------------------------- 
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
   *the following block handles all successful returns from lsode. 
   *if itask .ne. 1, y is loaded from yh and t is set accordingly. 
   *istate is set to 2, the illegal input counter is zeroed, and the 
   *optional outputs are loaded into the work arrays before returning. 
   *if istate = 1 and tout = t, there is a return with no action taken, 
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
  C2F (xerrwv) ("lsode--  calls with istate = 1 and tout = t (=r1)  ", &c__60,
		&c__301, &c__1, &c__0, &c__0, &c__0, &c__1, t, &c_b98, 51L);
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
   *$$$ 500     call xerrwv('lsode--  at t (=r1), mxstep (=i1) steps   ', 
   *$$$     1   50, 201, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv('         necessary before reaching tout', 
   *$$$     1   50, 201, 1, 1, mxstep, 0, 1, tn, 0.0d+0) 
   */
 L500:
  C2F (xerrwvb) ("lsode:", &c__6, &c__201, &c__1, &c__1, &ls0001_1.mxstep,
		 &c__0, &c__1, &ls0001_1.tn, &c_d0, 6L);
  *istate = -1;
  goto L580;
  /*ewt(i) .le. 0.0 for some i (not at start of problem). ---------------- 
   */
 L510:
  ewti = rwork[ls0001_1.lewt + i__ - 1];
  /*$$$      call xerrwv('lsode--  at t (=r1),ewt(i1) (=r2) is .le.0', 
   *$$$     1   50, 202, 1, 1, i, 0, 2, tn, ewti) 
   */
  C2F (xerrwvb) ("lsode:", &c__6, &c__202, &c__1, &c__1, &i__, &c__0, &c__2,
		 &ls0001_1.tn, &ewti, 6L);
  *istate = -6;
  goto L580;
  /*too much accuracy requested for machine precision. ------------------- 
   *$$$ 520  call xerrwv('lsode--  a t (=r1),  too much precision required', 
   *$$$     1   50, 203, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv('         w.r.t. machine precision tolsf (=r2) ', 
   *$$$     1   50, 203, 1, 0, 0, 0, 2, tn, tolsf) 
   */
 L520:
  C2F (xerrwvb) ("lsode:", &c__6, &c__203, &c__1, &c__0, &c__0, &c__0, &c__2,
		 &ls0001_1.tn, &tolsf, 6L);
  rwork[14] = tolsf;
  *istate = -2;
  goto L580;
  /*kflag = -1.  error test failed repeatedly or with Abs(h) = hmin. ----- 
   */
 L530:
  C2F (xerrwv) ("lsode--  at t(=r1) for step h(=r2), error test", &c__50,
		&c__204, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b98, &c_b98,
		46L);
  C2F (xerrwv) ("         failed with Abs(h) = hmin", &c__50, &c__204, &c__1,
		&c__0, &c__0, &c__0, &c__2, &ls0001_1.tn, &ls0001_1.h__, 34L);
  *istate = -4;
  goto L560;
  /*kflag = -2.  convergence failed repeatedly or with Abs(h) = hmin. ---- 
   *$$$ 540  call xerrwv('lsode--  at t (=r1) with step h (=r2), '    , 
   *$$$     1   50, 205, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv('         corrector does not converge ', 
   *$$$     1   50, 205, 1, 0, 0, 0, 0, 0.0d+0, 0.0d+0) 
   *$$$      call xerrwv('         with Abs(h) = hmin   ', 
   *$$$     1   30, 205, 1, 0, 0, 0, 2, tn, h) 
   */
 L540:
  C2F (xerrwvb) ("lsode:", &c__6, &c__205, &c__1, &c__0, &c__0, &c__0, &c__2,
		 &ls0001_1.tn, &ls0001_1.h__, 6L);
  *istate = -5;
  /*compute imxer if relevant. ------------------------------------------- 
   */
 L560:
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
	  goto L570;
	}
      big = size;
      imxer = i__;
    L570:
      ;
    }
  iwork[16] = imxer;
  /*set y vector, t, illin, and optional outputs. ------------------------ 
   */
 L580:
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L590: */
      y[i__] = rwork[i__ + ls0001_1.lyh - 1];
    }
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
  C2F (xerrwv) ("lsode--  istate (=i1) illegal ", &c__30, &c__1, &c__1, &c__1,
		istate, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L602:
  C2F (xerrwv) ("lsode--  itask (=i1) illegal  ", &c__30, &c__2, &c__1, &c__1,
		itask, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L603:
  C2F (xerrwv) ("lsode--  istate .gt. 1 ", &c__50, &c__3, &c__1, &c__0, &c__0,
		&c__0, &c__0, &c_b98, &c_b98, 23L);
  goto L700;
 L604:
  C2F (xerrwv) ("lsode--  neq (=i1) .lt. 1     ", &c__30, &c__4, &c__1, &c__1,
		&neq[1], &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L605:
  C2F (xerrwv) ("lsode--  istate and neq  increased from i1 to i2", &c__50,
		&c__5, &c__1, &c__2, &ls0001_1.n, &neq[1], &c__0, &c_b98,
		&c_b98, 48L);
  goto L700;
 L606:
  C2F (xerrwv) ("lsode--  itol (=i1) illegal   ", &c__30, &c__6, &c__1, &c__1,
		itol, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L607:
  C2F (xerrwv) ("lsode--  iopt (=i1) illegal   ", &c__30, &c__7, &c__1, &c__1,
		iopt, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L608:
  C2F (xerrwv) ("lsode--  mf (=i1) illegal     ", &c__30, &c__8, &c__1, &c__1,
		mf, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L609:
  C2F (xerrwv) ("lsode--  ml (=i1) illegal.. .lt.0 or .ge.neq (=i2)", &c__50,
		&c__9, &c__1, &c__2, &ml, &neq[1], &c__0, &c_b98, &c_b98,
		50L);
  goto L700;
 L610:
  C2F (xerrwv) ("lsode--  mu (=i1) illegal.. .lt.0 or .ge.neq (=i2)", &c__50,
		&c__10, &c__1, &c__2, &mu, &neq[1], &c__0, &c_b98, &c_b98,
		50L);
  goto L700;
 L611:
  C2F (xerrwv) ("lsode--  maxord (=i1) .lt. 0  ", &c__30, &c__11, &c__1,
		&c__1, &ls0001_1.maxord, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L612:
  C2F (xerrwv) ("lsode--  mxstep (=i1) .lt. 0  ", &c__30, &c__12, &c__1,
		&c__1, &ls0001_1.mxstep, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L613:
  C2F (xerrwv) ("lsode--  mxhnil (=i1) .lt. 0  ", &c__30, &c__13, &c__1,
		&c__1, &ls0001_1.mxhnil, &c__0, &c__0, &c_b98, &c_b98, 30L);
  goto L700;
 L614:
  C2F (xerrwv) ("lsode--  tout (=r1)  .gt.  t (=r2)      ", &c__40, &c__14,
		&c__1, &c__0, &c__0, &c__0, &c__2, tout, t, 40L);
  C2F (xerrwv) ("      h0 (=r1) gives integration direction", &c__50, &c__14,
		&c__1, &c__0, &c__0, &c__0, &c__1, &h0, &c_b98, 42L);
  goto L700;
 L615:
  C2F (xerrwv) ("lsode--  hmax (=r1) .lt. 0.0  ", &c__30, &c__15, &c__1,
		&c__0, &c__0, &c__0, &c__1, &hmax, &c_b98, 30L);
  goto L700;
 L616:
  C2F (xerrwv) ("lsode--  hmin (=r1) .lt. 0.0  ", &c__30, &c__16, &c__1,
		&c__0, &c__0, &c__0, &c__1, &ls0001_1.hmin, &c_b98, 30L);
  goto L700;
 L617:
  C2F (xerrwv) ("lsode-- necessary size for rwork (i1) larger than i2",
		&c__60, &c__17, &c__1, &c__2, &lenrw, lrw, &c__0, &c_b98,
		&c_b98, 52L);
  goto L700;
 L618:
  C2F (xerrwv) ("lsode-- necessary size for iwork (i1) larger than liw (i2)",
		&c__60, &c__18, &c__1, &c__2, &leniw, liw, &c__0, &c_b98,
		&c_b98, 58L);
  goto L700;
 L619:
  C2F (xerrwv) ("lsode--  rtol(i1) is r1 .lt. 0.0        ", &c__40, &c__19,
		&c__1, &c__1, &i__, &c__0, &c__1, &rtoli, &c_b98, 40L);
  goto L700;
 L620:
  C2F (xerrwv) ("lsode--  atol(i1) is r1 .lt. 0.0        ", &c__40, &c__20,
		&c__1, &c__1, &i__, &c__0, &c__1, &atoli, &c_b98, 40L);
  goto L700;
 L621:
  ewti = rwork[ls0001_1.lewt + i__ - 1];
  C2F (xerrwv) ("lsode--  ewt(i1) (=r1) is .le. 0.0         ", &c__40, &c__21,
		&c__1, &c__1, &i__, &c__0, &c__1, &ewti, &c_b98, 43L);
  goto L700;
 L622:
  C2F (xerrwv) ("lsode--  tout (=r1) too close to t(=r2) ", &c__60, &c__22,
		&c__1, &c__0, &c__0, &c__0, &c__2, tout, t, 40L);
  goto L700;
 L623:
  C2F (xerrwv) ("lsode--  itask (=i1) and tout (=r1) .gt. tcur - hu (= r2)  ",
		&c__60, &c__23, &c__1, &c__1, itask, &c__0, &c__2, tout, &tp,
		59L);
  goto L700;
 L624:
  C2F (xerrwv) ("lsode--  itask = 4 or 5 and tcrit (=r1) .gt. tcur (=r2)   ",
		&c__60, &c__24, &c__1, &c__0, &c__0, &c__0, &c__2, &tcrit,
		&ls0001_1.tn, 58L);
  goto L700;
 L625:
  C2F (xerrwv) ("lsode--  itask = 4 or 5 and tcrit (=r1)  .lt.  tout (=r2)",
		&c__60, &c__25, &c__1, &c__0, &c__0, &c__0, &c__2, &tcrit,
		tout, 57L);
  goto L700;
 L626:
  C2F (xerrwv) ("lsode-- initial precision required", &c__50, &c__26, &c__1,
		&c__0, &c__0, &c__0, &c__0, &c_b98, &c_b98, 34L);
  C2F (xerrwv) ("too high wrt machine precision tolsf (=r1)", &c__60, &c__26,
		&c__1, &c__0, &c__0, &c__0, &c__1, &tolsf, &c_b98, 42L);
  rwork[14] = tolsf;
  goto L700;
 L627:
  C2F (xerrwv) ("lsode--  problems in intdy. itask=i1,tout=r1", &c__50,
		&c__27, &c__1, &c__1, itask, &c__0, &c__1, tout, &c_b98, 44L);
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
  C2F (xerrwv) ("lsode-- incorrect inputs", &c__50, &c__302, &c__1, &c__0,
		&c__0, &c__0, &c__0, &c_b98, &c_b98, 24L);
  /* 
   */
 L800:
  C2F (xerrwv) ("lsode-- infinite loop ", &c__50, &c__303, &c__2, &c__0,
		&c__0, &c__0, &c__0, &c_b98, &c_b98, 22L);
  return 0;
  /*----------------------- end of subroutine lsode ----------------------- 
   */
}				/* lsode_ */
