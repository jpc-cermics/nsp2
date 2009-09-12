#include "integ.h"

#define ls0001_1 ls0001_._1
#define lsa001_1 lsa001_._1
#define lsr001_1 lsr001_._1

/* Table of constant values */

static int c__60 = 60;
static int c__103 = 103;
static int c__1 = 1;
static int c__0 = 0;
static double c_b46 = 0.;
static int c__50 = 50;
static int c__2 = 2;
static int c__104 = 104;
static int c__101 = 101;
static int c__102 = 102;
static int c__105 = 105;
static int c__106 = 106;
static int c__107 = 107;
static int c__3 = 3;
static int c__301 = 301;
static int c__201 = 201;
static int c__202 = 202;
static int c__203 = 203;
static int c__204 = 204;
static int c__205 = 205;
static int c__30 = 30;
static int c__206 = 206;
static int c__207 = 207;
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
static int c__20 = 20;
static int c__21 = 21;
static int c__22 = 22;
static int c__23 = 23;
static int c__24 = 24;
static int c__25 = 25;
static int c__26 = 26;
static int c__27 = 27;
static int c__28 = 28;
static int c__29 = 29;
static int c__31 = 31;
static int c__32 = 32;
static int c__302 = 302;
static int c__303 = 303;

int
C2F(lsodar2) (ode_f f, int *neq, double *y, double *t, double *tout,
	      int *itol, double *rtol, double *atol, int *itask,
	      int *istate, int *iopt, double *rwork, int *lrw,
	      int *iwork, int *liw, ode_jac jac, int *jt, lsodar_g g, int *ng,
	      int *jroot, void *param)
{
  /* Initialized data */

  static int mord[2] = { 12, 5 };
  static int mxstp0 = 500;
  static int mxhnl0 = 10;

  /* System generated locals */
  int i__1;
  double d__1, d__2;

  /* Local variables */
  double hmax;
  int irfp;
  int ihit;
  double ewti, size;
  int len1c, len1n, len1s, i__, iflag;
  double atoli;
  int leniw, lenwm, lenyh, imxer;
  double tcrit;
  int lenrw, i1, i2;
  double h0, rtoli, tdist, tnext, tolsf, w0;
  int ml;
  double rh;
  int mu;
  double tp;
  int lirfnd, leniwc, lenrwc, lf0, lenrwn, lenrws, lyhnew;
  double big;
  int kgo;
  double ayi, hmx;
  int irt;
  double tol, sum;
  int len1, len2;

  /*----------------------------------------------------------------------- 
   *this is the may 7, 1982 version of 
   *lsodar.. livermore solver for ordinary differential equations, with 
   *         automatic method switching for stiff and nonstiff problems, 
   *         and with root-finding. 
   * 
   *This version has been modified by scilab group on Feb 97 following Dr 
   *     Hindmarsh direction see Comments noted "cSCI" 
   * 
   *this version is in double precision. 
   * 
   *lsodar solves the initial value problem for stiff or nonstiff 
   *systems of first order ode-s, 
   *    dy/dt = f(t,y) ,  or, in component form, 
   *    dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq). 
   *at the same time, it locates the roots of any of a set of functions 
   *    g(i) = g(i,t,y(1),...,y(neq))  (i = 1,...,ng). 
   * 
   *this a variant version of the lsode package.  it differs from lsode 
   *in two ways.. 
   *(a) it switches automatically between stiff and nonstiff methods. 
   *this means that the user does not have to determine whether the 
   *problem is stiff or not, and the solver will automatically choose the 
   *appropriate method.  it always starts with the nonstiff method. 
   *(b) it finds the root of at least one of a set of constraint 
   *functions g(i) of the independent and dependent variables. 
   *it finds only those roots for which some g(i), as a function 
   *of t, changes sign in the interval of integration. 
   *it then returns the solution at the root, if that occurs 
   *sooner than the specified stop condition, and otherwise returns 
   *the solution according the specified stop condition. 
   * 
   *authors.. 
   *                     linda r. petzold 
   *                     applied mathematics division 8331 
   *                     sandia national laboratories 
   *                     livermore, ca 94550 
   *and 
   *                     alan c. hindmarsh, 
   *                     mathematics and statistics division, l-316 
   *                     lawrence livermore national laboratory 
   *                     livermore, ca 94550. 
   * 
   *references.. 
   *1.  alan c. hindmarsh,  lsode and lsodi, two new initial value 
   *    ordinary differential equation solvers, 
   *    acm-signum newsletter, vol. 15, no. 4 (1980), pp. 10-11. 
   *2.  linda r. petzold, automatic selection of methods for solving 
   *    stiff and nonstiff systems of ordinary differential equations, 
   *    siam j. sci. stat. comput. 4 (1983), pp. 136-148. 
   *3.  kathie l. hiebert and lawrence f. shampine, implicitly defined 
   *    output points for solutions of ode-s, sandia report sand80-0180, 
   *    february, 1980. 
   *----------------------------------------------------------------------- 
   *summary of usage. 
   * 
   *communication between the user and the lsodar package, for normal 
   *situations, is summarized here.  this summary describes only a subset 
   *of the full set of options available.  see the full description for 
   *details, including alternative treatment of the jacobian matrix, 
   *optional inputs and outputs, nonstandard options, and 
   *instructions for special situations.  see also the example 
   *problem (with program and output) following this summary. 
   * 
   *a. first provide a subroutine of the form.. 
   *              subroutine f (neq, t, y, ydot) 
   *              dimension y(neq), ydot(neq) 
   *which supplies the vector function f by loading ydot(i) with f(i). 
   * 
   *b. provide a subroutine of the form.. 
   *              subroutine g (neq, t, y, ng, gout) 
   *              dimension y(neq), gout(ng) 
   *which supplies the vector function g by loading gout(i) with 
   *g(i), the i-th constraint function whose root is sought. 
   * 
   *c. write a main program which calls subroutine lsodar once for 
   *each point at which answers are desired.  this should also provide 
   *for possible use of int unit 6 for output of error messages by 
   *lsodar.  on the first call to lsodar, supply arguments as follows.. 
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
   *         to be less than 
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
   *            22 + neq * Max(16, neq + 9) + 3*ng. 
   *         see also paragraph f below. 
   *lrw    = declared length of rwork (in user-s dimension). 
   *iwork  = int work array of length at least  20 + neq. 
   *liw    = declared length of iwork (in user-s dimension). 
   *jac    = name of subroutine for jacobian matrix. 
   *         use a dummy name.  see also paragraph f below. 
   *jt     = jacobian type indicator.  set jt = 2. 
   *         see also paragraph f below. 
   *g      = name of subroutine for constraint functions, whose 
   *         roots are desired during the integration. 
   *         this name must be declared external in calling program. 
   *ng     = number of constraint functions g(i).  if there are none, 
   *         set ng = 0, and pass a dummy name for g. 
   *jroot  = int array of length ng for output of root information. 
   *         see next paragraph. 
   *note that the main program must declare arrays y, rwork, iwork, 
   *jroot, and possibly atol. 
   * 
   *d. the output from the first call (or any call) is.. 
   *     y = array of computed values of y(t) vector. 
   *     t = corresponding value of independent variable.  this is 
   *         tout if istate = 2, or the root location if istate = 3, 
   *         or the farthest point reached if lsodar was unsuccessful. 
   *istate = 2 or 3  if lsodar was successful, negative otherwise. 
   *          2 means no root was found, and tout was reached as desired. 
   *          3 means a root was found prior to reaching tout. 
   *         -1 means excess work done on this call (perhaps wrong jt). 
   *         -2 means excess accuracy requested (tolerances too small). 
   *         -3 means illegal input detected (see printed message). 
   *         -4 means repeated error test failures (check all inputs). 
   *         -5 means repeated convergence failures (perhaps bad jacobian 
   *            supplied or wrong choice of jt or tolerances). 
   *         -6 means error weight became zero during problem. (solution 
   *            component i vanished, and atol or atol(i) = 0.) 
   *         -7 means work space insufficient to finish (see messages). 
   *jroot  = array showing roots found if istate = 3 on return. 
   *         jroot(i) = 1 if g(i) has a root at t, or 0 otherwise. 
   * 
   *e. to continue the integration after a successful return, proceed 
   *as follows.. 
   * (a) if istate = 2 on return, reset tout and call lsodar again. 
   * (b) if istate = 3 on return, reset istate to 2 and call lsodar again. 
   *in either case, no other parameters need be reset. 
   * 
   *f. note.. if and when lsodar regards the problem as stiff, and 
   *switches methods accordingly, it must make use of the neq by neq 
   *jacobian matrix, j = df/dy.  for the sake of simplicity, the 
   *inputs to lsodar recommended in paragraph c above cause lsodar to 
   *treat j as a full matrix, and to approximate it internally by 
   *difference quotients.  alternatively, j can be treated as a band 
   *matrix (with great potential reduction in the size of the rwork 
   *array).  also, in either the full or banded case, the user can supply 
   *j in closed form, with a routine whose name is passed as the jac 
   *argument.  these alternatives are described in the paragraphs on 
   *rwork, jac, and jt in the full description of the call sequence below. 
   * 
   *----------------------------------------------------------------------- 
   *example problem. 
   * 
   *the following is a simple example problem, with the coding 
   *needed for its solution by lsodar.  the problem is from chemical 
   *kinetics, and consists of the following three rate equations.. 
   *    dy1/dt = -.04*y1 + 1.e4*y2*y3 
   *    dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2 
   *    dy3/dt = 3.e7*y2**2 
   *on the interval from t = 0.0 to t = 4.e10, with initial conditions 
   *y1 = 1.0, y2 = y3 = 0.  the problem is stiff. 
   *in addition, we want to find the values of t, y1, y2, and y3 at which 
   *  (1) y1 reaches the value 1.e-4, and 
   *  (2) y3 reaches the value 1.e-2. 
   * 
   *the following coding solves this problem with lsodar, 
   *printing results at t = .4, 4., ..., 4.e10, and at the computed 
   *roots.  it uses itol = 2 and atol much smaller for y2 than y1 or y3 
   *because y2 has much smaller values. 
   *at the end of the run, statistical quantities of interest are 
   *printed (see optional outputs in the full description below). 
   * 
   *    external fex, gex 
   *    double precision atol, rtol, rwork, t, tout, y 
   *    dimension y(3), atol(3), rwork(76), iwork(23), jroot(2) 
   *    neq = 3 
   *    y(1) = 1.0d0 
   *    y(2) = 0.0d0 
   *    y(3) = 0.0d0 
   *    t = 0.0d0 
   *    tout = 0.4d0 
   *    itol = 2 
   *    rtol = 1.0d-4 
   *    atol(1) = 1.0d-6 
   *    atol(2) = 1.0d-10 
   *    atol(3) = 1.0d-6 
   *    itask = 1 
   *    istate = 1 
   *    iopt = 0 
   *    lrw = 76 
   *    liw = 23 
   *    jt = 2 
   *    ng = 2 
   *    do 40 iout = 1,12 
   *10    call lsodar(fex,neq,y,t,tout,itol,rtol,atol,itask,istate, 
   *   1     iopt,rwork,lrw,iwork,liw,jdum,jt,gex,ng,jroot) 
   *      write(6,20)t,y(1),y(2),y(3) 
   *20    format(7h at t =,e12.4,6h   y =,3e14.6) 
   *      if (istate .lt. 0) go to 80 
   *      if (istate .eq. 2) go to 40 
   *      write(6,30)jroot(1),jroot(2) 
   *30    format(5x,35h the above line is a root,  jroot =,2i5) 
   *      istate = 2 
   *      go to 10 
   *40    tout = tout*10.0d0 
   *    write(6,60)iwork(11),iwork(12),iwork(13),iwork(10), 
   *   1   iwork(19),rwork(15) 
   *60  format(/12h no. steps =,i4,11h  no. f-s =,i4,11h  no. j-s =,i4, 
   *   1   11h  no. g-s =,i4/ 
   *   2   19h method last used =,i2,25h   last switch was at t =,e12.4) 
   *    stop 
   *80  write(6,90)istate 
   *90  format(///22h error halt.. istate =,i3) 
   *    stop 
   *    end 
   * 
   *    subroutine fex (neq, t, y, ydot) 
   *    double precision t, y, ydot 
   *    dimension y(3), ydot(3) 
   *    ydot(1) = -0.04d0*y(1) + 1.0d4*y(2)*y(3) 
   *    ydot(3) = 3.0d7*y(2)*y(2) 
   *    ydot(2) = -ydot(1) - ydot(3) 
   *    return 
   *    end 
   * 
   *    subroutine gex (neq, t, y, ng, gout) 
   *    double precision t, y, gout 
   *    dimension y(3), gout(2) 
   *    gout(1) = y(1) - 1.0d-4 
   *    gout(2) = y(3) - 1.0d-2 
   *    return 
   *    end 
   * 
   *the output of this program (on a cdc-7600 in single precision) 
   *is as follows.. 
   * 
   *  at t =  2.6400e-01   y =  9.899653e-01  3.470563e-05  1.000000e-02 
   *       the above line is a root,  jroot =    0    1 
   *  at t =  4.0000e-01   y =  9.851712e-01  3.386380e-05  1.479493e-02 
   *  at t =  4.0000e+00   y =  9.055333e-01  2.240655e-05  9.444430e-02 
   *  at t =  4.0000e+01   y =  7.158403e-01  9.186334e-06  2.841505e-01 
   *  at t =  4.0000e+02   y =  4.505250e-01  3.222964e-06  5.494717e-01 
   *  at t =  4.0000e+03   y =  1.831975e-01  8.941774e-07  8.168016e-01 
   *  at t =  4.0000e+04   y =  3.898730e-02  1.621940e-07  9.610125e-01 
   *  at t =  4.0000e+05   y =  4.936363e-03  1.984221e-08  9.950636e-01 
   *  at t =  4.0000e+06   y =  5.161831e-04  2.065786e-09  9.994838e-01 
   *  at t =  2.0745e+07   y =  1.000000e-04  4.000395e-10  9.999000e-01 
   *       the above line is a root,  jroot =    1    0 
   *  at t =  4.0000e+07   y =  5.179817e-05  2.072032e-10  9.999482e-01 
   *  at t =  4.0000e+08   y =  5.283401e-06  2.113371e-11  9.999947e-01 
   *  at t =  4.0000e+09   y =  4.659031e-07  1.863613e-12  9.999995e-01 
   *  at t =  4.0000e+10   y =  1.404280e-08  5.617126e-14  1.000000e+00 
   * 
   *  no. steps = 361  no. f-s = 693  no. j-s =  64  no. g-s = 390 
   *  method last used = 2   last switch was at t =  6.0092e-03 
   *----------------------------------------------------------------------- 
   *full description of user interface to lsodar. 
   * 
   *the user interface to lsodar consists of the following parts. 
   * 
   *i.   the call sequence to subroutine lsodar, which is a driver 
   *     routine for the solver.  this includes descriptions of both 
   *     the call sequence arguments and of user-supplied routines. 
   *     following these descriptions is a description of 
   *     optional inputs available through the call sequence, and then 
   *     a description of optional outputs (in the work arrays). 
   * 
   *ii.  descriptions of other routines in the lsodar package that may be 
   *     (optionally) called by the user.  these provide the ability to 
   *     alter error message handling, save and restore the internal 
   *     common, and obtain specified derivatives of the solution y(t). 
   * 
   *iii. descriptions of common blocks to be declared in overlay 
   *     or similar environments, or to be saved when doing an interrupt 
   *     of the problem and continued solution later. 
   * 
   *iv.  description of a subroutine in the lsodar package, 
   *     which the user may replace with his own version, if desired. 
   *     this relates to the measurement of errors. 
   * 
   *----------------------------------------------------------------------- 
   *part i.  call sequence. 
   * 
   *the call sequence parameters used for input only are 
   *    f, neq, tout, itol, rtol, atol, itask, iopt, lrw, liw, jac, 
   *    jt, g, and ng, 
   *that used only for output is  jroot, 
   *and those used for both input and output are 
   *    y, t, istate. 
   *the work arrays rwork and iwork are also used for conditional and 
   *optional inputs and optional outputs.  (the term output here refers 
   *to the return from subroutine lsodar to the user-s calling program.) 
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
   *         (the lsodar package accesses only neq(1).)  in either case, 
   *         this parameter is passed as the neq argument in all calls 
   *         to f, jac, and g.  hence, if it is an array, locations 
   *         neq(2),... may be used to store other int data and pass 
   *         it to f, jac, and g.  each such subroutine must include 
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
   *         this array is passed as the y argument in all calls to f, 
   *         jac, and g.  hence its length may exceed neq, and locations 
   *         y(neq+1),... may be used to store other real data and 
   *         pass it to f, jac, and g.  (the lsodar package accesses only 
   *         y(1),...,y(neq).) 
   * 
   *t      = the independent variable.  on input, t is used only on the 
   *         first call, as the initial point of the integration. 
   *         on output, after each call, t is the value at which a 
   *         computed solution y is evaluated (usually the same as tout). 
   *         if a root was found, t is the computed location of the 
   *         root reached first, on output. 
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
   *                     max-norm of ( e(i)/ewt(i) )   .le.   1, 
   *         where ewt = (ewt(i)) is a vector of positive error weights. 
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
   *         error controls can be obtained by substituting a 
   *         user-supplied routine for the setting of ewt. 
   *         see part iv below. 
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
   *            neq, itol, rtol, atol, iopt, lrw, liw, jt, ml, mu, 
   *            and any optional inputs except h0, mxordn, and mxords. 
   *            (see iwork description for ml and mu.) 
   *            in addition, immediately following a return with 
   *            istate = 3 (root found), ng and g may be changed. 
   *            (but changing ng from 0 to .gt. 0 is not allowed.) 
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
   *          2  means the integration was performed successfully, and 
   *             no roots were found. 
   *          3  means the integration was successful, and one or more 
   *             roots were found before satisfying the stop condition 
   *             specified by itask.  see jroot. 
   *          4  zero lifting 
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
   *             this may be caused by an inaccurate jacobian matrix, 
   *             if one is being used. 
   *         -6  means ewt(i) became zero for some i during the 
   *             integration.  pure relative error control (atol(i)=0.0) 
   *             was requested on a variable which has now vanished. 
   *             the integration was successful as far as t. 
   *         -7  means the length of rwork and/or iwork was too small to 
   *             proceed, but the integration was successful as far as t. 
   *             this happens when lsodar chooses to switch methods 
   *             but lrw and/or liw is too small for the new method. 
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
   *rwork  = a real array (double precision) for work space, and (in the 
   *         first 20 words) for conditional and optional inputs and 
   *         optional outputs. 
   *         as lsodar switches automatically between stiff and nonstiff 
   *         methods, the required length of rwork can change during the 
   *         problem.  thus the rwork array passed to lsodar can either 
   *         have a static (fixed) length large enough for both methods, 
   *         or have a dynamic (changing) length altered by the calling 
   *         program in response to output from lsodar. 
   * 
   *                      --- fixed length case --- 
   *         if the rwork length is to be fixed, it should be at least 
   *              Max(lrn, lrs), 
   *         where lrn and lrs are the rwork lengths required when the 
   *         current method is nonstiff or stiff, respectively. 
   * 
   *         the separate rwork length requirements lrn and lrs are 
   *         as follows.. 
   *         if neq is constant and the maximum method orders have 
   *         their default values, then 
   *            lrn = 20 + 16*neq + 3*ng, 
   *            lrs = 22 + 9*neq + neq**2 + 3*ng           (jt = 1 or 2), 
   *            lrs = 22 + 10*neq + (2*ml+mu)*neq + 3*ng   (jt = 4 or 5). 
   *         under any other conditions, lrn and lrs are given by.. 
   *            lrn = 20 + nyh*(mxordn+1) + 3*neq + 3*ng, 
   *            lrs = 20 + nyh*(mxords+1) + 3*neq + lmat + 3*ng, 
   *         where 
   *            nyh    = the initial value of neq, 
   *            mxordn = 12, unless a smaller value is given as an 
   *                     optional input, 
   *            mxords = 5, unless a smaller value is given as an 
   *                     optional input, 
   *            lmat   = length of matrix work space.. 
   *            lmat   = neq**2 + 2              if jt = 1 or 2, 
   *            lmat   = (2*ml + mu + 1)*neq + 2 if jt = 4 or 5. 
   * 
   *                      --- dynamic length case --- 
   *         if the length of rwork is to be dynamic, then it should 
   *         be at least lrn or lrs, as defined above, depending on the 
   *         current method.  initially, it must be at least lrn (since 
   *         lsodar starts with the nonstiff method).  on any return 
   *         from lsodar, the optional output mcur indicates the current 
   *         method.  if mcur differs from the value it had on the 
   *         previous return, or if there has only been one call to 
   *         lsodar and mcur is now 2, then lsodar has switched 
   *         methods during the last call, and the length of rwork 
   *         should be reset (to lrn if mcur = 1, or to lrs if 
   *         mcur = 2).  (an increase in the rwork length is required 
   *         if lsodar returned istate = -7, but not otherwise.) 
   *         after resetting the length, call lsodar with istate = 3 
   *         to signal that change. 
   * 
   *lrw    = the length of the array rwork, as declared by the user. 
   *         (this will be checked by the solver.) 
   * 
   *iwork  = an int array for work space. 
   *         as lsodar switches automatically between stiff and nonstiff 
   *         methods, the required length of iwork can change during 
   *         problem, between 
   *            lis = 20 + neq   and   lin = 20, 
   *         respectively.  thus the iwork array passed to lsodar can 
   *         either have a fixed length of at least 20 + neq, or have a 
   *         dynamic length of at least lin or lis, depending on the 
   *         current method.  the comments on dynamic length under 
   *         rwork above apply here.  initially, this length need 
   *         only be at least lin = 20. 
   * 
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
   *                      these are required if jt is 4 or 5, and 
   *                      ignored otherwise.  ml and mu may in fact be 
   *                      the band parameters for a matrix to which 
   *                      df/dy is only approximately equal. 
   * 
   *liw    = the length of the array iwork, as declared by the user. 
   *         (this will be checked by the solver.) 
   * 
   *note.. the base addresses of the work arrays must not be 
   *altered between calls to lsodar for the same problem. 
   *the contents of the work arrays must not be altered 
   *between calls, except possibly for the conditional and 
   *optional inputs, and except for the last 3*neq words of rwork. 
   *the latter space is used for internal scratch space, and so is 
   *available for use by the user outside lsodar between calls, if 
   *desired (but not for use by f, jac, or g). 
   * 
   *jac    = the name of the user-supplied routine to compute the 
   *         jacobian matrix, df/dy, if jt = 1 or 4.  the jac routine 
   *         is optional, but if the problem is expected to be stiff much 
   *         of the time, you are encouraged to supply jac, for the sake 
   *         of efficiency.  (alternatively, set jt = 2 or 5 to have 
   *         lsodar compute df/dy internally by difference quotients.) 
   *         if and when lsodar uses df/dy, if treats this neq by neq 
   *         matrix either as full (jt = 1 or 2), or as banded (jt = 
   *         4 or 5) with half-bandwidths ml and mu (discussed under 
   *         iwork above).  in either case, if jt = 1 or 4, the jac 
   *         routine must compute df/dy as a function of the scalar t 
   *         and the vector y.  it is to have the form 
   *              subroutine jac (neq, t, y, ml, mu, pd, nrowpd) 
   *              dimension y(1), pd(nrowpd,1) 
   *         where neq, t, y, ml, mu, and nrowpd are input and the array 
   *         pd is to be loaded with partial derivatives (elements of 
   *         the jacobian matrix) on output.  pd must be given a first 
   *         dimension of nrowpd.  t and y have the same meaning as in 
   *         subroutine f.  (in the dimension statement above, 1 is a 
   *         dummy dimension.. it can be replaced by any value.) 
   *              in the full matrix case (jt = 1), ml and mu are 
   *         ignored, and the jacobian is to be loaded into pd in 
   *         columnwise manner, with df(i)/dy(j) loaded into pd(i,j). 
   *              in the band matrix case (jt = 4), the elements 
   *         within the band are to be loaded into pd in columnwise 
   *         manner, with diagonal lines of df/dy loaded into the rows 
   *         of pd.  thus df(i)/dy(j) is to be loaded into pd(i-j+mu+1,j). 
   *         ml and mu are the half-bandwidth parameters (see iwork). 
   *         the locations in pd in the two triangular areas which 
   *         correspond to nonexistent matrix elements can be ignored 
   *         or loaded arbitrarily, as they are overwritten by lsodar. 
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
   *jt     = jacobian type indicator.  used only for input. 
   *         jt specifies how the jacobian matrix df/dy will be 
   *         treated, if and when lsodar requires this matrix. 
   *         jt has the following values and meanings.. 
   *          1 means a user-supplied full (neq by neq) jacobian. 
   *          2 means an internally generated (difference quotient) full 
   *            jacobian (using neq extra calls to f per df/dy value). 
   *          4 means a user-supplied banded jacobian. 
   *          5 means an internally generated banded jacobian (using 
   *            ml+mu+1 extra calls to f per df/dy evaluation). 
   *         if jt = 1 or 4, the user must supply a subroutine jac 
   *         (the name is arbitrary) as described above under jac. 
   *         if jt = 2 or 5, a dummy argument can be used. 
   * 
   *g      = the name of subroutine for constraint functions, whose 
   *         roots are desired during the integration.  it is to have 
   *         the form 
   *              subroutine g (neq, t, y, ng, gout) 
   *              dimension y(neq), gout(ng) 
   *         where neq, t, y, and ng are input, and the array gout 
   *         is output.  neq, t, and y have the same meaning as in 
   *         the f routine, and gout is an array of length ng. 
   *         for i = 1,...,ng, this routine is to load into gout(i) 
   *         the value at (t,y) of the i-th constraint function g(i). 
   *         lsodar will find roots of the g(i) of odd multiplicity 
   *         (i.e. sign changes) as they occur during the integration. 
   *         g must be declared external in the calling program. 
   * 
   *         caution.. because of numerical errors in the functions 
   *         g(i) due to roundoff and integration error, lsodar may 
   *         return false roots, or return the same root at two or more 
   *         nearly equal values of t.  if such false roots are 
   *         suspected, the user should consider smaller error tolerances 
   *         and/or higher precision in the evaluation of the g(i). 
   * 
   *         if a root of some g(i) defines the end of the problem, 
   *         the input to lsodar should nevertheless allow integration 
   *         to a point slightly past that root, so that lsodar can 
   *         locate the root by interpolation. 
   * 
   *         subroutine g may access user-defined quantities in 
   *         neq(2),... and y(neq(1)+1),... if neq is an array 
   *         (dimensioned in g) and y has length exceeding neq(1). 
   *         see the descriptions of neq and y above. 
   * 
   *ng     = number of constraint functions g(i).  if there are none, 
   *         set ng = 0, and pass a dummy name for g. 
   * 
   *jroot  = int array of length ng.  used only for output. 
   *         on a return with istate = 3 (one or more roots found), 
   *         jroot(i) = 1 if g(i) has a root at t, or jroot(i) = 0 if not. 
   *----------------------------------------------------------------------- 
   *optional inputs. 
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
   *ixpr    iwork(5)  flag to generate extra printing at method switches. 
   *                  ixpr = 0 means no extra printing (the default). 
   *                  ixpr = 1 means print data on each switch. 
   *                  t, h, and nst will be printed on the same int 
   *                  unit as used for error messages. 
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
   *mxordn  iwork(8)  the maximum order to be allowed for the nonstiff 
   *                  (adams) method.  the default value is 12. 
   *                  if mxordn exceeds the default value, it will 
   *                  be reduced to the default value. 
   *                  mxordn is held constant during the problem. 
   * 
   *mxords  iwork(9)  the maximum order to be allowed for the stiff 
   *                  (bdf) method.  the default value is 5. 
   *                  if mxords exceeds the default value, it will 
   *                  be reduced to the default value. 
   *                  mxords is held constant during the problem. 
   *----------------------------------------------------------------------- 
   *optional outputs. 
   * 
   *as optional additional output from lsodar, the variables listed 
   *below are quantities related to the performance of lsodar 
   *which are available to the user.  these are communicated by way of 
   *the work arrays, but also have internal mnemonic names as shown. 
   *except where stated otherwise, all of these outputs are defined 
   *on any successful return from lsodar, and on any return with 
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
   *tsw     rwork(15) the value of t at the time of the last method 
   *                  switch, if any. 
   * 
   *nge     iwork(10) the number of g evaluations for the problem so far. 
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
   *lenrw   iwork(17) the length of rwork actually required, assuming 
   *                  that the length of rwork is to be fixed for the 
   *                  rest of the problem, and that switching may occur. 
   *                  this is defined on normal returns and on an illegal 
   *                  input return for insufficient storage. 
   * 
   *leniw   iwork(18) the length of iwork actually required, assuming 
   *                  that the length of iwork is to be fixed for the 
   *                  rest of the problem, and that switching may occur. 
   *                  this is defined on normal returns and on an illegal 
   *                  input return for insufficient storage. 
   * 
   *mused   iwork(19) the method indicator for the last successful step.. 
   *                  1 means adams (nonstiff), 2 means bdf (stiff). 
   * 
   *mcur    iwork(20) the current method indicator.. 
   *                  1 means adams (nonstiff), 2 means bdf (stiff). 
   *                  this is the method to be attempted 
   *                  on the next step.  thus it differs from mused 
   *                  only if a method switch has just been made. 
   * 
   *the following two arrays are segments of the rwork array which 
   *may also be of interest to the user as optional outputs. 
   *for each array, the table below gives its internal name, 
   *its base address in rwork, and its description. 
   * 
   *name    base address      description 
   * 
   *yh      21 + 3*ng      the nordsieck history array, of size nyh by 
   *                       (nqcur + 1), where nyh is the initial value 
   *                       of neq.  for j = 0,1,...,nqcur, column j+1 
   *                       of yh contains hcur**j/factorial(j) times 
   *                       the j-th derivative of the interpolating 
   *                       polynomial currently representing the solution, 
   *                       evaluated at t = tcur. 
   * 
   *acor     lacor         array of size neq used for the accumulated 
   *        (from common   corrections on each step, scaled on output 
   *          as noted)    to represent the estimated local error in y 
   *                       on the last step.  this is the vector e in 
   *                       the description of the error control.  it is 
   *                       defined only on a successful return from 
   *                       lsodar.  the base address lacor is obtained by 
   *                       including in the user-s program the 
   *                       following 3 lines.. 
   *                          double precision rls 
   *                          common /ls0001/ rls(219), ils(39) 
   *                          lacor = ils(5) 
   * 
   *----------------------------------------------------------------------- 
   *part ii.  other routines callable. 
   * 
   *the following are optional calls which the user may make to 
   *gain additional capabilities in conjunction with lsodar. 
   *(the routines xsetun and xsetf are designed to conform to the 
   *slatec error handling package.) 
   * 
   *    form of call                  function 
   *  call xsetun(lun)          set the int unit number, lun, for 
   *                            output of messages from lsodar, if 
   *                            the default is not desired. 
   *                            the default value of lun is 6. 
   * 
   *  call xsetf(mflag)         set a flag to control the printing of 
   *                            messages by lsodar. 
   *                            mflag = 0 means do not print. (danger.. 
   *                            this risks losing valuable information.) 
   *                            mflag = 1 means print (the default). 
   * 
   *                            either of the above calls may be made at 
   *                            any time and will take effect immediately. 
   * 
   *  call svcar (rsav, isav)   store in rsav and isav the contents 
   *                            of the internal common blocks used by 
   *                            lsodar (see part iii below). 
   *                            rsav must be a real array of length 246 
   *                            or more, and isav must be an int 
   *                            array of length 59 or more. 
   * 
   *  call rscar (rsav, isav)   restore, from rsav and isav, the contents 
   *                            of the internal common blocks used by 
   *                            lsodar.  presumes a prior call to svcar 
   *                            with the same arguments. 
   * 
   *                            svcar and rscar are useful if 
   *                            interrupting a run and restarting 
   *                            later, or alternating between two or 
   *                            more problems solved with lsodar. 
   * 
   *  call intdy(,,,,,)         provide derivatives of y, of various 
   *       (see below)          orders, at a specified point t, if 
   *                            desired.  it may be called only after 
   *                            a successful return from lsodar. 
   * 
   *the detailed instructions for using intdy are as follows. 
   *the form of the call is.. 
   * 
   *  call intdy (t, k, rwork(lyh), nyh, dky, iflag) 
   * 
   *the input parameters are.. 
   * 
   *t         = value of independent variable where answers are desired 
   *            (normally the same as the t last returned by lsodar). 
   *            for valid results, t must lie between tcur - hu and tcur. 
   *            (see optional outputs for tcur and hu.) 
   *k         = int order of the derivative desired.  k must satisfy 
   *            0 .le. k .le. nqcur, where nqcur is the current order 
   *            (see optional outputs).  the capability corresponding 
   *            to k = 0, i.e. computing y(t), is already provided 
   *            by lsodar directly.  since nqcur .ge. 1, the first 
   *            derivative dy/dt is always available with intdy. 
   *lyh       = 21 + 3*ng = base address in rwork of the history array yh. 
   *nyh       = column length of yh, equal to the initial value of neq. 
   * 
   *the output parameters are.. 
   * 
   *dky       = a real array of length neq containing the computed value 
   *            of the k-th derivative of y(t). 
   *iflag     = int flag, returned as 0 if k and t were legal, 
   *            -1 if k was illegal, and -2 if t was illegal. 
   *            on an error return, a message is also written. 
   *----------------------------------------------------------------------- 
   *part iii.  common blocks. 
   * 
   *if lsodar is to be used in an overlay situation, the user 
   *must declare, in the primary overlay, the variables in.. 
   *  (1) the call sequence to lsodar, 
   *  (2) the four internal common blocks 
   *        /ls0001/  of length  258  (219 double precision words 
   *                        followed by 39 int words), 
   *        /lsa001/  of length  31    (22 double precision words 
   *                        followed by  9 int words), 
   *        /lsr001/  of length  14     (5 double precision words 
   *                        followed by  9 int words), 
   *        /eh0001/  of length  2 (int words). 
   * 
   *if lsodar is used on a system in which the contents of internal 
   *common blocks are not preserved between calls, the user should 
   *declare the above common blocks in his main program to insure 
   *that their contents are preserved. 
   * 
   *if the solution of a given problem by lsodar is to be interrupted 
   *and then later continued, such as when restarting an interrupted run 
   *or alternating between two or more problems, the user should save, 
   *following the return from the last lsodar call prior to the 
   *interruption, the contents of the call sequence variables and the 
   *internal common blocks, and later restore these values before the 
   *next lsodar call for that problem.  to save and restore the common 
   *blocks, use subroutines svcar and rscar (see part ii above). 
   * 
   *----------------------------------------------------------------------- 
   *part iv.  optionally replaceable solver routines. 
   * 
   *below is a description of a routine in the lsodar package which 
   *relates to the measurement of errors, and can be 
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
   *where neq, itol, rtol, and atol are as in the lsodar call sequence, 
   *ycur contains the current dependent variable vector, and 
   *ewt is the array of weights set by ewset. 
   * 
   *if the user supplies this subroutine, it must return in ewt(i) 
   *(i = 1,...,neq) a positive quantity suitable for comparing errors 
   *in y(i) to.  the ewt array returned by ewset is passed to the 
   *vmnorm routine, and also used by lsodar in the computation 
   *of the optional output imxer, and the increments for difference 
   *quotient jacobians. 
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
   *----------------------------------------------------------------------- 
   *----------------------------------------------------------------------- 
   *other routines in the lsodar package. 
   * 
   *in addition to subroutine lsodar, the lsodar package includes the 
   *following subroutines and function routines.. 
   * rchek    does preliminary checking for roots, and serves as an 
   *          interface between subroutine lsodar and subroutine roots. 
   * roots    finds the leftmost root of a set of functions. 
   * intdy    computes an interpolated value of the y vector at t = tout. 
   * stoda    is the core integrator, which does one step of the 
   *          integration and the associated error control. 
   * cfode    sets all method coefficients and test constants. 
   * prja     computes and preprocesses the jacobian matrix j = df/dy 
   *          and the newton iteration matrix p = i - h*l0*j. 
   * solsy    manages solution of linear system in chord iteration. 
   * ewset    sets the error weight vector ewt before each step. 
   * vmnorm   computes the weighted max-norm of a vector. 
   * fnorm    computes the norm of a full matrix consistent with the 
   *          weighted max-norm on vectors. 
   * bnorm    computes the norm of a band matrix consistent with the 
   *          weighted max-norm on vectors. 
   * svcar and rscar   are user-callable routines to save and restore, 
   *          respectively, the contents of the internal common blocks. 
   * dgefa and dgesl   are routines from linpack for solving full 
   *          systems of linear algebraic equations. 
   * dgbfa and dgbsl   are routines from linpack for solving banded 
   *          linear systems. 
   * daxpy, dscal, idamax, ddot, and dcopy   are basic linear algebra 
   *          modules (blas) used by the above linpack routines. 
   * dlamch   computes the unit roundoff in a machine-independent manner. 
   * xerrwv, xsetun, and xsetf   handle the printing of all error 
   *          messages and warnings.  xerrwv is machine-dependent. 
   *note..  vmnorm, fnorm, bnorm, idamax, ddot, and dlamch are function 
   *routines.  all the others are subroutines. 
   * 
   *the intrinsic and external routines used by lsodar are.. 
   *dabs, dmax1, dmin1, dfloat, max0, min0, mod, dsign, dsqrt, and write. 
   * 
   *a block data subprogram is also included with the package, 
   *for loading some of the variables in internal common. 
   * 
   *----------------------------------------------------------------------- 
   *the following card is for optimized compilation on lll compilers. 
   *lll. optimize 
   *----------------------------------------------------------------------- 
   *use as a pointer to datas for external 
   *----------------------------------------------------------------------- 
   *the following three internal common blocks contain 
   *(a) variables which are local to any subroutine but whose values must 
   *    be preserved between calls to the routine (own variables), and 
   *(b) variables which are communicated between subroutines. 
   *the structure of each block is as follows..  all real variables are 
   *listed first, followed by all ints.  within each type, the 
   *variables are grouped with those local to subroutine lsodar first, 
   *then those local to subroutine roots or subroutine stoda 
   *(no other routines have own variables), and finally those used 
   *for communication.  the block ls0001 is declared in subroutines 
   *lsodar, intdy, stoda, prja, and solsy.  the block lsa001 is declared 
   *in subroutines lsodar, stoda, and prja.  the block lsr001 is declared 
   *in subroutines lsodar, rchek, and roots.  groups of variables are 
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
  --jroot;

  /* Function Body */
  /*----------------------------------------------------------------------- 
   *block a. 
   *this code block is executed on every call. 
   *it tests istate and itask for legality and branches appropriately. 
   *if istate .gt. 1 but the flag init shows that initialization has 
   *not yet been done, an error return occurs. 
   *if istate = 1 and tout = t, jump to block g and return immediately. 
   *----------------------------------------------------------------------- 
   */
  if (*istate < 1 || *istate > 3)
    {
      goto L601;
    }
  if (*itask < 1 || *itask > 5)
    {
      goto L602;
    }
  lsr001_1.itaskc = *itask;
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
   *jt, ml, mu, and ng. 
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
  if (*jt == 3 || *jt < 1 || *jt > 5)
    {
      goto L608;
    }
  lsa001_1.jtyp = *jt;
  if (*jt <= 2)
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
  if (*ng < 0)
    {
      goto L630;
    }
  if (*istate == 1)
    {
      goto L35;
    }
  if (lsr001_1.irfnd == 0 && *ng != lsr001_1.ngc)
    {
      goto L631;
    }
 L35:
  lsr001_1.ngc = *ng;
  /*next process and check the optional inputs. -------------------------- 
   */
  if (*iopt == 1)
    {
      goto L40;
    }
  lsa001_1.ixpr = 0;
  ls0001_1.mxstep = mxstp0;
  ls0001_1.mxhnil = mxhnl0;
  ls0001_1.hmxi = 0.;
  ls0001_1.hmin = 0.;
  if (*istate != 1)
    {
      goto L60;
    }
  h0 = 0.;
  lsa001_1.mxordn = mord[0];
  lsa001_1.mxords = mord[1];
  goto L60;
 L40:
  lsa001_1.ixpr = iwork[5];
  if (lsa001_1.ixpr < 0 || lsa001_1.ixpr > 1)
    {
      goto L611;
    }
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
  lsa001_1.mxordn = iwork[8];
  if (lsa001_1.mxordn < 0)
    {
      goto L628;
    }
  if (lsa001_1.mxordn == 0)
    {
      lsa001_1.mxordn = 100;
    }
  lsa001_1.mxordn = Min (lsa001_1.mxordn, mord[0]);
  lsa001_1.mxords = iwork[9];
  if (lsa001_1.mxords < 0)
    {
      goto L629;
    }
  if (lsa001_1.mxords == 0)
    {
      lsa001_1.mxords = 100;
    }
  lsa001_1.mxords = Min (lsa001_1.mxords, mord[1]);
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
   *if istate = 1, meth is initialized to 1 here to facilitate the 
   *checking of work space lengths. 
   *pointers to segments of rwork and iwork are named by prefixing l to 
   *the name of the segment.  e.g., the segment yh starts at rwork(lyh). 
   *segments of rwork (in order) are denoted  g0, g1, gx, yh, wm, 
   *ewt, savf, acor. 
   *if the lengths provided are insufficient for the current method, 
   *an error return occurs.  this is treated as illegal input on the 
   *first call, but as a problem interruption with istate = -7 on a 
   *continuation call.  if the lengths are sufficient for the current 
   *method but not for both methods, a warning message is sent. 
   *----------------------------------------------------------------------- 
   */
 L60:
  if (*istate == 1)
    {
      ls0001_1.meth = 1;
    }
  if (*istate == 1)
    {
      ls0001_1.nyh = ls0001_1.n;
    }
  lsr001_1.lg0 = 21;
  lsr001_1.lg1 = lsr001_1.lg0 + *ng;
  lsr001_1.lgx = lsr001_1.lg1 + *ng;
  lyhnew = lsr001_1.lgx + *ng;
  if (*istate == 1)
    {
      ls0001_1.lyh = lyhnew;
    }
  if (lyhnew == ls0001_1.lyh)
    {
      goto L62;
    }
  /*if istate = 3 and ng was changed, shift yh to its new location. ------ 
   */
  lenyh = ls0001_1.l * ls0001_1.nyh;
  if (*lrw < lyhnew - 1 + lenyh)
    {
      goto L62;
    }
  i1 = 1;
  if (lyhnew > ls0001_1.lyh)
    {
      i1 = -1;
    }
  C2F (dcopy) (&lenyh, &rwork[ls0001_1.lyh], &i1, &rwork[lyhnew], &i1);
  ls0001_1.lyh = lyhnew;
 L62:
  len1n = lyhnew - 1 + (lsa001_1.mxordn + 1) * ls0001_1.nyh;
  len1s = lyhnew - 1 + (lsa001_1.mxords + 1) * ls0001_1.nyh;
  ls0001_1.lwm = len1s + 1;
  if (*jt <= 2)
    {
      lenwm = ls0001_1.n * ls0001_1.n + 2;
    }
  if (*jt >= 4)
    {
      lenwm = ((ml << 1) + mu + 1) * ls0001_1.n + 2;
    }
  len1s += lenwm;
  len1c = len1n;
  if (ls0001_1.meth == 2)
    {
      len1c = len1s;
    }
  len1 = Max (len1n, len1s);
  len2 = ls0001_1.n * 3;
  lenrw = len1 + len2;
  lenrwn = len1n + len2;
  lenrws = len1s + len2;
  lenrwc = len1c + len2;
  iwork[17] = lenrw;
  ls0001_1.liwm = 1;
  leniw = ls0001_1.n + 20;
  /*    ----------------------------- masking ---------------- 
   */
  leniw += *ng;
  /*    ----------------------------- masking ---------------- 
   */
  leniwc = 20;
  if (ls0001_1.meth == 2)
    {
      leniwc = leniw;
    }
  iwork[18] = leniw;
  if (*istate == 1 && *lrw < lenrwc)
    {
      goto L617;
    }
  if (*istate == 1 && *liw < leniwc)
    {
      goto L618;
    }
  if (*istate == 3 && *lrw < lenrwc)
    {
      goto L550;
    }
  if (*istate == 3 && *liw < leniwc)
    {
      goto L555;
    }
  ls0001_1.lewt = len1 + 1;
  lsa001_1.insufr = 0;
  if (*lrw >= lenrw)
    {
      goto L65;
    }
  lsa001_1.insufr = 2;
  ls0001_1.lewt = len1c + 1;
  C2F (xerrwv)
    ("lsodar-  warning.. rwork length is sufficient for now, but  ", &c__60,
     &c__103, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46, 60L);
  C2F (xerrwv)
    ("      may not be later.  integration will proceed anyway.   ", &c__60,
     &c__103, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46, 60L);
  C2F (xerrwv) ("      length needed is lenrw = i1, while lrw = i2.", &c__50,
		&c__103, &c__1, &c__2, &lenrw, lrw, &c__0, &c_b46, &c_b46,
		50L);
 L65:
  ls0001_1.lsavf = ls0001_1.lewt + ls0001_1.n;
  ls0001_1.lacor = ls0001_1.lsavf + ls0001_1.n;
  lsa001_1.insufi = 0;
  if (*liw >= leniw)
    {
      goto L70;
    }
  lsa001_1.insufi = 2;
  C2F (xerrwv)
    ("lsodar-  warning.. iwork length is sufficient for now, but  ", &c__60,
     &c__104, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46, 60L);
  C2F (xerrwv)
    ("      may not be later.  integration will proceed anyway.   ", &c__60,
     &c__104, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46, 60L);
  C2F (xerrwv) ("      length needed is leniw = i1, while liw = i2.", &c__50,
		&c__104, &c__1, &c__2, &leniw, liw, &c__0, &c_b46, &c_b46,
		50L);
 L70:
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
      /* L75: */
    }
  if (*istate == 1)
    {
      goto L100;
    }
  /*if istate = 3, set flag to signal parameter changes to stoda. -------- 
   */
  ls0001_1.jstart = -1;
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
  lsa001_1.tsw = *t;
  ls0001_1.maxord = lsa001_1.mxordn;
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
  ls0001_1.nhnil = 0;
  ls0001_1.nst = 0;
  ls0001_1.nje = 0;
  ls0001_1.nslast = 0;
  ls0001_1.hu = 0.;
  ls0001_1.nqu = 0;
  lsa001_1.mused = 0;
  ls0001_1.miter = 0;
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
   * 
   *  h0**(-2)  =  1./(tol * w0**2)  +  tol * (norm(f))**2 
   * 
   *where   w0     = Max( Abs(t), Abs(tout) ), 
   *        f      = the initial value of the vector f(t,y), and 
   *        norm() = the weighted vector norm used throughout, given by 
   *                 the vmnorm function routine, and weighted by the 
   *                 tolerances initially loaded into the ewt array. 
   *the sign of h0 is inferred from the initial values of tout and t. 
   *abs(h0) is made .le. Abs(tout-t) in any case. 
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
  sum = nsp_ode_vmnorm (&ls0001_1.n, &rwork[lf0], &rwork[ls0001_1.lewt]);
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
  /* 
   *check for a zero of g at t. ------------------------------------------ 
   */
  lsr001_1.irfnd = 0;
  lsr001_1.toutc = *tout;
  if (lsr001_1.ngc == 0)
    {
      goto L270;
    }
  /*    --------------------- masking ----------------------- 
   */
  nsp_ode_rchek2 (&c__1,  g, &neq[1], &y[1], &rwork[ls0001_1.lyh],
		     &ls0001_1.nyh, &rwork[lsr001_1.lg0],
		     &rwork[lsr001_1.lg1], &rwork[lsr001_1.lgx], &jroot[1],
		     &irt, &iwork[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /*     if (irt .lt. 0) go to 632 
   */
  goto L270;
  /*----------------------------------------------------------------------- 
   *block d. 
   *the next code block is for continuation calls only (istate = 2 or 3) 
   *and is to check stop conditions before taking a step. 
   *first, rchek is called to check for a root within the last step 
   *taken, other than the last root found there, if any. 
   *if itask = 2 or 5, and y(tn) has not yet been returned to the user 
   *because of an intervening root, return through block g. 
   *----------------------------------------------------------------------- 
   */
 L200:
  ls0001_1.nslast = ls0001_1.nst;
  /* 
   */
  irfp = lsr001_1.irfnd;
  if (lsr001_1.ngc == 0)
    {
      goto L205;
    }
  if (*itask == 1 || *itask == 4)
    {
      lsr001_1.toutc = *tout;
    }
  /*    --------------------- masking ----------------------- 
   */
  nsp_ode_rchek2 (&c__2,  g, &neq[1], &y[1], &rwork[ls0001_1.lyh],
		     &ls0001_1.nyh, &rwork[lsr001_1.lg0],
		     &rwork[lsr001_1.lg1], &rwork[lsr001_1.lgx], &jroot[1],
		     &irt, &iwork[1], param);
  /*    --------------------- masking ----------------------- 
   */
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  if (irt < 0)
    {
      goto L632;
    }
  if (irt == 1)
    {
      lsr001_1.irfnd = 1;
      *istate = 3;
      *t = lsr001_1.t0;
      goto L425;
    }
 L205:
  lsr001_1.irfnd = 0;
  if (irfp == 1 && lsr001_1.tlast != ls0001_1.tn && *itask == 2)
    {
      goto L400;
    }
  /* 
   */
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
  *t = ls0001_1.tn;
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
      *t = tcrit;
    }
  if (irfp == 1 && lsr001_1.tlast != ls0001_1.tn && *itask == 5)
    {
      goto L400;
    }
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
  /*SCI 
   *SCI  if (istate .eq. 2) jstart = -2 
   *SCI  replaced by: 
   */
  if (*istate == 2 && ls0001_1.jstart >= 0)
    {
      ls0001_1.jstart = -2;
    }
  /*----------------------------------------------------------------------- 
   *block e. 
   *the next block is normally executed for all calls and contains 
   *the call to the one-step core integrator stoda. 
   * 
   *this is a looping point for the integration steps. 
   * 
   *first check for too many steps being taken, update ewt (if not at 
   *start of problem), check for too much accuracy being requested, and 
   *check for h below the roundoff level in t. 
   *----------------------------------------------------------------------- 
   */
 L250:
  if (ls0001_1.meth == lsa001_1.mused)
    {
      goto L255;
    }
  if (lsa001_1.insufr == 1)
    {
      goto L550;
    }
  if (lsa001_1.insufi == 1)
    {
      goto L555;
    }
 L255:
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
    ls0001_1.uround * nsp_ode_vmnorm (&ls0001_1.n, &rwork[ls0001_1.lyh],
					 &rwork[ls0001_1.lewt]);
  if (tolsf <= .01)
    {
      goto L280;
    }
  tolsf *= 200.;
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
  C2F (xerrwv) ("lsodar-  warning..internal t (=r1) and h (=r2) are", &c__50,
		&c__101, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv) ("      such that in the machine, t + h = t on the next step ",
		&c__60, &c__101, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46,
		&c_b46, 59L);
  C2F (xerrwv) ("      (h = step size). solver will continue anyway", &c__50,
		&c__101, &c__1, &c__0, &c__0, &c__0, &c__2, &ls0001_1.tn,
		&ls0001_1.h__, 50L);
  if (ls0001_1.nhnil < ls0001_1.mxhnil)
    {
      goto L290;
    }
  C2F (xerrwv) ("sodar-  above warning has been issued i1 times.  ", &c__50,
		&c__102, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		49L);
  C2F (xerrwv) ("     it will not be issued again for this problem", &c__50,
		&c__102, &c__1, &c__1, &ls0001_1.mxhnil, &c__0, &c__0, &c_b46,
		&c_b46, 49L);
 L290:
  /*----------------------------------------------------------------------- 
   *    call stoda(neq,y,yh,nyh,yh,ewt,savf,acor,wm,iwm,f,jac,prja,solsy) 
   *----------------------------------------------------------------------- 
   */
  nsp_ode_stoda (&neq[1], &y[1], &rwork[ls0001_1.lyh], &ls0001_1.nyh,
		    &rwork[ls0001_1.lyh], &rwork[ls0001_1.lewt],
		    &rwork[ls0001_1.lsavf], &rwork[ls0001_1.lacor],
		    &rwork[ls0001_1.lwm], &iwork[ls0001_1.liwm], f,
		    jac, nsp_ode_prja , nsp_ode_solsy, param);
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
   *core integrator (kflag = 0). 
   *if a method switch was just made, record tsw, reset maxord, 
   *set jstart to -1 to signal stoda to complete the switch, 
   *and do extra printing of data if ixpr = 1. 
   *then call rchek to check for a root within the last step. 
   *then, if no root was found, check for stop conditions. 
   *----------------------------------------------------------------------- 
   */
 L300:
  ls0001_1.init = 1;
  if (ls0001_1.meth == lsa001_1.mused)
    {
      goto L310;
    }
  lsa001_1.tsw = ls0001_1.tn;
  ls0001_1.maxord = lsa001_1.mxordn;
  if (ls0001_1.meth == 2)
    {
      ls0001_1.maxord = lsa001_1.mxords;
    }
  if (ls0001_1.meth == 2)
    {
      rwork[ls0001_1.lwm] = sqrt (ls0001_1.uround);
    }
  lsa001_1.insufr = Min (lsa001_1.insufr, 1);
  lsa001_1.insufi = Min (lsa001_1.insufi, 1);
  ls0001_1.jstart = -1;
  if (lsa001_1.ixpr == 0)
    {
      goto L310;
    }
  if (ls0001_1.meth == 2)
    {
      C2F (xerrwv)
	("lsodar- a switch to the bdf (stiff) method has occurred     ",
	 &c__60, &c__105, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
	 60L);
    }
  if (ls0001_1.meth == 1)
    {
      C2F (xerrwv)
	("lsodar- a switch to the adams (nonstiff) method has occurred",
	 &c__60, &c__106, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
	 60L);
    }
  C2F (xerrwv)
    ("     at t = r1,  tentative step size h = r2,  step nst = i1 ", &c__60,
     &c__107, &c__1, &c__1, &ls0001_1.nst, &c__0, &c__2, &ls0001_1.tn,
     &ls0001_1.h__, 60L);
 L310:
  /* 
   */
  if (lsr001_1.ngc == 0)
    {
      goto L315;
    }
  /*    --------------------- masking ----------------------- 
   */
  nsp_ode_rchek2 (&c__3,  g, &neq[1], &y[1], &rwork[ls0001_1.lyh],
		     &ls0001_1.nyh, &rwork[lsr001_1.lg0],
		     &rwork[lsr001_1.lg1], &rwork[lsr001_1.lgx], &jroot[1],
		     &irt, &iwork[1], param);
  /*    --------------------- masking ----------------------- 
   */
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  if (irt == 2)
    {
      lirfnd = 2;
      *istate = 4;
      *t = lsr001_1.t0;
      goto L425;
    }
  if (irt != 1)
    {
      goto L315;
    }
  lsr001_1.irfnd = 1;
  *istate = 3;
  *t = lsr001_1.t0;
  goto L425;
 L315:
  /* 
   */
  switch (*itask)
    {
    case 1:
      goto L320;
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
 L320:
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
  /*SCI 
   *SCI  jstart = -2 
   *SCI  replaced by: 
   */
  if (ls0001_1.jstart >= 0)
    {
      ls0001_1.jstart = -2;
    }
  goto L250;
  /*itask = 5.  see if tcrit was reached and jump to exit. --------------- 
   */
 L350:
  hmx = Abs (ls0001_1.tn) + Abs (ls0001_1.h__);
  ihit = (d__1 =
	  ls0001_1.tn - tcrit, Abs (d__1)) <= ls0001_1.uround * 100. * hmx;
  /*----------------------------------------------------------------------- 
   *block g. 
   *the following block handles all successful returns from lsodar. 
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
 L425:
  ls0001_1.illin = 0;
  rwork[11] = ls0001_1.hu;
  rwork[12] = ls0001_1.h__;
  rwork[13] = ls0001_1.tn;
  rwork[15] = lsa001_1.tsw;
  iwork[11] = ls0001_1.nst;
  iwork[12] = ls0001_1.nfe;
  iwork[13] = ls0001_1.nje;
  iwork[14] = ls0001_1.nqu;
  iwork[15] = ls0001_1.nq;
  iwork[19] = lsa001_1.mused;
  iwork[20] = ls0001_1.meth;
  iwork[10] = lsr001_1.nge;
  lsr001_1.tlast = *t;
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
    ("lsodar-  repeated calls with istate = 1 and tout = t (=r1)  ", &c__60,
     &c__301, &c__1, &c__0, &c__0, &c__0, &c__1, t, &c_b46, 60L);
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
  C2F (xerrwv) ("lsodar-  at current t (=r1), mxstep (=i1) steps", &c__50,
		&c__201, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		47L);
  C2F (xerrwv) ("      taken on this call before reaching tout     ", &c__50,
		&c__201, &c__1, &c__1, &ls0001_1.mxstep, &c__0, &c__1,
		&ls0001_1.tn, &c_b46, 50L);
  *istate = -1;
  goto L580;
  /*ewt(i) .le. 0.0 for some i (not at start of problem). ---------------- 
   */
 L510:
  ewti = rwork[ls0001_1.lewt + i__ - 1];
  C2F (xerrwv) ("lsodar-  at t (=r1), ewt(i1) has become r2 .le. 0.", &c__50,
		&c__202, &c__1, &c__1, &i__, &c__0, &c__2, &ls0001_1.tn,
		&ewti, 50L);
  *istate = -6;
  goto L580;
  /*too much accuracy requested for machine precision. ------------------- 
   */
 L520:
  C2F (xerrwv) ("lsodar-  at t (=r1), too much accuracy requested ", &c__50,
		&c__203, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		49L);
  C2F (xerrwv) ("      for precision of machine..  see tolsf (=r2)", &c__50,
		&c__203, &c__1, &c__0, &c__0, &c__0, &c__2, &ls0001_1.tn,
		&tolsf, 49L);
  rwork[14] = tolsf;
  *istate = -2;
  goto L580;
  /*kflag = -1.  error test failed repeatedly or with Abs(h) = hmin. ----- 
   */
 L530:
  C2F (xerrwv) ("lsodar-  at t(=r1) and step size h(=r2), the error", &c__50,
		&c__204, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv) ("      test failed repeatedly or with Abs(h) = hmin", &c__50,
		&c__204, &c__1, &c__0, &c__0, &c__0, &c__2, &ls0001_1.tn,
		&ls0001_1.h__, 50L);
  *istate = -4;
  goto L560;
  /*kflag = -2.  convergence failed repeatedly or with Abs(h) = hmin. ---- 
   */
 L540:
  C2F (xerrwv) ("lsodar-  at t (=r1) and step size h (=r2), the   ", &c__50,
		&c__205, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		49L);
  C2F (xerrwv) ("      corrector convergence failed repeatedly    ", &c__50,
		&c__205, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		49L);
  C2F (xerrwv) ("      or with Abs(h) = hmin   ", &c__30, &c__205, &c__1,
		&c__0, &c__0, &c__0, &c__2, &ls0001_1.tn, &ls0001_1.h__, 30L);
  *istate = -5;
  goto L560;
  /*rwork length too small to proceed. ----------------------------------- 
   */
 L550:
  C2F (xerrwv) ("lsodar-  at current t(=r1), rwork length too small", &c__50,
		&c__206, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv)
    ("      to proceed.  the integration was otherwise successful.", &c__60,
     &c__206, &c__1, &c__0, &c__0, &c__0, &c__1, &ls0001_1.tn, &c_b46, 60L);
  *istate = -7;
  goto L580;
  /*iwork length too small to proceed. ----------------------------------- 
   */
 L555:
  C2F (xerrwv) ("lsodar-  at current t(=r1), iwork length too small", &c__50,
		&c__207, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv)
    ("      to proceed.  the integration was otherwise successful.", &c__60,
     &c__207, &c__1, &c__0, &c__0, &c__0, &c__1, &ls0001_1.tn, &c_b46, 60L);
  *istate = -7;
  goto L580;
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
  rwork[15] = lsa001_1.tsw;
  iwork[11] = ls0001_1.nst;
  iwork[12] = ls0001_1.nfe;
  iwork[13] = ls0001_1.nje;
  iwork[14] = ls0001_1.nqu;
  iwork[15] = ls0001_1.nq;
  iwork[19] = lsa001_1.mused;
  iwork[20] = ls0001_1.meth;
  iwork[10] = lsr001_1.nge;
  lsr001_1.tlast = *t;
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
  C2F (xerrwv) ("lsodar-  istate (=i1) illegal ", &c__30, &c__1, &c__1, &c__1,
		istate, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L602:
  C2F (xerrwv) ("lsodar-  itask (=i1) illegal  ", &c__30, &c__2, &c__1, &c__1,
		itask, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L603:
  C2F (xerrwv) ("lsodar-  istate .gt. 1 but lsodar not initialized ", &c__50,
		&c__3, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  goto L700;
 L604:
  C2F (xerrwv) ("lsodar-  neq (=i1) .lt. 1     ", &c__30, &c__4, &c__1, &c__1,
		&neq[1], &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L605:
  C2F (xerrwv) ("lsodar-  istate = 3 and neq increased (i1 to i2)  ", &c__50,
		&c__5, &c__1, &c__2, &ls0001_1.n, &neq[1], &c__0, &c_b46,
		&c_b46, 50L);
  goto L700;
 L606:
  C2F (xerrwv) ("lsodar-  itol (=i1) illegal   ", &c__30, &c__6, &c__1, &c__1,
		itol, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L607:
  C2F (xerrwv) ("lsodar-  iopt (=i1) illegal   ", &c__30, &c__7, &c__1, &c__1,
		iopt, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L608:
  C2F (xerrwv) ("lsodar-  jt (=i1) illegal     ", &c__30, &c__8, &c__1, &c__1,
		jt, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L609:
  C2F (xerrwv) ("lsodar-  ml (=i1) illegal.. .lt.0 or .ge.neq (=i2)", &c__50,
		&c__9, &c__1, &c__2, &ml, &neq[1], &c__0, &c_b46, &c_b46,
		50L);
  goto L700;
 L610:
  C2F (xerrwv) ("lsodar-  mu (=i1) illegal.. .lt.0 or .ge.neq (=i2)", &c__50,
		&c__10, &c__1, &c__2, &mu, &neq[1], &c__0, &c_b46, &c_b46,
		50L);
  goto L700;
 L611:
  C2F (xerrwv) ("lsodar-  ixpr (=i1) illegal   ", &c__30, &c__11, &c__1,
		&c__1, &lsa001_1.ixpr, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L612:
  C2F (xerrwv) ("lsodar-  mxstep (=i1) .lt. 0  ", &c__30, &c__12, &c__1,
		&c__1, &ls0001_1.mxstep, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L613:
  C2F (xerrwv) ("lsodar-  mxhnil (=i1) .lt. 0  ", &c__30, &c__13, &c__1,
		&c__1, &ls0001_1.mxhnil, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L614:
  C2F (xerrwv) ("lsodar-  tout (=r1) behind t (=r2)      ", &c__40, &c__14,
		&c__1, &c__0, &c__0, &c__0, &c__2, tout, t, 40L);
  C2F (xerrwv) ("      integration direction is given by h0 (=r1)  ", &c__50,
		&c__14, &c__1, &c__0, &c__0, &c__0, &c__1, &h0, &c_b46, 50L);
  goto L700;
 L615:
  C2F (xerrwv) ("lsodar-  hmax (=r1) .lt. 0.0  ", &c__30, &c__15, &c__1,
		&c__0, &c__0, &c__0, &c__1, &hmax, &c_b46, 30L);
  goto L700;
 L616:
  C2F (xerrwv) ("lsodar-  hmin (=r1) .lt. 0.0  ", &c__30, &c__16, &c__1,
		&c__0, &c__0, &c__0, &c__1, &ls0001_1.hmin, &c_b46, 30L);
  goto L700;
 L617:
  C2F (xerrwv)
    ("lsodar-  rwork length needed, lenrw (=i1), exceeds lrw (=i2)", &c__60,
     &c__17, &c__1, &c__2, &lenrw, lrw, &c__0, &c_b46, &c_b46, 60L);
  goto L700;
 L618:
  C2F (xerrwv)
    ("lsodar-  iwork length needed, leniw (=i1), exceeds liw (=i2)", &c__60,
     &c__18, &c__1, &c__2, &leniw, liw, &c__0, &c_b46, &c_b46, 60L);
  goto L700;
 L619:
  C2F (xerrwv) ("lsodar-  rtol(i1) is r1 .lt. 0.0        ", &c__40, &c__19,
		&c__1, &c__1, &i__, &c__0, &c__1, &rtoli, &c_b46, 40L);
  goto L700;
 L620:
  C2F (xerrwv) ("lsodar-  atol(i1) is r1 .lt. 0.0        ", &c__40, &c__20,
		&c__1, &c__1, &i__, &c__0, &c__1, &atoli, &c_b46, 40L);
  goto L700;
 L621:
  ewti = rwork[ls0001_1.lewt + i__ - 1];
  C2F (xerrwv) ("lsodar-  ewt(i1) is r1 .le. 0.0         ", &c__40, &c__21,
		&c__1, &c__1, &i__, &c__0, &c__1, &ewti, &c_b46, 40L);
  goto L700;
 L622:
  C2F (xerrwv)
    ("lsodar-  tout (=r1) too close to t(=r2) to start integration", &c__60,
     &c__22, &c__1, &c__0, &c__0, &c__0, &c__2, tout, t, 60L);
  goto L700;
 L623:
  C2F (xerrwv)
    ("lsodar-  itask = i1 and tout (=r1) behind tcur - hu (= r2)  ", &c__60,
     &c__23, &c__1, &c__1, itask, &c__0, &c__2, tout, &tp, 60L);
  goto L700;
 L624:
  C2F (xerrwv)
    ("lsodar-  itask = 4 or 5 and tcrit (=r1) behind tcur (=r2)   ", &c__60,
     &c__24, &c__1, &c__0, &c__0, &c__0, &c__2, &tcrit, &ls0001_1.tn, 60L);
  goto L700;
 L625:
  C2F (xerrwv)
    ("lsodar-  itask = 4 or 5 and tcrit (=r1) behind tout (=r2)   ", &c__60,
     &c__25, &c__1, &c__0, &c__0, &c__0, &c__2, &tcrit, tout, 60L);
  goto L700;
 L626:
  C2F (xerrwv) ("lsodar-  at start of problem, too much accuracy   ", &c__50,
		&c__26, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv)
    ("      requested for precision of machine..  see tolsf (=r1) ", &c__60,
     &c__26, &c__1, &c__0, &c__0, &c__0, &c__1, &tolsf, &c_b46, 60L);
  rwork[14] = tolsf;
  goto L700;
 L627:
  C2F (xerrwv) ("lsodar-  trouble from intdy. itask = i1, tout = r1", &c__50,
		&c__27, &c__1, &c__1, itask, &c__0, &c__1, tout, &c_b46, 50L);
  goto L700;
 L628:
  C2F (xerrwv) ("lsodar-  mxordn (=i1) .lt. 0  ", &c__30, &c__28, &c__1,
		&c__1, &lsa001_1.mxordn, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L629:
  C2F (xerrwv) ("lsodar-  mxords (=i1) .lt. 0  ", &c__30, &c__29, &c__1,
		&c__1, &lsa001_1.mxords, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L630:
  C2F (xerrwv) ("lsodar-  ng (=i1) .lt. 0      ", &c__30, &c__30, &c__1,
		&c__1, ng, &c__0, &c__0, &c_b46, &c_b46, 30L);
  goto L700;
 L631:
  C2F (xerrwv) ("lsodar-  ng changed (from i1 to i2) illegally,    ", &c__50,
		&c__31, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv) ("      i.e. not immediately after a root was found ", &c__50,
		&c__31, &c__1, &c__2, &lsr001_1.ngc, ng, &c__0, &c_b46,
		&c_b46, 50L);
  goto L700;
 L632:
  C2F (xerrwv) ("lsodar-  one or more components of g has a root   ", &c__50,
		&c__32, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  C2F (xerrwv) ("      too near to the initial point     ", &c__40, &c__32,
		&c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46, 40L);
  /* 
   */
 L700:
  if (ls0001_1.illin == 5)
    {
      goto L710;
    }
  ++ls0001_1.illin;
  lsr001_1.tlast = *t;
  *istate = -3;
  return 0;
 L710:
  C2F (xerrwv) ("lsodar-  repeated occurrences of illegal input    ", &c__50,
		&c__302, &c__1, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  /* 
   */
 L800:
  C2F (xerrwv) ("lsodar-  run aborted.. apparent infinite loop     ", &c__50,
		&c__303, &c__2, &c__0, &c__0, &c__0, &c__0, &c_b46, &c_b46,
		50L);
  return 0;
  /*----------------------- end of subroutine lsodar ---------------------- 
   */
}				/* lsodar2_ */
