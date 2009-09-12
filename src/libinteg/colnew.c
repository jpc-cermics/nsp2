#include "nsp/math.h"
#include "nsp/sciio.h"
#include "colnew-n.h"

static int
nsp_colnew_subbak (double *w, int *nrow, int *ncol, int *last, double *x);

static int nsp_colnew_subfor (double *w, int *ipivot, int *nrow, int *last, double *x);

static int 
nsp_colnew_dmzsol (int *kd, int *mstar, int *n, double *v, double *z__,
		   double *dmz);

static int
nsp_colnew_shiftb (double *ai, int *nrowi, int *ncoli, int *last, double *ai1,
		   int *nrowi1, int *ncoli1);

static int 
nsp_colnew_factrb (double *w, int *ipivot, double *d__, int *nrow, int *ncol,
		   int *last, int *info);

static int 
nsp_colnew_sbblok (double *bloks, int *integs, int *nbloks, int *ipivot, double *x);

static int
nsp_colnew_fcblok (double *bloks, int *integs, int *nbloks, int *ipivot,
		   double *scrtch, int *info);

static int nsp_colnew_horder (int *i__, double *uhigh, double *hi, double *dmz,
			      int *ncomp, int *k);

static int
nsp_colnew_vmonde (double *rho, double *coef, int *k);

static int
nsp_colnew_rkbas (double *s, double *coef, int *k, int *m, double *rkb,
		  double *dm, int *mode);


static int
nsp_colnew_gblock (double *h__, double *gi, int *nrow, int *irow, double *wi,
		   double *vi, int *kd, double *rhsz, double *rhsdmz,
		   int *ipvtw, int *mode);

static int
nsp_colnew_gblock (double *h__, double *gi, int *nrow, int *irow, double *wi,
		   double *vi, int *kd, double *rhsz, double *rhsdmz,
		   int *ipvtw, int *mode);

static int
nsp_colnew_approx (int *i__, double *x, double *zval, double *a, double *coef,
		   double *xi, int *n, double *z__, double *dmz, int *k,
		   int *ncomp, int *mmax, int *m, int *mstar, int *mode,
		   double *dmval, int *modm);

static int
nsp_colnew_errchk (double *xi, double *z__, double *dmz, double *valstr,
		   int *ifin);

static int nsp_colnew_skale (int *n, int *mstar, int *kd, double *z__, double *xi,
			     double *scale, double *dscale);
static int nsp_colnew_newmsh (int *mode, double *xi, double *xiold, double *z__,
			      double *dmz, double *valstr, double *slope, double *accum,
			      int *nfxpnt, double *fixpnt);

static int nsp_colnew_consts (int *k, double *rho, double *coef);

static int 
nsp_colnew_vwblok (double *xcol, double *hrho, int *jj, double *wi,
		   double *vi, int *ipvtw, int *kd, double *zval,
		   double *df, double *acol, double *dmzo,
		   int *ncomp, DFsub dfsub, int *msing,void *args, int *colnew_err);

static int 
nsp_colnew_lsyslv (int *msing, double *xi, double *xiold, double *z__,
		   double *dmz, double *delz, double *deldmz, double *g,
		   double *w, double *v, double *rhs, double *dmzo,
		   int *integs, int *ipvtg, int *ipvtw, double *rnorm,
		   int *mode, Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub,
		   Guess guess, void *args, int *colnew_err);

static int 
nsp_colnew_gderiv (double *gi, int *nrow, int *irow, double *zval,
		   double *dgz, int *mode, DGsub dgsub, void *args, int *colnew_err);

static int
nsp_colnew_contrl (double *xi, double *xiold, double *z__, double *dmz,
		   double *rhs, double *delz, double *deldmz, double *dqz,
		   double *dqdmz, double *g, double *w, double *v,
		   double *valstr, double *slope, double *scale,
		   double *dscale, double *accum, int *ipvtg, int *integs,
		   int *ipvtw, int *nfxpnt, double *fixpnt, int *iflag,
		   Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub, Guess guess,
		   void *args, int *colnew_err);


struct
{
  double precis;
  int iout, iprint;
} colout_;

struct
{
  double rho[7], coef[49];
} colloc_;

struct
{
  int k, ncomp, mstar, kd, mmax, m[20];
} colord_;

struct
{
  int n, nold, nmax, nz, ndmz;
} colapr_;

struct
{
  int mshflg, mshnum, mshlmt, mshalt;
} colmsh_;

struct
{
  double zeta[40], aleft, aright;
  int izeta, idum;
} colsid_;

struct
{
  int nonlin, iter, limit, icare, iguess;
} colnln_;

struct
{
  double tol[40],wgtmsh[40],wgterr[40],tolin[40],root[40];
  int jtol[40], ltol[40], ntol;
} colest_1;


struct
{
  double b[28], acol[28*7] , asave[28*4];
} colbas_;


/* Table of constant values */

static int c__1 = 1;
static int c__0 = 0;
static int c__2 = 2;
static int c__3 = 3;
static int c__4 = 4;
static double c_b263 = 1.;
static double c_b267 = .16666666666666666;
static double c_b269 = .33333333333333331;
static double c_b271 = .66666666666666663;
static double c_b273 = .83333333333333337;

/*********************************************************************** 
 * this package solves boundary value problems for 
 * ordinary differential equations, as described below. 
 * 
 * COLNEW is a modification of the package COLSYS by ascher, 
 * christiansen and russell [1]. It incorporates a new basis 
 * representation replacing b-splines, and improvements for 
 * the linear and nonlinear algebraic equation solvers. 
 * the package can be referenced as either COLNEW or COLSYS. 
 *********************************************************************** 

 *---------------------------------------------------------------------- 
 *                           p a r t  1 
 *       main storage allocation and program control subroutines 
 *---------------------------------------------------------------------- 
 * 
 */

/* 
 * 
 *********************************************************************** 
 * 
 *    written by 
 *                 u. ascher, 
 *                           department of computer science, 
 *                           university of british columbia, 
 *                           vancouver, b. c., canada   v6t 1w5 
 *                 g. bader, 
 *                           institut f. angewandte mathematik 
 *                           university of heidelberg 
 *                           im neuenheimer feld 294 
 *                           d-6900 heidelberg 1 
 * 
 *********************************************************************** 
 * 
 *    purpose 
 * 
 *    this package solves a multi-point boundary value 
 *    problem for a mixed order system of ode-s given by 
 * 
 *         (m(i)) 
 *        u       =  f  ( x; z(u(x)) )      i = 1, ... ,ncomp 
 *         i          i 
 * 
 *                                         aleft .lt. x .lt. aright, 
 * 
 * 
 *        g  ( zeta(j); z(u(zeta(j))) ) = 0   j = 1, ... ,mstar 
 *         j 
 *                                   mstar = m(1)+m(2)+...+m(ncomp), 
 * 
 * 
 *        where                          t 
 *              u = (u , u , ... ,u     )  is the exact solution vector 
 *                    1   2        ncomp 
 * 
 *               (mi) 
 *              u     is the mi=m(i) th  derivative of u 
 *               i                                      i 
 * 
 *                                 (1)        (m1-1)       (mncomp-1) 
 *              z(u(x)) = ( u (x),u  (x),...,u    (x),...,u      (x) ) 
 *                           1     1          1            ncomp 
 * 
 *               f (x,z(u))   is a (generally) nonlinear function of 
 *                i 
 *                            z(u)=z(u(x)). 
 * 
 *               g (zeta(j);z(u))  is a (generally) nonlinear function 
 *                j 
 *                              used to represent a boundary condition. 
 * 
 *        the boundary points satisfy 
 *              aleft .le. zeta(1) .le. .. .le. zeta(mstar) .le. aright 
 * 
 *        the orders mi of the differential equations satisfy 
 *                           1 .le. m(i) .le. 4. 
 * 
 * 
 *********************************************************************** 
 * 
 *    method 
 * 
 *       the method used to approximate the solution u is 
 *    collocation at gaussian points, requiring m(i)-1 continuous 
 *    derivatives in the i-th component, i = 1, ..., ncomp. 
 *    here, k is the number of collocation points (stages) per 
 *    subinterval and is chosen such that k .ge. max m(i). 
 *    a runge-kutta-monomial solution representation is utilized. 
 * 
 *    references 
 * 
 *    [1] u. ascher, j. christiansen and r.d. russell, 
 *        collocation software for boundary-value odes, 
 *        acm trans. math software 7 (1981), 209-222. 
 *        this paper contains EXAMPLES where use of the code 
 *        is demonstrated. 
 * 
 *    [2] g. bader and u. ascher, 
 *        a new basis implementation for a mixed order 
 *        boundary value ode solver, 
 *        siam j. scient. stat. comput. (1987). 
 * 
 *    [3] u. ascher, j. christiansen and r.d. russell, 
 *        a collocation solver for mixed order 
 *        systems of boundary value problems, 
 *        math. comp. 33 (1979), 659-679. 
 * 
 *    [4] u. ascher, j. christiansen and r.d. russell, 
 *        colsys - a collocation code for boundary 
 *        value problems, 
 *        lecture notes comp.sc. 76, springer verlag, 
 *        b. childs et. al. (eds.) (1979), 164-185. 
 * 
 *    [5] c. deboor and r. weiss, 
 *        solveblok: a package for solving almost block diagonal 
 *        linear systems, 
 *        acm trans. math. software 6 (1980), 80-87. 
 * 
 *********************************************************************** 
 * 
 *    ***************     input to colnew     *************** 
 * 
 *    variables 
 * 
 *    ncomp - no. of differential equations   (ncomp .le. 20) 
 * 
 *    m(j) - order of the j-th differential equation 
 *           ( mstar = m(1) + ... + m(ncomp) .le. 40 ) 
 * 
 *    aleft - left end of interval 
 * 
 *    aright - right end of interval 
 * 
 *    zeta(j) - j-th side condition point (boundary point). must 
 *              have  zeta(j) .le. zeta(j+1). all side condition 
 *              points must be mesh points in all meshes used, 
 *              see description of ipar(11) and fixpnt below. 
 * 
 *    ipar - an int array dimensioned at least 11. 
 *           a list of the parameters in ipar and their meaning follows 
 *           some parameters are renamed in colnew; these new names are 
 *           given in parentheses. 
 * 
 *    ipar(1)     ( = nonlin ) 
 *            = 0 if the problem is linear 
 *            = 1 if the problem is nonlinear 
 * 
 *    ipar(2) = no. of collocation points per subinterval  (= k ) 
 *              where max m(i) .le.  k .le. 7 . if ipar(2)=0 then 
 *              colnew sets  k = Max( max m(i)+1, 5-max m(i) ) 
 * 
 *    ipar(3) = no. of subintervals in the initial mesh  ( = n ). 
 *              if ipar(3) = 0 then colnew arbitrarily sets n = 5. 
 * 
 *    ipar(4) = no. of solution and derivative tolerances.  ( = ntol ) 
 *              we require  0 .lt. ntol .le. mstar. 
 * 
 *    ipar(5) = dimension of fspace.     ( = ndimf ) 
 * 
 *    ipar(6) = dimension of ispace.     ( = ndimi ) 
 * 
 *    ipar(7) -  output control ( = iprint ) 
 *             = -1 for full diagnostic printout 
 *             = 0 for selected printout 
 *             = 1 for no printout 
 * 
 *    ipar(8)     ( = iread ) 
 *            = 0 causes colnew to generate a uniform initial mesh. 
 *            = 1 if the initial mesh is provided by the user.  it 
 *                is defined in fspace as follows:  the mesh 
 *                aleft=x(1).lt.x(2).lt. ... .lt.x(n).lt.x(n+1)=aright 
 *                will occupy  fspace(1), ..., fspace(n+1). the 
 *                user needs to supply only the interior mesh 
 *                points  fspace(j) = x(j), j = 2, ..., n. 
 *            = 2 if the initial mesh is supplied by the user 
 *                as with ipar(8)=1, and in addition no adaptive 
 *                mesh selection is to be done. 
 * 
 *    ipar(9)     ( = iguess ) 
 *            = 0 if no initial guess for the solution is 
 *                provided. 
 *            = 1 if an initial guess is provided by the user 
 *                in subroutine  guess. 
 *            = 2 if an initial mesh and approximate solution 
 *                coefficients are provided by the user in  fspace. 
 *                (the former and new mesh are the same). 
 *            = 3 if a former mesh and approximate solution 
 *                coefficients are provided by the user in fspace, 
 *                and the new mesh is to be taken twice as coarse; 
 *                i.e.,every second point from the former mesh. 
 *            = 4 if in addition to a former initial mesh and 
 *                approximate solution coefficients, a new mesh 
 *                is provided in fspace as well. 
 *                (see description of output for further details 
 *                on iguess = 2, 3, and 4.) 
 * 
 *    ipar(10)= 0 if the problem is regular 
 *            = 1 if the first relax factor is =rstart, and the 
 *                nonlinear iteration does not rely on past covergence 
 *                (use for an extra sensitive nonlinear problem only). 
 *            = 2 if we are to return immediately upon  (a) two 
 *                successive nonconvergences, or  (b) after obtaining 
 *                error estimate for the first time. 
 * 
 *    ipar(11)= no. of fixed points in the mesh other than aleft 
 *              and aright. ( = nfxpnt , the dimension of fixpnt) 
 *              the code requires that all side condition points 
 *              other than aleft and aright (see description of 
 *              zeta ) be included as fixed points in fixpnt. 
 * 
 *    ltol  -  an array of dimension ipar(4). ltol(j) = l  specifies 
 *             that the j-th tolerance in  tol  controls the error 
 *             in the l-th component of z(u).   also require that 
 *             1.le.ltol(1).lt.ltol(2).lt. ... .lt.ltol(ntol).le.mstar 
 * 
 *    tol    - an array of dimension ipar(4). tol(j) is the 
 *             error tolerance on the ltol(j) -th component 
 *             of z(u). thus, the code attempts to satisfy 
 *             for j=1,...,ntol  on each subinterval 
 *             Abs(z(v)-z(u))       .le. tol(j)*abs(z(u))       +tol(j) 
 *                           ltol(j)                     ltol(j) 
 * 
 *             if v(x) is the approximate solution vector. 
 * 
 *    fixpnt - an array of dimension ipar(11).   it contains 
 *             the points, other than aleft and aright, which 
 *             are to be included in every mesh. 
 * 
 *    ispace - an int work array of dimension ipar(6). 
 *             its size provides a constraint on nmax, 
 *             the maximum number of subintervals. choose 
 *             ipar(6) according to the formula 
 *                     ipar(6)  .ge.  nmax*nsizei 
 *               where 
 *                     nsizei = 3 + kdm 
 *               with 
 *                     kdm = kd + mstar  ;  kd = k * ncomp ; 
 *                     nrec = no. of right end boundary conditions. 
 * 
 * 
 *    fspace - a real work array of dimension ipar(5). 
 *             its size provides a constraint on nmax. 
 *             choose ipar(5) according to the formula 
 *                     ipar(5)  .ge.  nmax*nsizef 
 *               where 
 *                     nsizef = 4 + 3 * mstar + (5+kd) * kdm + 
 *                             (2*mstar-nrec) * 2*mstar. 
 * 
 * 
 *    iflag - the mode of return from colnew. 
 *          = 1 for normal return 
 *          = 0 if the collocation matrix is singular. 
 *          =-1 if the expected no. of subintervals exceeds storage 
 *              specifications. 
 *          =-2 if the nonlinear iteration has not converged. 
 *          =-3 if there is an input data error. 
 * 
 * 
 *********************************************************************** 
 * 
 *    *************    user supplied subroutines   ************* 
 * 
 * 
 *    the following subroutines must be declared external in the 
 *    main program which calls colnew. 
 * 
 * 
 *    fsub  - name of subroutine for evaluating f(x,z(u(x))) = 
 *                           t 
 *            (f ,...,f     )  at a point x in (aleft,aright).  it 
 *              1      ncomp 
 *            should have the heading 
 * 
 *                      subroutine fsub (x , z , f) 
 * 
 *            where f is the vector containing the value of fi(x,z(u)) 
 *            in the i-th component and                            t 
 *                                      z(u(x))=(z(1),...,z(mstar)) 
 *            is defined as above under  purpose . 
 * 
 * 
 *    dfsub - name of subroutine for evaluating the jacobian of 
 *            f(x,z(u)) at a point x.  it should have the heading 
 * 
 *                      subroutine dfsub (x , z , df) 
 * 
 *            where z(u(x)) is defined as for fsub and the (ncomp) by 
 *            (mstar) array df should be filled by the partial deriv- 
 *            atives of f, viz, for a particular call one calculates 
 *                               df(i,j) = dfi / dzj, i=1,...,ncomp 
 *                                                    j=1,...,mstar. 
 * 
 * 
 *    gsub  - name of subroutine for evaluating the i-th component of 
 *            g(x,z(u(x))) = g (zeta(i),z(u(zeta(i)))) at a point x = 
 *                            i 
 *            zeta(i) where 1.le.i.le.mstar. it should have the heading 
 * 
 *                      subroutine gsub (i , z , g) 
 * 
 *            where z(u) is as for fsub, and i and g=g  are as above. 
 *                                                    i 
 *            note that in contrast to f in  fsub , here 
 *            only one value per call is returned in g. 
 * 
 * 
 *    dgsub - name of subroutine for evaluating the i-th row of 
 *            the jacobian of g(x,u(x)).  it should have the heading 
 * 
 *                      subroutine dgsub (i , z , dg) 
 * 
 *            where z(u) is as for fsub, i as for gsub and the mstar- 
 *            vector dg should be filled with the partial derivatives 
 *            of g, viz, for a particular call one calculates 
 *                  dg(i,j) = dgi / dzj      j=1,...,mstar. 
 * 
 * 
 *    guess - name of subroutine to evaluate the initial 
 *            approximation for  z(u(x)) and for dmval(u(x))= vector 
 *            of the mj-th derivatives of u(x). it should have the 
 *            heading 
 * 
 *                      subroutine guess (x , z , dmval) 
 * 
 *            note that this subroutine is needed only if using 
 *            ipar(9) = 1, and then all  mstar  components of z 
 *            and  ncomp  components of  dmval  should be specified 
 *            for any x,  aleft .le. x .le. aright . 
 * 
 * 
 *********************************************************************** 
 * 
 *    ************   use of output from colnew   ************ 
 * 
 *                ***   solution evaluation   *** 
 * 
 *    on return from colnew, the arrays fspace and ispace 
 *    contain information specifying the approximate solution. 
 *    the user can produce the solution vector  z( u(x) )  at 
 *    any point x, aleft .le. x .le. aright, by the statement, 
 * 
 *          call appsln (x, z, fspace, ispace) 
 * 
 *    when saving the coefficients for later reference, only 
 *    ispace(1),...,ispace(7+ncomp)    and 
 *    fspace(1),...,fspace(ispace(7))    need to be saved as 
 *    these are the quantities used by appsln. 
 * 
 * 
 *                ***   simple continuation   *** 
 * 
 * 
 *    a formerly obtained solution can easily be used as the 
 *    first approximation for the nonlinear iteration for a 
 *    new problem by setting   (iguess =) ipar(9) = 2, 3 or 4. 
 * 
 *    if the former solution has just been obtained then the 
 *    values needed to define the first approximation are 
 *    already in ispace and fspace. 
 *    alternatively, if the former solution was obtained in a 
 *    previous run and its coefficients were saved then those 
 *    coefficients must be put back into 
 *    ispace(1),..., ispace(7+ncomp)    and 
 *    fspace(1),..., fspace(ispace(7)). 
 * 
 *    for ipar(9) = 2 or 3 set ipar(3) = ispace(1) ( = the 
 *    size of the previous mesh ). 
 * 
 *    for ipar(9) = 4 the user specifies a new mesh of n subintervals 
 *    as follows. 
 *    the values in  fspace(1),...,fspace(ispace(7))  have to be 
 *    shifted by n+1 locations to  fspace(n+2),..,fspace(ispace(7)+n+1) 
 *    and the new mesh is then specified in fspace(1),..., fspace(n+1). 
 *    also set ipar(3) = n. 
 * 
 * 
 *********************************************************************** 
 * 
 *    ***************      package subroutines      *************** 
 * 
 *    the following description gives a brief overview of how the 
 *    procedure is broken down into the subroutines which make up 
 *    the package called  colnew . for further details the 
 *    user should refer to documentation in the various subroutines 
 *    and to the references cited above. 
 * 
 *    the subroutines fall into four groups: 
 * 
 *part 1 - the main storage allocation and program control subr 
 * 
 *    colnew - tests input values, does initialization and breaks up 
 *             the work areas, fspace and ispace, into the arrays 
 *             used by the program. 
 *    colsys - another name for colnew 
 * 
 *    contrl - is the actual driver of the package. this routine 
 *             contains the strategy for nonlinear equation solving. 
 * 
 *    skale  - provides scaling for the control 
 *             of convergence in the nonlinear iteration. 
 * 
 * 
 *part 2 - mesh selection and error estimation subroutines 
 * 
 *    consts - is called once by  colnew  to initialize constants 
 *             which are used for error estimation and mesh selection. 
 * 
 *    newmsh - generates meshes. it contains the test to decide 
 *             whether or not to redistribute a mesh. 
 * 
 *    errchk - produces error estimates and checks against the 
 *             tolerances at each subinterval 
 * 
 * 
 *part 3 - collocation system set-up subroutines 
 * 
 *    lsyslv - controls the set-up and solution of the linear 
 *             algebraic systems of collocation equations which 
 *             arise at each newton iteration. 
 * 
 *    gderiv - is used by lsyslv to set up the equation associated 
 *             with a side condition point. 
 * 
 *    vwblok - is used by lsyslv to set up the equation(s) associated 
 *             with a collocation point. 
 * 
 *    gblock - is used by lsyslv to construct a block of the global 
 *             collocation matrix or the corresponding right hand 
 *             side. 
 * 
 * 
 *part 4 - service subroutines 
 * 
 *    appsln - sets up a standard call to  approx . 
 * 
 *    approx - evaluates a piecewise polynomial solution. 
 * 
 *    rkbas  - evaluates the mesh independent runge-kutta basis 
 * 
 *    vmonde - solves a vandermonde system for given right hand 
 *             side 
 * 
 *    horder - evaluates the highest order derivatives of the 
 *             current collocation solution used for mesh refinement. 
 * 
 * 
 *part 5 - linear algebra  subroutines 
 * 
 *    to solve the global linear systems of collocation equations 
 *    constructed in part 3,  colnew  uses a column oriented version 
 *    of the package  solveblok originally due to de boor and weiss. 
 * 
 *    to solve the linear systems for static parameter condensation 
 *    in each block of the collocation equations, the linpack 
 *    routines  dgefa and  dgesl  are included. but these 
 *    may be replaced when solving problems on vector processors 
 *    or when solving large scale sparse jacobian problems. 
 * 
 *---------------------------------------------------------------------- 
 *     IMPLICIT REAL*8 (A-H,O-Z) 
 * 
 * 
 * 
 *    this subroutine can be called either COLNEW or COLSYS 
 * 
 * 
 ********************************************************************** 
 * 
 *    the actual subroutine colnew serves as an interface with 
 *    the package of subroutines referred to collectively as 
 *    colnew. the subroutine serves to test some of the input 
 *    parameters, rename some of the parameters (to make under- 
 *    standing of the coding easier), to do some initialization, 
 *    and to break the work areas fspace and ispace up into the 
 *    arrays needed by the program. 
 * 
 *********************************************************************** 
 * 
 *...  specify machine dependent output unit  iout  and compute machine 
 *...  dependent constant  precis = 100 * machine unit roundoff 
 * 
 */

int colnew_nsp_colnew_0 (int n__, int *ncomp, int *m, double *aleft,
			 double *aright, double *zeta, int *ipar, int *ltol,
			 double *tol, double *fixpnt, int *ispace, double *fspace,
			 int *iflag, Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub,
			 Guess guess,void *args, int * colnew_err)
{
  /* System generated locals */
  int i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int nrec, lscl, ldmz, idmz, ldqz, lrhs, i__, iread, ndimf, ndimi, ldscl,
    nmaxf, nfixf, ldelz, nfixi, nmaxi;
  double dummy[1];
  int lpvtg, k2, lpvtw;
  double precp1;
  int ib, ic, lg, ip, lw, lv, lz, laccum, ldeldz, linteg, lxiold, ldqdmz,
    nsizef, lslope, nsizei;
  int lvalst;
  int nfxpnt;
  int np1, lxi;

  /* Fortran I/O blocks */
  /* Parameter adjustments */
  --m;
  --zeta;
  --ipar;
  --ltol;
  --tol;
  --fixpnt;
  --ispace;
  --fspace;

  /* Function Body */
  switch (n__)
    {
    case 1:
      goto L_colsys;
    }


 L_colsys:
  if (ipar[7] <= 0)
    {
      Sciprintf("Version colnew of colsys");
    }
  /* 
   */
  colout_.iout = 6;
  colout_.precis = 1.;
 L10:
  colout_.precis /= 2.;
  precp1 = colout_.precis + 1.;
  if (precp1 > 1.)
    {
      goto L10;
    }
  colout_.precis *= 100.;
  /* 
   *...  in case incorrect input data is detected, the program returns 
   *...  immediately with iflag=-3. 
   * 
   */
  *iflag = -3;
  if (*ncomp < 1 || *ncomp > 20)
    {
      return 0;
    }
  i__1 = *ncomp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (m[i__] < 1 || m[i__] > 4)
	{
	  return 0;
	}
      /* L20: */
    }
  /* 
   *...  rename some of the parameters and set default values. 
   * 
   */
  colnln_.nonlin = ipar[1];
  colord_.k = ipar[2];
  colapr_.n = ipar[3];
  if (colapr_.n == 0)
    {
      colapr_.n = 5;
    }
  iread = ipar[8];
  colnln_.iguess = ipar[9];
  if (colnln_.nonlin == 0 && colnln_.iguess == 1)
    {
      colnln_.iguess = 0;
    }
  if (colnln_.iguess >= 2 && iread == 0)
    {
      iread = 1;
    }
  colnln_.icare = ipar[10];
  colest_1.ntol = ipar[4];
  ndimf = ipar[5];
  ndimi = ipar[6];
  nfxpnt = ipar[11];
  colout_.iprint = ipar[7];
  colord_.mstar = 0;
  colord_.mmax = 0;
  i__1 = *ncomp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*Computing MAX 
       */
      i__2 = colord_.mmax, i__3 = m[i__];
      colord_.mmax = Max (i__2, i__3);
      colord_.mstar += m[i__];
      colord_.m[i__ - 1] = m[i__];
      /* L30: */
    }
  if (colord_.k == 0)
    {
      /*Computing MAX 
       */
      i__1 = colord_.mmax + 1, i__2 = 5 - colord_.mmax;
      colord_.k = Max (i__1, i__2);
    }
  i__1 = colord_.mstar;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L40: */
      colsid_.zeta[i__ - 1] = zeta[i__];
    }
  i__1 = colest_1.ntol;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      colest_1.ltol[i__ - 1] = ltol[i__];
      /* L50: */
      colest_1.tolin[i__ - 1] = tol[i__];
    }
  colsid_.aleft = *aleft;
  colsid_.aright = *aright;
  colord_.ncomp = *ncomp;
  colord_.kd = colord_.k * *ncomp;
  /* 
   *...  print the input data for checking. 
   * 
   */
  if (colout_.iprint > -1)
    {
      goto L80;
    }
  if (colnln_.nonlin > 0)
    {
      goto L60;
    }
  Sciprintf("number of (linear) diff eqns is %d\n",*ncomp);
  Sciprintf("their orders are ");
  i__1 = *ncomp;
  for (ip = 1; ip <= i__1; ++ip)
    {
      Sciprintf("%d ",m[ip]);
    }
  Sciprintf("\n");
  goto L70;
 L60:
  Sciprintf("number of (nonlinear) diff eqns is %d\n",*ncomp);
  Sciprintf("their orders are ");
  i__1 = *ncomp;
  for (ip = 1; ip <= i__1; ++ip)
    {
      Sciprintf("%d ",m[ip]);
    }
  Sciprintf("\n");
 L70:
  Sciprintf("side condition points zeta,\n");
  i__1 = colord_.mstar;
  for (ip = 1; ip <= i__1; ++ip)
    {
      Sciprintf("%10.6f ",zeta[ip]);
    }
  Sciprintf("\n");
  if (nfxpnt > 0)
    {
      Sciprintf("there are %d fixed points in the mesh\n",nfxpnt);
      i__1 = nfxpnt;
      for (ip = 1; ip <= i__1; ++ip)
	{
	  Sciprintf("%10.6f ",fixpnt[ip]);
	}
      Sciprintf("\n");
    }
  Sciprintf("NUMBER OF COLLOC PTS PER INTERVAL IS %d\n",colord_.k);
  Sciprintf("COMPONENTS OF Z REQUIRING TOLERANCES\n");
  i__1 = colest_1.ntol;
  for (ip = 1; ip <= i__1; ++ip)
    {
      Sciprintf("%10.6f ",ltol[ip]);
    }
  Sciprintf("\n");
  Sciprintf("CORRESPONDING ERROR TOLERANCES \n");
  i__1 = colest_1.ntol;
  for (ip = 1; ip <= i__1; ++ip)
    {
      Sciprintf("%10.6f ", tol[ip]);
    }
  Sciprintf("\n");
  if (colnln_.iguess >= 2)
    {
      Sciprintf("INITIAL MESH(ES) AND Z,DMZ PROVIDED BY USER\n");
    }
  if (iread == 2)
    {
      Sciprintf("NO ADAPTIVE MESH SELECTION\n");
    }
 L80:
  /* 
   *...  check for correctness of data 
   * 
   */
  if (colord_.k < 0 || colord_.k > 7)
    {
      return 0;
    }
  if (colapr_.n < 0)
    {
      return 0;
    }
  if (iread < 0 || iread > 2)
    {
      return 0;
    }
  if (colnln_.iguess < 0 || colnln_.iguess > 4)
    {
      return 0;
    }
  if (colnln_.icare < 0 || colnln_.icare > 2)
    {
      return 0;
    }
  if (colest_1.ntol < 0 || colest_1.ntol > colord_.mstar)
    {
      return 0;
    }
  if (nfxpnt < 0)
    {
      return 0;
    }
  if (colout_.iprint < -1 || colout_.iprint > 1)
    {
      return 0;
    }
  if (colord_.mstar < 0 || colord_.mstar > 40)
    {
      return 0;
    }
  ip = 1;
  i__1 = colord_.mstar;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = zeta[i__] - *aleft, Abs (d__1)) < colout_.precis
	  || (d__2 = zeta[i__] - *aright, Abs (d__2)) < colout_.precis)
	{
	  goto L100;
	}
    L90:
      if (ip > nfxpnt)
	{
	  return 0;
	}
      if (zeta[i__] - colout_.precis < fixpnt[ip])
	{
	  goto L95;
	}
      ++ip;
      goto L90;
    L95:
      if (zeta[i__] + colout_.precis < fixpnt[ip])
	{
	  return 0;
	}
    L100:
      ;
    }
  /* 
   *...  set limits on iterations and initialize counters. 
   *...  limit = maximum number of newton iterations per mesh. 
   *...  see subroutine  newmsh  for the roles of  mshlmt , mshflg , 
   *...  mshnum , and  mshalt . 
   * 
   */
  colmsh_.mshlmt = 3;
  colmsh_.mshflg = 0;
  colmsh_.mshnum = 1;
  colmsh_.mshalt = 1;
  colnln_.limit = 40;
  /* 
   *...  compute the maxium possible n for the given sizes of 
   *...  ispace  and  fspace. 
   * 
   */
  nrec = 0;
  i__1 = colord_.mstar;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ib = colord_.mstar + 1 - i__;
      if (zeta[ib] >= *aright)
	{
	  nrec = i__;
	}
      /* L110: */
    }
  nfixi = colord_.mstar;
  nsizei = colord_.kd + 3 + colord_.mstar;
  nfixf = nrec * (colord_.mstar << 1) + colord_.mstar * 5 + 3;
  nsizef =
    colord_.mstar * 3 + 4 + (colord_.kd + 5) * (colord_.kd +
						colord_.mstar) +
    (((colord_.mstar << 1) - nrec) << 1) * colord_.mstar;
  nmaxf = (ndimf - nfixf) / nsizef;
  nmaxi = (ndimi - nfixi) / nsizei;
  if (colout_.iprint < 1)
    {
      Sciprintf("THE MAXIMUM NUMBER OF SUBINTERVALS IS MIN (%d",nmaxf);
      Sciprintf("(ALLOWED FROM FSPACE),%d (ALLOWED FROM ISPACE)\n",nmaxi);
    }
  colapr_.nmax = Min (nmaxf, nmaxi);
  if (colapr_.nmax < colapr_.n)
    {
      return 0;
    }
  if (colapr_.nmax < nfxpnt + 1)
    {
      return 0;
    }
  if (colapr_.nmax < (nfxpnt << 1) + 2 && colout_.iprint < 1)
    {
      Sciprintf("INSUFFICIENT SPACE TO DOUBLE MESH FOR ERROR ESTIMATE\n");
    }
  /* 
   *...  generate pointers to break up  fspace  and  ispace . 
   * 
   */
  lxi = 1;
  lg = lxi + colapr_.nmax + 1;
  lxiold =
    lg +
    (colord_.mstar << 1) * (colapr_.nmax * ((colord_.mstar << 1) - nrec) +
			    nrec);
  lw = lxiold + colapr_.nmax + 1;
  /*Computing 2nd power 
   */
  i__1 = colord_.kd;
  lv = lw + i__1 * i__1 * colapr_.nmax;
  lz = lv + colord_.mstar * colord_.kd * colapr_.nmax;
  ldmz = lz + colord_.mstar * (colapr_.nmax + 1);
  ldelz = ldmz + colord_.kd * colapr_.nmax;
  ldeldz = ldelz + colord_.mstar * (colapr_.nmax + 1);
  ldqz = ldeldz + colord_.kd * colapr_.nmax;
  ldqdmz = ldqz + colord_.mstar * (colapr_.nmax + 1);
  lrhs = ldqdmz + colord_.kd * colapr_.nmax;
  lvalst = lrhs + colord_.kd * colapr_.nmax + colord_.mstar;
  lslope = lvalst + (colord_.mstar << 2) * colapr_.nmax;
  laccum = lslope + colapr_.nmax;
  lscl = laccum + colapr_.nmax + 1;
  ldscl = lscl + colord_.mstar * (colapr_.nmax + 1);
  lpvtg = 1;
  lpvtw = lpvtg + colord_.mstar * (colapr_.nmax + 1);
  linteg = lpvtw + colord_.kd * colapr_.nmax;
  /* 
   *...  if  iguess .ge. 2, move  xiold, z, and  dmz  to their proper 
   *...  locations in  fspace. 
   * 
   */
  if (colnln_.iguess < 2)
    {
      goto L160;
    }
  colapr_.nold = colapr_.n;
  if (colnln_.iguess == 4)
    {
      colapr_.nold = ispace[1];
    }
  colapr_.nz = colord_.mstar * (colapr_.nold + 1);
  colapr_.ndmz = colord_.kd * colapr_.nold;
  np1 = colapr_.n + 1;
  if (colnln_.iguess == 4)
    {
      np1 = np1 + colapr_.nold + 1;
    }
  i__1 = colapr_.nz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L120: */
      fspace[lz + i__ - 1] = fspace[np1 + i__];
    }
  idmz = np1 + colapr_.nz;
  i__1 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L125: */
      fspace[ldmz + i__ - 1] = fspace[idmz + i__];
    }
  np1 = colapr_.nold + 1;
  if (colnln_.iguess == 4)
    {
      goto L140;
    }
  i__1 = np1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L130: */
      fspace[lxiold + i__ - 1] = fspace[lxi + i__ - 1];
    }
  goto L160;
 L140:
  i__1 = np1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L150: */
      fspace[lxiold + i__ - 1] = fspace[colapr_.n + 1 + i__];
    }
 L160:
  /* 
   *...  initialize collocation points, constants, mesh. 
   * 
   */
  nsp_colnew_consts (&colord_.k, colloc_.rho, colloc_.coef);
  i__1 = iread + 3;
  nsp_colnew_newmsh (&i__1, &fspace[lxi], &fspace[lxiold], dummy, dummy,
		     dummy, dummy, dummy, &nfxpnt, &fixpnt[1]);
  /* 
   *...  determine first approximation, if the problem is nonlinear. 
   * 
   */
  if (colnln_.iguess >= 2)
    {
      goto L230;
    }
  np1 = colapr_.n + 1;
  i__1 = np1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L210: */
      fspace[i__ + lxiold - 1] = fspace[i__ + lxi - 1];
    }
  colapr_.nold = colapr_.n;
  if (colnln_.nonlin == 0 || colnln_.iguess == 1)
    {
      goto L230;
    }
  /* 
   *...  system provides first approximation of the solution. 
   *...  choose z(j) = 0  for j=1,...,mstar. 
   * 
   */
  i__1 = colapr_.nz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L220: */
      fspace[lz - 1 + i__] = 0.;
    }
  i__1 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L225: */
      fspace[ldmz - 1 + i__] = 0.;
    }
 L230:
  if (colnln_.iguess >= 2)
    {
      colnln_.iguess = 0;
    }
  nsp_colnew_contrl (&fspace[lxi], &fspace[lxiold], &fspace[lz],
		     &fspace[ldmz], &fspace[lrhs], &fspace[ldelz],
		     &fspace[ldeldz], &fspace[ldqz], &fspace[ldqdmz],
		     &fspace[lg], &fspace[lw], &fspace[lv], &fspace[lvalst],
		     &fspace[lslope], &fspace[lscl], &fspace[ldscl],
		     &fspace[laccum], &ispace[lpvtg], &ispace[linteg],
		     &ispace[lpvtw], &nfxpnt, &fixpnt[1], iflag, fsub,
		     dfsub, gsub, dgsub, guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...  prepare output 
   * 
   */
  ispace[1] = colapr_.n;
  ispace[2] = colord_.k;
  ispace[3] = *ncomp;
  ispace[4] = colord_.mstar;
  ispace[5] = colord_.mmax;
  ispace[6] = colapr_.nz + colapr_.ndmz + colapr_.n + 2;
  k2 = colord_.k * colord_.k;
  ispace[7] = ispace[6] + k2 - 1;
  i__1 = *ncomp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L240: */
      ispace[i__ + 7] = m[i__];
    }
  i__1 = colapr_.nz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L250: */
      fspace[colapr_.n + 1 + i__] = fspace[lz - 1 + i__];
    }
  idmz = colapr_.n + 1 + colapr_.nz;
  i__1 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L255: */
      fspace[idmz + i__] = fspace[ldmz - 1 + i__];
    }
  ic = idmz + colapr_.ndmz;
  i__1 = k2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L258: */
      fspace[ic + i__] = colloc_.coef[i__ - 1];
    }
  return 0;
}

int
nsp_colnew_colnew (int *ncomp, int *m, double *aleft, double *aright,
		   double *zeta, int *ipar, int *ltol, double *tol,
		   double *fixpnt, int *ispace, double *fspace, int *iflag,
		   Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub, Guess guess,
		   void *args, int *colnew_err)
{
  return colnew_nsp_colnew_0 (0, ncomp, m, aleft, aright, zeta, ipar, ltol,
			      tol, fixpnt, ispace, fspace, iflag, fsub, dfsub,
			      gsub, dgsub, guess,args, colnew_err);
}

int
nsp_colnew_colsys (int *ncomp, int *m, double *aleft, double *aright,
		   double *zeta, int *ipar, int *ltol, double *tol,
		   double *fixpnt, int *ispace, double *fspace, int *iflag,
		   Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub, Guess guess,
		   void *args, int *colnew_err)
{
  return colnew_nsp_colnew_0 (1, ncomp, m, aleft, aright, zeta, ipar, ltol,
			      tol, fixpnt, ispace, fspace, iflag, fsub, dfsub,
			      gsub, dgsub, guess,args, colnew_err);
}

/* 
 * 
 *  purpose 
 *    this subroutine is the actual driver.  the nonlinear iteration 
 *    strategy is controlled here ( see [4] ). upon convergence, errchk 
 *    is called to test for satisfaction of the requested tolerances. 
 * 
 *  variables 
 * 
 *    check  - maximum tolerance value, used as part of criteria for 
 *             checking for nonlinear iteration convergence 
 *    relax  - the relaxation factor for damped newton iteration 
 *    relmin - minimum allowable value for relax  (otherwise the 
 *             jacobian is considered singular). 
 *    rlxold - previous relax 
 *    rstart - initial value for relax when problem is sensitive 
 *    ifrz   - number of fixed jacobian iterations 
 *    lmtfrz - maximum value for ifrz before performing a reinversion 
 *    iter   - number of iterations (counted only when jacobian 
 *             reinversions are performed). 
 *    xi     - current mesh 
 *    xiold  - previous mesh 
 *    ipred  = 0  if relax is determined by a correction 
 *           = 1  if relax is determined by a prediction 
 *    ifreez = 0  if the jacobian is to be updated 
 *           = 1  if the jacobian is currently fixed (frozen) 
 *    iconv  = 0  if no previous convergence has been obtained 
 *           = 1  if convergence on a previous mesh has been obtained 
 *    icare  =-1  no convergence occurred (used for regular problems) 
 *           = 0  a regular problem 
 *           = 1  a sensitive problem 
 *           = 2  used for continuation (see description of ipar(10) 
 *                in colnew). 
 *    rnorm  - norm of rhs (right hand side) for current iteration 
 *    rnold  - norm of rhs for previous iteration 
 *    anscl  - scaled norm of newton correction 
 *    anfix  - scaled norm of newton correction at next step 
 *    anorm  - scaled norm of a correction obtained with jacobian fixed 
 *    nz     - number of components of  z  (see subroutine approx) 
 *    ndmz   - number of components of  dmz  (see subroutine approx) 
 *    imesh  - a control variable for subroutines newmsh and errchk 
 *           = 1  the current mesh resulted from mesh selection 
 *                or is the initial mesh. 
 *           = 2  the current mesh resulted from doubling the 
 *                previous mesh 
 * 
 */

static int
nsp_colnew_contrl (double *xi, double *xiold, double *z__, double *dmz,
		   double *rhs, double *delz, double *deldmz, double *dqz,
		   double *dqdmz, double *g, double *w, double *v,
		   double *valstr, double *slope, double *scale,
		   double *dscale, double *accum, int *ipvtg, int *integs,
		   int *ipvtw, int *nfxpnt, double *fixpnt, int *iflag,
		   Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub, Guess guess,
		   void *args, int *colnew_err)
{
  /* format strings */

  /* System generated locals */
  int i__1, i__2, i__3;
  double d__1, d__2;


  /* Local variables */
  double fact;
  int ifin, icor, ifrz, i__, j;
  double check, andif;
  double anscl;
  int imesh, ipred;
  double anfix, relax;
  int iconv, msing;
  double rnold, anorm, dummy[1], rnorm;
  int lj, it, iz;
  double factor;
  int ifreez;
  double relmin;
  int noconv;
  double rlxold;
  int lmtfrz;
  double rstart;
  int np1;
  double arg;
  int inz;

  /* Parameter adjustments */
  --fixpnt;
  --ipvtw;
  --integs;
  --ipvtg;
  --accum;
  --dscale;
  --scale;
  --slope;
  --valstr;
  --v;
  --w;
  --g;
  --dqdmz;
  --dqz;
  --deldmz;
  --delz;
  --rhs;
  --dmz;
  --z__;
  --xiold;
  --xi;

  /* Function Body */
  relmin = .001;
  rstart = .01;
  lmtfrz = 4;
  /* 
   *...  compute the maximum tolerance 
   * 
   */
  check = 0.;
  i__1 = colest_1.ntol;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L10: */
      /*Computing MAX 
       */
      d__1 = colest_1.tolin[i__ - 1];
      check = Max (d__1, check);
    }
  imesh = 1;
  iconv = 0;
  if (colnln_.nonlin == 0)
    {
      iconv = 1;
    }
  icor = 0;
  noconv = 0;
  msing = 0;
  /* 
   *...  the main iteration begins here . 
   *...  loop 20 is executed until error tolerances are satisfied or 
   *...  the code fails (due to a singular matrix or storage limitations) 
   * 
   */
 L20:
  /* 
   *...       initialization for a new mesh 
   * 
   */
  colnln_.iter = 0;
  if (colnln_.nonlin > 0)
    {
      goto L50;
    }
  /* 
   *...       the linear case. 
   *...       set up and solve equations 
   * 
   */
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], dummy, dummy, &z__[1],
		     &dmz[1], &g[1], &w[1], &v[1], &rhs[1], dummy, &integs[1],
		     &ipvtg[1], &ipvtw[1], &rnorm, &c__0, fsub,
		     dfsub,  gsub,  dgsub, guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       check for a singular matrix 
   * 
   */
  if (msing == 0)
    {
      goto L400;
    }
 L30:
  if (msing < 0)
    {
      goto L40;
    }
  if (colout_.iprint < 1)
    {
      Sciprintf("a local elimination matrix is singular \n");
    }
  goto L460;
 L40:
  if (colout_.iprint < 1)
    {
      Sciprintf("the global bvp-matrix is singular\n");
    }
  *iflag = 0;
  return 0;
  /* 
   *...       iteration loop for nonlinear case 
   *...       define the initial relaxation parameter (= relax) 
   * 
   */
 L50:
  relax = 1.;
  /* 
   *...       check for previous convergence and problem sensitivity 
   * 
   */
  if (colnln_.icare == 1 || colnln_.icare == -1)
    {
      relax = rstart;
    }
  if (iconv == 0)
    {
      goto L160;
    }
  /* 
   *...       convergence on a previous mesh has been obtained.    thus 
   *...       we have a very good initial approximation for the newton 
   *...       process.    proceed with one full newton and then iterate 
   *...       with a fixed jacobian. 
   * 
   */
  ifreez = 0;
  /* 
   *...       evaluate right hand side and its norm  and 
   *...       find the first newton correction 
   * 
   */
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &delz[1],
		     &deldmz[1], &g[1], &w[1], &v[1], &rhs[1], &dqdmz[1],
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnold, &c__1,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   */
  if (colout_.iprint < 0)
    {
      Sciprintf("fixed jacobian iterations\n");
    }
  if (colout_.iprint < 0)
    {
      Sciprintf("iteration = %d,  norm (rhs) = %10.2f\n",colnln_.iter,rnold);
    }
  goto L70;
  /* 
   *...       solve for the next iterate . 
   *...       the value of ifreez determines whether this is a full 
   *...       newton step (=0) or a fixed jacobian iteration (=1). 
   * 
   */
 L60:
  if (colout_.iprint < 0)
    {
      Sciprintf("iteration = %d,  norm (rhs) = %10.2f\n",colnln_.iter,rnorm);
    }
  rnold = rnorm;
  i__1 = ifreez + 3;
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &delz[1],
		     &deldmz[1], &g[1], &w[1], &v[1], &rhs[1], dummy,
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnorm, &i__1,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       check for a singular matrix 
   * 
   */
 L70:
  if (msing != 0)
    {
      goto L30;
    }
  if (ifreez == 1)
    {
      goto L80;
    }
  /* 
   *...       a full newton step 
   * 
   */
  ++colnln_.iter;
  ifrz = 0;
 L80:
  /* 
   *...       update   z and dmz , compute new  rhs  and its norm 
   * 
   */
  i__1 = colapr_.nz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      z__[i__] += delz[i__];
      /* L90: */
    }
  i__1 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dmz[i__] += deldmz[i__];
      /* L100: */
    }
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &delz[1],
		     &deldmz[1], &g[1], &w[1], &v[1], &rhs[1], dummy,
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnorm, &c__2,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       check monotonicity. if the norm of  rhs  gets smaller, 
   *...       proceed with a fixed jacobian; else proceed cautiously, 
   *...       as if convergence has not been obtained before (iconv=0). 
   * 
   */
  if (rnorm < colout_.precis)
    {
      goto L390;
    }
  if (rnorm > rnold)
    {
      goto L130;
    }
  if (ifreez == 1)
    {
      goto L110;
    }
  ifreez = 1;
  goto L60;
  /* 
   *...       verify that the linear convergence with fixed jacobian 
   *...       is fast enough. 
   * 
   */
 L110:
  ++ifrz;
  if (ifrz >= lmtfrz)
    {
      ifreez = 0;
    }
  if (rnold < rnorm * 4.)
    {
      ifreez = 0;
    }
  /* 
   *...       check convergence (iconv = 1). 
   * 
   */
  i__1 = colest_1.ntol;
  for (it = 1; it <= i__1; ++it)
    {
      inz = colest_1.ltol[it - 1];
      i__2 = colapr_.nz;
      i__3 = colord_.mstar;
      for (iz = inz; i__3 < 0 ? iz >= i__2 : iz <= i__2; iz += i__3)
	{
	  if ((d__1 =
	       delz[iz], Abs (d__1)) > colest_1.tolin[it - 1] * ((d__2 =
								  z__[iz],
								  Abs (d__2))
								 + 1.))
	    {
	      goto L60;
	    }
	  /* L120: */
	}
    }
  /* 
   *...       convergence obtained 
   * 
   */
  if (colout_.iprint < 1)
    {
      Sciprintf("convergence after %d iterations\n",colnln_.iter);
    }
  goto L400;
  /* 
   *...      convergence of fixed jacobian iteration failed. 
   * 
   */
 L130:
  if (colout_.iprint < 0)
    {
      Sciprintf("iteration = %d,  norm (rhs) = %10.2f\n",colnln_.iter,rnorm);
    }
  if (colout_.iprint < 0)
    {
      Sciprintf("switch to damped newton iteration\n");
    }
  iconv = 0;
  relax = rstart;
  i__3 = colapr_.nz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      z__[i__] -= delz[i__];
      /* L140: */
    }
  i__3 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      dmz[i__] -= deldmz[i__];
      /* L150: */
    }
  /* 
   *...       update old mesh 
   * 
   */
  np1 = colapr_.n + 1;
  i__3 = np1;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /* L155: */
      xiold[i__] = xi[i__];
    }
  colapr_.nold = colapr_.n;
  /* 
   */
  colnln_.iter = 0;
  /* 
   *...       no previous convergence has been obtained. proceed 
   *...       with the damped newton method. 
   *...       evaluate rhs and find the first newton correction. 
   * 
   */
 L160:
  if (colout_.iprint < 0)
    {
      Sciprintf("full damped newton iteration\n");
    }
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &delz[1],
		     &deldmz[1], &g[1], &w[1], &v[1], &rhs[1], &dqdmz[1],
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnold, &c__1,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       check for a singular matrix 
   * 
   */
  if (msing != 0)
    {
      goto L30;
    }
  /* 
   *...       bookkeeping for first mesh 
   * 
   */
  if (colnln_.iguess == 1)
    {
      colnln_.iguess = 0;
    }
  /* 
   *...       find initial scaling 
   * 
   */
  nsp_colnew_skale (&colapr_.n, &colord_.mstar, &colord_.kd, &z__[1],
		    &xi[1], &scale[1], &dscale[1]);
  goto L220;
  /* 
   *...       main iteration loop 
   * 
   */
 L170:
  rnold = rnorm;
  if (colnln_.iter >= colnln_.limit)
    {
      goto L430;
    }
  /* 
   *...       update scaling 
   * 
   */
  nsp_colnew_skale (&colapr_.n, &colord_.mstar, &colord_.kd, &z__[1],
		    &xi[1], &scale[1], &dscale[1]);
  /* 
   *...       compute norm of newton correction with new scaling 
   * 
   */
  anscl = 0.;
  i__3 = colapr_.nz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = delz[i__] * scale[i__];
      anscl += d__1 * d__1;
      /* L180: */
    }
  i__3 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = deldmz[i__] * dscale[i__];
      anscl += d__1 * d__1;
      /* L190: */
    }
  anscl = sqrt (anscl / (double) (colapr_.nz + colapr_.ndmz));
  /* 
   *...       find a newton direction 
   * 
   */
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &delz[1],
		     &deldmz[1], &g[1], &w[1], &v[1], &rhs[1], dummy,
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnorm, &c__3,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       check for a singular matrix 
   * 
   */
  if (msing != 0)
    {
      goto L30;
    }
  /* 
   *...       predict relaxation factor for newton step. 
   * 
   */
  andif = 0.;
  i__3 = colapr_.nz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = (dqz[i__] - delz[i__]) * scale[i__];
      andif += d__1 * d__1;
      /* L200: */
    }
  i__3 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = (dqdmz[i__] - deldmz[i__]) * dscale[i__];
      andif += d__1 * d__1;
      /* L210: */
    }
  andif =
    sqrt (andif / (double) (colapr_.nz + colapr_.ndmz) + colout_.precis);
  relax = relax * anscl / andif;
  if (relax > 1.)
    {
      relax = 1.;
    }
 L220:
  rlxold = relax;
  ipred = 1;
  ++colnln_.iter;
  /* 
   *...       determine a new  z and dmz  and find new  rhs  and its norm 
   * 
   */
  i__3 = colapr_.nz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      z__[i__] += relax * delz[i__];
      /* L230: */
    }
  i__3 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      dmz[i__] += relax * deldmz[i__];
      /* L240: */
    }
 L250:
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &dqz[1],
		     &dqdmz[1], &g[1], &w[1], &v[1], &rhs[1], dummy,
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnorm, &c__2,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       compute a fixed jacobian iterate (used to control relax) 
   * 
   */
  nsp_colnew_lsyslv (&msing, &xi[1], &xiold[1], &z__[1], &dmz[1], &dqz[1],
		     &dqdmz[1], &g[1], &w[1], &v[1], &rhs[1], dummy,
		     &integs[1], &ipvtg[1], &ipvtw[1], &rnorm, &c__4,
		     fsub,  dfsub,  gsub,  dgsub,
		     guess,args, colnew_err);
  if (*colnew_err == FAIL)
    {
      return 0;
    }
  /* 
   *...       find scaled norms of various terms used to correct relax 
   * 
   */
  anorm = 0.;
  anfix = 0.;
  i__3 = colapr_.nz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = delz[i__] * scale[i__];
      anorm += d__1 * d__1;
      /*Computing 2nd power 
       */
      d__1 = dqz[i__] * scale[i__];
      anfix += d__1 * d__1;
      /* L260: */
    }
  i__3 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = deldmz[i__] * dscale[i__];
      anorm += d__1 * d__1;
      /*Computing 2nd power 
       */
      d__1 = dqdmz[i__] * dscale[i__];
      anfix += d__1 * d__1;
      /* L270: */
    }
  anorm = sqrt (anorm / (double) (colapr_.nz + colapr_.ndmz));
  anfix = sqrt (anfix / (double) (colapr_.nz + colapr_.ndmz));
  if (icor == 1)
    {
      goto L280;
    }
  if (colout_.iprint < 0)
    {
      Sciprintf("iteration = %d  relaxation factor = %10.2f/ norm of scaled rhs changes from ,%10.2f, to,%10.2f/ norm   of   rhs  changes  from %10.2f, to %10.2f %10.2f\n",
		colnln_.iter, relax,anorm,anfix,rnold,rnorm);
    }
  goto L290;
 L280:
  if (colout_.iprint < 0)
    {
      Sciprintf("relaxation factor corrected to relax = %10.2f/ norm of scaled rhs changes from %10.2f to%10.2f/ norm of rhs changes from %10.2f to %10.2f ,%10.2f", 
		relax,anorm,anfix,rnold,rnorm);
    }
 L290:
  icor = 0;
  /* 
   *...       check for monotonic decrease in  delz and deldmz. 
   * 
   */
  if (anfix < colout_.precis || rnorm < colout_.precis)
    {
      goto L390;
    }
  if (anfix > anorm)
    {
      goto L300;
    }
  /* 
   *...       we have a decrease. 
   *...       if  dqz  and dqdmz  small, check for convergence 
   * 
   */
  if (anfix <= check)
    {
      goto L350;
    }
  /* 
   *...       correct the predicted  relax  unless the corrected 
   *...       value is within 10 percent of the predicted one. 
   * 
   */
  if (ipred != 1)
    {
      goto L170;
    }
 L300:
  if (colnln_.iter >= colnln_.limit)
    {
      goto L430;
    }
  /* 
   *...       correct the relaxation factor. 
   * 
   */
  ipred = 0;
  arg = (anfix / anorm - 1.) / relax + 1.;
  if (arg < 0.)
    {
      goto L170;
    }
  /*Computing 2nd power 
   */
  d__1 = relax;
  if (arg <= relax * .25 + d__1 * d__1 * .125)
    {
      goto L310;
    }
  factor = sqrt (arg * 8. + 1.) - 1.;
  if ((d__1 = factor - 1., Abs (d__1)) < factor * .1)
    {
      goto L170;
    }
  if (factor < .5)
    {
      factor = .5;
    }
  relax /= factor;
  goto L320;
 L310:
  if (relax >= .9)
    {
      goto L170;
    }
  relax = 1.;
 L320:
  icor = 1;
  if (relax < relmin)
    {
      goto L440;
    }
  fact = relax - rlxold;
  i__3 = colapr_.nz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      z__[i__] += fact * delz[i__];
      /* L330: */
    }
  i__3 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      dmz[i__] += fact * deldmz[i__];
      /* L340: */
    }
  rlxold = relax;
  goto L250;
  /* 
   *...       check convergence (iconv = 0). 
   * 
   */
 L350:
  i__3 = colest_1.ntol;
  for (it = 1; it <= i__3; ++it)
    {
      inz = colest_1.ltol[it - 1];
      i__2 = colapr_.nz;
      i__1 = colord_.mstar;
      for (iz = inz; i__1 < 0 ? iz >= i__2 : iz <= i__2; iz += i__1)
	{
	  if ((d__1 =
	       dqz[iz], Abs (d__1)) > colest_1.tolin[it - 1] * ((d__2 =
								 z__[iz],
								 Abs (d__2)) +
								1.))
	    {
	      goto L170;
	    }
	  /* L360: */
	}
    }
  /* 
   *...       convergence obtained 
   * 
   */
  if (colout_.iprint < 1)
    {
      Sciprintf("convergence after %d iterations\n", colnln_.iter);
    }
  /* 
   *...       since convergence obtained, update  z and dmz  with term 
   *...       from the fixed jacobian iteration. 
   * 
   */
  i__1 = colapr_.nz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      z__[i__] += dqz[i__];
      /* L370: */
    }
  i__1 = colapr_.ndmz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dmz[i__] += dqdmz[i__];
      /* L380: */
    }
 L390:
  if ((anfix < colout_.precis || rnorm < colout_.precis)
      && colout_.iprint < 1)
    {
      Sciprintf("convergence after %d iterations\n", colnln_.iter);
    }
  iconv = 1;
  if (colnln_.icare == -1)
    {
      colnln_.icare = 0;
    }
  /* 
   *...       if full output has been requested, print values of the 
   *...       solution components   z  at the meshpoints. 
   * 
   */
 L400:
  if (colout_.iprint >= 0)
    {
      goto L420;
    }
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      Sciprintf("mesh values for z(%d,",j);
      i__2 = colapr_.nz;
      i__3 = colord_.mstar;
      for (lj = j; i__3 < 0 ? lj >= i__2 : lj <= i__2; lj += i__3)
	{
	  Sciprintf("%15.7f ",z__[lj]);
	}
      Sciprintf(")\n");
    }
  /* 
   *...       check for error tolerance satisfaction 
   * 
   */
 L420:
  ifin = 1;
  if (imesh == 2)
    {
      nsp_colnew_errchk (&xi[1], &z__[1], &dmz[1], &valstr[1], &ifin);
    }
  if (imesh == 1 || (ifin == 0 && colnln_.icare != 2))
    {
      goto L460;
    }
  *iflag = 1;
  return 0;
  /* 
   *...       diagnostics for failure of nonlinear iteration. 
   * 
   */
 L430:
  if (colout_.iprint < 1)
    {
      Sciprintf("no convergence after %d iterations\n",colnln_.iter);
    }
  goto L450;
 L440:
  if (colout_.iprint < 1)
    {
      Sciprintf("no convergence.  relaxation factor %10.3f is too small (less than, %10.3f)\n",
		relax, relmin);
    }
 L450:
  *iflag = -2;
  ++noconv;
  if (colnln_.icare == 2 && noconv > 1)
    {
      return 0;
    }
  if (colnln_.icare == 0)
    {
      colnln_.icare = -1;
    }
  /* 
   *...       update old mesh 
   * 
   */
 L460:
  np1 = colapr_.n + 1;
  i__3 = np1;
  for (i__ = 1; i__ <= i__3; ++i__)
    {
      /* L470: */
      xiold[i__] = xi[i__];
    }
  colapr_.nold = colapr_.n;
  /* 
   *...       pick a new mesh 
   *...       check safeguards for mesh refinement 
   * 
   */
  imesh = 1;
  if (iconv == 0 || colmsh_.mshnum >= colmsh_.mshlmt
      || colmsh_.mshalt >= colmsh_.mshlmt)
    {
      imesh = 2;
    }
  if (colmsh_.mshalt >= colmsh_.mshlmt && colmsh_.mshnum < colmsh_.mshlmt)
    {
      colmsh_.mshalt = 1;
    }
  nsp_colnew_newmsh (&imesh, &xi[1], &xiold[1], &z__[1], &dmz[1], &valstr[1],
		     &slope[1], &accum[1], nfxpnt, &fixpnt[1]);
  /* 
   *...       exit if expected n is too large (but may try n=nmax once) 
   * 
   */
  if (colapr_.n <= colapr_.nmax)
    {
      goto L480;
    }
  colapr_.n /= 2;
  *iflag = -1;
  if (iconv == 0 && colout_.iprint < 1)
    {
      Sciprintf(" (no convergence)\n");
    }
  if (iconv == 1 && colout_.iprint < 1)
    {
      Sciprintf(" (probably tolerances too stringent, or nmax too ,small)\n");
    }
  return 0;
 L480:
  if (iconv == 0)
    {
      imesh = 1;
    }
  if (colnln_.icare == 1)
    {
      iconv = 0;
    }
  goto L20;
}


/* 
 *
 * 
 *  purpose 
 *           provide a proper scaling of the state variables, used 
 *           to control the damping factor for a newton iteration [2]. 
 *  variables 
 *           n      = number of mesh subintervals 
 *           mstar  = number of unknomns in z(u(x)) 
 *           kd     = number of unknowns in dmz 
 *           z      = the global unknown vector 
 *           xi     = the current mesh 
 *           scale  = scaling vector for z 
 *           dscale = scaling vector for dmz 
 * 
 */

static int nsp_colnew_skale (int *n, int *mstar, int *kd, double *z__, double *xi,
			     double *scale, double *dscale)
{
  /* System generated locals */
  int z_dim1, z_offset, scale_dim1, scale_offset, dscale_dim1, dscale_offset,
    i__1, i__2, i__3, i__4;
  double d__1, d__2;

  /* Local variables */
  double basm[5], scal;
  int idmz;
  double h__;
  int j, l, icomp, mj, iz, np1;
  /* Parameter adjustments */
  scale_dim1 = *mstar;
  scale_offset = scale_dim1 + 1;
  scale -= scale_offset;
  z_dim1 = *mstar;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  dscale_dim1 = *kd;
  dscale_offset = dscale_dim1 + 1;
  dscale -= dscale_offset;
  --xi;

  /* Function Body */
  basm[0] = 1.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      iz = 1;
      h__ = xi[j + 1] - xi[j];
      i__2 = colord_.mmax;
      for (l = 1; l <= i__2; ++l)
	{
	  basm[l] = basm[l - 1] * h__ / (double) l;
	  /* L10: */
	}
      i__2 = colord_.ncomp;
      for (icomp = 1; icomp <= i__2; ++icomp)
	{
	  scal =
	    ((d__1 = z__[iz + j * z_dim1], Abs (d__1)) + (d__2 =
							  z__[iz +
							      (j +
							       1) * z_dim1],
							  Abs (d__2))) * .5 +
	    1.;
	  mj = colord_.m[icomp - 1];
	  i__3 = mj;
	  for (l = 1; l <= i__3; ++l)
	    {
	      scale[iz + j * scale_dim1] = basm[l - 1] / scal;
	      ++iz;
	      /* L20: */
	    }
	  scal = basm[mj] / scal;
	  i__3 = *kd;
	  i__4 = colord_.ncomp;
	  for (idmz = icomp; i__4 < 0 ? idmz >= i__3 : idmz <= i__3;
	       idmz += i__4)
	    {
	      dscale[idmz + j * dscale_dim1] = scal;
	      /* L30: */
	    }
	  /* L40: */
	}
      /* L50: */
    }
  np1 = *n + 1;
  i__1 = *mstar;
  for (iz = 1; iz <= i__1; ++iz)
    {
      scale[iz + np1 * scale_dim1] = scale[iz + *n * scale_dim1];
      /* L60: */
    }
  return 0;
}	


/*---------------------------------------------------------------------- 
 *                           p a r t  2 
 *         mesh selection, error estimation, (and related 
 *         constant assignment) routines -- see [3], [4] 
 *---------------------------------------------------------------------- 
 * 
 */

static int nsp_colnew_newmsh (int *mode, double *xi, double *xiold, double *z__,
			      double *dmz, double *valstr, double *slope, double *accum,
			      int *nfxpnt, double *fixpnt)
{

  /* System generated locals */
  int i__1, i__2, i__3;
  double d__1, d__2, d__3, d__4, d__5;

  /* Local variables */
  double accl, accr;
  int lold;
  double avrg;
  int nmin;
  double temp;
  int lnew;
  double tsum;
  int nmax2, nfxp1, i__, j, l;
  double x, hiold;
  int ileft, iflip, nregn;
  double xleft, d1[40], d2[40], dummy[1];
  int n2, noldp1, jj, in;
  double dx;
  int jz, naccum;
  double degequ;
  int iright, lcarry;
  double oneovh, hd6, xright;
  int kstore;
  int np1;
  double slphmx;
  int nmx;



  /* 
*********************************************************************** 
* 
*  purpose 
*           select a mesh on which a collocation solution is to be 
*           determined 
* 
*                          there are 5 possible modes of action: 
*           mode = 5,4,3 - deal mainly with definition of an initial 
*                          mesh for the current boundary value problem 
*                = 2,1   - deal with definition of a new mesh, either 
*                          by simple mesh halving or by mesh selection 
*           more specifically, for 
*           mode = 5  an initial (generally nonuniform) mesh is 
*                     defined by the user and no mesh selection is to 
*                     be performed 
*                = 4  an initial (generally nonuniform) mesh is 
*                     defined by the user 
*                = 3  a simple uniform mesh (except possibly for some 
*                     fixed points) is defined; n= no. of subintervals 
*                = 1  the automatic mesh selection procedure is used 
*                     (see [3] for details) 
*                = 2  a simple mesh halving is performed 
* 
*********************************************************************** 
* 
*  variables 
* 
*           n      = number of mesh subintervals 
*           nold   = number of subintervals for former mesh 
*           xi     - mesh point array 
*           xiold  - former mesh point array 
*           mshlmt - maximum no. of mesh selections which are permitted 
*                    for a given n before mesh halving 
*           mshnum - no. of mesh selections which have actually been 
*                    performed for the given n 
*           mshalt - no. of consecutive times ( plus 1 ) the mesh 
*                    selection has alternately halved and doubled n. 
*                    if mshalt .ge. mshlmt then  contrl  requires 
*                    that the current mesh be halved. 
*           mshflg = 1  the mesh is a halving of its former mesh 
*                      (so an error estimate has been calculated) 
*                  = 0  otherwise 
*           iguess - ipar(9) in subroutine colnew.  it is used 
*                    here only for mode=5 and 4, where 
*                  = 2 the subroutine sets xi=xiold.  this is 
*                      used e.g. if continuation is being per- 
*                      formed, and a mesh for the old differen- 
*                      tial equation is being used 
*                  = 3 same as for =2, except xi uses every other 
*                      point of xiold (so mesh xiold is mesh xi 
*                      halved) 
*                  = 4 xi has been defined by the user, and an old 
*                      mesh xiold is also available 
*                      otherwise, xi has been defined by the user 
*                      and we set xiold=xi in this subroutine 
*           slope  - an approximate quantity to be equidistributed for 
*                    mesh selection (see [3]), viz, 
*                            .                        (k+mj) 
*                    slope(i)=     Max(weight(l) *u      (xi(i))) 
*                              1.le.l.le.ntol         j 
* 
*                    where j=jtol(l) 
*           slphmx - maximum of slope(i)*(xiold(i+1)-xiold(i)) for 
*                    i = 1 ,..., nold. 
*           accum  - accum(i) is the integral of  slope  from  aleft 
*                    to  xiold(i). 
*           valstr - is assigned values needed in  errchk  for the 
*                    error estimate. 
*********************************************************************** 
* 
*     IMPLICIT REAL*8 (A-H,O-Z) 
* 
* 
*/
  /* Parameter adjustments */
  --fixpnt;
  --accum;
  --slope;
  --valstr;
  --dmz;
  --z__;
  --xiold;
  --xi;

  /* Function Body */
  nfxp1 = *nfxpnt + 1;
  switch (*mode)
    {
    case 1:
      goto L180;
    case 2:
      goto L100;
    case 3:
      goto L50;
    case 4:
      goto L20;
    case 5:
      goto L10;
    }
  /* 
   *...  mode=5   set mshlmt=1 so that no mesh selection is performed 
   * 
   */
 L10:
  colmsh_.mshlmt = 1;
  /* 
   *...  mode=4   the user-specified initial mesh is already in place. 
   * 
   */
 L20:
  if (colnln_.iguess < 2)
    {
      goto L40;
    }
  /* 
   *...  iguess=2, 3 or 4. 
   * 
   */
  noldp1 = colapr_.nold + 1;
  if (colout_.iprint < 1)
    {
      Sciprintf("the former mesh (of %d subintervals)\n",colapr_.nold);
      i__1 = noldp1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  Sciprintf("%10.5f ",xiold[i__]);
	}
      Sciprintf("\n");
    }
  if (colnln_.iguess != 3)
    {
      goto L40;
    }
  /* 
   *...  if iread ( ipar(8) ) .ge. 1 and iguess ( ipar(9) ) .eq. 3 
   *...  then the first mesh is every second point of the 
   *...  mesh in  xiold . 
   * 
   */
  colapr_.n = colapr_.nold / 2;
  i__ = 0;
  i__1 = colapr_.nold;
  for (j = 1; j <= i__1; j += 2)
    {
      ++i__;
      /* L30: */
      xi[i__] = xiold[j];
    }
 L40:
  np1 = colapr_.n + 1;
  xi[1] = colsid_.aleft;
  xi[np1] = colsid_.aright;
  goto L320;
  /* 
   *...  mode=3   generate a (piecewise) uniform mesh. if there are 
   *...  fixed points then ensure that the n being used is large enough. 
   * 
   */
 L50:
  if (colapr_.n < nfxp1)
    {
      colapr_.n = nfxp1;
    }
  np1 = colapr_.n + 1;
  xi[1] = colsid_.aleft;
  ileft = 1;
  xleft = colsid_.aleft;
  /* 
   *...  loop over the subregions between fixed points. 
   * 
   */
  i__1 = nfxp1;
  for (j = 1; j <= i__1; ++j)
    {
      xright = colsid_.aright;
      iright = np1;
      if (j == nfxp1)
	{
	  goto L60;
	}
      xright = fixpnt[j];
      /* 
       *...       determine where the j-th fixed point should fall in the 
       *...       new mesh - this is xi(iright) and the (j-1)st fixed 
       *...       point is in xi(ileft) 
       * 
       */
      nmin =
	(int) ((xright - colsid_.aleft) / (colsid_.aright -
					   colsid_.aleft) *
	       (double) colapr_.n + 1.5);
      if (nmin > colapr_.n - *nfxpnt + j)
	{
	  nmin = colapr_.n - *nfxpnt + j;
	}
      /*Computing MAX 
       */
      i__2 = ileft + 1;
      iright = Max (i__2, nmin);
    L60:
      xi[iright] = xright;
      /* 
       *...       generate equally spaced points between the j-1st and the 
       *...       j-th fixed points. 
       * 
       */
      nregn = iright - ileft - 1;
      if (nregn == 0)
	{
	  goto L80;
	}
      dx = (xright - xleft) / (double) (nregn + 1);
      i__2 = nregn;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L70: */
	  xi[ileft + i__] = xleft + (double) i__ *dx;
	}
    L80:
      ileft = iright;
      xleft = xright;
      /* L90: */
    }
  goto L320;
  /* 
   *...  mode=2  halve the current mesh (i.e. double its size) 
   * 
   */
 L100:
  n2 = colapr_.n << 1;
  /* 
   *...  check that n does not exceed storage limitations 
   * 
   */
  if (n2 <= colapr_.nmax)
    {
      goto L120;
    }
  /* 
   *...  if possible, try with n=nmax. redistribute first. 
   * 
   */
  if (*mode == 2)
    {
      goto L110;
    }
  colapr_.n = colapr_.nmax / 2;
  goto L220;
 L110:
  if (colout_.iprint < 1)
    {
      Sciprintf(" expected n too large\n");
    }
  colapr_.n = n2;
  return 0;
  /* 
   *...  calculate the old approximate solution values at 
   *...  points to be used in  errchk  for error estimates. 
   *...  if  mshflg  =1 an error estimate was obtained for 
   *...  for the old approximation so half the needed values 
   *...  will already be in  valstr . 
   * 
   */
 L120:
  if (colmsh_.mshflg == 0)
    {
      goto L140;
    }
  /* 
   *...  save in  valstr  the values of the old solution 
   *...  at the relative positions 1/6 and 5/6 in each subinterval. 
   * 
   */
  kstore = 1;
  i__1 = colapr_.nold;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      hd6 = (xiold[i__ + 1] - xiold[i__]) / 6.;
      x = xiold[i__] + hd6;
      nsp_colnew_approx (&i__, &x, &valstr[kstore], colbas_.asave, dummy,
			 &xiold[1], &colapr_.nold, &z__[1], &dmz[1],
			 &colord_.k, &colord_.ncomp, &colord_.mmax,
			 colord_.m, &colord_.mstar, &c__4, dummy, &c__0);
      x += hd6 * 4.;
      kstore += colord_.mstar * 3;
      nsp_colnew_approx (&i__, &x, &valstr[kstore], &colbas_.asave[84],
			 dummy, &xiold[1], &colapr_.nold, &z__[1], &dmz[1],
			 &colord_.k, &colord_.ncomp, &colord_.mmax,
			 colord_.m, &colord_.mstar, &c__4, dummy, &c__0);
      kstore += colord_.mstar;
      /* L130: */
    }
  goto L160;
  /* 
   *...  save in  valstr  the values of the old solution 
   *...  at the relative positions 1/6, 2/6, 4/6 and 5/6 in 
   *...  each subinterval. 
   * 
   */
 L140:
  kstore = 1;
  i__1 = colapr_.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      x = xi[i__];
      hd6 = (xi[i__ + 1] - xi[i__]) / 6.;
      for (j = 1; j <= 4; ++j)
	{
	  x += hd6;
	  if (j == 3)
	    {
	      x += hd6;
	    }
	  nsp_colnew_approx (&i__, &x, &valstr[kstore],
			     &colbas_.asave[j * 28 - 28], dummy, &xiold[1],
			     &colapr_.nold, &z__[1], &dmz[1], &colord_.k,
			     &colord_.ncomp, &colord_.mmax, colord_.m,
			     &colord_.mstar, &c__4, dummy, &c__0);
	  kstore += colord_.mstar;
	  /* L150: */
	}
    }
 L160:
  colmsh_.mshflg = 0;
  colmsh_.mshnum = 1;
  *mode = 2;
  /* 
   *...  generate the halved mesh. 
   * 
   */
  j = 2;
  i__1 = colapr_.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xi[j] = (xiold[i__] + xiold[i__ + 1]) / 2.;
      xi[j + 1] = xiold[i__ + 1];
      /* L170: */
      j += 2;
    }
  colapr_.n = n2;
  goto L320;
  /* 
   *...  mode=1  we do mesh selection if it is deemed worthwhile 
   * 
   */
 L180:
  if (colapr_.nold == 1)
    {
      goto L100;
    }
  if (colapr_.nold <= *nfxpnt << 1)
    {
      goto L100;
    }
  /* 
   *...  the first interval has to be treated separately from the 
   *...  other intervals (generally the solution on the (i-1)st and ith 
   *...  intervals will be used to approximate the needed derivative, but 
   *...  here the 1st and second intervals are used.) 
   * 
   */
  i__ = 1;
  hiold = xiold[2] - xiold[1];
  nsp_colnew_horder (&c__1, d1, &hiold, &dmz[1], &colord_.ncomp,
		     &colord_.k);
  hiold = xiold[3] - xiold[2];
  nsp_colnew_horder (&c__2, d2, &hiold, &dmz[1], &colord_.ncomp,
		     &colord_.k);
  accum[1] = 0.;
  slope[1] = 0.;
  oneovh = 2. / (xiold[3] - xiold[1]);
  i__1 = colest_1.ntol;
  for (j = 1; j <= i__1; ++j)
    {
      jj = colest_1.jtol[j - 1];
      jz = colest_1.ltol[j - 1];
      /* L190: */
      /*Computing MAX 
       */
      d__5 = (d__1 =
	      d2[jj - 1] - d1[jj - 1],
	      Abs (d__1)) * colest_1.wgtmsh[j - 1] * oneovh / ((d__2 =
								z__[jz],
								Abs (d__2)) +
							       1.);
      d__3 = slope[1], d__4 = pow (d__5, colest_1.root[j - 1]);
      slope[1] = Max (d__3, d__4);
    }
  slphmx = slope[1] * (xiold[2] - xiold[1]);
  accum[2] = slphmx;
  iflip = 1;
  /* 
   *...  go through the remaining intervals generating  slope 
   *...  and  accum . 
   * 
   */
  i__1 = colapr_.nold;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      hiold = xiold[i__ + 1] - xiold[i__];
      if (iflip == -1)
	{
	  nsp_colnew_horder (&i__, d1, &hiold, &dmz[1], &colord_.ncomp,
			     &colord_.k);
	}
      if (iflip == 1)
	{
	  nsp_colnew_horder (&i__, d2, &hiold, &dmz[1], &colord_.ncomp,
			     &colord_.k);
	}
      oneovh = 2. / (xiold[i__ + 1] - xiold[i__ - 1]);
      slope[i__] = 0.;
      /* 
       *...       evaluate function to be equidistributed 
       * 
       */
      i__2 = colest_1.ntol;
      for (j = 1; j <= i__2; ++j)
	{
	  jj = colest_1.jtol[j - 1];
	  jz = colest_1.ltol[j - 1] + (i__ - 1) * colord_.mstar;
	  /* L200: */
	  /*Computing MAX 
	   */
	  d__5 = (d__1 =
		  d2[jj - 1] - d1[jj - 1],
		  Abs (d__1)) * colest_1.wgtmsh[j - 1] * oneovh / ((d__2 =
								    z__[jz],
								    Abs
								    (d__2)) +
								   1.);
	  d__3 = slope[i__], d__4 = pow (d__5, colest_1.root[j - 1]);
	  slope[i__] = Max (d__3, d__4);
	}
      /* 
       *...       accumulate approximate integral of function to be 
       *...       equidistributed 
       * 
       */
      temp = slope[i__] * (xiold[i__ + 1] - xiold[i__]);
      slphmx = Max (slphmx, temp);
      accum[i__ + 1] = accum[i__] + temp;
      /* L210: */
      iflip = -iflip;
    }
  avrg = accum[colapr_.nold + 1] / (double) colapr_.nold;
  degequ = avrg / Max (slphmx, colout_.precis);
  /* 
   *...  naccum=expected n to achieve .1x user requested tolerances 
   * 
   */
  naccum = (int) (accum[colapr_.nold + 1] + 1.);
  if (colout_.iprint < 0)
    {
      Sciprintf("mesh selection info, degree of equidistribution = %8.5f, prediction for required n =%d\n",degequ,naccum);
    }
  /* 
   *...  decide if mesh selection is worthwhile (otherwise, halve) 
   * 
   */
  if (avrg < colout_.precis)
    {
      goto L100;
    }
  if (degequ >= .5)
    {
      goto L100;
    }
  /* 
   *...  nmx assures mesh has at least half as many subintervals as the 
   *...  previous mesh 
   * 
   *Computing MAX 
   */
  i__1 = colapr_.nold + 1;
  nmx = Max (i__1, naccum) / 2;
  /* 
   *...  this assures that halving will be possible later (for error est) 
   * 
   */
  nmax2 = colapr_.nmax / 2;
  /* 
   *...  the mesh is at most halved 
   * 
   *Computing MIN 
   */
  i__1 = Min (nmax2, colapr_.nold);
  colapr_.n = Min (i__1, nmx);
 L220:
  noldp1 = colapr_.nold + 1;
  if (colapr_.n < nfxp1)
    {
      colapr_.n = nfxp1;
    }
  ++colmsh_.mshnum;
  /* 
   *...  if the new mesh is smaller than the old mesh set mshnum 
   *...  so that the next call to  newmsh  will produce a halved 
   *...  mesh. if n .eq. nold / 2 increment mshalt so there can not 
   *...  be an infinite loop alternating between n and n/2 points. 
   * 
   */
  if (colapr_.n < colapr_.nold)
    {
      colmsh_.mshnum = colmsh_.mshlmt;
    }
  if (colapr_.n > colapr_.nold / 2)
    {
      colmsh_.mshalt = 1;
    }
  if (colapr_.n == colapr_.nold / 2)
    {
      ++colmsh_.mshalt;
    }
  colmsh_.mshflg = 0;
  /* 
   *...  having decided to generate a new mesh with n subintervals we now 
   *...  do so, taking into account that the nfxpnt points in the array 
   *...  fixpnt must be included in the new mesh. 
   * 
   */
  in = 1;
  accl = 0.;
  lold = 2;
  xi[1] = colsid_.aleft;
  xi[colapr_.n + 1] = colsid_.aright;
  i__1 = nfxp1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (i__ == nfxp1)
	{
	  goto L250;
	}
      i__2 = noldp1;
      for (j = lold; j <= i__2; ++j)
	{
	  lnew = j;
	  if (fixpnt[i__] <= xiold[j])
	    {
	      goto L240;
	    }
	  /* L230: */
	}
    L240:
      accr = accum[lnew] + (fixpnt[i__] - xiold[lnew]) * slope[lnew - 1];
      nregn =
	(int) ((accr - accl) / accum[noldp1] * (double) colapr_.n - .5);
      /*Computing MIN 
       */
      i__2 = nregn, i__3 = colapr_.n - in - nfxp1 + i__;
      nregn = Min (i__2, i__3);
      xi[in + nregn + 1] = fixpnt[i__];
      goto L260;
    L250:
      accr = accum[noldp1];
      lnew = noldp1;
      nregn = colapr_.n - in;
    L260:
      if (nregn == 0)
	{
	  goto L300;
	}
      temp = accl;
      tsum = (accr - accl) / (double) (nregn + 1);
      i__2 = nregn;
      for (j = 1; j <= i__2; ++j)
	{
	  ++in;
	  temp += tsum;
	  i__3 = lnew;
	  for (l = lold; l <= i__3; ++l)
	    {
	      lcarry = l;
	      if (temp <= accum[l])
		{
		  goto L280;
		}
	      /* L270: */
	    }
	L280:
	  lold = lcarry;
	  /* L290: */
	  xi[in] =
	    xiold[lold - 1] + (temp - accum[lold - 1]) / slope[lold - 1];
	}
    L300:
      ++in;
      accl = accr;
      lold = lnew;
      /* L310: */
    }
  *mode = 1;
 L320:
  np1 = colapr_.n + 1;
  if (colout_.iprint < 1)
    {
      Sciprintf("the new mesh (of %d subintervals)\n",colapr_.n);
      i__1 = np1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  Sciprintf("%10.5f ",xi[i__]);
	}
      Sciprintf("\n");
    }
  colapr_.nz = colord_.mstar * (colapr_.n + 1);
  colapr_.ndmz = colord_.kd * colapr_.n;
  return 0;
  /*---------------------------------------------------------------- 
   */
}				/* newmsh_ */

static int nsp_colnew_consts (int *k, double *rho, double *coef)
{
  /* Initialized data */

  static double cnsts1[28] =
    { .25, .0625, .072169, .018342, .019065, .05819, .0054658, .005337,
      .01889, .027792, .0016095, .0014964, .0075938, .0057573, .018342, .004673, 4.15e-4, .001919,
      .001468, .006371, .00461, 1.342e-4, 1.138e-4, 4.889e-4, 4.177e-4, .001374, .001654, .002863 };
  static double cnsts2[28] =
    { .125, .002604, .008019, 2.17e-5, 7.453e-5, 5.208e-4, 9.689e-8, 3.689e-7,
      3.1e-6, 2.451e-5, 2.691e-10, 1.12e-9, 1.076e-8, 9.405e-8, 1.033e-6, 5.097e-13, 2.29e-12,
      2.446e-11, 2.331e-10, 2.936e-9, 3.593e-8, 7.001e-16, 3.363e-15, 3.921e-14, 4.028e-13, 5.646e-12,
      7.531e-11, 1.129e-9 };

  /* System generated locals */
  int coef_dim1, coef_offset, i__1, i__2;

  /* Local variables */
  int koff, mtot, i__, j, l;
  int jcomp, ltoli;
  double dummy[1];
  int mj, iz;

  /* 
*********************************************************************** 
* 
*  purpose 
*           assign (once) values to various array constants. 
* 
*  arrays assigned during compilation: 
*    cnsts1 - weights for extrapolation error estimate 
*    cnsts2 - weights for mesh selection 
*             (the above weights come from the theoretical form for 
*             the collocation error -- see [3]) 
* 
*  arrays assigned during execution: 
*    wgterr - the particular values of cnsts1 used for current run 
*             (depending on k, m) 
*    wgtmsh - gotten from the values of cnsts2 which in turn are 
*             the constants in the theoretical expression for the 
*             errors. the quantities in wgtmsh are 10x the values 
*             in cnsts2 so that the mesh selection algorithm 
*             is aiming for errors .1x as large as the user 
*             requested tolerances. 
*    jtol   - components of differential system to which tolerances 
*             refer (viz, if ltol(i) refers to a derivative of u(j), 
*             then jtol(i)=j) 
*    root   - reciprocals of expected rates of convergence of compo- 
*             nents of z(j) for which tolerances are specified 
*    rho    - the k collocation points on (0,1) 
*    coef   - 
*    acol  -  the runge-kutta coefficients values at collocation 
*             points 
* 
*********************************************************************** 
* 
*     IMPLICIT REAL*8 (A-H,O-Z) 
* 
* 
*/
  /* Parameter adjustments */
  coef_dim1 = *k;
  coef_offset = coef_dim1 + 1;
  coef -= coef_offset;
  --rho;

  /* Function Body */
  /* 
   *...  assign weights for error estimate 
   * 
   */
  koff = *k * (*k + 1) / 2;
  iz = 1;
  i__1 = colord_.ncomp;
  for (j = 1; j <= i__1; ++j)
    {
      mj = colord_.m[j - 1];
      i__2 = mj;
      for (l = 1; l <= i__2; ++l)
	{
	  colest_1.wgterr[iz - 1] = cnsts1[koff - mj + l - 1];
	  ++iz;
	  /* L10: */
	}
    }
  /* 
   *...  assign array values for mesh selection: wgtmsh, jtol, and root 
   * 
   */
  jcomp = 1;
  mtot = colord_.m[0];
  i__2 = colest_1.ntol;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      ltoli = colest_1.ltol[i__ - 1];
    L20:
      if (ltoli <= mtot)
	{
	  goto L30;
	}
      ++jcomp;
      mtot += colord_.m[jcomp - 1];
      goto L20;
    L30:
      colest_1.jtol[i__ - 1] = jcomp;
      colest_1.wgtmsh[i__ - 1] =
	cnsts2[koff + ltoli - mtot - 1] * 10. / colest_1.tolin[i__ - 1];
      colest_1.root[i__ - 1] = 1. / (double) (*k + mtot - ltoli + 1);
      /* L40: */
    }
  /* 
   *...  specify collocation points 
   * 
   */
  switch (*k)
    {
    case 1:
      goto L50;
    case 2:
      goto L60;
    case 3:
      goto L70;
    case 4:
      goto L80;
    case 5:
      goto L90;
    case 6:
      goto L100;
    case 7:
      goto L110;
    }
 L50:
  rho[1] = 0.;
  goto L120;
 L60:
  rho[2] = .57735026918962576451;
  rho[1] = -rho[2];
  goto L120;
 L70:
  rho[3] = .77459666924148337704;
  rho[2] = 0.;
  rho[1] = -rho[3];
  goto L120;
 L80:
  rho[4] = .86113631159405257523;
  rho[3] = .3399810435848562648;
  rho[2] = -rho[3];
  rho[1] = -rho[4];
  goto L120;
 L90:
  rho[5] = .9061798459386639928;
  rho[4] = .53846931010568309104;
  rho[3] = 0.;
  rho[2] = -rho[4];
  rho[1] = -rho[5];
  goto L120;
 L100:
  rho[6] = .93246951420315202781;
  rho[5] = .66120938646626451366;
  rho[4] = .23861918608319690863;
  rho[3] = -rho[4];
  rho[2] = -rho[5];
  rho[1] = -rho[6];
  goto L120;
 L110:
  rho[7] = .94910791234275852452;
  rho[6] = .74153118559939443986;
  rho[5] = .4058451513773971669;
  rho[4] = 0.;
  rho[3] = -rho[5];
  rho[2] = -rho[6];
  rho[1] = -rho[7];
 L120:
  /* 
   *...  map (-1,1) to (0,1) by  t = .5 * (1. + x) 
   * 
   */
  i__2 = *k;
  for (j = 1; j <= i__2; ++j)
    {
      rho[j] = (rho[j] + 1.) * .5;
      /* L130: */
    }
  /* 
   *...  now find runge-kutta coeffitients b, acol and asave 
   *...  the values of asave are to be used in  newmsh  and errchk . 
   * 
   */
  i__2 = *k;
  for (j = 1; j <= i__2; ++j)
    {
      i__1 = *k;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  /* L135: */
	  coef[i__ + j * coef_dim1] = 0.;
	}
      coef[j + j * coef_dim1] = 1.;
      nsp_colnew_vmonde (&rho[1], &coef[j * coef_dim1 + 1], k);
      /* L140: */
    }
  nsp_colnew_rkbas (&c_b263, &coef[coef_offset], k, &colord_.mmax,
		    colbas_.b, dummy, &c__0);
  i__2 = *k;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      nsp_colnew_rkbas (&rho[i__], &coef[coef_offset], k, &colord_.mmax,
			&colbas_.acol[i__ * 28 - 28], dummy, &c__0);
      /* L150: */
    }
  nsp_colnew_rkbas (&c_b267, &coef[coef_offset], k, &colord_.mmax,
		    colbas_.asave, dummy, &c__0);
  nsp_colnew_rkbas (&c_b269, &coef[coef_offset], k, &colord_.mmax,
		    &colbas_.asave[28], dummy, &c__0);
  nsp_colnew_rkbas (&c_b271, &coef[coef_offset], k, &colord_.mmax,
		    &colbas_.asave[56], dummy, &c__0);
  nsp_colnew_rkbas (&c_b273, &coef[coef_offset], k, &colord_.mmax,
		    &colbas_.asave[84], dummy, &c__0);
  return 0;
}				/* consts_ */

static int
nsp_colnew_errchk (double *xi, double *z__, double *dmz, double *valstr,
		   int *ifin)
{

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int knew, ltjz, iback, j, i__, l;
  double x;
  int ltolj;
  double dummy[1];
  int lj, mj;
  double errest[40];
  int kstore;
  double err[40];

  /* Fortran I/O blocks */

  /* Format strings */




  /* 
*********************************************************************** 
* 
*     purpose 
*              determine the error estimates and test to see if the 
*              error tolerances are satisfied. 
* 
*     variables 
*       xi     - current mesh points 
*       valstr - values of the previous solution which are needed 
*                for the extrapolation- like error estimate. 
*       wgterr - weights used in the extrapolation-like error 
*                estimate. the array values are assigned in 
*                subroutine  consts. 
*       errest - storage for error estimates 
*       err    - temporary storage used for error estimates 
*       z      - approximate solution on mesh xi 
*       ifin   - a 0-1 variable. on return it indicates whether 
*                the error tolerances were satisfied 
*       mshflg - is set by errchk to indicate to newmsh whether 
*                any values of the current solution are stored in 
*                the array valstr. (0 for no, 1 for yes) 
* 
*********************************************************************** 
* 
*     IMPLICIT REAL*8 (A-H,O-Z) 
* 
* 
*...  error estimates are to be generated and tested 
*...  to see if the tolerance requirements are satisfied. 
* 
*/
  /* Parameter adjustments */
  --valstr;
  --dmz;
  --z__;
  --xi;

  /* Function Body */
  *ifin = 1;
  colmsh_.mshflg = 1;
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      /* L10: */
      errest[j - 1] = 0.;
    }
  i__1 = colapr_.n;
  for (iback = 1; iback <= i__1; ++iback)
    {
      i__ = colapr_.n + 1 - iback;
      /* 
       *...       the error estimates are obtained by combining values of 
       *...       the numerical solutions for two meshes. 
       *...       for each value of iback we will consider the two 
       *...       approximations at 2 points in each of 
       *...       the new subintervals.  we work backwards through 
       *...       the subinterval so that new values can be stored 
       *...       in valstr in case they prove to be needed later 
       *...       for an error estimate. the routine  newmsh 
       *...       filled in the needed values of the old solution 
       *...       in valstr. 
       * 
       */
      knew = (((i__ - 1) << 2) + 2) * colord_.mstar + 1;
      kstore = (((i__ - 1) << 1) + 1) * colord_.mstar + 1;
      x = xi[i__] + (xi[i__ + 1] - xi[i__]) * 2. / 3.;
      nsp_colnew_approx (&i__, &x, &valstr[knew], &colbas_.asave[56], dummy,
			 &xi[1], &colapr_.n, &z__[1], &dmz[1], &colord_.k,
			 &colord_.ncomp, &colord_.mmax, colord_.m,
			 &colord_.mstar, &c__4, dummy, &c__0);
      i__2 = colord_.mstar;
      for (l = 1; l <= i__2; ++l)
	{
	  err[l - 1] = colest_1.wgterr[l - 1] * (d__1 =
						 valstr[knew] -
						 valstr[kstore], Abs (d__1));
	  ++knew;
	  ++kstore;
	  /* L20: */
	}
      knew = (((i__ - 1) << 2) + 1) * colord_.mstar + 1;
      kstore = ((i__ - 1) << 1) * colord_.mstar + 1;
      x = xi[i__] + (xi[i__ + 1] - xi[i__]) / 3.;
      nsp_colnew_approx (&i__, &x, &valstr[knew], &colbas_.asave[28], dummy,
			 &xi[1], &colapr_.n, &z__[1], &dmz[1], &colord_.k,
			 &colord_.ncomp, &colord_.mmax, colord_.m,
			 &colord_.mstar, &c__4, dummy, &c__0);
      i__2 = colord_.mstar;
      for (l = 1; l <= i__2; ++l)
	{
	  err[l - 1] += colest_1.wgterr[l - 1] * (d__1 =
						  valstr[knew] -
						  valstr[kstore], Abs (d__1));
	  ++knew;
	  ++kstore;
	  /* L30: */
	}
      /* 
       *...       find component-wise maximum error estimate 
       * 
       */
      i__2 = colord_.mstar;
      for (l = 1; l <= i__2; ++l)
	{
	  /*Computing MAX 
	   */
	  d__1 = errest[l - 1], d__2 = err[l - 1];
	  errest[l - 1] = Max (d__1, d__2);
	  /* L40: */
	}
      /* 
       *...       test whether the tolerance requirements are satisfied 
       *...       in the i-th interval. 
       * 
       */
      if (*ifin == 0)
	{
	  goto L60;
	}
      i__2 = colest_1.ntol;
      for (j = 1; j <= i__2; ++j)
	{
	  ltolj = colest_1.ltol[j - 1];
	  ltjz = ltolj + (i__ - 1) * colord_.mstar;
	  if (err[ltolj - 1] >
	      colest_1.tolin[j - 1] * ((d__1 = z__[ltjz], Abs (d__1)) + 1.))
	    {
	      *ifin = 0;
	    }
	  /* L50: */
	}
    L60:
      ;
    }
  if (colout_.iprint >= 0)
    {
      return 0;
    }
  Sciprintf("THE ESTIMATED ERRORS ARE ");
  lj = 1;
  i__1 = colord_.ncomp;
  for (j = 1; j <= i__1; ++j)
    {
      mj = lj - 1 + colord_.m[j - 1];
      Sciprintf("U(%d,",j);
      i__2 = mj;
      for (l = lj; l <= i__2; ++l)
	{
	  Sciprintf("%12.4f ",errest[l - 1]);
	}
      Sciprintf(")\n");
      lj = mj + 1;
      /* L70: */
    }
  return 0;
}

/*--------------------------------------------------------------------- 
 *                           p a r t  3 
 *         collocation system setup routines 
 *--------------------------------------------------------------------- 
 * 
 */
int
nsp_colnew_lsyslv (int *msing, double *xi, double *xiold, double *z__,
		   double *dmz, double *delz, double *deldmz, double *g,
		   double *w, double *v, double *rhs, double *dmzo,
		   int *integs, int *ipvtg, int *ipvtw, double *rnorm,
		   int *mode, Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub,
		   Guess guess, void *args, int *colnew_err)
{
  /* System generated locals */
  int i__1, i__2, i__3;
  double d__1;


  /* Local variables */
  int iold;
  double gval;
  int ncol, idmz, irhs;
  double hrho, xcol, zval[40];
  int izet, nrow;
  double f[40], h__;
  int i__, j, l, lside;
  double dmval[20], value;
  int idmzo;
  double dummy[1];
  int m1;
  double df[800];
  int ig, jj;
  double at[28];
  int iv, iw;
  int lw;
  int iz;
  double dgz[40], xii;

  /********************************************************************** 
   * 
   *  purpose 
   *        this routine controls the set up and solution of a linear 
   *     system of collocation equations. 
   *        the matrix  g  is cast into an almost block diagonal 
   *     form by an appropriate ordering of the columns and solved 
   *     using the package of de boor-weiss [5]. the matrix is composed 
   *     of n blocks. the i-th block has the size 
   *                 integs(1,i) * integs(2,i). 
   *     it contains in its last rows the linearized collocation 
   *     equations, condensed as described in [2], 
   *     and the linearized side conditions corresponding to 
   *     the i-th subinterval.  integs(3,i)  steps of gaussian 
   *     elimination are applied to it to achieve a  partial plu 
   *     decomposition.  the right hand side vector is put into  rhs 
   *     and the solution vector is returned in  delz and deldmz. 
   * 
   *        lsyslv operates according to one of 5 modes: 
   *     mode = 0 - set up the collocation matrices  v , w , g 
   *                and the right hand side  rhs ,  and solve. 
   *                (for linear problems only.) 
   *     mode = 1 - set up the collocation matrices  v , w , g 
   *                and the right hand sides  rhs  and  dmzo , 
   *                and solve. also set up  integs . 
   *                (first iteration of nonlinear problems only). 
   *     mode = 2 - set up  rhs  only and compute its norm. 
   *     mode = 3 - set up  v, w, g  only and solve system. 
   *     mode = 4 - perform forward and backward substitution only 
   *                (do not set up the matrices nor form the rhs). 
   * 
   *  variables 
   * 
   *     ig,izeta  - pointers to g,zeta respectively 
   *                      (necessary to keep track of blocks of g 
   *                      during matrix manipulations) 
   *     idmz,irhs,iv,iw - pointers to  rhs,v,w rspectively 
   *     df    - partial derivatives of f from dfsub 
   *     rnorm - euclidean norm of rhs 
   *     lside - number of side conditions in current and previous blocks 
   *     iguess = 1 when current soln is user specified via  guess 
   *            = 0 otherwise 
   * 
   ********************************************************************** 
   *     IMPLICIT REAL*8 (A-H,O-Z) 
   * 
   * 
   * 
   */
  /* Parameter adjustments */
  --ipvtw;
  --ipvtg;
  integs -= 4;
  --dmzo;
  --rhs;
  --v;
  --w;
  --g;
  --deldmz;
  --delz;
  --dmz;
  --z__;
  --xiold;
  --xi;

  /* Function Body */
  m1 = *mode + 1;
  switch (m1)
    {
    case 1:
      goto L10;
    case 2:
      goto L30;
    case 3:
      goto L30;
    case 4:
      goto L30;
    case 5:
      goto L310;
    }
  /* 
   *...  linear problem initialization 
   * 
   */
 L10:
  i__1 = colord_.mstar;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L20: */
      zval[i__ - 1] = 0.;
    }
  /* 
   *...  initialization 
   * 
   */
 L30:
  idmz = 1;
  idmzo = 1;
  irhs = 1;
  ig = 1;
  iw = 1;
  iv = 1;
  colsid_.izeta = 1;
  lside = 0;
  iold = 1;
  ncol = colord_.mstar << 1;
  *rnorm = 0.;
  if (*mode > 1)
    {
      goto L80;
    }
  /* 
   *...  build integs (describing block structure of matrix) 
   * 
   */
  i__1 = colapr_.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      integs[i__ * 3 + 2] = ncol;
      if (i__ < colapr_.n)
	{
	  goto L40;
	}
      integs[colapr_.n * 3 + 3] = ncol;
      lside = colord_.mstar;
      goto L60;
    L40:
      integs[i__ * 3 + 3] = colord_.mstar;
    L50:
      if (lside == colord_.mstar)
	{
	  goto L60;
	}
      if (colsid_.zeta[lside] >= xi[i__] + colout_.precis)
	{
	  goto L60;
	}
      ++lside;
      goto L50;
    L60:
      nrow = colord_.mstar + lside;
      /* L70: */
      integs[i__ * 3 + 1] = nrow;
    }
 L80:
  if (*mode == 2)
    {
      goto L90;
    }
  /* 
   *...  zero the matrices to be computed 
   * 
   */
  lw = colord_.kd * colord_.kd * colapr_.n;
  i__1 = lw;
  for (l = 1; l <= i__1; ++l)
    {
      /* L84: */
      w[l] = 0.;
    }
  /* 
   *...  the do loop 290 sets up the linear system of equations. 
   * 
   */
 L90:
  i__1 = colapr_.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       *...       construct a block of  a  and a corresponding piece of  rhs. 
       * 
       */
      xii = xi[i__];
      h__ = xi[i__ + 1] - xi[i__];
      nrow = integs[i__ * 3 + 1];
      /* 
       *...       go thru the ncomp collocation equations and side conditions 
       *...       in the i-th subinterval 
       * 
       */
    L100:
      if (colsid_.izeta > colord_.mstar)
	{
	  goto L140;
	}
      if (colsid_.zeta[colsid_.izeta - 1] > xii + colout_.precis)
	{
	  goto L140;
	}
      /* 
       *...       build equation for a side condition. 
       * 
       */
      if (*mode == 0)
	{
	  goto L110;
	}
      if (colnln_.iguess != 1)
	{
	  goto L102;
	}
      /* 
       *...       case where user provided current approximation 
       * 
       */
      *colnew_err = (*guess) (&xii, zval, dmval, args);
      if ( *colnew_err == FAIL ) return 0;
      goto L110;
      /* 
       *...       other nonlinear case 
       * 
       */
    L102:
      if (*mode != 1)
	{
	  goto L106;
	}
      nsp_colnew_approx (&iold, &xii, zval, at, colloc_.coef, &xiold[1],
			 &colapr_.nold, &z__[1], &dmz[1], &colord_.k,
			 &colord_.ncomp, &colord_.mmax, colord_.m,
			 &colord_.mstar, &c__2, dummy, &c__0);
      goto L110;
    L106:
      nsp_colnew_approx (&i__, &xii, zval, at, dummy, &xi[1], &colapr_.n,
			 &z__[1], &dmz[1], &colord_.k, &colord_.ncomp,
			 &colord_.mmax, colord_.m, &colord_.mstar, &c__1,
			 dummy, &c__0);
      /*    108  continue 
       */
      if (*mode == 3)
	{
	  goto L120;
	}
      /* 
       *...       find  rhs  boundary value. 
       * 
       */
    L110:
      *colnew_err =(*gsub) (&colsid_.izeta, zval, &gval, args);
      if ( *colnew_err == FAIL ) return 0;
      rhs[colapr_.ndmz + colsid_.izeta] = -gval;
      /*Computing 2nd power 
       */
      d__1 = gval;
      *rnorm += d__1 * d__1;
      if (*mode == 2)
	{
	  goto L130;
	}
      /* 
       *...       build a row of  a  corresponding to a boundary point 
       * 
       */
    L120:
      nsp_colnew_gderiv (&g[ig], &nrow, &colsid_.izeta, zval, dgz, &c__1,
			 dgsub,args, colnew_err);
      if (*colnew_err == FAIL)
	{
	  return 0;
	}
    L130:
      ++colsid_.izeta;
      goto L100;
      /* 
       *...       assemble collocation equations 
       * 
       */
    L140:
      i__2 = colord_.k;
      for (j = 1; j <= i__2; ++j)
	{
	  hrho = h__ * colloc_.rho[j - 1];
	  xcol = xii + hrho;
	  /* 
	   *...         this value corresponds to a collocation (interior) 
	   *...         point. build the corresponding  ncomp  equations. 
	   * 
	   */
	  if (*mode == 0)
	    {
	      goto L200;
	    }
	  if (colnln_.iguess != 1)
	    {
	      goto L160;
	    }
	  /* 
	   *...         use initial approximation provided by the user. 
	   * 
	   */
	  *colnew_err =(*guess) (&xcol, zval, &dmzo[irhs],args);
	  if ( *colnew_err == FAIL ) return 0;
	  goto L170;
	  /* 
	   *...         find  rhs  values 
	   * 
	   */
	L160:
	  if (*mode != 1)
	    {
	      goto L190;
	    }
	  nsp_colnew_approx (&iold, &xcol, zval, at, colloc_.coef, &xiold[1],
			     &colapr_.nold, &z__[1], &dmz[1], &colord_.k,
			     &colord_.ncomp, &colord_.mmax, colord_.m,
			     &colord_.mstar, &c__2, &dmzo[irhs], &c__1);
	  /* 
	   */
	L170:
	  *colnew_err =  (*fsub) (&xcol, zval, f,args);
	  if ( *colnew_err == FAIL ) return 0;

	  i__3 = colord_.ncomp;
	  for (jj = 1; jj <= i__3; ++jj)
	    {
	      value = dmzo[irhs] - f[jj - 1];
	      rhs[irhs] = -value;
	      /*Computing 2nd power 
	       */
	      d__1 = value;
	      *rnorm += d__1 * d__1;
	      ++irhs;
	      /* L180: */
	    }
	  goto L210;
	  /* 
	   *...         evaluate former collocation solution 
	   * 
	   */
	L190:
	  nsp_colnew_approx (&i__, &xcol, zval, &colbas_.acol[j * 28 - 28],
			     colloc_.coef, &xi[1], &colapr_.n, &z__[1],
			     &dmz[1], &colord_.k, &colord_.ncomp,
			     &colord_.mmax, colord_.m, &colord_.mstar,
			     &c__4, dummy, &c__0);
	  if (*mode == 3)
	    {
	      goto L210;
	    }
	  /* 
	   *...         fill in  rhs  values (and accumulate its norm). 
	   * 
	   */
	  *colnew_err =(*fsub) (&xcol, zval, f,args);
	  if ( *colnew_err == FAIL ) return 0;

	  i__3 = colord_.ncomp;
	  for (jj = 1; jj <= i__3; ++jj)
	    {
	      value = dmz[irhs] - f[jj - 1];
	      rhs[irhs] = -value;
	      /*Computing 2nd power 
	       */
	      d__1 = value;
	      *rnorm += d__1 * d__1;
	      ++irhs;
	      /* L195: */
	    }
	  goto L220;
	  /* 
	   *...         the linear case 
	   * 
	   */
	L200:
	  *colnew_err =(*fsub) (&xcol, zval, &rhs[irhs],args);
	  if ( *colnew_err == FAIL ) return 0;

	  irhs += colord_.ncomp;
	  /* 
	   *...         fill in ncomp rows of  w and v 
	   * 
	   */
	L210:
	  nsp_colnew_vwblok (&xcol, &hrho, &j, &w[iw], &v[iv], &ipvtw[idmz],
			     &colord_.kd, zval, df,
			     &colbas_.acol[j * 28 - 28], &dmzo[idmzo],
			     &colord_.ncomp,  dfsub, msing,args, colnew_err);
	  if (*colnew_err == FAIL)
	    {
	      return 0;
	    }
	  if (*msing != 0)
	    {
	      return 0;
	    }
	L220:
	  ;
	}
      /* 
       *...       build global bvp matrix  g 
       * 
       */
      if (*mode != 2)
	{
	  nsp_colnew_gblock (&h__, &g[ig], &nrow, &colsid_.izeta, &w[iw],
			     &v[iv], &colord_.kd, dummy, &deldmz[idmz],
			     &ipvtw[idmz], &c__1);
	}
      if (i__ < colapr_.n)
	{
	  goto L280;
	}
      colsid_.idum = colsid_.izeta;
    L240:
      if (colsid_.izeta > colord_.mstar)
	{
	  goto L290;
	}
      /* 
       *...       build equation for a side condition. 
       * 
       */
      if (*mode == 0)
	{
	  goto L250;
	}
      if (colnln_.iguess != 1)
	{
	  goto L245;
	}
      /* 
       *...       case where user provided current approximation 
       * 
       */
      *colnew_err =(*guess) (&colsid_.aright, zval, dmval,args);
      if ( *colnew_err == FAIL ) return 0;

      goto L250;
      /* 
       *...       other nonlinear case 
       * 
       */
    L245:
      if (*mode != 1)
	{
	  goto L246;
	}
      i__2 = colapr_.nold + 1;
      nsp_colnew_approx (&i__2, &colsid_.aright, zval, at, colloc_.coef,
			 &xiold[1], &colapr_.nold, &z__[1], &dmz[1],
			 &colord_.k, &colord_.ncomp, &colord_.mmax,
			 colord_.m, &colord_.mstar, &c__1, dummy, &c__0);
      goto L250;
    L246:
      i__2 = colapr_.n + 1;
      nsp_colnew_approx (&i__2, &colsid_.aright, zval, at, colloc_.coef,
			 &xi[1], &colapr_.n, &z__[1], &dmz[1], &colord_.k,
			 &colord_.ncomp, &colord_.mmax, colord_.m,
			 &colord_.mstar, &c__1, dummy, &c__0);
      /*    248  continue 
       */
      if (*mode == 3)
	{
	  goto L260;
	}
      /* 
       *...       find  rhs  boundary value. 
       * 
       */
    L250:
      *colnew_err =  (*gsub) (&colsid_.izeta, zval, &gval,args);
      if ( *colnew_err == FAIL ) return 0;

      rhs[colapr_.ndmz + colsid_.izeta] = -gval;
      /*Computing 2nd power 
       */
      d__1 = gval;
      *rnorm += d__1 * d__1;
      if (*mode == 2)
	{
	  goto L270;
	}
      /* 
       *...       build a row of  a  corresponding to a boundary point 
       * 
       */
    L260:
      i__2 = colsid_.izeta + colord_.mstar;
      nsp_colnew_gderiv (&g[ig], &nrow, &i__2, zval, dgz, &c__2,
			 dgsub,args, colnew_err);
      if (*colnew_err == FAIL)
	{
	  return 0;
	}
    L270:
      ++colsid_.izeta;
      goto L240;
      /* 
       *...       update counters -- i-th block completed 
       * 
       */
    L280:
      ig += nrow * ncol;
      iv += colord_.kd * colord_.mstar;
      iw += colord_.kd * colord_.kd;
      idmz += colord_.kd;
      if (*mode == 1)
	{
	  idmzo += colord_.kd;
	}
    L290:
      ;
    }
  /* 
   *...       assembly process completed 
   * 
   */
  if (*mode == 0 || *mode == 3)
    {
      goto L300;
    }
  *rnorm = sqrt (*rnorm / (double) (colapr_.nz + colapr_.ndmz));
  if (*mode != 2)
    {
      goto L300;
    }
  return 0;
  /* 
   *...  solve the linear system. 
   * 
   *...  matrix decomposition 
   * 
   */
 L300:
  nsp_colnew_fcblok (&g[1], &integs[4], &colapr_.n, &ipvtg[1], df, msing);
  /* 
   *...  check for singular matrix 
   * 
   */
  *msing = -(*msing);
  if (*msing != 0)
    {
      return 0;
    }
  /* 
   *...  perform forward and backward substitution for mode=4 only. 
   * 
   */
 L310:
  i__1 = colapr_.ndmz;
  for (l = 1; l <= i__1; ++l)
    {
      deldmz[l] = rhs[l];
      /* L311: */
    }
  iz = 1;
  idmz = 1;
  iw = 1;
  izet = 1;
  i__1 = colapr_.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nrow = integs[i__ * 3 + 1];
      colsid_.izeta = nrow + 1 - colord_.mstar;
      if (i__ == colapr_.n)
	{
	  colsid_.izeta = colsid_.idum;
	}
    L322:
      if (izet == colsid_.izeta)
	{
	  goto L324;
	}
      delz[iz - 1 + izet] = rhs[colapr_.ndmz + izet];
      ++izet;
      goto L322;
    L324:
      h__ = xi[i__ + 1] - xi[i__];
      nsp_colnew_gblock (&h__, &g[1], &nrow, &colsid_.izeta, &w[iw], &v[1],
			 &colord_.kd, &delz[iz], &deldmz[idmz], &ipvtw[idmz],
			 &c__2);
      iz += colord_.mstar;
      idmz += colord_.kd;
      iw += colord_.kd * colord_.kd;
      if (i__ < colapr_.n)
	{
	  goto L320;
	}
    L326:
      if (izet > colord_.mstar)
	{
	  goto L320;
	}
      delz[iz - 1 + izet] = rhs[colapr_.ndmz + izet];
      ++izet;
      goto L326;
    L320:
      ;
    }
  /* 
   *...  perform forward and backward substitution for mode=0,2, or 3. 
   * 
   */
  nsp_colnew_sbblok (&g[1], &integs[4], &colapr_.n, &ipvtg[1], &delz[1]);
  /* 
   *...  finaly find deldmz 
   * 
   */
  nsp_colnew_dmzsol (&colord_.kd, &colord_.mstar, &colapr_.n, &v[1],
		     &delz[1], &deldmz[1]);
  /* 
   */
  if (*mode != 1)
    {
      return 0;
    }
  i__1 = colapr_.ndmz;
  for (l = 1; l <= i__1; ++l)
    {
      dmz[l] = dmzo[l];
      /* L321: */
    }
  iz = 1;
  idmz = 1;
  iw = 1;
  izet = 1;
  i__1 = colapr_.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nrow = integs[i__ * 3 + 1];
      colsid_.izeta = nrow + 1 - colord_.mstar;
      if (i__ == colapr_.n)
	{
	  colsid_.izeta = colsid_.idum;
	}
    L330:
      if (izet == colsid_.izeta)
	{
	  goto L340;
	}
      z__[iz - 1 + izet] = dgz[izet - 1];
      ++izet;
      goto L330;
    L340:
      h__ = xi[i__ + 1] - xi[i__];
      nsp_colnew_gblock (&h__, &g[1], &nrow, &colsid_.izeta, &w[iw], df,
			 &colord_.kd, &z__[iz], &dmz[idmz], &ipvtw[idmz],
			 &c__2);
      iz += colord_.mstar;
      idmz += colord_.kd;
      iw += colord_.kd * colord_.kd;
      if (i__ < colapr_.n)
	{
	  goto L350;
	}
    L342:
      if (izet > colord_.mstar)
	{
	  goto L350;
	}
      z__[iz - 1 + izet] = dgz[izet - 1];
      ++izet;
      goto L342;
    L350:
      ;
    }
  nsp_colnew_sbblok (&g[1], &integs[4], &colapr_.n, &ipvtg[1], &z__[1]);
  /* 
   *...  finaly find dmz 
   * 
   */
  nsp_colnew_dmzsol (&colord_.kd, &colord_.mstar, &colapr_.n, &v[1],
		     &z__[1], &dmz[1]);
  /* 
   */
  return 0;
}				/* lsyslv_ */



/* 
 * 
 *  purpose: 
 * 
 *     construct a collocation matrix row according to mode: 
 *     mode = 1  -  a row corresponding to a initial condition 
 *                  (i.e. at the left end of the subinterval). 
 *     mode = 2  -  a row corresponding to a final condition. 
 * 
 *  variables: 
 * 
 *     gi     - the sub-block of the global bvp matrix in 
 *              which the equations are to be formed. 
 *     nrow   - no. of rows in gi. 
 *     irow   - the row in gi to be used for equations. 
 *     zval   - z(xi) 
 *     dg     - the derivatives of the side condition. 
 * 
 */

static int
nsp_colnew_gderiv (double *gi, int *nrow, int *irow, double *zval,
		   double *dgz, int *mode, DGsub dgsub, void *args, int *colnew_err)
{
  /* System generated locals */
  int gi_dim1, gi_offset, i__1;

  /* Local variables */
  int j;
  double dg[40], dot;
  /* Parameter adjustments */
  gi_dim1 = *nrow;
  gi_offset = gi_dim1 + 1;
  gi -= gi_offset;
  --zval;
  --dgz;

  /* Function Body */
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      /* L10: */
      dg[j - 1] = 0.;
    }
  /* 
   *...  evaluate jacobian dg 
   * 
   */
  *colnew_err =  (*dgsub) (&colsid_.izeta, &zval[1], dg,args);
  if ( *colnew_err == FAIL ) return 0;

  /* 
   *...  evaluate  dgz = dg * zval  once for a new mesh 
   * 
   */
  if (colnln_.nonlin == 0 || colnln_.iter > 0)
    {
      goto L30;
    }
  dot = 0.;
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      /* L20: */
      dot += dg[j - 1] * zval[j];
    }
  dgz[colsid_.izeta] = dot;
  /* 
   *...  branch according to  m o d e 
   * 
   */
 L30:
  if (*mode == 2)
    {
      goto L50;
    }
  /* 
   *...  provide coefficients of the j-th linearized side condition. 
   *...  specifically, at x=zeta(j) the j-th side condition reads 
   *...  dg(1)*z(1) + ... +dg(mstar)*z(mstar) + g = 0 
   * 
   * 
   *...  handle an initial condition 
   * 
   */
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      gi[*irow + j * gi_dim1] = dg[j - 1];
      /* L40: */
      gi[*irow + (colord_.mstar + j) * gi_dim1] = 0.;
    }
  return 0;
  /* 
   *...  handle a final condition 
   * 
   */
 L50:
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      gi[*irow + j * gi_dim1] = 0.;
      /* L60: */
      gi[*irow + (colord_.mstar + j) * gi_dim1] = dg[j - 1];
    }
  return 0;
}				/* gderiv_ */


/* 
 *     construct a group of  ncomp  rows of the matrices  wi  and  vi. 
 *     corresponding to an interior collocation point. 
 * 
 * 
 *  variables: 
 * 
 *     xcol   - the location of the collocation point. 
 *     jj     - xcol is the jj-th of k collocation points 
 *              in the i-th subinterval. 
 *     wi,vi  - the i-th block of the collocation matrix 
 *              before parameter condensation. 
 *     kd     - no. of rows in vi and wi . 
 *     zval   - z(xcol) 
 *     df     - the jacobian at xcol . 
 *     jcomp  - counter for the component being dealt with. 
 * 
 */


int
nsp_colnew_vwblok (double *xcol, double *hrho, int *jj, double *wi,
		   double *vi, int *ipvtw, int *kd, double *zval, double *df,
		   double *acol, double *dmzo, int *ncomp, DFsub dfsub,
		   int *msing, void *args, int *colnew_err)
{
  /* System generated locals */
  int wi_dim1, wi_offset, vi_dim1, vi_offset, df_dim1, df_offset, i__1, i__2,
    i__3, i__4;

  /* Local variables */
  double fact, basm[5];
  int jcol;
  int j, l;
  int jcomp, i0, i1, i2;
  double ha[28] /* was [7][4] */ ;
  int id;
  double bl;
  int mj, jn, ll, ir, jv, jw, iw, lp1, jdf;
  double ajl;

  /* Parameter adjustments */
  --ipvtw;
  vi_dim1 = *kd;
  vi_offset = vi_dim1 + 1;
  vi -= vi_offset;
  wi_dim1 = *kd;
  wi_offset = wi_dim1 + 1;
  wi -= wi_offset;
  --zval;
  acol -= 8;
  --dmzo;
  df_dim1 = *ncomp;
  df_offset = df_dim1 + 1;
  df -= df_offset;

  /* Function Body */
  if (*jj > 1)
    {
      goto L30;
    }
  i__1 = *kd;
  for (id = 1; id <= i__1; ++id)
    {
      wi[id + id * wi_dim1] = 1.;
      /* L10: */
    }
  /* 
   *...  calculate local basis 
   * 
   */
 L30:
  fact = 1.;
  i__1 = colord_.mmax;
  for (l = 1; l <= i__1; ++l)
    {
      fact = fact * *hrho / (double) l;
      basm[l - 1] = fact;
      i__2 = colord_.k;
      for (j = 1; j <= i__2; ++j)
	{
	  ha[j + l * 7 - 8] = fact * acol[j + l * 7];
	  /* L150: */
	}
    }
  /* 
   *... zero jacobian 
   * 
   */
  i__2 = colord_.mstar;
  for (jcol = 1; jcol <= i__2; ++jcol)
    {
      i__1 = *ncomp;
      for (ir = 1; ir <= i__1; ++ir)
	{
	  /* L40: */
	  df[ir + jcol * df_dim1] = 0.;
	}
    }
  /* 
   *...  build ncomp rows for interior collocation point x. 
   *...  the linear expressions to be constructed are: 
   *...   (m(id)) 
   *...  u     -  df(id,1)*z(1) - ... - df(id,mstar)*z(mstar) 
   *...   id 
   *...  for id = 1 to ncomp. 
   * 
   */
  *colnew_err =  (*dfsub) (xcol, &zval[1], &df[df_offset],args);
  if ( *colnew_err == FAIL ) return 0;
  i0 = (*jj - 1) * *ncomp;
  i1 = i0 + 1;
  i2 = i0 + *ncomp;
  /* 
   *...  evaluate  dmzo = dmz - df * zval  once for a new mesh 
   * 
   */
  if (colnln_.nonlin == 0 || colnln_.iter > 0)
    {
      goto L60;
    }
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      fact = -zval[j];
      i__2 = *ncomp;
      for (id = 1; id <= i__2; ++id)
	{
	  dmzo[i0 + id] += fact * df[id + j * df_dim1];
	  /* L50: */
	}
    }
  /* 
   *...  loop over the  ncomp  expressions to be set up for the 
   *...  current collocation point. 
   * 
   */
 L60:
  i__2 = colord_.mstar;
  for (j = 1; j <= i__2; ++j)
    {
      i__1 = *ncomp;
      for (id = 1; id <= i__1; ++id)
	{
	  vi[i0 + id + j * vi_dim1] = df[id + j * df_dim1];
	  /* L70: */
	}
    }
  jn = 1;
  i__1 = *ncomp;
  for (jcomp = 1; jcomp <= i__1; ++jcomp)
    {
      mj = colord_.m[jcomp - 1];
      jn += mj;
      i__2 = mj;
      for (l = 1; l <= i__2; ++l)
	{
	  jv = jn - l;
	  jw = jcomp;
	  i__3 = colord_.k;
	  for (j = 1; j <= i__3; ++j)
	    {
	      ajl = -ha[j + l * 7 - 8];
	      i__4 = i2;
	      for (iw = i1; iw <= i__4; ++iw)
		{
		  wi[iw + jw * wi_dim1] += ajl * vi[iw + jv * vi_dim1];
		  /* L80: */
		}
	      /* L90: */
	      jw += *ncomp;
	    }
	  lp1 = l + 1;
	  if (l == mj)
	    {
	      goto L130;
	    }
	  i__3 = mj;
	  for (ll = lp1; ll <= i__3; ++ll)
	    {
	      jdf = jn - ll;
	      bl = basm[ll - l - 1];
	      i__4 = i2;
	      for (iw = i1; iw <= i__4; ++iw)
		{
		  vi[iw + jv * vi_dim1] += bl * vi[iw + jdf * vi_dim1];
		  /* L100: */
		}
	      /* L110: */
	    }
	L130:
	  ;
	}
      /* L140: */
    }
  if (*jj < colord_.k)
    {
      return 0;
    }
  /* 
   *  ...decompose the wi block and solve for the mstar columns of vi 
   * 
   * 
   *...  do parameter condensation 
   * 
   */
  *msing = 0;
  C2F(dgefa) (&wi[wi_offset], kd, kd, &ipvtw[1], msing);
  /* 
   *...   check for singularity 
   * 
   */
  if (*msing != 0)
    {
      return 0;
    }
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      C2F(dgesl) (&wi[wi_offset], kd, kd, &ipvtw[1],
		  &vi[j * vi_dim1 + 1], &c__0);
    }
  return 0;
}

/* 
 * 
 *     construct collocation matrix rows according to mode: 
 *     mode = 1  -  a group of  mstar    rows corresponding 
 *                  an interior mesh interval. 
 *          = 2  -  corresponding right hand side 
 * 
 *  variables: 
 * 
 *     h      - the  local stepsize. 
 *     gi     - the sub-block of the collocation matrix in 
 *              which the equations are to be formed. 
 *     wi     - the sub-block of noncondensed collocation equations, 
 *              left-hand side part. 
 *     vi     - the sub-block of noncondensed collocation equations, 
 *              right-hand side part. 
 *     rhsdmz - the inhomogenous term of the uncondensed collocation 
 *              equations. 
 *     rhsz   - the inhomogenous term of the condensed collocation 
 *              equations. 
 *     nrow   - no. of rows in gi. 
 *     irow   - the first row in gi to be used for equations. 
 */


static int
nsp_colnew_gblock (double *h__, double *gi, int *nrow, int *irow, double *wi,
		   double *vi, int *kd, double *rhsz, double *rhsdmz,
		   int *ipvtw, int *mode)
{
  /* System generated locals */
  int gi_dim1, gi_offset, vi_dim1, vi_offset, i__1, i__2, i__3, i__4;

  /* Local variables */
  double fact, basm[5];
  int jcol;
  double rsum;
  int j, l;
  int icomp, jcomp;
  double hb[28] /* was [7][4] */ ;
  int id, jd, mj, ll, ir, ind;

  /* Parameter adjustments */
  gi_dim1 = *nrow;
  gi_offset = gi_dim1 + 1;
  gi -= gi_offset;
  --wi;
  vi_dim1 = *kd;
  vi_offset = vi_dim1 + 1;
  vi -= vi_offset;
  --rhsz;
  --rhsdmz;
  --ipvtw;

  /* Function Body */
  fact = 1.;
  basm[0] = 1.;
  i__1 = colord_.mmax;
  for (l = 1; l <= i__1; ++l)
    {
      fact = fact * *h__ / (double) l;
      basm[l] = fact;
      i__2 = colord_.k;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L20: */
	  hb[j + l * 7 - 8] = fact * colbas_.b[j + l * 7 - 8];
	}
      /* L30: */
    }
  /* 
   *...  branch according to  m o d e 
   * 
   */
  switch (*mode)
    {
    case 1:
      goto L40;
    case 2:
      goto L110;
    }
  /* 
   *...  set right gi-block to identity 
   * 
   */
 L40:
  i__1 = colord_.mstar;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = colord_.mstar;
      for (ir = 1; ir <= i__2; ++ir)
	{
	  gi[*irow - 1 + ir + j * gi_dim1] = 0.;
	  /* L50: */
	  gi[*irow - 1 + ir + (colord_.mstar + j) * gi_dim1] = 0.;
	}
      /* L60: */
      gi[*irow - 1 + j + (colord_.mstar + j) * gi_dim1] = 1.;
    }
  /* 
   *...  compute the block gi 
   * 
   */
  ir = *irow;
  i__1 = colord_.ncomp;
  for (icomp = 1; icomp <= i__1; ++icomp)
    {
      mj = colord_.m[icomp - 1];
      ir += mj;
      i__2 = mj;
      for (l = 1; l <= i__2; ++l)
	{
	  id = ir - l;
	  i__3 = colord_.mstar;
	  for (jcol = 1; jcol <= i__3; ++jcol)
	    {
	      ind = icomp;
	      rsum = 0.;
	      i__4 = colord_.k;
	      for (j = 1; j <= i__4; ++j)
		{
		  rsum -= hb[j + l * 7 - 8] * vi[ind + jcol * vi_dim1];
		  /* L70: */
		  ind += colord_.ncomp;
		}
	      gi[id + jcol * gi_dim1] = rsum;
	      /* L80: */
	    }
	  jd = id - *irow;
	  i__3 = l;
	  for (ll = 1; ll <= i__3; ++ll)
	    {
	      gi[id + (jd + ll) * gi_dim1] -= basm[ll - 1];
	      /* L85: */
	    }
	  /* L90: */
	}
      /* L100: */
    }
  return 0;
  /* 
   *...  compute the appropriate piece of  rhsz 
   * 
   */
 L110:
  C2F(dgesl) (&wi[1], kd, kd, &ipvtw[1], &rhsdmz[1], &c__0);
  ir = *irow;
  i__1 = colord_.ncomp;
  for (jcomp = 1; jcomp <= i__1; ++jcomp)
    {
      mj = colord_.m[jcomp - 1];
      ir += mj;
      i__2 = mj;
      for (l = 1; l <= i__2; ++l)
	{
	  ind = jcomp;
	  rsum = 0.;
	  i__3 = colord_.k;
	  for (j = 1; j <= i__3; ++j)
	    {
	      rsum += hb[j + l * 7 - 8] * rhsdmz[ind];
	      /* L120: */
	      ind += colord_.ncomp;
	    }
	  rhsz[ir - l] = rsum;
	  /* L130: */
	}
      /* L140: */
    }
  return 0;
}				/* gblock_ */

/* 
 *---------------------------------------------------------------------- 
 *                            p a r t  4 
 *              polynomial and service routines 
 *---------------------------------------------------------------------- 
 * 
 */
int nsp_colnew_appsln(double *x, double *z__, double *fspace, int *ispace)
{
  double a[28];
  int i__;
  double dummy[1];
  int is4, is5, is6;

  /* 
****************************************************************** 
* 
*    purpose 
* 
*          set up a standard call to  approx  to evaluate the 
*          approximate solution  z = z( u(x) )  at a point x 
*          (it has been computed by a call to  colnew ). 
*          the parameters needed for  approx  are retrieved 
*          from the work arrays  ispace  and  fspace . 
* 
****************************************************************** 
* 
*     IMPLICIT REAL*8 (A-H,O-Z) 
*/
  /* Parameter adjustments */
  --ispace;
  --fspace;
  --z__;

  /* Function Body */
  is6 = ispace[6];
  is5 = ispace[1] + 2;
  is4 = is5 + ispace[4] * (ispace[1] + 1);
  i__ = 1;
  nsp_colnew_approx (&i__, x, &z__[1], a, &fspace[is6], &fspace[1],
		     &ispace[1], &fspace[is5], &fspace[is4], &ispace[2],
		     &ispace[3], &ispace[5], &ispace[8], &ispace[4], &c__2,
		     dummy, &c__0);
  return 0;
}				/* appsln_ */

static int
nsp_colnew_approx (int *i__, double *x, double *zval, double *a, double *coef,
		   double *xi, int *n, double *z__, double *dmz, int *k,
		   int *ncomp, int *mmax, int *m, int *mstar, int *mode,
		   double *dmval, int *modm)
{
  /* Format strings */

  /* System generated locals */
  int i__1, i__2, i__3;

  /* Local variables */
  double fact;
  int idmz;
  double zsum;
  int j, l;
  double s;
  int ileft, jcomp, lb;
  double bm[4], dm[7];
  int mj, ll, ir, iz, iright, ind;


  /* 
*********************************************************************** 
* 
*  purpose 
*                                   (1)       (m1-1)     (mncomp-1) 
*          evaluate z(u(x))=(u (x),u (x),...,u  (x),...,u  (x)      ) 
*                             1     1         1          mncomp 
*          at one point x. 
* 
*  variables 
*    a      - array of mesh independent rk-basis coefficients 
*    basm   - array of mesh dependent monomial coefficients 
*    xi     - the current mesh (having n subintervals) 
*    z      - the current solution vector 
*    dmz    - the array of mj-th derivatives of the current solution 
*    mode   - determines the amount of initialization needed 
*           = 4  forms z(u(x)) using z, dmz and ha 
*           = 3  as in =4, but computes local rk-basis 
*           = 2  as in =3, but determines i such that 
*                      xi(i) .le. x .lt. xi(i+1) (unless x=xi(n+1)) 
*           = 1  retrieve  z=z(u(x(i)))  directly 
* 
*********************************************************************** 
* 
*     IMPLICIT REAL*8 (A-H,O-Z) 
* 
* 
*/
  /* Parameter adjustments */
  --dmval;
  --m;
  --dmz;
  --z__;
  --xi;
  --coef;
  a -= 8;
  --zval;

  /* Function Body */
  switch (*mode)
    {
    case 1:
      goto L10;
    case 2:
      goto L30;
    case 3:
      goto L80;
    case 4:
      goto L90;
    }
  /* 
   *...  mode = 1 , retrieve  z( u(x) )  directly for x = xi(i). 
   * 
   */
 L10:
  *x = xi[*i__];
  iz = (*i__ - 1) * *mstar;
  i__1 = *mstar;
  for (j = 1; j <= i__1; ++j)
    {
      ++iz;
      zval[j] = z__[iz];
      /* L20: */
    }
  return 0;
  /* 
   *...  mode = 2 ,  locate i so  xi(i) .le. x .lt. xi(i+1) 
   * 
   */
 L30:
  if (*x >= xi[1] - colout_.precis && *x <= xi[*n + 1] + colout_.precis)
    {
      goto L40;
    }
  if (colout_.iprint < 1)
    {
      Sciprintf("****** DOMAIN ERROR IN APPROX ******\n");
      Sciprintf("X = %20.10f, ALEFT =%20.10f, ARIGHT =%20.10f \n",
		(*x),xi[1],xi[*n + 1]);
    }
  if (*x < xi[1])
    {
      *x = xi[1];
    }
  if (*x > xi[*n + 1])
    {
      *x = xi[*n + 1];
    }
 L40:
  if (*i__ > *n || *i__ < 1)
    {
      *i__ = (*n + 1) / 2;
    }
  ileft = *i__;
  if (*x < xi[ileft])
    {
      goto L60;
    }
  i__1 = *n;
  for (l = ileft; l <= i__1; ++l)
    {
      *i__ = l;
      if (*x < xi[l + 1])
	{
	  goto L80;
	}
      /* L50: */
    }
  goto L80;
 L60:
  iright = ileft - 1;
  i__1 = iright;
  for (l = 1; l <= i__1; ++l)
    {
      *i__ = iright + 1 - l;
      if (*x >= xi[*i__])
	{
	  goto L80;
	}
      /* L70: */
    }
  /* 
   *...  mode = 2 or 3 , compute mesh independent rk-basis. 
   * 
   */
 L80:
  s = (*x - xi[*i__]) / (xi[*i__ + 1] - xi[*i__]);
  nsp_colnew_rkbas (&s, &coef[1], k, mmax, &a[8], dm, modm);
  /* 
   *...  mode = 2, 3, or 4 , compute mesh dependent rk-basis. 
   * 
   */
 L90:
  bm[0] = *x - xi[*i__];
  i__1 = *mmax;
  for (l = 2; l <= i__1; ++l)
    {
      bm[l - 1] = bm[0] / (double) l;
      /* L95: */
    }
  /* 
   *...  evaluate  z( u(x) ). 
   * 
   *100 continue 
   */
  ir = 1;
  iz = (*i__ - 1) * *mstar + 1;
  idmz = (*i__ - 1) * *k * *ncomp;
  i__1 = *ncomp;
  for (jcomp = 1; jcomp <= i__1; ++jcomp)
    {
      mj = m[jcomp];
      ir += mj;
      iz += mj;
      i__2 = mj;
      for (l = 1; l <= i__2; ++l)
	{
	  ind = idmz + jcomp;
	  zsum = 0.;
	  i__3 = *k;
	  for (j = 1; j <= i__3; ++j)
	    {
	      zsum += a[j + l * 7] * dmz[ind];
	      /* L110: */
	      ind += *ncomp;
	    }
	  i__3 = l;
	  for (ll = 1; ll <= i__3; ++ll)
	    {
	      lb = l + 1 - ll;
	      /* L120: */
	      zsum = zsum * bm[lb - 1] + z__[iz - ll];
	    }
	  /* L130: */
	  zval[ir - l] = zsum;
	}
      /* L140: */
    }
  if (*modm == 0)
    {
      return 0;
    }
  /* 
   *...  for modm = 1 evaluate  dmval(j) = mj-th derivative of uj. 
   * 
   */
  i__1 = *ncomp;
  for (jcomp = 1; jcomp <= i__1; ++jcomp)
    {
      /* L150: */
      dmval[jcomp] = 0.;
    }
  ++idmz;
  i__1 = *k;
  for (j = 1; j <= i__1; ++j)
    {
      fact = dm[j - 1];
      i__2 = *ncomp;
      for (jcomp = 1; jcomp <= i__2; ++jcomp)
	{
	  dmval[jcomp] += fact * dmz[idmz];
	  ++idmz;
	  /* L160: */
	}
      /* L170: */
    }
  return 0;
  /*-------------------------------------------------------------------- 
   */
}				/* approx_ */

static int
nsp_colnew_rkbas (double *s, double *coef, int *k, int *m, double *rkb,
		  double *dm, int *mode)
{
  /* System generated locals */
  int coef_dim1, coef_offset, i__1, i__2, i__3;

  /* Local variables */
  int i__, j, l;
  double p, t[10];
  int lb, kpm1;

  /* 
*********************************************************************** 
* 
*  purpose 
*          evaluate mesh independent runge-kutta basis for given s 
* 
*  variables 
*    s      - argument, i.e. the relative position for which 
*             the basis is to be evaluated ( 0. .le. s .le. 1. ). 
*    coef   - precomputed derivatives of the basis 
*    k      - number of collocatin points per subinterval 
*    m      - maximal order of the differential equation 
*    rkb    - the runge-kutta basis (0-th to (m-1)-th derivatives ) 
*    dm     - basis elements for m-th derivative 
* 
*********************************************************************** 
* 
*     IMPLICIT REAL*8 (A-H,O-Z) 
* 
*/
  /* Parameter adjustments */
  coef_dim1 = *k;
  coef_offset = coef_dim1 + 1;
  coef -= coef_offset;
  rkb -= 8;
  --dm;

  /* Function Body */
  if (*k == 1)
    {
      goto L70;
    }
  kpm1 = *k + *m - 1;
  i__1 = kpm1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L10: */
      t[i__ - 1] = *s / (double) i__;
    }
  i__1 = *m;
  for (l = 1; l <= i__1; ++l)
    {
      lb = *k + l + 1;
      i__2 = *k;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  p = coef[i__ * coef_dim1 + 1];
	  i__3 = *k;
	  for (j = 2; j <= i__3; ++j)
	    {
	      p = p * t[lb - j - 1] + coef[j + i__ * coef_dim1];
	      /* L20: */
	    }
	  rkb[i__ + l * 7] = p;
	  /* L30: */
	}
      /* L40: */
    }
  if (*mode == 0)
    {
      return 0;
    }
  i__1 = *k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      p = coef[i__ * coef_dim1 + 1];
      i__2 = *k;
      for (j = 2; j <= i__2; ++j)
	{
	  /* L50: */
	  p = p * t[*k + 1 - j - 1] + coef[j + i__ * coef_dim1];
	}
      dm[i__] = p;
      /* L60: */
    }
  return 0;
 L70:
  rkb[8] = 1.;
  dm[1] = 1.;
  return 0;
}				/* rkbas_ */

static int
nsp_colnew_vmonde (double *rho, double *coef, int *k)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int ifac, i__, j, km1, kmi;

  /* 
*********************************************************************** 
* 
*  purpose 
*         solve vandermonde system v * x = e 
*         with  v(i,j) = rho(j)**(i-1)/(i-1)! . 
* 
*********************************************************************** 
* 
* 
*/
  /* Parameter adjustments */
  --coef;
  --rho;

  /* Function Body */
  if (*k == 1)
    {
      return 0;
    }
  km1 = *k - 1;
  i__1 = km1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      kmi = *k - i__;
      i__2 = kmi;
      for (j = 1; j <= i__2; ++j)
	{
	  coef[j] = (coef[j + 1] - coef[j]) / (rho[j + i__] - rho[j]);
	  /* L10: */
	}
    }
  /* 
   */
  ifac = 1;
  i__2 = km1;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      kmi = *k + 1 - i__;
      i__1 = kmi;
      for (j = 2; j <= i__1; ++j)
	{
	  /* L30: */
	  coef[j] -= rho[j + i__ - 1] * coef[j - 1];
	}
      coef[kmi] = (double) ifac *coef[kmi];
      ifac *= i__;
      /* L40: */
    }
  coef[1] = (double) ifac *coef[1];
  return 0;
}				/* vmonde_ */


/* 
 * 
 *  purpose 
 *          determine highest order (piecewise constant) derivatives 
 *          of the current collocation solution 
 * 
 *  variables 
 *    hi     - the stepsize, hi = xi(i+1) - xi(i) 
 *    dmz    - vector of mj-th derivative of the solution 
 *    uhigh  - the array of highest order (piecewise constant) 
 *             derivatives of the approximate solution on 
 *             (xi(i),xi(i+1)), viz, 
 *                         (k+mj-1) 
 *             uhigh(j) = u   (x)    on (xi(i),xi(i+1)) 
 *                         j 
 * 
 */

static int nsp_colnew_horder (int *i__, double *uhigh, double *hi, double *dmz,
			      int *ncomp, int *k)
{
  /* System generated locals */
  int i__1, i__2;


  /* Local variables */
  double fact;
  int idmz, j, id;
  double dn;
  int kin;

  /* Parameter adjustments */
  --dmz;
  --uhigh;

  /* Function Body */
  i__1 = *k - 1;
  dn = 1. / pow (*hi, i__1);
  /* 
   *...  loop over the ncomp solution components 
   * 
   */
  i__1 = *ncomp;
  for (id = 1; id <= i__1; ++id)
    {
      uhigh[id] = 0.;
      /* L10: */
    }
  kin = 1;
  idmz = (*i__ - 1) * *k * *ncomp + 1;
  i__1 = *k;
  for (j = 1; j <= i__1; ++j)
    {
      fact = dn * colloc_.coef[kin - 1];
      i__2 = *ncomp;
      for (id = 1; id <= i__2; ++id)
	{
	  uhigh[id] += fact * dmz[idmz];
	  ++idmz;
	  /* L20: */
	}
      kin += *k;
      /* L30: */
    }
  return 0;
}				/* horder_ */

/* 
 *         compute dmz in a blockwise manner 
 *         dmz(i) = dmz(i)  +  v(i) * z(i), i = 1,...,n 
 * 
 */

static int 
nsp_colnew_dmzsol (int *kd, int *mstar, int *n, double *v, double *z__,
		   double *dmz)
{
  /* System generated locals */
  int v_dim1, v_offset, dmz_dim1, dmz_offset, i__1, i__2, i__3;

  /* Local variables */
  double fact;
  int i__, j, l, jz;

  /* Parameter adjustments */
  dmz_dim1 = *kd;
  dmz_offset = dmz_dim1 + 1;
  dmz -= dmz_offset;
  v_dim1 = *kd;
  v_offset = v_dim1 + 1;
  v -= v_offset;
  --z__;

  /* Function Body */
  jz = 1;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *mstar;
      for (j = 1; j <= i__2; ++j)
	{
	  fact = z__[jz];
	  i__3 = *kd;
	  for (l = 1; l <= i__3; ++l)
	    {
	      dmz[l + i__ * dmz_dim1] += fact * v[l + jz * v_dim1];
	      /* L10: */
	    }
	  ++jz;
	  /* L20: */
	}
      /* L30: */
    }
  return 0;
}			

/*---------------------------------------------------------------------- 
 *                           p a r t  5 
 *         we list here a modified (column oriented, faster) 
 *         version of the package solveblok of de boor - weiss [5]. 
 *         we also give a listing of the linpack 
 *         routines dgefa und dgesl used by colnew. 
 *---------------------------------------------------------------------- 
 * 
 */


/* 
 * 
 *    calls subroutines  factrb  and  shiftb . 
 * 
 *    fcblok  supervises the plu factorization with pivoting of 
 *    scaled rows of the almost block diagonal matrix stored in the 
 *    arrays  bloks  and  integs . 
 * 
 *    factrb = subprogram which carries out steps 1,...,last of gauss 
 *           elimination (with pivoting) for an individual block. 
 *    shiftb = subprogram which shifts the remaining rows to the top of 
 *           the next block 
 * 
 *    parameters 
 *     bloks   an array that initially contains the almost block diago- 
 *           nal matrix  a  to be factored, and on return contains the 
 *           computed factorization of  a . 
 *     integs  an int array describing the block structure of  a . 
 *     nbloks  the number of blocks in  a . 
 *     ipivot  an int array of dimension   sum (integs(3,n) ; n=1, 
 *           ...,nbloks) which, on return, contains the pivoting stra- 
 *           tegy used. 
 *     scrtch  work area required, of length  Max(integs(1,n) ; n=1, 
 *           ...,nbloks). 
 *     info    output parameter; 
 *           = 0  in case matrix was found to be nonsingular. 
 *           otherwise, 
 *           = n if the pivot element in the nth gauss step is zero. 
 * 
 * 
 */

static int
nsp_colnew_fcblok (double *bloks, int *integs, int *nbloks, int *ipivot,
		   double *scrtch, int *info)
{
  int ncol, last, nrow, i__, index;
  int indexn, indexx;
  /* Parameter adjustments */
  --bloks;
  integs -= 4;
  --ipivot;
  --scrtch;

  /* Function Body */
  *info = 0;
  indexx = 1;
  indexn = 1;
  i__ = 1;
  /* 
   *...  loop over the blocks.  i  is loop index 
   * 
   */
 L10:
  index = indexn;
  nrow = integs[i__ * 3 + 1];
  ncol = integs[i__ * 3 + 2];
  last = integs[i__ * 3 + 3];
  /* 
   *...       carry out elimination on the i-th block until next block 
   *...       enters, i.e., for columns 1,...,last  of i-th block. 
   * 
   */
  nsp_colnew_factrb (&bloks[index], &ipivot[indexx], &scrtch[1], &nrow, &ncol,
		     &last, info);
  /* 
   *...       check for having reached a singular block or the last block 
   * 
   */
  if (*info != 0)
    {
      goto L20;
    }
  if (i__ == *nbloks)
    {
      return 0;
    }
  ++i__;
  indexn = nrow * ncol + index;
  indexx += last;
  /* 
   *...       put the rest of the i-th block onto the next block 
   * 
   */
  nsp_colnew_shiftb (&bloks[index], &nrow, &ncol, &last, &bloks[indexn],
		     &integs[i__ * 3 + 1], &integs[i__ * 3 + 2]);
  goto L10;
 L20:
  *info = *info + indexx - 1;
  return 0;
}				/* fcblok_ */

/* 
 * 
 *    adapted from p.132 of  element.numer.analysis  by conte-de boor 
 * 
 *    constructs a partial plu factorization, corresponding to steps 
 *     1,..., last   in gauss elimination, for the matrix  w  of 
 *     order ( nrow ,  ncol ), using pivoting of scaled rows. 
 * 
 *    parameters 
 *      w       contains the (nrow,ncol) matrix to be partially factored 
 *              on input, and the partial factorization on output. 
 *      ipivot  an int array of length last containing a record of 
 *              the pivoting strategy used; explicit interchanges 
 *              are used for pivoting. 
 *      d       a work array of length nrow used to store row sizes 
 *              temporarily. 
 *      nrow    number of rows of w. 
 *      ncol    number of columns of w. 
 *      last    number of elimination steps to be carried out. 
 *      info    on output, zero if the matrix is found to be non- 
 *              singular, in case a zero pivot was encountered in row 
 *              n,  info = n on output. 
 * 
 */

static int 
nsp_colnew_factrb (double *w, int *ipivot, double *d__, int *nrow, int *ncol,
		   int *last, int *info)
{
  /* System generated locals */
  int w_dim1, w_offset, i__1, i__2;
  double d__1, d__2, d__3;

  /* Local variables */
  int i__, j, k, l;
  double s, t, colmax;
  int kp1;

  /* Parameter adjustments */
  --d__;
  --ipivot;
  w_dim1 = *nrow;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  i__1 = *nrow;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      d__[i__] = 0.;
      /* L10: */
    }
  i__1 = *ncol;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *nrow;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = d__[i__], d__3 = (d__1 = w[i__ + j * w_dim1], Abs (d__1));
	  d__[i__] = Max (d__2, d__3);
	  /* L20: */
	}
    }
  /* 
   *...  gauss elimination with pivoting of scaled rows, loop over 
   *...  k=1,.,last 
   * 
   */
  k = 1;
  /* 
   *...  as pivot row for k-th step, pick among the rows not yet used, 
   *...  i.e., from rows  k ,..., nrow , the one whose k-th entry 
   *...  (compared to the row size) is largest. then, if this row 
   *...  does not turn out to be row k, interchange row k with this 
   *...  particular row and redefine ipivot(k). 
   * 
   */
 L30:
  if (d__[k] == 0.)
    {
      goto L90;
    }
  if (k == *nrow)
    {
      goto L80;
    }
  l = k;
  kp1 = k + 1;
  colmax = (d__1 = w[k + k * w_dim1], Abs (d__1)) / d__[k];
  /* 
   *...       find the (relatively) largest pivot 
   * 
   */
  i__2 = *nrow;
  for (i__ = kp1; i__ <= i__2; ++i__)
    {
      if ((d__1 = w[i__ + k * w_dim1], Abs (d__1)) <= colmax * d__[i__])
	{
	  goto L40;
	}
      colmax = (d__1 = w[i__ + k * w_dim1], Abs (d__1)) / d__[i__];
      l = i__;
    L40:
      ;
    }
  ipivot[k] = l;
  t = w[l + k * w_dim1];
  s = d__[l];
  if (l == k)
    {
      goto L50;
    }
  w[l + k * w_dim1] = w[k + k * w_dim1];
  w[k + k * w_dim1] = t;
  d__[l] = d__[k];
  d__[k] = s;
 L50:
  /* 
   *...       if pivot element is too small in absolute value, declare 
   *...       matrix to be noninvertible and quit. 
   * 
   */
  if (Abs (t) + d__[k] <= d__[k])
    {
      goto L90;
    }
  /* 
   *...       otherwise, subtract the appropriate multiple of the pivot 
   *...       row from remaining rows, i.e., the rows (k+1),..., (nrow) 
   *...       to make k-th entry zero. save the multiplier in its place. 
   *...       for high performance do this operations column oriented. 
   * 
   */
  t = -1. / t;
  i__2 = *nrow;
  for (i__ = kp1; i__ <= i__2; ++i__)
    {
      /* L60: */
      w[i__ + k * w_dim1] *= t;
    }
  i__2 = *ncol;
  for (j = kp1; j <= i__2; ++j)
    {
      t = w[l + j * w_dim1];
      if (l == k)
	{
	  goto L62;
	}
      w[l + j * w_dim1] = w[k + j * w_dim1];
      w[k + j * w_dim1] = t;
    L62:
      if (t == 0.)
	{
	  goto L70;
	}
      i__1 = *nrow;
      for (i__ = kp1; i__ <= i__1; ++i__)
	{
	  /* L64: */
	  w[i__ + j * w_dim1] += w[i__ + k * w_dim1] * t;
	}
    L70:
      ;
    }
  k = kp1;
  /* 
   *...       check for having reached the next block. 
   * 
   */
  if (k <= *last)
    {
      goto L30;
    }
  return 0;
  /* 
   *...  if  last  .eq. nrow , check now that pivot element in last row 
   *...  is nonzero. 
   * 
   */
 L80:
  if ((d__1 =
       w[*nrow + *nrow * w_dim1], Abs (d__1)) + d__[*nrow] > d__[*nrow])
    {
      return 0;
    }
  /* 
   *...  singularity flag set 
   * 
   */
 L90:
  *info = k;
  return 0;
}				/* factrb_ */

/* 
 * 
 *    shifts the rows in current block, ai, not used as pivot rows, if 
 *    any, i.e., rows  (last+1),..., (nrowi), onto the first mmax = 
 *     = nrow-last  rows of the next block, ai1, with column last+j of 
 *     ai  going to column j , j=1,...,jmax=ncoli-last. the remaining 
 *    columns of these rows of ai1 are zeroed out. 
 * 
 *                               picture 
 * 
 *         original situation after         results in a new block i+1 
 *         last = 2 columns have been       created and ready to be 
 *         done in factrb (assuming no      factored by next factrb 
 *         interchanges of rows)            call. 
 *                     1 
 *                x  x 1x  x  x           x  x  x  x  x 
 *                     1 
 *                0  x 1x  x  x           0  x  x  x  x 
 *    block i          1                       --------------- 
 *    nrowi = 4   0  0 1x  x  x           0  0 1x  x  x  0  01 
 *    ncoli = 5        1                       1             1 
 *    last = 2    0  0 1x  x  x           0  0 1x  x  x  0  01 
 *    -------------------------------          1             1   new 
 *                     1x  x  x  x  x          1x  x  x  x  x1  block 
 *                     1                       1             1   i+1 
 *    block i+1        1x  x  x  x  x          1x  x  x  x  x1 
 *    nrowi1= 5        1                       1             1 
 *    ncoli1= 5        1x  x  x  x  x          1x  x  x  x  x1 
 *    -------------------------------          1-------------1 
 *                     1 
 * 
 */

static int
nsp_colnew_shiftb (double *ai, int *nrowi, int *ncoli, int *last, double *ai1,
		   int *nrowi1, int *ncoli1)
{
  /* System generated locals */
  int ai_dim1, ai_offset, ai1_dim1, ai1_offset, i__1, i__2;

  /* Local variables */
  int jmax, mmax, j, m, jmaxp1;

  /* Parameter adjustments */
  ai_dim1 = *nrowi;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ai1_dim1 = *nrowi1;
  ai1_offset = ai1_dim1 + 1;
  ai1 -= ai1_offset;

  /* Function Body */
  mmax = *nrowi - *last;
  jmax = *ncoli - *last;
  if (mmax < 1 || jmax < 1)
    {
      return 0;
    }
  /* 
   *...  put the remainder of block i into ai1 
   * 
   */
  i__1 = jmax;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = mmax;
      for (m = 1; m <= i__2; ++m)
	{
	  /* L10: */
	  ai1[m + j * ai1_dim1] = ai[*last + m + (*last + j) * ai_dim1];
	}
    }
  if (jmax == *ncoli1)
    {
      return 0;
    }
  /* 
   *...  zero out the upper right corner of ai1 
   * 
   */
  jmaxp1 = jmax + 1;
  i__2 = *ncoli1;
  for (j = jmaxp1; j <= i__2; ++j)
    {
      i__1 = mmax;
      for (m = 1; m <= i__1; ++m)
	{
	  /* L20: */
	  ai1[m + j * ai1_dim1] = 0.;
	}
    }
  return 0;
}				/* shiftb_ */

/* 
 * 
 *    calls subroutines  subfor  and  subbak . 
 * 
 *    supervises the solution (by forward and backward substitution) of 
 *    the linear system  a*x = b  for x, with the plu factorization of 
 *    a  already generated in  fcblok .  individual blocks of 
 *    equations are solved via  subfor  and  subbak . 
 * 
 *   parameters 
 *      bloks, integs, nbloks, ipivot    are as on return from fcblok. 
 *      x       on input: the right hand side, in dense storage 
 *              on output: the solution vector 
 * 
 *...  forward substitution pass 
 * 
 */

static int 
nsp_colnew_sbblok (double *bloks, int *integs, int *nbloks, int *ipivot, double *x)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int ncol, last, nrow, i__, j, index;
  int indexx;
  int nbp1;

  /* Parameter adjustments */
  --bloks;
  integs -= 4;
  --ipivot;
  --x;

  /* Function Body */
  index = 1;
  indexx = 1;
  i__1 = *nbloks;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nrow = integs[i__ * 3 + 1];
      last = integs[i__ * 3 + 3];
      nsp_colnew_subfor (&bloks[index], &ipivot[indexx], &nrow, &last,
			 &x[indexx]);
      index = nrow * integs[i__ * 3 + 2] + index;
      /* L10: */
      indexx += last;
    }
  /* 
   *...  back substitution pass 
   * 
   */
  nbp1 = *nbloks + 1;
  i__1 = *nbloks;
  for (j = 1; j <= i__1; ++j)
    {
      i__ = nbp1 - j;
      nrow = integs[i__ * 3 + 1];
      ncol = integs[i__ * 3 + 2];
      last = integs[i__ * 3 + 3];
      index -= nrow * ncol;
      indexx -= last;
      /* L20: */
      nsp_colnew_subbak (&bloks[index], &nrow, &ncol, &last, &x[indexx]);
    }
  return 0;
}				/* sbblok_ */

/* 
 * 
 *    carries out the forward pass of substitution for the current 
 *    block, i.e., the action on the right side corresponding to the 
 *    elimination carried out in  factrb  for this block. 
 * 
 *   parameters 
 *      w, ipivot, nrow, last  are as on return from factrb. 
 *      x(j)  is expected to contain, on input, the right side of j-th 
 *            equation for this block, j=1,...,nrow. 
 *      x(j)  contains, on output, the appropriately modified right 
 *            side of equation (j) in this block, j=1,...,last and 
 *            for j=last+1,...,nrow. 
 * 
 * 
 * 
 */

static int nsp_colnew_subfor (double *w, int *ipivot, int *nrow, int *last, double *x)
{
  /* System generated locals */
  int w_dim1, w_offset, i__1, i__2;

  /* Local variables */
  int i__, k;
  double t;
  int lstep, ip, kp1;

  /* Parameter adjustments */
  --x;
  --ipivot;
  w_dim1 = *nrow;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  if (*nrow == 1)
    {
      return 0;
    }
  /*Computing MIN 
   */
  i__1 = *nrow - 1;
  lstep = Min (i__1, *last);
  i__1 = lstep;
  for (k = 1; k <= i__1; ++k)
    {
      kp1 = k + 1;
      ip = ipivot[k];
      t = x[ip];
      x[ip] = x[k];
      x[k] = t;
      if (t == 0.)
	{
	  goto L20;
	}
      i__2 = *nrow;
      for (i__ = kp1; i__ <= i__2; ++i__)
	{
	  x[i__] += w[i__ + k * w_dim1] * t;
	  /* L10: */
	}
    L20:
      ;
    }
  return 0;
}				/* subfor_ */

/* 
 * 
 *    carries out backsubstitution for current block. 
 * 
 *   parameters 
 *      w, ipivot, nrow, ncol, last  are as on return from factrb. 
 *      x(1),...,x(ncol)  contains, on input, the right side for the 
 *              equations in this block after backsubstitution has been 
 *              carried up to but not including equation (last). 
 *              means that x(j) contains the right side of equation (j) 
 *              as modified during elimination, j=1,...,last, while 
 *              for j .gt. last, x(j) is already a component of the 
 *              solution vector. 
 *      x(1),...,x(ncol) contains, on output, the components of the 
 *              solution corresponding to the present block. 
 * 
 */

static int
nsp_colnew_subbak (double *w, int *nrow, int *ncol, int *last, double *x)
{
  int w_dim1, w_offset, i1, i2;
  int i, j, k;
  double t;
  int kb, km1, lm1, lp1;

  /* Parameter adjustments */
  --x;
  w_dim1 = *nrow;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  lp1 = *last + 1;
  if (lp1 > *ncol)
    {
      goto L30;
    }
  i1 = *ncol;
  for (j = lp1; j <= i1; ++j)
    {
      t = -x[j];
      if (t == 0.)
	{
	  goto L20;
	}
      i2 = *last;
      for (i = 1; i <= i2; ++i)
	{
	  /* L10: */
	  x[i] += w[i + j * w_dim1] * t;
	}
    L20:
      ;
    }
 L30:
  if (*last == 1)
    {
      goto L60;
    }
  lm1 = *last - 1;
  i1 = lm1;
  for (kb = 1; kb <= i1; ++kb)
    {
      km1 = *last - kb;
      k = km1 + 1;
      x[k] /= w[k + k * w_dim1];
      t = -x[k];
      if (t == 0.)
	{
	  goto L50;
	}
      i2 = km1;
      for (i = 1; i <= i2; ++i)
	{
	  /* L40: */
	  x[i] += w[i + k * w_dim1] * t;
	}
    L50:
      ;
    }
 L60:
  x[1] /= w[w_dim1 + 1];
  return 0;
}	

