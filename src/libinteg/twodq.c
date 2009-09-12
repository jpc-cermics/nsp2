#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"
#include "nsp/blas.h"

/* Table of constant values */

static int c__9 = 9;
static int c__1 = 1;
static double c_b28 = .5;
static double c_b58 = 1.5;

typedef int (*Twodqf) (const double *x,const double * y,double *fvect, int *n28);
typedef int (*Twodqg) (const double *x,const double * y,const int *n );

static int twodq_greater (const double *a,const double *b,const int *nwds);
static int tdq_hinitd (int *, int *, int *, int *);
static int tdq_hinitu (int *, int *, int *, int *);
static int tdq_hpacc (int *, int *, double *, int *,  int *, double *, int *);
static int tdq_hpdel (int *, int *, double *, int *, int *, Twodqg, int *);
static int tdq_hpins (int *, int *, double *, int *, int *, double *, Twodqg);
static int tdq_tridv (double *, double *, double *, double *, int *);
static int tdq_hpgro (int *, int *, double *, int *,   int *,Twodqg, int *);

static int tdq_hpins (int *nmax, int *nwds, double *data, int *n,
		      int *t, double *xnode, Twodqg hpfun);

static int tdq_tridv (double *node, double *node1, double *node2,
		      double *coef, int *rank);

static int tdq_lqm0vect (Twodqf, double *, double *,double *, double *, int *,int *);

static int tdq_lqm1vect (Twodqf, double *, double *, double *, double *, int *,int *);

/****BEGIN PROLOGUE  TWODQ 
 ****DATE WRITTEN   840518   (YYMMDD) 
 ****REVISION DATE  840518   (YYMMDD) 
 ****CATEGORY NO.  D1I 
 ****KEYWORDS  QUADRATURE,TWO DIMENSIONAL,ADAPTIVE,CUBATURE 
 ****AUTHOR KAHANER,D.K.,N.B.S. 
 *         RECHARD,O.W.,N.B.S. 
 *         BARNHILL,ROBERT,UNIV. OF UTAH 
 ****PURPOSE  To compute the two-dimensional integral of a function 
 *           F over a region consisting of N triangles. 
 ****DESCRIPTION 
 * 
 *  This subroutine computes the two-dimensional integral of a 
 *  function F over a region consisting of N triangles. 
 *  A total error estimate is obtained and compared with a 
 *  tolerance - TOL - that is provided as input to the subroutine. 
 *  The error tolerance is treated as either relative or absolute 
 *  depending on the input value of IFLAG.  A 'Local Quadrature 
 *  Module' is applied to each input triangle and estimates of the 
 *  total integral and the total error are computed.  The local 
 *  quadrature module is either subroutine LQM0 or subroutine 
 *  LQM1 and the choice between them is determined by the 
 *  value of the input variable ICLOSE. 
 * 
 *  If the total error estimate exceeds the tolerance, the triangle 
 *  with the largest absolute error is divided into two triangles 
 *  by a median to its longest side.  The local quadrature module 
 *  is then applied to each of the subtriangles to obtain new 
 *  estimates of the integral and the error.  This process is 
 *  repeated until either (1) the error tolerance is satisfied, 
 *  (2) the number of triangles generated exceeds the input 
 *  parameter MAXTRI, (3) the number of integrand evaluations 
 *  exceeds the input parameter MEVALS, or (4) the subroutine 
 *  senses that roundoff error is beginning to contaminate 
 *  the result. 
 * 
 *  The user must specify MAXTRI, the maximum number of triangles 
 *  in the final triangulation of the region, and provide two 
 *  storage arrays - DATA and IWORK - whose sizes are at least 
 *  9*MAXTRI and 2*MAXTRI respectively.  The user must also 
 *  specify MEVALS, the maximum number of function evaluations 
 *  to be allowed.  This number will be effective in limiting 
 *  the computation only if it is less than 92*MAXTRI when LQM1 
 *  is specified or 56*MAXTRI when LQM0 is specified. 
 * 
 *  After the subroutine has returned to the calling program 
 *  with output values, it can be called again with a smaller 
 *  value of TOL and/or a different value of MEVALS.  The tolerance 
 *  can also be changed from relative to absolute 
 *  or vice-versa by changing IFLAG.  Unless 
 *  the parameters NU and ND are reset to zero the subroutine 
 *  will restart with the final set of triangles and output 
 *  values from the previous call. 
 * 
 *  ARGUMENTS: 
 * 
 *  F function subprogram defining the integrand F(u,v); 
 *    the actual name for F needs to be declared EXTERNAL 
 *    by the calling program. 
 * 
 *  N the number of input triangles. 
 * 
 *  X a 3 by N array containing the abscissae of the vertices 
 *    of the N triangles. 
 * 
 *  Y a 3 by N array containing the ordinates of the vertices 
 *    of the N triangles 
 * 
 *  TOL the desired bound on the error.  If IFLAG=0 on input, 
 *      TOL is interpreted as a bound on the relative error; 
 *      if IFLAG=1, the bound is on the absolute error. 
 * 
 *  ICLOSE an int parameter that determines the selection 
 *         of LQM0 or LQM1.  If ICLOSE=1 then LQM1 is used. 
 *         Any other value of ICLOSE causes LQM0 to be used. 
 *         LQM0 uses function values only at interior points of 
 *         the triangle.  LQM1 is usually more accurate than LQM0 
 *         but involves evaluating the integrand at more points 
 *         including some on the boundary of the triangle.  It 
 *         will usually be better to use LQM1 unless the integrand 
 *         has singularities on the boundary of the triangle. 
 * 
 *  MAXTRI The maximum number of triangles that are allowed 
 *         to be generated by the computation. 
 * 
 *  MEVALS  The maximum number of function evaluations allowed. 
 * 
 *  RESULT output of the estimate of the integral. 
 * 
 *  ERROR output of the estimate of the absolute value of the 
 *        total error. 
 * 
 *  NU an int variable used for both input and output.   Must 
 *     be set to 0 on first call of the subroutine.  Subsequent 
 *     calls to restart the subroutine should use the previous 
 *     output value. 
 * 
 *  ND an int variable used for both input and output.  Must 
 *     be set to 0 on first call of the subroutine.  Subsequent 
 *     calls to restart the subroutine should use the previous 
 *     output value. 
 * 
 *  NEVALS  The actual number of function evaluations performed. 
 * 
 *  IFLAG on input: 
 *       IFLAG=0 means TOL is a bound on the relative error; 
 *       IFLAG=1 means TOL is a bound on the absolute error; 
 *       any other input value for IFLAG causes the subroutine 
 *       to return immediately with IFLAG set equal to 9. 
 * 
 *       on output: 
 *       IFLAG=0 means normal termination; 
 *       IFLAG=1 means termination for lack of space to divide 
 *               another triangle; 
 *       IFLAG=2 means termination because of roundoff noise 
 *       IFLAG=3 means termination with relative error <= 
 *               5.0* machine epsilon; 
 *       IFLAG=4 means termination because the number of function 
 *               evaluations has exceeded MEVALS. 
 *       IFLAG=9 means termination because of error in input flag 
 * 
 *  DATA a one dimensional array of length >= 9*MAXTRI 
 *       passed to the subroutine by the calling program.  It is 
 *       used by the subroutine to store information 
 *       about triangles used in the quadrature. 
 * 
 *  IWORK  a one dimensional int array of length >= 2*MAXTRI 
 *         passed to the subroutine by the calling program. 
 *         It is used by the subroutine to store pointers 
 *         to the information in the DATA array. 
 * 
 * 
 *  The information for each triangle is contained in a nine word 
 *  record consisting of the error estimate, the estimate of the 
 *  integral, the coordinates of the three vertices, and the area. 
 *  These records are stored in the DATA array 
 *  that is passed to the subroutine.  The storage is organized 
 *  into two heaps of length NU and ND respectively.  The first heap 
 *  contains those triangles for which the error exceeds 
 *  epsabs*a/ATOT where epsabs is a bound on the absolute error 
 *  derived from the input tolerance (which may refer to relative 
 *  or absolute error), a is the area of the triangle, and ATOT 
 *  is the total area of all triangles.  The second heap contains 
 *  those triangles for which the error is less than or equal to 
 *  epsabs*a/ATOT.  At the top of each heap is the triangle with 
 *  the largest absolute error. 
 * 
 *  Pointers into the heaps are contained in the array IWORK. 
 *  Pointers to the first heap are contained 
 *  between IWORK(1) and IWORK(NU).  Pointers to the second 
 *  heap are contained between IWORK(MAXTRI+1) and 
 *  IWORK(MAXTRI+ND).  The user thus has access to the records 
 *  stored in the DATA array through the pointers in IWORK. 
 *  For example, the following two DO loops will print out 
 *  the records for each triangle in the two heaps: 
 * 
 *    DO 10 I=1,NU 
 *      PRINT*,(DATA(IWORK(I)+J),J=0,8) 
 *   10  CONTINUE 
 *    DO 20 I=1,ND 
 *      PRINT*,(DATA(IWORK(MAXTRI+I)+J),J=0,8 
 *   20  CONTINUE 
 * 
 *  When the total number of triangles is equal to 
 *  MAXTRI, the program attempts to remove a triangle from the 
 *  bottom of the second heap and continue.  If the second heap 
 *  is empty, the program returns with the current estimates of 
 *  the integral and the error and with IFLAG set equal to 1. 
 *  Note that in this case the actual number of triangles 
 *  processed may exceed MAXTRI and the triangles stored in 
 *  the DATA array may not constitute a complete triangulation 
 *  of the region. 
 * 
 *  The following sample program will calculate the integral of 
 *  cos(x+y) over the square (0.,0.),(1.,0.),(1.,1.),(0.,1.) and 
 *  print out the values of the estimated integral, the estimated 
 *  error, the number of function evaluations, and IFLAG. 
 * 
 *    Double precision X(3,2),Y(3,2),DATA(450),RES,ERR 
 *    INT IWORK(100),NU,ND,NEVALS,IFLAG 
 *    EXTERNAL F 
 *    X(1,1)=0. 
 *    Y(1,1)=0. 
 *    X(2,1)=1. 
 *    Y(2,1)=0. 
 *    X(3,1)=1. 
 *    Y(3,1)=1. 
 *    X(1,2)=0. 
 *    Y(1,2)=0. 
 *    X(2,2)=1. 
 *    Y(2,2)=1. 
 *    X(3,2)=0. 
 *    Y(3,2)=1. 
 *    NU=0 
 *    ND=0 
 *    IFLAG=1 
 *    CALL TWODQ(F,2,X,Y,1.D-04,1,50,4000,RES,ERR,NU,ND, 
 *   *  NEVALS,IFLAG,DATA,IWORK) 
 *    PRINT*,RES,ERR,NEVALS,IFLAG 
 *    END 
 *    DOUBLE PRECISION FUNCTION F(X,Y) 
 *    DOUBLE PRECISION X,Y 
 *    F=COS(X+Y) 
 *    RETURN 
 *    END 
 * 
 ****REFERENCES  (NONE) 
 * 
 ****ROUTINES CALLED  HINITD,HINITU,HPACC,HPDEL,HPINS,LQM0,LQM1, 
 *                   TRIDV,DLAMCH 
 ****END PROLOGUE  TWODQ 
 */

int
nsp_twodq (Twodqf f, int *n, double *x, double *y, double *tol,
	   int *iclose, int *maxtri, int *mevals, double *result,
	   double *error, int *nu, int *nd, int *nevals, int *iflag,
	   double *data, int *iwork, int *vectflag, int *stat)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double node[9];
  int full;
  static double atot;
  double node1[9], node2[9], a, e;
  int i__, j;
  double emach;
  double r__, u[3], v[3];
  double epsabs;
  int rndcnt;
  double newerr, newres, fadd;

  /* Parameter adjustments */
  y -= 4;
  x -= 4;
  --data;
  --iwork;

  /* Function Body */
  emach = nsp_dlamch ("p");
  /* 
   *     If heaps are empty, apply LQM to each input triangle and 
   *     place all of the data on the second heap. 
   * 
   */
  if (*nu + *nd == 0)
    {
      tdq_hinitu (maxtri, &c__9, nu, &iwork[1]);
      tdq_hinitd (maxtri, &c__9, nd, &iwork[*maxtri + 1]);
      atot = 0.;
      *result = 0.;
      *error = 0.;
      rndcnt = 0;
      *nevals = 0;
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  for (j = 1; j <= 3; ++j)
	    {
	      u[j - 1] = x[j + i__ * 3];
	      v[j - 1] = y[j + i__ * 3];
	    }
	  a = (d__1 =
	       u[0] * v[1] + u[1] * v[2] + u[2] * v[0] - u[0] * v[2] -
	       u[1] * v[0] - u[2] * v[1], Abs (d__1)) * .5;
	  atot += a;
	  if (*iclose == 1)
	    {
	      tdq_lqm1vect ( f, u, v, &r__, &e, vectflag, stat);
	      if (*stat != 0)
		{
		  return 0;
		}
	      *nevals += 46;
	    }
	  else
	    {
	      tdq_lqm0vect ( f, u, v, &r__, &e, vectflag, stat);
	      if (*stat != 0)
		{
		  return 0;
		}
	      *nevals += 28;
	    }
	  *result += r__;
	  *error += e;
	  node[0] = e;
	  node[1] = r__;
	  node[2] = x[i__ * 3 + 1];
	  node[3] = y[i__ * 3 + 1];
	  node[4] = x[i__ * 3 + 2];
	  node[5] = y[i__ * 3 + 2];
	  node[6] = x[i__ * 3 + 3];
	  node[7] = y[i__ * 3 + 3];
	  node[8] = a;
	  tdq_hpins (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		     node, twodq_greater);
	}
    }
  /* 
   *     Check that input tolerance is consistent with 
   *     machine epsilon. 
   * 
   */
  if (*iflag == 0)
    {
      if (*tol <= emach * 5.)
	{
	  *tol = emach * 5.;
	  fadd = 3.;
	}
      else
	{
	  fadd = 0.;
	}
      epsabs = *tol * Abs (*result);
    }
  else if (*iflag == 1)
    {
      if (*tol <= emach * 5. * Abs (*result))
	{
	  epsabs = emach * 5. * Abs (*result);
	}
      else
	{
	  fadd = 0.;
	  epsabs = *tol;
	}
    }
  else
    {
      *iflag = 9;
      return 0;
    }
  /* 
   *     Adjust the second heap on the basis of the current 
   *     value of epsabs. 
   * 
   */
 L2:
  if (*nd == 0)
    {
      goto L40;
    }
  j = *nd;
 L3:
  if (j == 0)
    {
      goto L40;
    }
  tdq_hpacc (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1], node,
	     &j);
  if (node[0] > epsabs * node[8] / atot)
    {
      tdq_hpins (maxtri, &c__9, &data[1], nu, &iwork[1], node,
		 twodq_greater);
      tdq_hpdel (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 twodq_greater, &j);
      if (j > *nd)
	{
	  --j;
	}
    }
  else
    {
      --j;
    }
  goto L3;
  /* 
   *     Beginning of main loop from here to end 
   * 
   */
 L40:
  if (*nevals >= *mevals)
    {
      *iflag = 4;
      return 0;
    }
  if (*error <= epsabs)
    {
      if (*iflag == 0)
	{
	  if (*error <= Abs (*result) * *tol)
	    {
	      *iflag = (int) fadd;
	      return 0;
	    }
	  else
	    {
	      epsabs = Abs (*result) * *tol;
	      goto L2;
	    }
	}
      else
	{
	  if (*error <= *tol)
	    {
	      *iflag = 0;
	      return 0;
	    }
	  else if (*error <= emach * 5. * Abs (*result))
	    {
	      *iflag = 3;
	      return 0;
	    }
	  else
	    {
	      epsabs = emach * 5. * Abs (*result);
	      goto L2;
	    }
	}
    }
  /* 
   *     If there are too many triangles and second heap 
   *     is not empty remove bottom triangle from second 
   *     heap.  If second heap is empty return with iflag 
   *     set to 1 or 4. 
   * 
   */
  if (*nu + *nd >= *maxtri)
    {
      full = TRUE;
      if (*nd > 0)
	{
	  iwork[*nu + 1] = iwork[*maxtri + *nd];
	  --(*nd);
	}
      else
	{
	  *iflag = 1;
	  return 0;
	}
    }
  else
    {
      full = FALSE;
    }
  /* 
   *     Find triangle with largest error, divide it in 
   *     two, and apply LQM to each half. 
   * 
   */
  if (*nd == 0)
    {
      tdq_hpacc (maxtri, &c__9, &data[1], nu, &iwork[1], node, &c__1);
      tdq_hpdel (maxtri, &c__9, &data[1], nu, &iwork[1],
		 twodq_greater, &c__1);
    }
  else if (*nu == 0)
    {
      tdq_hpacc (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 node, &c__1);
      tdq_hpdel (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 twodq_greater, &c__1);
    }
  else if (data[iwork[1]] >= data[iwork[*maxtri + 1]])
    {
      if (full)
	{
	  iwork[*maxtri + *nd + 2] = iwork[*nu];
	}
      tdq_hpacc (maxtri, &c__9, &data[1], nu, &iwork[1], node, &c__1);
      tdq_hpdel (maxtri, &c__9, &data[1], nu, &iwork[1],
		 twodq_greater, &c__1);
    }
  else
    {
      if (full)
	{
	  iwork[*nu + 2] = iwork[*maxtri + *nd];
	}
      tdq_hpacc (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 node, &c__1);
      tdq_hpdel (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 twodq_greater, &c__1);
    }
  tdq_tridv (node, node1, node2, &c_b28, &c__1);
  for (j = 1; j <= 3; ++j)
    {
      u[j - 1] = node1[j * 2];
      v[j - 1] = node1[(j << 1) + 1];
    }
  if (*iclose == 1)
    {
      tdq_lqm1vect ( f, u, v, &node1[1], node1, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      *nevals += 46;
    }
  else
    {
      tdq_lqm0vect ( f, u, v, &node1[1], node1, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      *nevals += 28;
    }
  for (j = 1; j <= 3; ++j)
    {
      u[j - 1] = node2[j * 2];
      v[j - 1] = node2[(j << 1) + 1];
    }
  if (*iclose == 1)
    {
      tdq_lqm1vect ( f, u, v, &node2[1], node2, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      *nevals += 46;
    }
  else
    {
      tdq_lqm0vect ( f, u, v, &node2[1], node2, vectflag, stat);
      if (*stat != 0)
	{
	  return 0;
	}
      *nevals += 28;
    }
  newerr = node1[0] + node2[0];
  newres = node1[1] + node2[1];
  if (newerr > node[0] * .99)
    {
      if ((d__1 = node[1] - newres, Abs (d__1)) <= Abs (newres) * 1e-4)
	{
	  ++rndcnt;
	}
    }
  *result = *result - node[1] + newres;
  *error = *error - node[0] + newerr;
  if (node1[0] > node1[8] * epsabs / atot)
    {
      tdq_hpins (maxtri, &c__9, &data[1], nu, &iwork[1], node1,
		 twodq_greater);
    }
  else
    {
      tdq_hpins (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 node1, twodq_greater);
    }
  if (node2[0] > node2[8] * epsabs / atot)
    {
      tdq_hpins (maxtri, &c__9, &data[1], nu, &iwork[1], node2,
		 twodq_greater);
    }
  else
    {
      tdq_hpins (maxtri, &c__9, &data[1], nd, &iwork[*maxtri + 1],
		 node2, twodq_greater);
    }
  if (rndcnt >= 20)
    {
      *iflag = 2;
      return 0;
    }
  if (*iflag == 0)
    {
      if (epsabs < *tol * .5 * Abs (*result))
	{
	  epsabs = *tol * Abs (*result);
	  j = *nu;
	L5:
	  if (j == 0)
	    {
	      goto L40;
	    }
	  tdq_hpacc (maxtri, &c__9, &data[1], nu, &iwork[1], node, &j);
	  if (node[0] <= epsabs * node[8] / atot)
	    {
	      tdq_hpins (maxtri, &c__9, &data[1], nd,
			 &iwork[*maxtri + 1], node, twodq_greater);
	      tdq_hpdel (maxtri, &c__9, &data[1], nu, &iwork[1],
			 twodq_greater, &j);
	      if (j > *nu)
		{
		  --j;
		}
	    }
	  else
	    {
	      --j;
	    }
	  goto L5;
	}
    }
  goto L40;
}				/* twodq_ */


static int twodq_greater (const double *a,const double *b,const int *nwds)
{
  return a[0] > b[0];
}

/*  PURPOSE 
 *        THIS ROUTINE INITIALIZES THE HEAP PROGRAMS WITH T(NMAX) 
 *        POINTING TO THE TOP OF THE HEAP. 
 *        IT IS CALLED ONCE AT THE START OF EACH NEW CALCULATION. 
 *  INPUT 
 *        NMAX=MAXIMUM NUMBER OF NODES ALLOWED BY USER 
 *        NWDS=NUMBER OF WORDS PER NODE 
 *  OUTPUT 
 *        N=CURRENT NUMBER OF NODES IN HEAP = 0. 
 *        T=INT ARRAY OF POINTERS TO POTENTIAL HEAP NODES. 
 * 
 */


static int
tdq_hinitd (int *nmax, int *nwds, int *n, int *t)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;

  /* Parameter adjustments */
  --t;

  /* Function Body */
  i__1 = *nmax;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L1: */
      t[i__] = (*nmax - i__) * *nwds + 1;
    }
  *n = 0;
  return 0;
}				/* hinitd_ */

/*       H E A P  PACKAGE 
 *         A COLLECTION OF PROGRAMS WHICH MAINTAIN A HEAP DATA 
 *         STRUCTURE.  BY CALLING THESE SUBROUTINES IT IS POSSIBLE TO 
 *         INSERT, DELETE, AND ACCESS AN EXISTING HEAP OR TO BUILD A 
 *         NEW HEAP FROM AN UNORDERED COLLECTION OF NODES. THE HEAP 
 *         FUNCTION IS AN ARGUMENT TO THE SUBROUTINES ALLOWING VERY 
 *         GENERAL ORGANIZATIONS. 
 *           THE USER MUST DECIDE ON THE MAXIMUM NUMBER OF NODES 
 *         ALLOWED AND DIMENSION THE REAL ARRAY DATA AND THE INT 
 *         ARRAY T USED INTERNALLY BY THE PACKAGE.  THESE VARIABLES ARE 
 *         THEN PASSED THROUGH THE CALL SEQUENCE BETWEEN THE HEAP 
 *         PROGRAMS BUT ARE NOT IN GENERAL ACCESSED BY THE USER.  HE 
 *         MUST ALSO PROVIDE A HEAP FUNCTION WHOSE NAME MUST BE INCLUD- 
 *         ED IN AN EXTERNAL STATEMENT IN THE USER PROGRAM WHICH CALLS 
 *         THE HEAP SUBROUTINES.  TWO SIMPLE HEAP FUNCTIONS ARE 
 *         PROVIDED WITH THE PACKAGE. 
 * 
 * 
 *  PURPOSE 
 *        THIS ROUTINE INITIALIZES THE HEAP PROGRAMS WITH T(1) 
 *        POINTING TO THE TOP OF THE HEAP. 
 *        IT IS CALLED ONCE AT THE START OF EACH NEW CALCULATION 
 *  INPUT 
 *        NMAX = MAXIMUM NUMBER OF NODES ALLOWED BY USER. 
 *        NWDS = NUMBER OF WORDS PER NODE 
 *  OUTPUT 
 *        N = CURRENT NUMBER OF NODES IN HEAP = 0. 
 *        T = INT ARRAY OF POINTERS TO POTENTIAL HEAP NODES. 
 * 
 */

static int
tdq_hinitu (int *nmax, int *nwds, int *n, int *t)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;

  /* Parameter adjustments */
  --t;

  /* Function Body */
  i__1 = *nmax;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L1: */
      t[i__] = (i__ - 1) * *nwds + 1;
    }
  *n = 0;
  return 0;
}				/* hinitu_ */


/* 
 *  PURPOSE 
 *         TO ACCESS THE K-TH NODE OF THE HEAP, 
 *         1 .LE. K .LE. N .LE. NMAX 
 *  INPUT 
 *       NMAX = MAXIMUM NUMBER OF NODES ALLOWED BY USER. 
 *       DATA = WORK AREA FOR STORING NODES. 
 *       N = CURRENT NUMBER OF NODES IN THE HEAP. 
 *       T = INT ARRAY OF POINTERS TO HEAP NODES. 
 *       XNODE = A REAL ARRAY, NWDS WORDS LONG, IN WHICH NODAL IN- 
 *         FORMATION WILL BE INSERTED. 
 *       K = THE INDEX OF THE NODE TO BE FOUND AND INSERTED INTO 
 *               XNODE. 
 * 
 *  OUTPUT 
 *       XNODE =  A REAL ARRAY.    CONTAINS IN XNODE(1),...,XNODE(NWDS) 
 *         THE ELEMENTS OF THE K-TH NODE. 
 * 
 */

static int
tdq_hpacc (int *nmax, int *nwds, double *data, int *n, int *t,
	   double *xnode, int *k)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, j, ipj;

  /* Parameter adjustments */
  --xnode;
  --t;
  --data;

  /* Function Body */
  if (*k < 1 || *k > *n || *n > *nmax)
    {
      return 0;
    }
  j = t[*k] - 1;
  i__1 = *nwds;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ipj = i__ + j;
      /* L1: */
      xnode[i__] = data[ipj];
    }
  return 0;
}				/* hpacc_ */

/* 
 *  PURPOSE 
 *         DELETE K-TH ELEMENT OF HEAP.  RESULTING TREE IS REHEAPED. 
 *  INPUT 
 *        NMAX = MAXIMUN NUMBER OF NODES ALLOWED BY USER. 
 *        NWDS = NUMBER OF WORDS PER NODE. 
 *        DATA = WORK AREA IN WHICH THE NODES ARE STORED. 
 *       N = CURRENT NUMBER OF NODES. 
 *       T = INT ARRAY OF POINTERS TO NODES. 
 *        HPFUN = NAME OF USER WRITTEN FUNCTION TO DETERMINE TOP NODE. 
 *        K = INDEX OF NODE TO BE DELETED 
 *  OUTPUT 
 *        N = UPDATED NUMBER OF NODES. 
 *        T = UPDATED INT POINTER ARRAY TO NODES. 
 * 
 */

static int
tdq_hpdel (int *nmax, int *nwds, double *data, int *n, int *t,
	   Twodqg hpfun, int *k)
{
  int kdel, junk;
  int il, ir, khalve;

  /* Parameter adjustments */
  --t;
  --data;

  /* Function Body */
  if (*n == 0)
    {
      return 0;
    }
  if (*k == *n)
    {
      --(*n);
      return 0;
    }
  kdel = *k;
  junk = t[kdel];
  t[kdel] = t[*n];
  t[*n] = junk;
  --(*n);
 L10:
  if (kdel == 1)
    {
      tdq_hpgro (nmax, nwds, &data[1], n, &t[1],  hpfun, &kdel);
      return 0;
    }
  else
    {
      khalve = kdel / 2;
      il = t[khalve];
      ir = t[kdel];
      if ((*hpfun) (&data[il], &data[ir], nwds))
	{
	  tdq_hpgro (nmax, nwds, &data[1], n, &t[1], hpfun,   &kdel);
	  return 0;
	}
      else
	{
	  t[khalve] = ir;
	  t[kdel] = il;
	  kdel = khalve;
	}
    }
  goto L10;
}				/* hpdel_ */

/* 
 *  PURPOSE 
 *         FORMS A HEAP OUT OF A TREE. USED PRIVATELY BY HPBLD. 
 *         THE TOP OF THE TREE IS STORED IN LOCATION T(I). 
 *         FIRST SON IS IN LOCATION T(2I), NEXT SON 
 *         IS IN LOCATION T(2I+1). 
 *         THIS PROGRAM ASSUMES EACH BRANCH OF THE TREE IS A HEAP. 
 * 
 */

static int
tdq_hpgro (int *nmax, int *nwds, double *data, int *n, int *t,
	   Twodqg hpfun, int *i__)
{
  int j, k, itemp, il, ir;

  /* Parameter adjustments */
  --t;
  --data;

  /* Function Body */
  if (*n > *nmax)
    {
      return 0;
    }
  /* 
   */
  k = *i__;
 L1:
  j = k << 1;
  /* 
   *         TEST IF ELEMENT IN J TH POSITION IS A LEAF. 
   * 
   */
  if (j > *n)
    {
      return 0;
    }
  /* 
   *         IF THERE IS MORE THAN ONE SON, FIND WHICH SON IS SMALLEST. 
   * 
   */
  if (j == *n)
    {
      goto L2;
    }
  ir = t[j];
  il = t[j + 1];
  if ((*hpfun) (&data[il], &data[ir], nwds))
    {
      ++j;
    }
  /* 
   *         IF A SON IS LARGER THAN FATHER, INTERCHANGE 
   *         THIS DESTROYS HEAP PROPERTY, SO MUST RE-HEAP REMAINING 
   *         ELEMENTS 
   * 
   */
 L2:
  il = t[k];
  ir = t[j];
  if ((*hpfun) (&data[il], &data[ir], nwds))
    {
      return 0;
    }
  itemp = t[j];
  t[j] = t[k];
  t[k] = itemp;
  k = j;
  goto L1;
}				/* hpgro_ */

/* 
 *  PURPOSE 
 *        THIS ROUTINE INSERTS A NODE INTO AN ALREADY EXISTING HEAP. 
 *            THE RESULTING TREE IS RE-HEAPED. 
 * 
 *  INPUT 
 *        NMAX = MAXIMUM NUMBER OF NODES ALLOWED BY USER. 
 *        NWDS = NUMBER OF WORDS PER NODE. 
 *        DATA = WORK AREA FOR STORING NODES. 
 *        N = CURRENT NUMBER OF NODES IN THE TREE. 
 *        T = INT ARRAY OF POINTERS TO HEAP NODES. 
 *        XNODE = A REAL ARRAY, NWDS WORDS LONG, WHICH 
 *               CONTAINS THE NODAL INFORMATION TO BE INSERTED. 
 *        HPFUN = NAME OF USER WRITTEN FUNCTION TO DETERMINE 
 *               THE TOP NODE. 
 *  OUTPUT 
 *        DATA = WORK AREA WITH NEW NODE INSERTED. 
 *        N = UPDATED NUMBER OF NODES. 
 *        T = UPDATED INT POINTER ARRAY. 
 * 
 */

static int
tdq_hpins (int *nmax, int *nwds, double *data, int *n, int *t,
	   double *xnode, Twodqg hpfun)
{
  int i__1;

  /* Local variables */
  int i__, j, j2, jl, jr, ipj;

  /* Parameter adjustments */

  --xnode;
  --t;
  --data;

  /* Function Body */
  if (*n == *nmax)
    {
      return 0;
    }
  ++(*n);
  j = t[*n] - 1;
  i__1 = *nwds;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ipj = i__ + j;
      /* L1: */
      data[ipj] = xnode[i__];
    }
  j = *n;
 L2:
  if (j == 1)
    {
      return 0;
    }
  jr = t[j];
  j2 = j / 2;
  jl = t[j2];
  if ((*hpfun) (&data[jl], &data[jr], nwds))
    {
      return 0;
    }
  t[j2] = t[j];
  t[j] = jl;
  j = j2;
  goto L2;
}				/* hpins_ */

static int
tdq_tridv (double *node, double *node1, double *node2, double *coef,
	   int *rank)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double temp, coef1;
  int i__, j;
  double s[3];
  int t[3];

  /* Parameter adjustments */
  --node2;
  --node1;
  --node;

  /* Function Body */
  coef1 = 1. - *coef;
  /*Computing 2nd power 
   */
  d__1 = node[3] - node[5];
  /*Computing 2nd power 
   */
  d__2 = node[4] - node[6];
  s[0] = d__1 * d__1 + d__2 * d__2;
  /*Computing 2nd power 
   */
  d__1 = node[5] - node[7];
  /*Computing 2nd power 
   */
  d__2 = node[6] - node[8];
  s[1] = d__1 * d__1 + d__2 * d__2;
  /*Computing 2nd power 
   */
  d__1 = node[3] - node[7];
  /*Computing 2nd power 
   */
  d__2 = node[4] - node[8];
  s[2] = d__1 * d__1 + d__2 * d__2;
  t[0] = 1;
  t[1] = 2;
  t[2] = 3;
  for (i__ = 1; i__ <= 2; ++i__)
    {
      for (j = i__ + 1; j <= 3; ++j)
	{
	  if (s[i__ - 1] < s[j - 1])
	    {
	      temp = (double) t[i__ - 1];
	      t[i__ - 1] = t[j - 1];
	      t[j - 1] = (int) temp;
	    }
	  /* L10: */
	}
    }
  if (t[*rank - 1] == 1)
    {
      node1[3] = *coef * node[3] + coef1 * node[5];
      node1[4] = *coef * node[4] + coef1 * node[6];
      node1[5] = node[5];
      node1[6] = node[6];
      node1[7] = node[7];
      node1[8] = node[8];
      node2[3] = node1[3];
      node2[4] = node1[4];
      node2[5] = node[7];
      node2[6] = node[8];
      node2[7] = node[3];
      node2[8] = node[4];
    }
  else if (t[*rank - 1] == 2)
    {
      node1[3] = *coef * node[5] + coef1 * node[7];
      node1[4] = *coef * node[6] + coef1 * node[8];
      node1[5] = node[7];
      node1[6] = node[8];
      node1[7] = node[3];
      node1[8] = node[4];
      node2[3] = node1[3];
      node2[4] = node1[4];
      node2[5] = node[3];
      node2[6] = node[4];
      node2[7] = node[5];
      node2[8] = node[6];
    }
  else
    {
      node1[3] = *coef * node[3] + coef1 * node[7];
      node1[4] = *coef * node[4] + coef1 * node[8];
      node1[5] = node[3];
      node1[6] = node[4];
      node1[7] = node[5];
      node1[8] = node[6];
      node2[3] = node1[3];
      node2[4] = node1[4];
      node2[5] = node[5];
      node2[6] = node[6];
      node2[7] = node[7];
      node2[8] = node[8];
    }
  node1[9] = *coef * node[9];
  node2[9] = coef1 * node[9];
  return 0;
}				/* tridv_ */

/* 
 *     PURPOSE 
 *          compute on approximation of I(f) = \int_T f(x,y) dx dy 
 *          T being the triangle with vertices (u(i),v(j)), j=1..3 
 *          together with an estimate the error, 
 * 
 *     PARAMETERS 
 *          f       - function subprogram defining the integrand. 
 *                    It must have the form: 
 *                                func(x,y,z,n) 
 *                    and should return an int. func computes 
 *                    z(k) = f(x(k),y(k)) for 1<=k<=n. The returned value 
 *                    is used to communicate a possible failure in the 
 *                    computation of f (in most cases f is evaluated 
 *                    by the nsp interpretor from a function written 
 *                    in the nsp langage). The return value must be 
 *                    0 if all is OK, other value imply to return to 
 *                    the caller (the variable stat being used to 
 *                    communicate the problem). 
 * 
 *         u(1),u(2),u(3)- abscissae of vertices 
 *         v(1),v(2),v(3)- ordinates of vertices 
 * 
 *         res8     - approximation of I(f), obtained by the 
 *                    Lyness and Jespersen rule of degree 8, 
 *                    using 16 points 
 *         est      - estimate of the absolute error 
 * 
 *         vectflag - boolean TRUE if the external f could be evaluated 
 *                    on a vector 
 * 
 *         stat     - return 0 if the evaluation of f by the nsp interpretor 
 *                    is successful. Other values stop the computation and 
 *                    return immediatly to the caller. 
 * 
 * 
 *     REMARKS 
 *         date of last update : 10 april 1984 o.w. rechard nbs 
 * 
 *         modified by Bruno Pincon (24 feb 2008) for nsp: 
 *           - add the possibility of a "vector" evaluation of f 
 *             (this speed-up the computation when f is evaluated 
 *              by the nsp interpretor). 
 *           - clean up the code 
 * 
 *         subroutines or functions called : 
 *                  - f (user-supplied integrand function) 
 *                  - dlamch for machine dependent information 
 * 
 *    local var 
 * 
 * 
 *    first homogeneous coordinates of points in degree-6 
 *    and degree-8 formula, taken with multiplicity 3 
 */

static int
tdq_lqm0vect (Twodqf f, double *u, double *v, double *res8,
	      double *est, int *vectflag, int *stat)
{
  /* Initialized data */

  static double zeta1[9] =
    { .5014265096581342, .8738219710169965, .6365024991213939,
      .05314504984483216, .08141482341455413, .8989055433659379, .6588613844964797,
      .008394777409957211, .7284923929554041 };
  static double zeta2[9] =
    { .2492867451709329, .06308901449150177, .05314504984483216,
      .6365024991213939, .4592925882927229, .05054722831703103, .1705693077517601, .7284923929554041,
      .008394777409957211 };
  static double w80 = .1443156076777862;
  static double w[9] =
    { .1167862757263407, .05084490637020547, .08285107561839291,
      .08285107561839291, .09509163426728497, .03245849762319813, .1032173705347184,
      .02723031417443487, .02723031417443487 };

  /* System generated locals */
  double d__1, d__2, d__3, d__4, d__5;


  /* Local variables */
  int j, k;
  double emach, x[28], y[28], dresc, fvect[28], uflow;
  int n1;
  double u1, u2, r6, u3, v1, v2, v3, z1, z2, z3, resab6;
  int n28;
  double t_area__, res6;

  /* Parameter adjustments */
  --v;
  --u;

  /* Function Body */
  /*    second homogeneous coordinates of points in degree-6 
   *    and degree-8 formula, taken with multiplicity 3 
   *    weights of mid-point of triangle in degree-6 
   *    resp. degree-8 formulae 
   *    weights in degree-6 and degree-8 rule 
   * 
   *          list of major variables 
   *          ---------------------- 
   *         res6, resab6 and dresc are used for error estimation: 
   * 
   *         res6     - approximation of I(f), obtained by the Lyness and 
   *                    Jespersen rule of degree 6, using 12 points. 
   *         resab6   - approximation of I(|f|) by the rule of degree 6 
   *         dresc    - approximation of I(|f- I(f)/T_area|) by the rule of degree 6 
   * 
   *         r6       - res6 / area(T) (the approximation of I(f)/T_area 
   *                    used to compute dresc). 
   * 
   *          x       - cartesian abscissae of the integration points 
   *          y       - cartesian ordinates of the integration points 
   * 
   *    various initialisation 
   * 
   */
  emach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  n1 = 1;
  n28 = 28;
  u1 = u[1];
  u2 = u[2];
  u3 = u[3];
  v1 = v[1];
  v2 = v[2];
  v3 = v[3];
  t_area__ = (d__1 =
	      u1 * v2 - u2 * v1 - u1 * v3 + v1 * u3 + u2 * v3 - v2 * u3,
	      Abs (d__1)) * .5;
  /* 
   *    compute integration points for both degree-6 and degree-8 formulae 
   * 
   */
  k = 1;
  for (j = 1; j <= 9; ++j)
    {
      z1 = zeta1[j - 1];
      z2 = zeta2[j - 1];
      z3 = 1. - z1 - z2;
      x[k - 1] = z1 * u1 + z2 * u2 + z3 * u3;
      y[k - 1] = z1 * v1 + z2 * v2 + z3 * v3;
      x[k] = z2 * u1 + z3 * u2 + z1 * u3;
      y[k] = z2 * v1 + z3 * v2 + z1 * v3;
      x[k + 1] = z3 * u1 + z1 * u2 + z2 * u3;
      y[k + 1] = z3 * v1 + z1 * v2 + z2 * v3;
      k += 3;
    }
  x[27] = (u1 + u2 + u3) / 3.;
  y[27] = (v1 + v2 + v3) / 3.;
  /* 
   *    evaluation of the function f onto the integration points 
   * 
   */
  if (*vectflag)
    {
      *stat = (*f) (x, y, fvect, &n28);
      if (*stat != 0)
	{
	  return 0;
	}
    }
  else
    {
      for (j = 1; j <= 28; ++j)
	{
	  *stat = (*f) (&x[j - 1], &y[j - 1], &fvect[j - 1], &n1);
	  if (*stat != 0)
	    {
	      return 0;
	    }
	}
    }
  /* 
   *    compute degree-6 approximation for I(f) and I(|f|) 
   * 
   */
  r6 = 0.;
  resab6 = 0.;
  k = 1;
  for (j = 1; j <= 4; ++j)
    {
      r6 += w[j - 1] * (fvect[k - 1] + fvect[k] + fvect[k + 1]);
      resab6 +=
	w[j - 1] * ((d__1 = fvect[k - 1], Abs (d__1)) +
		    (d__2 = fvect[k], Abs (d__2)) + (d__3 =
						     fvect[k + 1],
						     Abs (d__3)));
      k += 3;
    }
  res6 = t_area__ * r6;
  resab6 = t_area__ * resab6;
  /* 
   *    compute degree-8 approximation for I(f) 
   * 
   */
  *res8 = w80 * fvect[27];
  for (j = 5; j <= 9; ++j)
    {
      *res8 += w[j - 1] * (fvect[k - 1] + fvect[k] + fvect[k + 1]);
      k += 3;
    }
  *res8 = t_area__ * *res8;
  /* 
   *    compute degree-6 approximation for I(|f - I(f)/T_area|) 
   * 
   */
  dresc = 0.;
  k = 1;
  for (j = 1; j <= 4; ++j)
    {
      dresc +=
	w[j - 1] * ((d__1 = fvect[k - 1] - r6, Abs (d__1)) +
		    (d__2 = fvect[k] - r6, Abs (d__2)) + (d__3 =
							  fvect[k + 1] - r6,
							  Abs (d__3)));
      k += 3;
    }
  dresc = t_area__ * dresc;
  /* 
   *    compute error estimate 
   * 
   */
  *est = (d__1 = *res8 - res6, Abs (d__1));
  if (dresc != 0.)
    {
      /*Computing MAX 
       *Computing MIN 
       */
      d__5 = *est * 20. / dresc;
      d__3 = 1., d__4 = pow (d__5, c_b58);
      d__1 = *est, d__2 = dresc * Min (d__3, d__4);
      *est = Max (d__1, d__2);
    }
  if (resab6 > uflow)
    {
      /*Computing MAX 
       */
      d__1 = emach * resab6;
      *est = Max (d__1, *est);
    }
  return 0;
}				

/* 
 *     PURPOSE 
 *          compute on approximation of I(f) = \int_T f(x,y) dx dy 
 *          T being the triangle with vertices (u(i),v(j)), j=1..3 
 *          together with an estimate the error, 
 * 
 *     PARAMETERS 
 *          f       - function subprogram defining the integrand. 
 *                    It must have the form: 
 *                                func(x,y,z,n) 
 *                    and should return an int. func computes 
 *                    z(k) = f(x(k),y(k)) for 1<=k<=n. The returned value 
 *                    is used to communicate a possible failure in the 
 *                    computation of f (in most cases f is evaluated 
 *                    by the nsp interpretor from a function written 
 *                    in the nsp langage). The return value must be 
 *                    0 if all is OK, other value imply to return to 
 *                    the caller (the variable stat being used to 
 *                    communicate the problem). 
 * 
 *         u(1),u(2),u(3)- abscissae of vertices 
 *         v(1),v(2),v(3)- ordinates of vertices 
 * 
 *         res11    - approximation of I(f), obtained by the 
 *                    Lyness and Jespersen rule of degree 11, 
 *                    using 28 points 
 *         est      - estimate of the absolute error 
 * 
 *         vectflag - boolean TRUE if the external f could be evaluated 
 *                    on a vector 
 * 
 *         stat     - return 0 if the evaluation of f by the nsp interpretor 
 *                    is successful. Other values stop the computation and 
 *                    return immediatly to the caller. 
 * 
 * 
 *     REMARKS 
 *          date of last update : 18 jan 1984 d. kahaner nbs 
 * 
 *         modified by Bruno Pincon (24 feb 2008) for nsp: 
 *           - add the possibility of a "vector" evaluation of f 
 *             (this speed-up the computation when f is evaluated 
 *              by the nsp interpretor). 
 *           - clean up the code 
 * 
 *         subroutines or functions called : 
 *                  - f (user-supplied integrand function) 
 *                  - dlamch for machine dependent information 
 *    local var 
 * 
 *    first homogeneous coordinates of points in degree-9 
 *    and degree-11 formula, taken with multiplicity 3 
 */


static int
tdq_lqm1vect (Twodqf f, double *u, double *v, double *res11,
	      double *est, int *vectflag, int *stat)
{
  /* Initialized data */

  static double zeta1[15] =
    { .02063496160252593, .125820817014129, .6235929287619356,
      .9105409732110941, .03683841205473626, .741198598784498, .9480217181434233, .8114249947041546,
      .0107264499655706, .5853132347709715, .1221843885990187, .04484167758913055, .6779376548825902,
      0., .8588702812826364 };
  static double zeta2[15] =
    { .489682519198737, .4370895914929355, .1882035356190322,
      .04472951339445297, .741198598784498, .03683841205473626, .02598914092828833, .0942875026479227,
      .4946367750172147, .2073433826145142, .4389078057004907, .6779376548825902, .04484167758913055,
      .8588702812826364, 0. };
  static double w90 = .0971357962827961;
  static double w110 = .0879773011622219;
  static double w[15] =
    { .03133470022713983, .07782754100477543, .0796477389272091,
      .0255776756586981, .0432835393772894, .0432835393772894, .00874431155373619,
      .03808157199393533, .01885544805613125, .072159697544741, .0693291387055372,
      .0410563154292886, .0410563154292886, .007362383783300573, .007362383783300573 };

  /* System generated locals */
  double d__1, d__2, d__3, d__4, d__5;


  /* Local variables */
  int j, k;
  double emach, x[46], y[46], dresc, fvect[46], f0, uflow;
  int n1;
  double u1, u2, u3, v1, v2, r9, v3, z1, z2, z3, resab9;
  int n46;
  double t_area__, res9;

  /* Parameter adjustments */
  --v;
  --u;

  /* Function Body */
  /*    second homogeneous coordinates of points in degree-9 
   *    and degree-11 formula, taken with munltiplicity 3 
   *    weights of mid-point of triangle in degree-9 
   *    resp. degree-11 formulae 
   *    weights in degree-9 and degree-11 rule 
   * 
   *          list of major variables 
   *          ---------------------- 
   *         res9, resab9 and dresc are used for error estimation: 
   * 
   *         res9     - approximation of I(f), obtained by the Lyness and 
   *                    Jespersen rule of degree 9, using 19 points. 
   *         resab9   - approximation of I(|f|) by the rule of degree 9 
   *         dresc    - approximation of I(|f- I(f)/T_area|) by the rule of degree 9 
   * 
   *         r9       - res9 / area(T) (the approximation of I(f)/T_area 
   *                    used to compute dresc). 
   * 
   *          x       - cartesian abscissae of the integration points 
   *          y       - cartesian ordinates of the integration points 
   * 
   *    various initialisation 
   * 
   */
  emach = nsp_dlamch ("p");
  uflow = nsp_dlamch ("u");
  n1 = 1;
  n46 = 46;
  u1 = u[1];
  u2 = u[2];
  u3 = u[3];
  v1 = v[1];
  v2 = v[2];
  v3 = v[3];
  t_area__ = (d__1 =
	      u1 * v2 - u2 * v1 - u1 * v3 + v1 * u3 + u2 * v3 - v2 * u3,
	      Abs (d__1)) * .5;
  /* 
   *    compute integration points for both degree-9 and degree-11 formulae 
   * 
   */
  k = 1;
  for (j = 1; j <= 15; ++j)
    {
      z1 = zeta1[j - 1];
      z2 = zeta2[j - 1];
      z3 = 1. - z1 - z2;
      x[k - 1] = z1 * u1 + z2 * u2 + z3 * u3;
      y[k - 1] = z1 * v1 + z2 * v2 + z3 * v3;
      x[k] = z2 * u1 + z3 * u2 + z1 * u3;
      y[k] = z2 * v1 + z3 * v2 + z1 * v3;
      x[k + 1] = z3 * u1 + z1 * u2 + z2 * u3;
      y[k + 1] = z3 * v1 + z1 * v2 + z2 * v3;
      k += 3;
    }
  x[45] = (u1 + u2 + u3) / 3.;
  y[45] = (v1 + v2 + v3) / 3.;
  /* 
   *    evaluation of the function f onto the integration points 
   * 
   */
  if (*vectflag)
    {
      *stat = (*f) (x, y, fvect, &n46);
      if (*stat != 0)
	{
	  return 0;
	}
    }
  else
    {
      for (j = 1; j <= 46; ++j)
	{
	  *stat = (*f) (&x[j - 1], &y[j - 1], &fvect[j - 1], &n1);
	  if (*stat != 0)
	    {
	      return 0;
	    }
	}
    }
  f0 = fvect[45];
  /* 
   *    compute degree-9 approximation for I(f) and I(|f|) 
   * 
   */
  r9 = w90 * f0;
  resab9 = w90 * Abs (f0);
  k = 1;
  for (j = 1; j <= 6; ++j)
    {
      r9 += w[j - 1] * (fvect[k - 1] + fvect[k] + fvect[k + 1]);
      resab9 +=
	w[j - 1] * ((d__1 = fvect[k - 1], Abs (d__1)) +
		    (d__2 = fvect[k], Abs (d__2)) + (d__3 =
						     fvect[k + 1],
						     Abs (d__3)));
      k += 3;
    }
  res9 = t_area__ * r9;
  resab9 = t_area__ * resab9;
  /* 
   *    compute degree-11 approximation for I(f) 
   * 
   */
  *res11 = w110 * f0;
  for (j = 7; j <= 15; ++j)
    {
      *res11 += w[j - 1] * (fvect[k - 1] + fvect[k] + fvect[k + 1]);
      k += 3;
    }
  *res11 = t_area__ * *res11;
  /* 
   *    compute degree-9 approximation for I(|f - I(f)/T_area|) 
   * 
   */
  dresc = w90 * (d__1 = f0 - r9, Abs (d__1));
  k = 1;
  for (j = 1; j <= 6; ++j)
    {
      dresc +=
	w[j - 1] * ((d__1 = fvect[k - 1] - r9, Abs (d__1)) +
		    (d__2 = fvect[k] - r9, Abs (d__2)) + (d__3 =
							  fvect[k + 1] - r9,
							  Abs (d__3)));
      k += 3;
    }
  dresc = t_area__ * dresc;
  /* 
   *    compute error estimate 
   * 
   */
  *est = (d__1 = *res11 - res9, Abs (d__1));
  if (dresc != 0.)
    {
      /*Computing MAX 
       *Computing MIN 
       */
      d__5 = *est * 20. / dresc;
      d__3 = 1., d__4 = pow (d__5, c_b58);
      d__1 = *est, d__2 = dresc * Min (d__3, d__4);
      *est = Max (d__1, d__2);
    }
  if (resab9 > uflow)
    {
      /*Computing MAX 
       */
      d__1 = emach * resab9;
      *est = Max (d__1, *est);
    }
  return 0;
}	

