/* cshep2d.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_cshep2 (int *n, double *x, double *y, double *f, int *nc,
		    int *nw, int *nr, int *lcell, int *lnext, double *xmin,
		    double *ymin, double *dx, double *dy, double *rmax,
		    double *rw, double *a, int *ier)
{
  /* Initialized data */

  static double rtol = 1e-5;
  static double dtol = .01;

  /* System generated locals */
  int lcell_dim1, lcell_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  double dmin__;
  int ierr, lmax, irow, npts[40];
  double rsmx, b[100] /* was [10][10] */ , c__;
  int i__, j, k;
  double s, t, rsold;
  double fk;
  double rc, sf;
  int nn, np;
  double xk, yk, rs;
  int ncwmax;
  int ip1, jp1;
  double sfc;
  int nnc;
  double ddx, ddy;
  int neq, lnp;
  double sfs, stf;
  int nnr, nnw;
  double xmn, sum, ymn, rws;
  int irm1;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  02/13/97 
   * 
   *  This subroutine computes a set of parameters defining a 
   *C2 (twice continuously differentiable) bivariate function 
   *C(X,Y) which interpolates data values F at a set of N 
   *arbitrarily distributed points (X,Y) in the plane (nodes). 
   *The interpolant C may be evaluated at an arbitrary point 
   *by function CS2VAL, and its first partial derivatives are 
   *computed by Subroutine CS2GRD. 
   * 
   *  The interpolation scheme is a modified Cubic Shepard 
   *method: 
   * 
   *C = [W(1)*C(1)+W(2)*C(2)+..+W(N)*C(N)]/[W(1)+W(2)+..+W(N)] 
   * 
   *for bivariate functions W(k) and C(k).  The nodal func- 
   *tions are given by 
   * 
   * C(k)(x,y) = A(1,k)*(x-X(k))**3 + 
   *             A(2,k)*(x-X(k))**2*(y-Y(k)) + 
   *             A(3,k)*(x-X(k))*(y-Y(k))**2 + 
   *             A(4,k)*(y-Y(k))**3 + A(5,k)*(x-X(k))**2 + 
   *             A(6,k)*(x-X(k))*(y-Y(k)) + A(7,k)*(y-Y(k))**2 
   *             + A(8,k)*(x-X(k)) + A(9,k)*(y-Y(k)) + F(k) . 
   * 
   *Thus, C(k) is a cubic function which interpolates the data 
   *value at node k.  Its coefficients A(,k) are obtained by a 
   *weighted least squares fit to the closest NC data points 
   *with weights similar to W(k).  Note that the radius of 
   *influence for the least squares fit is fixed for each k, 
   *but varies with k. 
   * 
   *The weights are taken to be 
   * 
   *  W(k)(x,y) = ( (R(k)-D(k))+ / R(k)*D(k) )**3 , 
   * 
   *where (R(k)-D(k))+ = 0 if R(k) < D(k), and D(k)(x,y) is 
   *the Euclidean distance between (x,y) and (X(k),Y(k)).  The 
   *radius of influence R(k) varies with k and is chosen so 
   *that NW nodes are within the radius.  Note that W(k) is 
   *not defined at node (X(k),Y(k)), but C(x,y) has limit F(k) 
   *as (x,y) approaches (X(k),Y(k)). 
   * 
   *On input: 
   * 
   *      N = Number of nodes and data values.  N .GE. 10. 
   * 
   *      X,Y = Arrays of length N containing the Cartesian 
   *            coordinates of the nodes. 
   * 
   *      F = Array of length N containing the data values 
   *          in one-to-one correspondence with the nodes. 
   * 
   *      NC = Number of data points to be used in the least 
   *           squares fit for coefficients defining the nodal 
   *           functions C(k).  Values found to be optimal for 
   *           test data sets ranged from 11 to 25.  A recom- 
   *           mended value for general data sets is NC = 17. 
   *           For nodes lying on (or close to) a rectangular 
   *           grid, the recommended value is NC = 11.  In any 
   *           case, NC must be in the range 9 to Min(40,N-1). 
   * 
   *      NW = Number of nodes within (and defining) the radii 
   *           of influence R(k) which enter into the weights 
   *           W(k).  For N sufficiently large, a recommended 
   *           value is NW = 30.  In general, NW should be 
   *           about 1.5*NC.  1 .LE. NW .LE. Min(40,N-1). 
   * 
   *      NR = Number of rows and columns in the cell grid de- 
   *           fined in Subroutine STORE2.  A rectangle con- 
   *           taining the nodes is partitioned into cells in 
   *           order to increase search efficiency.  NR = 
   *           Sqrt(N/3) is recommended.  NR .GE. 1. 
   * 
   *The above parameters are not altered by this routine. 
   * 
   *      LCELL = Array of length .GE. NR**2. 
   * 
   *      LNEXT = Array of length .GE. N. 
   * 
   *      RW = Array of length .GE. N. 
   * 
   *      A = Array of length .GE. 9N. 
   * 
   *On output: 
   * 
   *      LCELL = NR by NR array of nodal indexes associated 
   *              with cells.  Refer to Subroutine STORE2. 
   * 
   *      LNEXT = Array of length N containing next-node 
   *              indexes.  Refer to Subroutine STORE2. 
   * 
   *      XMIN,YMIN,DX,DY = Minimum nodal coordinates and cell 
   *                        dimensions.  Refer to Subroutine 
   *                        STORE2. 
   * 
   *      RMAX = Largest element in RW -- maximum radius R(k). 
   * 
   *      RW = Array containing the the radii R(k) which enter 
   *           into the weights W(k). 
   * 
   *      A = 9 by N array containing the coefficients for 
   *          cubic nodal function C(k) in column k. 
   * 
   *  Note that the output parameters described above are not 
   *defined unless IER = 0. 
   * 
   *      IER = Error indicator: 
   *            IER = 0 if no errors were encountered. 
   *            IER = 1 if N, NC, NW, or NR is outside its 
   *                    valid range. 
   *            IER = 2 if duplicate nodes were encountered. 
   *            IER = 3 if all nodes are collinear. 
   * 
   *Modules required by CSHEP2:  GETNP2, GIVENS, ROTATE, 
   *                               SETUP2, STORE2 
   * 
   *Intrinsic functions called by CSHEP2:  ABS, DBLE, MAX, 
   *                                         MIN, SQRT 
   * 
   ************************************************************ 
   * 
   * 
   */
  /* Parameter adjustments */
  a -= 10;
  --rw;
  --lnext;
  --f;
  --y;
  --x;
  lcell_dim1 = *nr;
  lcell_offset = lcell_dim1 + 1;
  lcell -= lcell_offset;

  /* Function Body */
  /* 
   *Local parameters: 
   * 
   *B =          Transpose of the augmented regression matrix 
   *C =          First component of the plane rotation used to 
   *               zero the lower triangle of B**T -- computed 
   *               by Subroutine GIVENS 
   *DDX,DDY =    Local variables for DX and DY 
   *DMIN =       Minimum of the magnitudes of the diagonal 
   *               elements of the regression matrix after 
   *               zeros are introduced below the diagonal 
   *DTOL =       Tolerance for detecting an ill-conditioned 
   *               system.  The system is accepted when 
   *               DMIN*RC .GE. DTOL. 
   *FK =         Data value at mode K -- F(K) 
   *I =          Index for A, B, and NPTS 
   *IERR =       Error flag for the call to Subroutine STORE2 
   *IP1 =        I+1 
   *IRM1 =       IROW-1 
   *IROW =       Row index for B 
   *J =          Index for A and B 
   *JP1 =        J+1 
   *K =          Nodal function index and column index for A 
   *LMAX =       Maximum number of NPTS elements 
   *LMX =        Maximum value of LMAX 
   *LNP =        Current length of NPTS 
   *NEQ =        Number of equations in the least squares fit 
   *NN,NNC,NNR = Local copies of N, NC, and NR 
   *NNW =        Local copy of NW 
   *NP =         NPTS element 
   *NPTS =       Array containing the indexes of a sequence of 
   *               nodes to be used in the least squares fit 
   *               or to compute RW.  The nodes are ordered 
   *               by distance from K, and the last element 
   *               (usually indexed by LNP) is used only to 
   *               determine RC, or RW(K) if NW > NC. 
   *NCWMAX =     Max(NC,NW) 
   *RC =         Radius of influence which enters into the 
   *               weights for C(K) (see Subroutine SETUP2) 
   *RS =         Squared distance between K and NPTS(LNP) -- 
   *               used to compute RC and RW(K) 
   *RSMX =       Maximum squared RW element encountered 
   *RSOLD =      Squared distance between K and NPTS(LNP-1) -- 
   *               used to compute a relative change in RS 
   *               between succeeding NPTS elements 
   *RTOL =       Tolerance for detecting a sufficiently large 
   *               relative change in RS.  If the change is 
   *               not greater than RTOL, the nodes are 
   *               treated as being the same distance from K 
   *RWS =        Current squared value of RW(K) 
   *S =          Second component of the plane rotation deter- 
   *               mined by subroutine GIVENS 
   *SF =        Scale factor for the linear terms (columns 8 
   *              and 9) in the least squares fit -- inverse 
   *              of the root-mean-square distance between K 
   *              and the nodes (other than K) in the least 
   *              squares fit 
   *SFS =       Scale factor for the quadratic terms (columns 
   *              5, 6, and 7) in the least squares fit -- 
   *              SF*SF 
   *SFC =       Scale factor for the cubic terms (first 4 
   *              columns) in the least squares fit -- SF**3 
   *STF =        Marquardt stabilization factor used to damp 
   *               out the first 4 solution components (third 
   *               partials of the cubic) when the system is 
   *               ill-conditioned.  As STF increases, the 
   *               fitting function approaches a quadratic 
   *               polynomial. 
   *SUM =        Sum of squared Euclidean distances between 
   *               node K and the nodes used in the least 
   *               squares fit (unless additional nodes are 
   *               added for stability) 
   *T =          Temporary variable for accumulating a scalar 
   *               product in the back solve 
   *XK,YK =      Coordinates of node K -- X(K), Y(K) 
   *XMN,YMN =    Local variables for XMIN and YMIN 
   * 
   */
  nn = *n;
  nnc = *nc;
  nnw = *nw;
  nnr = *nr;
  ncwmax = Max (nnc, nnw);
  /*Computing MIN 
   */
  i__1 = 40, i__2 = nn - 1;
  lmax = Min (i__1, i__2);
  if (nnc < 9 || nnw < 1 || ncwmax > lmax || nnr < 1)
    {
      goto L21;
    }
  /* 
   *Create the cell data structure, and initialize RSMX. 
   * 
   */
  nsp_calpack_store2 (&nn, &x[1], &y[1], &nnr, &lcell[lcell_offset],
		      &lnext[1], &xmn, &ymn, &ddx, &ddy, &ierr);
  if (ierr != 0)
    {
      goto L23;
    }
  rsmx = 0.;
  /* 
   *Outer loop on node K: 
   * 
   */
  i__1 = nn;
  for (k = 1; k <= i__1; ++k)
    {
      xk = x[k];
      yk = y[k];
      fk = f[k];
      /* 
       *Mark node K to exclude it from the search for nearest 
       *  neighbors. 
       * 
       */
      lnext[k] = -lnext[k];
      /* 
       *Initialize for loop on NPTS. 
       * 
       */
      rs = 0.;
      sum = 0.;
      rws = 0.;
      rc = 0.;
      lnp = 0;
      /* 
       *Compute NPTS, LNP, RWS, NEQ, RC, and SFS. 
       * 
       */
    L1:
      sum += rs;
      if (lnp == lmax)
	{
	  goto L2;
	}
      ++lnp;
      rsold = rs;
      nsp_calpack_getnp2 (&xk, &yk, &x[1], &y[1], &nnr, &lcell[lcell_offset],
			  &lnext[1], &xmn, &ymn, &ddx, &ddy, &np, &rs);
      if (rs == 0.)
	{
	  goto L22;
	}
      npts[lnp - 1] = np;
      if ((rs - rsold) / rs < rtol)
	{
	  goto L1;
	}
      if (rws == 0. && lnp > nnw)
	{
	  rws = rs;
	}
      if (rc == 0. && lnp > nnc)
	{
	  /* 
	   *  RC = 0 (not yet computed) and LNP > NC.  RC = Sqrt(RS) 
	   *    is sufficiently large to (strictly) include NC nodes. 
	   *    The least squares fit will include NEQ = LNP - 1 
	   *    equations for 9 .LE. NC .LE. NEQ .LT. LMAX .LE. N-1. 
	   * 
	   */
	  neq = lnp - 1;
	  rc = sqrt (rs);
	  sfs = (double) neq / sum;
	}
      /* 
       *  Bottom of loop -- test for termination. 
       * 
       */
      if (lnp > ncwmax)
	{
	  goto L3;
	}
      goto L1;
      /* 
       *All LMAX nodes are included in NPTS.  RWS and/or RC**2 is 
       *  (arbitrarily) taken to be 10 percent larger than the 
       *  distance RS to the last node included. 
       * 
       */
    L2:
      if (rws == 0.)
	{
	  rws = rs * 1.1;
	}
      if (rc == 0.)
	{
	  neq = lmax;
	  rc = sqrt (rs * 1.1);
	  sfs = (double) neq / sum;
	}
      /* 
       *Store RW(K), update RSMX if necessary, and compute SF 
       *  and SFC. 
       * 
       */
    L3:
      rw[k] = sqrt (rws);
      if (rws > rsmx)
	{
	  rsmx = rws;
	}
      sf = sqrt (sfs);
      sfc = sf * sfs;
      /* 
       *A Q-R decomposition is used to solve the least squares 
       *  system.  The transpose of the augmented regression 
       *  matrix is stored in B with columns (rows of B) defined 
       *  as follows:  1-4 are the cubic terms, 5-7 are the quad- 
       *  ratic terms, 8 and 9 are the linear terms, and the last 
       *  column is the right hand side. 
       * 
       *Set up the equations and zero out the lower triangle with 
       *  Givens rotations. 
       * 
       */
      i__ = 0;
    L4:
      ++i__;
      np = npts[i__ - 1];
      irow = Min (i__, 10);
      nsp_calpack_setup2 (&xk, &yk, &fk, &x[np], &y[np], &f[np], &sf, &sfs,
			  &sfc, &rc, &b[irow * 10 - 10]);
      if (i__ == 1)
	{
	  goto L4;
	}
      irm1 = irow - 1;
      i__2 = irm1;
      for (j = 1; j <= i__2; ++j)
	{
	  jp1 = j + 1;
	  nsp_calpack_givens (&b[j + j * 10 - 11], &b[j + irow * 10 - 11],
			      &c__, &s);
	  i__3 = 10 - j;
	  nsp_calpack_rotate (&i__3, &c__, &s, &b[jp1 + j * 10 - 11],
			      &b[jp1 + irow * 10 - 11]);
	  /* L5: */
	}
      if (i__ < neq)
	{
	  goto L4;
	}
      /* 
       *Test the system for ill-conditioning. 
       * 
       *Computing MIN 
       */
      d__1 = Abs (b[0]), d__2 = Abs (b[11]), d__1 = Min (d__1, d__2), d__2 =
	Abs (b[22]), d__1 = Min (d__1, d__2), d__2 = Abs (b[33]), d__1 =
	Min (d__1, d__2), d__2 = Abs (b[44]), d__1 = Min (d__1, d__2), d__2 =
	Abs (b[55]), d__1 = Min (d__1, d__2), d__2 = Abs (b[66]), d__1 =
	Min (d__1, d__2), d__2 = Abs (b[77]), d__1 = Min (d__1, d__2), d__2 =
	Abs (b[88]);
      dmin__ = Min (d__1, d__2);
      if (dmin__ * rc >= dtol)
	{
	  goto L11;
	}
      if (neq == lmax)
	{
	  goto L7;
	}
      /* 
       *Increase RC and add another equation to the system to 
       *  improve the conditioning.  The number of NPTS elements 
       *  is also increased if necessary. 
       * 
       */
    L6:
      rsold = rs;
      ++neq;
      if (neq == lmax)
	{
	  rc = sqrt (rs * 1.1);
	  goto L4;
	}
      if (neq < lnp)
	{
	  /* 
	   *  NEQ < LNP. 
	   * 
	   */
	  np = npts[neq];
	  /*Computing 2nd power 
	   */
	  d__1 = x[np] - xk;
	  /*Computing 2nd power 
	   */
	  d__2 = y[np] - yk;
	  rs = d__1 * d__1 + d__2 * d__2;
	  if ((rs - rsold) / rs < rtol)
	    {
	      goto L6;
	    }
	  rc = sqrt (rs);
	  goto L4;
	}
      /* 
       *  NEQ = LNP.  Add an element to NPTS. 
       * 
       */
      ++lnp;
      nsp_calpack_getnp2 (&xk, &yk, &x[1], &y[1], &nnr, &lcell[lcell_offset],
			  &lnext[1], &xmn, &ymn, &ddx, &ddy, &np, &rs);
      if (np == 0)
	{
	  goto L22;
	}
      npts[lnp - 1] = np;
      if ((rs - rsold) / rs < rtol)
	{
	  goto L6;
	}
      rc = sqrt (rs);
      goto L4;
      /* 
       *Stabilize the system by damping third partials -- add 
       *  multiples of the first four unit vectors to the first 
       *  four equations. 
       * 
       */
    L7:
      stf = 1. / rc;
      for (i__ = 1; i__ <= 4; ++i__)
	{
	  b[i__ + 89] = stf;
	  ip1 = i__ + 1;
	  for (j = ip1; j <= 10; ++j)
	    {
	      b[j + 89] = 0.;
	      /* L8: */
	    }
	  for (j = i__; j <= 9; ++j)
	    {
	      jp1 = j + 1;
	      nsp_calpack_givens (&b[j + j * 10 - 11], &b[j + 89], &c__, &s);
	      i__2 = 10 - j;
	      nsp_calpack_rotate (&i__2, &c__, &s, &b[jp1 + j * 10 - 11],
				  &b[jp1 + 89]);
	      /* L9: */
	    }
	  /* L10: */
	}
      /* 
       *Test the damped system for ill-conditioning. 
       * 
       *Computing MIN 
       */
      d__1 = Abs (b[44]), d__2 = Abs (b[55]), d__1 = Min (d__1, d__2), d__2 =
	Abs (b[66]), d__1 = Min (d__1, d__2), d__2 = Abs (b[77]), d__1 =
	Min (d__1, d__2), d__2 = Abs (b[88]);
      dmin__ = Min (d__1, d__2);
      if (dmin__ * rc < dtol)
	{
	  goto L23;
	}
      /* 
       *Solve the 9 by 9 triangular system for the coefficients. 
       * 
       */
    L11:
      for (i__ = 9; i__ >= 1; --i__)
	{
	  t = 0.;
	  if (i__ != 9)
	    {
	      ip1 = i__ + 1;
	      for (j = ip1; j <= 9; ++j)
		{
		  t += b[j + i__ * 10 - 11] * a[j + k * 9];
		  /* L12: */
		}
	    }
	  a[i__ + k * 9] = (b[i__ * 10 - 1] - t) / b[i__ + i__ * 10 - 11];
	  /* L13: */
	}
      /* 
       *Scale the coefficients to adjust for the column scaling. 
       * 
       */
      for (i__ = 1; i__ <= 4; ++i__)
	{
	  a[i__ + k * 9] *= sfc;
	  /* L14: */
	}
      a[k * 9 + 5] *= sfs;
      a[k * 9 + 6] *= sfs;
      a[k * 9 + 7] *= sfs;
      a[k * 9 + 8] *= sf;
      a[k * 9 + 9] *= sf;
      /* 
       *Unmark K and the elements of NPTS. 
       * 
       */
      lnext[k] = -lnext[k];
      i__2 = lnp;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  np = npts[i__ - 1];
	  lnext[np] = -lnext[np];
	  /* L15: */
	}
      /* L16: */
    }
  /* 
   *No errors encountered. 
   * 
   */
  *xmin = xmn;
  *ymin = ymn;
  *dx = ddx;
  *dy = ddy;
  *rmax = sqrt (rsmx);
  *ier = 0;
  return 0;
  /* 
   *N, NC, NW, or NR is outside its valid range. 
   * 
   */
L21:
  *ier = 1;
  return 0;
  /* 
   *Duplicate nodes were encountered by GETNP2. 
   * 
   */
L22:
  *ier = 2;
  return 0;
  /* 
   *No unique solution due to collinear nodes. 
   * 
   */
L23:
  *xmin = xmn;
  *ymin = ymn;
  *dx = ddx;
  *dy = ddy;
  *ier = 3;
  return 0;
}				/* cshep2_ */

double
nsp_calpack_cs2val (double *px, double *py, int *n, double *x, double *y,
		    double *f, int *nr, int *lcell, int *lnext, double *xmin,
		    double *ymin, double *dx, double *dy, double *rmax,
		    double *rw, double *a)
{
  /* System generated locals */
  int lcell_dim1, lcell_offset, i__1, i__2;
  double ret_val=0, d__1, d__2;

  /* Local variables */
  int imin, jmin, imax, jmax;
  double delx, dely, d__;
  int i__, j, k;
  double r__, w;
  int kp;
  double sw, xp, yp, swc;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  02/03/97 
   * 
   *  This function returns the value C(PX,PY), where C is the 
   *weighted sum of cubic nodal functions defined in Subrou- 
   *tine CSHEP2.  CS2GRD may be called to compute a gradient 
   *of C along with the value, and/or to test for errors. 
   *CS2HES may be called to compute a value, first partial 
   *derivatives, and second partial derivatives at a point. 
   * 
   *On input: 
   * 
   *      PX,PY = Cartesian coordinates of the point P at 
   *              which C is to be evaluated. 
   * 
   *      N = Number of nodes and data values defining C. 
   *          N .GE. 10. 
   * 
   *      X,Y,F = Arrays of length N containing the nodes and 
   *              data values interpolated by C. 
   * 
   *      NR = Number of rows and columns in the cell grid. 
   *           Refer to Subroutine STORE2.  NR .GE. 1. 
   * 
   *      LCELL = NR by NR array of nodal indexes associated 
   *              with cells.  Refer to Subroutine STORE2. 
   * 
   *      LNEXT = Array of length N containing next-node 
   *              indexes.  Refer to Subroutine STORE2. 
   * 
   *      XMIN,YMIN,DX,DY = Minimum nodal coordinates and cell 
   *                        dimensions.  DX and DY must be 
   *                        positive.  Refer to Subroutine 
   *                        STORE2. 
   * 
   *      RMAX = Largest element in RW -- maximum radius R(k). 
   * 
   *      RW = Array containing the the radii R(k) which enter 
   *           into the weights W(k) defining C. 
   * 
   *      A = 9 by N array containing the coefficients for 
   *          cubic nodal function C(k) in column k. 
   * 
   *  Input parameters are not altered by this function.  The 
   *parameters other than PX and PY should be input unaltered 
   *from their values on output from CSHEP2.  This function 
   *should not be called if a nonzero error flag was returned 
   *by CSHEP2. 
   * 
   *On output: 
   * 
   *      CS2VAL = Function value C(PX,PY) unless N, NR, DX, 
   *               DY, or RMAX is invalid, in which case no 
   *               value is returned. 
   * 
   *Modules required by CS2VAL:  NONE 
   * 
   *Intrinsic functions called by CS2VAL:  INT, SQRT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *D =         Distance between P and node K 
   *DELX =      XP - X(K) 
   *DELY =      YP - Y(K) 
   *I =         Cell row index in the range IMIN to IMAX 
   *IMIN,IMAX = Range of cell row indexes of the cells 
   *              intersected by a disk of radius RMAX 
   *              centered at P 
   *J =         Cell column index in the range JMIN to JMAX 
   *JMIN,JMAX = Range of cell column indexes of the cells 
   *              intersected by a disk of radius RMAX 
   *              centered at P 
   *K =         Index of a node in cell (I,J) 
   *KP =        Previous value of K in the sequence of nodes 
   *              in cell (I,J) 
   *R =         Radius of influence for node K 
   *SW =        Sum of weights W(K) 
   *SWC =       Sum of weighted nodal function values at P 
   *W =         Weight W(K) value at P:  ((R-D)+/(R*D))**3, 
   *              where (R-D)+ = 0 if R < D 
   *XP,YP =     Local copies of PX and PY -- coordinates of P 
   * 
   */
  /* Parameter adjustments */
  a -= 10;
  --rw;
  --lnext;
  --f;
  --y;
  --x;
  lcell_dim1 = *nr;
  lcell_offset = lcell_dim1 + 1;
  lcell -= lcell_offset;

  /* Function Body */
  xp = *px;
  yp = *py;
  if (*n < 10 || *nr < 1 || *dx <= 0. || *dy <= 0. || *rmax < 0.)
    {
      return ret_val;
    }
  /* 
   *Set IMIN, IMAX, JMIN, and JMAX to cell indexes defining 
   *  the range of the search for nodes whose radii include 
   *  P.  The cells which must be searched are those inter- 
   *  sected by (or contained in) a circle of radius RMAX 
   *  centered at P. 
   * 
   */
  imin = (int) ((xp - *xmin - *rmax) / *dx) + 1;
  imax = (int) ((xp - *xmin + *rmax) / *dx) + 1;
  if (imin < 1)
    {
      imin = 1;
    }
  if (imax > *nr)
    {
      imax = *nr;
    }
  jmin = (int) ((yp - *ymin - *rmax) / *dy) + 1;
  jmax = (int) ((yp - *ymin + *rmax) / *dy) + 1;
  if (jmin < 1)
    {
      jmin = 1;
    }
  if (jmax > *nr)
    {
      jmax = *nr;
    }
  /* 
   *The following is a test for no cells within the circle 
   *  of radius RMAX. 
   * 
   */
  if (imin > imax || jmin > jmax)
    {
      goto L6;
    }
  /* 
   *Accumulate weight values in SW and weighted nodal function 
   *  values in SWC.  The weights are W(K) = ((R-D)+/(R*D))**3 
   *  for R = RW(K) and D = distance between P and node K. 
   * 
   */
  sw = 0.;
  swc = 0.;
  /* 
   *Outer loop on cells (I,J). 
   * 
   */
  i__1 = jmax;
  for (j = jmin; j <= i__1; ++j)
    {
      i__2 = imax;
      for (i__ = imin; i__ <= i__2; ++i__)
	{
	  k = lcell[i__ + j * lcell_dim1];
	  if (k == 0)
	    {
	      goto L3;
	    }
	  /* 
	   *Inner loop on nodes K. 
	   * 
	   */
	L1:
	  delx = xp - x[k];
	  dely = yp - y[k];
	  d__ = sqrt (delx * delx + dely * dely);
	  r__ = rw[k];
	  if (d__ >= r__)
	    {
	      goto L2;
	    }
	  if (d__ == 0.)
	    {
	      goto L5;
	    }
	  /*Computing 3rd power 
	   */
	  d__1 = 1. / d__ - 1. / r__, d__2 = d__1;
	  w = d__2 * (d__1 * d__1);
	  sw += w;
	  swc +=
	    w *
	    (((a[k * 9 + 1] * delx + a[k * 9 + 2] * dely +
	       a[k * 9 + 5]) * delx + (a[k * 9 + 3] * dely + a[k * 9 +
							       6]) * dely +
	      a[k * 9 + 8]) * delx + ((a[k * 9 + 4] * dely +
				       a[k * 9 + 7]) * dely + a[k * 9 +
								9]) * dely +
	     f[k]);
	  /* 
	   *Bottom of loop on nodes in cell (I,J). 
	   * 
	   */
	L2:
	  kp = k;
	  k = lnext[kp];
	  if (k != kp)
	    {
	      goto L1;
	    }
	L3:
	  ;
	}
      /* L4: */
    }
  /* 
   *SW = 0 iff P is not within the radius R(K) for any node K. 
   * 
   */
  if (sw == 0.)
    {
      goto L6;
    }
  ret_val = swc / sw;
  return ret_val;
  /* 
   *(PX,PY) = (X(K),Y(K)). 
   * 
   */
L5:
  ret_val = f[k];
  return ret_val;
  /* 
   *All weights are 0 at P. 
   * 
   */
L6:
  ret_val = 0.;
  return ret_val;
}				/* cs2val_ */

int
nsp_calpack_cs2grd (double *px, double *py, int *n, double *x, double *y,
		    double *f, int *nr, int *lcell, int *lnext, double *xmin,
		    double *ymin, double *dx, double *dy, double *rmax,
		    double *rw, double *a, double *c__, double *cx,
		    double *cy, int *ier)
{
  /* System generated locals */
  int lcell_dim1, lcell_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int imin, jmin, imax, jmax;
  double delx, dely, swcx, swcy, d__;
  int i__, j, k;
  double r__, t, w, ck;
  int kp;
  double sw, xp, yp, wx, wy, ckx, cky, swc, sws, swx, swy;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  02/03/97 
   * 
   *  This subroutine computes the value and gradient at P = 
   *(PX,PY) of the interpolatory function C defined in Sub- 
   *routine CSHEP2.  C is a weighted sum of cubic nodal 
   *functions. 
   * 
   *On input: 
   * 
   *      PX,PY = Cartesian coordinates of the point P at 
   *              which C and its partial derivatives are 
   *              to be evaluated. 
   * 
   *      N = Number of nodes and data values defining C. 
   *          N .GE. 10. 
   * 
   *      X,Y,F = Arrays of length N containing the nodes and 
   *              data values interpolated by C. 
   * 
   *      NR = Number of rows and columns in the cell grid. 
   *           Refer to Subroutine STORE2.  NR .GE. 1. 
   * 
   *      LCELL = NR by NR array of nodal indexes associated 
   *              with cells.  Refer to Subroutine STORE2. 
   * 
   *      LNEXT = Array of length N containing next-node 
   *              indexes.  Refer to Subroutine STORE2. 
   * 
   *      XMIN,YMIN,DX,DY = Minimum nodal coordinates and cell 
   *                        dimensions.  DX and DY must be 
   *                        positive.  Refer to Subroutine 
   *                        STORE2. 
   * 
   *      RMAX = Largest element in RW -- maximum radius R(k). 
   * 
   *      RW = Array of length N containing the the radii R(k) 
   *           which enter into the weights W(k) defining C. 
   * 
   *      A = 9 by N array containing the coefficients for 
   *          cubic nodal function C(k) in column k. 
   * 
   *  Input parameters are not altered by this subroutine. 
   *The parameters other than PX and PY should be input 
   *unaltered from their values on output from CSHEP2.  This 
   *subroutine should not be called if a nonzero error flag 
   *was returned by CSHEP2. 
   * 
   *On output: 
   * 
   *      C = Value of C at (PX,PY) unless IER .EQ. 1, in 
   *          which case no values are returned. 
   * 
   *      CX,CY = First partial derivatives of C at (PX,PY) 
   *              unless IER .EQ. 1. 
   * 
   *      IER = Error indicator: 
   *            IER = 0 if no errors were encountered. 
   *            IER = 1 if N, NR, DX, DY or RMAX is invalid. 
   *            IER = 2 if no errors were encountered but 
   *                    (PX,PY) is not within the radius R(k) 
   *                    for any node k (and thus C=CX=CY=0). 
   * 
   *Modules required by CS2GRD:  None 
   * 
   *Intrinsic functions called by CS2GRD:  INT, SQRT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *CK =        Value of cubic nodal function C(K) at P 
   *CKX,CKY =   Partial derivatives of C(K) with respect to X 
   *              and Y, respectively 
   *D =         Distance between P and node K 
   *DELX =      XP - X(K) 
   *DELY =      YP - Y(K) 
   *I =         Cell row index in the range IMIN to IMAX 
   *IMIN,IMAX = Range of cell row indexes of the cells 
   *              intersected by a disk of radius RMAX 
   *              centered at P 
   *J =         Cell column index in the range JMIN to JMAX 
   *JMIN,JMAX = Range of cell column indexes of the cells 
   *              intersected by a disk of radius RMAX 
   *              centered at P 
   *K =         Index of a node in cell (I,J) 
   *KP =        Previous value of K in the sequence of nodes 
   *              in cell (I,J) 
   *R =         Radius of influence for node K 
   *SW =        Sum of weights W(K) 
   *SWC =       Sum of weighted nodal function values at P 
   *SWCX,SWCY = Partial derivatives of SWC with respect to X 
   *              and Y, respectively 
   *SWS =       SW**2 
   *SWX,SWY =   Partial derivatives of SW with respect to X 
   *              and Y, respectively 
   *T =         Temporary variable 
   *W =         Weight W(K) value at P:  ((R-D)+/(R*D))**3, 
   *              where (R-D)+ = 0 if R < D 
   *WX,WY =     Partial derivatives of W with respect to X 
   *              and Y, respectively 
   *XP,YP =     Local copies of PX and PY -- coordinates of P 
   * 
   */
  /* Parameter adjustments */
  a -= 10;
  --rw;
  --lnext;
  --f;
  --y;
  --x;
  lcell_dim1 = *nr;
  lcell_offset = lcell_dim1 + 1;
  lcell -= lcell_offset;

  /* Function Body */
  xp = *px;
  yp = *py;
  if (*n < 10 || *nr < 1 || *dx <= 0. || *dy <= 0. || *rmax < 0.)
    {
      goto L6;
    }
  /* 
   *Set IMIN, IMAX, JMIN, and JMAX to cell indexes defining 
   *  the range of the search for nodes whose radii include 
   *  P.  The cells which must be searched are those inter- 
   *  sected by (or contained in) a circle of radius RMAX 
   *  centered at P. 
   * 
   */
  imin = (int) ((xp - *xmin - *rmax) / *dx) + 1;
  imax = (int) ((xp - *xmin + *rmax) / *dx) + 1;
  if (imin < 1)
    {
      imin = 1;
    }
  if (imax > *nr)
    {
      imax = *nr;
    }
  jmin = (int) ((yp - *ymin - *rmax) / *dy) + 1;
  jmax = (int) ((yp - *ymin + *rmax) / *dy) + 1;
  if (jmin < 1)
    {
      jmin = 1;
    }
  if (jmax > *nr)
    {
      jmax = *nr;
    }
  /* 
   *The following is a test for no cells within the circle 
   *  of radius RMAX. 
   * 
   */
  if (imin > imax || jmin > jmax)
    {
      goto L7;
    }
  /* 
   *C = SWC/SW = Sum(W(K)*C(K))/Sum(W(K)), where the sum is 
   *  from K = 1 to N, C(K) is the cubic nodal function value, 
   *  and W(K) = ((R-D)+/(R*D))**3 for radius R(K) and dist- 
   *  ance D(K).  Thus 
   * 
   *       CX = (SWCX*SW - SWC*SWX)/SW**2  and 
   *       CY = (SWCY*SW - SWC*SWY)/SW**2 
   * 
   *  where SWCX and SWX are partial derivatives with respect 
   *  to X of SWC and SW, respectively.  SWCY and SWY are de- 
   *  fined similarly. 
   * 
   */
  sw = 0.;
  swx = 0.;
  swy = 0.;
  swc = 0.;
  swcx = 0.;
  swcy = 0.;
  /* 
   *Outer loop on cells (I,J). 
   * 
   */
  i__1 = jmax;
  for (j = jmin; j <= i__1; ++j)
    {
      i__2 = imax;
      for (i__ = imin; i__ <= i__2; ++i__)
	{
	  k = lcell[i__ + j * lcell_dim1];
	  if (k == 0)
	    {
	      goto L3;
	    }
	  /* 
	   *Inner loop on nodes K. 
	   * 
	   */
	L1:
	  delx = xp - x[k];
	  dely = yp - y[k];
	  d__ = sqrt (delx * delx + dely * dely);
	  r__ = rw[k];
	  if (d__ >= r__)
	    {
	      goto L2;
	    }
	  if (d__ == 0.)
	    {
	      goto L5;
	    }
	  t = 1. / d__ - 1. / r__;
	  /*Computing 3rd power 
	   */
	  d__1 = t, d__2 = d__1;
	  w = d__2 * (d__1 * d__1);
	  /*Computing 3rd power 
	   */
	  d__1 = d__, d__2 = d__1;
	  t = t * -3. * t / (d__2 * (d__1 * d__1));
	  wx = delx * t;
	  wy = dely * t;
	  t = a[k * 9 + 2] * delx + a[k * 9 + 3] * dely + a[k * 9 + 6];
	  cky =
	    (a[k * 9 + 4] * 3. * dely + a[k * 9 + 3] * delx +
	     a[k * 9 + 7] * 2.) * dely + t * delx + a[k * 9 + 9];
	  t = t * dely + a[k * 9 + 8];
	  ckx =
	    (a[k * 9 + 1] * 3. * delx + a[k * 9 + 2] * dely +
	     a[k * 9 + 5] * 2.) * delx + t;
	  ck =
	    ((a[k * 9 + 1] * delx + a[k * 9 + 5]) * delx + t) * delx +
	    ((a[k * 9 + 4] * dely + a[k * 9 + 7]) * dely +
	     a[k * 9 + 9]) * dely + f[k];
	  sw += w;
	  swx += wx;
	  swy += wy;
	  swc += w * ck;
	  swcx = swcx + wx * ck + w * ckx;
	  swcy = swcy + wy * ck + w * cky;
	  /* 
	   *Bottom of loop on nodes in cell (I,J). 
	   * 
	   */
	L2:
	  kp = k;
	  k = lnext[kp];
	  if (k != kp)
	    {
	      goto L1;
	    }
	L3:
	  ;
	}
      /* L4: */
    }
  /* 
   *SW = 0 iff P is not within the radius R(K) for any node K. 
   * 
   */
  if (sw == 0.)
    {
      goto L7;
    }
  *c__ = swc / sw;
  sws = sw * sw;
  *cx = (swcx * sw - swc * swx) / sws;
  *cy = (swcy * sw - swc * swy) / sws;
  *ier = 0;
  return 0;
  /* 
   *(PX,PY) = (X(K),Y(K)). 
   * 
   */
L5:
  *c__ = f[k];
  *cx = a[k * 9 + 8];
  *cy = a[k * 9 + 9];
  *ier = 0;
  return 0;
  /* 
   *Invalid input parameter. 
   * 
   */
L6:
  *ier = 1;
  return 0;
  /* 
   *No cells contain a point within RMAX of P, or 
   *  SW = 0 and thus D .GE. RW(K) for all K. 
   * 
   */
L7:
  *c__ = 0.;
  *cx = 0.;
  *cy = 0.;
  *ier = 2;
  return 0;
}				/* cs2grd_ */

int
nsp_calpack_cs2hes (double *px, double *py, int *n, double *x, double *y,
		    double *f, int *nr, int *lcell, int *lnext, double *xmin,
		    double *ymin, double *dx, double *dy, double *rmax,
		    double *rw, double *a, double *c__, double *cx,
		    double *cy, double *cxx, double *cxy, double *cyy,
		    int *ier)
{
  /* System generated locals */
  int lcell_dim1, lcell_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int imin, jmin, imax, jmax;
  double delx, dely, ckxx, ckxy, ckyy, dxsq, dysq, swcx, swcy, swxx, swxy,
    swyy, d__;
  int i__, j, k;
  double r__, w, t1, t2, t3, t4, swcxx, swcxy, swcyy, ck;
  int kp;
  double sw, xp, yp, wx, wy, ckx, cky, swc, sws, swx, swy, wxx, wxy, wyy;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  02/03/97 
   * 
   *  This subroutine computes the value, gradient, and 
   *Hessian at P = (PX,PY) of the interpolatory function C 
   *defined in Subroutine CSHEP2.  C is a weighted sum of 
   *cubic nodal functions. 
   * 
   *On input: 
   * 
   *      PX,PY = Cartesian coordinates of the point P at 
   *              which C and its partial derivatives are 
   *              to be evaluated. 
   * 
   *      N = Number of nodes and data values defining C. 
   *          N .GE. 10. 
   * 
   *      X,Y,F = Arrays of length N containing the nodes and 
   *              data values interpolated by C. 
   * 
   *      NR = Number of rows and columns in the cell grid. 
   *           Refer to Subroutine STORE2.  NR .GE. 1. 
   * 
   *      LCELL = NR by NR array of nodal indexes associated 
   *              with cells.  Refer to Subroutine STORE2. 
   * 
   *      LNEXT = Array of length N containing next-node 
   *              indexes.  Refer to Subroutine STORE2. 
   * 
   *      XMIN,YMIN,DX,DY = Minimum nodal coordinates and cell 
   *                        dimensions.  DX and DY must be 
   *                        positive.  Refer to Subroutine 
   *                        STORE2. 
   * 
   *      RMAX = Largest element in RW -- maximum radius R(k). 
   * 
   *      RW = Array of length N containing the the radii R(k) 
   *           which enter into the weights W(k) defining C. 
   * 
   *      A = 9 by N array containing the coefficients for 
   *          cubic nodal function C(k) in column k. 
   * 
   *  Input parameters are not altered by this subroutine. 
   *The parameters other than PX and PY should be input 
   *unaltered from their values on output from CSHEP2.  This 
   *subroutine should not be called if a nonzero error flag 
   *was returned by CSHEP2. 
   * 
   *On output: 
   * 
   *      C = Value of C at (PX,PY) unless IER .EQ. 1, in 
   *          which case no values are returned. 
   * 
   *      CX,CY = First partial derivatives of C at (PX,PY) 
   *              unless IER .EQ. 1. 
   * 
   *      CXX,CXY,CYY = Second partial derivatives of C at 
   *                    (PX,PY) unless IER .EQ. 1. 
   * 
   *      IER = Error indicator: 
   *            IER = 0 if no errors were encountered. 
   *            IER = 1 if N, NR, DX, DY or RMAX is invalid. 
   *            IER = 2 if no errors were encountered but 
   *                    (PX,PY) is not within the radius R(k) 
   *                    for any node k (and thus C = 0). 
   * 
   *Modules required by CS2HES:  None 
   * 
   *Intrinsic functions called by CS2HES:  INT, SQRT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *CK =        Value of cubic nodal function C(K) at P 
   *CKX,CKY =   Partial derivatives of C(K) with respect to X 
   *              and Y, respectively 
   *CKXX,CKXY,CKYY = Second partial derivatives of CK 
   *D =         Distance between P and node K 
   *DELX =      XP - X(K) 
   *DELY =      YP - Y(K) 
   *DXSQ,DYSQ = DELX**2, DELY**2 
   *I =         Cell row index in the range IMIN to IMAX 
   *IMIN,IMAX = Range of cell row indexes of the cells 
   *              intersected by a disk of radius RMAX 
   *              centered at P 
   *J =         Cell column index in the range JMIN to JMAX 
   *JMIN,JMAX = Range of cell column indexes of the cells 
   *              intersected by a disk of radius RMAX 
   *              centered at P 
   *K =         Index of a node in cell (I,J) 
   *KP =        Previous value of K in the sequence of nodes 
   *              in cell (I,J) 
   *R =         Radius of influence for node K 
   *SW =        Sum of weights W(K) 
   *SWC =       Sum of weighted nodal function values at P 
   *SWCX,SWCY = Partial derivatives of SWC with respect to X 
   *              and Y, respectively 
   *SWCXX,SWCXY,SWCYY = Second partial derivatives of SWC 
   *SWS =       SW**2 
   *SWX,SWY =   Partial derivatives of SW with respect to X 
   *              and Y, respectively 
   *SWXX,SWXY,SWYY = Second partial derivatives of SW 
   *T1,T2,T3,T4 = Temporary variables 
   *W =         Weight W(K) value at P:  ((R-D)+/(R*D))**3, 
   *              where (R-D)+ = 0 if R < D 
   *WX,WY =     Partial derivatives of W with respect to X 
   *              and Y, respectively 
   *WXX,WXY,WYY = Second partial derivatives of W 
   *XP,YP =     Local copies of PX and PY -- coordinates of P 
   * 
   */
  /* Parameter adjustments */
  a -= 10;
  --rw;
  --lnext;
  --f;
  --y;
  --x;
  lcell_dim1 = *nr;
  lcell_offset = lcell_dim1 + 1;
  lcell -= lcell_offset;

  /* Function Body */
  xp = *px;
  yp = *py;
  if (*n < 10 || *nr < 1 || *dx <= 0. || *dy <= 0. || *rmax < 0.)
    {
      goto L6;
    }
  /* 
   *Set IMIN, IMAX, JMIN, and JMAX to cell indexes defining 
   *  the range of the search for nodes whose radii include 
   *  P.  The cells which must be searched are those inter- 
   *  sected by (or contained in) a circle of radius RMAX 
   *  centered at P. 
   * 
   */
  imin = (int) ((xp - *xmin - *rmax) / *dx) + 1;
  imax = (int) ((xp - *xmin + *rmax) / *dx) + 1;
  if (imin < 1)
    {
      imin = 1;
    }
  if (imax > *nr)
    {
      imax = *nr;
    }
  jmin = (int) ((yp - *ymin - *rmax) / *dy) + 1;
  jmax = (int) ((yp - *ymin + *rmax) / *dy) + 1;
  if (jmin < 1)
    {
      jmin = 1;
    }
  if (jmax > *nr)
    {
      jmax = *nr;
    }
  /* 
   *The following is a test for no cells within the circle 
   *  of radius RMAX. 
   * 
   */
  if (imin > imax || jmin > jmax)
    {
      goto L7;
    }
  /* 
   *C = SWC/SW = Sum(W(K)*C(K))/Sum(W(K)), where the sum is 
   *  from K = 1 to N, C(K) is the cubic nodal function value, 
   *  and W(K) = ((R-D)+/(R*D))**3 for radius R(K) and dist- 
   *  ance D(K).  Thus 
   * 
   *       CX = (SWCX*SW - SWC*SWX)/SW**2  and 
   *       CY = (SWCY*SW - SWC*SWY)/SW**2 
   * 
   *  where SWCX and SWX are partial derivatives with respect 
   *  to x of SWC and SW, respectively.  SWCY and SWY are de- 
   *  fined similarly.  The second partials are 
   * 
   *       CXX = ( SW*(SWCXX -    2*SWX*CX) - SWC*SWXX )/SW**2 
   *       CXY = ( SW*(SWCXY-SWX*CY-SWY*CX) - SWC*SWXY )/SW**2 
   *       CYY = ( SW*(SWCYY -    2*SWY*CY) - SWC*SWYY )/SW**2 
   * 
   *  where SWCXX and SWXX are second partials with respect 
   *  to x, SWCXY and SWXY are mixed partials, and SWCYY and 
   *  SWYY are second partials with respect to y. 
   * 
   */
  sw = 0.;
  swx = 0.;
  swy = 0.;
  swxx = 0.;
  swxy = 0.;
  swyy = 0.;
  swc = 0.;
  swcx = 0.;
  swcy = 0.;
  swcxx = 0.;
  swcxy = 0.;
  swcyy = 0.;
  /* 
   *Outer loop on cells (I,J). 
   * 
   */
  i__1 = jmax;
  for (j = jmin; j <= i__1; ++j)
    {
      i__2 = imax;
      for (i__ = imin; i__ <= i__2; ++i__)
	{
	  k = lcell[i__ + j * lcell_dim1];
	  if (k == 0)
	    {
	      goto L3;
	    }
	  /* 
	   *Inner loop on nodes K. 
	   * 
	   */
	L1:
	  delx = xp - x[k];
	  dely = yp - y[k];
	  dxsq = delx * delx;
	  dysq = dely * dely;
	  d__ = sqrt (dxsq + dysq);
	  r__ = rw[k];
	  if (d__ >= r__)
	    {
	      goto L2;
	    }
	  if (d__ == 0.)
	    {
	      goto L5;
	    }
	  t1 = 1. / d__ - 1. / r__;
	  /*Computing 3rd power 
	   */
	  d__1 = t1, d__2 = d__1;
	  w = d__2 * (d__1 * d__1);
	  /*Computing 3rd power 
	   */
	  d__1 = d__, d__2 = d__1;
	  t2 = t1 * -3. * t1 / (d__2 * (d__1 * d__1));
	  wx = delx * t2;
	  wy = dely * t2;
	  /*Computing 6th power 
	   */
	  d__1 = d__, d__1 *= d__1, d__2 = d__1;
	  t1 = t1 * 3. * (d__ * 3. * t1 + 2.) / (d__2 * (d__1 * d__1));
	  wxx = t1 * dxsq + t2;
	  wxy = t1 * delx * dely;
	  wyy = t1 * dysq + t2;
	  t1 = a[k * 9 + 1] * delx + a[k * 9 + 2] * dely + a[k * 9 + 5];
	  t2 = t1 + t1 + a[k * 9 + 1] * delx;
	  t3 = a[k * 9 + 4] * dely + a[k * 9 + 3] * delx + a[k * 9 + 7];
	  t4 = t3 + t3 + a[k * 9 + 4] * dely;
	  ck =
	    (t1 * delx + a[k * 9 + 6] * dely + a[k * 9 + 8]) * delx +
	    (t3 * dely + a[k * 9 + 9]) * dely + f[k];
	  ckx =
	    t2 * delx + (a[k * 9 + 3] * dely + a[k * 9 + 6]) * dely +
	    a[k * 9 + 8];
	  cky =
	    t4 * dely + (a[k * 9 + 2] * delx + a[k * 9 + 6]) * delx +
	    a[k * 9 + 9];
	  ckxx = t2 + a[k * 9 + 1] * 3. * delx;
	  ckxy =
	    (a[k * 9 + 2] * delx + a[k * 9 + 3] * dely) * 2. + a[k * 9 + 6];
	  ckyy = t4 + a[k * 9 + 4] * 3. * dely;
	  sw += w;
	  swx += wx;
	  swy += wy;
	  swxx += wxx;
	  swxy += wxy;
	  swyy += wyy;
	  swc += w * ck;
	  swcx = swcx + wx * ck + w * ckx;
	  swcy = swcy + wy * ck + w * cky;
	  swcxx = swcxx + w * ckxx + wx * 2. * ckx + ck * wxx;
	  swcxy = swcxy + w * ckxy + wx * cky + wy * ckx + ck * wxy;
	  swcyy = swcyy + w * ckyy + wy * 2. * cky + ck * wyy;
	  /* 
	   *Bottom of loop on nodes in cell (I,J). 
	   * 
	   */
	L2:
	  kp = k;
	  k = lnext[kp];
	  if (k != kp)
	    {
	      goto L1;
	    }
	L3:
	  ;
	}
      /* L4: */
    }
  /* 
   *SW = 0 iff P is not within the radius R(K) for any node K. 
   * 
   */
  if (sw == 0.)
    {
      goto L7;
    }
  *c__ = swc / sw;
  sws = sw * sw;
  *cx = (swcx * sw - swc * swx) / sws;
  *cy = (swcy * sw - swc * swy) / sws;
  *cxx = (sw * (swcxx - swx * 2. * *cx) - swc * swxx) / sws;
  *cxy = (sw * (swcxy - swy * *cx - swx * *cy) - swc * swxy) / sws;
  *cyy = (sw * (swcyy - swy * 2. * *cy) - swc * swyy) / sws;
  *ier = 0;
  return 0;
  /* 
   *(PX,PY) = (X(K),Y(K)). 
   * 
   */
L5:
  *c__ = f[k];
  *cx = a[k * 9 + 8];
  *cy = a[k * 9 + 9];
  *cxx = a[k * 9 + 5] * 2.;
  *cxy = a[k * 9 + 6];
  *cyy = a[k * 9 + 7] * 2.;
  *ier = 0;
  return 0;
  /* 
   *Invalid input parameter. 
   * 
   */
L6:
  *ier = 1;
  return 0;
  /* 
   *No cells contain a point within RMAX of P, or 
   *  SW = 0 and thus D .GE. RW(K) for all K. 
   * 
   */
L7:
  *c__ = 0.;
  *cx = 0.;
  *cy = 0.;
  *cxx = 0.;
  *cxy = 0.;
  *cyy = 0.;
  *ier = 2;
  return 0;
}				/* cs2hes_ */

int
nsp_calpack_getnp2 (double *px, double *py, double *x, double *y, int *nr,
		    int *lcell, int *lnext, double *xmin, double *ymin,
		    double *dx, double *dy, int *np, double *dsq)
{
  /* System generated locals */
  int lcell_dim1, lcell_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int imin, jmin, imax, jmax, lmin;
  double delx, dely;
  int i__, j, l;
  double r__;
  int first;
  double rsmin;
  int i0, i1, i2, j0, j1, j2, ln;
  double xp, yp, rsq;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  02/03/97 
   * 
   *  Given a set of N nodes and the data structure defined in 
   *Subroutine STORE2, this subroutine uses the cell method to 
   *find the closest unmarked node NP to a specified point P. 
   *NP is then marked by setting LNEXT(NP) to -LNEXT(NP).  (A 
   *node is marked if and only if the corresponding LNEXT ele- 
   *ment is negative.  The absolute values of LNEXT elements, 
   *however, must be preserved.)  Thus, the closest M nodes to 
   *P may be determined by a sequence of M calls to this rou- 
   *tine.  Note that if the nearest neighbor to node K is to 
   *be determined (PX = X(K) and PY = Y(K)), then K should be 
   *marked before the call to this routine. 
   * 
   *  The search is begun in the cell containing (or closest 
   *to) P and proceeds outward in rectangular layers until all 
   *cells which contain points within distance R of P have 
   *been searched, where R is the distance from P to the first 
   *unmarked node encountered (infinite if no unmarked nodes 
   *are present). 
   * 
   *  This code is essentially unaltered from the subroutine 
   *of the same name in QSHEP2D. 
   * 
   *On input: 
   * 
   *      PX,PY = Cartesian coordinates of the point P whose 
   *              nearest unmarked neighbor is to be found. 
   * 
   *      X,Y = Arrays of length N, for N .GE. 2, containing 
   *            the Cartesian coordinates of the nodes. 
   * 
   *      NR = Number of rows and columns in the cell grid. 
   *           Refer to Subroutine STORE2.  NR .GE. 1. 
   * 
   *      LCELL = NR by NR array of nodal indexes associated 
   *              with cells.  Refer to Subroutine STORE2. 
   * 
   *      LNEXT = Array of length N containing next-node 
   *              indexes (or their negatives).  Refer to 
   *              Subroutine STORE2. 
   * 
   *      XMIN,YMIN,DX,DY = Minimum nodal coordinates and cell 
   *                        dimensions.  DX and DY must be 
   *                        positive.  Refer to Subroutine 
   *                        STORE2. 
   * 
   *  Input parameters other than LNEXT are not altered by 
   *this routine.  With the exception of (PX,PY) and the signs 
   *of LNEXT elements, these parameters should be unaltered 
   *from their values on output from Subroutine STORE2. 
   * 
   *On output: 
   * 
   *      NP = Index (for X and Y) of the nearest unmarked 
   *           node to P, or 0 if all nodes are marked or NR 
   *           .LT. 1 or DX .LE. 0 or DY .LE. 0.  LNEXT(NP) 
   *           .LT. 0 IF NP .NE. 0. 
   * 
   *      DSQ = Squared Euclidean distance between P and node 
   *            NP, or 0 if NP = 0. 
   * 
   *Modules required by GETNP2:  None 
   * 
   *Intrinsic functions called by GETNP2:  ABS, INT, SQRT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *DELX,DELY =   PX-XMIN, PY-YMIN 
   *FIRST =       Int variable with value TRUE iff the 
   *                first unmarked node has yet to be 
   *                encountered 
   *I,J =         Cell indexes in the range [I1,I2] X [J1,J2] 
   *I0,J0 =       Indexes of the cell containing or closest 
   *                to P 
   *I1,I2,J1,J2 = Range of cell indexes defining the layer 
   *                whose intersection with the range 
   *                [IMIN,IMAX] X [JMIN,JMAX] is currently 
   *                being searched 
   *IMIN,IMAX =   Cell row indexes defining the range of the 
   *                search 
   *JMIN,JMAX =   Cell column indexes defining the range of 
   *                the search 
   *L,LN =        Indexes of nodes in cell (I,J) 
   *LMIN =        Current candidate for NP 
   *R =           Distance from P to node LMIN 
   *RSMIN =       Squared distance from P to node LMIN 
   *RSQ =         Squared distance from P to node L 
   *XP,YP =       Local copy of PX,PY -- coordinates of P 
   * 
   */
  /* Parameter adjustments */
  --x;
  --y;
  lcell_dim1 = *nr;
  lcell_offset = lcell_dim1 + 1;
  lcell -= lcell_offset;
  --lnext;

  /* Function Body */
  xp = *px;
  yp = *py;
  /* 
   *Test for invalid input parameters. 
   * 
   */
  if (*nr < 1 || *dx <= 0. || *dy <= 0.)
    {
      goto L9;
    }
  /* 
   *Initialize parameters. 
   * 
   */
  first = TRUE;
  imin = 1;
  imax = *nr;
  jmin = 1;
  jmax = *nr;
  delx = xp - *xmin;
  dely = yp - *ymin;
  i0 = (int) (delx / *dx) + 1;
  if (i0 < 1)
    {
      i0 = 1;
    }
  if (i0 > *nr)
    {
      i0 = *nr;
    }
  j0 = (int) (dely / *dy) + 1;
  if (j0 < 1)
    {
      j0 = 1;
    }
  if (j0 > *nr)
    {
      j0 = *nr;
    }
  i1 = i0;
  i2 = i0;
  j1 = j0;
  j2 = j0;
  /* 
   *Outer loop on layers, inner loop on layer cells, excluding 
   *  those outside the range [IMIN,IMAX] X [JMIN,JMAX]. 
   * 
   */
L1:
  i__1 = j2;
  for (j = j1; j <= i__1; ++j)
    {
      if (j > jmax)
	{
	  goto L7;
	}
      if (j < jmin)
	{
	  goto L6;
	}
      i__2 = i2;
      for (i__ = i1; i__ <= i__2; ++i__)
	{
	  if (i__ > imax)
	    {
	      goto L6;
	    }
	  if (i__ < imin)
	    {
	      goto L5;
	    }
	  if (j != j1 && j != j2 && i__ != i1 && i__ != i2)
	    {
	      goto L5;
	    }
	  /* 
	   *Search cell (I,J) for unmarked nodes L. 
	   * 
	   */
	  l = lcell[i__ + j * lcell_dim1];
	  if (l == 0)
	    {
	      goto L5;
	    }
	  /* 
	   *  Loop on nodes in cell (I,J). 
	   * 
	   */
	L2:
	  ln = lnext[l];
	  if (ln < 0)
	    {
	      goto L4;
	    }
	  /* 
	   *  Node L is not marked. 
	   * 
	   *Computing 2nd power 
	   */
	  d__1 = x[l] - xp;
	  /*Computing 2nd power 
	   */
	  d__2 = y[l] - yp;
	  rsq = d__1 * d__1 + d__2 * d__2;
	  if (!first)
	    {
	      goto L3;
	    }
	  /* 
	   *  Node L is the first unmarked neighbor of P encountered. 
	   *    Initialize LMIN to the current candidate for NP, and 
	   *    RSMIN to the squared distance from P to LMIN.  IMIN, 
	   *    IMAX, JMIN, and JMAX are updated to define the smal- 
	   *    lest rectangle containing a circle of radius R = 
	   *    Sqrt(RSMIN) centered at P, and contained in [1,NR] X 
	   *    [1,NR] (except that, if P is outside the rectangle 
	   *    defined by the nodes, it is possible that IMIN > NR, 
	   *    IMAX < 1, JMIN > NR, or JMAX < 1).  FIRST is reset to 
	   *    FALSE. 
	   * 
	   */
	  lmin = l;
	  rsmin = rsq;
	  r__ = sqrt (rsmin);
	  imin = (int) ((delx - r__) / *dx) + 1;
	  if (imin < 1)
	    {
	      imin = 1;
	    }
	  imax = (int) ((delx + r__) / *dx) + 1;
	  if (imax > *nr)
	    {
	      imax = *nr;
	    }
	  jmin = (int) ((dely - r__) / *dy) + 1;
	  if (jmin < 1)
	    {
	      jmin = 1;
	    }
	  jmax = (int) ((dely + r__) / *dy) + 1;
	  if (jmax > *nr)
	    {
	      jmax = *nr;
	    }
	  first = FALSE;
	  goto L4;
	  /* 
	   *  Test for node L closer than LMIN to P. 
	   * 
	   */
	L3:
	  if (rsq >= rsmin)
	    {
	      goto L4;
	    }
	  /* 
	   *  Update LMIN and RSMIN. 
	   * 
	   */
	  lmin = l;
	  rsmin = rsq;
	  /* 
	   *  Test for termination of loop on nodes in cell (I,J). 
	   * 
	   */
	L4:
	  if (Abs (ln) == l)
	    {
	      goto L5;
	    }
	  l = Abs (ln);
	  goto L2;
	L5:
	  ;
	}
    L6:
      ;
    }
  /* 
   *Test for termination of loop on cell layers. 
   * 
   */
L7:
  if (i1 <= imin && i2 >= imax && j1 <= jmin && j2 >= jmax)
    {
      goto L8;
    }
  --i1;
  ++i2;
  --j1;
  ++j2;
  goto L1;
  /* 
   *Unless no unmarked nodes were encountered, LMIN is the 
   *  closest unmarked node to P. 
   * 
   */
L8:
  if (first)
    {
      goto L9;
    }
  *np = lmin;
  *dsq = rsmin;
  lnext[lmin] = -lnext[lmin];
  return 0;
  /* 
   *Error:  NR, DX, or DY is invalid or all nodes are marked. 
   * 
   */
L9:
  *np = 0;
  *dsq = 0.;
  return 0;
}				/* getnp2_ */

int nsp_calpack_givens (double *a, double *b, double *c__, double *s)
{
  /* Local variables */
  double r__, u, v, aa, bb;

  /* 
   ************************************************************ 
   * 
   *                                              From SRFPACK 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  09/01/88 
   * 
   *  This subroutine constructs the Givens plane rotation, 
   * 
   *          ( C  S) 
   *      G = (     ) , where C*C + S*S = 1, 
   *          (-S  C) 
   * 
   *which zeros the second component of the vector (A,B)**T 
   *(transposed).  Subroutine ROTATE may be called to apply 
   *the transformation to a 2 by N matrix. 
   * 
   *  This routine is identical to subroutine SROTG from the 
   *LINPACK BLAS (Basic Linear Algebra Subroutines). 
   * 
   *On input: 
   * 
   *      A,B = Components of the vector defining the rota- 
   *            tion.  These are overwritten by values R 
   *            and Z (described below) which define C and S. 
   * 
   *On output: 
   * 
   *      A = Signed Euclidean norm R of the input vector: 
   *          R = +/-SQRT(A*A + B*B) 
   * 
   *      B = Value Z such that: 
   *            C = SQRT(1-Z*Z) and S=Z if ABS(Z) .LE. 1, and 
   *            C = 1/Z and S = SQRT(1-C*C) if ABS(Z) > 1. 
   * 
   *      C = +/-(A/R) or 1 if R = 0. 
   * 
   *      S = +/-(B/R) or 0 if R = 0. 
   * 
   *Modules required by GIVENS:  None 
   * 
   *Intrinsic functions called by GIVENS:  ABS, SQRT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *AA,BB = Local copies of A and B 
   *R =     C*A + S*B = +/-SQRT(A*A+B*B) 
   *U,V =   Variables used to scale A and B for computing R 
   * 
   */
  aa = *a;
  bb = *b;
  if (Abs (aa) <= Abs (bb))
    {
      goto L1;
    }
  /* 
   *ABS(A) > ABS(B). 
   * 
   */
  u = aa + aa;
  v = bb / u;
  r__ = sqrt (v * v + .25) * u;
  *c__ = aa / r__;
  *s = v * (*c__ + *c__);
  /* 
   *Note that R has the sign of A, C > 0, and S has 
   *  SIGN(A)*SIGN(B). 
   * 
   */
  *b = *s;
  *a = r__;
  return 0;
  /* 
   *ABS(A) .LE. ABS(B). 
   * 
   */
L1:
  if (bb == 0.)
    {
      goto L2;
    }
  u = bb + bb;
  v = aa / u;
  /* 
   *Store R in A. 
   * 
   */
  *a = sqrt (v * v + .25) * u;
  *s = bb / *a;
  *c__ = v * (*s + *s);
  /* 
   *Note that R has the sign of B, S > 0, and C has 
   *  SIGN(A)*SIGN(B). 
   * 
   */
  *b = 1.;
  if (*c__ != 0.)
    {
      *b = 1. / *c__;
    }
  return 0;
  /* 
   *A = B = 0. 
   * 
   */
L2:
  *c__ = 1.;
  *s = 0.;
  return 0;
}				/* givens_ */

int nsp_calpack_rotate (int *n, double *c__, double *s, double *x, double *y)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;
  double xi, yi;

  /* 
   ************************************************************ 
   * 
   *                                              From SRFPACK 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  09/01/88 
   * 
   *                                               ( C  S) 
   *  This subroutine applies the Givens rotation  (     )  to 
   *                                               (-S  C) 
   *                   (X(1) ... X(N)) 
   *the 2 by N matrix  (             ) . 
   *                   (Y(1) ... Y(N)) 
   * 
   *  This routine is identical to subroutine SROT from the 
   *LINPACK BLAS (Basic Linear Algebra Subroutines). 
   * 
   *On input: 
   * 
   *      N = Number of columns to be rotated. 
   * 
   *      C,S = Elements of the Givens rotation.  Refer to 
   *            subroutine GIVENS. 
   * 
   *The above parameters are not altered by this routine. 
   * 
   *      X,Y = Arrays of length .GE. N containing the compo- 
   *            nents of the vectors to be rotated. 
   * 
   *On output: 
   * 
   *      X,Y = Arrays containing the rotated vectors (not 
   *            altered if N < 1). 
   * 
   *Modules required by ROTATE:  None 
   * 
   ************************************************************ 
   * 
   * 
   */
  /* Parameter adjustments */
  --y;
  --x;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xi = x[i__];
      yi = y[i__];
      x[i__] = *c__ * xi + *s * yi;
      y[i__] = -(*s) * xi + *c__ * yi;
      /* L1: */
    }
  return 0;
}				/* rotate_ */

int
nsp_calpack_setup2 (double *xk, double *yk, double *zk, double *xi,
		    double *yi, double *zi, double *s1, double *s2,
		    double *s3, double *r__, double *row)
{
  /* Local variables */
  double dxsq, dysq, d__;
  int i__;
  double w, w1, w2, w3, dx, dy;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  02/03/97 
   * 
   *  This subroutine sets up the I-th row of an augmented re- 
   *gression matrix for a weighted least squares fit of a 
   *cubic function f(x,y) to a set of data values z, where 
   *f(XK,YK) = ZK.  The first four columns (cubic terms) are 
   *scaled by S3, the next three columns (quadratic terms) 
   *are scaled by S2, and the eighth and ninth columns (lin- 
   *ear terms) are scaled by S1. 
   * 
   *On input: 
   * 
   *      XK,YK = Coordinates of node K. 
   * 
   *      ZK = Data value at node K to be interpolated by f. 
   * 
   *      XI,YI,ZI = Coordinates and data value at node I. 
   * 
   *      S1,S2,S3 = Scale factors. 
   * 
   *      R = Radius of influence about node K defining the 
   *          weight. 
   * 
   *The above parameters are not altered by this routine. 
   * 
   *      ROW = Array of length 10. 
   * 
   *On output: 
   * 
   *      ROW = Array containing a row of the augmented re- 
   *            gression matrix. 
   * 
   *Modules required by SETUP2:  None 
   * 
   *Intrinsic function called by SETUP2:  SQRT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *D =    Distance between nodes K and I 
   *DX =   XI - XK 
   *DXSQ = DX*DX 
   *DY =   YI - YK 
   *DYSQ = DY*DY 
   *I =    DO-loop index 
   *W =    Weight associated with the row:  (R-D)/(R*D) 
   *         (0 if D = 0 or D > R) 
   *W1 =   S1*W 
   *W2 =   S2*W 
   *W3 =   W3*W 
   * 
   */
  /* Parameter adjustments */
  --row;

  /* Function Body */
  dx = *xi - *xk;
  dy = *yi - *yk;
  dxsq = dx * dx;
  dysq = dy * dy;
  d__ = sqrt (dxsq + dysq);
  if (d__ <= 0. || d__ >= *r__)
    {
      goto L1;
    }
  w = (*r__ - d__) / *r__ / d__;
  w1 = *s1 * w;
  w2 = *s2 * w;
  w3 = *s3 * w;
  row[1] = dxsq * dx * w3;
  row[2] = dxsq * dy * w3;
  row[3] = dx * dysq * w3;
  row[4] = dysq * dy * w3;
  row[5] = dxsq * w2;
  row[6] = dx * dy * w2;
  row[7] = dysq * w2;
  row[8] = dx * w1;
  row[9] = dy * w1;
  row[10] = (*zi - *zk) * w;
  return 0;
  /* 
   *Nodes K and I coincide or node I is outside of the radius 
   *  of influence.  Set ROW to the zero vector. 
   * 
   */
L1:
  for (i__ = 1; i__ <= 10; ++i__)
    {
      row[i__] = 0.;
      /* L2: */
    }
  return 0;
}				/* setup2_ */

int
nsp_calpack_store2 (int *n, double *x, double *y, int *nr, int *lcell,
		    int *lnext, double *xmin, double *ymin, double *dx,
		    double *dy, int *ier)
{
  /* System generated locals */
  int lcell_dim1, lcell_offset, i__1, i__2;

  /* Local variables */
  double delx, dely;
  int i__, j, k, l, nn, nnr;
  double xmn, ymn, xmx, ymx;

  /* 
   ************************************************************ 
   * 
   *                                              From CSHEP2D 
   *                                           Robert J. Renka 
   *                                 Dept. of Computer Science 
   *                                      Univ. of North Texas 
   *                                          renka@cs.unt.edu 
   *                                                  03/28/97 
   * 
   *  Given a set of N arbitrarily distributed nodes in the 
   *plane, this subroutine creates a data structure for a 
   *cell-based method of solving closest-point problems.  The 
   *smallest rectangle containing the nodes is partitioned 
   *into an NR by NR uniform grid of cells, and nodes are as- 
   *sociated with cells.  In particular, the data structure 
   *stores the indexes of the nodes contained in each cell. 
   *For a uniform random distribution of nodes, the nearest 
   *node to an arbitrary point can be determined in constant 
   *expected time. 
   * 
   *  This code is essentially unaltered from the subroutine 
   *of the same name in QSHEP2D. 
   * 
   *On input: 
   * 
   *      N = Number of nodes.  N .GE. 2. 
   * 
   *      X,Y = Arrays of length N containing the Cartesian 
   *            coordinates of the nodes. 
   * 
   *      NR = Number of rows and columns in the grid.  The 
   *           cell density (average number of nodes per cell) 
   *           is D = N/(NR**2).  A recommended value, based 
   *           on empirical evidence, is D = 3 -- NR = 
   *           Sqrt(N/3).  NR .GE. 1. 
   * 
   *The above parameters are not altered by this routine. 
   * 
   *      LCELL = Array of length .GE. NR**2. 
   * 
   *      LNEXT = Array of length .GE. N. 
   * 
   *On output: 
   * 
   *      LCELL = NR by NR cell array such that LCELL(I,J) 
   *              contains the index (for X and Y) of the 
   *              first node (node with smallest index) in 
   *              cell (I,J), or LCELL(I,J) = 0 if no nodes 
   *              are contained in the cell.  The upper right 
   *              corner of cell (I,J) has coordinates (XMIN+ 
   *              I*DX,YMIN+J*DY).  LCELL is not defined if 
   *              IER .NE. 0. 
   * 
   *      LNEXT = Array of next-node indexes such that 
   *              LNEXT(K) contains the index of the next node 
   *              in the cell which contains node K, or 
   *              LNEXT(K) = K if K is the last node in the 
   *              cell for K = 1,...,N.  (The nodes contained 
   *              in a cell are ordered by their indexes.) 
   *              If, for example, cell (I,J) contains nodes 
   *              2, 3, and 5 (and no others), then LCELL(I,J) 
   *              = 2, LNEXT(2) = 3, LNEXT(3) = 5, and 
   *              LNEXT(5) = 5.  LNEXT is not defined if 
   *              IER .NE. 0. 
   * 
   *      XMIN,YMIN = Cartesian coordinates of the lower left 
   *                  corner of the rectangle defined by the 
   *                  nodes (smallest nodal coordinates) un- 
   *                  less IER = 1.  The upper right corner is 
   *                  (XMAX,YMAX) for XMAX = XMIN + NR*DX and 
   *                  YMAX = YMIN + NR*DY. 
   * 
   *      DX,DY = Dimensions of the cells unless IER = 1.  DX 
   *              = (XMAX-XMIN)/NR and DY = (YMAX-YMIN)/NR, 
   *              where XMIN, XMAX, YMIN, and YMAX are the 
   *              extrema of X and Y. 
   * 
   *      IER = Error indicator: 
   *            IER = 0 if no errors were encountered. 
   *            IER = 1 if N < 2 or NR < 1. 
   *            IER = 2 if DX = 0 or DY = 0. 
   * 
   *Modules required by STORE2:  None 
   * 
   *Intrinsic functions called by STORE2:  DBLE, INT 
   * 
   ************************************************************ 
   * 
   * 
   *Local parameters: 
   * 
   *DELX,DELY = Components of the cell dimensions -- local 
   *              copies of DX,DY 
   *I,J =       Cell indexes 
   *K =         Nodal index 
   *L =         Index of a node in cell (I,J) 
   *NN =        Local copy of N 
   *NNR =       Local copy of NR 
   *XMN,XMX =   Range of nodal X coordinates 
   *YMN,YMX =   Range of nodal Y coordinates 
   * 
   */
  /* Parameter adjustments */
  --lnext;
  --y;
  --x;
  lcell_dim1 = *nr;
  lcell_offset = lcell_dim1 + 1;
  lcell -= lcell_offset;

  /* Function Body */
  nn = *n;
  nnr = *nr;
  if (nn < 2 || nnr < 1)
    {
      goto L5;
    }
  /* 
   *Compute the dimensions of the rectangle containing the 
   *  nodes. 
   * 
   */
  xmn = x[1];
  xmx = xmn;
  ymn = y[1];
  ymx = ymn;
  i__1 = nn;
  for (k = 2; k <= i__1; ++k)
    {
      if (x[k] < xmn)
	{
	  xmn = x[k];
	}
      if (x[k] > xmx)
	{
	  xmx = x[k];
	}
      if (y[k] < ymn)
	{
	  ymn = y[k];
	}
      if (y[k] > ymx)
	{
	  ymx = y[k];
	}
      /* L1: */
    }
  *xmin = xmn;
  *ymin = ymn;
  /* 
   *Compute cell dimensions and test for zero area. 
   * 
   */
  delx = (xmx - xmn) / (double) nnr;
  dely = (ymx - ymn) / (double) nnr;
  *dx = delx;
  *dy = dely;
  if (delx == 0. || dely == 0.)
    {
      goto L6;
    }
  /* 
   *Initialize LCELL. 
   * 
   */
  i__1 = nnr;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = nnr;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  lcell[i__ + j * lcell_dim1] = 0;
	  /* L2: */
	}
      /* L3: */
    }
  /* 
   *Loop on nodes, storing indexes in LCELL and LNEXT. 
   * 
   */
  for (k = nn; k >= 1; --k)
    {
      i__ = (int) ((x[k] - xmn) / delx) + 1;
      if (i__ > nnr)
	{
	  i__ = nnr;
	}
      j = (int) ((y[k] - ymn) / dely) + 1;
      if (j > nnr)
	{
	  j = nnr;
	}
      l = lcell[i__ + j * lcell_dim1];
      lnext[k] = l;
      if (l == 0)
	{
	  lnext[k] = k;
	}
      lcell[i__ + j * lcell_dim1] = k;
      /* L4: */
    }
  /* 
   *No errors encountered. 
   * 
   */
  *ier = 0;
  return 0;
  /* 
   *Invalid input parameter. 
   * 
   */
L5:
  *ier = 1;
  return 0;
  /* 
   *DX = 0 or DY = 0. 
   * 
   */
L6:
  *ier = 2;
  return 0;
}				/* store2_ */
