/* somespline.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

double return_a_nan_nsp_calpack_ (void)
{
  double d = 0.0;
  return d/d;
}

#if 0

struct
{
  int new_call__;
} info_hermite__;

#define info_hermite__1 info_hermite__

/* Table of constant values */

static int c__1 = 1;
static int c__4 = 4;
static double c_b4 = 0.;
static int c__2 = 2;
static int c__0 = 0;

int
nsp_calpack_splinecub (double *x, double *y, double *d__, int *n,
		       int *type__, double *a_d__, double *a_sd__,
		       double *qdy, double *lll)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  int i__;
  double r__;

  /* 
   *    PURPOSE 
   *       computes a cubic spline interpolation function 
   *       in Hermite form (ie computes the derivatives d(i) of the 
   *       spline in each interpolation point (x(i), y(i))) 
   * 
   *    ARGUMENTS 
   *     inputs : 
   *        n       number of interpolation points (n >= 3) 
   *        x, y    the n interpolation points, x must be in strict increasing order 
   *        type    type of the spline : currently 
   *                   type = 0 correspond to a  NOT_A_KNOT spline where it is 
   *                            imposed the conditions : 
   *                                s'''(x(2)-) = s'''(x(2)+) 
   *                            and s'''(x(n-1)-) = s'''(x(n-1)+) 
   *                   type = 1 correspond to a NATURAL spline with the conditions : 
   *                                s''(x1) = 0 
   *                            and s''(xn) = 0 
   *                   type = 2 correspond to a CLAMPED spline (d(1) and d(n) are given) 
   * 
   *                   type = 3 correspond to a PERIODIC spline 
   *     outputs : 
   *        d    the derivatives in each x(i) i = 1..n 
   * 
   *     work arrays : 
   *        A_d(1..n), A_sd(1..n-1), qdy(1..n-1) 
   *        lll(1..n-1) (used only in the periodic case) 
   * 
   *   NOTES 
   *        this routine requires (i)   n >= 3 (for natural) n >=4 (for not_a_knot) 
   *                              (ii)  strict increasing abscissae x(i) 
   *                              (iii) y(1) = y(n) in the periodic case 
   *        THESE CONDITIONS MUST BE TESTED IN THE CALLING CODE 
   * 
   *    AUTHOR 
   *       Bruno Pincon 
   * 
   *    July 22 2004 : correction of the case not_a_knot which worked only 
   *                   for equidistant abscissae 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --lll;
  --qdy;
  --a_sd__;
  --a_d__;
  --d__;
  --y;
  --x;

  /* Function Body */
  if (*n == 2)
    {
      if (*type__ != 2)
	{
	  d__[1] = (y[2] - y[1]) / (x[2] - x[1]);
	  d__[2] = d__[1];
	}
      return 0;
    }
  if (*n == 3 && *type__ == 0)
    {
      nsp_calpack_derivd (&x[1], &y[1], &d__[1], n, &c__1, &c__4);
      return 0;
    }
  i__1 = *n - 1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      a_sd__[i__] = 1. / (x[i__ + 1] - x[i__]);
      /*Computing 2nd power 
       */
      d__1 = a_sd__[i__];
      qdy[i__] = (y[i__ + 1] - y[i__]) * (d__1 * d__1);
    }
  /*    compute the coef matrix and r.h.s. for rows 2..n-1 
   *    (which don't relies on the type) 
   */
  i__1 = *n - 1;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      a_d__[i__] = (a_sd__[i__ - 1] + a_sd__[i__]) * 2.;
      d__[i__] = (qdy[i__ - 1] + qdy[i__]) * 3.;
    }
  /*    compute equ 1 and n in function of the type 
   */
  if (*type__ == 1)
    {
      a_d__[1] = a_sd__[1] * 2.;
      d__[1] = qdy[1] * 3.;
      a_d__[*n] = a_sd__[*n - 1] * 2.;
      d__[*n] = qdy[*n - 1] * 3.;
      nsp_calpack_tridiagldltsolve (&a_d__[1], &a_sd__[1], &d__[1], n);
    }
  else if (*type__ == 0)
    {
      /* s'''(x(2)-) = s'''(x(2)+) 
       */
      r__ = a_sd__[2] / a_sd__[1];
      a_d__[1] = a_sd__[1] / (r__ + 1.);
      /*Computing 2nd power 
       */
      d__1 = r__ + 1.;
      d__[1] = ((r__ * 3. + 2.) * qdy[1] + r__ * qdy[2]) / (d__1 * d__1);
      /* s'''(x(n-1)-) = s'''(x(n-1)+) 
       */
      r__ = a_sd__[*n - 2] / a_sd__[*n - 1];
      a_d__[*n] = a_sd__[*n - 1] / (r__ + 1.);
      /*Computing 2nd power 
       */
      d__1 = r__ + 1.;
      d__[*n] =
	((r__ * 3. + 2.) * qdy[*n - 1] + r__ * qdy[*n - 2]) / (d__1 * d__1);
      nsp_calpack_tridiagldltsolve (&a_d__[1], &a_sd__[1], &d__[1], n);
    }
  else if (*type__ == 2)
    {
      /*d(1) and d(n) are already known 
       */
      d__[2] -= d__[1] * a_sd__[1];
      d__[*n - 1] -= d__[*n] * a_sd__[*n - 1];
      i__1 = *n - 2;
      nsp_calpack_tridiagldltsolve (&a_d__[2], &a_sd__[2], &d__[2], &i__1);
    }
  else if (*type__ == 3)
    {
      a_d__[1] = (a_sd__[1] + a_sd__[*n - 1]) * 2.;
      d__[1] = (qdy[1] + qdy[*n - 1]) * 3.;
      lll[1] = a_sd__[*n - 1];
      i__1 = *n - 2;
      nsp_calpack_dset (&i__1, &c_b4, &lll[2], &c__1);
      /*mise a zero 
       */
      lll[*n - 2] = a_sd__[*n - 2];
      i__1 = *n - 1;
      nsp_calpack_cyclictridiagldltsolve (&a_d__[1], &a_sd__[1], &lll[1],
					  &d__[1], &i__1);
      d__[*n] = d__[1];
    }
  return 0;
}				/* splinecub_ */

/*subroutine SplineCub 
 */
int nsp_calpack_tridiagldltsolve (double *d__, double *l, double *b, int *n)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double temp;
  int i__;

  /* 
   *    PURPOSE 
   *       solve a linear system A x = b with a symetric tridiagonal positive definite 
   *       matrix A by using an LDL^t factorization 
   * 
   *    PARAMETERS 
   *       d(1..n)   : on input the diagonal of A 
   *                   on output the diagonal of the (diagonal) matrix D 
   *       l(1..n-1) : on input the sub-diagonal of A 
   *                   on output the sub-diagonal of L 
   *       b(1..n)   : on input contains the r.h.s. b 
   *                   on output the solution x 
   *       n         : the dimension 
   * 
   *    CAUTION 
   *       no zero pivot detection 
   * 
   */
  /* Parameter adjustments */
  --b;
  --l;
  --d__;

  /* Function Body */
  i__1 = *n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      temp = l[i__ - 1];
      l[i__ - 1] /= d__[i__ - 1];
      d__[i__] -= temp * l[i__ - 1];
      b[i__] -= l[i__ - 1] * b[i__ - 1];
    }
  b[*n] /= d__[*n];
  for (i__ = *n - 1; i__ >= 1; --i__)
    {
      b[i__] = b[i__] / d__[i__] - l[i__] * b[i__ + 1];
    }
  return 0;
}				/* tridiagldltsolve_ */

/*subroutine TriDiagLDLtSolve 
 */
int
nsp_calpack_cyclictridiagldltsolve (double *d__, double *lsd, double *lll,
				    double *b, int *n)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double temp1, temp2;
  int i__, j;

  /* 
   *    PURPOSE 
   *       solve a linear system A x = b with a symetric "nearly" tridiagonal 
   *       positive definite matrix A by using an LDL^t factorization, 
   *       the matrix A has the form : 
   * 
   *         |x x         x|                        |1            | 
   *         |x x x        |                        |x 1          | 
   *         |  x x x      |                        |  x 1        | 
   *         |    x x x    |  and so the L is like  |    x 1      | 
   *         |      x x x  |                        |      x 1    | 
   *         |        x x x|                        |        x 1  | 
   *         |x         x x|                        |x x x x x x 1| 
   * 
   *    PARAMETERS 
   *       d(1..n)     : on input the diagonal of A 
   *                     on output the diagonal of the (diagonal) matrix D 
   *       lsd(1..n-2) : on input the sub-diagonal of A (without  A(n,n-1)) 
   *                     on output the sub-diagonal of L (without  L(n,n-1)) 
   *       lll(1..n-1) : on input the last line of A (without A(n,n)) 
   *                     on output the last line of L (without L(n,n)) 
   *       b(1..n)     : on input contains the r.h.s. b 
   *                     on output the solution x 
   *       n           : the dimension 
   * 
   *    CAUTION 
   *       no zero pivot detection 
   * 
   *    compute the LDL^t factorization 
   */
  /* Parameter adjustments */
  --b;
  --lll;
  --lsd;
  --d__;

  /* Function Body */
  i__1 = *n - 2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      temp1 = lsd[i__];
      temp2 = lll[i__];
      lsd[i__] /= d__[i__];
      /*elimination coef L(i,i-1) 
       */
      lll[i__] /= d__[i__];
      /*elimination coef L(n,i-1) 
       */
      d__[i__ + 1] -= lsd[i__] * temp1;
      /*elimination on line i+1 
       */
      lll[i__ + 1] -= lll[i__] * temp1;
      /*elimination on line n 
       */
      d__[*n] -= lll[i__] * temp2;
      /*elimination on line n 
       */
    }
  temp2 = lll[*n - 1];
  lll[*n - 1] /= d__[*n - 1];
  d__[*n] -= lll[*n - 1] * temp2;
  /*    solve LDL^t x = b  (but use b for x and for the intermediary vectors...) 
   */
  i__1 = *n - 1;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      b[i__] -= lsd[i__ - 1] * b[i__ - 1];
    }
  i__1 = *n - 1;
  for (j = 1; j <= i__1; ++j)
    {
      b[*n] -= lll[j] * b[j];
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b[i__] /= d__[i__];
    }
  b[*n - 1] -= lll[*n - 1] * b[*n];
  for (i__ = *n - 2; i__ >= 1; --i__)
    {
      b[i__] = b[i__] - lsd[i__] * b[i__ + 1] - lll[i__] * b[*n];
    }
  return 0;
}				/* cyclictridiagldltsolve_ */

/*subroutine CyclicTriDiagLDLtSolve 
 */
int nsp_calpack_isearch (double *t, double *x, int *n)
{
  /* System generated locals */
  int ret_val;

  /* Local variables */
  int i__, i1, i2;

  /* 
   *    PURPOSE 
   *       x(1..n) being an array (with strict increasing order and n >=2) 
   *       representing intervals, this routine return i such that : 
   * 
   *          x(i) <= t <= x(i+1) 
   * 
   *       and 0 if t is not in [x(1), x(n)] 
   * 
   */
  /* Parameter adjustments */
  --x;

  /* Function Body */
  if (x[1] <= *t && *t <= x[*n])
    {
      /*       dichotomic search 
       */
      i1 = 1;
      i2 = *n;
      while (i2 - i1 > 1)
	{
	  i__ = (i1 + i2) / 2;
	  if (*t <= x[i__])
	    {
	      i2 = i__;
	    }
	  else
	    {
	      i1 = i__;
	    }
	}
      ret_val = i1;
    }
  else
    {
      ret_val = 0;
    }
  return ret_val;
}				/* isearch_ */

int
nsp_calpack_evalpwhermite (double *t, double *st, double *dst, double *d2st,
			   double *d3st, int *m, double *x, double *y,
			   double *d__, int *n, int *outmode)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, j;
  double tt;

  /* 
   *    PURPOSE 
   *       evaluation at the abscissae t(1..m) of the piecewise hermite function 
   *       define by x(1..n), y(1..n), d(1..n) (d being the derivatives at the 
   *       x(i)) together with its derivative, second derivative and third derivative 
   * 
   *    PARAMETERS 
   * 
   *       outmode : define what return in case t(j) not in [x(1), x(n)] 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --d3st;
  --d2st;
  --dst;
  --st;
  --t;
  --d__;
  --y;
  --x;

  /* Function Body */
  info_hermite__1.new_call__ = TRUE;
  i__ = 0;
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      tt = t[j];
      fast_int_search_nsp_calpack_ (&tt, &x[1], n, &i__);
      /*recompute i only if necess 
       */
      if (i__ != 0)
	{
	  nsp_calpack_evalhermite (&tt, &x[i__], &x[i__ + 1], &y[i__],
				   &y[i__ + 1], &d__[i__], &d__[i__ + 1],
				   &st[j], &dst[j], &d2st[j], &d3st[j], &i__);
	}
      else
	{
	  /*t(j) is outside [x(1), x(n)] evaluation depend upon ou 
	   */
	  if (*outmode == 10 || nsp_calpack_isanan (&tt) == 1)
	    {
	      double d=0.0;
	      st[j] = d/d; /* a nan */
	      dst[j] = st[j];
	      d2st[j] = st[j];
	      d3st[j] = st[j];
	    }
	  else if (*outmode == 7)
	    {
	      st[j] = 0.;
	      dst[j] = 0.;
	      d2st[j] = 0.;
	      d3st[j] = 0.;
	    }
	  else if (*outmode == 8)
	    {
	      dst[j] = 0.;
	      d2st[j] = 0.;
	      d3st[j] = 0.;
	      if (tt < x[1])
		{
		  st[j] = y[1];
		}
	      else
		{
		  st[j] = y[*n];
		}
	    }
	  else if (*outmode == 9)
	    {
	      d2st[j] = 0.;
	      d3st[j] = 0.;
	      if (tt < x[1])
		{
		  dst[j] = d__[1];
		  st[j] = y[1] + (tt - x[1]) * d__[1];
		}
	      else
		{
		  dst[j] = d__[*n];
		  st[j] = y[*n] + (tt - x[*n]) * d__[*n];
		}
	    }
	  else
	    {
	      if (*outmode == 1)
		{
		  near_interval_nsp_calpack_ (&tt, &x[1], n, &i__);
		}
	      else if (*outmode == 3)
		{
		  coord_by_periodicity_nsp_calpack_ (&tt, &x[1], n, &i__);
		}
	      nsp_calpack_evalhermite (&tt, &x[i__], &x[i__ + 1], &y[i__],
				       &y[i__ + 1], &d__[i__], &d__[i__ + 1],
				       &st[j], &dst[j], &d2st[j], &d3st[j],
				       &i__);
	    }
	}
    }
  return 0;
}				/* evalpwhermite_ */

/*subroutine EvalPWHermite 
 */
int
nsp_calpack_evalhermite (double *t, double *xa, double *xb, double *ya,
			 double *yb, double *da, double *db, double *h__,
			 double *dh, double *ddh, double *dddh, int *i__)
{
  /* Initialized data */

  static int old_i__ = 0;

  double tmxa, p;
  static double c2, c3;
  double dx;

  if (old_i__ != *i__ || info_hermite__1.new_call__)
    {
      /*       compute the following Newton form : 
       *          h(t) = ya + da*(t-xa) + c2*(t-xa)^2 + c3*(t-xa)^2*(t-xb) 
       */
      dx = 1. / (*xb - *xa);
      p = (*yb - *ya) * dx;
      c2 = (p - *da) * dx;
      c3 = (*db - p + (*da - p)) * (dx * dx);
      info_hermite__1.new_call__ = FALSE;
    }
  old_i__ = *i__;
  /*    eval h(t), h'(t), h"(t) and h"'(t), by a generalised Horner 's scheme 
   */
  tmxa = *t - *xa;
  *h__ = c2 + c3 * (*t - *xb);
  *dh = *h__ + c3 * tmxa;
  *ddh = (*dh + c3 * tmxa) * 2.;
  *dddh = c3 * 6.;
  *h__ = *da + *h__ * tmxa;
  *dh = *h__ + *dh * tmxa;
  *h__ = *ya + *h__ * tmxa;
  return 0;
}				/* evalhermite_ */

/*subroutine EvalHermite 
 */
int fast_int_search_nsp_calpack_ (double *xx, double *x, int *nx, int *i__)
{

  /* Parameter adjustments */
  --x;

  /* Function Body */
  if (*i__ == 0)
    {
      *i__ = nsp_calpack_isearch (xx, &x[1], nx);
    }
  else if (!(x[*i__] <= *xx && *xx <= x[*i__ + 1]))
    {
      *i__ = nsp_calpack_isearch (xx, &x[1], nx);
    }
  return 0;
}				/* fast_int_search__ */

int coord_by_periodicity_nsp_calpack_ (double *t, double *x, int *n, int *i__)
{
  /* Local variables */
  double r__, dx;

  /* 
   *    PURPOSE 
   *       recompute t such that t in [x(1), x(n)] by periodicity : 
   *       and then the interval i of this new t 
   * 
   */
  /* Parameter adjustments */
  --x;

  /* Function Body */
  dx = x[*n] - x[1];
  r__ = (*t - x[1]) / dx;
  if (r__ >= 0.)
    {
      *t = x[1] + (r__ - d_int (&r__)) * dx;
    }
  else
    {
      r__ = Abs (r__);
      *t = x[*n] - (r__ - d_int (&r__)) * dx;
    }
  /*some cautions in case of roundoff errors (is necessary ?) 
   */
  if (*t < x[1])
    {
      *t = x[1];
      *i__ = 1;
    }
  else if (*t > x[*n])
    {
      *t = x[*n];
      *i__ = *n - 1;
    }
  else
    {
      *i__ = nsp_calpack_isearch (t, &x[1], n);
    }
  return 0;
}				/* coord_by_periodicity__ */

/*subroutine coord_by_periodicity 
 */
int near_grid_point_nsp_calpack_ (double *xx, double *x, int *nx, int *i__)
{
  /* 
   *    calcule le point de la grille le plus proche ... a detailler 
   * 
   */
  /* Parameter adjustments */
  --x;

  /* Function Body */
  if (*xx < x[1])
    {
      *i__ = 1;
      *xx = x[1];
    }
  else
    {
      /* xx > x(nx) 
       */
      *i__ = *nx - 1;
      *xx = x[*nx];
    }
  return 0;
}				/* near_grid_point__ */

int near_interval_nsp_calpack_ (double *xx, double *x, int *nx, int *i__)
{
  /* 
   *    idem sans modifier xx 
   * 
   */
  /* Parameter adjustments */
  --x;

  /* Function Body */
  if (*xx < x[1])
    {
      *i__ = 1;
    }
  else
    {
      /* xx > x(nx) 
       */
      *i__ = *nx - 1;
    }
  return 0;
}				/* near_interval__ */

int proj_by_per_nsp_calpack_ (double *t, double *xmin, double *xmax)
{
  /* Local variables */
  double r__, dx;

  /* 
   *    PURPOSE 
   *       recompute t such that t in [xmin, xmax] by periodicity. 
   * 
   */
  dx = *xmax - *xmin;
  r__ = (*t - *xmin) / dx;
  if (r__ >= 0.)
    {
      *t = *xmin + (r__ - d_int (&r__)) * dx;
    }
  else
    {
      r__ = Abs (r__);
      *t = *xmax - (r__ - d_int (&r__)) * dx;
    }
  /*some cautions in case of roundoff errors (is necessary ?) 
   */
  if (*t < *xmin)
    {
      *t = *xmin;
    }
  else if (*t > *xmax)
    {
      *t = *xmax;
    }
  return 0;
}				/* proj_by_per__ */

/*subroutine proj_by_per 
 */
int proj_on_grid_nsp_calpack_ (double *xx, double *xmin, double *xmax)
{
  /* 
   */
  if (*xx < *xmin)
    {
      *xx = *xmin;
    }
  else
    {
      *xx = *xmax;
    }
  return 0;
}				/* proj_on_grid__ */



int
nsp_calpack_bicubicsubspline (double *x, double *y, double *u, int *nx,
			      int *ny, double *c__, double *p, double *q,
			      double *r__, int *type__)
{
  /* System generated locals */
  int u_dim1, u_offset, c_dim3, c_offset, p_dim1, p_offset, q_dim1, q_offset,
    r_dim1, r_offset, i__1;

  /* Local variables */
  int i__, j;

  /* 
   *    PURPOSE 
   *       compute bicubic subsplines 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --x;
  r_dim1 = *nx;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  q_dim1 = *nx;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  p_dim1 = *nx;
  p_offset = p_dim1 + 1;
  p -= p_offset;
  c_dim3 = *nx - 1;
  c_offset = ((c_dim3 + 1 << 2) + 1 << 2) + 1;
  c__ -= c_offset;
  u_dim1 = *nx;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --y;

  /* Function Body */
  if (*type__ == 6)
    {
      /*       approximation des derivees par SUBROUTINE DPCHIM(N,X,F,D,INCFD) 
       *p = du/dx 
       */
      i__1 = *ny;
      for (j = 1; j <= i__1; ++j)
	{
	  nsp_calpack_dpchim (nx, &x[1], &u[j * u_dim1 + 1],
			      &p[j * p_dim1 + 1], &c__1);
	}
      /*q = du/dy 
       */
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_dpchim (ny, &y[1], &u[i__ + u_dim1], &q[i__ + q_dim1],
			      nx);
	}
      /*r = d2 u/ dx dy  approchee via  dq / dx 
       */
      i__1 = *ny;
      for (j = 1; j <= i__1; ++j)
	{
	  nsp_calpack_dpchim (nx, &x[1], &q[j * q_dim1 + 1],
			      &r__[j * r_dim1 + 1], &c__1);
	}
    }
  else if (*type__ == 4 || *type__ == 5)
    {
      /*       approximation des derivees partielles par methode simple 
       *p = du/dx 
       */
      i__1 = *ny;
      for (j = 1; j <= i__1; ++j)
	{
	  nsp_calpack_derivd (&x[1], &u[j * u_dim1 + 1], &p[j * p_dim1 + 1],
			      nx, &c__1, type__);
	}
      /*q = du/dy 
       */
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_derivd (&y[1], &u[i__ + u_dim1], &q[i__ + q_dim1], ny,
			      nx, type__);
	}
      /*r = d2 u/ dx dy  approchee via  dq / dx 
       */
      i__1 = *ny;
      for (j = 1; j <= i__1; ++j)
	{
	  nsp_calpack_derivd (&x[1], &q[j * q_dim1 + 1],
			      &r__[j * r_dim1 + 1], nx, &c__1, type__);
	}
    }
  /*    calculs des coefficients dans les bases (x-x(i))^k (y-y(j))^l  0<= k,l <= 3 
   *    pour evaluation rapide via Horner par la suite 
   */
  coef_bicubic_nsp_calpack_ (&u[u_offset], &p[p_offset], &q[q_offset],
			     &r__[r_offset], &x[1], &y[1], nx, ny,
			     &c__[c_offset]);
  return 0;
}				/* bicubicsubspline_ */

/*subroutine BiCubicSubSpline 
 */
int
nsp_calpack_bicubicspline (double *x, double *y, double *u, int *nx, int *ny,
			   double *c__, double *p, double *q, double *r__,
			   double *a_d__, double *a_sd__, double *d__,
			   double *ll, double *qdu, double *u_temp__,
			   int *type__)
{
  /* System generated locals */
  int u_dim1, u_offset, c_dim3, c_offset, p_dim1, p_offset, q_dim1, q_offset,
    r_dim1, r_offset, i__1, i__2;

  /* Local variables */
  int i__, j;

  /* 
   *    PURPOSE 
   *       compute bicubic splines 
   * 
   *    various constant used in somespline.f 
   *compute du/dx 
   */
  /* Parameter adjustments */
  --x;
  --u_temp__;
  --d__;
  r_dim1 = *nx;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  q_dim1 = *nx;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  p_dim1 = *nx;
  p_offset = p_dim1 + 1;
  p -= p_offset;
  c_dim3 = *nx - 1;
  c_offset = ((c_dim3 + 1 << 2) + 1 << 2) + 1;
  c__ -= c_offset;
  u_dim1 = *nx;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --y;
  --a_d__;
  --a_sd__;
  --ll;
  --qdu;

  /* Function Body */
  i__1 = *ny;
  for (j = 1; j <= i__1; ++j)
    {
      nsp_calpack_splinecub (&x[1], &u[j * u_dim1 + 1], &p[j * p_dim1 + 1],
			     nx, type__, &a_d__[1], &a_sd__[1], &qdu[1],
			     &ll[1]);
    }
  /*compute du/dy 
   */
  i__1 = *nx;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      C2F (dcopy) (ny, &u[i__ + u_dim1], nx, &u_temp__[1], &c__1);
      nsp_calpack_splinecub (&y[1], &u_temp__[1], &d__[1], ny, type__,
			     &a_d__[1], &a_sd__[1], &qdu[1], &ll[1]);
      C2F (dcopy) (ny, &d__[1], &c__1, &q[i__ + q_dim1], nx);
    }
  /*compute ddu/dxdy 
   */
  nsp_calpack_splinecub (&x[1], &q[q_dim1 + 1], &r__[r_dim1 + 1], nx, type__,
			 &a_d__[1], &a_sd__[1], &qdu[1], &ll[1]);
  nsp_calpack_splinecub (&x[1], &q[*ny * q_dim1 + 1], &r__[*ny * r_dim1 + 1],
			 nx, type__, &a_d__[1], &a_sd__[1], &qdu[1], &ll[1]);
  i__1 = *nx;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      C2F (dcopy) (ny, &p[i__ + p_dim1], nx, &u_temp__[1], &c__1);
      d__[1] = r__[i__ + r_dim1];
      d__[*ny] = r__[i__ + *ny * r_dim1];
      nsp_calpack_splinecub (&y[1], &u_temp__[1], &d__[1], ny, &c__2,
			     &a_d__[1], &a_sd__[1], &qdu[1], &ll[1]);
      i__2 = *ny - 2;
      C2F (dcopy) (&i__2, &d__[2], &c__1, &r__[i__ + (r_dim1 << 1)], nx);
    }
  /*    calculs des coefficients dans les bases (x-x(i))^k (y-y(j))^l  0<= k,l <= 3 
   *    pour evaluation rapide via Horner par la suite 
   */
  coef_bicubic_nsp_calpack_ (&u[u_offset], &p[p_offset], &q[q_offset],
			     &r__[r_offset], &x[1], &y[1], nx, ny,
			     &c__[c_offset]);
  return 0;
}				/* bicubicspline_ */

/*subroutine BiCubicSpline 
 */
int
nsp_calpack_derivd (double *x, double *u, double *du, int *n, int *inc,
		    int *type__)
{
  /* System generated locals */
  int u_dim1, u_offset, du_dim1, du_offset, i__1;

  /* Local variables */
  double du_l__, dx_l__, du_r__, dx_r__;
  int i__;
  double w_l__, w_r__;

  /* 
   *    PURPOSE 
   *       given functions values u(i) at points x(i),  i = 1, ..., n 
   *       this subroutine computes approximations du(i) of the derivative 
   *       at the points x(i). 
   * 
   *    METHOD 
   *       For i in [2,n-1], the "centered" formula of order 2 is used : 
   *           d(i) = derivative at x(i) of the interpolation polynomial 
   *                  of the points {(x(j),u(j)), j in [i-1,i+1]} 
   * 
   *        For i=1 and n, if type = FAST_PERIODIC  (in which case u(n)=u(1)) then 
   *        the previus "centered" formula is also used else (type = FAST), d(1) 
   *        is the derivative at x(1) of the interpolation polynomial of 
   *        {(x(j),u(j)), j in [1,3]} and the same method is used for d(n) 
   * 
   *    ARGUMENTS 
   *     inputs : 
   *        n       int : number of point (n >= 2) 
   *        x, u    double precision : the n points, x must be in strict increasing order 
   *        type    int : FAST (the function is non periodic) or FAST_PERIODIC 
   *                (the function is periodic), in this last case u(n) must be equal to u(1)) 
   *        inc     int : to deal easily with 2d applications, u(i) is in fact 
   *                u(1,i) with u declared as u(inc,*) to avoid the direct management of 
   *                the increment inc (the i th value given with u(1 + inc*(i-1) ...) 
   *     outputs : 
   *        d       the derivatives in each x(i) i = 1..n 
   * 
   *   NOTES 
   *        this routine requires (i)   n >= 2 
   *                              (ii)  strict increasing abscissae x(i) 
   *                              (iii) u(1)=u(n) if type = FAST_PERIODIC 
   *        ALL THESE CONDITIONS MUST BE TESTED IN THE CALLING CODE 
   * 
   *    AUTHOR 
   *       Bruno Pincon 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --x;
  du_dim1 = *inc;
  du_offset = du_dim1 + 1;
  du -= du_offset;
  u_dim1 = *inc;
  u_offset = u_dim1 + 1;
  u -= u_offset;

  /* Function Body */
  if (*n == 2)
    {
      /*special case used linear interp 
       */
      du[du_dim1 + 1] =
	(u[(u_dim1 << 1) + 1] - u[u_dim1 + 1]) / (x[2] - x[1]);
      du[(du_dim1 << 1) + 1] = du[du_dim1 + 1];
      return 0;
    }
  if (*type__ == 5)
    {
      dx_r__ = x[*n] - x[*n - 1];
      du_r__ = (u[u_dim1 + 1] - u[(*n - 1) * u_dim1 + 1]) / dx_r__;
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  dx_l__ = dx_r__;
	  du_l__ = du_r__;
	  dx_r__ = x[i__ + 1] - x[i__];
	  du_r__ = (u[(i__ + 1) * u_dim1 + 1] - u[i__ * u_dim1 + 1]) / dx_r__;
	  w_l__ = dx_r__ / (dx_l__ + dx_r__);
	  w_r__ = 1. - w_l__;
	  du[i__ * du_dim1 + 1] = w_l__ * du_l__ + w_r__ * du_r__;
	}
      du[*n * du_dim1 + 1] = du[du_dim1 + 1];
    }
  else if (*type__ == 4)
    {
      dx_l__ = x[2] - x[1];
      du_l__ = (u[(u_dim1 << 1) + 1] - u[u_dim1 + 1]) / dx_l__;
      dx_r__ = x[3] - x[2];
      du_r__ = (u[u_dim1 * 3 + 1] - u[(u_dim1 << 1) + 1]) / dx_r__;
      w_l__ = dx_r__ / (dx_l__ + dx_r__);
      w_r__ = 1. - w_l__;
      du[du_dim1 + 1] = (w_r__ + 1.) * du_l__ - w_r__ * du_r__;
      du[(du_dim1 << 1) + 1] = w_l__ * du_l__ + w_r__ * du_r__;
      i__1 = *n - 1;
      for (i__ = 3; i__ <= i__1; ++i__)
	{
	  dx_l__ = dx_r__;
	  du_l__ = du_r__;
	  dx_r__ = x[i__ + 1] - x[i__];
	  du_r__ = (u[(i__ + 1) * u_dim1 + 1] - u[i__ * u_dim1 + 1]) / dx_r__;
	  w_l__ = dx_r__ / (dx_l__ + dx_r__);
	  w_r__ = 1. - w_l__;
	  du[i__ * du_dim1 + 1] = w_l__ * du_l__ + w_r__ * du_r__;
	}
      du[*n * du_dim1 + 1] = (w_l__ + 1.) * du_r__ - w_l__ * du_l__;
    }
  return 0;
}				/* derivd_ */

int
coef_bicubic_nsp_calpack_ (double *u, double *p, double *q, double *r__,
			   double *x, double *y, int *nx, int *ny,
			   double *c__)
{
  /* System generated locals */
  int u_dim1, u_offset, p_dim1, p_offset, q_dim1, q_offset, r_dim1, r_offset,
    c_dim3, c_offset, i__1, i__2;

  /* Local variables */
  double a, b, d__;
  int i__, j;
  double cc, dx, dy;

  /* 
   *    PURPOSE 
   *       compute for each polynomial (i,j)-patch (defined on 
   *       [x(i),x(i+1)]x[y(i),y(i+1)]) the following base 
   *       representation : 
   *          i,j        _4_  _4_   i,j       k-1       l-1 
   *         u   (x,y) = >__  >__  C   (x-x(i))  (y-y(j)) 
   *                     k=1  l=1   k,l 
   * 
   *       from the "Hermite" representation (values of u, p = du/dx, 
   *       q = du/dy, r = ddu/dxdy at the 4 vertices (x(i),y(j)), 
   *       (x(i+1),y(j)), (x(i+1),y(j+1)), (x(i),y(j+1)). 
   * 
   */
  /* Parameter adjustments */
  --x;
  c_dim3 = *nx - 1;
  c_offset = ((c_dim3 + 1 << 2) + 1 << 2) + 1;
  c__ -= c_offset;
  --y;
  r_dim1 = *nx;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  q_dim1 = *nx;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  p_dim1 = *nx;
  p_offset = p_dim1 + 1;
  p -= p_offset;
  u_dim1 = *nx;
  u_offset = u_dim1 + 1;
  u -= u_offset;

  /* Function Body */
  i__1 = *ny - 1;
  for (j = 1; j <= i__1; ++j)
    {
      dy = 1. / (y[j + 1] - y[j]);
      i__2 = *nx - 1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  dx = 1. / (x[i__ + 1] - x[i__]);
	  c__[((i__ + j * c_dim3 << 2) + 1 << 2) + 1] = u[i__ + j * u_dim1];
	  c__[((i__ + j * c_dim3 << 2) + 1 << 2) + 2] = p[i__ + j * p_dim1];
	  c__[((i__ + j * c_dim3 << 2) + 2 << 2) + 1] = q[i__ + j * q_dim1];
	  c__[((i__ + j * c_dim3 << 2) + 2 << 2) + 2] = r__[i__ + j * r_dim1];
	  a = (u[i__ + 1 + j * u_dim1] - u[i__ + j * u_dim1]) * dx;
	  c__[((i__ + j * c_dim3 << 2) + 1 << 2) + 3] =
	    (a * 3. - p[i__ + j * p_dim1] * 2. -
	     p[i__ + 1 + j * p_dim1]) * dx;
	  c__[((i__ + j * c_dim3 << 2) + 1 << 2) + 4] =
	    (p[i__ + 1 + j * p_dim1] + p[i__ + j * p_dim1] -
	     a * 2.) * (dx * dx);
	  a = (u[i__ + (j + 1) * u_dim1] - u[i__ + j * u_dim1]) * dy;
	  c__[((i__ + j * c_dim3 << 2) + 3 << 2) + 1] =
	    (a * 3. - q[i__ + j * q_dim1] * 2. -
	     q[i__ + (j + 1) * q_dim1]) * dy;
	  c__[((i__ + j * c_dim3 << 2) + 4 << 2) + 1] =
	    (q[i__ + (j + 1) * q_dim1] + q[i__ + j * q_dim1] -
	     a * 2.) * (dy * dy);
	  a = (q[i__ + 1 + j * q_dim1] - q[i__ + j * q_dim1]) * dx;
	  c__[((i__ + j * c_dim3 << 2) + 2 << 2) + 3] =
	    (a * 3. - r__[i__ + 1 + j * r_dim1] -
	     r__[i__ + j * r_dim1] * 2.) * dx;
	  c__[((i__ + j * c_dim3 << 2) + 2 << 2) + 4] =
	    (r__[i__ + 1 + j * r_dim1] + r__[i__ + j * r_dim1] -
	     a * 2.) * (dx * dx);
	  a = (p[i__ + (j + 1) * p_dim1] - p[i__ + j * p_dim1]) * dy;
	  c__[((i__ + j * c_dim3 << 2) + 3 << 2) + 2] =
	    (a * 3. - r__[i__ + (j + 1) * r_dim1] -
	     r__[i__ + j * r_dim1] * 2.) * dy;
	  c__[((i__ + j * c_dim3 << 2) + 4 << 2) + 2] =
	    (r__[i__ + (j + 1) * r_dim1] + r__[i__ + j * r_dim1] -
	     a * 2.) * (dy * dy);
	  a =
	    (u[i__ + 1 + (j + 1) * u_dim1] + u[i__ + j * u_dim1] -
	     u[i__ + 1 + j * u_dim1] - u[i__ +
					 (j +
					  1) * u_dim1]) * (dx * dx * dy *
							   dy) - (p[i__ +
								    (j +
								     1) *
								    p_dim1] -
								  p[i__ +
								    j *
								    p_dim1]) *
	    (dx * dy * dy) - (q[i__ + 1 + j * q_dim1] -
			      q[i__ + j * q_dim1]) * (dx * dx * dy) +
	    r__[i__ + j * r_dim1] * (dx * dy);
	  b =
	    (p[i__ + 1 + (j + 1) * p_dim1] + p[i__ + j * p_dim1] -
	     p[i__ + 1 + j * p_dim1] - p[i__ +
					 (j + 1) * p_dim1]) * (dx * dy * dy) -
	    (r__[i__ + 1 + j * r_dim1] - r__[i__ + j * r_dim1]) * (dx * dy);
	  cc =
	    (q[i__ + 1 + (j + 1) * q_dim1] + q[i__ + j * q_dim1] -
	     q[i__ + 1 + j * q_dim1] - q[i__ +
					 (j + 1) * q_dim1]) * (dx * dx * dy) -
	    (r__[i__ + (j + 1) * r_dim1] - r__[i__ + j * r_dim1]) * (dx * dy);
	  d__ =
	    (r__[i__ + 1 + (j + 1) * r_dim1] + r__[i__ + j * r_dim1] -
	     r__[i__ + 1 + j * r_dim1] - r__[i__ +
					     (j + 1) * r_dim1]) * (dx * dy);
	  c__[((i__ + j * c_dim3 << 2) + 3 << 2) + 3] =
	    a * 9. - b * 3. - cc * 3. + d__;
	  c__[((i__ + j * c_dim3 << 2) + 4 << 2) + 3] =
	    (a * -6. + b * 2. + cc * 3. - d__) * dy;
	  c__[((i__ + j * c_dim3 << 2) + 3 << 2) + 4] =
	    (a * -6. + b * 3. + cc * 2. - d__) * dx;
	  c__[((i__ + j * c_dim3 << 2) + 4 << 2) + 4] =
	    (a * 4. - b * 2. - cc * 2. + d__) * dx * dy;
	}
    }
  return 0;
}				/* coef_bicubic__ */

double
nsp_calpack_evalbicubic (double *xx, double *yy, double *xk, double *yk,
			 double *ck)
{
  /* System generated locals */
  double ret_val;

  /* Local variables */
  int i__;
  double u, dx, dy;

  /* Parameter adjustments */
  ck -= 5;

  /* Function Body */
  dx = *xx - *xk;
  dy = *yy - *yk;
  u = 0.;
  for (i__ = 4; i__ >= 1; --i__)
    {
      u =
	ck[i__ + 4] + dy * (ck[i__ + 8] +
			    dy * (ck[i__ + 12] + dy * ck[i__ + 16])) + u * dx;
    }
  ret_val = u;
  return ret_val;
}				/* evalbicubic_ */

/*function EvalBicubic 
 */
int
evalbicubic_with_grad_nsp_calpack_ (double *xx, double *yy, double *xk,
				    double *yk, double *ck, double *u,
				    double *dudx, double *dudy)
{
  int i__;
  double dx, dy;

  /* Parameter adjustments */
  ck -= 5;

  /* Function Body */
  dx = *xx - *xk;
  dy = *yy - *yk;
  *u = 0.;
  *dudx = 0.;
  *dudy = 0.;
  for (i__ = 4; i__ >= 1; --i__)
    {
      *u =
	ck[i__ + 4] + dy * (ck[i__ + 8] +
			    dy * (ck[i__ + 12] + dy * ck[i__ + 16])) +
	*u * dx;
      *dudx =
	ck[(i__ << 2) + 2] + dx * (ck[(i__ << 2) + 3] * 2. +
				   dx * 3. * ck[(i__ << 2) + 4]) + *dudx * dy;
      *dudy =
	ck[i__ + 8] + dy * (ck[i__ + 12] * 2. + dy * 3. * ck[i__ + 16]) +
	*dudy * dx;
    }
  return 0;
}				/* evalbicubic_with_grad__ */

/*subroutine EvalBicubic_with_grad 
 */
int
evalbicubic_with_grad_and_hes_nsp_calpack_ (double *xx, double *yy,
					    double *xk, double *yk,
					    double *ck, double *u,
					    double *dudx, double *dudy,
					    double *d2udx2, double *d2udxy,
					    double *d2udy2)
{
  int i__;
  double dx, dy;

  /* Parameter adjustments */
  ck -= 5;

  /* Function Body */
  dx = *xx - *xk;
  dy = *yy - *yk;
  *u = 0.;
  *dudx = 0.;
  *dudy = 0.;
  *d2udx2 = 0.;
  *d2udy2 = 0.;
  *d2udxy = 0.;
  for (i__ = 4; i__ >= 1; --i__)
    {
      *u =
	ck[i__ + 4] + dy * (ck[i__ + 8] +
			    dy * (ck[i__ + 12] + dy * ck[i__ + 16])) +
	*u * dx;
      *dudx =
	ck[(i__ << 2) + 2] + dx * (ck[(i__ << 2) + 3] * 2. +
				   dx * 3. * ck[(i__ << 2) + 4]) + *dudx * dy;
      *dudy =
	ck[i__ + 8] + dy * (ck[i__ + 12] * 2. + dy * 3. * ck[i__ + 16]) +
	*dudy * dx;
      *d2udx2 =
	ck[(i__ << 2) + 3] * 2. + dx * 6. * ck[(i__ << 2) + 4] + *d2udx2 * dy;
      *d2udy2 = ck[i__ + 12] * 2. + dy * 6. * ck[i__ + 16] + *d2udy2 * dx;
    }
  *d2udxy =
    ck[10] + dy * (ck[14] * 2. + dy * 3. * ck[18]) +
    dx * ((ck[11] + dy * (ck[15] * 2. + dy * 3. * ck[19])) * 2. +
	  dx * ((ck[12] + dy * (ck[16] * 2. + dy * 3. * ck[20])) * 3.));
  return 0;
}				/* evalbicubic_with_grad_and_hes__ */

/*subroutine EvalBicubic_with_grad_and_hes 
 */
int
nsp_calpack_bicubicinterp (double *x, double *y, double *c__, int *nx,
			   int *ny, double *x_eval__, double *y_eval__,
			   double *z_eval__, int *m, int *outmode)
{
  /* System generated locals */
  int c_dim3, c_offset, i__1;

  /* Local variables */
  int i__, j, k;
  double xx, yy;

  /* 
   *    PURPOSE 
   *       bicubic interpolation : 
   *         the grid is defined by x(1..nx), y(1..ny) 
   *         the known values are z(1..nx,1..ny), (z(i,j) being the value 
   *         at point (x(i),y(j))) 
   *         the interpolation is done on the points x_eval,y_eval(1..m) 
   *         z_eval(k) is the result of the bicubic interpolation of 
   *         (x_eval(k), y_eval(k)) 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --x;
  c_dim3 = *nx - 1;
  c_offset = ((c_dim3 + 1 << 2) + 1 << 2) + 1;
  c__ -= c_offset;
  --y;
  --z_eval__;
  --y_eval__;
  --x_eval__;

  /* Function Body */
  i__ = 0;
  j = 0;
  i__1 = *m;
  for (k = 1; k <= i__1; ++k)
    {
      xx = x_eval__[k];
      fast_int_search_nsp_calpack_ (&xx, &x[1], nx, &i__);
      yy = y_eval__[k];
      fast_int_search_nsp_calpack_ (&yy, &y[1], ny, &j);
      if (i__ != 0 && j != 0)
	{
	  z_eval__[k] =
	    nsp_calpack_evalbicubic (&xx, &yy, &x[i__], &y[j],
				     &c__[((i__ + j * c_dim3 << 2) +
					   1 << 2) + 1]);
	}
      else if (*outmode == 10 || nsp_calpack_isanan (&xx) == 1
	       || nsp_calpack_isanan (&yy) == 1)
	{
	  z_eval__[k] = return_a_nan_nsp_calpack_ ();
	}
      else if (*outmode == 7)
	{
	  z_eval__[k] = 0.;
	}
      else if (*outmode == 3)
	{
	  if (i__ == 0)
	    {
	      coord_by_periodicity_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      coord_by_periodicity_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  z_eval__[k] =
	    nsp_calpack_evalbicubic (&xx, &yy, &x[i__], &y[j],
				     &c__[((i__ + j * c_dim3 << 2) +
					   1 << 2) + 1]);
	}
      else if (*outmode == 8)
	{
	  if (i__ == 0)
	    {
	      near_grid_point_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      near_grid_point_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  z_eval__[k] =
	    nsp_calpack_evalbicubic (&xx, &yy, &x[i__], &y[j],
				     &c__[((i__ + j * c_dim3 << 2) +
					   1 << 2) + 1]);
	}
      else if (*outmode == 1)
	{
	  if (i__ == 0)
	    {
	      near_interval_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      near_interval_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  z_eval__[k] =
	    nsp_calpack_evalbicubic (&xx, &yy, &x[i__], &y[j],
				     &c__[((i__ + j * c_dim3 << 2) +
					   1 << 2) + 1]);
	}
    }
  return 0;
}				/* bicubicinterp_ */

int
nsp_calpack_bicubicinterpwithgrad (double *x, double *y, double *c__,
				   int *nx, int *ny, double *x_eval__,
				   double *y_eval__, double *z_eval__,
				   double *dzdx_eval__, double *dzdy_eval__,
				   int *m, int *outmode)
{
  /* System generated locals */
  int c_dim3, c_offset, i__1;

  /* Local variables */
  int i__, j, k;
  double xx, yy;
  int change_dzdx__, change_dzdy__;

  /* 
   *    PURPOSE 
   *       bicubic interpolation : 
   *         the grid is defined by x(1..nx), y(1..ny) 
   *         the known values are z(1..nx,1..ny), (z(i,j) being the value 
   *         at point (x(i),y(j))) 
   *         the interpolation is done on the points x_eval,y_eval(1..m) 
   *         z_eval(k) is the result of the bicubic interpolation of 
   *         (x_eval(k), y_eval(k)) and dzdx_eval(k), dzdy_eval(k) is the gradient 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --x;
  c_dim3 = *nx - 1;
  c_offset = ((c_dim3 + 1 << 2) + 1 << 2) + 1;
  c__ -= c_offset;
  --y;
  --dzdy_eval__;
  --dzdx_eval__;
  --z_eval__;
  --y_eval__;
  --x_eval__;

  /* Function Body */
  i__ = 0;
  j = 0;
  i__1 = *m;
  for (k = 1; k <= i__1; ++k)
    {
      xx = x_eval__[k];
      fast_int_search_nsp_calpack_ (&xx, &x[1], nx, &i__);
      yy = y_eval__[k];
      fast_int_search_nsp_calpack_ (&yy, &y[1], ny, &j);
      if (i__ != 0 && j != 0)
	{
	  evalbicubic_with_grad_nsp_calpack_ (&xx, &yy, &x[i__], &y[j],
					      &c__[((i__ + j * c_dim3 << 2) +
						    1 << 2) + 1],
					      &z_eval__[k], &dzdx_eval__[k],
					      &dzdy_eval__[k]);
	}
      else if (*outmode == 10 || nsp_calpack_isanan (&xx) == 1
	       || nsp_calpack_isanan (&yy) == 1)
	{
	  z_eval__[k] = return_a_nan_nsp_calpack_ ();
	  dzdx_eval__[k] = z_eval__[k];
	  dzdy_eval__[k] = z_eval__[k];
	}
      else if (*outmode == 7)
	{
	  z_eval__[k] = 0.;
	  dzdx_eval__[k] = 0.;
	  dzdy_eval__[k] = 0.;
	}
      else if (*outmode == 3)
	{
	  if (i__ == 0)
	    {
	      coord_by_periodicity_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      coord_by_periodicity_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  evalbicubic_with_grad_nsp_calpack_ (&xx, &yy, &x[i__], &y[j],
					      &c__[((i__ + j * c_dim3 << 2) +
						    1 << 2) + 1],
					      &z_eval__[k], &dzdx_eval__[k],
					      &dzdy_eval__[k]);
	}
      else if (*outmode == 8)
	{
	  if (i__ == 0)
	    {
	      near_grid_point_nsp_calpack_ (&xx, &x[1], nx, &i__);
	      change_dzdx__ = TRUE;
	    }
	  else
	    {
	      change_dzdx__ = FALSE;
	    }
	  if (j == 0)
	    {
	      near_grid_point_nsp_calpack_ (&yy, &y[1], ny, &j);
	      change_dzdy__ = TRUE;
	    }
	  else
	    {
	      change_dzdy__ = FALSE;
	    }
	  evalbicubic_with_grad_nsp_calpack_ (&xx, &yy, &x[i__], &y[j],
					      &c__[((i__ + j * c_dim3 << 2) +
						    1 << 2) + 1],
					      &z_eval__[k], &dzdx_eval__[k],
					      &dzdy_eval__[k]);
	  if (change_dzdx__)
	    {
	      dzdx_eval__[k] = 0.;
	    }
	  if (change_dzdy__)
	    {
	      dzdy_eval__[k] = 0.;
	    }
	}
      else if (*outmode == 1)
	{
	  if (i__ == 0)
	    {
	      near_interval_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      near_interval_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  evalbicubic_with_grad_nsp_calpack_ (&xx, &yy, &x[i__], &y[j],
					      &c__[((i__ + j * c_dim3 << 2) +
						    1 << 2) + 1],
					      &z_eval__[k], &dzdx_eval__[k],
					      &dzdy_eval__[k]);
	}
    }
  return 0;
}				/* bicubicinterpwithgrad_ */

int
nsp_calpack_bicubicinterpwithgradandhes (double *x, double *y, double *c__,
					 int *nx, int *ny, double *x_eval__,
					 double *y_eval__, double *z_eval__,
					 double *dzdx_eval__,
					 double *dzdy_eval__,
					 double *d2zdx2_eval__,
					 double *d2zdxy_eval__,
					 double *d2zdy2_eval__, int *m,
					 int *outmode)
{
  /* System generated locals */
  int c_dim3, c_offset, i__1;

  /* Local variables */
  int i__, j, k;
  double xx, yy;
  int change_dzdx__, change_dzdy__;

  /* 
   *    PURPOSE 
   *       bicubic interpolation : 
   *         the grid is defined by x(1..nx), y(1..ny) 
   *         the known values are z(1..nx,1..ny), (z(i,j) being the value 
   *         at point (x(i),y(j))) 
   *         the interpolation is done on the points x_eval,y_eval(1..m) 
   *         z_eval(k) is the result of the bicubic interpolation of 
   *         (x_eval(k), y_eval(k)), [dzdx_eval(k), dzdy_eval(k)] is the gradient 
   *         and [d2zdx2(k), d2zdxy(k), d2zdy2(k)] the Hessean 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --x;
  c_dim3 = *nx - 1;
  c_offset = ((c_dim3 + 1 << 2) + 1 << 2) + 1;
  c__ -= c_offset;
  --y;
  --d2zdy2_eval__;
  --d2zdxy_eval__;
  --d2zdx2_eval__;
  --dzdy_eval__;
  --dzdx_eval__;
  --z_eval__;
  --y_eval__;
  --x_eval__;

  /* Function Body */
  i__ = 0;
  j = 0;
  i__1 = *m;
  for (k = 1; k <= i__1; ++k)
    {
      xx = x_eval__[k];
      fast_int_search_nsp_calpack_ (&xx, &x[1], nx, &i__);
      yy = y_eval__[k];
      fast_int_search_nsp_calpack_ (&yy, &y[1], ny, &j);
      if (i__ != 0 && j != 0)
	{
	  evalbicubic_with_grad_and_hes_nsp_calpack_ (&xx, &yy, &x[i__],
						      &y[j],
						      &c__[((i__ +
							     j *
							     c_dim3 << 2) +
							    1 << 2) + 1],
						      &z_eval__[k],
						      &dzdx_eval__[k],
						      &dzdy_eval__[k],
						      &d2zdx2_eval__[k],
						      &d2zdxy_eval__[k],
						      &d2zdy2_eval__[k]);
	}
      else if (*outmode == 10 || nsp_calpack_isanan (&xx) == 1
	       || nsp_calpack_isanan (&yy) == 1)
	{
	  z_eval__[k] = return_a_nan_nsp_calpack_ ();
	  dzdx_eval__[k] = z_eval__[k];
	  dzdy_eval__[k] = z_eval__[k];
	  d2zdx2_eval__[k] = z_eval__[k];
	  d2zdxy_eval__[k] = z_eval__[k];
	  d2zdy2_eval__[k] = z_eval__[k];
	}
      else if (*outmode == 7)
	{
	  z_eval__[k] = 0.;
	  dzdx_eval__[k] = 0.;
	  dzdy_eval__[k] = 0.;
	  d2zdx2_eval__[k] = 0.;
	  d2zdxy_eval__[k] = 0.;
	  d2zdy2_eval__[k] = 0.;
	}
      else if (*outmode == 3)
	{
	  if (i__ == 0)
	    {
	      coord_by_periodicity_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      coord_by_periodicity_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  evalbicubic_with_grad_and_hes_nsp_calpack_ (&xx, &yy, &x[i__],
						      &y[j],
						      &c__[((i__ +
							     j *
							     c_dim3 << 2) +
							    1 << 2) + 1],
						      &z_eval__[k],
						      &dzdx_eval__[k],
						      &dzdy_eval__[k],
						      &d2zdx2_eval__[k],
						      &d2zdxy_eval__[k],
						      &d2zdy2_eval__[k]);
	}
      else if (*outmode == 8)
	{
	  if (i__ == 0)
	    {
	      near_grid_point_nsp_calpack_ (&xx, &x[1], nx, &i__);
	      change_dzdx__ = TRUE;
	    }
	  else
	    {
	      change_dzdx__ = FALSE;
	    }
	  if (j == 0)
	    {
	      near_grid_point_nsp_calpack_ (&yy, &y[1], ny, &j);
	      change_dzdy__ = TRUE;
	    }
	  else
	    {
	      change_dzdy__ = FALSE;
	    }
	  evalbicubic_with_grad_and_hes_nsp_calpack_ (&xx, &yy, &x[i__],
						      &y[j],
						      &c__[((i__ +
							     j *
							     c_dim3 << 2) +
							    1 << 2) + 1],
						      &z_eval__[k],
						      &dzdx_eval__[k],
						      &dzdy_eval__[k],
						      &d2zdx2_eval__[k],
						      &d2zdxy_eval__[k],
						      &d2zdy2_eval__[k]);
	  if (change_dzdx__)
	    {
	      dzdx_eval__[k] = 0.;
	      d2zdx2_eval__[k] = 0.;
	      d2zdxy_eval__[k] = 0.;
	    }
	  if (change_dzdy__)
	    {
	      dzdy_eval__[k] = 0.;
	      d2zdxy_eval__[k] = 0.;
	      d2zdy2_eval__[k] = 0.;
	    }
	}
      else if (*outmode == 1)
	{
	  if (i__ == 0)
	    {
	      near_interval_nsp_calpack_ (&xx, &x[1], nx, &i__);
	    }
	  if (j == 0)
	    {
	      near_interval_nsp_calpack_ (&yy, &y[1], ny, &j);
	    }
	  evalbicubic_with_grad_and_hes_nsp_calpack_ (&xx, &yy, &x[i__],
						      &y[j],
						      &c__[((i__ +
							     j *
							     c_dim3 << 2) +
							    1 << 2) + 1],
						      &z_eval__[k],
						      &dzdx_eval__[k],
						      &dzdy_eval__[k],
						      &d2zdx2_eval__[k],
						      &d2zdxy_eval__[k],
						      &d2zdy2_eval__[k]);
	}
    }
  return 0;
}				/* bicubicinterpwithgradandhes_ */

int
nsp_calpack_driverdb3val (double *xp, double *yp, double *zp, double *fp,
			  int *np, double *tx, double *ty, double *tz,
			  int *nx, int *ny, int *nz, int *kx, int *ky,
			  int *kz, double *bcoef, double *work, double *xmin,
			  double *xmax, double *ymin, double *ymax,
			  double *zmin, double *zmax, int *outmode)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int k;
  double x, y, z__;
  int flag_x__, flag_y__, flag_z__;

  /* 
   *    PURPOSE 
   *       driver on to db3val 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --fp;
  --zp;
  --yp;
  --xp;
  --tx;
  --ty;
  --tz;
  --bcoef;
  --work;

  /* Function Body */
  i__1 = *np;
  for (k = 1; k <= i__1; ++k)
    {
      x = xp[k];
      if (*xmin <= x && x <= *xmax)
	{
	  flag_x__ = TRUE;
	}
      else
	{
	  flag_x__ = FALSE;
	}
      y = yp[k];
      if (*ymin <= y && y <= *ymax)
	{
	  flag_y__ = TRUE;
	}
      else
	{
	  flag_y__ = FALSE;
	}
      z__ = zp[k];
      if (*zmin <= z__ && z__ <= *zmax)
	{
	  flag_z__ = TRUE;
	}
      else
	{
	  flag_z__ = FALSE;
	}
      if (flag_x__ && flag_y__ && flag_z__)
	{
	  fp[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	}
      else if (*outmode == 10 || nsp_calpack_isanan (&x) == 1
	       || nsp_calpack_isanan (&y) == 1
	       || nsp_calpack_isanan (&z__) == 1)
	{
	  fp[k] = return_a_nan_nsp_calpack_ ();
	}
      else if (*outmode == 7)
	{
	  fp[k] = 0.;
	}
      else
	{
	  if (*outmode == 3)
	    {
	      if (!flag_x__)
		{
		  proj_by_per_nsp_calpack_ (&x, xmin, xmax);
		}
	      if (!flag_y__)
		{
		  proj_by_per_nsp_calpack_ (&y, ymin, ymax);
		}
	      if (!flag_z__)
		{
		  proj_by_per_nsp_calpack_ (&z__, zmin, zmax);
		}
	    }
	  else if (*outmode == 8)
	    {
	      if (!flag_x__)
		{
		  proj_on_grid_nsp_calpack_ (&x, xmin, xmax);
		}
	      if (!flag_y__)
		{
		  proj_on_grid_nsp_calpack_ (&y, ymin, ymax);
		}
	      if (!flag_z__)
		{
		  proj_on_grid_nsp_calpack_ (&z__, zmin, zmax);
		}
	    }
	  fp[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	}
    }
  return 0;
}				/* driverdb3val_ */

int
nsp_calpack_driverdb3valwithgrad (double *xp, double *yp, double *zp,
				  double *fp, double *dfpdx, double *dfpdy,
				  double *dfpdz, int *np, double *tx,
				  double *ty, double *tz, int *nx, int *ny,
				  int *nz, int *kx, int *ky, int *kz,
				  double *bcoef, double *work, double *xmin,
				  double *xmax, double *ymin, double *ymax,
				  double *zmin, double *zmax, int *outmode)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int k;
  double x, y, z__;
  int flag_x__, flag_y__, flag_z__;

  /* 
   *    PURPOSE 
   *       driver on to db3val with gradient computing 
   * 
   *    various constant used in somespline.f 
   */
  /* Parameter adjustments */
  --dfpdz;
  --dfpdy;
  --dfpdx;
  --fp;
  --zp;
  --yp;
  --xp;
  --tx;
  --ty;
  --tz;
  --bcoef;
  --work;

  /* Function Body */
  i__1 = *np;
  for (k = 1; k <= i__1; ++k)
    {
      x = xp[k];
      if (*xmin <= x && x <= *xmax)
	{
	  flag_x__ = TRUE;
	}
      else
	{
	  flag_x__ = FALSE;
	}
      y = yp[k];
      if (*ymin <= y && y <= *ymax)
	{
	  flag_y__ = TRUE;
	}
      else
	{
	  flag_y__ = FALSE;
	}
      z__ = zp[k];
      if (*zmin <= z__ && z__ <= *zmax)
	{
	  flag_z__ = TRUE;
	}
      else
	{
	  flag_z__ = FALSE;
	}
      if (flag_x__ && flag_y__ && flag_z__)
	{
	  fp[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  dfpdx[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__1, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  dfpdy[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__1, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  dfpdz[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__1, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	}
      else if (*outmode == 10 || nsp_calpack_isanan (&x) == 1
	       || nsp_calpack_isanan (&y) == 1
	       || nsp_calpack_isanan (&z__) == 1)
	{
	  fp[k] = return_a_nan_nsp_calpack_ ();
	  dfpdx[k] = fp[k];
	  dfpdy[k] = fp[k];
	  dfpdz[k] = fp[k];
	}
      else if (*outmode == 7)
	{
	  fp[k] = 0.;
	  dfpdx[k] = 0.;
	  dfpdy[k] = 0.;
	  dfpdz[k] = 0.;
	}
      else if (*outmode == 3)
	{
	  if (!flag_x__)
	    {
	      proj_by_per_nsp_calpack_ (&x, xmin, xmax);
	    }
	  if (!flag_y__)
	    {
	      proj_by_per_nsp_calpack_ (&y, ymin, ymax);
	    }
	  if (!flag_z__)
	    {
	      proj_by_per_nsp_calpack_ (&z__, zmin, zmax);
	    }
	  fp[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  dfpdx[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__1, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  dfpdy[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__1, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  dfpdz[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__1, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	}
      else if (*outmode == 8)
	{
	  if (!flag_x__)
	    {
	      proj_on_grid_nsp_calpack_ (&x, xmin, xmax);
	    }
	  if (!flag_y__)
	    {
	      proj_on_grid_nsp_calpack_ (&y, ymin, ymax);
	    }
	  if (!flag_z__)
	    {
	      proj_on_grid_nsp_calpack_ (&z__, zmin, zmax);
	    }
	  fp[k] =
	    nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__0, &tx[1],
				&ty[1], &tz[1], nx, ny, nz, kx, ky, kz,
				&bcoef[1], &work[1]);
	  if (flag_x__)
	    {
	      dfpdx[k] = 0.;
	    }
	  else
	    {
	      dfpdx[k] =
		nsp_calpack_db3val (&x, &y, &z__, &c__1, &c__0, &c__0,
				    &tx[1], &ty[1], &tz[1], nx, ny, nz, kx,
				    ky, kz, &bcoef[1], &work[1]);
	    }
	  if (flag_y__)
	    {
	      dfpdy[k] = 0.;
	    }
	  else
	    {
	      dfpdy[k] =
		nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__1, &c__0,
				    &tx[1], &ty[1], &tz[1], nx, ny, nz, kx,
				    ky, kz, &bcoef[1], &work[1]);
	    }
	  if (flag_z__)
	    {
	      dfpdz[k] = 0.;
	    }
	  else
	    {
	      dfpdz[k] =
		nsp_calpack_db3val (&x, &y, &z__, &c__0, &c__0, &c__1,
				    &tx[1], &ty[1], &tz[1], nx, ny, nz, kx,
				    ky, kz, &bcoef[1], &work[1]);
	    }
	}
    }
  return 0;
}				/* driverdb3valwithgrad_ */
#endif
