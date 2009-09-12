#include "integ.h"

/* Common Block Declarations */

#define ls0001_1 ls0001_._1



static int c__0 = 0;


int
nsp_ode_prepji (int *neq, double *y, double *yh, int *nyh,
		   double *ewt, double *rtem, double *savr,
		   double *s, double *wm, int *iwm, lsodi_res res,
		   Dgbydy jac, ode_jac adda, void *param)
{
  /* System generated locals */
  int yh_dim1, yh_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2;

  /* Local variables */
  int lenp, ires;
  double srur;
  int i__, j, mband;
  double r__;
  int i1, i2, j1, ii, jj, meband, ml, mu;
  double yi, yj, hl0;
  int ml3;
  double fac;
  int mba, ier;
  double con, yjj;
  int meb1;

  /*lll. optimize 
   *----------------------------------------------------------------------- 
   *%purpose 
   *prepji is called by stodi to compute and process the matrix 
   *p = a - h*el(1)*j , where j is an approximation to the jacobian dr/dy, 
   *where r = g(t,y) - a(t,y)*s. here j is computed by the user-supplied 
   *routine jac if miter = 1 or 4, or by finite differencing if miter = 
   *2 or 5. j is stored in wm, rescaled, and adda is called to generate 
   *p. p is then subjected to lu decomposition in preparation 
   *for later solution of linear systems with p as coefficient 
   *matrix.  this is done by dgefa if miter = 1 or 2, and by 
   *dgbfa if miter = 4 or 5. 
   * 
   *%additional parameters 
   *in addition to variables described previously, communication 
   *with prepji uses the following.. 
   *y     = array containing predicted values on entry. 
   *rtem  = work array of length n (acor in stodi). 
   *savr  = array used for output only.  on output it contains the 
   *        residual evaluated at current values of t and y. 
   *s     = array containing predicted values of dy/dt (savf in stodi). 
   *wm    = real work space for matrices.  on output it contains the 
   *        lu decomposition of p. 
   *        storage of matrix elements starts at wm(3). 
   *        wm also contains the following matrix-related data.. 
   *        wm(1) = sqrt(uround), used in numerical jacobian increments. 
   *iwm   = int work space containing pivot information, starting at 
   *        iwm(21).  iwm also contains the band parameters 
   *        ml = iwm(1) and mu = iwm(2) if miter is 4 or 5. 
   *el0   = el(1) (input). 
   *ierpj = output error flag. 
   *        = 0 if no trouble occurred, 
   *        = 1 if the p matrix was found to be singular, 
   *        = ires (= 2 or 3) if res returned ires = 2 or 3. 
   *jcur  = output flag = 1 to indicate that the jacobian matrix 
   *        (or approximation) is now current. 
   *this routine also uses the common variables el0, h, tn, uround, 
   *miter, n, nfe, and nje. 
   *! 
   *----------------------------------------------------------------------- 
   */
  /* Parameter adjustments */
  --neq;
  --y;
  yh_dim1 = *nyh;
  yh_offset = yh_dim1 + 1;
  yh -= yh_offset;
  --ewt;
  --rtem;
  --savr;
  --s;
  --wm;
  --iwm;

  /* Function Body */
  ++ls0001_1.nje;
  hl0 = ls0001_1.h__ * ls0001_1.el0;
  ls0001_1.ierpj = 0;
  ls0001_1.jcur = 1;
  switch (ls0001_1.miter)
    {
    case 1:
      goto L100;
    case 2:
      goto L200;
    case 3:
      goto L300;
    case 4:
      goto L400;
    case 5:
      goto L500;
    }
  /*if miter = 1, call res, then jac, and multiply by scalar. ------------ 
   */
 L100:
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires > 1)
    {
      goto L600;
    }
  lenp = ls0001_1.n * ls0001_1.n;
  i__1 = lenp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L110: */
      wm[i__ + 2] = 0.;
    }
  (*jac) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &c__0, &c__0, &wm[3],
	  &ls0001_1.n);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  con = -hl0;
  i__1 = lenp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L120: */
      wm[i__ + 2] *= con;
    }
  goto L240;
  /*if miter = 2, make n + 1 calls to res to approximate j. -------------- 
   */
 L200:
  ires = -1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires > 1)
    {
      goto L600;
    }
  srur = wm[1];
  j1 = 2;
  i__1 = ls0001_1.n;
  for (j = 1; j <= i__1; ++j)
    {
      yj = y[j];
      /*Computing MAX 
       */
      d__1 = srur * Abs (yj), d__2 = .01 / ewt[j];
      r__ = Max (d__1, d__2);
      y[j] += r__;
      fac = -hl0 / r__;
      (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &rtem[1], &ires);
      if (ierode_1.iero > 0)
	{
	  return 0;
	}
      ++ls0001_1.nfe;
      if (ires > 1)
	{
	  goto L600;
	}
      i__2 = ls0001_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L220: */
	  wm[i__ + j1] = (rtem[i__] - savr[i__]) * fac;
	}
      y[j] = yj;
      j1 += ls0001_1.n;
      /* L230: */
    }
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires > 1)
    {
      goto L600;
    }
  /*add matrix a. -------------------------------------------------------- 
   */
 L240:
  (*adda) (&neq[1], &ls0001_1.tn, &y[1], &c__0, &c__0, &wm[3], &ls0001_1.n, param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /*do lu decomposition on p. -------------------------------------------- 
   */
  C2F(dgefa) (&wm[3], &ls0001_1.n, &ls0001_1.n, &iwm[21], &ier);
  if (ier != 0)
    {
      ls0001_1.ierpj = 1;
    }
  return 0;
  /*dummy section for miter = 3 
   */
 L300:
  return 0;
  /*if miter = 4, call res, then jac, and multiply by scalar. ------------ 
   */
 L400:
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires > 1)
    {
      goto L600;
    }
  ml = iwm[1];
  mu = iwm[2];
  /*c mod 06-01-89 
   *c      ml3 = ml + 3 
   */
  ml3 = 3;
  /*c fin 
   */
  mband = ml + mu + 1;
  meband = mband + ml;
  lenp = meband * ls0001_1.n;
  i__1 = lenp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L410: */
      wm[i__ + 2] = 0.;
    }
  (*jac) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &ml, &mu, &wm[ml3], &meband);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  con = -hl0;
  i__1 = lenp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L420: */
      wm[i__ + 2] *= con;
    }
  goto L570;
  /*if miter = 5, make ml + mu + 2 calls to res to approximate j. -------- 
   */
 L500:
  ires = -1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires > 1)
    {
      goto L600;
    }
  ml = iwm[1];
  mu = iwm[2];
  ml3 = ml + 3;
  mband = ml + mu + 1;
  mba = Min (mband, ls0001_1.n);
  meband = mband + ml;
  meb1 = meband - 1;
  srur = wm[1];
  i__1 = mba;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = ls0001_1.n;
      i__3 = mband;
      for (i__ = j; i__3 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__3)
	{
	  yi = y[i__];
	  /*Computing MAX 
	   */
	  d__1 = srur * Abs (yi), d__2 = .01 / ewt[i__];
	  r__ = Max (d__1, d__2);
	  /* L530: */
	  y[i__] += r__;
	}
      (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &rtem[1], &ires);
      if (ierode_1.iero > 0)
	{
	  return 0;
	}
      ++ls0001_1.nfe;
      if (ires > 1)
	{
	  goto L600;
	}
      i__3 = ls0001_1.n;
      i__2 = mband;
      for (jj = j; i__2 < 0 ? jj >= i__3 : jj <= i__3; jj += i__2)
	{
	  y[jj] = yh[jj + yh_dim1];
	  yjj = y[jj];
	  /*Computing MAX 
	   */
	  d__1 = srur * Abs (yjj), d__2 = .01 / ewt[jj];
	  r__ = Max (d__1, d__2);
	  fac = -hl0 / r__;
	  /*Computing MAX 
	   */
	  i__4 = jj - mu;
	  i1 = Max (i__4, 1);
	  /*Computing MIN 
	   */
	  i__4 = jj + ml;
	  i2 = Min (i__4, ls0001_1.n);
	  ii = jj * meb1 - ml + 2;
	  i__4 = i2;
	  for (i__ = i1; i__ <= i__4; ++i__)
	    {
	      /* L540: */
	      wm[ii + i__] = (rtem[i__] - savr[i__]) * fac;
	    }
	  /* L550: */
	}
      /* L560: */
    }
  ires = 1;
  (*res) (&neq[1], &ls0001_1.tn, &y[1], &s[1], &savr[1], &ires);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  if (ires > 1)
    {
      goto L600;
    }
  /*add matrix a. -------------------------------------------------------- 
   */
 L570:
  (*adda) (&neq[1], &ls0001_1.tn, &y[1], &ml, &mu, &wm[ml3], &meband, param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  /*do lu decomposition of p. -------------------------------------------- 
   */
  nsp_ode_dgbfa (&wm[3], &meband, &ls0001_1.n, &ml, &mu, &iwm[21], &ier);
  if (ier != 0)
    {
      ls0001_1.ierpj = 1;
    }
  return 0;
  /*error return for ires = 2 or ires = 3 return from res. --------------- 
   */
 L600:
  ls0001_1.ierpj = ires;
  return 0;
  /*----------------------- end of subroutine prepji ---------------------- 
   */
}				/* nsp_ode_prepji */
