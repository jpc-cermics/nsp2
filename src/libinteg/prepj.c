#include "integ.h"

#define ls0001_1 ls0001_._1

/*
 *  prepj is called by stode to compute and process the matrix 
 *  p = i - h*  el(1)*  j , where j is an approximation to the jacobian. 
 *  here j is computed by the user-supplied routine jac if 
 *  miter = 1 or 4, or by finite differencing if miter = 2, 3, or 5. 
 *  if miter = 3, a diagonal approximation to j is used. 
 *  j is stored in wm and replaced by p.  if miter .ne. 3, p is then 
 *  subjected to lu decomposition in preparation for later solution 
 *  of linear systems with p as coefficient matrix. this is done 
 *  by dgefa if miter = 1 or 2, and by dgbfa if miter = 4 or 5. 
 *   
 *  %additonal parameters 
 *  in addition to variables described previously, communication 
 *  with prepj uses the following.. 
 *  y     = array containing predicted values on entry. 
 *  ftem  = work array of length n (acor in stode). 
 *  savf  = array containing f evaluated at predicted y. 
 *  wm    = real work space for matrices.  on output it contains the 
 *          inverse diagonal matrix if miter = 3 and the lu decomposition 
 *          of p if miter is 1, 2 , 4, or 5. 
 *          storage of matrix elements starts at wm(3). 
 *          wm also contains the following matrix-related data.. 
 *          wm(1) = sqrt(uround), used in numerical jacobian increments. 
 *          wm(2) = h*el0, saved for later use if miter = 3. 
 *  iwm   = int work space containing pivot information, starting at 
 *          iwm(21), if miter is 1, 2, 4, or 5.  iwm also contains band 
 *          parameters ml = iwm(1) and mu = iwm(2) if miter is 4 or 5. 
 *  el0   = el(1) (input). 
 *  ierpj = output error flag,  = 0 if no trouble, .gt. 0 if 
 *          p matrix found to be singular. 
 *  jcur  = output flag = 1 to indicate that the jacobian matrix 
 *          (or approximation) is now current. 
 *  this routine also uses the common variables el0, h, tn, uround, 
 *  miter, n, nfe, and nje. 
 */

int
nsp_ode_prepj (int *neq, double *y, double *yh, int *nyh, double *ewt,
		  double *ftem, double *savf, double *wm, int *iwm, ode_f f,
		  ode_jac jac, void *param)
{
  int c_n1 = -1;
  int yh_dim1, yh_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2;
  /* Local variables */

  double con, yjj, fac, r0, di, r__, srur, yi, yj, hl0;
  int i1, i2, j1;
  int i__, j, mband;
  int ii, jj, meband, ml, mu;
  int lenp;
  int mba, ier;
  int meb1;
  int ml3;
  /* Parameter adjustments */
  --neq;
  --y;
  yh_dim1 = *nyh;
  yh_offset = yh_dim1 + 1;
  yh -= yh_offset;
  --ewt;
  --ftem;
  --savf;
  --wm;
  --iwm;

  /* Function Body */
  ++ls0001_1.nje;
  ls0001_1.ierpj = 0;
  ls0001_1.jcur = 1;
  hl0 = ls0001_1.h__ * ls0001_1.el0;
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
  /*if miter = 1, call jac and multiply by scalar. ----------------------- 
   */
 L100:
  lenp = ls0001_1.n * ls0001_1.n;
  i__1 = lenp;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L110: */
      wm[i__ + 2] = 0.;
    }
  (*jac) (&neq[1], &ls0001_1.tn, &y[1], &c_n1, &c_n1, &wm[3], &ls0001_1.n, param);
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
  /*if miter = 2, make n calls to f to approximate j. -------------------- 
   */
 L200:
  fac = nsp_ode_vnorm (&ls0001_1.n, &savf[1], &ewt[1]);
  r0 = Abs (ls0001_1.h__) * 1e3 * ls0001_1.uround * (double) ls0001_1.n * fac;
  if (r0 == 0.)
    {
      r0 = 1.;
    }
  srur = wm[1];
  j1 = 2;
  i__1 = ls0001_1.n;
  for (j = 1; j <= i__1; ++j)
    {
      yj = y[j];
      /*Computing MAX 
       */
      d__1 = srur * Abs (yj), d__2 = r0 / ewt[j];
      r__ = Max (d__1, d__2);
      y[j] += r__;
      fac = -hl0 / r__;
      (*f) (&neq[1], &ls0001_1.tn, &y[1], &ftem[1], param);
      if (ierode_1.iero > 0)
	{
	  return 0;
	}
      i__2 = ls0001_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L220: */
	  wm[i__ + j1] = (ftem[i__] - savf[i__]) * fac;
	}
      y[j] = yj;
      j1 += ls0001_1.n;
      /* L230: */
    }
  ls0001_1.nfe += ls0001_1.n;
  /*add identity matrix. ------------------------------------------------- 
   */
 L240:
  j = 3;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      wm[j] += 1.;
      /* L250: */
      j += ls0001_1.n + 1;
    }
  /*do lu decomposition on p. -------------------------------------------- 
   */
  C2F(dgefa) (&wm[3], &ls0001_1.n, &ls0001_1.n, &iwm[21], &ier);
  if (ier != 0)
    {
      ls0001_1.ierpj = 1;
    }
  return 0;
  /*if miter = 3, construct a diagonal approximation to j and p. --------- 
   */
 L300:
  wm[2] = hl0;
  r__ = ls0001_1.el0 * .1;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L310: */
      y[i__] += r__ * (ls0001_1.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)]);
    }
  (*f) (&neq[1], &ls0001_1.tn, &y[1], &wm[3], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++ls0001_1.nfe;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      r0 = ls0001_1.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)];
      di = r0 * .1 - ls0001_1.h__ * (wm[i__ + 2] - savf[i__]);
      wm[i__ + 2] = 1.;
      if (Abs (r0) < ls0001_1.uround / ewt[i__])
	{
	  goto L320;
	}
      if (Abs (di) == 0.)
	{
	  goto L330;
	}
      wm[i__ + 2] = r0 * .1 / di;
    L320:
      ;
    }
  return 0;
 L330:
  ls0001_1.ierpj = 1;
  return 0;
  /*if miter = 4, call jac and multiply by scalar. ----------------------- 
   */
 L400:
  ml = iwm[1];
  mu = iwm[2];
  /* XXX mod 06-01-89 
   *  ml3 = ml + 3 
   * changed to ml3 = 3
   */
  ml3 = ml+ 3;
  /*
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
  (*jac) (&neq[1], &ls0001_1.tn, &y[1], &ml, &mu, &wm[ml3], &meband, param);
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
  /*if miter = 5, make mband calls to f to approximate j. ---------------- 
   */
 L500:
  ml = iwm[1];
  mu = iwm[2];
  mband = ml + mu + 1;
  mba = Min (mband, ls0001_1.n);
  meband = mband + ml;
  meb1 = meband - 1;
  srur = wm[1];
  fac = nsp_ode_vnorm (&ls0001_1.n, &savf[1], &ewt[1]);
  r0 = Abs (ls0001_1.h__) * 1e3 * ls0001_1.uround * (double) ls0001_1.n * fac;
  if (r0 == 0.)
    {
      r0 = 1.;
    }
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
	  d__1 = srur * Abs (yi), d__2 = r0 / ewt[i__];
	  r__ = Max (d__1, d__2);
	  /* L530: */
	  y[i__] += r__;
	}
      (*f) (&neq[1], &ls0001_1.tn, &y[1], &ftem[1], param);
      if (ierode_1.iero > 0)
	{
	  return 0;
	}
      i__3 = ls0001_1.n;
      i__2 = mband;
      for (jj = j; i__2 < 0 ? jj >= i__3 : jj <= i__3; jj += i__2)
	{
	  y[jj] = yh[jj + yh_dim1];
	  yjj = y[jj];
	  /*Computing MAX 
	   */
	  d__1 = srur * Abs (yjj), d__2 = r0 / ewt[jj];
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
	      wm[ii + i__] = (ftem[i__] - savf[i__]) * fac;
	    }
	  /* L550: */
	}
      /* L560: */
    }
  ls0001_1.nfe += mba;
  /*add identity matrix. ------------------------------------------------- 
   */
 L570:
  ii = mband + 2;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      wm[ii] += 1.;
      /* L580: */
      ii += meband;
    }
  /*do lu decomposition of p. -------------------------------------------- 
   */
  nsp_ode_dgbfa (&wm[3], &meband, &ls0001_1.n, &ml, &mu, &iwm[21], &ier);
  if (ier != 0)
    {
      ls0001_1.ierpj = 1;
    }
  return 0;
}

