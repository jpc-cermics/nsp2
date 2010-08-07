#include "integ.h"
#include "nsp/lapack-c.h"

/* Common Block Declarations */


#define ls0001_1 ls0001_._1

/* Table of constant values */

/*
 *this routine manages the solution of the linear system arising from 
 *a chord iteration.  it is called if miter .ne. 0. 
 *if miter is 1 or 2, it calls dgesl to accomplish this. 
 * Note: dgesl replaced bu dgetrs (Bruno 6/08/2010)
 *if miter = 3 it updates the coefficient h*el0 in the diagonal 
 *matrix, and then computes the solution. 
 *if miter is 4 or 5, it calls dgbsl. 
 * Note: dgbsl replaced bu dgbtrs (Bruno 6/08/2010)
 *%calling sequence 
 *communication with solsy uses the following variables.. 
 *wm    = real work space containing the inverse diagonal matrix if 
 *        miter = 3 and the lu decomposition of the matrix otherwise. 
 *        storage of matrix elements starts at wm(3). 
 *        wm also contains the following matrix-related data.. 
 *        wm(1) = sqrt(uround) (not used here), 
 *        wm(2) = hl0, the previous value of h*el0, used if miter = 3. 
 *iwm   = int work space containing pivot information, starting at 
 *        iwm(21), if miter is 1, 2, 4, or 5.  iwm also contains band 
 *        parameters ml = iwm(1) and mu = iwm(2) if miter is 4 or 5. 
 *x     = the right-hand side vector on input, and the solution vector 
 *        on output, of length n. 
 *tem   = vector of work space of length n, not used in this version. 
 *iersl = output flag (in common).  iersl = 0 if no trouble occurred. 
 *        iersl = 1 if a singular matrix arose with miter = 3. 
 *this routine also uses the common variables el0, h, miter, and n. 
 */

int nsp_ode_solsy (double *wm, int *iwm, double *x, double *tem)
{
  int i1, i;
  double r, di, hl0, phl0;
  int meband, ml, mu;
  int one=1, rien;

  /* Parameter adjustments */
  --tem;
  --x;
  --iwm;
  --wm;

  /* Function Body */
  ls0001_1.iersl = 0;
  switch (ls0001_1.miter)
    {
    case 1:
      goto L100;
    case 2:
      goto L100;
    case 3:
      goto L300;
    case 4:
      goto L400;
    case 5:
      goto L400;
    }
 L100:
/*   C2F(dgesl) (&wm[3], &ls0001_1.n, &ls0001_1.n, &iwm[21], &x[1], &c0); */
/*  Note dgesl replaced by dgetrs (Bruno 6/08/2010) */
  C2F(dgetrs) ("N", &ls0001_1.n, &one, &wm[3], &ls0001_1.n, &iwm[21], &x[1], &ls0001_1.n, &rien, 1);
  return 0;
  /* 
   */
 L300:
  phl0 = wm[2];
  hl0 = ls0001_1.h__ * ls0001_1.el0;
  wm[2] = hl0;
  if (hl0 == phl0)
    {
      goto L330;
    }
  r = hl0 / phl0;
  i1 = ls0001_1.n;
  for (i = 1; i <= i1; ++i)
    {
      di = 1. - r * (1. - 1. / wm[i + 2]);
      if (Abs (di) == 0.)
	{
	  goto L390;
	}
      /* L320: */
      wm[i + 2] = 1. / di;
    }
 L330:
  i1 = ls0001_1.n;
  for (i = 1; i <= i1; ++i)
    {
      /* L340: */
      x[i] = wm[i + 2] * x[i];
    }
  return 0;
 L390:
  ls0001_1.iersl = 1;
  return 0;
  /* 
   */
 L400:
  ml = iwm[1];
  mu = iwm[2];
  meband = (ml << 1) + mu + 1;
/*   nsp_ode_dgbsl (&wm[3], &meband, &ls0001_1.n, &ml, &mu, &iwm[21], &x[1], &c0); */
/*  Note dgbsl replaced by dgbtrs (Bruno 6/08/2010) */
  C2F(dgbtrs)("N", &ls0001_1.n, &ml, &mu, &one, &wm[3], &meband, &iwm[21], &x[1], &ls0001_1.n, &rien,1);
  return 0;
}
