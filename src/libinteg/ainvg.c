#include "integ.h"

/* Table of constant values */

static int c__0 = 0;

/*
 * this subroutine computes the initial value 
 * of the vector ydot satisfying 
 * a * ydot = g(t,y) 
 * when a is nonsingular.  it is called by lsodi for 
 * initialization only, when istate = 0 . 
 * ainvg returns an error flag ier.. 
 *  ier  =  0  means ainvg was successful. 
 *  ier .ge. 2 means res returned an error flag ires = ier. 
 *  ier .lt. 0 means the a-matrix was found to be singular. 
 */

int nsp_ode_ainvg (lsodi_res res, ode_jac adda, int *neq, double *t, double *y,
		      double *ydot, int *miter, int *ml, int *mu, double *pw,
		      int *ipvt, int *ier,void * param)
{
  int i__1;
  int i__;
  int lenpw, nrowpw, mlp1;

  /* Parameter adjustments */

  --ipvt;
  --pw;
  --ydot;
  --y;

  /* Function Body */
  if (*miter >= 4)
    {
      goto L100;
    }
  /* 
   *full matrix case ----------------------------------------------------- 
   * 
   */
  lenpw = *neq * *neq;
  i__1 = lenpw;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L10: */
      pw[i__] = 0.;
    }
  /* 
   */
  *ier = 1;
  (*res) (neq, t, &y[1], &pw[1], &ydot[1], ier);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  if (*ier > 1)
    {
      return 0;
    }
  /* 
   */
  (*adda) (neq, t, &y[1], &c__0, &c__0, &pw[1], neq, param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  C2F(dgefa) (&pw[1], neq, neq, &ipvt[1], ier);
  if (*ier == 0)
    {
      goto L20;
    }
  *ier = -(*ier);
  return 0;

 L20:
  C2F(dgesl) (&pw[1], neq, neq, &ipvt[1], &ydot[1], &c__0);
  return 0;
  /* 
   *band matrix case ----------------------------------------------------- 
   * 
   */
 L100:
  nrowpw = (*ml << 1) + *mu + 1;
  lenpw = *neq * nrowpw;
  i__1 = lenpw;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L110: */
      pw[i__] = 0.;
    }
  /* 
   */
  *ier = 1;
  (*res) (neq, t, &y[1], &pw[1], &ydot[1], ier);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  if (*ier > 1)
    {
      return 0;
    }
  /* 
   */
  mlp1 = *ml + 1;
  (*adda) (neq, t, &y[1], ml, mu, &pw[mlp1], &nrowpw, param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  nsp_ode_dgbfa (&pw[1], &nrowpw, neq, ml, mu, &ipvt[1], ier);
  if (*ier == 0)
    {
      goto L120;
    }
  *ier = -(*ier);
  return 0;
 L120:
  nsp_ode_dgbsl (&pw[1], &nrowpw, neq, ml, mu, &ipvt[1], &ydot[1], &c__0);
  return 0;
}
