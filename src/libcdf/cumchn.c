/* ********************************************************************** */
/*     SUBROUTINE CUMCHN(X,DF,PNONC,CUM,CCUM) */
/*             CUMulative of the Non-central CHi-square distribution */
/*                              Function */
/*     Calculates     the       cumulative      non-central    chi-square */
/*     distribution, i.e.,  the probability   that  a   random   variable */
/*     which    follows  the  non-central chi-square  distribution,  with */
/*     non-centrality  parameter    PNONC  and   continuous  degrees   of */
/*     freedom DF, is less than or equal to X. */
/*                              Arguments */
/*     X       --> Upper limit of integration of the non-central */
/*                 chi-square distribution. */
/*                                                 X is DOUBLE PRECISION */
/*     DF      --> Degrees of freedom of the non-central */
/*                 chi-square distribution. */
/*                                                 DF is DOUBLE PRECISION */
/*     PNONC   --> Non-centrality parameter of the non-central */
/*                 chi-square distribution. */
/*                                                 PNONC is DOUBLE PRECIS */
/*     CUM <-- Cumulative non-central chi-square distribution. */
/*                                                 CUM is DOUBLE PRECISIO */
/*     CCUM <-- Compliment of Cumulative non-central chi-square distribut */
/*                                                 CCUM is DOUBLE PRECISI */
/*                              Method */
/*     Uses  formula  26.4.25   of  Abramowitz  and  Stegun, Handbook  of */
/*     Mathematical    Functions,  US   NBS   (1966)    to calculate  the */
/*     non-central chi-square. */
/*                              Variables */
/*     EPS     --- Convergence criterion.  The sum stops when a */
/*                 term is less than EPS*SUM. */
/*                                                 EPS is DOUBLE PRECISIO */
/*     NTIRED  --- Maximum number of terms to be evaluated */
/*                 in each sum. */
/*                                                 NTIRED is INT */
/*     QCONV   --- .TRUE. if convergence achieved - */
/*                 i.e., program did not stop on NTIRED criterion. */
/*                                                 QCONV is INT */
/*     CCUM <-- Compliment of Cumulative non-central */
/*              chi-square distribution. */
/*                                                 CCUM is DOUBLE PRECISI */
/* ********************************************************************** */

#include "cdf.h"

int cdf_cumchn (double *x, double *df, double *pnonc, double *cum, double *ccum)
{
  const int ntired = 1000;
  const double eps = 1e-5;
  double d__1;
  double term, chid2;
  int i__;
  double lfact;
  int icent, iterb;
  double pcent;
  int iterf;
  double xnonc, pterm;
  double centaj;
  double lcntaj, wt, sumadj, centwt, lcntwt, adj, sum, dfd2;

  if (!(*x <= 0.))
    {
      goto L10;
    }
  *cum = 0.;
  *ccum = 1.;
  return 0;
L10:
  if (!(*pnonc <= 1e-10))
    {
      goto L20;
    }


/*     When non-centrality parameter is (essentially) zero, */
/*     use cumulative chi-square distribution */


  cdf_cumchi (x, df, cum, ccum);
  return 0;
L20:
  xnonc = *pnonc / 2.;
/* ********************************************************************** */
/*     The following code calcualtes the weight, chi-square, and */
/*     adjustment term for the central term in the infinite series. */
/*     The central term is the one in which the poisson weight is */
/*     greatest.  The adjustment term is the amount that must */
/*     be subtracted from the chi-square to move up two degrees */
/*     of freedom. */
/* ********************************************************************** */
  icent = (int) xnonc;
  if (icent == 0)
    {
      icent = 1;
    }
  chid2 = *x / 2.;


/*     Calculate central weight term */


  d__1 = (double) (icent + 1);
  lfact = cdf_alngam (&d__1);
  lcntwt = -xnonc + icent * log (xnonc) - lfact;
  centwt = exp (lcntwt);


/*     Calculate central chi-square */


  d__1 = *df + (double) icent *2.;
  cdf_cumchi (x, &d__1, &pcent, ccum);


/*     Calculate central adjustment term */


  dfd2 = (*df + (double) icent * 2.) / 2.;
  d__1 = dfd2 + 1.;
  lfact = cdf_alngam (&d__1);
  lcntaj = dfd2 * log (chid2) - chid2 - lfact;
  centaj = exp (lcntaj);
  sum = centwt * pcent;
/* ********************************************************************** */

/*     Sum backwards from the central term towards zero. */
/*     Quit whenever either */
/*     (1) the zero term is reached, or */
/*     (2) the term gets small relative to the sum, or */
/*     (3) More than NTIRED terms are totaled. */

/* ********************************************************************** */
  iterb = 0;
  sumadj = 0.;
  adj = centaj;
  wt = centwt;
  i__ = icent;

  goto L40;
L30:
  if (iterb > ntired || (sum < 1e-20 || term < eps * sum) || i__ == 0)
    {
      goto L50;
    }
L40:
  dfd2 = (*df + (double) i__ * 2.) / 2.;


/*     Adjust chi-square for two fewer degrees of freedom. */
/*     The adjusted value ends up in PTERM. */


  adj = adj * dfd2 / chid2;
  sumadj += adj;
  pterm = pcent + sumadj;


/*     Adjust poisson weight for J decreased by one */


  wt *= i__ / xnonc;
  term = wt * pterm;
  sum += term;
  --i__;
  ++iterb;
  goto L30;
L50:
  iterf = 0;
/* ********************************************************************** */

/*     Now sum forward from the central term towards infinity. */
/*     Quit when either */
/*     (1) the term gets small relative to the sum, or */
/*     (2) More than NTIRED terms are totaled. */

/* ********************************************************************** */
  sumadj = centaj;
  adj = centaj;
  wt = centwt;
  i__ = icent;

  goto L70;
L60:
  if (iterf > ntired || (sum < 1e-20 || term < eps * sum))
    {
      goto L80;
    }


/*     Update weights for next higher J */


L70:
  wt *= xnonc / (i__ + 1);


/*     Calculate PTERM and add term to sum */


  pterm = pcent - sumadj;
  term = wt * pterm;
  sum += term;


/*     Update adjustment term for DF for next iteration */


  ++i__;
  dfd2 = (*df + (double) i__ * 2.) / 2.;
  adj = adj * chid2 / dfd2;
  sumadj += adj;
  ++iterf;
  goto L60;
L80:
  *cum = sum;
  *ccum = .5 - *cum + .5;

  return 0;
}				/* cumchn_ */
