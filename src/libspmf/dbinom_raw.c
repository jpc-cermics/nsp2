/*
 * The author of this software is Catherine Loader, c@herine.net
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, with the exceptions noted below,
 * and provided that this entire notice is included in all copies of any
 * software which is or includes a copy or modification of this software
 * and in all copies of the supporting documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR LUCENT TECHNOLOGIES
 * MAKE ANY REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE
 * MERCHANTABILITY OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 * modifs (Bruno Pincon) for inclusion in nsp: 
 * - this file contains the original bd0.c stirlerr.c and raw.c files
 *   of Catherine Loader 's dbinom package.
 * - add a nsp_ prefix for the names
 * - the dbinom.h header file is in the nsp/spmf.h
 */

#include <nsp/math.h> 
#include <nsp/spmf.h>

/* stirlerr(n) = log(n!) - log( sqrt(2*pi*n)*(n/e)^n ) */

#define S0 0.083333333333333333333       /* 1/12 */
#define S1 0.00277777777777777777778     /* 1/360 */
#define S2 0.00079365079365079365079365  /* 1/1260 */
#define S3 0.000595238095238095238095238 /* 1/1680 */
#define S4 0.0008417508417508417508417508 /* 1/1188 */

/*
  error for 0, 0.5, 1.0, 1.5, ..., 14.5, 15.0.
*/
static double sferr_halves[31] = {
0.0, /* n=0 - wrong, place holder only */
0.1534264097200273452913848,  /* 0.5 */
0.0810614667953272582196702,  /* 1.0 */
0.0548141210519176538961390,  /* 1.5 */
0.0413406959554092940938221,  /* 2.0 */
0.03316287351993628748511048, /* 2.5 */
0.02767792568499833914878929, /* 3.0 */
0.02374616365629749597132920, /* 3.5 */
0.02079067210376509311152277, /* 4.0 */
0.01848845053267318523077934, /* 4.5 */
0.01664469118982119216319487, /* 5.0 */
0.01513497322191737887351255, /* 5.5 */
0.01387612882307074799874573, /* 6.0 */
0.01281046524292022692424986, /* 6.5 */
0.01189670994589177009505572, /* 7.0 */
0.01110455975820691732662991, /* 7.5 */
0.010411265261972096497478567, /* 8.0 */
0.009799416126158803298389475, /* 8.5 */
0.009255462182712732917728637, /* 9.0 */
0.008768700134139385462952823, /* 9.5 */
0.008330563433362871256469318, /* 10.0 */
0.007934114564314020547248100, /* 10.5 */
0.007573675487951840794972024, /* 11.0 */
0.007244554301320383179543912, /* 11.5 */
0.006942840107209529865664152, /* 12.0 */
0.006665247032707682442354394, /* 12.5 */
0.006408994188004207068439631, /* 13.0 */
0.006171712263039457647532867, /* 13.5 */
0.005951370112758847735624416, /* 14.0 */
0.005746216513010115682023589, /* 14.5 */
0.005554733551962801371038690  /* 15.0 */
};

double nsp_stirlerr(NTYPE n)
{ double nn;

  if (n<15.0)
  { nn = 2.0*n;
    if (nn==(int)nn) return(sferr_halves[(int)nn]);
    return(lgamma(n+1.0) - (n+0.5)*log((double)n)+n - HF_LG_PIx2);
  }

  nn = (double)n;
  nn = nn*nn;
  if (n>500) return((S0-S1/nn)/n);
  if (n>80) return((S0-(S1-S2/nn)/nn)/n);
  if (n>35) return((S0-(S1-(S2-S3/nn)/nn)/nn)/n);
  return((S0-(S1-(S2-(S3-S4/nn)/nn)/nn)/nn)/n);
}

/*
 * This function evaluates bd0(x,np) = x log(x/np) + np - x
 * in a manner that should be stable (with small relative error)
 * for all x and np.
 * In particular for x/np close to 1 direct evaluation fails,
 * and evaluation is based on the Taylor series of log((1+v)/(1-v))
 * with v = (x-np)/(x+np).
 *
 * Inputs x and np should be positive -- I don't check this.
 */

double nsp_bd0(NTYPE x, double np)
{ double ej, s, s1, v;
  int j;
  if (fabs(x-np)<0.1*(x+np))
  {
    s = (x-np)*(x-np)/(x+np);
    v = (x-np)/(x+np);
    ej = 2*x*v; v = v*v;
    for (j=1; ;++j)
    { ej *= v;
      s1 = s+ej/((j<<1)+1);
      if (s1==s) return(s1);
      s = s1;
    }
  }
  return(x*log(x/np)+np-x);
}

/*
 * double nsp_dbinom_raw(x,n,p,q,give_log)
 * double nsp_dpois_raw(x,lambda,give_log)
 *   
 * arguments x and n have type NTYPE. This can be defined either to be
 *   integer or double in the dbinom.h header file.
 * p, q and lambda have type double.
 * if give_log = 0, the probability is returned. if give_log=1, the
 *   log of the probability is returned.
 *
 * Notes:
 * (1) dbinom has both p and q arguments. This is useful when one may be
 *     represented more accurately than the other (in particular, in df()).
 * (2) there is no check that x, n are integers - the routines do not require
 *     this. Also, there is no check for validity of parameters p, q, lambda
 *     - do this in the calling function as appropriate.
 */

double nsp_dbinom_raw(NTYPE x, NTYPE n, double p, double q, int give_log)
{ double f, lc;

  if (p==0.0) return((x==0) ? D_1 : D_0);
  if (q==0.0) return((x==n) ? D_1 : D_0);

  if (x==0)
  { lc = (p<0.1) ? -nsp_bd0(n,n*q) - n*p : n*log(q);
    return( DEXP(lc) );
  }

  if (x==n)
  { lc = (q<0.1) ? -nsp_bd0(n,n*p) - n*q : n*log(p);
    return( DEXP(lc) );
  }

  if ((x<0) | (x>n)) return( D_0 );

  lc = STIRLERR(n) - STIRLERR(x) - STIRLERR(n-x)
         - nsp_bd0(x,n*p) - nsp_bd0(n-x,n*q);
  f = (PIx2*x*(n-x))/n;

  return( FEXP(f,lc) );
}

double nsp_dpois_raw(NTYPE x, double lambda, int give_log)
{
  if (lambda==0) return( (x==0) ? D_1 : D_0 );
  if (x==0) return( DEXP(-lambda) );
  if (x<0) return( D_0 );

  return(FEXP( PIx2*x, -STIRLERR(x)-nsp_bd0(x,lambda) ));
}
