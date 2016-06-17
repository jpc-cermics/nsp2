#include "signal.h"

static int signal_r2_tx (int *nthpo, double *cr0, double *cr1, double *ci0, double *ci1);
static int signal_r4_tx (int *nthpo, double *cr0, double *cr1, double *cr2, double *cr3,
			 double *ci0, double *ci1, double *ci2, double *ci3);
static int signal_r8_tx (int *nxtlt, int *nthpo, int *lengt, double *cr0, double *cr1,
			 double *cr2, double *cr3, double *cr4, double *cr5, double *cr6,
			 double *cr7, double *ci0, double *ci1, double *ci2, double *ci3,
			 double *ci4, double *ci5, double *ci6, double *ci7);



/* programe de transformee de fourier rapide pour n=2**m 
 * algorithme de cooley-tukey
 * ce logiciel traite des entrees complexes sous forme de deux tableaux 
 * 
 *  tableau representant les coefficients reels      :x(i) 
 *  tableau representant les coefficients complexes  :y(i) 
 * 
 *options : 
 *          directe  ----> code=0 
 *          indirecte----> code=1 
 * 
 *         si la dimension (suite de nombres complexes n'est pas de 
 *         dimension 2**n tel que  0<n<15 )err est positionne a 1 . 
 */

static const int c__2 = 2;
static const double c_b15 = 2.;

int signal_fft842 (const int *in,const int *n, double *x, double *y, int *err)
{
  int i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10, i__11,
    i__12, i__13, i__14, i__15, i__16, i__17, i__18, i__19, i__20, i__21,
    i__22, i__23, i__24, i__25, i__26, i__27, i__28, i__29;

  static int equiv_14[15];
  double fi;
  double r__;
  double cres;

  int n2pow, i__, j, n8pow, m;
  int lengt, ipass, nthpo, j1, j2;
  int j3, j4, nxtlt, j5, j6, j7, j8, j9, j10, j11;
  int ij, ji, fn, j12, j13, j14, nt;

#define l (equiv_14)
#define l1 (equiv_14 + 14)
#define l2 (equiv_14 + 13)
#define l3 (equiv_14 + 12)
#define l4 (equiv_14 + 11)
#define l5 (equiv_14 + 10)
#define l6 (equiv_14 + 9)
#define l7 (equiv_14 + 8)
#define l8 (equiv_14 + 7)
#define l9 (equiv_14 + 6)
#define l10 (equiv_14 + 5)
#define l11 (equiv_14 + 4)
#define l13 (equiv_14 + 2)
#define l14 (equiv_14 + 1)
#define l15 (equiv_14)
#define l12 (equiv_14 + 3)

  /* Parameter adjustments */
  --y;
  --x;

  /* Function Body */

  for (i__ = 1; i__ <= 15; ++i__)
    {
      m = i__;
      nt = nsp_pow_ii (c__2, i__);
      if (*n == nt)
	{
	  goto L20;
	}
      /* L10: */
    }
  /* 
   *         erreur puissance de 2 non respecte)==> err=1 
   * 
   */
  *err = 1;
  return 0;
  /*        aucune erreur===> err=0 
   */
 L20:
  *err = 0;
  n2pow = m;
  nthpo = *n;
  fn = nthpo;
  if (*in == 1)
    {
      goto L40;
    }
  i__1 = nthpo;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = -y[i__];
      /* L30: */
    }
 L40:
  n8pow = n2pow / 3;
  if (n8pow == 0)
    {
      goto L60;
    }
  /* 
   *developement de l'algoritme en base 8 si ... 
   * 
   */
  i__1 = n8pow;
  for (ipass = 1; ipass <= i__1; ++ipass)
    {
      i__2 = n2pow - ipass * 3;
      nxtlt = nsp_pow_ii (c__2, i__2);
      lengt = nxtlt << 3;
      signal_r8_tx (&nxtlt, &nthpo, &lengt, &x[1], &x[nxtlt + 1],
		    &x[(nxtlt << 1) + 1], &x[nxtlt * 3 + 1],
		    &x[(nxtlt << 2) + 1], &x[nxtlt * 5 + 1],
		    &x[nxtlt * 6 + 1], &x[nxtlt * 7 + 1], &y[1],
		    &y[nxtlt + 1], &y[(nxtlt << 1) + 1], &y[nxtlt * 3 + 1],
		    &y[(nxtlt << 2) + 1], &y[nxtlt * 5 + 1],
		    &y[nxtlt * 6 + 1], &y[nxtlt * 7 + 1]);
      /* L50: */
    }
  /* 
   *is there a four factor left 
   * 
   */
 L60:
  cres = (double) (n2pow - n8pow * 3 - 1);
  if (cres < 0.)
    {
      goto L90;
    }
  else if (cres == 0.)
    {
      goto L70;
    }
  else
    {
      goto L80;
    }
  /* 
   *iteration de l'algoritme en base 2 
   * 
   */
 L70:
  signal_r2_tx (&nthpo, &x[1], &x[2], &y[1], &y[2]);
  goto L90;
  /* 
   *iteration de l'algoritme en base 4 
   * 
   */
 L80:
  signal_r4_tx (&nthpo, &x[1], &x[2], &x[3], &x[4], &y[1], &y[2], &y[3],
		&y[4]);
  /* 
   */
 L90:
  for (j = 1; j <= 15; ++j)
    {
      l[j - 1] = 1;
      cres = (double) (j - n2pow);
      if (cres <= 0.)
	{
	  goto L100;
	}
      goto L110;
    L100:
      i__1 = n2pow + 1 - j;
      l[j - 1] = (int) nsp_pow_di (c_b15, i__1);
    L110:
      ;
    }
  ij = 1;
  i__1 = *l1;
  for (j1 = 1; j1 <= i__1; ++j1)
    {
      i__2 = *l2;
      i__3 = *l1;
      for (j2 = j1; i__3 < 0 ? j2 >= i__2 : j2 <= i__2; j2 += i__3)
	{
	  i__4 = *l3;
	  i__5 = *l2;
	  for (j3 = j2; i__5 < 0 ? j3 >= i__4 : j3 <= i__4; j3 += i__5)
	    {
	      i__6 = *l4;
	      i__7 = *l3;
	      for (j4 = j3; i__7 < 0 ? j4 >= i__6 : j4 <= i__6; j4 += i__7)
		{
		  i__8 = *l5;
		  i__9 = *l4;
		  for (j5 = j4; i__9 < 0 ? j5 >= i__8 : j5 <= i__8;
		       j5 += i__9)
		    {
		      i__10 = *l6;
		      i__11 = *l5;
		      for (j6 = j5; i__11 < 0 ? j6 >= i__10 : j6 <= i__10;
			   j6 += i__11)
			{
			  i__12 = *l7;
			  i__13 = *l6;
			  for (j7 = j6; i__13 < 0 ? j7 >= i__12 : j7 <= i__12;
			       j7 += i__13)
			    {
			      i__14 = *l8;
			      i__15 = *l7;
			      for (j8 = j7;
				   i__15 < 0 ? j8 >= i__14 : j8 <= i__14;
				   j8 += i__15)
				{
				  i__16 = *l9;
				  i__17 = *l8;
				  for (j9 = j8;
				       i__17 < 0 ? j9 >= i__16 : j9 <= i__16;
				       j9 += i__17)
				    {
				      i__18 = *l10;
				      i__19 = *l9;
				      for (j10 = j9;
					   i__19 < 0 ? j10 >= i__18 : j10 <=
					     i__18; j10 += i__19)
					{
					  i__20 = *l11;
					  i__21 = *l10;
					  for (j11 = j10;
					       i__21 < 0 ? j11 >=
						 i__20 : j11 <= i__20;
					       j11 += i__21)
					    {
					      i__22 = *l12;
					      i__23 = *l11;
					      for (j12 = j11;
						   i__23 < 0 ? j12 >=
						     i__22 : j12 <= i__22;
						   j12 += i__23)
						{
						  i__24 = *l13;
						  i__25 = *l12;
						  for (j13 = j12;
						       i__25 < 0 ? j13 >=
							 i__24 : j13 <= i__24;
						       j13 += i__25)
						    {
						      i__26 = *l14;
						      i__27 = *l13;
						      for (j14 = j13;
							   i__27 < 0 ? j14 >=
							     i__26 : j14 <=
							     i__26;
							   j14 += i__27)
							{
							  i__28 = *l15;
							  i__29 = *l14;
							  for (ji = j14;
							       i__29 <
								 0 ? ji >=
								 i__28 : ji <=
								 i__28;
							       ji += i__29)
							    {
							      cres =
								(double) (ij -
									  ji);
							      if (cres >= 0.)
								{
								  goto L130;
								}
							      else
								{
								  goto L120;
								}
							    L120:
							      r__ = x[ij];
							      x[ij] = x[ji];
							      x[ji] = r__;
							      fi = y[ij];
							      y[ij] = y[ji];
							      y[ji] = fi;
							    L130:
							      ++ij;
							    }
							}
						    }
						}
					    }
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  if (*in == 1)
    {
      goto L150;
    }
  i__29 = nthpo;
  for (i__ = 1; i__ <= i__29; ++i__)
    {
      y[i__] = -y[i__];
      /* L140: */
    }
  goto L170;
 L150:
  i__29 = nthpo;
  for (i__ = 1; i__ <= i__29; ++i__)
    {
      x[i__] /= fn;
      y[i__] /= fn;
      /* L160: */
    }
 L170:
  return 0;
}	

#undef l12
#undef l15
#undef l14
#undef l13
#undef l11
#undef l10
#undef l9
#undef l8
#undef l7
#undef l6
#undef l5
#undef l4
#undef l3
#undef l2
#undef l1
#undef l

/*! 
 *subroutine:  r2tx 
 *sous-programme d'iteration en base 2 
 *! 
 */

int
signal_r2_tx (int *nthpo, double *cr0, double *cr1, double *ci0, double *ci1)
{
  int i__1;
  int k;
  double r1, fi1;

  --ci1;
  --ci0;
  --cr1;
  --cr0;

  /* Function Body */
  i__1 = *nthpo;
  for (k = 1; k <= i__1; k += 2)
    {
      r1 = cr0[k] + cr1[k];
      cr1[k] = cr0[k] - cr1[k];
      cr0[k] = r1;
      fi1 = ci0[k] + ci1[k];
      ci1[k] = ci0[k] - ci1[k];
      ci0[k] = fi1;
      /* L10: */
    }
  return 0;
}				/* r2tx_ */

int
signal_r4_tx (int *nthpo, double *cr0, double *cr1, double *cr2, double *cr3,
	      double *ci0, double *ci1, double *ci2, double *ci3)
{
  int i__1;

  /* Local variables */
  int k;
  double r1, r2, r3, r4, fi1, fi2, fi3, fi4;

  /*! 
   *subroutine:  r4tx 
   *sous-programme d'iteration en base 4 
   *! 
   * 
   */
  /* Parameter adjustments */
  --ci3;
  --ci2;
  --ci1;
  --ci0;
  --cr3;
  --cr2;
  --cr1;
  --cr0;

  /* Function Body */
  i__1 = *nthpo;
  for (k = 1; k <= i__1; k += 4)
    {
      r1 = cr0[k] + cr2[k];
      r2 = cr0[k] - cr2[k];
      r3 = cr1[k] + cr3[k];
      r4 = cr1[k] - cr3[k];
      fi1 = ci0[k] + ci2[k];
      fi2 = ci0[k] - ci2[k];
      fi3 = ci1[k] + ci3[k];
      fi4 = ci1[k] - ci3[k];
      cr0[k] = r1 + r3;
      ci0[k] = fi1 + fi3;
      cr1[k] = r1 - r3;
      ci1[k] = fi1 - fi3;
      cr2[k] = r2 - fi4;
      ci2[k] = fi2 + r4;
      cr3[k] = r2 + fi4;
      ci3[k] = fi2 - r4;
      /* L10: */
    }
  return 0;
}				/* r4tx_ */

/* 
 *subroutine:  r8tx 
 *sous-programme d'iteration en base 8 
 */

int
signal_r8_tx (int *nxtlt, int *nthpo, int *lengt, double *cr0, double *cr1,
	      double *cr2, double *cr3, double *cr4, double *cr5, double *cr6,
	      double *cr7, double *ci0, double *ci1, double *ci2, double *ci3,
	      double *ci4, double *ci5, double *ci6, double *ci7)
{
  int i__1, i__2, i__3;
  double d__1, d__2;

  int j, k;
  double scale, c1, c2, c3, c4, c5, c6, c7, s1, s2, s3, s4, s5, s6, s7, p7,
    ti, tr, ai0, ai1, ar0, ar1, ar2, ar3, ar4, ar5, ar6, ar7, ai2, ai3, ai4,
    ai5, ai6, ai7, br0, br1, br2, br3, br4, br5, br6, br7, bi0, bi1, bi2, bi3,
    bi4, bi5, bi6, bi7, pi2, arg;

  /* Parameter adjustments */
  --ci7;
  --ci6;
  --ci5;
  --ci4;
  --ci3;
  --ci2;
  --ci1;
  --ci0;
  --cr7;
  --cr6;
  --cr5;
  --cr4;
  --cr3;
  --cr2;
  --cr1;
  --cr0;

  /* Function Body */
  pi2 = atan (1.) * 8.;
  p7 = 1. / sqrt (2.);
  scale = pi2 / (double) (*lengt);
  i__1 = *nxtlt;
  for (j = 1; j <= i__1; ++j)
    {
      arg = (double) (j - 1) * scale;
      c1 = cos (arg);
      s1 = sin (arg);
      /*Computing 2nd power 
       */
      d__1 = c1;
      /*Computing 2nd power 
       */
      d__2 = s1;
      c2 = d__1 * d__1 - d__2 * d__2;
      s2 = c1 * s1 + c1 * s1;
      c3 = c1 * c2 - s1 * s2;
      s3 = c2 * s1 + s2 * c1;
      /*Computing 2nd power 
       */
      d__1 = c2;
      /*Computing 2nd power 
       */
      d__2 = s2;
      c4 = d__1 * d__1 - d__2 * d__2;
      s4 = c2 * s2 + c2 * s2;
      c5 = c2 * c3 - s2 * s3;
      s5 = c3 * s2 + s3 * c2;
      /*Computing 2nd power 
       */
      d__1 = c3;
      /*Computing 2nd power 
       */
      d__2 = s3;
      c6 = d__1 * d__1 - d__2 * d__2;
      s6 = c3 * s3 + c3 * s3;
      c7 = c3 * c4 - s3 * s4;
      s7 = c4 * s3 + s4 * c3;
      i__2 = *nthpo;
      i__3 = *lengt;
      for (k = j; i__3 < 0 ? k >= i__2 : k <= i__2; k += i__3)
	{
	  ar0 = cr0[k] + cr4[k];
	  ar1 = cr1[k] + cr5[k];
	  ar2 = cr2[k] + cr6[k];
	  ar3 = cr3[k] + cr7[k];
	  ar4 = cr0[k] - cr4[k];
	  ar5 = cr1[k] - cr5[k];
	  ar6 = cr2[k] - cr6[k];
	  ar7 = cr3[k] - cr7[k];
	  ai0 = ci0[k] + ci4[k];
	  ai1 = ci1[k] + ci5[k];
	  ai2 = ci2[k] + ci6[k];
	  ai3 = ci3[k] + ci7[k];
	  ai4 = ci0[k] - ci4[k];
	  ai5 = ci1[k] - ci5[k];
	  ai6 = ci2[k] - ci6[k];
	  ai7 = ci3[k] - ci7[k];
	  br0 = ar0 + ar2;
	  br1 = ar1 + ar3;
	  br2 = ar0 - ar2;
	  br3 = ar1 - ar3;
	  br4 = ar4 - ai6;
	  br5 = ar5 - ai7;
	  br6 = ar4 + ai6;
	  br7 = ar5 + ai7;
	  bi0 = ai0 + ai2;
	  bi1 = ai1 + ai3;
	  bi2 = ai0 - ai2;
	  bi3 = ai1 - ai3;
	  bi4 = ai4 + ar6;
	  bi5 = ai5 + ar7;
	  bi6 = ai4 - ar6;
	  bi7 = ai5 - ar7;
	  cr0[k] = br0 + br1;
	  ci0[k] = bi0 + bi1;
	  if (j <= 1)
	    {
	      goto L10;
	    }
	  cr1[k] = c4 * (br0 - br1) - s4 * (bi0 - bi1);
	  ci1[k] = c4 * (bi0 - bi1) + s4 * (br0 - br1);
	  cr2[k] = c2 * (br2 - bi3) - s2 * (bi2 + br3);
	  ci2[k] = c2 * (bi2 + br3) + s2 * (br2 - bi3);
	  cr3[k] = c6 * (br2 + bi3) - s6 * (bi2 - br3);
	  ci3[k] = c6 * (bi2 - br3) + s6 * (br2 + bi3);
	  tr = p7 * (br5 - bi5);
	  ti = p7 * (br5 + bi5);
	  cr4[k] = c1 * (br4 + tr) - s1 * (bi4 + ti);
	  ci4[k] = c1 * (bi4 + ti) + s1 * (br4 + tr);
	  cr5[k] = c5 * (br4 - tr) - s5 * (bi4 - ti);
	  ci5[k] = c5 * (bi4 - ti) + s5 * (br4 - tr);
	  tr = -p7 * (br7 + bi7);
	  ti = p7 * (br7 - bi7);
	  cr6[k] = c3 * (br6 + tr) - s3 * (bi6 + ti);
	  ci6[k] = c3 * (bi6 + ti) + s3 * (br6 + tr);
	  cr7[k] = c7 * (br6 - tr) - s7 * (bi6 - ti);
	  ci7[k] = c7 * (bi6 - ti) + s7 * (br6 - tr);
	  goto L20;
	L10:
	  cr1[k] = br0 - br1;
	  ci1[k] = bi0 - bi1;
	  cr2[k] = br2 - bi3;
	  ci2[k] = bi2 + br3;
	  cr3[k] = br2 + bi3;
	  ci3[k] = bi2 - br3;
	  tr = p7 * (br5 - bi5);
	  ti = p7 * (br5 + bi5);
	  cr4[k] = br4 + tr;
	  ci4[k] = bi4 + ti;
	  cr5[k] = br4 - tr;
	  ci5[k] = bi4 - ti;
	  tr = -p7 * (br7 + bi7);
	  ti = p7 * (br7 - bi7);
	  cr6[k] = br6 + tr;
	  ci6[k] = bi6 + ti;
	  cr7[k] = br6 - tr;
	  ci7[k] = bi6 - ti;
	L20:
	  ;
	}
      /* L30: */
    }
  return 0;
}
