#ifndef _fftpack_H
#define _fftpack_H

#include "nsp/cnumeric.h"
extern int fftpack_cfftb1 (int *n, double *c__, double *ch, double *wa,
			   int *ifac);
extern int fftpack_cfftf1 (int *n, double *c__, double *ch, double *wa,
			   int *ifac);
extern int fftpack_cffti1 (int *n, double *wa, int *ifac);
extern int fftpack_cosqb1 (int *n, double *x, double *w, double *xh);
extern int fftpack_cosqf1 (int *n, double *x, double *w, double *xh);
extern int fftpack_cpassf4 (int *, int *, double *, double *, double *,
			    double *, double *);
extern int fftpack_ezfft1 (int *n, double *wa, int *ifac);

extern int fftpack_rfftb1 (int *n, double *c__, double *ch, double *wa,
			   int *ifac);
extern int fftpack_rfftf1 (int *n, double *c__, double *ch, double *wa,
			   int *ifac);
extern int fftpack_rffti1 (int *n, double *wa, int *ifac);
extern int fftpack_int (int *n, double *war, double *was, double *xh,
			double *x, int *ifac);

extern int fftpack_dfftb (int *n, double *r__, double *wsave);
extern int fftpack_dfftf (int *n, double *r__, double *wsave);
extern int fftpack_dffti (int *n, double *wsave);
extern int fftpack_dzfftb (int *n, double *r__, double *azero, double *a,
			   double *b, double *wsave);
extern int fftpack_dzfftf (int *n, double *r__, double *azero, double *a,
			   double *b, double *wsave);
extern int fftpack_dzffti (int *n, double *wsave);
extern int fftpack_zfftb (int *n, doubleC *c__, double *wsave);
extern int fftpack_zfftf (int *n, doubleC *c__, double *wsave);
extern int fftpack_zffti (int *n, double *wsave);

#endif
