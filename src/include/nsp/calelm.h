#ifndef SCI_CALELM
#define SCI_CALELM 

/*------------------------------------------------------------------------
 *    Copyright (C) 1998-2003 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

extern int C2F(franck) (double *a, int *na, int *n, int *job);
extern int C2F(hilber) (double *a, int *lda, int *n);
extern int C2F(magic) (double *a, int *lda, int *n);
extern double C2F(urand) (int *iy);
extern int C2F(entier) (int *n, double *dx, int *ix); 
extern int C2F(int2db) (int *n, int *idx, int *incx, double *dy, int *incy); 
const int C2F(dset)(const int *n,const double * dx,double * dy,const int * incy);
extern int C2F(dadd) (int *n, double *dx, int *incx, double *dy, int *incy); 
extern int C2F(dsub) (int *n, double *dx, int *incx, double *dy, int *incy); 
extern double C2F(dsum) (int *n, double *dx, int *incx); 
extern int C2F(dvmul) (int *n, double *dx, int *incx, double *dy, int *incy); 
extern int C2F(icopy) (const int *n,const int *idx,const int *incx, int *idy, const int *incy); 
extern int C2F(iset) (int *n, int *idx, int *idy, int *incy); 
extern int C2F(iadd) (int *n, int *ival, int *idy, int *incy); 
extern int C2F(dzcopy) (const int *n,const double *zx, const int *incx, doubleC *zy,const int *incy); 
extern int C2F(dzscal) (int *n, double *da, doubleC *zx, int *incx); 
extern int C2F(dzset) (int *n, double *dx, doubleC *zy, int *incy); 
extern int C2F(zadd) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern double C2F(myzabs) (double real, double imag); 
extern double C2F(zasum) (int *n, doubleC *zx, int *incx); 
extern int C2F(zsub) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern void C2F(zsum) (doubleC *ret_val, int *n, doubleC *zx, int *incx); 
extern int C2F(zvmul) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 

#endif /** SCI_   **/
