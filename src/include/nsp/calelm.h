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
extern double nsp_urand(int *iy);
extern int C2F(entier) (int *n, double *dx, int *ix); 
extern int C2F(int2db) (int *n, int *idx, int *incx, double *dy, int *incy); 
extern int nsp_dset(const int *n,const double * dx,double * dy,const int * incy);
extern int nsp_dadd(int *n, double *dx, int *incx, double *dy, int *incy); 
extern int nsp_dsub(int *n, double *dx, int *incx, double *dy, int *incy); 
extern double nsp_dsum(int *n, double *dx, int *incx); 
extern int nsp_dvmul(int *n, double *dx, int *incx, double *dy, int *incy); 
extern int nsp_icopy(const int *n,const int *idx,const int *incx, int *idy, const int *incy); 
extern int nsp_iset(int *n, int *idx, int *idy, int *incy); 
extern int nsp_iadd(int *n, int *ival, int *idy, int *incy); 
extern int nsp_dzcopy(const int *n,const double *zx, const int *incx, doubleC *zy,const int *incy); 
extern int nsp_dzscal(int *n, double *da, doubleC *zx, int *incx); 
extern int nsp_dzset(int *n, double *dx, doubleC *zy, int *incy); 
extern int nsp_zadd(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern double C2F(myzabs) (double real, double imag); 
extern double nsp_zasum(int *n, doubleC *zx, int *incx); 
extern int nsp_zsub(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern void nsp_zsum(doubleC *ret_val, int *n, doubleC *zx, int *incx); 
extern int nsp_zvmul(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 

#endif /** SCI_   **/
