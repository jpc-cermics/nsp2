#ifndef SCI_MATUTIL
#define SCI_MATUTIL 

/*------------------------------------------------------------------------
 *    Copyright (C) 1998-2003 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

extern int nsp_double2int (int *n, double *dx, int *ix); 
extern int nsp_float2int (int *n, float *dx, int *ix); 
extern int nsp_double2float (int *n, double *dx, float *rx); 
extern int nsp_int2double (int *n, int *idx, int *incx, double *dy, int *incy); 
extern int nsp_int2float (int *n, int *idx, int *incx, float *dy, int *incy); 
extern int nsp_float2double (int *n, float *rdx, int *incx, double *dy, int *incy); 
extern int nsp_convert_double_to_type (double *x, int n, const char *type); 
extern int nsp_convert_type_to_double (double *x, int n, const char *type); 
extern int nsp_dset (const int *n, const double *dx, double *dy, const int *incy); 
extern int nsp_dadd (int *n, double *dx, int *incx, double *dy, int *incy); 
extern int nsp_dadd_maxplus (int *n, double *dx, int *incx, double *dy, int *incy); 
extern int nsp_dsub (int *n, double *dx, int *incx, double *dy, int *incy); 
extern int nsp_dsub_maxplus (int *n, double *dx, int *incx, double *dy, int *incy); 
extern double nsp_dsum (int *n, double *dx, int *incx); 
extern int nsp_dvmul (int *n, double *dx, int *incx, double *dy, int *incy); 
extern int nsp_icopy (const int *n, const int *idx, const int *incx, int *idy, const int *incy); 
extern int nsp_iset (int *n, int *idx, int *idy, int *incy); 
extern int nsp_iadd (int *n, int *ival, int *idy, int *incy); 
extern int nsp_dzcopy (const int *n, const double *zx, const int *incx, doubleC *zy, const int *incy); 
extern int nsp_dzscal (int *n, double *da, doubleC *zx, int *incx); 
extern int nsp_dzset (int *n, double *dx, doubleC *zy, int *incy); 
extern int nsp_zadd (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern int nsp_zadd_maxplus (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern double myzabs (double real, double imag); 
extern double nsp_zasum (int *n, doubleC *zx, int *incx); 
extern int nsp_zsub (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern int nsp_zsub_maxplus (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern void nsp_zsum (doubleC *ret_val, int *n, doubleC *zx, int *incx); 
extern int nsp_zvmul (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); 
extern double nsp_urand (int *iy); 
extern void nsp_magic_matrix_fill (double *m, int n); 
extern void nsp_franck_matrix (double *a, int n); 
extern void nsp_franck_inverse_matrix (double *a, int n); 
extern void nsp_hilbert_matrix (double *a, int n); 
extern void nsp_hilbert_inverse_matrix (double *a, int n); 

#endif /* SCI_   **/
