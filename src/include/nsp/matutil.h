#ifndef NSP_INC_MATUTIL
#define NSP_INC_MATUTIL 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

extern int nsp_double2int (int *n, double *dx, int *ix); 
extern int nsp_float2int (int *n, float *dx, int *ix); 
extern int nsp_double2float (int *n, double *dx, float *rx); 
extern int nsp_int2double (int *n, int *idx, int *incx, double *dy, int *incy); 
extern int nsp_int2float (int *n, int *idx, int *incx, float *dy, int *incy); 
extern int nsp_float2double (int *n, float *rdx, int *incx, double *dy, int *incy); 
extern int nsp_convert_double_to_type (double *x, int n, const char *type); 
extern int nsp_convert_type_to_double (double *x, int n, const char *type); 
extern int nsp_dset (const int *n, const double *dx, double *dy, const int *incy); 
extern int nsp_dadd (const int n, const double *dx,const int incx, double *dy,const int incy); 
extern int nsp_dsub(const int n,const  double *dx,const int incx, double *dy,const  int incy);
extern int nsp_dadd_maxplus(const int n, const double *dx, const  int incx, double *dy, const  int incy);
extern int nsp_dsub_maxplus(const int n,const double *dx,const int incx, double *dy,const int incy);
extern double nsp_dsum (int *n, double *dx, int *incx); 
extern int nsp_dvmul(const int n,const  double *dx,const  int incx, double *dy,const  int incy);
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
extern void nsp_complex2double(double *tab, int size);
extern void nsp_double2complex( double *tab, int size);
extern void nsp_dsumrows(double *x, double *res, int m, int n);
extern void nsp_zsumrows(doubleC *x, doubleC *res, int m, int n);
extern double nsp_dprod(double *x, int n, int incx);
extern void nsp_dprodrows(double *x, double *res, int m, int n);
extern void nsp_dcross(double *A, double *B, double *C, int n, int dim_flag);
extern void nsp_zcross(doubleC *A, doubleC *B, doubleC *C, int n, int dim_flag);



#endif /* NSP_INC_   **/
