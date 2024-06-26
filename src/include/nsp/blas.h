#ifndef NSP_INC_BLAS
#define NSP_INC_BLAS

/* prototypes for blas routines */

extern int C2F(dswap) (const int *n, double *dx, const int *incx, double *dy, const int *incy);
extern int C2F(dcopy) (const int *n, const double *dx, const int *incx, double *dy, const int *incy);
extern int C2F(idamax) (const int *n, const double *dx, const int *incx);
extern double C2F(ddot) (const int *n,const double *dx,const int *incx,const  double *dy,const int *incy);
extern double C2F(dnrm2) (const int *n,const double *x,const int *incx);
extern int C2F(dscal) (const int *n,const double *da, double *dx, const int *incx);
extern int C2F(daxpy) (const int *n, double *da, double *dx,const int *incx, double *dy,const int *incy);

extern double C2F(dasum) (int *n, double *dx, int *incx);

extern double C2F(dcabs1) (doubleC *z__);

extern int C2F(dgbmv) (char *trans, int *m, int *n, int *kl, int *ku, double *alpha, double *a, int *lda, double *x, int *incx, double *beta, double *y, int *incy, int trans_len);
extern int C2F(dgemm) (char *transa, char *transb, int *m, int *n, int *k, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c__, int *ldc, int transa_len, int transb_len);
extern int C2F(dgemv) (char *trans, int *m, int *n, double *alpha, double *a, int *lda, double *x, int *incx, double *beta, double *y, int *incy, int trans_len);
extern int C2F(dger) (int *m, int *n, double *alpha, double *x, int *incx, double *y, int *incy, double *a, int *lda);

extern int C2F(drot) (int *n, double *dx, int *incx, double *dy, int *incy, double *c__, double *s);
extern int C2F(drotg) (double *da, double *db, double *c__, double *s);
extern int C2F(dsbmv) (char *uplo, int *n, int *k, double *alpha, double *a, int *lda, double *x, int *incx, double *beta, double *y, int *incy, int uplo_len);

extern int C2F(dspmv) (char *uplo, int *n, double *alpha, double *ap, double *x, int *incx, double *beta, double *y, int *incy, int uplo_len);
extern int C2F(dspr) (char *uplo, int *n, double *alpha, double *x, int *incx, double *ap, int uplo_len);
extern int C2F(dspr2) (char *uplo, int *n, double *alpha, double *x, int *incx, double *y, int *incy, double *ap, int uplo_len);
extern int C2F(dsymm) (char *side, char *uplo, int *m, int *n, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c__, int *ldc, int side_len, int uplo_len);
extern int C2F(dsymv) (char *uplo, int *n, double *alpha, double *a, int *lda, double *x, int *incx, double *beta, double *y, int *incy, int uplo_len);
extern int C2F(dsyr) (char *uplo, int *n, double *alpha, double *x, int *incx, double *a, int *lda, int uplo_len);
extern int C2F(dsyr2) (char *uplo, int *n, double *alpha, double *x, int *incx, double *y, int *incy, double *a, int *lda, int uplo_len);
extern int C2F(dsyr2k) (char *uplo, char *trans, int *n, int *k, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c__, int *ldc, int uplo_len, int trans_len);
extern int C2F(dsyrk) (char *uplo, char *trans, int *n, int *k, double *alpha, double *a, int *lda, double *beta, double *c__, int *ldc, int uplo_len, int trans_len);
extern int C2F(dtbmv) (char *uplo, char *trans, char *diag, int *n, int *k, double *a, int *lda, double *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(dtbsv) (char *uplo, char *trans, char *diag, int *n, int *k, double *a, int *lda, double *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(dtpmv) (char *uplo, char *trans, char *diag, int *n, double *ap, double *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(dtpsv) (char *uplo, char *trans, char *diag, int *n, double *ap, double *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(dtrmm) (char *side, char *uplo, char *transa, char *diag, int *m, int *n, double *alpha, double *a, int *lda, double *b, int *ldb, int side_len, int uplo_len, int transa_len, int diag_len);
extern int C2F(dtrmv) (char *uplo, char *trans, char *diag, int *n, double *a, int *lda, double *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(dtrsm) (char *side, char *uplo, char *transa, char *diag, int *m, int *n, double *alpha, double *a, int *lda, double *b, int *ldb, int side_len, int uplo_len, int transa_len, int diag_len);
extern int C2F(dtrsv) (char *uplo, char *trans, char *diag, int *n, double *a, int *lda, double *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern double C2F(dzasum) (int *n, doubleC *zx, int *incx);
extern double C2F(dznrm2) (int *n, doubleC *x, int *incx);
extern int C2F(izamax) (int *n, doubleC *zx, int *incx);
extern int C2F(lsame) (char *ca, char *cb, int ca_len, int cb_len);
extern int C2F(xerbla) (char *srname, int *info, int srname_len);
extern int C2F(zaxpy) (int *n, doubleC *za, doubleC *zx, int *incx, doubleC *zy, int *incy);
extern int C2F(zcopy) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy);
/* extern void C2F(zdotc) (doubleC * ret_val, int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); */
/* extern void C2F(zdotu) (doubleC * ret_val, int *n, doubleC *zx, int *incx, doubleC *zy, int *incy); */
extern doubleC C2F(zdotc) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy);
extern doubleC C2F(zdotu) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy);
extern int C2F(zdscal) (int *n, double *da, doubleC *zx, int *incx);
extern int C2F(zgbmv) (char *trans, int *m, int *n, int *kl, int *ku, doubleC *alpha, doubleC *a, int *lda, doubleC *x, int *incx, doubleC *beta, doubleC *y, int *incy, int trans_len);
extern int C2F(zgemm) (char *transa, char *transb, int *m, int *n, int *k, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, doubleC *beta, doubleC *c__, int *ldc, int transa_len, int transb_len);
extern int C2F(zgemv) (char *trans, int *m, int *n, doubleC *alpha, doubleC *a, int *lda, doubleC *x, int *incx, doubleC *beta, doubleC *y, int *incy, int trans_len);
extern int C2F(zgerc) (int *m, int *n, doubleC *alpha, doubleC *x, int *incx, doubleC *y, int *incy, doubleC *a, int *lda);
extern int C2F(zgeru) (int *m, int *n, doubleC *alpha, doubleC *x, int *incx, doubleC *y, int *incy, doubleC *a, int *lda);
extern int C2F(zhbmv) (char *uplo, int *n, int *k, doubleC *alpha, doubleC *a, int *lda, doubleC *x, int *incx, doubleC *beta, doubleC *y, int *incy, int uplo_len);
extern int C2F(zhemm) (char *side, char *uplo, int *m, int *n, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, doubleC *beta, doubleC *c__, int *ldc, int side_len, int uplo_len);
extern int C2F(zhemv) (char *uplo, int *n, doubleC *alpha, doubleC *a, int *lda, doubleC *x, int *incx, doubleC *beta, doubleC *y, int *incy, int uplo_len);
extern int C2F(zher) (char *uplo, int *n, double *alpha, doubleC *x, int *incx, doubleC *a, int *lda, int uplo_len);
extern int C2F(zher2) (char *uplo, int *n, doubleC *alpha, doubleC *x, int *incx, doubleC *y, int *incy, doubleC *a, int *lda, int uplo_len);
extern int C2F(zher2k) (char *uplo, char *trans, int *n, int *k, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, double *beta, doubleC *c__, int *ldc, int uplo_len, int trans_len);
extern int C2F(zherk) (char *uplo, char *trans, int *n, int *k, double *alpha, doubleC *a, int *lda, double *beta, doubleC *c__, int *ldc, int uplo_len, int trans_len);
extern int C2F(zhpmv) (char *uplo, int *n, doubleC *alpha, doubleC *ap, doubleC *x, int *incx, doubleC *beta, doubleC *y, int *incy, int uplo_len);
extern int C2F(zhpr) (char *uplo, int *n, double *alpha, doubleC *x, int *incx, doubleC *ap, int uplo_len);
extern int C2F(zhpr2) (char *uplo, int *n, doubleC *alpha, doubleC *x, int *incx, doubleC *y, int *incy, doubleC *ap, int uplo_len);
extern int C2F(zrotg) (doubleC *ca, doubleC *cb, double *c__, doubleC *s);
extern int C2F(zscal) (int *n, doubleC *za, doubleC *zx, int *incx);
extern int C2F(zswap) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy);
extern int C2F(zsymm) (char *side, char *uplo, int *m, int *n, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, doubleC *beta, doubleC *c__, int *ldc, int side_len, int uplo_len);
extern int C2F(zsyr2k) (char *uplo, char *trans, int *n, int *k, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, doubleC *beta, doubleC *c__, int *ldc, int uplo_len, int trans_len);
extern int C2F(zsyrk) (char *uplo, char *trans, int *n, int *k, doubleC *alpha, doubleC *a, int *lda, doubleC *beta, doubleC *c__, int *ldc, int uplo_len, int trans_len);
extern int C2F(ztbmv) (char *uplo, char *trans, char *diag, int *n, int *k, doubleC *a, int *lda, doubleC *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(ztbsv) (char *uplo, char *trans, char *diag, int *n, int *k, doubleC *a, int *lda, doubleC *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(ztpmv) (char *uplo, char *trans, char *diag, int *n, doubleC *ap, doubleC *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(ztpsv) (char *uplo, char *trans, char *diag, int *n, doubleC *ap, doubleC *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(ztrmm) (char *side, char *uplo, char *transa, char *diag, int *m, int *n, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, int side_len, int uplo_len, int transa_len, int diag_len);
extern int C2F(ztrmv) (char *uplo, char *trans, char *diag, int *n, doubleC *a, int *lda, doubleC *x, int *incx, int uplo_len, int trans_len, int diag_len);
extern int C2F(ztrsm) (char *side, char *uplo, char *transa, char *diag, int *m, int *n, doubleC *alpha, doubleC *a, int *lda, doubleC *b, int *ldb, int side_len, int uplo_len, int transa_len, int diag_len);
extern int C2F(ztrsv) (char *uplo, char *trans, char *diag, int *n, doubleC *a, int *lda, doubleC *x, int *incx, int uplo_len, int trans_len, int diag_len);


#endif /* NSP_INC_BLAS   **/
