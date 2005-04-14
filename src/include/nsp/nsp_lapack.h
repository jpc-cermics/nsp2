#ifndef NSP_LAPACK_LIB
#define NSP_LAPACK_LIB

extern int nsp_qr(NspMatrix *A,NspMatrix **q,NspMatrix **r,NspMatrix **rank,NspMatrix **E,double *tol,char mode);
extern NspMatrix *nsp_lsq(NspMatrix *A,NspMatrix *B,char flag);
extern double C2F(dlamch)(char *,int );
extern double cdf_dlamch(char *,int );
extern NspMatrix * nsp_det(NspMatrix *A,char mode);
extern int nsp_spec(NspMatrix *A, NspMatrix **d,NspMatrix **v) ;
extern int nsp_rcond(NspMatrix *A,double *rcond) ;
extern int nsp_cholewsky(NspMatrix *A) ;
extern int nsp_lu(NspMatrix *A,NspMatrix **L,NspMatrix **U,NspMatrix **E);
extern int nsp_svd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);
extern int nsp_inv(NspMatrix *A) ;

#endif 
