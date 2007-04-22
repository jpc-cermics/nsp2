#ifndef NSP_INC_LAPACK_LIB
#define NSP_INC_LAPACK_LIB

extern int nsp_qr(NspMatrix *A,NspMatrix **q,NspMatrix **r,NspMatrix **E,NspMatrix **Rank,NspMatrix **Sval,double *tol,char mode);
extern int nsp_lsq(NspMatrix *A, NspMatrix *B, double *Rcond, int *Rank);
extern NspMatrix * nsp_det(NspMatrix *A,char mode);
extern int nsp_spec(NspMatrix *A, NspMatrix **d,NspMatrix **v) ;
extern int nsp_spec_sym(NspMatrix *A,NspMatrix **d,char flag);
extern int nsp_gspec(NspMatrix *A, NspMatrix *B, NspMatrix **Vl, NspMatrix **Vr,
		     NspMatrix **alpha, NspMatrix **beta);
extern int nsp_rcond(NspMatrix *A,double *rcond) ;
extern int nsp_cholesky(NspMatrix *A, int *minor) ;
extern int nsp_lu(NspMatrix *A,NspMatrix **L,NspMatrix **E, NspMatrix **Rcond);
extern int nsp_svd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);
extern int nsp_inv(NspMatrix *A) ;
extern double nsp_matrix_norm(NspMatrix *A, char flag);
extern double nsp_vector_norm(NspMatrix *A, double p); 
extern int nsp_balanc(NspMatrix *A,NspMatrix **D);
extern int nsp_gbalanc(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y);
extern int nsp_hess(NspMatrix *A,NspMatrix **U) ;
extern int nsp_mat_is_symmetric(NspMatrix *A);
extern int nsp_mat_is_upper_triangular(NspMatrix *A);
extern int nsp_mat_is_lower_triangular(NspMatrix *A);
extern int nsp_mat_have_nan_or_inf(NspMatrix *A);
extern int nsp_expm(NspMatrix *A);
extern NspMatrix *nsp_matrix_bdiv(NspMatrix *A, NspMatrix *B, double tol_rcond);
extern int nsp_mat_bdiv_lsq(NspMatrix *A, NspMatrix *B, double tol_rcond);
extern int nsp_mat_bdiv_square(NspMatrix *A, NspMatrix *B, double *rcond, double tol_rcond);
extern int nsp_mat_bdiv_triangular(NspMatrix *A, NspMatrix *B, char tri_type, int *info);
extern int nsp_mat_bdiv_diagonal(NspMatrix *A, NspMatrix *B, int *info);
extern int nsp_solve_banded(NspMatrix *A, NspMatrix *B, NspMatrix **X);
#endif 
