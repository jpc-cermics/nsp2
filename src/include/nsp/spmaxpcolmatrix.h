#ifndef NSP_INC_SPMAXPCOLMATRIX 
#define NSP_INC_SPMAXPCOLMATRIX 

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>  
#include <nsp/sciio.h>
#include <nsp/objectf.h>
#include <nsp/matrix.h>
#include <nsp/triplet.h>
#include <nsp/spcolmatrix.h>
#include <nsp/mpmatrix.h>
/*
 * NspSpMaxpColMatrix inherits from NspObject 
 */

/* defined in <nsp/objectf.h>: typedef struct _NspSpMaxpColmatrix NspSpMaxpColMatrix; */

typedef struct _NspTypeSpMaxpColMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeSpMaxpColMatrix;

/* Sparse matrices 
 * 
 * m,n :  size of matrix 
 * D[j]-> points to the jth column
 * D[j]->size : number of non nul elements in col j 
 * D[j]->J : array of rows indices of elements of col j
 * D[j]->R : array of values (real) of elements of col j
 * D[j]->C : array of values (complex) of elements of col j
 */

/* used to store a matlab compatible representation */

struct _NspSpMaxpColmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeSpMaxpColMatrix *type; 
  /*< public >*/
  char rc_type;        /* 'r' or 'i' : real or complex matrix */
  int m,n;   /* mn should be removed since m*n can be bigger than int */
  SpCol **D; /* array of size m giving the Rows datas */
  char convert; /* 't' : the matrix is stored in triplet , 'n': triplet not used   */
  nsp_sparse_triplet triplet; 
} ;

extern int nsp_type_spmaxpcolmatrix_id;
extern NspTypeSpMaxpColMatrix *nsp_type_spmaxpcolmatrix;

int nsp_type_spmaxpcolmatrix_init();

/* only useful when building a new class derived from spmaxpcolmatrix */

NspTypeSpMaxpColMatrix *new_type_spmaxpcolmatrix(type_mode mode);

NspSpMaxpColMatrix *new_spmaxpcolmatrix();

/*
 * Object methods redefined for spmaxpcolmatrix 
 */


#define NULLSPMAXPCOL (  NspSpMaxpColMatrix *) 0
#define NULLSPMAXPCOLMAT (  NspSpMaxpColMatrix *) 0

/* SpMaxpColMatObj.c */

extern NspSpMaxpColMatrix *SpMaxpColObj (NspObject *O); 
extern int IsSpMaxpColMatObj (Stack stack, int i); 
extern int IsSpMaxpColMat(const NspObject *O);
extern NspSpMaxpColMatrix *GetSpMaxpColCopy (Stack stack, int i); 
extern NspSpMaxpColMatrix *GetSpMaxpCol (Stack stack, int i); 
extern NspSpMaxpColMatrix *GetRealSpMaxpCol(Stack stack, int i);

/* NspSpMaxpColMatrix.c */

extern void nsp_spmaxpcolmatrix_destroy(NspSpMaxpColMatrix *Mat); 
extern void nsp_spmaxpcolmatrix_col_destroy(SpCol *Col); 
extern int nsp_spmaxpcolmatrix_nnz(const NspSpMaxpColMatrix *HMat);
extern int nsp_spmaxpcolmatrix_info(NspSpMaxpColMatrix *Sp, int indent,const char *name, int rec_level);
extern int nsp_spmaxpcolmatrix_print(NspSpMaxpColMatrix *Sp, int indent,char *name, int rec_level); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_copy(NspSpMaxpColMatrix *A); 
extern NspSpMaxpColMatrix   *nsp_spmaxpcolmatrix_object(NspObject *O);


 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_create(char *name, char type, int m, int n); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n); 
 extern int nsp_spmaxpcolmatrix_get(NspSpMaxpColMatrix *A, NspMatrix **RC, NspMatrix **Values); 
 extern int nsp_spmaxpcolmatrix_resize_col(NspSpMaxpColMatrix *Sp, int i, int n); 
 extern void SpMaxpColRowDestroy (SpCol *Row); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_redim(NspSpMaxpColMatrix *A, int m, int n, Boolean inplace); 
 extern int nsp_spmaxpcolmatrix_enlarge_cols(NspSpMaxpColMatrix *Sp, int n); 
 extern int nsp_spmaxpcolmatrix_enlarge(NspSpMaxpColMatrix *A, int m, int n); 
 extern int nsp_spmaxpcolmatrix_concatr(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
 extern int nsp_spmaxpcolmatrix_concatd(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
 extern int nsp_spmaxpcolmatrix_concatdiag(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
 extern void  nsp_spmaxpcolmatrix_store(NspSpMaxpColMatrix *A, int r, int c, int col, NspSpMaxpColMatrix *B, int r1, int c1); 
 extern int nsp_spmaxpcolmatrix_insert_elt(NspSpMaxpColMatrix *A, int i, int j, NspSpMaxpColMatrix *B, int rb, int cb); 
 extern int nsp_spmaxpcolmatrix_delete_elt(NspSpMaxpColMatrix *A, int row, int col, int amin, int amax); 
 extern int nsp_spmaxpcolmatrix_get_elt(NspSpMaxpColMatrix *B, int i, int j); 
 extern int nsp_spmaxpcolmatrix_set_rowcol(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols, NspSpMaxpColMatrix *B); 
 extern int nsp_spmaxpcolmatrix_set_row(NspSpMaxpColMatrix *A, NspObject *Inds, NspSpMaxpColMatrix *B); 
 extern int nsp_spmaxpcolmatrix_delete_cols(NspSpMaxpColMatrix *A, NspObject *Cols); 
 extern int nsp_spmaxpcolmatrix_compress_col(NspSpMaxpColMatrix *A, int i); 
 extern int nsp_spmaxpcolmatrix_compress_col_simple(NspSpMaxpColMatrix *A, int i);
 extern int nsp_spmaxpcolmatrix_delete_rows(NspSpMaxpColMatrix *A, NspObject *Rows); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract_elts(NspSpMaxpColMatrix *A, NspObject *Elts); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract_cols(NspSpMaxpColMatrix *A, NspObject *Cols, int *err); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract_rows(NspSpMaxpColMatrix *A, NspObject *Rows, int *err); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_diag_extract(NspSpMaxpColMatrix *A, int k); 
 extern int nsp_spmaxpcolmatrix_set_diag(NspSpMaxpColMatrix *A, NspObject *ODiag, int k); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_diag_create(NspSpMaxpColMatrix *Diag, int k); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_mult(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
 extern NspMatrix *nsp_spmaxpcolmatrix_mult_sp_m(NspSpMaxpColMatrix *A, NspMatrix *B, NspMatrix *Res);
 extern NspMatrix *nsp_spmaxpcolmatrix_pmult_sp_m(NspSpMaxpColMatrix *A, NspMatrix *B, NspMatrix *Res);
 extern NspMatrix *nsp_spmaxpcolmatrix_mult_m_sp(NspMatrix *X,NspSpMaxpColMatrix *A);
 extern int nsp_spmaxpcolmatrix_mult_scalar(double *val, char val_type, NspSpMaxpColMatrix *A);
 extern int nsp_spmaxpcolmatrix_mult_scal(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
 extern int nsp_spmaxpcolmatrix_complexify(NspSpMaxpColMatrix *A); 
 extern int nsp_spmaxpcolmatrix_setr(NspSpMaxpColMatrix *A, double d); 
 extern int nsp_spmaxpcolmatrix_seti(NspSpMaxpColMatrix *A, double d); 
 extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_from_mat(NspMatrix *A); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_from_mat_transpose(NspMatrix *A);
extern NspMaxpMatrix *nsp_spmaxpcolmatrix_to_mpmat(NspSpMaxpColMatrix *Sp); 
extern NspMatrix *nsp_spmaxpcolmatrix_to_mat_transpose(NspSpMaxpColMatrix *Sp);


extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_transpose(const NspSpMaxpColMatrix *A); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_add(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_sub(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_multtt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_divel(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B);

extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_and(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_or(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B);

extern int nsp_spmaxpcolmatrix_mult_scal(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B); 
extern NspMatrix *nsp_spmaxpcolmatrix_op_scal(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int *flag, char op); 

/* SpMaxpColMatOps.c */

extern int nsp_spmaxpcolmatrix_clean(NspSpMaxpColMatrix *A, int rhs, double epsa, double epsr); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_maximinitt_g(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int flag, int minmaxflag, int *err);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_maxitt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int flag, int *err); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_minitt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int flag, int *err); 

extern int nsp_spmaxpcolmatrix_triu(NspSpMaxpColMatrix *A,int k);
extern int nsp_spmaxpcolmatrix_tril(NspSpMaxpColMatrix *A,int k);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_ones(int m, int n);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_spones(NspSpMaxpColMatrix *A);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_eye(int m, int n);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_zeros(int m, int n);

extern int nsp_spmaxpcolmatrix_realpart(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_imagpart(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_isreal(const NspSpMaxpColMatrix *A, int strict);

extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_sum(NspSpMaxpColMatrix *A, int dim); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_maxi(NspSpMaxpColMatrix *A, int dim, NspMatrix **Imax, int lhs); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_mini(NspSpMaxpColMatrix *A, int dim, NspMatrix **Imax, int lhs);

extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_eye(int m, int n); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_ones(int m, int n); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_zeros(int m, int n); 
extern NspMatrix *nsp_spmaxpcolmatrix_acos(NspSpMaxpColMatrix *A); 
extern NspMatrix *nsp_spmaxpcolmatrix_acosh(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_asin(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_asinh(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_atan(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_atanh(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_ceil(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_int(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_floor(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_round(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_sign(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_tan(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_tanh(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_abs(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_erf(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_arg(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_conj(NspSpMaxpColMatrix *A); 
extern NspMatrix *nsp_spmaxpcolmatrix_cos(NspSpMaxpColMatrix *A); 
extern NspMatrix *nsp_spmaxpcolmatrix_cosh(NspSpMaxpColMatrix *A); 
extern NspMatrix *nsp_spmaxpcolmatrix_expel(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_logel(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_sin(NspSpMaxpColMatrix *A); 
extern void nsp_spmaxpcolmatrix_sinh(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_sqrtel(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_minus(NspSpMaxpColMatrix *A); 
extern int nsp_spmaxpcolmatrix_find(NspSpMaxpColMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2,NspMatrix **V); 
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_rand(int m,int n,double sparsity,char crand);
extern int nsp_spmaxpcol_update_from_triplet(NspSpMaxpColMatrix *M);
extern int nsp_spmaxpcol_set_triplet_from_m(NspSpMaxpColMatrix *M,int flag);
extern int nsp_spmaxpcol_alloc_col_triplet(NspSpMaxpColMatrix *M,int nzmax);
extern int nsp_spmaxpcol_realloc_col_triplet(NspSpMaxpColMatrix *M,int nzmax);
extern void nsp_spmaxpcol_free_triplet(NspSpMaxpColMatrix *M);

extern double nsp_spmaxpcolmatrix_norm(NspSpMaxpColMatrix *A, char c);
extern double nsp_spmaxpcolmatrix_vnorm(NspSpMaxpColMatrix *A, double p);

extern int nsp_spmaxpcolmatrix_div_scal_tt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B);
extern NspSpMaxpColMatrix * nsp_spmaxpcolmatrix_div_zero_tt(NspSpMaxpColMatrix *A);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_scal_div_tt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_isnan(NspSpMaxpColMatrix *A,int flag);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_isinf(NspSpMaxpColMatrix *A,int flag);

extern Boolean nsp_spmaxpcolmatrix_is_lower_triangular(const NspSpMaxpColMatrix *A);
extern Boolean nsp_spmaxpcolmatrix_is_upper_triangular(NspSpMaxpColMatrix *A);
extern Boolean nsp_spmaxpcolmatrix_is_symmetric(NspSpMaxpColMatrix *A);
extern int nsp_spmaxpcolmatrix_lower_and_upper_bandwidth(NspSpMaxpColMatrix *A, int *Kl, int *Ku);
extern NspMatrix *nsp_spmaxpcolmatrix_to_lapack_band_format(NspSpMaxpColMatrix *A, int kl, int ku, Boolean enlarge);
extern int nsp_spmaxpcolmatrix_solve_utri(NspSpMaxpColMatrix *U, NspMatrix *x, NspMatrix *b);
extern int nsp_spmaxpcolmatrix_solve_ltri(NspSpMaxpColMatrix *L, NspMatrix *x, NspMatrix *b);
extern int nsp_spmaxpcolmatrix_scale_rows(NspSpMaxpColMatrix *A, NspMatrix *x, char op);
extern int nsp_spmaxpcolmatrix_scale_cols(NspSpMaxpColMatrix *A, NspMatrix *x, char op);
extern int nsp_spmaxpcolmatrix_locate(SpCol *Col,int j);
extern NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_kron(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B);

extern int GenericMatSeRo(void *A, int Am, int An, int Amn,   index_vector *index,
			  void *B, int Bm, int Bn, int Bmn, F_Enlarge F, int *Bscal); 

extern Boolean nsp_spmaxpcolmatrix_test_in_place_assign_OK(NspSpMaxpColMatrix *A, int jA, index_vector *index_r, int *q);
extern void nsp_spmaxpcolmatrix_clean_zeros(SpCol *Col, char type);
extern void nsp_spmaxpcolmatrix_in_place_assign(NspSpMaxpColMatrix *A, int jA, index_vector *index_r, int *q, int *p, NspMatrix *B, int jB);
extern int nsp_spmaxpcolmatrix_assign_by_merge(NspSpMaxpColMatrix *A, int jA, index_vector *index_r, int *p, NspMatrix *B, int jB);
extern int nsp_spmaxpcolmatrix_set_rowcol_from_full(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols, NspMatrix *B);

#endif 

#ifdef SpMaxpColMatrix_Private 
static int init_spmaxpcolmatrix(NspSpMaxpColMatrix *ob,NspTypeSpMaxpColMatrix *type);
static int nsp_spmaxpcolmatrix_size(NspSpMaxpColMatrix *Mat, int flag);
static char *nsp_spmaxpcolmatrix_type_as_string(void);
static char *nsp_spmaxpcolmatrix_type_short_string(NspObject *v);
/* static NspObject *SpMaxpColLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep); */
static int nsp_spmaxpcolmatrix_eq(NspObject *A,NspObject *B);
static int nsp_spmaxpcolmatrix_neq(NspObject *A,NspObject *B);
static NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_xdr_load(XDR  *F);
static int nsp_spmaxpcolmatrix_xdr_save(XDR  *F, NspSpMaxpColMatrix *M);
static NspMethods *spmaxpcolmatrix_get_methods(void); 
#endif 
