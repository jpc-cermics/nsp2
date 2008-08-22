#ifndef NSP_INC_SPCOLMATRIX 
#define NSP_INC_SPCOLMATRIX 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/triplet.h"

/*
 * NspSpColMatrix inherits from NspObject 
 */

typedef struct _NspSpColmatrix NspSpColMatrix;

typedef struct _NspTypeSpColMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeSpColMatrix;

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

typedef struct _spcol SpCol ;


struct _spcol {
  int size,iw ; /* size of a row, iw : used for working storage*/
  int *J   ; /* pointer to an int array giving the columns or row i 
		in increasing order */
  union { 
    double *R;     /* Pointer on real values */
    doubleC *C;    /* Pointer on complex values */
    int *I;        /* Pointer on integer values */
  };
} ;
  

struct _NspSpColmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeSpColMatrix *type; 
  /*< public >*/
  char rc_type;        /* 'r' or 'i' : real or complex matrix */
  int m,n;   /* mn should be removed since m*n can be bigger than int */
  SpCol **D; /* array of size m giving the Rows datas */
  char convert; /* 't' : the matrix is stored in triplet , 'n': triplet not used   */
  nsp_sparse_triplet triplet; 
} ;

extern int nsp_type_spcolmatrix_id;
extern NspTypeSpColMatrix *nsp_type_spcolmatrix;

int nsp_type_spcolmatrix_init();

/* only useful when building a new class derived from spcolmatrix */

NspTypeSpColMatrix *new_type_spcolmatrix(type_mode mode);

NspSpColMatrix *new_spcolmatrix();

/*
 * Object methods redefined for spcolmatrix 
 */


#define NULLSPCOL (  NspSpColMatrix *) 0

/* SpColMatObj.c */

extern NspSpColMatrix *SpColObj (NspObject *O); 
extern int IsSpColMatObj (Stack stack, int i); 
extern int IsSpColMat(const NspObject *O);
extern NspSpColMatrix *GetSpColCopy (Stack stack, int i); 
extern NspSpColMatrix *GetSpCol (Stack stack, int i); 
extern NspSpColMatrix *GetRealSpCol(Stack stack, int i);

/* NspSpColMatrix.c */

extern void nsp_spcolmatrix_destroy(NspSpColMatrix *Mat); 
extern void nsp_spcolmatrix_col_destroy(SpCol *Col); 
extern int nsp_spcolmatrix_nnz(const NspSpColMatrix *HMat);
extern int nsp_spcolmatrix_info(NspSpColMatrix *Sp, int indent,const char *name, int rec_level);
extern int nsp_spcolmatrix_print(NspSpColMatrix *Sp, int indent,char *name, int rec_level); 
extern NspSpColMatrix *nsp_spcolmatrix_copy(NspSpColMatrix *A); 
extern NspSpColMatrix   *nsp_spcolmatrix_object(NspObject *O);


 extern NspSpColMatrix *nsp_spcolmatrix_create(char *name, char type, int m, int n); 
 extern NspSpColMatrix *nsp_spcolmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n); 
 extern int nsp_spcolmatrix_get(NspSpColMatrix *A, NspMatrix **RC, NspMatrix **Values); 
 extern int nsp_spcolmatrix_resize_col(NspSpColMatrix *Sp, int i, int n); 
extern void SpColRowDestroy (SpCol *Row); 
 extern NspSpColMatrix *nsp_spcolmatrix_redim(NspSpColMatrix *A, int m, int n); 
 extern int nsp_spcolmatrix_enlarge_cols(NspSpColMatrix *Sp, int n); 
 extern int nsp_spcolmatrix_enlarge(NspSpColMatrix *A, int m, int n); 
 extern int nsp_spcolmatrix_concatr(NspSpColMatrix *A, NspSpColMatrix *B); 
 extern int nsp_spcolmatrix_concatd(NspSpColMatrix *A, NspSpColMatrix *B); 
 extern int nsp_spcolmatrix_concatdiag(NspSpColMatrix *A, NspSpColMatrix *B); 
 extern void  nsp_spcolmatrix_store(NspSpColMatrix *A, int r, int c, int col, NspSpColMatrix *B, int r1, int c1); 
 extern int nsp_spcolmatrix_insert_elt(NspSpColMatrix *A, int i, int j, NspSpColMatrix *B, int rb, int cb); 
 extern int nsp_spcolmatrix_delete_elt(NspSpColMatrix *A, int row, int col, int amin, int amax); 
 extern int nsp_spcolmatrix_get_elt(NspSpColMatrix *B, int i, int j); 
 extern int nsp_spcolmatrix_set_rowcol(NspSpColMatrix *A, NspObject *Rows, NspObject *Cols, NspSpColMatrix *B); 
 extern int nsp_spcolmatrix_set_row(NspSpColMatrix *A, NspObject *Inds, NspSpColMatrix *B); 
 extern int nsp_spcolmatrix_delete_cols(NspSpColMatrix *A, NspObject *Cols); 
 extern int nsp_spcolmatrix_compress_col(NspSpColMatrix *A, int i); 
extern int nsp_spcolmatrix_compress_col_simple(NspSpColMatrix *A, int i);
 extern int nsp_spcolmatrix_delete_rows(NspSpColMatrix *A, NspObject *Rows); 
 extern NspSpColMatrix *nsp_spcolmatrix_extract(NspSpColMatrix *A, NspObject *Rows, NspObject *Cols); 
 extern NspSpColMatrix *nsp_spcolmatrix_extract_elts(NspSpColMatrix *A, NspObject *Elts); 
 extern NspSpColMatrix *nsp_spcolmatrix_extract_cols(NspSpColMatrix *A, NspObject *Cols, int *err); 
 extern NspSpColMatrix *nsp_spcolmatrix_extract_rows(NspSpColMatrix *A, NspObject *Rows, int *err); 
 extern NspSpColMatrix *nsp_spcolmatrix_diag_extract(NspSpColMatrix *A, int k); 
 extern int nsp_spcolmatrix_set_diag(NspSpColMatrix *A, NspSpColMatrix *Diag, int k); 
 extern NspSpColMatrix *nsp_spcolmatrix_diag_create(NspSpColMatrix *Diag, int k); 
 extern NspSpColMatrix *nsp_spcolmatrix_mult(NspSpColMatrix *A, NspSpColMatrix *B); 
 extern NspMatrix *nsp_spcolmatrix_mult_sp_m(NspSpColMatrix *A, NspMatrix *B, NspMatrix *Res);
 extern NspMatrix *nsp_spcolmatrix_pmult_sp_m(NspSpColMatrix *A, NspMatrix *B, NspMatrix *Res);
 extern NspMatrix *nsp_spcolmatrix_mult_m_sp(NspMatrix *X,NspSpColMatrix *A);
 extern int nsp_spcolmatrix_mult_scalar(double *val, char val_type, NspSpColMatrix *A);
 extern int nsp_spcolmatrix_mult_scal(NspSpColMatrix *A, NspSpColMatrix *B); 
 extern int nsp_spcolmatrix_complexify(NspSpColMatrix *A); 
 extern int nsp_spcolmatrix_setr(NspSpColMatrix *A, double d); 
 extern int nsp_spcolmatrix_seti(NspSpColMatrix *A, double d); 
 extern NspSpColMatrix *nsp_spcolmatrix_from_mat(NspMatrix *A); 
extern NspSpColMatrix *nsp_spcolmatrix_from_mat_transpose(NspMatrix *A);
 extern NspMatrix *nsp_spcolmatrix_to_mat(NspSpColMatrix *Sp); 
extern NspMatrix *nsp_spcolmatrix_to_mat_transpose(NspSpColMatrix *Sp);


extern NspSpColMatrix *nsp_spcolmatrix_transpose(const NspSpColMatrix *A); 
extern NspSpColMatrix *nsp_spcolmatrix_add(NspSpColMatrix *A, NspSpColMatrix *B); 
extern NspSpColMatrix *nsp_spcolmatrix_sub(NspSpColMatrix *A, NspSpColMatrix *B); 
extern NspSpColMatrix *nsp_spcolmatrix_multtt(NspSpColMatrix *A, NspSpColMatrix *B); 
extern NspSpColMatrix *nsp_spcolmatrix_divel(NspSpColMatrix *A, NspSpColMatrix *B);

extern NspSpColMatrix *nsp_spcolmatrix_and(NspSpColMatrix *A, NspSpColMatrix *B);
extern NspSpColMatrix *nsp_spcolmatrix_or(NspSpColMatrix *A, NspSpColMatrix *B);

extern int nsp_spcolmatrix_mult_scal(NspSpColMatrix *A, NspSpColMatrix *B); 
extern NspMatrix *nsp_spcolmatrix_op_scal(NspSpColMatrix *A, NspSpColMatrix *B, int *flag, char op); 

/* SpColMatOps.c */

extern int nsp_spcolmatrix_clean(NspSpColMatrix *A, int rhs, double epsa, double epsr); 
extern NspSpColMatrix *nsp_spcolmatrix_maximinitt_g(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int minmaxflag, int *err);
extern NspSpColMatrix *nsp_spcolmatrix_maxitt(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int *err); 
extern NspSpColMatrix *nsp_spcolmatrix_minitt(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int *err); 

extern int nsp_spcolmatrix_triu(NspSpColMatrix *A,int k);
extern int nsp_spcolmatrix_tril(NspSpColMatrix *A,int k);
extern NspSpColMatrix *nsp_spcolmatrix_ones(int m, int n);
extern NspSpColMatrix *nsp_spcolmatrix_spones(NspSpColMatrix *A);
extern NspSpColMatrix *nsp_spcolmatrix_eye(int m, int n);
extern NspSpColMatrix *nsp_spcolmatrix_zeros(int m, int n);

extern int nsp_spcolmatrix_realpart(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_imagpart(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_isreal(const NspSpColMatrix *A, int strict);

extern NspSpColMatrix *nsp_spcolmatrix_sum(NspSpColMatrix *A, char *flag); 
extern NspSpColMatrix *nsp_spcolmatrix_maxi(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
extern NspSpColMatrix *nsp_spcolmatrix_mini(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs);

extern NspSpColMatrix *nsp_spcolmatrix_eye(int m, int n); 
extern NspSpColMatrix *nsp_spcolmatrix_ones(int m, int n); 
extern NspSpColMatrix *nsp_spcolmatrix_zeros(int m, int n); 
extern NspMatrix *nsp_spcolmatrix_acos(NspSpColMatrix *A); 
extern NspMatrix *nsp_spcolmatrix_acosh(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_asin(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_asinh(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_atan(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_atanh(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_ceil(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_int(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_floor(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_round(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_sign(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_tan(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_tanh(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_abs(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_erf(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_arg(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_conj(NspSpColMatrix *A); 
extern NspMatrix *nsp_spcolmatrix_cos(NspSpColMatrix *A); 
extern NspMatrix *nsp_spcolmatrix_cosh(NspSpColMatrix *A); 
extern NspMatrix *nsp_spcolmatrix_expel(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_logel(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_sin(NspSpColMatrix *A); 
extern void nsp_spcolmatrix_sinh(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_sqrtel(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_minus(NspSpColMatrix *A); 
extern int nsp_spcolmatrix_find(NspSpColMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
extern NspSpColMatrix *nsp_spcolmatrix_rand(int m,int n,double sparsity,char crand);
extern int nsp_spcol_update_from_triplet(NspSpColMatrix *M);
extern int nsp_spcol_set_triplet_from_m(NspSpColMatrix *M,int flag);
extern int nsp_spcol_alloc_col_triplet(NspSpColMatrix *M,int nzmax);
extern int nsp_spcol_realloc_col_triplet(NspSpColMatrix *M,int nzmax);
extern void nsp_spcol_free_triplet(NspSpColMatrix *M);

extern double nsp_spcolmatrix_norm(NspSpColMatrix *A, char c);
extern double nsp_spcolmatrix_vnorm(NspSpColMatrix *A, double p);

extern int nsp_spcolmatrix_div_scal_tt(NspSpColMatrix *A, NspSpColMatrix *B);
extern NspSpColMatrix * nsp_spcolmatrix_div_zero_tt(NspSpColMatrix *A);
extern NspSpColMatrix *nsp_spcolmatrix_scal_div_tt(NspSpColMatrix *A, NspSpColMatrix *B);
extern NspSpColMatrix *nsp_spcolmatrix_isnan(NspSpColMatrix *A,int flag);
extern NspSpColMatrix *nsp_spcolmatrix_isinf(NspSpColMatrix *A,int flag);
extern Boolean nsp_spcolmatrix_is_lower_triangular(NspSpColMatrix *A);
extern Boolean nsp_spcolmatrix_is_upper_triangular(NspSpColMatrix *A);
extern Boolean nsp_spcolmatrix_is_symmetric(NspSpColMatrix *A);
extern int nsp_spcolmatrix_lower_and_upper_bandwidth(NspSpColMatrix *A, int *Kl, int *Ku);

extern int nsp_spcolmatrix_solve_utri(NspSpColMatrix *U, NspMatrix *x, NspMatrix *b);
extern int nsp_spcolmatrix_solve_ltri(NspSpColMatrix *L, NspMatrix *x, NspMatrix *b);
extern int nsp_spcolmatrix_scale_rows(NspSpColMatrix *A, NspMatrix *x);
extern int nsp_spcolmatrix_scale_cols(NspSpColMatrix *A, NspMatrix *x);


extern int GenericMatSeRo(void *A, int Am, int An, int Amn,   index_vector *index,
			  void *B, int Bm, int Bn, int Bmn, F_Enlarge F, int *Bscal); 


#endif 

#ifdef SpColMatrix_Private 
static int init_spcolmatrix(NspSpColMatrix *ob,NspTypeSpColMatrix *type);
static int nsp_spcolmatrix_size(NspSpColMatrix *Mat, int flag);
static char *nsp_spcolmatrix_type_as_string(void);
static char *nsp_spcolmatrix_type_short_string(NspObject *v);
/* static NspObject *SpColLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep); */
static int nsp_spcolmatrix_eq(NspObject *A,NspObject *B);
static int nsp_spcolmatrix_neq(NspObject *A,NspObject *B);
static NspSpColMatrix *nsp_spcolmatrix_xdr_load(XDR  *F);
static int nsp_spcolmatrix_xdr_save(XDR  *F, NspSpColMatrix *M);
static NspMethods *spcolmatrix_get_methods(void); 
#endif 
