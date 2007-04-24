#ifndef NSP_INC_SPROWMATRIX 
#define NSP_INC_SPROWMATRIX 

/*
 * This Software is GPL (Copyright ENPC 1998-2006) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/triplet.h"

/*
 * NspSpRowMatrix inherits from NspObject 
 */

typedef struct _NspSpRowmatrix NspSpRowMatrix;

typedef struct _NspTypeSpRowMatrix  NspTypeSpRowMatrix;

struct _NspTypeSpRowMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

/* Sparse matrices 
 * 
 * m,n :  size of matrix 
 * D[i]-> points to the ith column
 * D[i]-> size : number of non nul elements in col i 
 * D[i]->J : array of rows of the elements of lin i
 * D[i]->R : array of real part of the elements of lin i
 * D[i]->C : array of imaginary part of the elements of lin i
 */

/* used to store a matlab compatible representation */

typedef struct _sprow  SpRow ;

struct _sprow {
  int size,iw ; /* size of a row, iw : used for working storage*/
  int *J   ; /* pointer to an int array giving the columns or row i 
		in increasing order */
  union { 
    double *R;     /* Pointer on real values */
    doubleC *C;    /* Pointer on complex values */
    int *I;        /* Pointer on integer values */
  };
};
  

struct _NspSpRowmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeSpRowMatrix *type; 
  /*< public >*/
  char rc_type;        /* 'r' or 'i' : real or complex matrix */
  int n,m,mn;   /* mn should be removed since m*n can be bigger than int 
		 * Warning: the order n,m should be the inverse to the 
		 * one implemented in _NspSpColmatrix. Thus we can cast 
		 * from _NspSpColmatrix to _NspSpRowmatrix
		 */
  SpRow **D; /* array of size m giving the Rows datas */
  char convert; /* 't' : the matrix is stored in triplet , 'n': triplet not used   */
  nsp_sparse_triplet triplet; 
} ;

extern int nsp_type_sprowmatrix_id;
extern NspTypeSpRowMatrix *nsp_type_sprowmatrix;

int nsp_type_sprowmatrix_init();

/* only useful when building a new class derived from sprowmatrix */

NspTypeSpRowMatrix *new_type_sprowmatrix(type_mode mode);

NspSpRowMatrix *new_sprowmatrix();

/*
 * Object methods redefined for sprowmatrix 
 */


#define NULLSPROW (  NspSpRowMatrix *) 0

/* SpRowMatObj.c */

extern NspSpRowMatrix *SpRowObj (NspObject *O); 
extern int IsSpRowMatObj (Stack stack, int i); 
extern int IsSpRowMat(const NspObject *O);
extern NspSpRowMatrix *GetSpRowCopy (Stack stack, int i); 
extern NspSpRowMatrix *GetSpRow (Stack stack, int i); 
extern NspSpRowMatrix *GetRealSpRow(Stack stack, int i);

/* NspSpRowMatrix.c */

extern void nsp_sprowmatrix_destroy(NspSpRowMatrix *Mat); 
extern void nsp_sprowmatrix_row_destroy(SpRow *Row); 
extern int nsp_sprowmatrix_nnz(const NspSpRowMatrix *HMat);
extern void nsp_sprowmatrix_info(NspSpRowMatrix *Sp, int indent,char *name, int rec_level); 
extern void nsp_sprowmatrix_print(NspSpRowMatrix *Sp, int indent,char *name, int rec_level); 
extern NspSpRowMatrix *nsp_sprowmatrix_copy(NspSpRowMatrix *A); 
extern NspSpRowMatrix   *nsp_sprowmatrix_object(NspObject *O);


 extern NspSpRowMatrix *nsp_sprowmatrix_create(char *name, char type, int m, int n); 
 extern NspSpRowMatrix *nsp_sprowmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n); 
 extern int nsp_sprowmatrix_get(NspSpRowMatrix *A, NspMatrix **RC, NspMatrix **Values); 
 extern int nsp_sprowmatrix_resize_row(NspSpRowMatrix *Sp, int i, int n); 
extern void SpRowRowDestroy (SpRow *Row); 
 extern NspSpRowMatrix *nsp_sprowmatrix_redim(NspSpRowMatrix *A, int m, int n); 
 extern int nsp_sprowmatrix_enlarge_rows(NspSpRowMatrix *Sp, int m); 
 extern int nsp_sprowmatrix_enlarge(NspSpRowMatrix *A, int m, int n); 
 extern int nsp_sprowmatrix_concatr(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern int nsp_sprowmatrix_concatd(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern int nsp_sprowmatrix_concatdiag(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern void  nsp_sprowmatrix_store(NspSpRowMatrix *A, int r, int c, int col, NspSpRowMatrix *B, int r1, int c1); 
 extern int nsp_sprowmatrix_insert_elt(NspSpRowMatrix *A, int i, int j, NspSpRowMatrix *B, int rb, int cb); 
 extern int nsp_sprowmatrix_delete_elt(NspSpRowMatrix *A, int row, int col, int amin, int amax); 
 extern int nsp_sprowmatrix_get_elt(NspSpRowMatrix *B, int i, int j); 
 extern int nsp_sprowmatrix_set_rowcol(NspSpRowMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspSpRowMatrix *B); 
 extern int nsp_sprowmatrix_set_row(NspSpRowMatrix *A, NspMatrix *Inds, NspSpRowMatrix *B); 
 extern int nsp_sprowmatrix_delete_cols(NspSpRowMatrix *A, NspMatrix *Cols); 
 extern int nsp_sprowmatrix_compress_row(NspSpRowMatrix *A, int i); 
extern int nsp_sprowmatrix_compress_row_simple(NspSpRowMatrix *A, int i);
 extern int nsp_sprowmatrix_delete_rows(NspSpRowMatrix *A, NspMatrix *Rows); 
 extern NspSpRowMatrix *nsp_sprowmatrix_extract(NspSpRowMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
 extern NspSpRowMatrix *nsp_sprowmatrix_extract_elts(NspSpRowMatrix *A, NspMatrix *Elts); 
 extern NspSpRowMatrix *nsp_sprowmatrix_extract_cols(NspSpRowMatrix *A, NspMatrix *Cols, int *err); 
 extern NspSpRowMatrix *nsp_sprowmatrix_extract_rows(NspSpRowMatrix *A, NspMatrix *Rows, int *err); 
 extern NspSpRowMatrix *nsp_sprowmatrix_diag_extract(NspSpRowMatrix *A, int k); 
 extern int nsp_sprowmatrix_diag_set(NspSpRowMatrix *A, NspSpRowMatrix *Diag, int k); 
 extern NspSpRowMatrix *nsp_sprowmatrix_diag_create(NspSpRowMatrix *Diag, int k); 
 extern NspSpRowMatrix *nsp_sprowmatrix_mult(NspSpRowMatrix *A, NspSpRowMatrix *B); 
extern NspMatrix *nsp_sprowmatrix_mult_sp_m(NspSpRowMatrix *A, NspMatrix *X);
extern NspMatrix *nsp_sprowmatrix_mult_m_sp(NspMatrix *X,NspSpRowMatrix *A);
extern int nsp_sprowmatrix_mult_scal(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern int nsp_sprowmatrix_complexify(NspSpRowMatrix *A); 
 extern int nsp_sprowmatrix_setr(NspSpRowMatrix *A, double d); 
 extern int nsp_sprowmatrix_seti(NspSpRowMatrix *A, double d); 
 extern NspSpRowMatrix *nsp_sprowmatrix_from_mat(NspMatrix *A); 
 extern NspMatrix *nsp_sprowmatrix_to_mat(NspSpRowMatrix *Sp); 
extern NspSpRowMatrix *nsp_sprowmatrix_transpose(const NspSpRowMatrix *A); 
 extern NspSpRowMatrix *nsp_sprowmatrix_add(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern NspSpRowMatrix *nsp_sprowmatrix_sub(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern NspSpRowMatrix *nsp_sprowmatrix_multtt(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern int nsp_sprowmatrix_mult_scal(NspSpRowMatrix *A, NspSpRowMatrix *B); 
 extern NspMatrix *nsp_sprowmatrix_op_scal(NspSpRowMatrix *A, NspSpRowMatrix *B, int *flag, char op); 

/* SpRowMatOps.c */

 extern int nsp_sprowmatrix_clean(NspSpRowMatrix *A, int rhs, double epsa, double epsr); 
 extern NspSpRowMatrix *nsp_sprowmatrix_maximinitt_g(NspSpRowMatrix *A, NspSpRowMatrix *B, int flag, int minmaxflag, int *err); 
extern NspSpRowMatrix *nsp_sprowmatrix_maxitt(NspSpRowMatrix *A, NspSpRowMatrix *B, int flag, int *err); 
 extern NspSpRowMatrix *nsp_sprowmatrix_minitt(NspSpRowMatrix *A, NspSpRowMatrix *B, int flag, int *err); 

extern int nsp_sprowmatrix_triu(NspSpRowMatrix *A,int k);
extern int nsp_sprowmatrix_tril(NspSpRowMatrix *A,int k);
extern NspSpRowMatrix *nsp_sprowmatrix_ones(int m, int n);
extern NspSpRowMatrix *nsp_sprowmatrix_eye(int m, int n);
extern NspSpRowMatrix *nsp_sprowmatrix_zeros(int m, int n);

 extern int nsp_sprowmatrix_realpart(NspSpRowMatrix *A); 
 extern int nsp_sprowmatrix_imagpart(NspSpRowMatrix *A); 
 extern NspSpRowMatrix *nsp_sprowmatrix_sum(NspSpRowMatrix *A, char *flag); 
 extern NspSpRowMatrix *nsp_sprowmatrix_maxi(NspSpRowMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
 extern NspSpRowMatrix *nsp_sprowmatrix_eye(int m, int n); 
 extern NspSpRowMatrix *nsp_sprowmatrix_ones(int m, int n); 
 extern NspSpRowMatrix *nsp_sprowmatrix_zeros(int m, int n); 
 extern NspMatrix *nsp_sprowmatrix_acos(NspSpRowMatrix *A); 
 extern NspMatrix *nsp_sprowmatrix_acosh(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_asin(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_asinh(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_atan(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_atanh(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_ceil(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_int(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_floor(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_round(NspSpRowMatrix *A); 
 extern int nsp_sprowmatrix_sign(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_tan(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_tanh(NspSpRowMatrix *A); 
 extern int nsp_sprowmatrix_abs(NspSpRowMatrix *A); 
 extern int nsp_sprowmatrix_erf(NspSpRowMatrix *A); 
 extern int nsp_sprowmatrix_arg(NspSpRowMatrix *A); 
 extern void nsp_sprowmatrix_conj(NspSpRowMatrix *A); 
 extern NspMatrix *nsp_sprowmatrix_cos(NspSpRowMatrix *A); 
 extern NspMatrix *nsp_sprowmatrix_cosh(NspSpRowMatrix *A); 
 extern NspMatrix *nsp_sprowmatrix_expel(NspSpRowMatrix *A); 
extern int nsp_sprowmatrix_logel(NspSpRowMatrix *A); 
extern void nsp_sprowmatrix_sin(NspSpRowMatrix *A); 
extern void nsp_sprowmatrix_sinh(NspSpRowMatrix *A); 
extern int nsp_sprowmatrix_sqrtel(NspSpRowMatrix *A); 
extern int nsp_sprowmatrix_minus(NspSpRowMatrix *A); 
extern int nsp_sprowmatrix_find(NspSpRowMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
extern NspSpRowMatrix *nsp_sprowmatrix_rand(int m,int n,double sparsity,char crand);

extern int nsp_sprow_update_from_triplet(NspSpRowMatrix *M);
extern int nsp_sprow_set_triplet_from_m(NspSpRowMatrix *M,int flag);
extern int nsp_sprow_alloc_col_triplet(NspSpRowMatrix *M,int nzmax);
extern int nsp_sprow_realloc_col_triplet(NspSpRowMatrix *M,int nzmax);

#endif 


#ifdef SpRowMatrix_Private 
static int init_sprowmatrix(NspSpRowMatrix *ob,NspTypeSpRowMatrix *type);
static int nsp_sprowmatrix_size(NspSpRowMatrix *Mat, int flag);
static NspSpRowMatrix *nsp_sprowmatrix_xdr_load(XDR  *F);
static int nsp_sprowmatrix_xdr_save(XDR  *F, NspSpRowMatrix *M);
static char *nsp_sprowmatrix_type_as_string(void);
static char *nsp_sprowmatrix_type_short_string(NspObject *v);
static NspObject *SpRowLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int nsp_sprowmatrix_eq(NspObject *A,NspObject *B);
static int nsp_sprowmatrix_neq(NspObject *A,NspObject *B);
#endif 
