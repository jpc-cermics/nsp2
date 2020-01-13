#ifndef NSP_INC_BMATRIX 
#define NSP_INC_BMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* Boolean Matrix */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/objectf.h" 

/*
 * NspBMatrix inherits from NspObject 
 */

/**
 * NspBMatrix: 
 * @m: number of rows 
 * @n: number of columns
 * @mn: @m x @n
 * @B: array containing the values 
 *
 * inherits from #NspObject 
 */

/* typedef struct _NspBMatrix NspBMatrix; */
typedef struct _NspTypeBMatrix  NspTypeBMatrix;

struct _NspTypeBMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

struct _NspBMatrix {
  /*< private >*/
  NspObject father; 
  NspTypeBMatrix *type; 
  /*< public >*/
  int m,n,mn;
  Boolean *B;	
};

/**
 * nsp_type_bmatrix_id:
 *
 * to be done 
 */

extern int nsp_type_bmatrix_id;
extern NspTypeBMatrix *nsp_type_bmatrix;

int nsp_type_bmatrix_init();

/* only useful when building a new class derived from bmatrix */

NspTypeBMatrix *new_type_bmatrix(type_mode mode);

/* initialize type for Object */

int nsp_type_bmatrix_init(void);

/* only useful when building a new class derived from bmatrix */

void nsp_type_bmatrix_set(NspBMatrix *bmatrix, NspTypeBMatrix *type);

NspBMatrix *new_bmatrix();

/*
 * Object methods redefined for bmatrix 
 */

#define NULLBMAT (NspBMatrix*) 0

/* from BMatObj.c */

extern NspBMatrix *BMatObj (NspObject *O); 
extern int IsBMatObj (Stack stack, int i); 
extern int IsBMat(const NspObject *O);
extern NspBMatrix *GetBMatCopy (Stack stack, int i); 
extern NspBMatrix *GetBMat (Stack stack, int i); 
extern int BoolScalar (NspObject *O, Boolean *val); 
extern int GetScalarBool (Stack stack, int i, int *val); 

/* from NspBMatrix.c */

extern NspBMatrix *nsp_bmatrix_create(const char *name, int m, int n); 
extern NspBMatrix *nsp_bmatrix_clone(const char *name, NspBMatrix *A, int m, int n, int init);
extern NspBMatrix *nsp_bmatrix_copy(NspBMatrix *A); 
extern unsigned int  nsp_bmatrix_elt_size(NspMatrix *M);
extern int nsp_bmatrix_resize(NspBMatrix *A, int m, int n); 
extern void nsp_bmatrix_destroy(NspBMatrix *BMat); 
extern int nsp_bmatrix_info(NspBMatrix *BMat, int indent,const char *name, int rec_level); 
extern int nsp_bmatrix_print(NspBMatrix *BMat, int indent,const char *name, int rec_level); 
extern int nsp_bmatrix_latex_print(NspBMatrix *BMat, int use_math,const char *name, int rec_level);
extern int nsp_bmatrix_latex_tab_print(NspBMatrix *BMat, int use_math,const char *name, int rec_level); 
extern int nsp_bmatrix_redim(NspBMatrix *A, int m, int n); 
extern int nsp_bmatrix_enlarge(NspBMatrix *A, int m, int n); 
extern int nsp_bmatrix_concat_right(NspBMatrix *A, NspBMatrix *B); 
extern int nsp_bmatrix_add_columns(NspBMatrix *A, int n); 
extern NspBMatrix *nsp_bmatrix_concat_down(NspBMatrix *A, NspBMatrix *B); 
extern NspBMatrix *nsp_bmatrix_concat_diag(NspBMatrix *A, NspBMatrix *B); 
extern int nsp_bmatrix_add_rows(NspBMatrix *A, int m); 
extern int nsp_bmatrix_set_submatrix(NspBMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspBMatrix *B); 
extern int nsp_bmatrix_set_rows(NspBMatrix *A, NspMatrix *Rows, NspBMatrix *B); 
extern NspBMatrix *nsp_bmatrix_extract(NspBMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspBMatrix *nsp_bmatrix_extract_elements(NspBMatrix *A, NspMatrix *Elts); 
extern NspBMatrix *nsp_bmatrix_extract_columns(NspBMatrix *A, NspMatrix *Cols); 
extern NspBMatrix *nsp_bmatrix_extract_rows(NspBMatrix *A, NspMatrix *Rows); 
extern NspBMatrix *BMatLoopCol (char *str, NspBMatrix *Col, NspBMatrix *A, int icol, int *rep); 
extern NspBMatrix *nsp_bmatrix_extract_diag(NspBMatrix *A, int k); 
extern int nsp_bmatrix_set_diag(NspBMatrix *A, NspBMatrix *Diag, int k); 
extern NspBMatrix *nsp_bmatrix_create_diag(NspBMatrix *Diag, int k); 
extern NspBMatrix *nsp_bmatrix_transpose(NspBMatrix *A); 
extern NspBMatrix *nsp_matrix_to_bmatrix(NspMatrix *M); 
extern NspMatrix *nsp_bmatrix_to_matrix(NspBMatrix *M); 
extern int MatIsTrue (NspMatrix *M); 
extern int nsp_bmatrix_and(NspBMatrix *A,const NspBMatrix *B); 
extern int nsp_bmatrix_scalar_and(NspBMatrix *A,const NspBMatrix *B); 
extern int nsp_bmatrix_or(NspBMatrix *A,const NspBMatrix *B); 
extern int nsp_bmatrix_scalar_or(NspBMatrix *A,const NspBMatrix *B); 
extern int nsp_bmatrix_not(NspBMatrix *A); 
extern int BMatIsTrue (NspBMatrix *A); 
extern NspMatrix *nsp_bmatrix_count_true(const NspBMatrix *A); 
extern NspMatrix *nsp_bmatrix_find(const NspBMatrix *A); 
extern int nsp_bmatrix_find_2(const NspBMatrix *A, int lhs, NspObject **Res1, NspObject **Res2, char ind_type);
extern NspBMatrix *nsp_bmatrix_compare(const NspBMatrix *A, const NspBMatrix *B, char *op); 
extern int nsp_bmatrix_full_compare(const NspBMatrix *A,const  NspBMatrix *B, char *op, int *err); 

/* from MatOps.c */

extern NspBMatrix *nsp_mat_comp(NspMatrix *A, NspMatrix *B,const char *op); 
extern NspBMatrix *nsp_mat_comp_real(NspMatrix *A, NspMatrix *B,const char *op);

extern NspBMatrix  *nsp_mat_isinf(NspMatrix *A);
extern NspBMatrix  *nsp_mat_isnan(NspMatrix *A);
extern NspBMatrix  *nsp_mat_finite(NspMatrix *A);

extern NspBMatrix *nsp_bmatrix_eye(int m, int n);
extern NspBMatrix *nsp_bmatrix_ones(int m, int n);
extern NspBMatrix *nsp_bmatrix_zeros(int m, int n);

#endif 

#ifdef BMatrix_Private 
static int init_bmatrix(NspBMatrix *ob,NspTypeBMatrix *type);
static int bmatrix_size(NspBMatrix *Mat, int flag);
static char *bmatrix_type_as_string(void);
static char *bmatrix_type_short_string(NspObject *v);
static NspObject *bmatrix_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int bmatrix_eq(NspBMatrix *A, NspObject *B);
static int bmatrix_neq(NspBMatrix *A, NspObject *B);
static int bmatrix_xdr_save(XDR  *xdrs, NspBMatrix *M);
static NspBMatrix  *bmatrix_xdr_load(XDR  *F);
/* static AttrTab bmatrix_attrs[];*/
static NspMethods *bmatrix_get_methods(void); 
/*static NspObject *bmatrix_path_extract(NspBMatrix *A, NspObject *O); */
static int bmatrix_is_true(NspBMatrix *M);
static int nsp_bmatrix_as_index(NspBMatrix * M, index_vector *index);
#endif /* BMatrix_Private */
