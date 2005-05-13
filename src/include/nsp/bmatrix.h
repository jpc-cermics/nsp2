#ifndef INC_NSP_BMATRIX 
#define INC_NSP_BMATRIX

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/
  
/* Boolean Matrix */
typedef integer Boolean;

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

/*
 * NspBMatrix inherits from NspObject 
 */

typedef struct _NspBmatrix NspBMatrix;

typedef int (*bmatrix_save) (NspFile  *F, NspBMatrix *M);

typedef struct _NspTypeBMatrix { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  bmatrix_save *save;
} NspTypeBMatrix;

struct _NspBmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeBMatrix *type; 
  /*< public >*/
  integer m,n,mn;
  Boolean *B;	
};

#include "nsp/matrix.h" 
#include "nsp/smatrix.h" 

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

#ifdef BMatrix_Private 
static int init_bmatrix(NspBMatrix *ob,NspTypeBMatrix *type);
static int bmatrix_size(NspBMatrix *Mat, int flag);
static char *bmatrix_type_as_string(void);
static char *bmatrix_type_short_string(void);
static NspObject *bmatrix_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int bmatrix_eq(NspBMatrix *A, NspObject *B);
static int bmatrix_neq(NspBMatrix *A, NspObject *B);
static int bmatrix_xdr_save(NspFile  *F, NspBMatrix *M);
static NspBMatrix  *bmatrix_xdr_load(NspFile  *F);
static AttrTab bmatrix_attrs[];
static NspMethods *bmatrix_get_methods(void); 
/*static NspObject *bmatrix_path_extract(NspBMatrix *A, NspObject *O); */
static int bmatrix_is_true(NspBMatrix *M);
#endif /* BMatrix_Private */

#define NULLBMAT (NspBMatrix*) 0

/* from BMatObj.c */

extern NspBMatrix *BMatObj (NspObject *O); 
extern int IsBMatObj (Stack stack, int i); 
extern int IsBMat(NspObject *O);
extern NspBMatrix *GetBMatCopy (Stack stack, int i); 
extern NspBMatrix *GetBMat (Stack stack, int i); 
extern int BoolScalar (NspObject *O, Boolean *val); 
extern int GetScalarBool (Stack stack, int i, integer *val); 

/* from NspBMatrix.c */

extern NspBMatrix *nsp_bmatrix_create(char *name, integer m, integer n); 
extern NspBMatrix *nsp_bmatrix_copy(NspBMatrix *A); 
extern int nsp_bmatrix_resize(NspBMatrix *A, integer m, integer n); 
extern void nsp_bmatrix_destroy(NspBMatrix *BMat); 
extern void nsp_bmatrix_info(NspBMatrix *BMat, int indent); 
extern void nsp_bmatrix_print(NspBMatrix *BMat, int indent,int header); 
extern void nsp_bmatrix_latex_print(NspBMatrix *BMat); 
extern void nsp_bmatrix_latex_tab_print(NspBMatrix *BMat); 
extern int nsp_bmatrix_redim(NspBMatrix *A, integer m, integer n); 
extern int nsp_bmatrix_enlarge(NspBMatrix *A, integer m, integer n); 
extern int nsp_bmatrix_concat_right(NspBMatrix *A, NspBMatrix *B); 
extern int nsp_bmatrix_add_columns(NspBMatrix *A, integer n); 
extern NspBMatrix *nsp_bmatrix_concat_down(NspBMatrix *A, NspBMatrix *B); 
extern NspBMatrix *nsp_bmatrix_concat_diag(NspBMatrix *A, NspBMatrix *B); 
extern int nsp_bmatrix_add_rows(NspBMatrix *A, integer m); 
extern int nsp_bmatrix_set_submatrix(NspBMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspBMatrix *B); 
extern int nsp_bmatrix_set_rows(NspBMatrix *A, NspMatrix *Rows, NspBMatrix *B); 
extern int nsp_bmatrix_delete_columns(NspBMatrix *A, NspMatrix *Cols); 
extern int nsp_bmatrix_delete_rows(NspBMatrix *A, NspMatrix *Rows); 
extern int nsp_bmatrix_delete_elements(NspBMatrix *A, NspMatrix *Elts); 
extern NspBMatrix *nsp_bmatrix_extract(NspBMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspBMatrix *nsp_bmatrix_extract_elements(NspBMatrix *A, NspMatrix *Elts); 
extern NspBMatrix *nsp_bmatrix_extract_columns(NspBMatrix *A, NspMatrix *Cols); 
extern NspBMatrix *nsp_bmatrix_extract_rows(NspBMatrix *A, NspMatrix *Rows); 
extern NspBMatrix *BMatLoopCol (char *str, NspBMatrix *Col, NspBMatrix *A, int icol, int *rep); 
extern NspBMatrix *nsp_bmatrix_extract_diag(NspBMatrix *A, integer k); 
extern int nsp_bmatrix_set_diag(NspBMatrix *A, NspBMatrix *Diag, integer k); 
extern NspBMatrix *nsp_bmatrix_create_diag(NspBMatrix *Diag, integer k); 
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
extern int nsp_bmatrix_find_2(const NspBMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
extern NspBMatrix *nsp_bmatrix_compare(const NspBMatrix *A, const NspBMatrix *B, char *op); 
extern int nsp_bmatrix_full_compare(const NspBMatrix *A,const  NspBMatrix *B, char *op, int *err); 

/* from MatOps.c */

extern NspBMatrix *nsp_mat_comp(NspMatrix *A, NspMatrix *B, char *op); 

extern NspBMatrix  *nsp_mat_isinf(NspMatrix *A);
extern NspBMatrix  *nsp_mat_isnan(NspMatrix *A);
extern NspBMatrix  *nsp_mat_finite(NspMatrix *A);


#endif 

