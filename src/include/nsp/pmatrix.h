#ifndef NSP_INC_PMATRIX 
#define NSP_INC_PMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"

/*
 * NspPMatrix inherits from NspObject 
 */

typedef struct _NspPmatrix  NspPMatrix;

typedef struct _NspTypePMatrix NspTypePMatrix;


struct _NspTypePMatrix { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} ;

typedef NspMatrix * nsp_polynom;

struct _NspPmatrix {
  /*< private >*/
  NspObject father; 
  NspTypePMatrix *type; 
  /*< public >*/
  int m,n,mn;       /* matrix dimension (m,n,m*n) */
  nsp_polynom *S;     /* Each polynom is a Matrix **/
  char rc_type  ;   /* type 'r' or 'i' */
  char *var  ;   /* name of polynom variable */
};

extern int nsp_type_pmatrix_id;
extern NspTypePMatrix *nsp_type_pmatrix;

int nsp_type_pmatrix_init();

/* only useful when building a new class derived from pmatrix */

NspTypePMatrix *new_type_pmatrix(type_mode mode);

NspPMatrix *new_pmatrix();

/*
 * Object methods redefined for pmatrix 
 */

#define NULLPMAT (NspPMatrix *) 0
#define NULLPOLY (nsp_polynom) 0

/* PMatObj.c */

extern NspPMatrix *nsp_pmatrix_object(NspObject *O); 



/* NspPMatrix.c */

extern NspMatrix *nsp_pmatrix_length(NspPMatrix *A); 
extern NspPMatrix *nsp_matrix_to_pmatrix(NspMatrix *A, nsp_const_string str, int flag); 
extern NspPMatrix *nsp_matrix_to_polynom(NspMatrix *M); 
extern NspPMatrix *nsp_pmatrix_concat_down(const NspPMatrix *A,const NspPMatrix *B); 
extern NspPMatrix *nsp_pmatrix_copy(NspPMatrix *A); 
extern NspPMatrix *nsp_pmatrix_create(char *name, int m, int n, doubleC *cval, int flag); 
extern NspPMatrix *nsp_pmatrix_clone(char *name, NspPMatrix *A, int m, int n, int init);
extern NspPMatrix *nsp_pmatrix_extract(NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspPMatrix *nsp_pmatrix_extract_columns(NspPMatrix *A, NspMatrix *Cols, int *err); 
extern NspPMatrix *nsp_pmatrix_extract_elements(NspPMatrix *A, NspMatrix *Elts, int *err); 
extern NspPMatrix *nsp_pmatrix_extract_rows(NspPMatrix *A, NspMatrix *Rows, int *err); 
extern NspPMatrix *nsp_pmatrix_transpose(const NspPMatrix *A); 
extern int nsp_pcopy_polynom(int n, nsp_polynom *s1, nsp_polynom *s2); 
extern int nsp_pmatrix_add_columns(NspPMatrix *A, int n); 
extern int nsp_pmatrix_add_rows(NspPMatrix *A, int m); 
extern int nsp_pmatrix_concat_right(NspPMatrix *A,const NspPMatrix *B); 
extern int nsp_pmatrix_enlarge(NspPMatrix *A, int m, int n); 
extern int nsp_pmatrix_redim(NspPMatrix *A, int m, int n); 
extern int nsp_pmatrix_resize(NspPMatrix *A, int m, int n); 
extern int nsp_pmatrix_set_rows(NspPMatrix *A, NspMatrix *Rows, NspPMatrix *B); 
extern int nsp_pmatrix_set_submatrix(NspPMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspPMatrix *B); 
extern int nsp_pmatrix_setrc(NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspPMatrix *B); 
extern int nsp_pset_polynom(int n, doubleC *s1, nsp_polynom *s2); 
extern nsp_polynom nsp_basic_to_polynom(doubleC *d, char type); 
extern nsp_polynom nsp_polynom_copy(nsp_polynom P); 
extern unsigned int  nsp_pmatrix_elt_size(NspPMatrix *M);
extern void nsp_pmatrix_destroy(NspPMatrix *A); 
extern int nsp_pmatrix_info(NspPMatrix *Mat, int indent,const char *name, int rec_level); 
extern int nsp_pmatrix_print(NspPMatrix *Mat, int indent,const char *name, int rec_level); 
extern void nsp_polynom_destroy(nsp_polynom *P); 

extern int IsPMatObj (Stack stack, int i); 
extern int IsPMat (NspObject *O); 
extern NspPMatrix *GetPMatCopy (Stack stack, int i); 
extern NspPMatrix *GetPMat (Stack stack, int i); 
extern nsp_polynom GetPolynom (Stack stack, int i); 

extern NspBMatrix *PMatCompOp (NspPMatrix *A, NspPMatrix *B, char *op); 
extern int PMatFullComp (NspPMatrix *A, NspPMatrix *B, char *op, int *err); 
extern NspMatrix *nsp_matrix_companion(NspMatrix *A);
extern NspMatrix *nsp_polynom_roots(nsp_polynom poly);

#endif 

#ifdef PMatrix_Private 
static int init_pmatrix(NspPMatrix *ob,NspTypePMatrix *type);
int nsp_pmatrix_size(NspPMatrix *Mat, int flag);
char *nsp_pmatrix_type_as_string(void);
char *nsp_pmatrix_type_short_string(NspObject *v);
NspObject *nsp_pmatrix_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_pmatrix_eq(NspObject *A,NspObject *B);
int nsp_pmatrix_neq(NspObject *A,NspObject *B);
static int nsp_pmatrix_xdr_save(XDR  *F, NspMatrix *M);
static NspPMatrix *nsp_pmatrix_xdr_load(XDR  *F);
#endif 
