#ifndef NSP_INC_Matint
#define NSP_INC_Matint

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* interface Matint */

#include "nsp/object.h"

typedef int matint_redim(NspObject *M,int m,int n); 
typedef int matint_resize(void *M,int m,int n); 
typedef int matint_free_elt(void **elt);
typedef unsigned int matint_elt_size(const void *M);
typedef NspObject *matint_clone(const char *name,const void *M, int m,int n, int init); 
typedef char *matint_copy_elt(char *from);
typedef int matint_enlarge(void *M, int m, int n);
typedef NspObject * matint_canonic(NspObject *obj);
typedef int matint_basic_copy_ind(void);

typedef struct _NspTypeMatint NspTypeMatint ; 

struct _NspTypeMatint {
  NSP_TYPE_OBJECT__
  /* added */
  matint_redim *redim; 
  matint_resize *resize;
  matint_free_elt *free_elt;
  matint_elt_size *elt_size;
  matint_clone *clone;
  matint_copy_elt *copy_elt;
  matint_enlarge *enlarge;
  matint_canonic *canonic;
  matint_basic_copy_ind *copy_ind;
};

#define MAT_INT(t) ((NspTypeMatint *) t)

extern int nsp_type_matint_id;
extern NspTypeMatint *nsp_type_matint;

NspTypeMatint *new_type_matint(type_mode mode);
extern NspMethods *matint_get_methods(void);

extern int nsp_matint_tozero(NspObject *Obj);
extern int nsp_matint_delete_columns(NspObject  *Obj, int *ind, int nb_elts, int cmin, int cmax);
extern int nsp_matint_delete_rows(NspObject *Obj, int *ind, int nb_elts, int rmin, int rmax);
extern int nsp_matint_delete_elements(NspObject *Obj, int *ind, int nb_elts, int rmin, int rmax);
extern int nsp_matint_delete_elements2(NspObject *Obj, 
				       int *indrow, int nr, int rmin, int rmax,
				       int *indcol, int nc, int cmin, int cmax);
/* extern NspObject *nsp_matint_extract_elements(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax); */
extern NspObject *nsp_matint_extract_elements1(NspObject *Obj,NspObject *Elts);

/* extern NspObject *nsp_matint_extract_columns(NspObject *Obj, const int *ind, int nb_elts, int cmin, int cmax); */
extern NspObject *nsp_matint_extract_columns1(NspObject *Obj,NspObject *Cols);
/* extern NspObject *nsp_matint_extract_rows(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax);*/
extern NspObject *nsp_matint_extract_rows1(NspObject *Obj,NspObject *Rows);

extern NspObject *nsp_matint_extract(NspObject *Obj, 
				     const int *row, int nr, int rmin, int rmax, 
				     const int *col, int nc, int cmin, int cmax);

extern NspObject *nsp_matint_extract1(NspObject *Obj,NspObject *Rows, NspObject *Cols);

extern int nsp_matint_set_submatrix(NspObject *ObjA, 
				    const int *row, int nr, int rmin, int rmax,
				    const int *col, int nc, int cmin, int cmax,
				    NspObject *ObjB);
extern int nsp_matint_set_elts(NspObject *ObjA, const int *ind, int nb_elts, int imin, int imax, NspObject *ObjB);
extern int nsp_matint_set_elts1(NspObject *ObjA, NspObject *Elts, NspObject *ObjB);
extern NspObject *nsp_matint_concat_right( NspObject *ObjA, NspObject *ObjB);
extern int nsp_matint_concat_right_bis(NspObject *ObjA, NspObject *ObjB);
extern NspObject *nsp_matint_repmat(const NspObject *ObjA, int m, int n);
typedef NspObject *(*Fconcat_d) (const NspObject *, const NspObject *);
extern NspObject *nsp_matint_concat_down(NspObject *ObjA, NspObject *ObjB);
extern int nsp_matint_redim(NspObject *Obj, int m, int n);
extern int nsp_matint_concat_down_bis(NspObject *ObjA, NspObject *ObjB);
extern NspObject *nsp_matint_concat_diag( NspObject *ObjA, NspObject *ObjB);
typedef enum { matint_iwork1=0, matint_iwork2=1} matint_workid;
extern int *get_index_vector_from_object(NspObject *Obj, int *Nb_elts, int *Rmin, int *Rmax,matint_workid iwork);
extern int nsp_matint_set_submatrix1(NspObject *ObjA,NspObject *Row, NspObject *Col, NspObject *ObjB);
extern int nsp_matint_perm_elem(NspObject *ObjA, int p, int q, int dim_flag);

extern int int_matint_cells_setrowscols(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_concat_diag(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_concat_down(Stack stack, int rhs, int opt, int lhs, Fconcat_d F);
extern int int_matint_concat_emptymat_and_mat(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_concatd(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_concatr(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_deletecols(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_deleteelts(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_deleteelts2(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_deleterows(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extract(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractcols(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractelts(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractrows(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractrows_mat(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractrows_int(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractrows_gen(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_extractrows_pointer(Stack stack, int rhs, int opt, int lhs);

extern int int_matint_redim(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_repmat(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_resize2vect(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_setrowscols(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_tozero(Stack stack, int rhs, int opt, int lhs);
extern NspObject * nsp_matint_canonic(NspObject *obj);
extern int int_matint_isvector(Stack stack, int rhs, int opt, int lhs);

#ifdef COPY_IND1 
extern int nsp_matint_basic_copy_pointer(const NspTypeBase *type,NspObject *A,NspObject *B, 
				  const int *ind, int nb_elts);
extern int nsp_matint_basic_copy_mat(const NspTypeBase *type,NspObject *A,NspObject *B, 
				     const int *ind, int nb_elts);
extern int nsp_matint_basic_copy_int(const NspTypeBase *type,NspObject *A, NspObject *B, 
				     const int *ind, int nb_elts);
extern int nsp_matint_basic_copy(const NspTypeBase *type,NspObject *A, NspObject *B, 
				 const int *ind, int nb_elts);
#else 
extern int nsp_matint_basic_copy_pointer(void);
extern int nsp_matint_basic_copy_mat(void);
extern int nsp_matint_basic_copy_int(void);
extern int nsp_matint_basic_copy(void);
#endif 

#endif 

#ifdef   Matint_Private 
static char *matint_type_as_string (void);
#endif 
