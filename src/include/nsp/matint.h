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
typedef NspObject *matint_clone(const char *name,const void *M, int m,int n); 
typedef char *matint_copy_elt(char *from);
typedef int matint_enlarge(void *M, int m, int n);


typedef struct _NspTypeMatint {
  NSP_TYPE_OBJECT__
  /* added */
  matint_redim *redim; 
  matint_resize *resize;
  matint_free_elt *free_elt;
  matint_elt_size *elt_size;
  matint_clone *clone;
  matint_copy_elt *copy_elt;
  matint_enlarge *enlarge;
} NspTypeMatint ; 

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
extern NspObject *nsp_matint_extract_elements(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax);
extern NspObject *nsp_matint_extract_elements1(NspObject *Obj,NspObject *Elts);

extern NspObject *nsp_matint_extract_columns(NspObject *Obj, const int *ind, int nb_elts, int cmin, int cmax);
extern NspObject *nsp_matint_extract_columns1(NspObject *Obj,NspObject *Cols);
extern NspObject *nsp_matint_extract_rows(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax);
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
extern NspObject *nsp_matint_concat_right(const NspObject *ObjA,const NspObject *ObjB);
extern int nsp_matint_concat_right_bis(NspObject *ObjA,const NspObject *ObjB);

typedef NspObject *(*Fconcat_d) (const NspObject *, const NspObject *);
extern NspObject *nsp_matint_concat_down(NspObject *ObjA, NspObject *ObjB);
extern int int_matint_concat_down_yy(Stack stack, int rhs, int opt, int lhs, Fconcat_d F);
extern int nsp_matint_resize2vect_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_extractelts_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_extractcols_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_extractrows_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_extract_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_setrowscols_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_deleteelts_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_deleteelts2_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_deletecols_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_deleterows_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_tozero_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_concatr_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_concatd_xx(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_cells_setrowscols_xx(Stack stack, int rhs, int opt, int lhs);
extern int int_matint_redim(Stack stack, int rhs, int opt, int lhs);
extern int nsp_matint_redim(NspObject *Obj, int m, int n);

typedef enum { matint_iwork1=0, matint_iwork2=1} matint_workid;
extern int *get_index_vector_from_object(NspObject *Obj, int *Nb_elts, int *Rmin, int *Rmax,matint_workid iwork);
extern int nsp_matint_set_submatrix1(NspObject *ObjA,NspObject *Row, NspObject *Col, NspObject *ObjB);

#endif 

#ifdef   Matint_Private 
static char *matint_type_as_string (void);
#endif 
