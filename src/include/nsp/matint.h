#ifndef NSP_INC_Matint
#define NSP_INC_Matint

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* interface Matint */

#include <nsp/objectf.h>

typedef int matint_redim(NspObject *M,int m,int n); 
typedef int matint_resize(void *M,int m,int n); 
typedef int matint_free_elt(void **elt);
typedef unsigned int matint_elt_size(const void *M);
typedef NspObject *matint_clone(const char *name,const void *M, int m,int n, int init); 
typedef char *matint_copy_elt(char *from);
typedef int matint_enlarge(void *M, int m, int n);
typedef NspObject * matint_canonic(NspObject *obj);


/* nsp_matint_basic_copy_pointer: contain an array of pointers 
 * nsp_matint_basic_copy_mat: behaves like NspMatrix 
 * nsp_matint_basic_copy_int: behaves like NspBMatrix 
 * nsp_matint_basic_copy: contains an array of numeric data  
 */

typedef enum {
  nsp_matint_basic_copy_pointer, 
  nsp_matint_basic_copy_mat,     
  nsp_matint_basic_copy_int,     
  nsp_matint_basic_copy          
} matint_copy_style;

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
  matint_copy_style copy_ind;
};

#define MAT_INT(t) ((NspTypeMatint *) t)

extern int nsp_type_matint_id;
extern NspTypeMatint *nsp_type_matint;

NspTypeMatint *new_type_matint(type_mode mode);
extern NspMethods *matint_get_methods(void);

extern int nsp_matint_tozero(NspObject *Obj);
extern int nsp_matint_delete_columns_from_index(NspObject  *Obj,index_vector *index);
extern int nsp_matint_delete_rows_from_index(NspObject *Obj, index_vector *index);
extern int nsp_matint_delete_elements_from_index(NspObject *Obj, index_vector *index);
extern int nsp_matint_delete_elements2(NspObject *Obj, 
				       int *indrow, int nr, int rmin, int rmax,
				       int *indcol, int nc, int cmin, int cmax);
/* extern NspObject *nsp_matint_extract_elements(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax); */
extern NspObject *nsp_matint_extract_elements1(NspObject *Obj,NspObject *Elts);

/* extern NspObject *nsp_matint_extract_columns(NspObject *Obj, const int *ind, int nb_elts, int cmin, int cmax); */
extern NspObject *nsp_matint_extract_columns1(NspObject *Obj,NspObject *Cols);
/* extern NspObject *nsp_matint_extract_rows(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax);*/
extern NspObject *nsp_matint_extract_rows1(NspObject *Obj,NspObject *Rows);

/* extern NspObject *nsp_matint_extract(NspObject *Obj, 
				     const int *row, int nr, int rmin, int rmax, 
				     const int *col, int nc, int cmin, int cmax); */

extern NspObject *nsp_matint_extract1(NspObject *Obj,NspObject *Rows, NspObject *Cols);
extern int nsp_matint_set_submatrix(NspObject *ObjA, index_vector *index_r, index_vector *index_c, NspObject *ObjB);
extern int nsp_matint_set_elts(NspObject *ObjA, index_vector *index,  NspObject *ObjB);
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

extern int get_index_vector_from_object(NspObject *Obj, index_vector *index) ;
extern int nsp_matint_set_submatrix1(NspObject *ObjA,NspObject *Row, NspObject *Col, NspObject *ObjB);
extern int nsp_matint_perm_elem(NspObject *ObjA, int p, int q, int dim_flag);

extern int nsp_matint_delete_elements( NspObject *Obj, NspObject *Index) ;
extern int nsp_matint_delete_columns( NspObject *Obj, NspObject *Index) ;
extern int nsp_matint_delete_rows( NspObject *Obj, NspObject *Index) ;

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



#endif 

#ifdef   Matint_Private 
static char *matint_type_as_string (void);
#endif 
