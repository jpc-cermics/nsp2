#ifndef NSP_INC_RMATRIX 
#define NSP_INC_RMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include <nsp/objectf.h>

/*
 * NspRMatrix inherits from NspObject 
 */

/* typedef struct _NspPmatrix  NspRMatrix; */

typedef struct _NspTypeRMatrix NspTypeRMatrix;


struct _NspTypeRMatrix { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} ;

typedef struct _nsp_rational *nsp_rational;

struct _nsp_rational {
  NspMatrix *num;
  NspMatrix *den;
};
  
struct _NspRmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeRMatrix *type; 
  /*< public >*/
  int m,n,mn;       /* matrix dimension (m,n,m*n) */
  nsp_rational *S;  /* Each polynom is a Matrix **/
  char rc_type  ;   /* type 'r' or 'i' */
  char *var  ;      /* name of rational variable */
  char dom;         /* domain: 'c','d','s','u' */
  double dt;        /* sample */
};

typedef struct  _NspRmatrix NspRMatrix;

extern int nsp_type_rmatrix_id;
extern NspTypeRMatrix *nsp_type_rmatrix;

int nsp_type_rmatrix_init();

/* only useful when building a new class derived from rmatrix */

NspTypeRMatrix *new_type_rmatrix(type_mode mode);

NspRMatrix *new_rmatrix();

/*
 * Object methods redefined for rmatrix 
 */

#define NULLRMAT (NspRMatrix *) 0

extern NspRMatrix *nsp_rmatrix_object(NspObject *O); 
extern NspMatrix *nsp_rmatrix_length(NspRMatrix *A); 
extern NspRMatrix *nsp_matrix_to_rmatrix(NspMatrix *A);
extern NspRMatrix *nsp_matrix_to_rmatrix_with_varname(NspMatrix *A,const char *varname);

extern NspRMatrix *nsp_matrices_to_rmatrix(NspMatrix *A,NspMatrix *B);
extern NspRMatrix *nsp_matrices_to_rmatrix_with_varname(NspMatrix *A,NspMatrix *B,const char *varname);
extern NspRMatrix *nsp_rmatrix_concat_down(const NspRMatrix *A,const NspRMatrix *B); 
extern NspRMatrix *nsp_rmatrix_copy(NspRMatrix *A); 
extern NspRMatrix *nsp_rmatrix_create(const char *name, int m, int n,const doubleC *cval, int flag,
				      const char *var_name, char dom, double sample);
extern NspRMatrix *nsp_rmatrix_create_m(char *name, int m, int n,NspMatrix *Val, const char *var_name);
extern NspRMatrix *nsp_rmatrix_clone(char *name, NspRMatrix *A, int m, int n, int init);
extern NspRMatrix *nsp_rmatrix_extract(NspRMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspRMatrix *nsp_rmatrix_extract_columns(NspRMatrix *A, NspMatrix *Cols, int *err); 
extern NspRMatrix *nsp_rmatrix_extract_elements(NspRMatrix *A, NspMatrix *Elts, int *err); 
extern NspRMatrix *nsp_rmatrix_extract_rows(NspRMatrix *A, NspMatrix *Rows, int *err); 
extern NspRMatrix *nsp_rmatrix_transpose(const NspRMatrix *A); 
extern int nsp_rmatrix_add_columns(NspRMatrix *A, int n); 
extern int nsp_rmatrix_add_rows(NspRMatrix *A, int m); 
extern int nsp_rmatrix_concat_right(NspRMatrix *A,const NspRMatrix *B); 
extern int nsp_rmatrix_enlarge(NspRMatrix *A, int m, int n); 
extern int nsp_rmatrix_redim(NspRMatrix *A, int m, int n); 
extern int nsp_rmatrix_resize(NspRMatrix *A, int m, int n); 
extern int nsp_rmatrix_set_rows(NspRMatrix *A, NspMatrix *Rows, NspRMatrix *B); 
extern int nsp_rmatrix_set_submatrix(NspRMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspRMatrix *B); 
extern int nsp_rmatrix_setrc(NspRMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspRMatrix *B); 
extern unsigned int  nsp_rmatrix_elt_size(NspRMatrix *M);
extern void nsp_rmatrix_destroy(NspRMatrix *A); 
extern int nsp_rmatrix_info(NspRMatrix *Mat, int indent,const char *name, int rec_level); 
extern int nsp_rmatrix_print(NspRMatrix *Mat, int indent,const char *name, int rec_level); 
extern void nsp_polynom_destroy(nsp_polynom *P); 
extern NspRMatrix *nsp_rmatrix_add(NspRMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_minus(NspRMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_mult_m_p(NspMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_mult_p_m(NspRMatrix *A, NspMatrix *B);
extern NspRMatrix *nsp_rmatrix_mult_tt(NspRMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_mult_tt_p_m(NspRMatrix *A, NspMatrix *B);
extern NspRMatrix *nsp_rmatrix_mult_tt_m_p(NspMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_div_tt_p_m(NspRMatrix *A, NspMatrix *B,int flag);
extern NspRMatrix *nsp_rmatrix_div_tt_m_p(NspMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_mult_p_p(NspRMatrix *A, NspRMatrix *B);
extern NspRMatrix *nsp_rmatrix_dh_p_m(const NspRMatrix *P,const NspMatrix *M) ;
extern NspRMatrix *nsp_rmatrix_hat_p_m(NspRMatrix *P,int n);
extern NspRMatrix * nsp_rmatrix_minus_m(NspRMatrix *A,NspMatrix *B, int flag);
extern NspRMatrix * nsp_rmatrix_add_m(NspRMatrix *A,NspMatrix *B);

extern int IsRMatObj (Stack stack, int i); 
extern int IsRMat (NspObject *O); 
extern NspRMatrix *GetRMatCopy (Stack stack, int i); 
extern NspRMatrix *GetRMat (Stack stack, int i); 

extern int RMatFullComp (NspRMatrix *A, NspRMatrix *B, char *op, int *err); 
extern NspMatrix *nsp_matrix_companion(NspMatrix *A);
extern NspCells *nsp_rmatrix_to_cells(const char *name, NspRMatrix *M);
extern NspMatrix *nsp_rmatrix_horner(NspRMatrix *P,NspMatrix *V,int k);
extern NspMatrix *nsp_rmatrix_horner_tt(NspRMatrix *P,NspMatrix *V);
extern NspBMatrix  *nsp_rmatrix_comp(NspRMatrix *A, NspRMatrix *B,const char *op);
extern int nsp_rmatrix_pdiv_tt(NspRMatrix *A, NspRMatrix *B, NspRMatrix **Q, NspRMatrix **R);
extern NspRMatrix  *nsp_rmatrix_extract_diag(NspRMatrix *A, int k);
extern int nsp_rmatrix_set_diag(NspRMatrix *A, NspRMatrix *Diag, int k);
extern NspRMatrix  *nsp_rmatrix_create_diag(NspRMatrix *Diag, int k);
extern NspRMatrix *nsp_cells_to_rmatrix(const char *name, NspCells *C1,  NspCells *C2);
extern int nsp_rmatrix_set_varname(NspRMatrix *p, const char *varname);
extern int nsp_rmatrix_same_varname(const NspRMatrix *P1,const NspRMatrix *P2);
extern NspRMatrix *nsp_rmatrix_sum(NspRMatrix *A, int dim);
extern NspRMatrix *nsp_rmatrix_prod(NspRMatrix *A, int dim);
extern int nsp_rmatrix_triu(NspRMatrix *A, int k);
extern int nsp_rmatrix_tril(NspRMatrix *A, int k);

extern int nsp_rmatrix_latex_print (NspRMatrix *Mat, int indent,const char *name, int rec_level);

extern NspRMatrix *nsp_matrix_to_rational(NspMatrix *M);
extern NspRMatrix *nsp_matrices_to_rational(NspMatrix *A,NspMatrix *B);
extern NspRMatrix *nsp_pmatrix_to_rmatrix(NspPMatrix *A);
extern NspRMatrix *nsp_pmatrices_to_rmatrix(NspPMatrix *A,NspPMatrix *B,int simp);
  
extern int nsp_rational_resize(nsp_rational poly);
extern nsp_rational nsp_rational_add(nsp_rational P,nsp_rational Q);
extern nsp_rational nsp_rational_minus(nsp_rational P,nsp_rational Q);
extern nsp_rational nsp_rational_mult(nsp_rational a,nsp_rational b);
extern nsp_rational nsp_rational_mult_std(nsp_rational a,nsp_rational b);
extern nsp_rational nsp_rational_mult_fft(nsp_rational a,nsp_rational b);
extern int nsp_rational_pdiv(nsp_rational a,nsp_rational b,nsp_rational *hq, nsp_rational *hr);

extern int nsp_rational_add_in_place(nsp_rational P,nsp_rational Q);
extern nsp_rational nsp_rational_zero_create(int degree, char rc_type);
extern int nsp_pset_rational(int n, const doubleC *s1, nsp_rational *s2); 
extern nsp_rational nsp_basic_to_rational(const doubleC *d, char type); 
extern nsp_rational nsp_rational_copy(nsp_rational P); 
extern nsp_rational nsp_rational_copy_with_name(nsp_rational P); 
extern nsp_rational nsp_rational_power(nsp_rational p,int n);
extern nsp_rational nsp_rational_minus_m(nsp_rational p, void *v, char type);
extern nsp_rational nsp_rational_add_m(nsp_rational p, void *v, char type);
extern NspMatrix *nsp_rational_horner(nsp_rational P,NspMatrix *b);
extern NspMatrix *nsp_rational_hornerm(nsp_rational P,NspMatrix *b);
extern NspMatrix *nsp_rational_roots(nsp_rational poly);
extern nsp_rational nsp_rational_mult_m(nsp_rational p, void *v, char type);
extern nsp_rational nsp_rational_div_m(nsp_rational p, void *v, char type);
extern nsp_rational nsp_rational_copy(nsp_rational P);
extern nsp_rational nsp_rational_copy_with_name(nsp_rational P);
extern nsp_rational nsp_rational_copy_and_name(const char *name, nsp_rational P);
extern nsp_rational nsp_basic_to_rational(const doubleC *d, char type);
extern void nsp_rational_destroy(nsp_rational *P); 
extern int nsp_polynoms_simp(NspMatrix *P,NspMatrix *Q);


#endif 

#ifdef RMatrix_Private 
static int init_rmatrix(NspRMatrix *ob,NspTypeRMatrix *type);
int nsp_rmatrix_size(NspRMatrix *Mat, int flag);
char *nsp_rmatrix_type_as_string(void);
char *nsp_rmatrix_type_short_string(NspObject *v);
NspObject *nsp_rmatrix_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_rmatrix_eq(NspObject *A,NspObject *B);
int nsp_rmatrix_neq(NspObject *A,NspObject *B);
static int nsp_rmatrix_xdr_save(XDR  *F, NspRMatrix *M);
static NspRMatrix *nsp_rmatrix_xdr_load(XDR  *F);
static AttrTab rmatrix_attrs[] ;
static NspMethods *rmatrix_get_methods(void);

#endif 
