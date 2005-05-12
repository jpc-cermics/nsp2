#ifndef INC_NSP_MATRIX 
#define INC_NSP_MATRIX

/*
 * This Software is (Copyright ENPC 1998-2004) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 

/*
 * NspMatrix inherits from NspObject 
 */

typedef struct { double r, i; } doubleC;

typedef struct _nsp_matrix NspMatrix;

typedef int (*matrix_save) (NspFile  *F, NspMatrix *M);

typedef struct _nsp_type_Matrix { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  matrix_save * save; /* FIXME: unused since it was move in NSP_TYPE_OBJECT */
} NspTypeMatrix;

struct _nsp_matrix {
  NspObject father; 
  NspTypeMatrix *type; 
  integer m,n,mn;/* NspMatrix dimensions */
  char rc_type;     /* 'r' : real or  'i' : complex  */
  char convert;  /* 'd','i','f' : double, integer, float
		    used for array data conversion */
  double *R;     /* Pointer on real values */
  doubleC *C;    /* Pointer on complex values */
};

#include "nsp/bmatrix.h" 
#include "nsp/smatrix.h" 

extern int nsp_type_matrix_id;
extern NspTypeMatrix *nsp_type_matrix;

int nsp_type_matrix_init();

/* only useful when building a new class derived from matrix */

NspTypeMatrix *new_type_matrix(type_mode mode) ;

/* only useful when building a new class derived from matrix */

NspMatrix *new_matrix();

/*
 * Object methods redefined for matrix 
 */

#ifdef Matrix_Private
static int init_matrix(NspMatrix *ob,NspTypeMatrix *type);
static int init_matrix(NspMatrix *o,NspTypeMatrix *type);
static int matrix_size(NspMatrix *Mat, int flag);
static char *matrix_type_as_string(void);
static char *matrix_type_short_string(void);
static int matrix_neq(NspObject *A,NspObject *B);
static int matrix_eq(NspObject *A,NspObject *B);
static int matrix_xdr_save(NspFile  *F, NspMatrix *M);
static NspMatrix  *matrix_xdr_load(NspFile  *F);
static AttrTab matrix_attrs[];
static NspMethods *matrix_get_methods(void); 
/*static NspObject *matrix_path_extract(NspMatrix *A, NspObject *O); */
static int matrix_is_true(NspMatrix *M);
#endif 

#ifdef OCAML 
NspMatrix *MatCreateFromData  (char *name, char type, integer m, 
			       integer n,struct caml_bigarray *b);
#endif 

#define NULLMAT (NspMatrix*) 0

extern double *nsp_alloc_doubles(unsigned int n);
extern double *nsp_realloc_doubles(double *dp, unsigned int n);
extern int *nsp_alloc_int(unsigned int n);
extern int *nsp_realloc_int(int *dp, unsigned int n);
extern doubleC *nsp_alloc_doubleC(unsigned int n);
extern doubleC *nsp_realloc_doubleC(doubleC *dp, unsigned int n);

/* from MatObj.c */

extern void nsp_matrix_destroy(NspMatrix *Mat); 
extern void nsp_matrix_info (const NspMatrix *Mat, int indent); 
extern NspMatrix *nsp_matrix_copy(const NspMatrix *A); 
extern void nsp_matrix_print (NspMatrix *Mat, int indent,int header); 

extern void matrix_destroy (NspMatrix *Mat); 
extern void matrix_info (const NspMatrix *Mat, int indent); 
extern void matrix_print (NspMatrix *Mat, int indent); 
extern NspObject *matrix_loop_extract (char *str, NspObject *O, NspObject *O1, int i, int *rep); 
extern NspMatrix *matrix_copy (const NspMatrix *A); 
extern NspMatrix  *matrix_object(NspObject *O);
extern int nsp_mat_fullcomp();
extern NspMatrix *matrix_object(NspObject *O); 
extern int IsMatObj (Stack stack, int i); 
extern int IsMat (NspObject *O); 
extern NspMatrix *GetMatCopy (Stack stack, int i); 
extern NspMatrix *GetMat (Stack stack, int i); 
extern NspMatrix *GetMatCopyInt (Stack stack, int i); 
extern NspMatrix *GetMatInt (Stack stack, int i); 
extern NspMatrix *GetMatCopyFloat (Stack stack, int i); 
extern NspMatrix *GetMatFloat (Stack stack, int i); 
extern NspMatrix *GetRealMatCopy_G (Stack stack, int i); 
extern NspMatrix *GetRealMat_G (Stack stack, int i); 
extern NspMatrix *GetRealMatCopy (Stack stack, int i); 
extern NspMatrix *GetRealMat (Stack stack, int i); 
extern NspMatrix *GetRealMatCopyInt (Stack stack, int i); 
extern NspMatrix *GetRealMatInt (Stack stack, int i); 
extern NspMatrix *GetRealMatCopyFloat (Stack stack, int i); 
extern NspMatrix *GetRealMatFloat (Stack stack, int i); 
extern int IntScalar (NspObject *O, integer *val); 
extern int GetScalarInt (Stack stack, int i, integer *val); 
extern int DoubleScalar (NspObject *O, double *val); 
extern int GetScalarDouble (Stack stack, int i, double *val); 
extern int *Matd2i (NspMatrix *A, integer *imin, integer *imax); 
extern void Bounds (const NspMatrix *A, integer *imin, integer *imax); 
extern int *nsp_complement_for_deletions(int mn, const NspMatrix *Elts, int *Count);
extern int *nsp_indices_for_deletions(int mn, const NspMatrix *Elts, int *Count);
extern NspMatrix *Mat2double (NspMatrix *A); 
extern NspMatrix *Mat2int (NspMatrix *A); 
extern NspMatrix *Mat2float (NspMatrix *A); 

typedef int (*F_Enlarge) (void *A,int m,int n);

extern NspMatrix *nsp_matrix_create (const char *name, char type, integer m, integer n); 
extern NspMatrix *nsp_matrix_create_impl (double first, double step, double last); 
extern NspMatrix *nsp_matrix_create_linspace(const double first[],const double last[],int r,int n);
extern NspMatrix *nsp_matrix_create_logspace(const double first[],const double last[],int r,int n);
extern NspMatrix *nsp_matrix_create_from_doubles(const char *name,integer m,integer n,...);
extern NspMatrix *nsp_matrix_create_from_array(const char *name,int m,int n,const double valr[],const double valc[]);

extern int nsp_matrix_fill_with (NspMatrix *A, const NspMatrix *B); 
extern int nsp_matrix_resize (NspMatrix *A, integer m, integer n); 
extern int nsp_matrix_scalar_to_mn (NspMatrix *A, integer m, integer n); 
extern void nsp_matrix_latex_print (const NspMatrix *Mat); 
extern void nsp_matrix_latex_tab_print(const NspMatrix *Mat); 
extern int nsp_matrix_redim (NspMatrix *A, integer m, integer n); 
extern int nsp_matrix_enlarge (NspMatrix *A, integer m, integer n); 
extern int nsp_matrix_concat_right(NspMatrix *A, const NspMatrix *B); 
extern int nsp_matrix_add_columns(NspMatrix *A, integer n); 
extern NspMatrix *nsp_matrix_concat_down(const NspMatrix *A,const NspMatrix *B); 
extern NspMatrix *nsp_matrix_concat_diag(const NspMatrix *A,const NspMatrix *B); 
extern int nsp_matrix_add_rows(NspMatrix *A, integer m); 
extern int nsp_matrix_set_submatrix(NspMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspMatrix *B); 
extern int GenericMatSeRo (void *A, int Am, int An, int Amn, NspMatrix *Rows, void *B, int Bm, int Bn, int Bmn, F_Enlarge F, int *Bscal); 
extern int nsp_matrix_set_rows(NspMatrix *A, NspMatrix *Rows, NspMatrix *B); 
extern int mat_is_increasing (const NspMatrix *A); 
extern int nsp_matrix_delete_columns(NspMatrix *A, NspMatrix *Cols); 
extern int nsp_matrix_delete_rows(NspMatrix *A, NspMatrix *Rows); 
extern int nsp_matrix_delete_elements(NspMatrix *A, NspMatrix *Elts); 
extern NspMatrix *nsp_matrix_extract(const NspMatrix *A,const  NspMatrix *Rows,const  NspMatrix *Cols); 
extern NspMatrix *nsp_matrix_extract_elements(const NspMatrix *A,const  NspMatrix *Elts); 
extern NspMatrix *nsp_matrix_extract_columns(const NspMatrix *A,const  NspMatrix *Cols); 
extern NspMatrix *nsp_matrix_extract_rows(const NspMatrix *A,const  NspMatrix *Rows); 
extern NspMatrix *MatLoopCol (char *str, NspMatrix *Col, NspMatrix *A, int icol, int *rep); 
extern NspMatrix *nsp_matrix_extract_diag(const NspMatrix *A, integer k); 
extern int nsp_matrix_set_diag(NspMatrix *A, NspMatrix *Diag, integer k); 
extern NspMatrix *nsp_matrix_create_diag(const NspMatrix *Diag, integer k); 
extern NspMatrix *nsp_matrix_transpose(const NspMatrix *A); 

/* from MatOps.c */

extern void nsp_mat_set_rval(NspMatrix *A, double dval); 
extern int nsp_mat_set_ival(NspMatrix *A, double dval); 
extern NspMatrix *nsp_mat_mult(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_add(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_dadd(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_dadd_maxplus(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_add_scalar(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_add_scalar_maxplus(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_sub(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_dsub(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_sub_scalar(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_sub_scalar_maxplus(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_subs_calarm(NspMatrix *Mat1, NspMatrix *Mat2); 
extern void nsp_mat_clean(NspMatrix *A, int rhs, double epsa, double epsr); 
extern int nsp_mat_maxitt1(NspMatrix *A, NspMatrix *B, NspMatrix *Ind, integer j, integer flag); 
extern int nsp_mat_minitt1(NspMatrix *A, NspMatrix *B, NspMatrix *Ind, integer j, integer flag); 
extern NspMatrix **nsp_mat_slec(char *file, int *Count); 
extern FILE *fopen (const char *, const char *);
extern NspMatrix *MatLec (FILE *fd); 
extern NspMatrix *fooBOU (void); 
extern int nsp_mat_readline(FILE *fd); 
extern void nsp_testnumtokens(void); 
extern int nsp_numtokens(char *string); 

extern void nsp_csetd(const int *n,const double *z,doubleC *tab,const int *inc) ;
extern void nsp_ciset(const integer *n,const double *z, doubleC *tab, const integer *inc);
extern int nsp_mat_complexify(NspMatrix *Mat, double d); 
extern int nsp_mat_get_real(NspMatrix *A); 
extern int nsp_mat_get_imag(NspMatrix *A); 
extern int nsp_mat_inv_el(NspMatrix *A); 
extern NspMatrix *nsp_mat_kron(NspMatrix *A, NspMatrix *B); 
extern NspMatrix *nsp_mat_sort(NspMatrix *A, int flag, char *str1, char *str2); 
extern NspMatrix *nsp_mat_sum(NspMatrix *A, char *flag); 
extern NspMatrix *nsp_mat_prod(NspMatrix *A, char *flag); 
extern NspMatrix *nsp_mat_cum_prod(NspMatrix *A, char *flag); 
extern NspMatrix *nsp_mat_cum_sum(NspMatrix *A, char *flag); 
extern NspMatrix *nsp_mat_maxi(NspMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
extern NspMatrix *nsp_mat_mini(NspMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
extern NspMatrix *nsp_mat_createinit(char *name, char type, integer m, integer n, double (*func) ()); 
extern void nsp_mat_triu(NspMatrix *A, integer k); 
extern void nsp_mat_tril(NspMatrix *A, integer k); 
extern NspMatrix *nsp_mat_eye(integer m, integer n); 
extern NspMatrix *nsp_mat_ones(integer m, integer n); 
extern NspMatrix *nsp_mat_zeros(integer m, integer n); 
extern NspMatrix *nsp_mat_rand(integer m, integer n); 
extern void nsp_set_urandseed(int m); 
extern int nsp_get_urandseed(void); 
extern void nsp_set_urandtype(int m); 
extern int nsp_get_urandtype(void); 
extern int nsp_mat_pow_tt(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_pow_el(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_pow_scalar(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_pow_scalarm(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_div_tt(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_div_el(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_div_scalar(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_bdiv_tt(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_bdiv_el(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_bdiv_scalar(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_mult_tt(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_mult_el(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_mult_scalar(NspMatrix *Mat1, NspMatrix *Mat2); 
extern int nsp_mat_acos(NspMatrix *A); 
extern int nsp_mat_acosh(NspMatrix *A); 
extern int nsp_mat_asin(NspMatrix *A); 
extern int nsp_mat_asinh(NspMatrix *A); 
extern int nsp_mat_atan(NspMatrix *A); 
extern int nsp_mat_atan2(NspMatrix *A,NspMatrix *B); 
extern int nsp_mat_atanh(NspMatrix *A); 
extern void nsp_mat_ceil(NspMatrix *A); 
extern void nsp_mat_modulo(NspMatrix *A, int n); 
extern void nsp_mat_idiv(NspMatrix *A, int n); 
extern void nsp_mat_int(NspMatrix *A); 
extern void nsp_mat_floor(NspMatrix *A); 
extern void nsp_mat_round(NspMatrix *A); 
extern int nsp_mat_sign(NspMatrix *A); 
extern int nsp_mat_tan(NspMatrix *A); 
extern int nsp_mat_tanh(NspMatrix *A); 
extern int nsp_mat_abs(NspMatrix *A); 
extern int nsp_mat_erf(NspMatrix *A); 
extern int nsp_mat_erfc(NspMatrix *A); 
extern int nsp_mat_lgamma(NspMatrix *A); 
extern int nsp_mat_tgamma(NspMatrix *A); 

extern int nsp_mat_arg(NspMatrix *A); 
extern int nsp_mat_polar(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_iand(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_iandu(NspMatrix *A, unsigned int *res); 
extern int nsp_mat_ior(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_ioru(NspMatrix *A, unsigned int *res); 
extern int nsp_mat_ishift(NspMatrix *A,int shift,char dir);
extern void nsp_mat_conj(NspMatrix *A); 
extern void nsp_mat_cos(NspMatrix *A); 
extern void nsp_mat_cosh(NspMatrix *A); 
extern void nsp_mat_expel(NspMatrix *A); 
extern int nsp_mat_logel(NspMatrix *A); 
extern void nsp_mat_sin(NspMatrix *A); 
extern void nsp_mat_sinh(NspMatrix *A); 
extern int nsp_mat_sqrtel(NspMatrix *A); 
extern int nsp_mat_minus(NspMatrix *A); 
extern int nsp_mat_minus_maxplus(NspMatrix *A); 
extern NspMatrix *nsp_mat_magic(integer n); 
extern NspMatrix *nsp_mat_franck(integer n, int job); 
extern NspMatrix *nsp_mat_hilbert(integer n,int job); 
extern int nsp_mat_fullcomp(NspMatrix *A, NspMatrix *B, char *op, int *err); 
extern int nsp_mat_find(NspMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
/* maxplus operations */
extern int nsp_mat_maxplus_add(NspMatrix *A, NspMatrix *B) ;
extern NspMatrix *nsp_mat_maxplus_mult(NspMatrix *A, NspMatrix *B);
extern NspMatrix *nsp_mat_minplus_mult(NspMatrix *A, NspMatrix *B);

/* from lapack */

extern NspMatrix *nsp_mat_bdiv(NspMatrix *A, NspMatrix *B);


#endif 

