#ifndef NSP_INC_MATRIX 
#define NSP_INC_MATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 

/*
 * NspMatrix inherits from NspObject 
 */

/**
 * doubleC: 
 * @r: real part 
 * @i: imaginary part
 * 
 * structure used to store complex values i.e two doubles.
 */

typedef struct { double r, i; } doubleC;

/**
 * NspMatrix: 
 * @m: number of rows 
 * @n: number of columns
 * @mn: @m x @n
 * @rc_type: 'r' for real or  'c' for complex 
 * @convert: 'd','i','f','c','u' : double, int, float, old_complex, unexpanded
 *          used to remember array converted in place
 *
 * inherits from #NspObject 
 */

typedef struct _NspMatrix NspMatrix;

typedef struct _NspTypeMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeMatrix;

struct _NspMatrix {
  /*< private >*/
  NspObject father; 
  NspTypeMatrix *type; 
  /*< public >*/
  int m,n,mn;/* NspMatrix dimensions */
  union { 
    double *R;     /* Pointer on real values */
    doubleC *C;    /* Pointer on complex values */
    int *I;        /* Pointer on integer values */
    float *F;        /* Pointer on float values */
  };
  char rc_type;    /* 'r' : real or  'c' : complex  */
  char convert;    /* 'd','i','f','c','u': double, int, float, old_complex, unexpanded
		    * used to remember array converted in place 
		    */
  int impl[2];     /* start, step */
};

#include "nsp/bmatrix.h" 
#include "nsp/smatrix.h" 

extern int nsp_type_matrix_id;
extern NspTypeMatrix *nsp_type_matrix;

extern int nsp_type_matrix_init();

/* only useful when building a new class derived from matrix */

extern NspTypeMatrix *new_type_matrix(type_mode mode) ;

/* only useful when building a new class derived from matrix */

extern NspMatrix *new_matrix();

/*
 * Object methods redefined for matrix 
 */

#define NULLMAT (NspMatrix*) 0

extern double *nsp_alloc_doubles(unsigned int n);
extern double *nsp_realloc_doubles(double *dp, unsigned int n);
extern int *nsp_alloc_int(unsigned int n);
extern int *nsp_realloc_int(int *dp, unsigned int n);
extern doubleC *nsp_alloc_doubleC(unsigned int n);
extern doubleC *nsp_realloc_doubleC(doubleC *dp, unsigned int n);

extern double *nsp_alloc_work_doubles(unsigned int n);
extern int *nsp_alloc_work_int(unsigned int n);
extern doubleC *nsp_alloc_work_doubleC(unsigned int n);

/* from MatObj.c */

extern void nsp_matrix_destroy(NspMatrix *Mat); 
extern int nsp_matrix_info ( NspMatrix *Mat, int indent,const char *name, int rec_level); 
extern NspMatrix *nsp_matrix_copy(const NspMatrix *A); 
extern int nsp_matrix_print (NspMatrix *Mat, int indent,const char *name, int rec_level); 
extern int nsp_print_latex_array_double(int indent, char *name, double *val, int size, int rec_level);
extern int nsp_print_array_double(int indent, char *name, double *val, int size, int rec_level);
extern unsigned int  nsp_matrix_elt_size(NspMatrix *M);
extern NspObject *matrix_loop_extract (char *str, NspObject *O, NspObject *O1, int i, int *rep); 
extern NspMatrix  *matrix_object(NspObject *O);
extern int nsp_mat_fullcomp(NspMatrix *A, NspMatrix *B, char *op,int *err);
extern NspMatrix *matrix_object(NspObject *O); 
extern int IsMatObj (Stack stack, int i); 
extern int IsMat (const NspObject *O); 

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
extern NspMatrix *GetMtlbMatCopy (Stack stack, int i);
extern NspMatrix *GetMtlbMat (Stack stack, int i);

extern int IntScalar (NspObject *O, int *val); 
extern int GetScalarInt (Stack stack, int i, int *val); 
extern int DoubleScalar (NspObject *O, double *val); 
extern int GetScalarDouble (Stack stack, int i, double *val); 
extern int *Matd2i (NspMatrix *A, int *imin, int *imax); 
extern void Bounds (const NspMatrix *A, int *imin, int *imax); 
extern void nsp_matrix_boundster(const NspMatrix *A, int *ind, int *imin, int *imax);
extern NspMatrix *Mat2double (NspMatrix *A); 
extern NspMatrix *Mat2int (NspMatrix *A); 
extern NspMatrix *Mat2float (NspMatrix *A); 
extern NspMatrix *Mat2mtlb_cplx (NspMatrix * A);

typedef int (*F_Enlarge) (void *A,int m,int n);

extern NspMatrix *nsp_matrix_create (const char *name, char type, int m, int n); 
extern NspMatrix *nsp_matrix_clone (const char *name, NspMatrix *A, int m, int n, int init); 
extern NspMatrix *nsp_matrix_create_impl (double first, double step, double last); 
extern NspMatrix *nsp_matrix_create_int_impl(int first, int step, int last);


extern NspMatrix *nsp_matrix_create_linspace(const double first[],const double last[],int r,int n);
extern NspMatrix *nsp_matrix_create_logspace(const double first[],const double last[],int r,int n);
extern NspMatrix *nsp_matrix_create_from_doubles(const char *name,int m,int n,...);
extern NspMatrix *nsp_matrix_create_from_array(const char *name,int m,int n,const double valr[],const double valc[]);

extern int nsp_matrix_fill_with (NspMatrix *A, const NspMatrix *B); 
extern int nsp_matrix_resize (NspMatrix *A, int m, int n); 
extern int nsp_matrix_scalar_to_mn (NspMatrix *A, int m, int n); 
extern int nsp_matrix_latex_print (const NspMatrix *Mat); 
extern int nsp_matrix_latex_tab_print(const NspMatrix *Mat); 
extern int nsp_matrix_redim (NspMatrix *A, int m, int n); 
extern int nsp_matrix_enlarge (NspMatrix *A, int m, int n); 
extern int nsp_matrix_concat_right(NspMatrix *A, const NspMatrix *B); 
extern int nsp_matrix_add_columns(NspMatrix *A, int n); 
extern NspMatrix *nsp_matrix_concat_down(const NspMatrix *A,const NspMatrix *B); 
extern NspMatrix *nsp_matrix_concat_diag(const NspMatrix *A,const NspMatrix *B); 
extern int nsp_matrix_add_rows(NspMatrix *A, int m); 
extern int nsp_matrix_set_submatrix(NspMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspMatrix *B); 
extern int GenericMatSeRoBis(void *A, int Am, int An, int Amn, int nb_ind, int rmin, int rmax,
			     void *B, int Bm, int Bn, int Bmn, F_Enlarge F, int *Bscal);
extern int nsp_matrix_set_rows(NspMatrix *A, NspMatrix *Rows, NspMatrix *B); 
extern int mat_is_increasing (const NspMatrix *A); 
extern NspMatrix *nsp_matrix_extract(const NspMatrix *A,const  NspMatrix *Rows,const  NspMatrix *Cols); 
extern NspMatrix *nsp_matrix_extract_elements(const NspMatrix *A,const  NspMatrix *Elts); 
extern NspMatrix *nsp_matrix_extract_columns(const NspMatrix *A,const  NspMatrix *Cols); 
extern NspMatrix *nsp_matrix_extract_rows(const NspMatrix *A,const  NspMatrix *Rows); 
extern NspMatrix *MatLoopCol (char *str, NspMatrix *Col, NspMatrix *A, int icol, int *rep); 
extern NspMatrix *nsp_matrix_extract_diag(const NspMatrix *A, int k); 
extern int nsp_matrix_set_diag(NspMatrix *A, NspMatrix *Diag, int k); 
extern NspMatrix *nsp_matrix_create_diag(const NspMatrix *Diag, int k); 
extern NspMatrix *nsp_matrix_transpose(const NspMatrix *A); 

/* from MatOps.c */
extern int nsp_mat_scale_rows(NspMatrix *A, NspMatrix *x);
extern int nsp_mat_scale_cols(NspMatrix *A, NspMatrix *x);
extern NspMatrix *nsp_mat_diff(NspMatrix *A, int order, int dim);
extern int nsp_mat_mult_scalar_bis(NspMatrix *A, NspMatrix *B);
extern int nsp_mat_add_scalar_bis(NspMatrix *A, NspMatrix *B);
extern int nsp_mat_add_mat(NspMatrix *A, NspMatrix *B);
extern int nsp_mat_sub_scalar_bis(NspMatrix *A, NspMatrix *B);
extern int nsp_scalar_sub_mat_bis(NspMatrix *A, NspMatrix *B);
extern int nsp_mat_sub_mat(NspMatrix *A, NspMatrix *B);
extern void nsp_mat_set_rval(NspMatrix *A, double dval); 
extern NspMatrix *nsp_mat_copy_and_complexify(const NspMatrix *A);
extern int nsp_mat_set_ival(NspMatrix *A, double dval); 
extern NspMatrix *nsp_mat_mult(NspMatrix *A, NspMatrix *B, int flag);
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
extern int nsp_mat_maxitt1(NspMatrix *A, NspMatrix *B, NspMatrix *Ind, int j, int flag); 
extern int nsp_mat_minitt1(NspMatrix *A, NspMatrix *B, NspMatrix *Ind, int j, int flag); 
extern int nsp_mat_minmax(NspMatrix *A, int dim, NspMatrix **Amin, NspMatrix **Imin,
			  NspMatrix **Amax, NspMatrix **Imax, int lhs);
extern NspMatrix **nsp_mat_slec(char *file, int *Count); 
extern FILE *fopen (const char *, const char *);
extern NspMatrix *MatLec (FILE *fd); 
extern NspMatrix *fooBOU (void); 
/* extern int nsp_mat_readline(FILE *fd);  */
/* extern void nsp_testnumtokens(void);  */
/* extern int nsp_numtokens(char *string);  */
extern void nsp_csetd(const int *n,const double *z,doubleC *tab,const int *inc) ;
/* extern void nsp_ciset(const int *n,const double *z, doubleC *tab, const int *inc);*/
extern int nsp_mat_complexify(NspMatrix *Mat, double d); 
extern int nsp_mat_get_real(NspMatrix *A); 
extern int nsp_mat_get_imag(NspMatrix *A); 
extern int nsp_mat_isreal(const NspMatrix *A, int strict);

extern int nsp_mat_inv_el(NspMatrix *A); 
extern NspMatrix *nsp_mat_kron(NspMatrix *A, NspMatrix *B); 
extern NspMatrix *nsp_mat_sort(NspMatrix *A, int flag, char *str1, char *str2); 
extern NspMatrix *nsp_mat_sum(NspMatrix *A, int dim); 
extern NspMatrix *nsp_mat_prod(NspMatrix *A, int dim); 
extern NspMatrix *nsp_mat_cum_prod(NspMatrix *A,  int dim); 
extern NspMatrix *nsp_mat_cum_sum(NspMatrix *A,  int dim); 
extern NspMatrix *nsp_mat_maxi(NspMatrix *A, int dim_flag, NspMatrix **Imax, int lhs); 
extern NspMatrix *nsp_mat_mini(NspMatrix *A, int dim_flag, NspMatrix **Imax, int lhs); 
extern NspMatrix *nsp_mat_createinit(char *name, char type, int m, int n, double (*func) ()); 
extern void nsp_mat_triu(NspMatrix *A, int k); 
extern void nsp_mat_tril(NspMatrix *A, int k); 
extern NspMatrix *nsp_mat_eye(int m, int n); 
extern NspMatrix *nsp_mat_ones(int m, int n); 
extern NspMatrix *nsp_mat_zeros(int m, int n); 
extern NspMatrix *nsp_mat_rand(int m, int n); 
extern void nsp_set_urandseed(int m); 
extern int nsp_get_urandseed(void); 
extern void nsp_set_urandtype(int m); 
extern int nsp_get_urandtype(void); 
extern int nsp_mat_pow_matscalar(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_pow_matmat(NspMatrix *A, NspMatrix *B); 
extern int nsp_mat_pow_scalarmat(NspMatrix *A, NspMatrix *B); 
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
extern NspMatrix *nsp_mat_atan2(NspMatrix *A,NspMatrix *B);
extern NspMatrix *nsp_mat_angle(NspMatrix *Z);
extern int nsp_mat_atanh(NspMatrix *A); 
extern void nsp_mat_ceil(NspMatrix *A); 
extern void nsp_mat_modulo(NspMatrix *A, int n); 
extern void nsp_mat_idiv(NspMatrix *A, int n); 
extern void nsp_mat_mod(NspMatrix *x, NspMatrix *y);
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

extern int nsp_mat_polar(NspMatrix *A, NspMatrix *B); 
extern NspMatrix *nsp_mat_complex(NspMatrix *A, NspMatrix *B);
extern int nsp_mat_nearfloat(int dir, NspMatrix *x);
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
extern NspMatrix *nsp_mat_magic(int n); 
extern NspMatrix *nsp_mat_franck(int n, int job); 
extern NspMatrix *nsp_mat_hilbert(int n,int job); 
extern int nsp_mat_fullcomp(NspMatrix *A, NspMatrix *B, char *op, int *err); 
extern int nsp_mat_find(NspMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
extern int nsp_mat_mfind(const NspMatrix *x, int m,const char **ops,const double *scalars, NspMatrix **Ind);
extern int nsp_mat_ndind2ind(int *dims, int nd, NspMatrix **ndind, NspMatrix **Ind);
extern int nsp_mat_sub2ind(int *dims, int nd, NspMatrix **ndind, int nb_ind, NspMatrix **Ind);
extern int nsp_mat_nnz(NspMatrix *A);
extern int nsp_mat_unique(NspMatrix *x, NspMatrix **Ind, NspMatrix **Occ, Boolean first_ind);
extern NspMatrix *nsp_mat_dot(NspMatrix *A, NspMatrix *B, int dim_flag);
extern NspMatrix *nsp_mat_cross(NspMatrix *X, NspMatrix *Y, int dim);
extern NspBMatrix *nsp_mat_issorted(NspMatrix *A, int dim_flag, Boolean strict_order);
extern NspBMatrix *nsp_mat_has(NspMatrix *A, NspMatrix *x, int lhs, NspMatrix **ind, NspMatrix **ind2);

/* maxplus operations */
extern int nsp_mat_maxplus_add(NspMatrix *A, NspMatrix *B) ;
extern NspMatrix *nsp_mat_maxplus_mult(NspMatrix *A, NspMatrix *B);
extern NspMatrix *nsp_mat_minplus_mult(NspMatrix *A, NspMatrix *B);

extern int nsp_array_maxi(int n,const double *A, int incr, double *amax);
extern int nsp_array_mini(int n,const double *A, int incr, double *amax);

/* cache */
extern void nsp_free_index_vector_cache(index_vector *index);
extern int nsp_get_index_vector_cache(index_vector *index);
extern int nsp_get_index_vector_from_object(NspObject *Obj,index_vector *index) ;
extern int nsp_get_index_vector(Stack stack, int ipos,NspObject **Obj,index_vector *index);

/*
 * inlined functions 
 */

#ifndef HAVE_INLINE 
extern NspMatrix *GetMatCopy (Stack stack, int i); 
extern NspMatrix *GetMat (Stack stack, int i); 
#else 
static inline NspMatrix *GetMatCopy(Stack stack, int i)
{
  NspMatrix *M= Mat2double(matrix_object(stack.val->S[stack.first+i-1]));
  if ( M== NULLMAT) { ArgMessage (stack, i);return M;}
  /**/ return (NspMatrix *)  MaybeObjCopy (&stack.val->S[stack.first+i-1]);
}

static inline NspMatrix *GetMat(Stack stack, int i)
{
  NspMatrix *M= Mat2double(matrix_object(stack.val->S[stack.first+i-1]));
  if ( M== NULLMAT) ArgMessage (stack, i);
  return M;
}
#endif 


#endif 


#ifdef Matrix_Private
static int init_matrix(NspMatrix *ob,NspTypeMatrix *type);
static int init_matrix(NspMatrix *o,NspTypeMatrix *type);
static int matrix_size(NspMatrix *Mat, int flag);
static char *matrix_type_as_string(void);
static char *matrix_type_short_string(NspObject *v);
static int matrix_neq(NspObject *A,NspObject *B);
static int matrix_eq(NspObject *A,NspObject *B);
static int matrix_xdr_save(XDR *xdrs, NspMatrix *M);
static NspMatrix  *matrix_xdr_load(XDR  *F);
static AttrTab matrix_attrs[];
static NspMethods *matrix_get_methods(void); 
/*static NspObject *matrix_path_extract(NspMatrix *A, NspObject *O); */
static int matrix_is_true(NspMatrix *M);
static int nsp_matrix_as_index(NspMatrix * M, index_vector *index);

#endif 
