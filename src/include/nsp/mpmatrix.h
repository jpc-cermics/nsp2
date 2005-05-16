#ifndef NSP_INC_MPMATRIX 
#define NSP_INC_MPMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 

/*
 * NspMaxpMatrix inherits from NspObject 
 */

/* typedef struct { double r, i; } doubleC; */

typedef struct _NspMpmatrix NspMaxpMatrix;

typedef struct _NspTypeMaxpMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeMaxpMatrix;

struct _NspMpmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeMaxpMatrix *type; 
  /*< public >*/
  int m,n,mn;/* NspMaxpMatrix dimensions */
  union { 
    double *R;     /* Pointer on real values */
    doubleC *C;    /* Pointer on complex values */
    doubleC *I;    /* Pointer on integer values */
  };
  char rc_type;     /* 'r' : real or  'i' : complex  */
  char convert;  /* 'd','i','f' : double, int, float
		    used for array data conversion */
};

#include "nsp/matrix.h" 
#include "nsp/bmatrix.h" 
#include "nsp/smatrix.h" 

extern int nsp_type_mpmatrix_id;
extern NspTypeMaxpMatrix *nsp_type_mpmatrix;

int nsp_type_mpmatrix_init();

/* only useful when building a new class derived from matrix */

NspTypeMaxpMatrix *new_type_mpmatrix(type_mode mode) ;

/* only useful when building a new class derived from matrix */

NspMaxpMatrix *new_mpmatrix();

/*
 * Object methods redefined for matrix 
 */

#ifdef MaxpMatrix_Private
static int init_mpmatrix(NspMaxpMatrix *ob,NspTypeMaxpMatrix *type);
static int mpmatrix_size(NspMaxpMatrix *Mat, int flag);
static char *mpmatrix_type_as_string(void);
static char *mpmatrix_type_short_string(void);
static int mpmatrix_neq(NspObject *A,NspObject *B);
static int mpmatrix_eq(NspObject *A,NspObject *B);
static int mpmatrix_xdr_save(NspFile  *F, NspMaxpMatrix *M);
static NspMaxpMatrix  *mpmatrix_xdr_load(NspFile  *F);
static AttrTab mpmatrix_attrs[];
static NspMethods *mpmatrix_get_methods(void); 
/*static NspObject *mpmatrix_path_extract(NspMaxpMatrix *A, NspObject *O); */
static int mpmatrix_is_true(NspMaxpMatrix *M);
#endif 

#define NULLMAXPMAT (NspMaxpMatrix*) 0

/* from MatObj.c */

extern void nsp_mpmatrix_destroy(NspMaxpMatrix *Mat); 
extern void nsp_mpmatrix_info (const NspMaxpMatrix *Mat, int indent); 
extern NspMaxpMatrix *nsp_mpmatrix_copy(const NspMaxpMatrix *A); 
extern void nsp_mpmatrix_print (NspMaxpMatrix *Mat, int indent,int header); 

extern void mpmatrix_destroy (NspMaxpMatrix *Mat); 
extern void mpmatrix_info (const NspMaxpMatrix *Mat, int indent); 
extern void mpmatrix_print (NspMaxpMatrix *Mat, int indent); 
extern NspObject * mpmatrix_loop_extract (char *str, NspObject *O, NspObject *O1, int i, int *rep); 
extern NspMaxpMatrix *mpmatrix_copy (const NspMaxpMatrix *A); 
extern NspMaxpMatrix  *mpmatrix_object(NspObject *O);

extern int nsp_mpmat_fullcomp();
extern NspMaxpMatrix *mpmatrix_object(NspObject *O); 
extern int IsMpMatObj (Stack stack, int i); 
extern int IsMpMat (NspObject *O); 
extern NspMaxpMatrix *GetMpMatCopy (Stack stack, int i); 
extern NspMaxpMatrix *GetMpMat (Stack stack, int i); 
extern NspMaxpMatrix *GetMpMatCopyInt (Stack stack, int i); 
extern NspMaxpMatrix *GetMpMatInt (Stack stack, int i); 
extern NspMaxpMatrix *GetMpMatCopyFloat (Stack stack, int i); 
extern NspMaxpMatrix *GetMpMatFloat (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMatCopy_G (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMat_G (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMatCopy (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMat (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMatCopyInt (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMatInt (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMatCopyFloat (Stack stack, int i); 
extern NspMaxpMatrix *GetRealMpMatFloat (Stack stack, int i); 
extern int *MpMatd2i (NspMaxpMatrix *A, int *imin, int *imax); 
extern NspMaxpMatrix *MpMat2double (NspMaxpMatrix *A); 
extern NspMaxpMatrix *MpMat2int (NspMaxpMatrix *A); 
extern NspMaxpMatrix *MpMat2float (NspMaxpMatrix *A); 

/* typedef int (*F_Enlarge) (void *A,int m,int n); */

extern NspMaxpMatrix * nsp_mp_matrix_from_m(const char *name,NspMatrix *M);
extern NspMaxpMatrix *nsp_mpmatrix_create (const char *name, char type, int m, int n); 
extern NspMaxpMatrix *nsp_mpmatrix_create_impl (double first, double step, double last); 
extern NspMaxpMatrix *nsp_mpmatrix_create_from_doubles(const char *name,int m,int n,...);
extern int nsp_mpmatrix_fill_with (NspMaxpMatrix *A, const NspMaxpMatrix *B); 
extern int nsp_mpmatrix_resize (NspMaxpMatrix *A, int m, int n); 
extern int nsp_mpmatrix_scalar_to_mn (NspMaxpMatrix *A, int m, int n); 
extern void nsp_mpmatrix_latex_print (const NspMaxpMatrix *Mat); 
extern void nsp_mpmatrix_latex_tab_print(const NspMaxpMatrix *Mat); 
extern int nsp_mpmatrix_redim (NspMaxpMatrix *A, int m, int n); 
extern int nsp_mpmatrix_enlarge (NspMaxpMatrix *A, int m, int n); 
extern int nsp_mpmatrix_concat_right(NspMaxpMatrix *A, const NspMaxpMatrix *B); 
extern int nsp_mpmatrix_add_columns(NspMaxpMatrix *A, int n); 
extern NspMaxpMatrix *nsp_mpmatrix_concat_down(const NspMaxpMatrix *A,const NspMaxpMatrix *B); 
extern NspMaxpMatrix *nsp_mpmatrix_concat_diag(const NspMaxpMatrix *A,const NspMaxpMatrix *B); 
extern int nsp_mpmatrix_add_rows(NspMaxpMatrix *A, int m); 
extern int nsp_mpmatrix_set_submatrix(NspMaxpMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspMaxpMatrix *B); 
extern int nsp_mpmatrix_set_rows(NspMaxpMatrix *A, NspMatrix *Rows, NspMaxpMatrix *B); 
extern int nsp_mpmatrix_delete_columns(NspMaxpMatrix *A, NspMatrix *Cols); 
extern int nsp_mpmatrix_delete_rows(NspMaxpMatrix *A, NspMatrix *Rows); 
extern int nsp_mpmatrix_delete_elements(NspMaxpMatrix *A, NspMatrix *Elts); 
extern NspMaxpMatrix *nsp_mpmatrix_extract(const NspMaxpMatrix *A,const  NspMatrix *Rows,const  NspMatrix *Cols); 
extern NspMaxpMatrix *nsp_mpmatrix_extract_elements(const NspMaxpMatrix *A,const  NspMatrix *Elts); 
extern NspMaxpMatrix *nsp_mpmatrix_extract_columns(const NspMaxpMatrix *A,const  NspMatrix *Cols); 
extern NspMaxpMatrix *nsp_mpmatrix_extract_rows(const NspMaxpMatrix *A,const  NspMatrix *Rows); 
extern NspMaxpMatrix *MpMatLoopCol (char *str, NspMaxpMatrix *Col, NspMaxpMatrix *A, int icol, int *rep); 
extern NspMaxpMatrix *nsp_mpmatrix_extract_diag(const NspMaxpMatrix *A, int k); 
extern int nsp_mpmatrix_set_diag(NspMaxpMatrix *A, NspMaxpMatrix *Diag, int k); 
extern NspMaxpMatrix *nsp_mpmatrix_create_diag(const NspMaxpMatrix *Diag, int k); 
extern NspMaxpMatrix *nsp_mpmatrix_transpose(const NspMaxpMatrix *A); 

/* from MatOps.c */

extern void nsp_mpmat_set_rval(NspMaxpMatrix *A, double dval); 
extern int nsp_mpmat_set_ival(NspMaxpMatrix *A, double dval); 
extern NspMaxpMatrix *nsp_mpmat_mult(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_add(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_dadd(NspMaxpMatrix *Mat1, NspMaxpMatrix *Mat2); 
extern int nsp_mpmat_add_scalar(NspMaxpMatrix *Mat1, NspMaxpMatrix *Mat2); 
extern int nsp_mpmat_sub(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_dsub(NspMaxpMatrix *Mat1, NspMaxpMatrix *Mat2); 
extern int nsp_mpmat_sub_scalar(NspMaxpMatrix *Mat1, NspMaxpMatrix *Mat2); 
extern int nsp_mpmat_subs_calarm(NspMaxpMatrix *Mat1, NspMaxpMatrix *Mat2); 
extern void nsp_mpmat_clean(NspMaxpMatrix *A, int rhs, double epsa, double epsr); 
extern int nsp_mpmat_maxitt1(NspMaxpMatrix *A, NspMaxpMatrix *B, NspMaxpMatrix *Ind, int j, int flag); 
extern int nsp_mpmat_minitt1(NspMaxpMatrix *A, NspMaxpMatrix *B, NspMaxpMatrix *Ind, int j, int flag); 
extern NspMaxpMatrix **nsp_mpmat_slec(char *file, int *Count); 
extern NspMaxpMatrix *MaxpMatLec (FILE *fd); 
extern int nsp_mpmat_readline(FILE *fd); 
extern int nsp_mpmat_complexify(NspMaxpMatrix *Mat, double d); 
extern int nsp_mpmat_get_real(NspMaxpMatrix *A); 
extern int nsp_mpmat_get_imag(NspMaxpMatrix *A); 
extern int nsp_mpmat_inv_el(NspMaxpMatrix *A); 
extern NspMaxpMatrix *nsp_mpmat_kron(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern NspMaxpMatrix *nsp_mpmat_sort(NspMaxpMatrix *A, int flag, char *str1, char *str2); 
extern NspMaxpMatrix *nsp_mpmat_sum(NspMaxpMatrix *A, char *flag); 
extern NspMaxpMatrix *nsp_mpmat_prod(NspMaxpMatrix *A, char *flag); 
extern NspMaxpMatrix *nsp_mpmat_cum_prod(NspMaxpMatrix *A, char *flag); 
extern NspMaxpMatrix *nsp_mpmat_cum_sum(NspMaxpMatrix *A, char *flag); 
extern NspMaxpMatrix *nsp_mpmat_maxi(NspMaxpMatrix *A, char *flag, NspMaxpMatrix **Imax, int lhs); 
extern NspMaxpMatrix *nsp_mpmat_mini(NspMaxpMatrix *A, char *flag, NspMaxpMatrix **Imax, int lhs); 
extern NspMaxpMatrix *nsp_mpmat_createinit(char *name, char type, int m, int n, double (*func) ()); 
extern void nsp_mpmat_triu(NspMaxpMatrix *A, int k); 
extern void nsp_mpmat_tril(NspMaxpMatrix *A, int k); 
extern NspMaxpMatrix *nsp_mpmat_eye(int m, int n); 
extern NspMaxpMatrix *nsp_mpmat_ones(int m, int n); 
extern NspMaxpMatrix *nsp_mpmat_zeros(int m, int n); 
extern NspMaxpMatrix *nsp_mpmat_rand(int m, int n); 
extern int nsp_mpmat_pow_tt(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_pow_el(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_pow_scalar(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_pow_scalarm(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_div_tt(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_div_el(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_div_scalar(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_bdiv_tt(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_bdiv_el(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_bdiv_scalar(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_mult_tt(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_mult_el(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_mult_scalar(NspMaxpMatrix *Mat1, NspMaxpMatrix *Mat2); 
extern int nsp_mpmat_acos(NspMaxpMatrix *A); 
extern int nsp_mpmat_acosh(NspMaxpMatrix *A); 
extern int nsp_mpmat_asin(NspMaxpMatrix *A); 
extern int nsp_mpmat_asinh(NspMaxpMatrix *A); 
extern int nsp_mpmat_atan(NspMaxpMatrix *A); 
extern int nsp_mpmat_atan2(NspMaxpMatrix *A,NspMaxpMatrix *B); 
extern int nsp_mpmat_atanh(NspMaxpMatrix *A); 
extern void nsp_mpmat_ceil(NspMaxpMatrix *A); 
extern void nsp_mpmat_modulo(NspMaxpMatrix *A, int n); 
extern void nsp_mpmat_idiv(NspMaxpMatrix *A, int n); 
extern void nsp_mpmat_int(NspMaxpMatrix *A); 
extern void nsp_mpmat_floor(NspMaxpMatrix *A); 
extern void nsp_mpmat_round(NspMaxpMatrix *A); 
extern int nsp_mpmat_sign(NspMaxpMatrix *A); 
extern int nsp_mpmat_tan(NspMaxpMatrix *A); 
extern int nsp_mpmat_tanh(NspMaxpMatrix *A); 
extern int nsp_mpmat_abs(NspMaxpMatrix *A); 
extern int nsp_mpmat_erf(NspMaxpMatrix *A); 
extern int nsp_mpmat_erfc(NspMaxpMatrix *A); 
extern int nsp_mpmat_arg(NspMaxpMatrix *A); 
extern int nsp_mpmat_polar(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_iand(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_iandu(NspMaxpMatrix *A, unsigned int *res); 
extern int nsp_mpmat_ior(NspMaxpMatrix *A, NspMaxpMatrix *B); 
extern int nsp_mpmat_ioru(NspMaxpMatrix *A, unsigned int *res); 
extern void nsp_mpmat_conj(NspMaxpMatrix *A); 
extern void nsp_mpmat_cos(NspMaxpMatrix *A); 
extern void nsp_mpmat_cosh(NspMaxpMatrix *A); 
extern void nsp_mpmat_expel(NspMaxpMatrix *A); 
extern int nsp_mpmat_logel(NspMaxpMatrix *A); 
extern void nsp_mpmat_sin(NspMaxpMatrix *A); 
extern void nsp_mpmat_sinh(NspMaxpMatrix *A); 
extern int nsp_mpmat_sqrtel(NspMaxpMatrix *A); 
extern int nsp_mpmat_minus(NspMaxpMatrix *A); 
extern NspMaxpMatrix *nsp_mpmat_magic(int n); 
extern NspMaxpMatrix *nsp_mpmat_franck(int n, int job); 
extern NspMaxpMatrix *nsp_mpmat_hilbert(int n,int job); 
extern int nsp_mpmat_fullcomp(NspMaxpMatrix *A, NspMaxpMatrix *B, char *op, int *err); 
extern int nsp_mpmat_find(NspMaxpMatrix *A, int lhs, NspMaxpMatrix **Res1, NspMaxpMatrix **Res2); 

extern NspMatrix * nsp_mpmatrix_cast_to_matrix(NspMaxpMatrix *M);
extern NspMaxpMatrix * nsp_matrix_cast_to_mpmatrix(NspMatrix *M);

#endif 

