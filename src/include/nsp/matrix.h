#ifndef INC_NSP_MATRIX 
#define INC_NSP_MATRIX

/*
 * This Software is ( Copyright ENPC 1998-2003 ) 
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
  matrix_save * save;
} NspTypeMatrix;

struct _nsp_matrix {
  NspObject father; 
  NspTypeMatrix *type; 
  integer m,n,mn;/* NspMatrix dimensions */
  char rc_type;     /* 'r' : real or  'i' : complex  */
  char convert;  /* 'd','i','f' : double, integer, float
		    used for array data conversion */
  double *R;     /* Pointer on real values */
  doubleC *I;    /* Pointer on complex values */
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

/* XXXXXX */ 

#define EPSILON 1.e-15
#define NULLMAT (NspMatrix*) 0


double *nsp_alloc_doubles(unsigned int n);
double *nsp_realloc_doubles(double *dp, unsigned int n);
int *nsp_alloc_int(unsigned int n);
int *nsp_realloc_int(int *dp, unsigned int n);
doubleC *nsp_alloc_doubleC(unsigned int n);
doubleC *nsp_realloc_doubleC(doubleC *dp, unsigned int n);

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


extern int MatFullComp ();
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
extern NspMatrix *Mat2double (NspMatrix *A); 
extern NspMatrix *Mat2int (NspMatrix *A); 
extern NspMatrix *Mat2float (NspMatrix *A); 

typedef int (*F_Enlarge) (void *A,int m,int n);

extern NspMatrix *nsp_matrix_create (const char *name, char type, integer m, integer n); 
extern NspMatrix *nsp_matrix_create_impl (double first, double step, double last); 
NspMatrix *nsp_matrix_create_from_doubles(const char *name,integer m,integer n,...);
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

extern void MatSetR (NspMatrix *A, double dval); 
extern int MatSetI (NspMatrix *A, double dval); 
extern NspMatrix *MatMult (NspMatrix *A, NspMatrix *B); 
extern int Mat_add (NspMatrix *A, NspMatrix *B); 
extern int MatDadd (NspMatrix *Mat1, NspMatrix *Mat2); 
extern int MatAddScalar (NspMatrix *Mat1, NspMatrix *Mat2); 
extern int Mat_sub (NspMatrix *A, NspMatrix *B); 
extern int MatDsub (NspMatrix *Mat1, NspMatrix *Mat2); 
extern int MatSubScalar (NspMatrix *Mat1, NspMatrix *Mat2); 
extern int MatSubScalarM (NspMatrix *Mat1, NspMatrix *Mat2); 
extern void MatClean (NspMatrix *A, int rhs, double epsa, double epsr); 
extern int MatMaxitt1 (NspMatrix *A, NspMatrix *B, NspMatrix *Ind, integer j, integer flag); 
extern int MatMinitt1 (NspMatrix *A, NspMatrix *B, NspMatrix *Ind, integer j, integer flag); 
extern NspMatrix **MatsLec (char *file, int *Count); 
extern FILE *fopen (const char *, const char *);
extern NspMatrix *MatLec (FILE *fd); 
extern NspMatrix *fooBOU (void); 
extern int ReadLine (FILE *fd); 
extern void TestNumTokens (void); 
extern int NumTokens (char *string); 

extern void CsetD(const int *n,const double *z,doubleC *tab,const int *inc) ;
extern void CIset(const integer *n,const double *z, doubleC *tab, const integer *inc);
extern int Complexify (NspMatrix *Mat, double d); 
extern int RealPart (NspMatrix *A); 
extern int ImagPart (NspMatrix *A); 
extern int MatInvEl (NspMatrix *A); 
extern NspMatrix *MatKron (NspMatrix *A, NspMatrix *B); 
extern NspMatrix *MatSort (NspMatrix *A, int flag, char *str1, char *str2); 
extern NspMatrix *MatSum (NspMatrix *A, char *flag); 
extern NspMatrix *MatProd (NspMatrix *A, char *flag); 
extern NspMatrix *MatCuProd (NspMatrix *A, char *flag); 
extern NspMatrix *MatCuSum (NspMatrix *A, char *flag); 
extern NspMatrix *MatMaxi (NspMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
extern NspMatrix *MatMini (NspMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
extern NspMatrix *MatCreateInit (char *name, char type, integer m, integer n, double (*func) ()); 
extern void MatTriu (NspMatrix *A, integer k); 
extern void MatTril (NspMatrix *A, integer k); 
extern NspMatrix *MatEye (integer m, integer n); 
extern NspMatrix *MatOnes (integer m, integer n); 
extern NspMatrix *MatZeros (integer m, integer n); 
extern NspMatrix *MatRand (integer m, integer n); 
extern void SetUrandSeed (int m); 
extern int GetUrandSeed (void); 
extern void SetUrandType (int m); 
extern int GetUrandType (void); 
extern int Mat_pow_tt (NspMatrix *A, NspMatrix *B); 
extern int MatPowEl (NspMatrix *A, NspMatrix *B); 
extern int MatPowScalar (NspMatrix *A, NspMatrix *B); 
extern int MatPowScalarM (NspMatrix *A, NspMatrix *B); 
extern int Mat_div_tt (NspMatrix *A, NspMatrix *B); 
extern int MatDivEl (NspMatrix *A, NspMatrix *B); 
extern int MatDivScalar (NspMatrix *A, NspMatrix *B); 
extern int Mat_bdiv_tt (NspMatrix *A, NspMatrix *B); 
extern int MatBackDivEl (NspMatrix *A, NspMatrix *B); 
extern int MatBackDivScalar (NspMatrix *A, NspMatrix *B); 
extern int Mat_mult_tt (NspMatrix *A, NspMatrix *B); 
extern int MatMultEl (NspMatrix *A, NspMatrix *B); 
extern int MatMultScalar (NspMatrix *Mat1, NspMatrix *Mat2); 
extern int MatAcos (NspMatrix *A); 
extern int MatAcosh (NspMatrix *A); 
extern int MatAsin (NspMatrix *A); 
extern int MatAsinh (NspMatrix *A); 
extern int MatAtan (NspMatrix *A); 
extern int MatAtan2 (NspMatrix *A,NspMatrix *B); 
extern int MatAtanh (NspMatrix *A); 
extern void MatCeil (NspMatrix *A); 
extern void MatModulo (NspMatrix *A, int n); 
extern void MatIdiv (NspMatrix *A, int n); 
extern void MatInt (NspMatrix *A); 
extern void MatFloor (NspMatrix *A); 
extern void MatRound (NspMatrix *A); 
extern int MatSign (NspMatrix *A); 
extern int MatTan (NspMatrix *A); 
extern int MatTanh (NspMatrix *A); 
extern int MatAbs (NspMatrix *A); 
extern int MatErf (NspMatrix *A); 
extern int MatErfc (NspMatrix *A); 
extern int MatArg (NspMatrix *A); 
extern int MatPolar (NspMatrix *A, NspMatrix *B); 
extern int MatIand (NspMatrix *A, NspMatrix *B); 
extern int MatIandU (NspMatrix *A, unsigned int *res); 
extern int MatIor (NspMatrix *A, NspMatrix *B); 
extern int MatIorU (NspMatrix *A, unsigned int *res); 
extern void MatConj (NspMatrix *A); 
extern void MatCos (NspMatrix *A); 
extern void MatCosh (NspMatrix *A); 
extern void MatExpEl (NspMatrix *A); 
extern int MatLogEl (NspMatrix *A); 
extern void MatSin (NspMatrix *A); 
extern void MatSinh (NspMatrix *A); 
extern int MatSqrtEl (NspMatrix *A); 
extern int MatMinus (NspMatrix *A); 
extern NspMatrix *MatMagic (integer n); 
extern NspMatrix *MatFranck (integer n, integer job); 
extern NspMatrix *MatHilbert (integer n); 
extern int MatFullComp (NspMatrix *A, NspMatrix *B, char *op, int *err); 
extern int MatFind (NspMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 

#endif 

