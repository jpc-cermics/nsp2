#ifndef INC_NSP_SPMATRIX 
#define INC_NSP_SPMATRIX 

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspSpMatrix inherits from NspObject 
 */

typedef struct _nsp_spmatrix NspSpMatrix;

typedef int (*spmatrix_save) (NspFile  *F, NspSpMatrix *M);

typedef struct _nsp_type_SpMatrix { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  spmatrix_save *save;
} NspTypeSpMatrix;

/* Sparse Matrix */

/*****************************************************************
  m,n :  size of matrix 
  D[i]-> points to the ith line 
  D[i]-> size : number of non nul elements in row i 
  D[i]->J : array of column of the elements of lin i
  D[i]->R : array of real part of the elements of lin i
  D[i]->I : array of imaginary part of the elements of lin i
 ******************************************************************/

typedef struct SpRow {
  integer size,iw ; /* size of a row, iw : used for working storage*/
  integer *J   ; /* pointer to an integer array giving the columns or row i 
		in increasing order */
  double  *R; /* pointer to Real datas */
  doubleC *I; /* pointer to complex  datas */
} SpRow ;
  

struct _nsp_spmatrix {
  NspObject father; 
  NspTypeSpMatrix *type; 
  char rc_type;        /* 'r' or 'i'  : real or complex matrix */
  integer m,n,mn;   
  struct SpRow **D; /* array of size m giving the Rows datas */
} ;

extern int nsp_type_spmatrix_id;
extern NspTypeSpMatrix *nsp_type_spmatrix;

int nsp_type_spmatrix_init();

/* only useful when building a new class derived from spmatrix */

NspTypeSpMatrix *new_type_spmatrix(type_mode mode);

NspSpMatrix *new_spmatrix();

/*
 * Object methods redefined for spmatrix 
 */

#ifdef SpMatrix_Private 
static int init_spmatrix(NspSpMatrix *ob,NspTypeSpMatrix *type);
static int nsp_spmatrix_size(NspSpMatrix *Mat, int flag);
char *nsp_spmatrix_type_as_string(void);
char *nsp_spmatrix_type_short_string(void);
NspObject *SpLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_spmatrix_eq(NspObject *A,NspObject *B);
int nsp_spmatrix_neq(NspObject *A,NspObject *B);
static NspSpMatrix *nsp_spmatrix_xdr_load(NspFile  *F);
static int nsp_spmatrix_xdr_save(NspFile  *F, NspMatrix *M);
#endif 

#define NULLSP (  NspSpMatrix *) 0
/* XXXXXXXXXXX */
#define EPSILON 1.e-15

/* SpMatObj.c */

 extern NspSpMatrix *SpObj (NspObject *O); 
 extern int IsSpMatObj (Stack stack, int i); 
 extern NspSpMatrix *GetSpCopy (Stack stack, int i); 
 extern NspSpMatrix *GetSp (Stack stack, int i); 

/* NspSpMatrix.c */

extern void nsp_spmatrix_destroy(NspSpMatrix *Mat); 
extern void nsp_spmatrix_info(NspSpMatrix *Sp, int indent); 
extern void nsp_spmatrix_print(NspSpMatrix *Sp, int indent); 
extern NspSpMatrix *nsp_spmatrix_copy(NspSpMatrix *A); 
extern NspSpMatrix   *nsp_spmatrix_object(NspObject *O);


 extern NspSpMatrix *nsp_spmatrix_create(char *name, char type, integer m, integer n); 
 extern NspSpMatrix *nsp_spmatrix_sparse(NspMatrix *RC, NspMatrix *Values, int m, int n); 
 extern int nsp_spmatrix_get(NspSpMatrix *A, NspMatrix **RC, NspMatrix **Values); 
 extern int nsp_spmatrix_resize_row(NspSpMatrix *Sp, int i, int n); 
 extern void SpRowDestroy (SpRow *Row); 
 extern NspSpMatrix *nsp_spmatrix_redim(NspSpMatrix *A, integer m, integer n); 
 extern int nsp_spmatrix_enlarge_rows(NspSpMatrix *Sp, integer m); 
 extern int nsp_spmatrix_enlarge(NspSpMatrix *A, integer m, integer n); 
 extern int nsp_spmatrix_concatr(NspSpMatrix *A, NspSpMatrix *B); 
 extern int nsp_spmatrix_concatd(NspSpMatrix *A, NspSpMatrix *B); 
 extern int nsp_spmatrix_concatdiag(NspSpMatrix *A, NspSpMatrix *B); 
 extern void  nsp_spmatrix_store(NspSpMatrix *A, int r, int c, int col, NspSpMatrix *B, int r1, int c1); 
 extern int nsp_spmatrix_insert_elt(NspSpMatrix *A, int i, int j, NspSpMatrix *B, int rb, int cb); 
 extern int nsp_spmatrix_delete_elt(NspSpMatrix *A, int row, int col, int amin, int amax); 
 extern int nsp_spmatrix_get_elt(NspSpMatrix *B, int i, int j); 
 extern int nsp_spmatrix_set_rowcol(NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspSpMatrix *B); 
 extern int nsp_spmatrix_set_row(NspSpMatrix *A, NspMatrix *Inds, NspSpMatrix *B); 
 extern int nsp_spmatrix_delete_cols(NspSpMatrix *A, NspMatrix *Cols); 
 extern int nsp_spmatrix_compress_row(NspSpMatrix *A, int i); 
 extern int nsp_spmatrix_delete_rows(NspSpMatrix *A, NspMatrix *Rows); 
 extern NspSpMatrix *nsp_spmatrix_extract(NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
 extern NspSpMatrix *nsp_spmatrix_extract_elts(NspSpMatrix *A, NspMatrix *Elts); 
 extern NspSpMatrix *nsp_spmatrix_extract_cols(NspSpMatrix *A, NspMatrix *Cols, int *err); 
 extern NspSpMatrix *nsp_spmatrix_extract_rows(NspSpMatrix *A, NspMatrix *Rows, int *err); 
 extern NspSpMatrix *nsp_spmatrix_diag_extract(NspSpMatrix *A, integer k); 
 extern int nsp_spmatrix_diag_set(NspSpMatrix *A, NspSpMatrix *Diag, integer k); 
 extern NspSpMatrix *nsp_spmatrix_diag_create(NspSpMatrix *Diag, integer k); 
 extern NspSpMatrix *nsp_spmatrix_mult(NspSpMatrix *A, NspSpMatrix *B); 
 extern int nsp_spmatrix_mult_scal(NspSpMatrix *A, NspSpMatrix *B); 
 extern int nsp_spmatrix_complexify(NspSpMatrix *A); 
 extern int nsp_spmatrix_setr(NspSpMatrix *A, double d); 
 extern int nsp_spmatrix_seti(NspSpMatrix *A, double d); 
 extern int RowCountNonNull (NspMatrix *A, integer i); 
 extern int CountNonNull (NspMatrix *A); 
 extern NspSpMatrix *nsp_spmatrix_from_mat(NspMatrix *A); 
 extern NspMatrix *nsp_spmatrix_to_mat(NspSpMatrix *Sp); 
 extern NspSpMatrix *nsp_spmatrix_transpose(NspSpMatrix *A); 
 extern double plus (double x, double y, double xi, double yi, double *ival, char type); 
 extern NspSpMatrix *nsp_spmatrix_add(NspSpMatrix *A, NspSpMatrix *B); 
 extern NspSpMatrix *nsp_spmatrix_sub(NspSpMatrix *A, NspSpMatrix *B); 
 extern NspSpMatrix *nsp_spmatrix_multtt(NspSpMatrix *A, NspSpMatrix *B); 
 extern int nsp_spmatrix_mult_scal(NspSpMatrix *A, NspSpMatrix *B); 
 extern NspMatrix *nsp_spmatrix_op_scal(NspSpMatrix *A, NspSpMatrix *B, int *flag, char op); 

/* SpMatOps.c */

 extern int SpClean (NspSpMatrix *A, int rhs, double epsa, double epsr); 
 extern NspMatrix *SpMaxiMinitt_G (NspSpMatrix *A, NspSpMatrix *B, int flag, int minmaxflag, int *err); 
 extern NspMatrix *SpMaxitt (NspSpMatrix *A, NspSpMatrix *B, int flag, int *err); 
 extern NspMatrix *SpMinitt (NspSpMatrix *A, NspSpMatrix *B, int flag, int *err); 
 extern int SpRealPart (NspSpMatrix *A); 
 extern int SpImagPart (NspSpMatrix *A); 
 extern NspSpMatrix *SpSum (NspSpMatrix *A, char *flag); 
 extern NspSpMatrix *SpMaxi (NspSpMatrix *A, char *flag, NspMatrix **Imax, int lhs); 
 extern NspSpMatrix *SpEye (integer m, integer n); 
 extern NspSpMatrix *SpOnes (integer m, integer n); 
 extern NspSpMatrix *SpZeros (integer m, integer n); 
 extern NspMatrix *SpAcos (NspSpMatrix *A); 
 extern NspMatrix *SpAcosh (NspSpMatrix *A); 
 extern void SpAsin (NspSpMatrix *A); 
 extern void SpAsinh (NspSpMatrix *A); 
 extern void SpAtan (NspSpMatrix *A); 
 extern void SpAtanh (NspSpMatrix *A); 
 extern void SpCeil (NspSpMatrix *A); 
 extern double R_aint (double x); 
 extern void SpInt (NspSpMatrix *A); 
 extern void SpFloor (NspSpMatrix *A); 
 extern double R_anint (double x); 
 extern void SpRound (NspSpMatrix *A); 
 extern int SpSign (NspSpMatrix *A); 
 extern void SpTan (NspSpMatrix *A); 
 extern void SpTanh (NspSpMatrix *A); 
 extern int SpAbs (NspSpMatrix *A); 
 extern int SpErf (NspSpMatrix *A); 
 extern int SpArg (NspSpMatrix *A); 
 extern void SpConj (NspSpMatrix *A); 
 extern NspMatrix *SpCos (NspSpMatrix *A); 
 extern NspMatrix *SpCosh (NspSpMatrix *A); 
 extern NspMatrix *SpExpEl (NspSpMatrix *A); 
 extern int SpLogEl (NspSpMatrix *A); 
 extern void SpSin (NspSpMatrix *A); 
 extern void SpSinh (NspSpMatrix *A); 
 extern int SpSqrtEl (NspSpMatrix *A); 
 extern int SpMinus (NspSpMatrix *A); 
 extern int SpFind (NspSpMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 


#endif 
