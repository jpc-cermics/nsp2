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
static int SpMatSize(NspSpMatrix *Mat, int flag);
char *SpMatType(void);
char *SpMatShType(void);
NspObject *SpLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int SpMatObjEq(NspObject *A,NspObject *B);
int SpMatObjNeq(NspObject *A,NspObject *B);
static NspSpMatrix *SpMatXdrLoad(NspFile  *F);
static int SpMatXdrSave(NspFile  *F, NspMatrix *M);
#endif 

#define NULLSP (  NspSpMatrix *) 0
/* XXXXXXXXXXX */
#define EPSILON 1.e-15

/* SpMatObj.c */

 extern NspSpMatrix *SpObj (NspObject *O); 
 extern int IsSpObj (Stack stack, int i); 
 extern NspSpMatrix *GetSpCopy (Stack stack, int i); 
 extern NspSpMatrix *GetSp (Stack stack, int i); 

/* NspSpMatrix.c */

extern void SpDestroy (NspSpMatrix *Mat); 
extern void SpInfo (NspSpMatrix *Sp, int indent); 
extern void SpPrint (NspSpMatrix *Sp, int indent); 
extern NspSpMatrix *SpCopy (NspSpMatrix *A); 
extern NspSpMatrix   *SpMatObj(NspObject *O);

 extern void PlusLeft (SpRow *, char, int *, SpRow *, char, int);
 extern void PlusBoth (SpRow *, char, int *, SpRow *, char, int, SpRow *, char, int);
 extern void PlusRight (SpRow *, char, int *, SpRow *, char, int);
 extern void MinusLeft (SpRow *, char, int *, SpRow *, char, int);
 extern void MinusBoth (SpRow *, char, int *, SpRow *, char, int, SpRow *, char, int);
 extern void MinusRight (SpRow *, char, int *, SpRow *, char, int);
 extern void MultttLeft (SpRow *, char, int *, SpRow *, char, int);
 extern void MultttBoth (SpRow *, char, int *, SpRow *, char, int, SpRow *, char, int);
 extern void MultttRight (SpRow *, char, int *, SpRow *, char, int);
 extern NspSpMatrix *SpCreate (char *name, char type, integer m, integer n); 
 extern NspSpMatrix *Sparse (NspMatrix *RC, NspMatrix *Values, int m, int n); 
 extern int SpGet (NspSpMatrix *A, NspMatrix **RC, NspMatrix **Values); 
 extern int SpResizeRow (NspSpMatrix *Sp, int i, int n); 
 extern void SpRowDestroy (SpRow *Row); 
 extern NspSpMatrix *SpRedim (NspSpMatrix *A, integer m, integer n); 
 extern int EnlargeSpRows (NspSpMatrix *Sp, integer m); 
 extern int SpEnlarge (NspSpMatrix *A, integer m, integer n); 
 extern int SpConcatR (NspSpMatrix *A, NspSpMatrix *B); 
 extern int SpConcatD (NspSpMatrix *A, NspSpMatrix *B); 
 extern int SpConcatDiag (NspSpMatrix *A, NspSpMatrix *B); 
 extern void SpStore (NspSpMatrix *A, int r, int c, int col, NspSpMatrix *B, int r1, int c1); 
 extern int SpInsertElt (NspSpMatrix *A, int i, int j, NspSpMatrix *B, int rb, int cb); 
 extern int SpDeleteElt (NspSpMatrix *A, int row, int col, int amin, int amax); 
 extern int SpGetElt (NspSpMatrix *B, int i, int j); 
 extern int SpSetRoCo (NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspSpMatrix *B); 
 extern int SpSetRo (NspSpMatrix *A, NspMatrix *Inds, NspSpMatrix *B); 
 extern int SpDeleteCols (NspSpMatrix *A, NspMatrix *Cols); 
 extern int CompressRow (NspSpMatrix *A, int i); 
 extern int SpDeleteRows (NspSpMatrix *A, NspMatrix *Rows); 
 extern NspSpMatrix *SpExtract (NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
 extern NspSpMatrix *SpExtractElts (NspSpMatrix *A, NspMatrix *Elts); 
 extern NspSpMatrix *SpExtractCols (NspSpMatrix *A, NspMatrix *Cols, int *err); 
 extern NspSpMatrix *SpExtractRows (NspSpMatrix *A, NspMatrix *Rows, int *err); 
 extern NspSpMatrix *SpDiagExtract (NspSpMatrix *A, integer k); 
 extern int SpDiagSet (NspSpMatrix *A, NspSpMatrix *Diag, integer k); 
 extern NspSpMatrix *SpDiagCreate (NspSpMatrix *Diag, integer k); 
 extern NspSpMatrix *SpMult (NspSpMatrix *A, NspSpMatrix *B); 
 extern int SpMultScal (NspSpMatrix *A, NspSpMatrix *B); 
 extern int SpComplexify (NspSpMatrix *A); 
 extern int SpSetR (NspSpMatrix *A, double d); 
 extern int SpSetI (NspSpMatrix *A, double d); 
 extern int RowCountNonNull (NspMatrix *A, integer i); 
 extern int CountNonNull (NspMatrix *A); 
 extern NspSpMatrix *Full2Sparse (NspMatrix *A); 
 extern NspMatrix *Sparse2Full (NspSpMatrix *Sp); 
 extern NspSpMatrix *SpTranspose (NspSpMatrix *A); 
 extern double plus (double x, double y, double xi, double yi, double *ival, char type); 
 extern void PlusLeft (SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1); 
 extern void PlusBoth (SpRow *Li, char Ltype, integer *count, SpRow *Ai, char Atype, integer k1, SpRow *Bi, char Btype, int k2); 
 extern void PlusRight (SpRow *Li, char Ltype, integer *count, SpRow *Bi, char Btype, integer k1); 
 extern void MinusLeft (SpRow *Li, char Ltype, integer *count, SpRow *Ai, char Atype, integer k1); 
 extern void MinusBoth (SpRow *Li, char Ltype, integer *count, SpRow *Ai, char Atype, integer k1, SpRow *Bi, char Btype, int k2); 
 extern void MinusRight (SpRow *Li, char Ltype, integer *count, SpRow *Bi, char Btype, integer k1); 
 extern void MultttLeft (SpRow *Li, char Ltype, integer *count, SpRow *Ai, char Atype, integer k1); 
 extern void MultttBoth (SpRow *Li, char Ltype, integer *count, SpRow *Ai, char Atype, integer k1, SpRow *Bi, char Btype, int k2); 
 extern void MultttRight (SpRow *Li, char Ltype, integer *count, SpRow *Bi, char Btype, integer k1); 
 extern NspSpMatrix *SpAdd (NspSpMatrix *A, NspSpMatrix *B); 
 extern NspSpMatrix *SpSub (NspSpMatrix *A, NspSpMatrix *B); 
 extern NspSpMatrix *SpMulttt (NspSpMatrix *A, NspSpMatrix *B); 
 extern int SparseMultScal (NspSpMatrix *A, NspSpMatrix *B); 
 extern NspMatrix *SparseOpScal (NspSpMatrix *A, NspSpMatrix *B, int *flag, char op); 

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
