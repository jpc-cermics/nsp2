#ifndef SCI_ARRAY 
#define SCI_ARRAY 

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for FILE declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

#ifdef OCAML 
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#endif 

typedef struct Array
{
  OBJ_COMMON 
  integer ndims; /* number of dimensions  */
  integer size;  /* product of dims */
  char type;     /* 'r' : real or  'i' : complex  */
  char convert;  /* 'd','i','f' : double, integer, float
		    used for array data conversion */
  double *R;     /* Pointer on real values */
  doubleC *I;    /* Pointer on complex values */
  int *dims;
#ifdef OCAML 
  int flags; 
  struct caml_bigarray_proxy *proxy; /* when data is shared with 
					a bigarray */
#endif 
} Array ;

#ifdef OCAML 
Matrix *MatCreateFromData  (char *name, char type, integer m, 
				     integer n,struct caml_bigarray *b);
#endif 

#define EPSILON 1.e-15
#define NULLARRAY (struct Array*) 0

double *nsp_alloc_doubles(integer n);
double *nsp_realloc_doubles(double *dp, integer n);
integer *nsp_alloc_int(integer n);
integer *nsp_realloc_int(integer *dp, integer n);
doubleC *nsp_alloc_doubleC(integer n);
doubleC *nsp_realloc_doubleC(doubleC *dp, integer n);
int ArraySize  (Array *,int) ;
Array *ArrayMult  (Array *A, Array *B);
Array *ArrayCreate  (char *name, char type,int ndim, int dims[]);
Array *ArrayCreateImpl  (double first,double step,double last);
Array *ArrayMagic  (integer n);
Array *ArrayFranck  (integer n,int job);
Array *ArrayHilbert  (integer n);
Array *ArrayCopy  (Array *A);
int ArrayFillWith   (Array *A,Array *B);
int ArrayScalar2Full(Array *A, int  ndim, int dims[]);

int AComplexify  (Array *Mat, double d);
int ARealPart  (Array *A);
int AImagPart  (Array *A);
void ArrayConj  ( Array *A);
void ArrayRound  ( Array *A);
int ArraySign  ( Array *A);
Array *ArrayKron  (Array *A, Array *B);
int ArraySqrtEl  ( Array *A);
void ArrayExpEl  ( Array *A);
Array *ArraySort  (Array *A,int, char *str1,char *str2);
void ArrayCos  ( Array *A);
void ArraySin  ( Array *A);
void ArrayInt  ( Array *A);
void ArrayCeil  ( Array *A);
void ArrayModulo  ( Array *A,int n);
void ArrayIdiv  ( Array *A,int n));
void ArrayFloor  ( Array *A);
Array *ArraySum  (Array *A, char *flag);
Array *ArrayCuSum  (Array *A, char *flag);
Array *ArrayProd  (Array *A, char *flag);
Array *ArrayCuProd  (Array *A, char *flag);

Array *ArrayMaxi  (Array *A,char *,Array **Imax,int lhs);
Array *ArrayMini  (Array *A,char *,Array **Imax,int lhs);
Array *ArrayCreateInit  (char *name, char type, integer m, integer n, double (*func) (/* ??? */));
void ArrayTriu  (Array *A, integer k);
void ArrayTril  (Array *A, integer k);
Array *ArrayEye  (integer m, integer n);
Array *ArrayOnes  (integer m, integer n);
Array *ArrayZeros (integer m, integer n);
Array *ArrayRand  (integer m, integer n);
void SetUrandSeed (integer m);
int GetUrandSeed (void);
void SetUrandType  (integer m);
int GetUrandType (void);
int ArrayResize  (Array *A, int ndim, int dims[]);
void ArraySetR  (Array *A, double dval);
int ArraySetI  (Array *A, double dval);
int Array_add(Array *A, Array *B) ;
int Array_sub(Array *A, Array *B) ;
int Array_mult_tt(Array *A, Array *B) ;
int Array_div_tt(Array *A, Array *B) ;
int Array_bdiv_tt(Array *A, Array *B) ;
int Array_pow_tt(Array *A, Array *B) ;
int ArrayDadd  (Array *Array1, Array *Mat2);
int  ArrayAddScalar  ( Array *A,  Array *B);
int ArrayDsub  (Array *Mat1, Array *Mat2);
int  ArraySubScalar  ( Array *A,  Array *B);
int  ArraySubScalarM  ( Array *A,  Array *B);

void ArrayDestroy  (Array *Mat);
void  ArrayClean  (Array *A,int rhs, double epsa,double epsr);
void ArrayInfo  (Array *Mat,int indent);
Array **ArraysLec  (char *file, int *Count);
Array *ArrayLec  (FILE *fd);
void ArrayPrint  (Array *Mat,int indent);
void Array2LaTeXMat  (Array *Mat);
void Array2LaTeXTab  (Array *Mat);
int ReadLine  (FILE *fd);
void TestNumTokens  (void);
int NumTokens  (char *string);
Array *ArrayMaxitt   (Array *A, Array *B,int flag,int *err);
int  ArrayMaxitt1  (Array *A, Array *B, Array *Ind, integer j,int flag);

Array *ArrayMinitt   (Array *A, Array *B,int flag,int *err);
int  ArrayMinitt1  (Array *A, Array *B, Array *Ind, integer j,int flag);

int ArrayAbs  ( Array *A);
int ArrayMinus  ( Array *A);
int ArrayErf  ( Array *A);
int ArrayErfc  ( Array *A);
int ArrayArg  ( Array *A);
int ArrayPolar  ( Array *A,  Array *B);
int ArrayIand  ( Array *A,  Array *B);
int ArrayIor  ( Array *A,  Array *B);
void ArrayConj  ( Array *A);
void ArrayCos  ( Array *A);
void ArrayCosh  ( Array *A);
void ArrayExpEl  ( Array *A); 
int ArrayLogEl  ( Array *A);
int ArrayPowEl  ( Array *A,  Array *B);
int ArrayPowScalar  ( Array *A,  Array *B);
int ArrayPowScalarM  ( Array *A,  Array *B);
void ArraySin  ( Array *A);
void ArraySinh  ( Array *A);
int ArrayDivEl  ( Array *A,  Array *B);
int ArrayDivScalar  ( Array *A,  Array *B);
int ArrayBackDivEl  ( Array *A,  Array *B);
int ArrayBackDivScalar  ( Array *A,  Array *B);
int ArraySqrtEl  ( Array *A);
int ArrayMultEl  ( Array *A,  Array *B);
int ArrayMultScalar  ( Array *A,  Array *B);
int ArrayAcos  ( Array *A);
int ArrayAcosh  ( Array *A);
int ArrayAsin  ( Array *A);
int ArrayAsinh  ( Array *A);
int ArrayAtan  ( Array *A);
int ArrayAtanh  ( Array *A);
void ArrayCeil  ( Array *A);
void ArrayInt  ( Array *A);
void ArrayFloor  ( Array *A);
void ArrayRound  ( Array *A);
int ArraySign  ( Array *A);
int ArrayTan  ( Array *A);
int ArrayTanh  ( Array *A);     

/** A Faire ..... **/

int ArraySetRoCo  ( Array *A,  Array *Rows,  Array *Cols,  Array *B);
int ArraySetRo  ( Array *A,  Array *Cols,  Array *B);
int ArrayRedim  ( Array *A, integer m, integer n);
int ArrayConcatR  ( Array *A,  Array *B);
int ArrayAddCols  ( Array *A, integer n);

int ArrayDeleteRows  ( Array *A,  Array *B);
int ArrayDeleteElts  ( Array *A,  Array *B);
int ArrayDeleteCols  ( Array *A,  Array *B);

Array* ArrayConcatD  ( Array *A,  Array *B);
Array* ArrayConcatDiag  ( Array *A,  Array *B);
int  ArrayAddRows  ( Array *A, integer m);

Array *ArrayExtract  ( Array *A,  Array *Rows,  Array *Cols);
Array *ArrayExtractElts ( Array *A,  Array *Elts);
Array *ArrayExtractCols  ( Array *A,Array *Cols);
Array *ArrayExtractRows ( Array *A,Array *Rows);

Array *ArrayLoopCol   (char *str, Array *Col,Array *A, int icol,int *rep);

Array *ArrayDiagExtract  ( Array *A, integer k);
int ArrayDiagSet  ( Array *A,  Array *Diag, integer k);
Array *ArrayDiagCreate  ( Array *Diag, integer k);
int ArrayLogEl  (Array *);

int *Arrayd2i (Array *A,integer *imin,integer *imax);
void Arrayi2d (Array *A);
void ABounds (Array *A,integer *imin,integer *imax);

void CIset (integer * n,double *z,doubleC *tab,integer *inc) );
void C2F(CsetD) (integer * n,double *z,doubleC *tab,integer *inc);

/* typedef int (*F_Enlarge) (void *A,int m,int n); */

int GenericArraySeRo (void *A,int Am,int An,int Amn,Array *Rows,void* B,int Bm,int Bn,int Bmn,F_Enlarge F,int *Bscal);

int ArrayFind  (Array *A,int rhs,Array **Res1,Array **Res2);
Array *ArrayTranspose (Array *A);

int Array_is_increasing (Array *A);


int ArrayIandU  (Array *A, unsigned int *res);
int ArrayIand  (Array *A,Array *B);
int ArrayIorU  (Array *A, unsigned int *res);
#endif 

