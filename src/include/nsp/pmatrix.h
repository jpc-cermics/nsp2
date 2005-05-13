#ifndef INC_NSP_PMATRIX 
#define INC_NSP_PMATRIX

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspPMatrix inherits from NspObject 
 */

typedef struct _NspPmatrix  NspPMatrix;

typedef int (*pmatrix_save) (NspFile  *F, NspPMatrix *M);

typedef struct _NspTypePMatrix { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  pmatrix_save *save;
} NspTypePMatrix;

typedef NspMatrix Poly;

struct _NspPmatrix {
  NspObject father; 
  NspTypePMatrix *type; 
  char rc_type  ;   /* type 'r' or 'i' */
  char *var  ;   /* name of polynom variable */
  integer m,n,mn;       /* matrix dimension (m,n,m*n) */
  Poly **S;     /* Each polynom is a Matrix **/
};

extern int nsp_type_pmatrix_id;
extern NspTypePMatrix *nsp_type_pmatrix;

int nsp_type_pmatrix_init();

/* only useful when building a new class derived from pmatrix */

NspTypePMatrix *new_type_pmatrix(type_mode mode);

NspPMatrix *new_pmatrix();

/*
 * Object methods redefined for pmatrix 
 */

#ifdef PMatrix_Private 
static int init_pmatrix(NspPMatrix *ob,NspTypePMatrix *type);
int PMatSize(NspPMatrix *Mat, int flag);
char *PMatType(void);
char *PMatShType(void);
NspObject *PMatLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int PMatObjEq(NspObject *A,NspObject *B);
int PMatObjNeq(NspObject *A,NspObject *B);
static int PMatXdrSave(NspFile  *F, NspMatrix *M);
static NspPMatrix *PMatXdrLoad(NspFile  *F);
#endif 


#define NULLPMAT ( NspPMatrix *) 0
#define NULLPOLY ( Poly *) 0

/* PMatObj.c */

extern NspPMatrix *PMatObj (NspObject *O); 

/* NspPMatrix.c */

extern Poly *CopyPoly (Poly *P); 
extern Poly *Basic2Poly (doubleC *d, char type); 
extern void PolyDestroy (Poly *P); 
extern NspPMatrix *Mat2Poly (NspMatrix *M); 
extern void nsp_pmatrix_info(NspPMatrix *Mat, int indent); 
extern void nsp_pmatrix_print(NspPMatrix *Mat, int indent); 
extern NspPMatrix *nsp_pmatrix_create(char *name, integer m, integer n, doubleC *cval, integer flag); 
extern void nsp_pmatrix_destroy(NspPMatrix *A); 
extern NspPMatrix *nsp_pmatrix_copy(NspPMatrix *A); 
extern NspMatrix *PMatLength (NspPMatrix *A); 
extern NspPMatrix *Mat2PMat (NspMatrix *A, char *str, integer flag); 
extern int nsp_pmatrix_redim(NspPMatrix *A, integer m, integer n); 
extern int nsp_pmatrix_resize(NspPMatrix *A, integer m, integer n); 
extern int nsp_pmatrix_enlarge(NspPMatrix *A, integer m, integer n); 
extern int nsp_pmatrix_concat_right(NspPMatrix *A, NspPMatrix *B); 
extern int Pcopy (integer n, Poly **s1, Poly **s2); 
extern int nsp_pmatrix_add_columns(NspPMatrix *A, integer n); 
extern int Pset (integer n, doubleC *s1, Poly **s2); 
extern NspPMatrix *nsp_pmatrix_concat_down(NspPMatrix *A, NspPMatrix *B); 
extern int nsp_pmatrix_add_rows(NspPMatrix *A, integer m); 
extern int PMatSetRC (NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspPMatrix *B); 
extern NspPMatrix *nsp_pmatrix_extract(NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 

#endif 
