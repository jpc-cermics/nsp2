#ifndef INC_NSP_FUNCTION 
#define INC_NSP_FUNCTION

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspFunction inherits from NspObject 
 */

typedef struct _nsp_function  NspFunction;

typedef int (*function_save) (NspFile  *F, NspFunction *M);

typedef struct _nsp_type_Function { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  function_save *save;
} NspTypeFunction;

struct _nsp_function {
  NspObject father; 
  NspTypeFunction *type; 
  int  Num;      /* function position inside interface */
  int  Int;      /* Interface number in which the function is stored */
  int  status;   /* status of the function,.... */
};

extern int nsp_type_function_id;
extern NspTypeFunction *nsp_type_function;

/* only useful when building a new class derived from function */

NspTypeFunction *new_type_function(type_mode mode);

NspFunction *new_function();

/*
 * Object methods redefined for function 
 */

#ifdef Function_Private 
static int init_function(NspFunction *ob,NspTypeFunction *type);
static int FuncSize(NspFunction *Mat, int flag);
char *FuncType(void);
char *FuncShType(NspFunction *M);
NspObject *FuncLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int FuncObjEq(NspObject *A,NspObject *B);
int FuncObjNeq(NspObject *A,NspObject *B);
#endif 

#define NULLFUNC (NspFunction *) 0

extern NspFunction *FuncObj (NspObject *O); 
extern NspFunction *GetFuncCopy (Stack stack, int i); 
extern NspFunction *GetFunc (Stack stack, int i); 
extern NspFunction *FuncCreate (char *name); 
extern NspFunction *FuncCopy (NspFunction *H); 
extern void FuncDestroy (NspFunction *H); 
extern void FuncInfo (NspFunction *H, int indent); 
extern void FuncPrint (NspFunction *H, int indent); 

#endif

