/* -*- Mode: C -*- */
#ifndef INC_NSP_Function
#define INC_NSP_Function

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2004 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* Function */

#include "nsp/object.h"

/*
 * NspFunction inherits from NspObject
 */

typedef struct _nsp_function NspFunction;

typedef int (*function_save) (NspFile  *F, NspFunction *M);

typedef struct _nsp_type_Function { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeFunction;

struct _nsp_function {
  NspObject father; 
  NspTypeFunction *type; 
  char *fname; /* function name */
  int  pos;/* function position inside interface */
  int  iface;/* Interface number in which the function is stored */
  int  status;/* status of the function */
};

extern int nsp_type_function_id;
extern NspTypeFunction *nsp_type_function;

/* type instances for classa */

NspTypeFunction *new_type_function(type_mode mode);

/* instance for Function */

NspFunction *new_function();

/*
 * Object methods redefined for function 
 */

#define NULLFUNC (NspFunction*) 0

NspFunction *function_create(char *name,char *fname,int iface,int pos,int status,NspTypeBase *type);
NspFunction *function_copy(NspFunction *H);
void function_destroy(NspFunction *H);
void function_info(NspFunction *H, int indent);
void function_print(NspFunction *H, int indent);

/* from FunctionObj.c */

extern NspFunction *function_object (NspObject *O); 
extern int IsFunctionObj (Stack stack, int i); 
extern int IsFunction(NspObject *O);
extern NspFunction *GetFunctionCopy (Stack stack, int i); 
extern NspFunction *GetFunction (Stack stack, int i); 

#endif 

/* private part */

#ifdef Function_Private 
static int init_function(NspFunction *o,NspTypeFunction *type);
static int function_size(NspFunction *Mat, int flag);
static char *function_type_as_string(void);
static char *function_type_short_string(void);
static int function_eq(NspFunction *A, NspObject *B);
static int function_neq(NspFunction *A, NspObject *B);
static int function_xdr_save(NspFile  *F, NspFunction *M);
static NspFunction  *function_xdr_load(NspFile  *F);
/* static AttrTab function_attrs[]; */
static NspMethods *function_get_methods(void); 
#endif /* Function_Private */
