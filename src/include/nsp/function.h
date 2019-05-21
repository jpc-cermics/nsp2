#ifndef NSP_INC_Function
#define NSP_INC_Function

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/**
 * NspFunction:
 * @fname: function name 
 * @pos: position inside interface,
 * @iface: interface number in which the function is stored,
 * @status: status of the function 
 *
 * inherits from #NspObject used for storing information about 
 * a primitive. 
 */

#include <nsp/objectf.h>

/*
 * NspFunction inherits from NspObject
 */

/* typedef struct _NspFunction NspFunction; */

typedef struct _NspTypeFunction { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeFunction;

struct _NspFunction {
  /*< private >*/
  NspObject father; 
  NspTypeFunction *type; 
  /*< public >*/
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

extern NspFunction *function_create(const char *name,const char *fname,int iface,int pos,int status,NspTypeBase *type);
extern NspFunction *function_copy(NspFunction *H);
extern void function_destroy(NspFunction *H);
extern int function_info(NspFunction *H, int indent,const char *name, int rec_level);
extern int function_print(NspFunction *H, int indent,const char *name, int rec_level);

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
static char *function_type_short_string(NspObject *v);
static int function_eq(NspFunction *A, NspObject *B);
static int function_neq(NspFunction *A, NspObject *B);
static int function_xdr_save(XDR  *F, NspFunction *M);
static NspFunction  *function_xdr_load(XDR  *F);
/* static AttrTab function_attrs[]; */
static NspMethods *function_get_methods(void); 
#endif /* Function_Private */
