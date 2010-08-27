/* -*- Mode: C -*- */
#ifndef NSP_INC_ScalExp
#define NSP_INC_ScalExp

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* ScalExp */
#include "nsp/object.h"

/*
 * NspScalExp inherits from NspObject
 */

/* typedef struct _NspScalExp NspScalExp ; */
typedef struct _NspTypeScalExp NspTypeScalExp ;

struct _NspTypeScalExp {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspScalExp {
  /*< private >*/
  NspObject father;
  NspTypeScalExp*type;
  /*< public >*/
  PList code;
  NspSMatrix *expr;
  NspSMatrix *vars;
  NspSMatrix *extra_vars;
  NspMatrix *bcode;
  NspMatrix *values;
};

extern int nsp_type_scalexp_id;
extern NspTypeScalExp *nsp_type_scalexp;

/* type instances for object */

NspTypeScalExp *new_type_scalexp(type_mode mode);

/* instance for ScalExp */

NspScalExp *new_scalexp();

/*
 * Object methods redefined for scalexp 
 */


#define NULLSCALEXP (NspScalExp*) 0

extern NspScalExp *scalexp_create(char *name,NspSMatrix *expr,NspTypeBase *type);

/* from ScalExpObj.c */

extern NspScalExp *nsp_scalexp_copy(NspScalExp *H);
extern void nsp_scalexp_destroy(NspScalExp *H);
extern int nsp_scalexp_info(NspScalExp *M, int indent,const char *name, int rec_level);
extern int nsp_scalexp_print(NspScalExp *M, int indent,const char *name, int rec_level);
extern NspScalExp *nsp_scalexp_object (NspObject *O); 
extern int IsScalExpObj (Stack stack, int i); 
extern int IsScalExp(NspObject *O);
extern NspScalExp *GetScalExpCopy (Stack stack, int i); 
extern NspScalExp *GetScalExp (Stack stack, int i); 
extern int int_scalexp_create(Stack stack, int rhs, int opt, int lhs);

typedef enum { 
  f_sin, f_cos, f_tan, f_exp, f_log, f_sinh, f_cosh, f_tanh,
  f_int, f_round, f_ceil, f_floor, f_sign, f_abs, f_max, f_min,
  f_asin, f_acos, f_atan, f_asinh, f_acosh, f_atanh,
  f_atan2, f_log10, f_gamma
} f_enum;

typedef struct _expr_func expr_func;

struct _expr_func {
  const char *name;
  f_enum id;
  double (*f1)(double);
  double (*f2)(double,double);
  int logical;
};

extern expr_func expr_functions[];


#endif /* NSP_INC_ScalExp */ 

#ifdef ScalExp_Private 
static int init_scalexp(NspScalExp *o,NspTypeScalExp *type);
static int nsp_scalexp_size(NspScalExp *Mat, int flag);
static char *nsp_scalexp_type_as_string(void);
static char *nsp_scalexp_type_short_string(NspObject *v);
static int nsp_scalexp_eq(NspScalExp *A, NspObject *B);
static int nsp_scalexp_neq(NspScalExp *A, NspObject *B);
static int nsp_scalexp_xdr_save(XDR  *xdrs, NspScalExp *M);
static NspScalExp *nsp_scalexp_xdr_load(XDR *xdrs);
static AttrTab scalexp_attrs[];
static NspMethods *scalexp_get_methods(void);
static NspScalExp *scalexp_create_void(char *name,NspTypeBase *type);
#endif /* ScalExp_Private */

