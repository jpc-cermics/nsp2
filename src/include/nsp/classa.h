/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassA
#define NSP_INC_NspClassA

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspClassA */

#include <nsp/object.h>

/*
 * NspClassA inherits from Object
 */

typedef struct _NspClassA NspClassA ;
typedef struct _NspTypeClassA NspTypeClassA ;

#line 22 "./classa.h"

struct _NspTypeClassA {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./classa.h"
};

struct _NspClassA {
  /*< private >*/
  NspObject father;
  NspTypeClassA*type;
  /*< public >*/
    int cla_color;
  int cla_thickness;
  NspMatrix* cla_val;
  NspBMatrix* cla_bval;
  NspList* cla_lval;
};

extern int nsp_type_classa_id;
extern NspTypeClassA *nsp_type_classa;

/* type instances for object */

NspTypeClassA *new_type_classa(type_mode mode);

/* instance for NspClassA */

NspClassA *new_classa();

/*
 * Object methods redefined for classa 
 */


#define NULLCLASSA (NspClassA*) 0

extern NspClassA *nsp_classa_create(char *name,int cla_color,int cla_thickness,NspMatrix* cla_val,NspBMatrix* cla_bval,NspList* cla_lval,NspTypeBase *type);
extern NspClassA *nsp_classa_create_default(char *name);

/* from NspClassAObj.c */

extern NspClassA *nsp_classa_copy(NspClassA *H);
extern void nsp_classa_destroy(NspClassA *H);
extern int nsp_classa_info(NspClassA *H, int indent,const char *name, int rec_level);
extern int nsp_classa_print(NspClassA *H, int indent,const char *name, int rec_level);
extern int nsp_classa_latex(NspClassA *H, int indent,const char *name, int rec_level);
extern NspClassA *nsp_classa_object (NspObject *O);
extern int IsClassAObj (Stack stack, int i);
extern int IsClassA(NspObject *O);
extern NspClassA *GetClassACopy (Stack stack, int i);
extern NspClassA *GetClassA (Stack stack, int i);
extern int nsp_classa_create_partial(NspClassA *H);
extern void nsp_classa_destroy_partial(NspClassA *H);
extern NspClassA * nsp_classa_copy_partial(NspClassA *H,NspClassA *self);
extern NspClassA * nsp_classa_full_copy_partial(NspClassA *H,NspClassA *self);
extern NspClassA * nsp_classa_full_copy(NspClassA *self);
extern int nsp_classa_check_values(NspClassA *H);
extern int int_classa_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassA *nsp_classa_xdr_load_partial(XDR *xdrs, NspClassA *M);
extern int nsp_classa_xdr_save(XDR  *xdrs, NspClassA *M);

#line 4 "codegen/classa.override"

/* inserted at the end of public part of include file
 * of classa.h
 */

#line 93 "./classa.h"
#endif /* NSP_INC_NspClassA */ 

#ifdef NspClassA_Private 
static int init_classa(NspClassA *o,NspTypeClassA *type);
static int nsp_classa_size(NspClassA *Mat, int flag);
static char *nsp_classa_type_as_string(void);
static char *nsp_classa_type_short_string(NspObject *v);
static int nsp_classa_eq(NspClassA *A, NspObject *B);
static int nsp_classa_neq(NspClassA *A, NspObject *B);
static NspClassA *nsp_classa_xdr_load(XDR *xdrs);
static AttrTab classa_attrs[];
static NspMethods *classa_get_methods(void);
/* static int int_classa_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassA *nsp_classa_create_void(char *name,NspTypeBase *type);
#line 11 "codegen/classa.override"

/* inserted in the private part of include file
 * of classa.h
 */

#line 114 "./classa.h"
#endif /* NspClassA_Private */

