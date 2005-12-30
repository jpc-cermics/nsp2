/* -*- Mode: C -*- */
#ifndef NSP_INC_ClassA
#define NSP_INC_ClassA

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* ClassA */

#include "nsp/object.h"

/*
 * NspClassA inherits from NspObject
 */

typedef struct _NspClassA NspClassA;
typedef struct _NspTypeClassA NspTypeClassA;

typedef int (*classa_save) (NspFile  *F, NspClassA *M);

struct _NspTypeClassA { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

struct _NspClassA {
  /*< private >*/
  NspObject father; 
  NspTypeClassA *type; 
  /*< public >*/
  int classa_color;
  int classa_thickness;
  NspMatrix *classa_val;
};

extern int nsp_type_classa_id;
extern NspTypeClassA *nsp_type_classa;

/* type instances for classa */

NspTypeClassA *new_type_classa(type_mode mode);

/* instance for ClassA */

NspClassA *new_classa();

/*
 * Object methods redefined for classa 
 */

#define NULLCLA (NspClassA*) 0

NspClassA *nsp_classa_create(char *name,int color,int thickness,NspTypeBase *type);
NspClassA *nsp_classa_copy(NspClassA *H);
void nsp_classa_destroy(NspClassA *H);
void nsp_classa_info(NspClassA *H, int indent);
void nsp_classa_print(NspClassA *H, int indent,char *name, int rec_level);

/* from ClassAObj.c */

extern NspClassA *nsp_classa_object (NspObject *O); 
extern int IsClassAObj (Stack stack, int i); 
extern int IsClassA(NspObject *O);
extern NspClassA *GetClassACopy (Stack stack, int i); 
extern NspClassA *GetClassA (Stack stack, int i); 

#endif 

/* private part */

#ifdef ClassA_Private 
static int init_classa(NspClassA *o,NspTypeClassA *type);
static int nsp_classa_size(NspClassA *Mat, int flag);
static char *nsp_classa_type_as_string(void);
static char *nsp_classa_type_short_string(void);
static int nsp_classa_eq(NspClassA *A, NspObject *B);
static int nsp_classa_neq(NspClassA *A, NspObject *B);
static int nsp_classa_xdr_save(NspFile  *F, NspClassA *M);
static NspClassA  *nsp_classa_xdr_load(NspFile  *F);
static AttrTab classa_attrs[];
static NspMethods *classa_get_methods(void); 
static int int_cla_create(Stack stack, int rhs, int opt, int lhs);
#endif /* ClassA_Private */
