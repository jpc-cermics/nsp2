/* -*- Mode: C -*- */
#ifndef INC_NSP_ClassA
#define INC_NSP_ClassA

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* ClassA */

#include "nsp/object.h"

/*
 * NspClassA inherits from NspObject
 */

typedef struct _nsp_classa NspClassA;

typedef int (*classa_save) (NspSciFile  *F, NspClassA *M);

typedef struct _nsp_type_ClassA { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeClassA;

struct _nsp_classa {
  NspObject father; 
  NspTypeClassA *type; 
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

#ifdef ClassA_Private 
static int init_classa(NspClassA *o,NspTypeClassA *type);
static int classa_size(NspClassA *Mat, int flag);
static char *classa_type_as_string(void);
static char *classa_type_short_string(void);
static int classa_eq(NspClassA *A, NspObject *B);
static int classa_neq(NspClassA *A, NspObject *B);
static int classa_xdr_save(NspSciFile  *F, NspClassA *M);
static NspClassA  *classa_xdr_load(NspSciFile  *F);
static AttrTab *classa_get_attrs_table(void);
static NspMethods *classa_get_methods(void); 
static NspObject *classa_path_extract(NspClassA *A, NspObject *O);
#endif /* ClassA_Private */

#define NULLCLA (NspClassA*) 0

NspClassA *classa_create(char *name,int color,int thickness,NspTypeBase *type);
NspClassA *classa_copy(NspClassA *H);
void classa_destroy(NspClassA *H);
void classa_info(NspClassA *H, int indent);
void classa_print(NspClassA *H, int indent);

/* from ClassAObj.c */

extern NspClassA *classa_object (NspObject *O); 
extern int IsClassAObj (Stack stack, int i); 
extern int IsClassA(NspObject *O);
extern NspClassA *GetClassACopy (Stack stack, int i); 
extern NspClassA *GetClassA (Stack stack, int i); 



#endif 

