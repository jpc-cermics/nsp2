/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoAttribute
#define INC_NSP_PangoAttribute

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* PangoAttribute */

#include "nsp/object.h"

/*
 * NspPangoAttribute inherits from NspObject
 */

typedef struct _nsp_pangoattribute NspPangoAttribute;

typedef int (*pangoattribute_save) (NspFile  *F, NspPangoAttribute *M);

typedef struct _nsp_type_PangoAttribute { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypePangoAttribute;

struct _nsp_pangoattribute {
  NspObject father; 
  NspTypePangoAttribute *type; 
  PangoAttribute *attr;
};

extern int nsp_type_pangoattribute_id;
extern NspTypePangoAttribute *nsp_type_pangoattribute;

/* type instances for classa */

NspTypePangoAttribute *new_type_pangoattribute(type_mode mode);

/* instance for PangoAttribute */

NspPangoAttribute *new_pangoattribute();

/*
 * Object methods redefined for pangoattribute 
 */

#ifdef PangoAttribute_Private 
static int init_pangoattribute(NspPangoAttribute *o,NspTypePangoAttribute *type);
static int pangoattribute_size(NspPangoAttribute *Mat, int flag);
static char *pangoattribute_type_as_string(void);
static char *pangoattribute_type_short_string(void);
static int pangoattribute_eq(NspPangoAttribute *A, NspObject *B);
static int pangoattribute_neq(NspPangoAttribute *A, NspObject *B);
static int pangoattribute_xdr_save(NspFile  *F, NspPangoAttribute *M);
static NspPangoAttribute  *pangoattribute_xdr_load(NspFile  *F);
static AttrTab pangoattribute_attrs[];
static NspMethods *pangoattribute_get_methods(void); 
static NspObject *pangoattribute_path_extract(NspPangoAttribute *A, NspObject *O);
#endif /* PangoAttribute_Private */

#define NULLPATTR (NspPangoAttribute*) 0

NspPangoAttribute *pangoattribute_create(char *name,  PangoAttribute *attr, NspTypeBase *type);
NspPangoAttribute *pangoattribute_copy(NspPangoAttribute *H);
void pangoattribute_destroy(NspPangoAttribute *H);
void pangoattribute_info(NspPangoAttribute *H, int indent);
void pangoattribute_print(NspPangoAttribute *H, int indent);

/* from PangoAttributeObj.c */

extern NspPangoAttribute *pangoattribute_object (NspObject *O); 
extern int IsPangoAttributeObj (Stack stack, int i); 
extern int IsPangoAttribute(NspObject *O);
extern NspPangoAttribute *GetPangoAttributeCopy (Stack stack, int i); 
extern NspPangoAttribute *GetPangoAttribute (Stack stack, int i); 



#endif 

