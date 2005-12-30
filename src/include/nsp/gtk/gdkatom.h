/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkAtom
#define INC_NSP_GdkAtom

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* GdkAtom */

#include "nsp/object.h"

/*
 * NspGdkAtom inherits from NspObject
 */

typedef struct _nsp_gdkatom NspGdkAtom;

typedef int (*gdkatom_save) (NspFile  *F, NspGdkAtom *M);

typedef struct _nsp_type_GdkAtom { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeGdkAtom;

struct _nsp_gdkatom {
  NspObject father; 
  NspTypeGdkAtom *type; 
  gchar *name; /* unused */
  GdkAtom atom;
};

extern int nsp_type_gdkatom_id;
extern NspTypeGdkAtom *nsp_type_gdkatom;

/* type instances for classa */

NspTypeGdkAtom *new_type_gdkatom(type_mode mode);

/* instance for GdkAtom */

NspGdkAtom *new_gdkatom();

/*
 * Object methods redefined for gdkatom 
 */

#ifdef GdkAtom_Private 
static int init_gdkatom(NspGdkAtom *o,NspTypeGdkAtom *type);
static int gdkatom_size(NspGdkAtom *Mat, int flag);
static char *gdkatom_type_as_string(void);
static char *gdkatom_type_short_string(void);
static int gdkatom_eq(NspGdkAtom *A, NspObject *B);
static int gdkatom_neq(NspGdkAtom *A, NspObject *B);
static int gdkatom_xdr_save(XDR *xdrs, NspGdkAtom *M);
static NspGdkAtom  *gdkatom_xdr_load(XDR *xdrs);
static NspMethods *gdkatom_get_methods(void); 
static NspObject *gdkatom_path_extract(NspGdkAtom *A, NspObject *O);
#endif /* GdkAtom_Private */

#define NULLGDKATOM (NspGdkAtom*) 0

NspGdkAtom *gdkatom_create(char *name,char *aname,GdkAtom atom,NspTypeBase *type);
NspGdkAtom *gdkatom_copy(NspGdkAtom *H);
void gdkatom_destroy(NspGdkAtom *H);
void gdkatom_info(NspGdkAtom *H, int indent,char *name, int rec_level);
void gdkatom_print(NspGdkAtom *H, int indent,char *name, int rec_level);

/* from GdkAtomObj.c */

extern NspGdkAtom *gdkatom_object (NspObject *O); 
extern int IsGdkAtomObj (Stack stack, int i); 
extern int IsGdkAtom(NspObject *O);
extern NspGdkAtom *GetGdkAtomCopy (Stack stack, int i); 
extern NspGdkAtom *GetGdkAtom (Stack stack, int i); 


extern GdkAtom nsp_gdkatom_get(NspObject *object); 
extern int nsp_gdk_atom_from_object(NspObject *object, GdkAtom *atom);

#endif 

