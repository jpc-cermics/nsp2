/* -*- Mode: C -*- */
#ifndef INC_NSP_GPointer
#define INC_NSP_GPointer

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* GPointer */

#include "nsp/object.h"
#include "gobject.h"
#include <glib.h>
#include <glib-object.h>

/*
 * NspGPointer inherits from NspObject
 */

typedef struct _nsp_gpointer NspGPointer;

typedef int (*gpointer_save) (NspFile  *F, NspGPointer *M);

typedef struct _nsp_type_GPointer { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeGPointer;

struct _nsp_gpointer {
  NspObject father; 
  NspTypeGPointer *type; 
  gpointer pointer;
  GType gtype;
};

extern int nsp_type_gpointer_id;
extern NspTypeGPointer *nsp_type_gpointer;

/* type instances for classa */

NspTypeGPointer *new_type_gpointer(type_mode mode);

/* instance for GPointer */

NspGPointer *new_gpointer();

/*
 * Object methods redefined for gpointer 
 */

#ifdef GPointer_Private 
static int init_gpointer(NspGPointer *o,NspTypeGPointer *type);
static char *gpointer_type_as_string(void);
static char *gpointer_type_short_string(void);
static int gpointer_eq(NspGPointer *A, NspObject *B);
static int gpointer_neq(NspGPointer *A, NspObject *B);
/* static AttrTab *gpointer_get_attrs_table(void); */
static NspMethods *gpointer_get_methods(void); 
#endif /* GPointer_Private */

#define NULLGPOINTER (NspGPointer*) 0

NspGPointer *gpointer_create(char *name,GType gtype, gpointer pointer,NspTypeBase *type);
NspGPointer *gpointer_copy(NspGPointer *H);
void gpointer_destroy(NspGPointer *H);
void gpointer_info(NspGPointer *H, int indent);
void gpointer_print(NspGPointer *H, int indent);

/* from GPointerObj.c */

extern NspGPointer *gpointer_object (NspObject *O); 
extern int IsGPointerObj (Stack stack, int i); 
extern int IsGPointer(NspObject *O);
extern NspGPointer *GetGPointerCopy (Stack stack, int i); 
extern NspGPointer *GetGPointer (Stack stack, int i); 

/* added */

#define nspg_pointer_get(x, type) ((type *) ((NspGPointer *) x)->pointer)

#endif 

