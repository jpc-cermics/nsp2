/* -*- Mode: C -*- */
#ifndef INC_NSP_GBoxed
#define INC_NSP_GBoxed

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* GBoxed */

#include "nsp/object.h"
#include "gobject.h"
#include <glib.h>
#include <glib-object.h>

/*
 * NspGBoxed inherits from NspObject
 */

typedef struct _nsp_gboxed NspGBoxed;

typedef int (*gboxed_save) (NspFile  *F, NspGBoxed *M);

typedef struct _nsp_type_GBoxed { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeGBoxed;

struct _nsp_gboxed {
  NspObject father; 
  NspTypeGBoxed *type; 
  gpointer boxed;
  GType gtype;
  gboolean free_on_dealloc;
};

extern int nsp_type_gboxed_id;
extern NspTypeGBoxed *nsp_type_gboxed;

/* type instances for classa */

NspTypeGBoxed *new_type_gboxed(type_mode mode);

/* instance for GBoxed */

NspGBoxed *new_gboxed();

/*
 * Object methods redefined for gboxed 
 */

#ifdef GBoxed_Private 
static int init_gboxed(NspGBoxed *o,NspTypeGBoxed *type);
static char *gboxed_type_as_string(void);
static char *gboxed_type_short_string(NspObject *v);
static int gboxed_eq(NspGBoxed *A, NspObject *B);
static int gboxed_neq(NspGBoxed *A, NspObject *B);
static NspMethods *gboxed_get_methods(void); 
#endif /* GBoxed_Private */

#define NULLGBOXED (NspGBoxed*) 0

NspGBoxed *gboxed_create(char *name,GType boxed_type, gpointer boxed, gboolean copy_boxed,
			 gboolean own_ref, /* (NspTypeBase *) */ void *  type );

NspGBoxed *gboxed_copy(NspGBoxed *H);
void gboxed_destroy(NspGBoxed *H);
void gboxed_info(NspGBoxed *H, int indent,char *name, int rec_level);
void gboxed_print(NspGBoxed *H, int indent,char *name, int rec_level);

/* from GBoxedObj.c */

extern NspGBoxed *gboxed_object (NspObject *O); 
extern int IsGBoxedObj (Stack stack, int i); 
extern int IsGBoxed(NspObject *O);
extern NspGBoxed *GetGBoxedCopy (Stack stack, int i); 
extern NspGBoxed *GetGBoxed (Stack stack, int i); 

/* added */

#define nspg_boxed_get(x, type) ((type *) ((NspGBoxed *) x)->boxed)
#define NSP_GBOXED_GET(x, type) ((type *) ((NspGBoxed *) x)->boxed)

extern int nspg_boxed_check(NspObject *self,GType boxed_type) ; 

#endif 

