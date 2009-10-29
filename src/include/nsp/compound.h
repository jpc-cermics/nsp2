/* -*- Mode: C -*- */
#ifndef NSP_INC_NspCompound
#define NSP_INC_NspCompound

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspCompound */

#include <nsp/graphic.h>

/*
 * NspCompound inherits from Graphic
 */

typedef struct _NspCompound NspCompound ;
typedef struct _NspTypeCompound NspTypeCompound ;

#line 22 "./compound.h"

struct _NspTypeCompound {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./compound.h"
};

typedef struct _nsp_compound nsp_compound;
struct _nsp_compound {
  NspMatrix* bounds;
  NspList* children;
  int ref_count;
};

struct _NspCompound {
  /*< private >*/
  NspGraphic father;
  NspTypeCompound*type;
  /*< public >*/
  nsp_compound *obj;
};

extern int nsp_type_compound_id;
extern NspTypeCompound *nsp_type_compound;

/* type instances for graphic */

NspTypeCompound *new_type_compound(type_mode mode);

/* instance for NspCompound */

NspCompound *new_compound();

/*
 * Object methods redefined for compound 
 */


#define NULLCOMPOUND (NspCompound*) 0

extern NspCompound *nsp_compound_create(char *name,NspMatrix* bounds,NspList* children,NspTypeBase *type);
extern NspCompound *nsp_compound_create_default(char *name);

/* from NspCompoundObj.c */

extern NspCompound *nsp_compound_copy(NspCompound *H);
extern void nsp_compound_destroy(NspCompound *H);
extern int nsp_compound_info(NspCompound *H, int indent,const char *name, int rec_level);
extern int nsp_compound_print(NspCompound *H, int indent,const char *name, int rec_level);
extern int nsp_compound_latex(NspCompound *H, int indent,const char *name, int rec_level);
extern NspCompound *nsp_compound_object (NspObject *O);
extern int IsCompoundObj (Stack stack, int i);
extern int IsCompound(NspObject *O);
extern NspCompound *GetCompoundCopy (Stack stack, int i);
extern NspCompound *GetCompound (Stack stack, int i);
extern int nsp_compound_create_partial(NspCompound *H);
extern void nsp_compound_destroy_partial(NspCompound *H);
extern NspCompound * nsp_compound_copy_partial(NspCompound *H,NspCompound *self);
extern NspCompound * nsp_compound_full_copy_partial(NspCompound *H,NspCompound *self);
extern NspCompound * nsp_compound_full_copy(NspCompound *self);
extern int nsp_compound_check_values(NspCompound *H);
extern int int_compound_create(Stack stack, int rhs, int opt, int lhs);
extern NspCompound *nsp_compound_xdr_load_partial(XDR *xdrs, NspCompound *M);
extern int nsp_compound_xdr_save(XDR  *xdrs, NspCompound *M);

#line 4 "codegen/compound.override"

/* inserted at the end of public part of include file */
extern NspCompound *nsp_figure_get_axe_elts_as_compound(char *name,NspFigure *F);

#line 95 "./compound.h"
#endif /* NSP_INC_NspCompound */ 

#ifdef NspCompound_Private 
static int init_compound(NspCompound *o,NspTypeCompound *type);
static int nsp_compound_size(NspCompound *Mat, int flag);
static char *nsp_compound_type_as_string(void);
static char *nsp_compound_type_short_string(NspObject *v);
static int nsp_compound_eq(NspCompound *A, NspObject *B);
static int nsp_compound_neq(NspCompound *A, NspObject *B);
static NspCompound *nsp_compound_xdr_load(XDR *xdrs);
static AttrTab compound_attrs[];
static NspMethods *compound_get_methods(void);
/* static int int_compound_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspCompound *nsp_compound_create_void(char *name,NspTypeBase *type);
#line 10 "codegen/compound.override"

/* inserted in the private part of include file */

static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_compound(NspGraphic *o,const double *tr);
static void nsp_rotate_compound(NspGraphic *o,double *R);
static void nsp_scale_compound(NspGraphic *o,double *alpha);
static int nsp_getbounds_compound(NspGraphic *o,double *bounds);
static void nsp_compound_compute_inside_bounds(NspGraphic *Obj);
static void nsp_compound_link_figure(NspGraphic *G, void *F);
static void nsp_compound_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_compound_children(NspGraphic *Obj);

#line 124 "./compound.h"
#endif /* NspCompound_Private */

