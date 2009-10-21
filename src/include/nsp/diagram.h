/* -*- Mode: C -*- */
#ifndef NSP_INC_NspDiagram
#define NSP_INC_NspDiagram

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspDiagram */

#include <nsp/graphic.h>

/*
 * NspDiagram inherits from Graphic
 */

typedef struct _NspDiagram NspDiagram ;
typedef struct _NspTypeDiagram NspTypeDiagram ;

#line 22 "./diagram.h"

struct _NspTypeDiagram {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./diagram.h"
};

typedef struct _nsp_diagram nsp_diagram;
struct _nsp_diagram {
  NspMatrix* bounds;
  NspList* children;
  int ref_count;
};

struct _NspDiagram {
  /*< private >*/
  NspGraphic father;
  NspTypeDiagram*type;
  /*< public >*/
  nsp_diagram *obj;
};

extern int nsp_type_diagram_id;
extern NspTypeDiagram *nsp_type_diagram;

/* type instances for graphic */

NspTypeDiagram *new_type_diagram(type_mode mode);

/* instance for NspDiagram */

NspDiagram *new_diagram();

/*
 * Object methods redefined for diagram 
 */


#define NULLDIAGRAM (NspDiagram*) 0

extern NspDiagram *nsp_diagram_create(char *name,NspMatrix* bounds,NspList* children,NspTypeBase *type);
extern NspDiagram *nsp_diagram_create_default(char *name);

/* from NspDiagramObj.c */

extern NspDiagram *nsp_diagram_copy(NspDiagram *H);
extern void nsp_diagram_destroy(NspDiagram *H);
extern int nsp_diagram_info(NspDiagram *H, int indent,const char *name, int rec_level);
extern int nsp_diagram_print(NspDiagram *H, int indent,const char *name, int rec_level);
extern int nsp_diagram_latex(NspDiagram *H, int indent,const char *name, int rec_level);
extern NspDiagram *nsp_diagram_object (NspObject *O);
extern int IsDiagramObj (Stack stack, int i);
extern int IsDiagram(NspObject *O);
extern NspDiagram *GetDiagramCopy (Stack stack, int i);
extern NspDiagram *GetDiagram (Stack stack, int i);
extern int nsp_diagram_create_partial(NspDiagram *H);
extern void nsp_diagram_destroy_partial(NspDiagram *H);
extern NspDiagram * nsp_diagram_copy_partial(NspDiagram *H,NspDiagram *self);
extern NspDiagram * nsp_diagram_full_copy_partial(NspDiagram *H,NspDiagram *self);
extern NspDiagram * nsp_diagram_full_copy(NspDiagram *self);
extern int nsp_diagram_check_values(NspDiagram *H);
extern int int_diagram_create(Stack stack, int rhs, int opt, int lhs);
extern NspDiagram *nsp_diagram_xdr_load_partial(XDR *xdrs, NspDiagram *M);
extern int nsp_diagram_xdr_save(XDR  *xdrs, NspDiagram *M);

#line 4 "codegen/diagram.override"

/* inserted at the end of public part of include file */
extern NspDiagram *nsp_figure_get_axe_elts_as_diagram(char *name,NspFigure *F);

#line 95 "./diagram.h"
#endif /* NSP_INC_NspDiagram */ 

#ifdef NspDiagram_Private 
static int init_diagram(NspDiagram *o,NspTypeDiagram *type);
static int nsp_diagram_size(NspDiagram *Mat, int flag);
static char *nsp_diagram_type_as_string(void);
static char *nsp_diagram_type_short_string(NspObject *v);
static int nsp_diagram_eq(NspDiagram *A, NspObject *B);
static int nsp_diagram_neq(NspDiagram *A, NspObject *B);
static NspDiagram *nsp_diagram_xdr_load(XDR *xdrs);
static AttrTab diagram_attrs[];
static NspMethods *diagram_get_methods(void);
/* static int int_diagram_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspDiagram *nsp_diagram_create_void(char *name,NspTypeBase *type);
#line 10 "codegen/diagram.override"

/* inserted in the private part of include file */

static void nsp_draw_diagram(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_diagram(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_diagram(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_diagram(BCG *Xgc,NspGraphic *o,double *alpha);
static int nsp_getbounds_diagram(BCG *Xgc,NspGraphic *o,double *bounds);
static void nsp_diagram_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj);
static void nsp_diagram_link_figure(NspGraphic *G, void *F);
static void nsp_diagram_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_diagram_children(NspGraphic *Obj);

#line 124 "./diagram.h"
#endif /* NspDiagram_Private */

