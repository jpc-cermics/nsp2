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
typedef enum { MOVE, MOVE_CONTROL } move_action ;
typedef enum _list_move_action list_move_action; 
enum _list_move_action {  L_DRAW,  L_TRANSLATE,  L_LOCK_UPDATE,  L_LINK_CHECK};

/* inserted at the end of public part of include file */
extern NspDiagram *nsp_figure_get_axe_elts_as_diagram(char *name,NspFigure *F);
/* extern NspDiagram *diagram_object(NspObject *O);  */
extern int IsDiagramObj (Stack stack, int i); 
extern NspDiagram *GetDiagramCopy (Stack stack, int i); 
extern NspDiagram *GetDiagram (Stack stack, int i); 
extern int IsDiagram (NspObject *O); 
extern void nsp_diagram_draw (NspDiagram *R); 
extern int nsp_diagram_select_obj (NspDiagram *R, const double *pt, NspObject **Objs,NspObject *exclude); 
extern int nsp_diagram_select_lock(NspDiagram *F,double pt[2], NspObject **O, int *cp, int *lock_c) ;
extern int nsp_diagram_select_and_move (NspDiagram *R, const double *pt, int mask); 
extern int nsp_diagram_select_and_hilite(NspDiagram *R,const double pt[2]);
extern int nsp_diagram_select_and_toggle_hilite(NspDiagram *R,const double pt[2]);
extern int nsp_diagram_move_obj (NspDiagram *R,NspObject *O, const double *pt, int stop, int cp,move_action  action ); 
extern void nsp_diagram_unhilite_objs (NspDiagram *R, int draw); 
extern void nsp_diagram_delete_hilited (NspDiagram *R); 
extern NspObject* nsp_diagram_create_new_block(NspDiagram *R);
extern NspObject* nsp_diagram_create_new_connector(NspDiagram *R);
extern NspObject* nsp_diagram_create_new_link(NspDiagram *F);
extern NspObject * nsp_diagram_create_new_gridblock(NspDiagram *F, int flag);
extern int nsp_diagram_create_new_rect(NspDiagram *F);
extern int  nsp_diagram_hilite_near_pt(NspDiagram *R,const double pt[2]);

extern void nsp_diagram_locks_update(NspDiagram *R,NspObject *O);
extern int nsp_diagram_select_and_split(NspDiagram *R,const double pt[2]);
extern int nsp_diagram_select_link_and_add_control(NspDiagram *R,const double pt[2]);
extern int nsp_diagram_select_link_and_remove_control(NspDiagram *R,const double pt[2]);

extern NspObject * nsp_diagram_get_hilited(NspDiagram *R) ;
extern void nsp_diagram_set_frame_field(NspDiagram *F);
extern NspDiagram *nsp_diagram_full_copy( NspDiagram *F);
extern NspDiagram *nsp_diagram_from_nspdiagram(char *name,BCG *Xgc, nsp_diagram *gf);
extern void nsp_diagram_destroy (NspDiagram *H); 
extern NspList *nsp_diagram_get_hilited_list(nsp_diagram *gf, int full_copy);
extern int nsp_diagram_select_and_move_list(NspDiagram *R,NspObject *Obj,const double pt[2], int *click);
extern int nsp_diagram_move_list_obj(NspDiagram *F,NspList *L,const double pt[2],int stop,int cp,
				     move_action action, int *click);
extern NspDiagram *nsp_diagram_hilited_full_copy( NspDiagram *F);

extern int link_split(NspDiagram *F,NspLink *L,NspLink **L1,const double pt[2]);
extern int link_add_control(NspLink *L,const double pt[2]);
extern int link_remove_control(NspLink *L,const double pt[2]);
extern void link_check(NspDiagram *F,NspLink *L);
extern NspLink *link_create_n(char *name,int n,int color,int thickness);
extern void link_lock_update(NspDiagram *F, NspLink *L,int lp,double ptnew[2]);

#line 141 "./diagram.h"
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
#line 56 "codegen/diagram.override"

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
static int nsp_diagram_list_obj_action(NspDiagram *F,NspList *L,const double pt[2],list_move_action action);
static void nspdiagram_recompute_pointers(nsp_diagram *gf);
static void nsp_diagram_list_recompute_pointers(NspList *L);

static int init_diagram(NspDiagram *ob,NspTypeDiagram *type);
static int nsp_diagram_size(NspDiagram *Mat, int flag);
static char *nsp_diagram_type_as_string(void);
static char *nsp_diagram_type_short_string(NspObject *v);
static int nsp_diagram_eq(NspDiagram *A, NspObject *B);
static int nsp_diagram_neq(NspDiagram *A, NspObject *B);
static NspDiagram  *nsp_diagram_xdr_load(XDR  *F);
static AttrTab nsp_diagram_attrs[];
/* static void nspdiagram_recompute_pointers(nsp_diagram *gf); */
static void nsp_diagram_list_recompute_pointers(NspList *L);
static void nspdiagram_set_frame_field(nsp_diagram *gf);
static void *nspdiagram_get_adress(NspList *L,void *old );
static nsp_diagram *nspdiagram_full_copy(nsp_diagram *gf,int hilited_only);

#line 187 "./diagram.h"
#endif /* NspDiagram_Private */

