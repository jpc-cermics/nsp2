/* -*- Mode: C -*- */
#ifndef NSP_INC_NspDiagram
#define NSP_INC_NspDiagram

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#line 4 "codegen/diagram.override"
/* inserted at the start of include file */
 #include <nsp/figure.h>

#line 29 "./diagram.h"
/* NspDiagram */

#include <nsp/graphic.h>

/*
 * NspDiagram inherits from Graphic
 */

typedef struct _NspDiagram NspDiagram ;
typedef struct _NspTypeDiagram NspTypeDiagram ;

struct _NspTypeDiagram {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
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

extern NspDiagram *nsp_diagram_create(const char *name,NspMatrix* bounds,NspList* children,NspTypeBase *type);
extern NspDiagram *nsp_diagram_create_default(const char *name);

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

#line 9 "codegen/diagram.override"
typedef enum { MOVE, MOVE_CONTROL } move_action ;
typedef enum _list_move_action list_move_action; 
enum _list_move_action {  L_INVALIDATE,  L_TRANSLATE,  L_LOCK_UPDATE,  L_LINK_CHECK};

/* inserted at the end of public part of include file */
extern NspGraphic *nsp_get_point_axes(BCG *Xgc,int px,int py,double *dp);
extern NspDiagram *nsp_figure_get_axe_elts_as_diagram(char *name,NspFigure *F);
/* extern NspDiagram *diagram_object(NspObject *O);  */
extern int IsDiagramObj (Stack stack, int i); 
extern NspDiagram *GetDiagramCopy (Stack stack, int i); 
extern NspDiagram *GetDiagram (Stack stack, int i); 
extern int IsDiagram (NspObject *O); 
extern int nsp_diagram_select_obj (NspDiagram *R, const double *pt, NspObject **Objs,NspObject *exclude); 
extern int nsp_diagram_select_lock(NspDiagram *F,double pt[2], NspObject **O, int *cp, int *lock_c) ;
extern int nsp_diagram_select_and_move (NspDiagram *R, const double *pt, int mask); 
extern int nsp_diagram_select_and_hilite(NspDiagram *R,const double pt[2]);
extern int nsp_diagram_select_and_toggle_hilite(NspDiagram *R,const double pt[2]);
extern int nsp_diagram_move_obj (NspDiagram *R,NspObject *O, const double *pt, int stop, int cp,move_action  action ); 
extern void nsp_diagram_unhilite_objs (NspDiagram *R, int draw); 
extern void nsp_diagram_delete_hilited (NspDiagram *R); 
extern NspObject* nsp_diagram_create_new_block(NspDiagram *R,const double pt[2],int mode);
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
extern NspDiagram *nsp_diagram_full_copy( NspDiagram *F);
extern NspDiagram *nsp_diagram_from_nspdiagram(char *name,BCG *Xgc, nsp_diagram *gf);
extern void nsp_diagram_destroy (NspDiagram *H); 
extern NspList *nsp_diagram_get_hilited_list(nsp_diagram *gf, int full_copy);
extern int nsp_diagram_select_and_move_list(NspDiagram *R,NspObject *Obj,const double pt[2], int *click);
extern int nsp_diagram_move_list_obj(NspDiagram *F,NspList *L,const double pt[2],int stop,int cp,
				     move_action action, int *click);
extern NspDiagram *nsp_diagram_hilited_full_copy(const char *name, NspDiagram *F);
extern void link_lock_update(NspDiagram *F, NspLink *L,int lp,double ptnew[2]);
extern void link_check(NspDiagram *F,NspLink *L);
extern int link_split(NspDiagram *F,NspLink *L,NspLink **L1,const double pt[2]);
extern int link_split(NspDiagram *F,NspLink *L,NspLink **L1,const double pt[2]);

extern NspDiagram *nsp_diagram_from_gridblock(char *name,void *);/* NspGridBlock *B); */

#line 154 "./diagram.h"
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
static NspDiagram *nsp_diagram_create_void(const char *name,NspTypeBase *type);
#line 59 "codegen/diagram.override"

/* inserted in the private part of include file */

static void nsp_draw_diagram(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_diagram(NspGraphic *o,const double *tr);
static void nsp_rotate_diagram(NspGraphic *o,double *R);
static void nsp_scale_diagram(NspGraphic *o,double *alpha);
static int nsp_getbounds_diagram(NspGraphic *o,double *bounds);
static void nsp_diagram_compute_inside_bounds(NspGraphic *Obj);
static void nsp_diagram_link_figure(NspGraphic *G, void *F, void *A);
static void nsp_diagram_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_diagram_children(NspGraphic *Obj);
static int nsp_diagram_list_obj_action(NspDiagram *F,NspList *L,const double pt[2],
				       list_move_action action);
static void nspdiagram_recompute_pointers(nsp_diagram *gf);
static void nsp_diagram_list_recompute_pointers(NspList *L);

static int init_diagram(NspDiagram *ob,NspTypeDiagram *type);
static int nsp_diagram_size(NspDiagram *Mat, int flag);
static char *nsp_diagram_type_as_string(void);
static char *nsp_diagram_type_short_string(NspObject *v);
static int nsp_diagram_eq(NspDiagram *A, NspObject *B);
static int nsp_diagram_neq(NspDiagram *A, NspObject *B);
static NspDiagram  *nsp_diagram_xdr_load(XDR  *F);
static AttrTab diagram_attrs[];
static void *nspdiagram_get_adress(NspList *L,void *old );
static NspList * nsp_diagram_list_full_copy_and_name(NspList *L,int hilited_only);

#line 198 "./diagram.h"
#endif /* NspDiagram_Private */

