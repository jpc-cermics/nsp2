/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPoints3d
#define NSP_INC_NspPoints3d

/*
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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

#line 4 "codegen/points3d.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./points3d.h"
/* NspPoints3d */

#include <nsp/graphic.h>

/*
 * NspPoints3d inherits from Graphic
 */

typedef struct _NspPoints3d NspPoints3d ;
typedef struct _NspTypePoints3d NspTypePoints3d ;

#line 41 "./points3d.h"

struct _NspTypePoints3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 48 "./points3d.h"
};

typedef struct _nsp_points3d nsp_points3d;
struct _nsp_points3d {
  NspMatrix* Mcoord;
  void* Mcoord_l;
  int color;
  int mark_type;
  int mark_size;
  int* pos;  int pos_length;
  int ref_count;
};

struct _NspPoints3d {
  /*< private >*/
  NspGraphic father;
  NspTypePoints3d*type;
  /*< public >*/
  nsp_points3d *obj;
};

extern int nsp_type_points3d_id;
extern NspTypePoints3d *nsp_type_points3d;

/* type instances for graphic */

NspTypePoints3d *new_type_points3d(type_mode mode);

/* instance for NspPoints3d */

NspPoints3d *new_points3d();

/*
 * Object methods redefined for points3d 
 */


#define NULLPOINTS3D (NspPoints3d*) 0

extern NspPoints3d *nsp_points3d_create(const char *name,NspMatrix* Mcoord,void* Mcoord_l,int color,int mark_type,int mark_size,int* pos, int pos_length,NspTypeBase *type);
extern NspPoints3d *nsp_points3d_create_default(const char *name);

/* from NspPoints3dObj.c */

extern NspPoints3d *nsp_points3d_copy(NspPoints3d *H);
extern void nsp_points3d_destroy(NspPoints3d *H);
extern int nsp_points3d_info(NspPoints3d *H, int indent,const char *name, int rec_level);
extern int nsp_points3d_print(NspPoints3d *H, int indent,const char *name, int rec_level);
extern int nsp_points3d_latex(NspPoints3d *H, int indent,const char *name, int rec_level);
extern NspPoints3d *nsp_points3d_object (NspObject *O);
extern int IsPoints3dObj (Stack stack, int i);
extern int IsPoints3d(NspObject *O);
extern NspPoints3d *GetPoints3dCopy (Stack stack, int i);
extern NspPoints3d *GetPoints3d (Stack stack, int i);
extern int nsp_points3d_create_partial(NspPoints3d *H);
extern void nsp_points3d_destroy_partial(NspPoints3d *H);
extern NspPoints3d * nsp_points3d_copy_partial(NspPoints3d *H,NspPoints3d *self);
extern NspPoints3d * nsp_points3d_full_copy_partial(NspPoints3d *H,NspPoints3d *self);
extern NspPoints3d * nsp_points3d_full_copy(NspPoints3d *self);
extern int nsp_points3d_check_values(NspPoints3d *H);
extern int int_points3d_create(Stack stack, int rhs, int opt, int lhs);
extern NspPoints3d *nsp_points3d_xdr_load_partial(XDR *xdrs, NspPoints3d *M);
extern int nsp_points3d_xdr_save(XDR  *xdrs, NspPoints3d *M);

#line 9 "codegen/points3d.override"

#include "../graphics-new/Plo3dObj.h"
/* inserted at the end of public part of include file */
extern void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) ;
extern BCG *nsp_check_graphic_context(void);
extern void apply_transforms(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);

#line 121 "./points3d.h"
#endif /* NSP_INC_NspPoints3d */ 

#ifdef NspPoints3d_Private 
static int init_points3d(NspPoints3d *o,NspTypePoints3d *type);
static int nsp_points3d_size(NspPoints3d *Mat, int flag);
static char *nsp_points3d_type_as_string(void);
static char *nsp_points3d_type_short_string(NspObject *v);
static int nsp_points3d_eq(NspPoints3d *A, NspObject *B);
static int nsp_points3d_neq(NspPoints3d *A, NspObject *B);
static NspPoints3d *nsp_points3d_xdr_load(XDR *xdrs);
static AttrTab points3d_attrs[];
static NspMethods *points3d_get_methods(void);
/* static int int_points3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPoints3d *nsp_points3d_create_void(const char *name,NspTypeBase *type);
#line 18 "codegen/points3d.override"
static void nsp_draw_points3d(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_points3d(NspGraphic *o,const double *tr);
static void nsp_rotate_points3d(NspGraphic *o,double *R);
static void nsp_scale_points3d(NspGraphic *o,double *alpha);
static int nsp_getbounds_points3d(NspGraphic *o,double *bounds);

static void nsp_points3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
static int nsp_points3d_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_points3d(NspPoints3d *P);

static void draw_points3d_ogl(BCG *Xgc,void *Ob);
static void draw_points3d_face(BCG *Xgc,NspGraphic *Ob, int j);

/* inserted in the private part of include file */

#line 152 "./points3d.h"
#endif /* NspPoints3d_Private */

