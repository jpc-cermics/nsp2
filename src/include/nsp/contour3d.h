/* -*- Mode: C -*- */
#ifndef NSP_INC_NspContour3d
#define NSP_INC_NspContour3d

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

#line 4 "codegen/contour3d.override"
/* inserted at the start of include file */
#include <nsp/figure.h>


#line 30 "./contour3d.h"
/* NspContour3d */

#include <nsp/graphic.h>

/*
 * NspContour3d inherits from Graphic
 */

typedef struct _NspContour3d NspContour3d ;
typedef struct _NspTypeContour3d NspTypeContour3d ;

struct _NspTypeContour3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_contour3d nsp_contour3d;
struct _nsp_contour3d {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* z;
  NspMatrix* zz;
  int flag;
  double zlev;
  int ref_count;
};

struct _NspContour3d {
  /*< private >*/
  NspGraphic father;
  NspTypeContour3d*type;
  /*< public >*/
  nsp_contour3d *obj;
};

extern int nsp_type_contour3d_id;
extern NspTypeContour3d *nsp_type_contour3d;

/* type instances for graphic */

NspTypeContour3d *new_type_contour3d(type_mode mode);

/* instance for NspContour3d */

NspContour3d *new_contour3d();

/*
 * Object methods redefined for contour3d 
 */


#define NULLCONTOUR3D (NspContour3d*) 0

extern NspContour3d *nsp_contour3d_create(const char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,NspMatrix* zz,int flag,double zlev,NspTypeBase *type);
extern NspContour3d *nsp_contour3d_create_default(const char *name);

/* from NspContour3dObj.c */

extern NspContour3d *nsp_contour3d_copy(NspContour3d *H);
extern void nsp_contour3d_destroy(NspContour3d *H);
extern int nsp_contour3d_info(NspContour3d *H, int indent,const char *name, int rec_level);
extern int nsp_contour3d_print(NspContour3d *H, int indent,const char *name, int rec_level);
extern int nsp_contour3d_latex(NspContour3d *H, int indent,const char *name, int rec_level);
extern NspContour3d *nsp_contour3d_object (NspObject *O);
extern int IsContour3dObj (Stack stack, int i);
extern int IsContour3d(NspObject *O);
extern NspContour3d *GetContour3dCopy (Stack stack, int i);
extern NspContour3d *GetContour3d (Stack stack, int i);
extern int nsp_contour3d_create_partial(NspContour3d *H);
extern void nsp_contour3d_destroy_partial(NspContour3d *H);
extern NspContour3d * nsp_contour3d_copy_partial(NspContour3d *H,NspContour3d *self);
extern NspContour3d * nsp_contour3d_full_copy_partial(NspContour3d *H,NspContour3d *self);
extern NspContour3d * nsp_contour3d_full_copy(NspContour3d *self);
extern int nsp_contour3d_check_values(NspContour3d *H);
extern int int_contour3d_create(Stack stack, int rhs, int opt, int lhs);
extern NspContour3d *nsp_contour3d_xdr_load_partial(XDR *xdrs, NspContour3d *M);
extern int nsp_contour3d_xdr_save(XDR  *xdrs, NspContour3d *M);

#line 10 "codegen/contour3d.override"

/* inserted at the end of public part of include file */

#line 114 "./contour3d.h"
#endif /* NSP_INC_NspContour3d */ 

#ifdef NspContour3d_Private 
static int init_contour3d(NspContour3d *o,NspTypeContour3d *type);
static int nsp_contour3d_size(NspContour3d *Mat, int flag);
static char *nsp_contour3d_type_as_string(void);
static char *nsp_contour3d_type_short_string(NspObject *v);
static int nsp_contour3d_eq(NspContour3d *A, NspObject *B);
static int nsp_contour3d_neq(NspContour3d *A, NspObject *B);
static NspContour3d *nsp_contour3d_xdr_load(XDR *xdrs);
static AttrTab contour3d_attrs[];
static NspMethods *contour3d_get_methods(void);
/* static int int_contour3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspContour3d *nsp_contour3d_create_void(const char *name,NspTypeBase *type);
#line 15 "codegen/contour3d.override"

/* inserted in the private part of include file */
static void nsp_draw_contour3d(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_contour3d(NspGraphic *o,const double *tr);
static void nsp_rotate_contour3d(NspGraphic *o,double *R);
static void nsp_scale_contour3d(NspGraphic *o,double *alpha);
static int nsp_getbounds_contour3d(NspGraphic *o,double *bounds);
static void nsp_contour3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
static int nsp_contour3d_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_contour3d(NspContour3d *P);

#ifdef  WITH_OPENGL 
static void draw_contour3d_ogl(BCG *Xgc,void *Ob);
#endif 
static void draw_contour3d_face(BCG *Xgc,NspGraphic *Ob, int j);

#line 146 "./contour3d.h"
#endif /* NspContour3d_Private */

