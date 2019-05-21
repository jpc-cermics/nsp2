/* -*- Mode: C -*- */
#ifndef NSP_INC_NspCurve
#define NSP_INC_NspCurve

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

#line 4 "codegen/curve.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./curve.h"
/* NspCurve */

#include <nsp/graphic.h>

/*
 * NspCurve inherits from Graphic
 */

typedef struct _NspCurve NspCurve ;
typedef struct _NspTypeCurve NspTypeCurve ;

struct _NspTypeCurve {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_curve nsp_curve;
struct _nsp_curve {
  int mark;
  int mark_size;
  int mark_color;
  int width;
  int color;
  int mode;
  NspMatrix* Pts;
  char* legend;
  int ref_count;
};

struct _NspCurve {
  /*< private >*/
  NspGraphic father;
  NspTypeCurve*type;
  /*< public >*/
  nsp_curve *obj;
};

extern int nsp_type_curve_id;
extern NspTypeCurve *nsp_type_curve;

/* type instances for graphic */

NspTypeCurve *new_type_curve(type_mode mode);

/* instance for NspCurve */

NspCurve *new_curve();

/*
 * Object methods redefined for curve 
 */


#define NULLCURVE (NspCurve*) 0

extern NspCurve *nsp_curve_create(const char *name,int mark,int mark_size,int mark_color,int width,int color,int mode,NspMatrix* Pts,char* legend,NspTypeBase *type);
extern NspCurve *nsp_curve_create_default(const char *name);

/* from NspCurveObj.c */

extern NspCurve *nsp_curve_copy(NspCurve *H);
extern void nsp_curve_destroy(NspCurve *H);
extern int nsp_curve_info(NspCurve *H, int indent,const char *name, int rec_level);
extern int nsp_curve_print(NspCurve *H, int indent,const char *name, int rec_level);
extern int nsp_curve_latex(NspCurve *H, int indent,const char *name, int rec_level);
extern NspCurve *nsp_curve_object (NspObject *O);
extern int IsCurveObj (Stack stack, int i);
extern int IsCurve(NspObject *O);
extern NspCurve *GetCurveCopy (Stack stack, int i);
extern NspCurve *GetCurve (Stack stack, int i);
extern int nsp_curve_create_partial(NspCurve *H);
extern void nsp_curve_destroy_partial(NspCurve *H);
extern NspCurve * nsp_curve_copy_partial(NspCurve *H,NspCurve *self);
extern NspCurve * nsp_curve_full_copy_partial(NspCurve *H,NspCurve *self);
extern NspCurve * nsp_curve_full_copy(NspCurve *self);
extern int nsp_curve_check_values(NspCurve *H);
extern int int_curve_create(Stack stack, int rhs, int opt, int lhs);
extern NspCurve *nsp_curve_xdr_load_partial(XDR *xdrs, NspCurve *M);
extern int nsp_curve_xdr_save(XDR  *xdrs, NspCurve *M);

#line 9 "codegen/curve.override"
/* inserted at the end of public part of include file */
typedef enum { curve_std, curve_stairs, curve_stem , curve_arrow, curve_fill, curve_stairs_fill } nsp_curve_mode ;

#line 115 "./curve.h"
#endif /* NSP_INC_NspCurve */ 

#ifdef NspCurve_Private 
static int init_curve(NspCurve *o,NspTypeCurve *type);
static int nsp_curve_size(NspCurve *Mat, int flag);
static char *nsp_curve_type_as_string(void);
static char *nsp_curve_type_short_string(NspObject *v);
static int nsp_curve_eq(NspCurve *A, NspObject *B);
static int nsp_curve_neq(NspCurve *A, NspObject *B);
static NspCurve *nsp_curve_xdr_load(XDR *xdrs);
static AttrTab curve_attrs[];
static NspMethods *curve_get_methods(void);
/* static int int_curve_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspCurve *nsp_curve_create_void(const char *name,NspTypeBase *type);
#line 14 "codegen/curve.override"

/* inserted in the private part of include file */
static void nsp_draw_curve(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_curve(NspGraphic *o,const double *tr);
static void nsp_rotate_curve(NspGraphic *o,double *R);
static void nsp_scale_curve(NspGraphic *o,double *alpha);
static int nsp_getbounds_curve(NspGraphic *o,double *bounds);
static void nsp_curve_fill(BCG *Xgc,NspCurve *C,NspMatrix *M);
static void nsp_curve_fill_basic(BCG *Xgc,NspCurve *C,NspMatrix *M);

#ifdef  WITH_OPENGL
static void nsp_curve_fill_ext(BCG *Xgc,NspCurve *C,NspMatrix *M);
static int nsp_curve_fill_part(BCG *Xgc,NspCurve *C, NspMatrix *M, int start,double *xi);
#endif

static void nsp_curve_stairs_fill_basic(BCG *Xgc,NspCurve *P,NspMatrix *M);
static void nsp_curve_stairs_fill(BCG *Xgc,NspCurve *P,NspMatrix *M);

#line 149 "./curve.h"
#endif /* NspCurve_Private */

