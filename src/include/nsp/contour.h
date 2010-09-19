/* -*- Mode: C -*- */
#ifndef NSP_INC_NspContour
#define NSP_INC_NspContour

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

#line 4 "codegen/contour.override"
/* inserted at the start of include file */
#include <nsp/figure.h>


#line 30 "./contour.h"
/* NspContour */

#include <nsp/graphic.h>

/*
 * NspContour inherits from Graphic
 */

typedef struct _NspContour NspContour ;
typedef struct _NspTypeContour NspTypeContour ;

#line 42 "./contour.h"

struct _NspTypeContour {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 49 "./contour.h"
};

typedef struct _nsp_contour nsp_contour;
struct _nsp_contour {
  NspMatrix* z;
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* levels;
  int nlevels;
  NspMatrix* style;
  int ref_count;
};

struct _NspContour {
  /*< private >*/
  NspGraphic father;
  NspTypeContour*type;
  /*< public >*/
  nsp_contour *obj;
};

extern int nsp_type_contour_id;
extern NspTypeContour *nsp_type_contour;

/* type instances for graphic */

NspTypeContour *new_type_contour(type_mode mode);

/* instance for NspContour */

NspContour *new_contour();

/*
 * Object methods redefined for contour 
 */


#define NULLCONTOUR (NspContour*) 0

extern NspContour *nsp_contour_create(const char *name,NspMatrix* z,NspMatrix* x,NspMatrix* y,NspMatrix* levels,int nlevels,NspMatrix* style,NspTypeBase *type);
extern NspContour *nsp_contour_create_default(const char *name);

/* from NspContourObj.c */

extern NspContour *nsp_contour_copy(NspContour *H);
extern void nsp_contour_destroy(NspContour *H);
extern int nsp_contour_info(NspContour *H, int indent,const char *name, int rec_level);
extern int nsp_contour_print(NspContour *H, int indent,const char *name, int rec_level);
extern int nsp_contour_latex(NspContour *H, int indent,const char *name, int rec_level);
extern NspContour *nsp_contour_object (NspObject *O);
extern int IsContourObj (Stack stack, int i);
extern int IsContour(NspObject *O);
extern NspContour *GetContourCopy (Stack stack, int i);
extern NspContour *GetContour (Stack stack, int i);
extern int nsp_contour_create_partial(NspContour *H);
extern void nsp_contour_destroy_partial(NspContour *H);
extern NspContour * nsp_contour_copy_partial(NspContour *H,NspContour *self);
extern NspContour * nsp_contour_full_copy_partial(NspContour *H,NspContour *self);
extern NspContour * nsp_contour_full_copy(NspContour *self);
extern int nsp_contour_check_values(NspContour *H);
extern int int_contour_create(Stack stack, int rhs, int opt, int lhs);
extern NspContour *nsp_contour_xdr_load_partial(XDR *xdrs, NspContour *M);
extern int nsp_contour_xdr_save(XDR  *xdrs, NspContour *M);

#line 10 "codegen/contour.override"

extern int nsp_contour2_obj(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2, 
			    int *flagnz, int *nz, double *zz, int *style);

/* inserted at the end of public part of include file */

#line 121 "./contour.h"
#endif /* NSP_INC_NspContour */ 

#ifdef NspContour_Private 
static int init_contour(NspContour *o,NspTypeContour *type);
static int nsp_contour_size(NspContour *Mat, int flag);
static char *nsp_contour_type_as_string(void);
static char *nsp_contour_type_short_string(NspObject *v);
static int nsp_contour_eq(NspContour *A, NspObject *B);
static int nsp_contour_neq(NspContour *A, NspObject *B);
static NspContour *nsp_contour_xdr_load(XDR *xdrs);
static AttrTab contour_attrs[];
static NspMethods *contour_get_methods(void);
/* static int int_contour_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspContour *nsp_contour_create_void(const char *name,NspTypeBase *type);
#line 18 "codegen/contour.override"

/* inserted in the private part of include file */

static void nsp_draw_contour(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_contour(NspGraphic *o,const double *tr);
static void nsp_rotate_contour(NspGraphic *o,double *R);
static void nsp_scale_contour(NspGraphic *o,double *alpha);
static int nsp_getbounds_contour(NspGraphic *o,double *bounds);

#line 146 "./contour.h"
#endif /* NspContour_Private */

