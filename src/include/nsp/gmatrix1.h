/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMatrix1
#define NSP_INC_NspGMatrix1

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

#line 4 "codegen/gmatrix1.override"
/* inserted at the start of include file */
#include <nsp/figure.h>


#line 30 "./gmatrix1.h"
/* NspGMatrix1 */

#include <nsp/graphic.h>

/*
 * NspGMatrix1 inherits from Graphic
 */

typedef struct _NspGMatrix1 NspGMatrix1 ;
typedef struct _NspTypeGMatrix1 NspTypeGMatrix1 ;

#line 42 "./gmatrix1.h"

struct _NspTypeGMatrix1 {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 49 "./gmatrix1.h"
};

typedef struct _nsp_gmatrix1 nsp_gmatrix1;
struct _nsp_gmatrix1 {
  NspMatrix* data;
  gboolean remap;
  gboolean shade;
  NspMatrix* colminmax;
  NspMatrix* zminmax;
  NspMatrix* colout;
  NspMatrix* x;
  NspMatrix* y;
  int ref_count;
};

struct _NspGMatrix1 {
  /*< private >*/
  NspGraphic father;
  NspTypeGMatrix1*type;
  /*< public >*/
  nsp_gmatrix1 *obj;
};

extern int nsp_type_gmatrix1_id;
extern NspTypeGMatrix1 *nsp_type_gmatrix1;

/* type instances for graphic */

NspTypeGMatrix1 *new_type_gmatrix1(type_mode mode);

/* instance for NspGMatrix1 */

NspGMatrix1 *new_gmatrix1();

/*
 * Object methods redefined for gmatrix1 
 */


#define NULLGMATRIX1 (NspGMatrix1*) 0

extern NspGMatrix1 *nsp_gmatrix1_create(const char *name,NspMatrix* data,gboolean remap,gboolean shade,NspMatrix* colminmax,NspMatrix* zminmax,NspMatrix* colout,NspMatrix* x,NspMatrix* y,NspTypeBase *type);
extern NspGMatrix1 *nsp_gmatrix1_create_default(const char *name);

/* from NspGMatrix1Obj.c */

extern NspGMatrix1 *nsp_gmatrix1_copy(NspGMatrix1 *H);
extern void nsp_gmatrix1_destroy(NspGMatrix1 *H);
extern int nsp_gmatrix1_info(NspGMatrix1 *H, int indent,const char *name, int rec_level);
extern int nsp_gmatrix1_print(NspGMatrix1 *H, int indent,const char *name, int rec_level);
extern int nsp_gmatrix1_latex(NspGMatrix1 *H, int indent,const char *name, int rec_level);
extern NspGMatrix1 *nsp_gmatrix1_object (NspObject *O);
extern int IsGMatrix1Obj (Stack stack, int i);
extern int IsGMatrix1(NspObject *O);
extern NspGMatrix1 *GetGMatrix1Copy (Stack stack, int i);
extern NspGMatrix1 *GetGMatrix1 (Stack stack, int i);
extern int nsp_gmatrix1_create_partial(NspGMatrix1 *H);
extern void nsp_gmatrix1_destroy_partial(NspGMatrix1 *H);
extern NspGMatrix1 * nsp_gmatrix1_copy_partial(NspGMatrix1 *H,NspGMatrix1 *self);
extern NspGMatrix1 * nsp_gmatrix1_full_copy_partial(NspGMatrix1 *H,NspGMatrix1 *self);
extern NspGMatrix1 * nsp_gmatrix1_full_copy(NspGMatrix1 *self);
extern int nsp_gmatrix1_check_values(NspGMatrix1 *H);
extern int int_gmatrix1_create(Stack stack, int rhs, int opt, int lhs);
extern NspGMatrix1 *nsp_gmatrix1_xdr_load_partial(XDR *xdrs, NspGMatrix1 *M);
extern int nsp_gmatrix1_xdr_save(XDR  *xdrs, NspGMatrix1 *M);

#line 10 "codegen/gmatrix1.override"

/* inserted at the end of public part of include file */

extern void PermutOfSort (const int *tab, int *perm);
extern void PaintTriangle (BCG *Xgc,const double *sx,const double *sy,const  double *fxy, 
			   const int *zxy, const double *zlevel,const int *fill);

#line 124 "./gmatrix1.h"
#endif /* NSP_INC_NspGMatrix1 */ 

#ifdef NspGMatrix1_Private 
static int init_gmatrix1(NspGMatrix1 *o,NspTypeGMatrix1 *type);
static int nsp_gmatrix1_size(NspGMatrix1 *Mat, int flag);
static char *nsp_gmatrix1_type_as_string(void);
static char *nsp_gmatrix1_type_short_string(NspObject *v);
static int nsp_gmatrix1_eq(NspGMatrix1 *A, NspObject *B);
static int nsp_gmatrix1_neq(NspGMatrix1 *A, NspObject *B);
static NspGMatrix1 *nsp_gmatrix1_xdr_load(XDR *xdrs);
static AttrTab gmatrix1_attrs[];
static NspMethods *gmatrix1_get_methods(void);
/* static int int_gmatrix1_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGMatrix1 *nsp_gmatrix1_create_void(const char *name,NspTypeBase *type);
#line 19 "codegen/gmatrix1.override"

/* inserted in the private part of include file */

static void nsp_draw_gmatrix1(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_gmatrix1(NspGraphic *o,const double *tr);
static void nsp_rotate_gmatrix1(NspGraphic *o,double *R);
static void nsp_scale_gmatrix1(NspGraphic *o,double *alpha);
static int nsp_getbounds_gmatrix1(NspGraphic *o,double *bounds);
static void nsp_draw_matrix_zmoy(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_draw_matrix_shade(BCG *Xgc,NspGraphic *Obj, void *data);
static void FindIntersection(const double *sx,const double *sy,const double *fxy,
			     double z,int inda, int indb,  int *xint, int *yint);

#line 153 "./gmatrix1.h"
#endif /* NspGMatrix1_Private */

