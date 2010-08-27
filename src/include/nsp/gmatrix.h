/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMatrix
#define NSP_INC_NspGMatrix

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

#line 4 "codegen/gmatrix.override"
/* inserted at the start of include file */
#include <nsp/figure.h>


#line 30 "./gmatrix.h"
/* NspGMatrix */

#include <nsp/graphic.h>

/*
 * NspGMatrix inherits from Graphic
 */

typedef struct _NspGMatrix NspGMatrix ;
typedef struct _NspTypeGMatrix NspTypeGMatrix ;

#line 42 "./gmatrix.h"

struct _NspTypeGMatrix {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 49 "./gmatrix.h"
};

typedef struct _nsp_gmatrix nsp_gmatrix;
struct _nsp_gmatrix {
  NspMatrix* data;
  NspMatrix* rect;
  gboolean remap;
  NspMatrix* colminmax;
  NspMatrix* zminmax;
  int ref_count;
};

struct _NspGMatrix {
  /*< private >*/
  NspGraphic father;
  NspTypeGMatrix*type;
  /*< public >*/
  nsp_gmatrix *obj;
};

extern int nsp_type_gmatrix_id;
extern NspTypeGMatrix *nsp_type_gmatrix;

/* type instances for graphic */

NspTypeGMatrix *new_type_gmatrix(type_mode mode);

/* instance for NspGMatrix */

NspGMatrix *new_gmatrix();

/*
 * Object methods redefined for gmatrix 
 */


#define NULLGMATRIX (NspGMatrix*) 0

extern NspGMatrix *nsp_gmatrix_create(char *name,NspMatrix* data,NspMatrix* rect,gboolean remap,NspMatrix* colminmax,NspMatrix* zminmax,NspTypeBase *type);
extern NspGMatrix *nsp_gmatrix_create_default(char *name);

/* from NspGMatrixObj.c */

extern NspGMatrix *nsp_gmatrix_copy(NspGMatrix *H);
extern void nsp_gmatrix_destroy(NspGMatrix *H);
extern int nsp_gmatrix_info(NspGMatrix *H, int indent,const char *name, int rec_level);
extern int nsp_gmatrix_print(NspGMatrix *H, int indent,const char *name, int rec_level);
extern int nsp_gmatrix_latex(NspGMatrix *H, int indent,const char *name, int rec_level);
extern NspGMatrix *nsp_gmatrix_object (NspObject *O);
extern int IsGMatrixObj (Stack stack, int i);
extern int IsGMatrix(NspObject *O);
extern NspGMatrix *GetGMatrixCopy (Stack stack, int i);
extern NspGMatrix *GetGMatrix (Stack stack, int i);
extern int nsp_gmatrix_create_partial(NspGMatrix *H);
extern void nsp_gmatrix_destroy_partial(NspGMatrix *H);
extern NspGMatrix * nsp_gmatrix_copy_partial(NspGMatrix *H,NspGMatrix *self);
extern NspGMatrix * nsp_gmatrix_full_copy_partial(NspGMatrix *H,NspGMatrix *self);
extern NspGMatrix * nsp_gmatrix_full_copy(NspGMatrix *self);
extern int nsp_gmatrix_check_values(NspGMatrix *H);
extern int int_gmatrix_create(Stack stack, int rhs, int opt, int lhs);
extern NspGMatrix *nsp_gmatrix_xdr_load_partial(XDR *xdrs, NspGMatrix *M);
extern int nsp_gmatrix_xdr_save(XDR  *xdrs, NspGMatrix *M);

#line 10 "codegen/gmatrix.override"

/* inserted at the end of public part of include file */

#line 117 "./gmatrix.h"
#endif /* NSP_INC_NspGMatrix */ 

#ifdef NspGMatrix_Private 
static int init_gmatrix(NspGMatrix *o,NspTypeGMatrix *type);
static int nsp_gmatrix_size(NspGMatrix *Mat, int flag);
static char *nsp_gmatrix_type_as_string(void);
static char *nsp_gmatrix_type_short_string(NspObject *v);
static int nsp_gmatrix_eq(NspGMatrix *A, NspObject *B);
static int nsp_gmatrix_neq(NspGMatrix *A, NspObject *B);
static NspGMatrix *nsp_gmatrix_xdr_load(XDR *xdrs);
static AttrTab gmatrix_attrs[];
static NspMethods *gmatrix_get_methods(void);
/* static int int_gmatrix_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGMatrix *nsp_gmatrix_create_void(char *name,NspTypeBase *type);
#line 15 "codegen/gmatrix.override"

/* inserted in the private part of include file */
static void nsp_draw_gmatrix(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_gmatrix(NspGraphic *o,const double *tr);
static void nsp_rotate_gmatrix(NspGraphic *o,double *R);
static void nsp_scale_gmatrix(NspGraphic *o,double *alpha);
static int nsp_getbounds_gmatrix(NspGraphic *o,double *bounds);

#line 141 "./gmatrix.h"
#endif /* NspGMatrix_Private */

