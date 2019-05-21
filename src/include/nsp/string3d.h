/* -*- Mode: C -*- */
#ifndef NSP_INC_NspString3d
#define NSP_INC_NspString3d

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

#line 4 "codegen/string3d.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./string3d.h"
/* NspString3d */

#include <nsp/graphic.h>

/*
 * NspString3d inherits from Graphic
 */

typedef struct _NspString3d NspString3d ;
typedef struct _NspTypeString3d NspTypeString3d ;

struct _NspTypeString3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_string3d nsp_string3d;
struct _nsp_string3d {
  NspMatrix* Mcoord;
  void* Mcoord_l;
  char* str;
  int font_type;
  int font_size;
  int* pos;  int pos_length;
  int ref_count;
};

struct _NspString3d {
  /*< private >*/
  NspGraphic father;
  NspTypeString3d*type;
  /*< public >*/
  nsp_string3d *obj;
};

extern int nsp_type_string3d_id;
extern NspTypeString3d *nsp_type_string3d;

/* type instances for graphic */

NspTypeString3d *new_type_string3d(type_mode mode);

/* instance for NspString3d */

NspString3d *new_string3d();

/*
 * Object methods redefined for string3d 
 */


#define NULLSTRING3D (NspString3d*) 0

extern NspString3d *nsp_string3d_create(const char *name,NspMatrix* Mcoord,void* Mcoord_l,char* str,int font_type,int font_size,int* pos, int pos_length,NspTypeBase *type);
extern NspString3d *nsp_string3d_create_default(const char *name);

/* from NspString3dObj.c */

extern NspString3d *nsp_string3d_copy(NspString3d *H);
extern void nsp_string3d_destroy(NspString3d *H);
extern int nsp_string3d_info(NspString3d *H, int indent,const char *name, int rec_level);
extern int nsp_string3d_print(NspString3d *H, int indent,const char *name, int rec_level);
extern int nsp_string3d_latex(NspString3d *H, int indent,const char *name, int rec_level);
extern NspString3d *nsp_string3d_object (NspObject *O);
extern int IsString3dObj (Stack stack, int i);
extern int IsString3d(NspObject *O);
extern NspString3d *GetString3dCopy (Stack stack, int i);
extern NspString3d *GetString3d (Stack stack, int i);
extern int nsp_string3d_create_partial(NspString3d *H);
extern void nsp_string3d_destroy_partial(NspString3d *H);
extern NspString3d * nsp_string3d_copy_partial(NspString3d *H,NspString3d *self);
extern NspString3d * nsp_string3d_full_copy_partial(NspString3d *H,NspString3d *self);
extern NspString3d * nsp_string3d_full_copy(NspString3d *self);
extern int nsp_string3d_check_values(NspString3d *H);
extern int int_string3d_create(Stack stack, int rhs, int opt, int lhs);
extern NspString3d *nsp_string3d_xdr_load_partial(XDR *xdrs, NspString3d *M);
extern int nsp_string3d_xdr_save(XDR  *xdrs, NspString3d *M);

#line 9 "codegen/string3d.override"

#include "../graphics-new/Plo3dObj.h"
/* inserted at the end of public part of include file */
extern BCG *nsp_check_graphic_context(void);

#line 115 "./string3d.h"
#endif /* NSP_INC_NspString3d */ 

#ifdef NspString3d_Private 
static int init_string3d(NspString3d *o,NspTypeString3d *type);
static int nsp_string3d_size(NspString3d *Mat, int flag);
static char *nsp_string3d_type_as_string(void);
static char *nsp_string3d_type_short_string(NspObject *v);
static int nsp_string3d_eq(NspString3d *A, NspObject *B);
static int nsp_string3d_neq(NspString3d *A, NspObject *B);
static NspString3d *nsp_string3d_xdr_load(XDR *xdrs);
static AttrTab string3d_attrs[];
static NspMethods *string3d_get_methods(void);
/* static int int_string3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspString3d *nsp_string3d_create_void(const char *name,NspTypeBase *type);
#line 16 "codegen/string3d.override"
static void nsp_draw_string3d(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_string3d(NspGraphic *o,const double *tr);
static void nsp_rotate_string3d(NspGraphic *o,double *R);
static void nsp_scale_string3d(NspGraphic *o,double *alpha);
static int nsp_getbounds_string3d(NspGraphic *o,double *bounds);
static void nsp_string3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
static int nsp_string3d_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_string3d(NspString3d *P);
#ifdef  WITH_OPENGL
static void draw_string3d_ogl(BCG *Xgc,void *Ob);
#endif
static void draw_string3d_face(BCG *Xgc,NspGraphic *Ob, int j);

/* inserted in the private part of include file */

#line 146 "./string3d.h"
#endif /* NspString3d_Private */

