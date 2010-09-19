/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGrstring
#define NSP_INC_NspGrstring

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

#line 4 "codegen/grstring.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./grstring.h"
/* NspGrstring */

#include <nsp/graphic.h>

/*
 * NspGrstring inherits from Graphic
 */

typedef struct _NspGrstring NspGrstring ;
typedef struct _NspTypeGrstring NspTypeGrstring ;

#line 41 "./grstring.h"

struct _NspTypeGrstring {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 48 "./grstring.h"
};

typedef struct _nsp_grstring nsp_grstring;
struct _nsp_grstring {
  double x;
  double y;
  char* font;
  NspSMatrix* text;
  double angle;
  double w;
  double h;
  int fill;
  int posx;
  int posy;
  int size;
  int ref_count;
};

struct _NspGrstring {
  /*< private >*/
  NspGraphic father;
  NspTypeGrstring*type;
  /*< public >*/
  nsp_grstring *obj;
};

extern int nsp_type_grstring_id;
extern NspTypeGrstring *nsp_type_grstring;

/* type instances for graphic */

NspTypeGrstring *new_type_grstring(type_mode mode);

/* instance for NspGrstring */

NspGrstring *new_grstring();

/*
 * Object methods redefined for grstring 
 */


#define NULLGRSTRING (NspGrstring*) 0

extern NspGrstring *nsp_grstring_create(const char *name,double x,double y,char* font,NspSMatrix* text,double angle,double w,double h,int fill,int posx,int posy,int size,NspTypeBase *type);
extern NspGrstring *nsp_grstring_create_default(const char *name);

/* from NspGrstringObj.c */

extern NspGrstring *nsp_grstring_copy(NspGrstring *H);
extern void nsp_grstring_destroy(NspGrstring *H);
extern int nsp_grstring_info(NspGrstring *H, int indent,const char *name, int rec_level);
extern int nsp_grstring_print(NspGrstring *H, int indent,const char *name, int rec_level);
extern int nsp_grstring_latex(NspGrstring *H, int indent,const char *name, int rec_level);
extern NspGrstring *nsp_grstring_object (NspObject *O);
extern int IsGrstringObj (Stack stack, int i);
extern int IsGrstring(NspObject *O);
extern NspGrstring *GetGrstringCopy (Stack stack, int i);
extern NspGrstring *GetGrstring (Stack stack, int i);
extern int nsp_grstring_create_partial(NspGrstring *H);
extern void nsp_grstring_destroy_partial(NspGrstring *H);
extern NspGrstring * nsp_grstring_copy_partial(NspGrstring *H,NspGrstring *self);
extern NspGrstring * nsp_grstring_full_copy_partial(NspGrstring *H,NspGrstring *self);
extern NspGrstring * nsp_grstring_full_copy(NspGrstring *self);
extern int nsp_grstring_check_values(NspGrstring *H);
extern int int_grstring_create(Stack stack, int rhs, int opt, int lhs);
extern NspGrstring *nsp_grstring_xdr_load_partial(XDR *xdrs, NspGrstring *M);
extern int nsp_grstring_xdr_save(XDR  *xdrs, NspGrstring *M);

#line 9 "codegen/grstring.override"

/* inserted at the end of public part of include file */

#line 122 "./grstring.h"
#endif /* NSP_INC_NspGrstring */ 

#ifdef NspGrstring_Private 
static int init_grstring(NspGrstring *o,NspTypeGrstring *type);
static int nsp_grstring_size(NspGrstring *Mat, int flag);
static char *nsp_grstring_type_as_string(void);
static char *nsp_grstring_type_short_string(NspObject *v);
static int nsp_grstring_eq(NspGrstring *A, NspObject *B);
static int nsp_grstring_neq(NspGrstring *A, NspObject *B);
static NspGrstring *nsp_grstring_xdr_load(XDR *xdrs);
static AttrTab grstring_attrs[];
static NspMethods *grstring_get_methods(void);
/* static int int_grstring_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGrstring *nsp_grstring_create_void(const char *name,NspTypeBase *type);
#line 14 "codegen/grstring.override"

/* inserted in the private part of include file */
static void nsp_draw_grstring(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_grstring(NspGraphic *o,const double *tr);
static void nsp_rotate_grstring(NspGraphic *o,double *R);
static void nsp_scale_grstring(NspGraphic *o,double *alpha);
static int nsp_getbounds_grstring(NspGraphic *o,double *bounds);

#line 146 "./grstring.h"
#endif /* NspGrstring_Private */

