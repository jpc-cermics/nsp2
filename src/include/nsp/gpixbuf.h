/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGPixbuf
#define NSP_INC_NspGPixbuf

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

#line 4 "codegen/gpixbuf.override"
/* inserted at the start of include file */
#include <nsp/gtk/gobject.h>
#include <nsp/figure.h>

#line 30 "./gpixbuf.h"
/* NspGPixbuf */

#include <nsp/graphic.h>

/*
 * NspGPixbuf inherits from Graphic
 */

typedef struct _NspGPixbuf NspGPixbuf ;
typedef struct _NspTypeGPixbuf NspTypeGPixbuf ;

struct _NspTypeGPixbuf {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gpixbuf nsp_gpixbuf;
struct _nsp_gpixbuf {
  gint src_x;
  gint src_y;
  gdouble dest_x;
  gdouble dest_y;
  gdouble width;
  gdouble height;
  void* pixbuf;
  int ref_count;
};

struct _NspGPixbuf {
  /*< private >*/
  NspGraphic father;
  NspTypeGPixbuf*type;
  /*< public >*/
  nsp_gpixbuf *obj;
};

extern int nsp_type_gpixbuf_id;
extern NspTypeGPixbuf *nsp_type_gpixbuf;

/* type instances for graphic */

NspTypeGPixbuf *new_type_gpixbuf(type_mode mode);

/* instance for NspGPixbuf */

NspGPixbuf *new_gpixbuf();

/*
 * Object methods redefined for gpixbuf 
 */


#define NULLGPIXBUF (NspGPixbuf*) 0

extern NspGPixbuf *nsp_gpixbuf_create(const char *name,gint src_x,gint src_y,gdouble dest_x,gdouble dest_y,gdouble width,gdouble height,void* pixbuf,NspTypeBase *type);
extern NspGPixbuf *nsp_gpixbuf_create_default(const char *name);

/* from NspGPixbufObj.c */

extern NspGPixbuf *nsp_gpixbuf_copy(NspGPixbuf *H);
extern void nsp_gpixbuf_destroy(NspGPixbuf *H);
extern int nsp_gpixbuf_info(NspGPixbuf *H, int indent,const char *name, int rec_level);
extern int nsp_gpixbuf_print(NspGPixbuf *H, int indent,const char *name, int rec_level);
extern int nsp_gpixbuf_latex(NspGPixbuf *H, int indent,const char *name, int rec_level);
extern NspGPixbuf *nsp_gpixbuf_object (NspObject *O);
extern int IsGPixbufObj (Stack stack, int i);
extern int IsGPixbuf(NspObject *O);
extern NspGPixbuf *GetGPixbufCopy (Stack stack, int i);
extern NspGPixbuf *GetGPixbuf (Stack stack, int i);
extern int nsp_gpixbuf_create_partial(NspGPixbuf *H);
extern void nsp_gpixbuf_destroy_partial(NspGPixbuf *H);
extern NspGPixbuf * nsp_gpixbuf_copy_partial(NspGPixbuf *H,NspGPixbuf *self);
extern NspGPixbuf * nsp_gpixbuf_full_copy_partial(NspGPixbuf *H,NspGPixbuf *self);
extern NspGPixbuf * nsp_gpixbuf_full_copy(NspGPixbuf *self);
extern int nsp_gpixbuf_check_values(NspGPixbuf *H);
extern int int_gpixbuf_create(Stack stack, int rhs, int opt, int lhs);
extern NspGPixbuf *nsp_gpixbuf_xdr_load_partial(XDR *xdrs, NspGPixbuf *M);
extern int nsp_gpixbuf_xdr_save(XDR  *xdrs, NspGPixbuf *M);

#line 10 "codegen/gpixbuf.override"
/* inserted at the end of public part of include file */

#line 114 "./gpixbuf.h"
#endif /* NSP_INC_NspGPixbuf */ 

#ifdef NspGPixbuf_Private 
static int init_gpixbuf(NspGPixbuf *o,NspTypeGPixbuf *type);
static int nsp_gpixbuf_size(NspGPixbuf *Mat, int flag);
static char *nsp_gpixbuf_type_as_string(void);
static char *nsp_gpixbuf_type_short_string(NspObject *v);
static int nsp_gpixbuf_eq(NspGPixbuf *A, NspObject *B);
static int nsp_gpixbuf_neq(NspGPixbuf *A, NspObject *B);
static NspGPixbuf *nsp_gpixbuf_xdr_load(XDR *xdrs);
static AttrTab gpixbuf_attrs[];
static NspMethods *gpixbuf_get_methods(void);
/* static int int_gpixbuf_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGPixbuf *nsp_gpixbuf_create_void(const char *name,NspTypeBase *type);
#line 14 "codegen/gpixbuf.override"
/* inserted in the private part of include file */
static void nsp_draw_gpixbuf(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_gpixbuf(NspGraphic *o,const double *tr);
static void nsp_rotate_gpixbuf(NspGraphic *o,double *R);
static void nsp_scale_gpixbuf(NspGraphic *o,double *alpha);
static int nsp_getbounds_gpixbuf(NspGraphic *o,double *bounds);

#line 137 "./gpixbuf.h"
#endif /* NspGPixbuf_Private */

