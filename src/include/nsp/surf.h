/* -*- Mode: C -*- */
#ifndef NSP_INC_NspSurf
#define NSP_INC_NspSurf

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

#line 4 "codegen/surf.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./surf.h"
/* NspSurf */

#include <nsp/graphic.h>

/*
 * NspSurf inherits from Graphic
 */

typedef struct _NspSurf NspSurf ;
typedef struct _NspTypeSurf NspTypeSurf ;

#line 41 "./surf.h"

struct _NspTypeSurf {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 48 "./surf.h"
};

typedef struct _nsp_surf nsp_surf;
struct _nsp_surf {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* z;
  NspMatrix* colors;
  gboolean mesh;
  gboolean zcolor;
  int mesh_color;
  int face_color;
  int ref_count;
};

struct _NspSurf {
  /*< private >*/
  NspGraphic father;
  NspTypeSurf*type;
  /*< public >*/
  nsp_surf *obj;
};

extern int nsp_type_surf_id;
extern NspTypeSurf *nsp_type_surf;

/* type instances for graphic */

NspTypeSurf *new_type_surf(type_mode mode);

/* instance for NspSurf */

NspSurf *new_surf();

/*
 * Object methods redefined for surf 
 */


#define NULLSURF (NspSurf*) 0

extern NspSurf *nsp_surf_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,NspMatrix* colors,gboolean mesh,gboolean zcolor,int mesh_color,int face_color,NspTypeBase *type);
extern NspSurf *nsp_surf_create_default(char *name);

/* from NspSurfObj.c */

extern NspSurf *nsp_surf_copy(NspSurf *H);
extern void nsp_surf_destroy(NspSurf *H);
extern int nsp_surf_info(NspSurf *H, int indent,const char *name, int rec_level);
extern int nsp_surf_print(NspSurf *H, int indent,const char *name, int rec_level);
extern int nsp_surf_latex(NspSurf *H, int indent,const char *name, int rec_level);
extern NspSurf *nsp_surf_object (NspObject *O);
extern int IsSurfObj (Stack stack, int i);
extern int IsSurf(NspObject *O);
extern NspSurf *GetSurfCopy (Stack stack, int i);
extern NspSurf *GetSurf (Stack stack, int i);
extern int nsp_surf_create_partial(NspSurf *H);
extern void nsp_surf_destroy_partial(NspSurf *H);
extern NspSurf * nsp_surf_copy_partial(NspSurf *H,NspSurf *self);
extern NspSurf * nsp_surf_full_copy_partial(NspSurf *H,NspSurf *self);
extern NspSurf * nsp_surf_full_copy(NspSurf *self);
extern int nsp_surf_check_values(NspSurf *H);
extern int int_surf_create(Stack stack, int rhs, int opt, int lhs);
extern NspSurf *nsp_surf_xdr_load_partial(XDR *xdrs, NspSurf *M);
extern int nsp_surf_xdr_save(XDR  *xdrs, NspSurf *M);

#line 9 "codegen/surf.override"

/* inserted at the end of public part of include file */

#line 119 "./surf.h"
#endif /* NSP_INC_NspSurf */ 

#ifdef NspSurf_Private 
static int init_surf(NspSurf *o,NspTypeSurf *type);
static int nsp_surf_size(NspSurf *Mat, int flag);
static char *nsp_surf_type_as_string(void);
static char *nsp_surf_type_short_string(NspObject *v);
static int nsp_surf_eq(NspSurf *A, NspObject *B);
static int nsp_surf_neq(NspSurf *A, NspObject *B);
static NspSurf *nsp_surf_xdr_load(XDR *xdrs);
static AttrTab surf_attrs[];
static NspMethods *surf_get_methods(void);
/* static int int_surf_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspSurf *nsp_surf_create_void(char *name,NspTypeBase *type);
#line 14 "codegen/surf.override"

/* inserted in the private part of include file */
static void nsp_draw_surf(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_surf(NspGraphic *o,const double *tr);
static void nsp_rotate_surf(NspGraphic *o,double *R);
static void nsp_scale_surf(NspGraphic *o,double *alpha);
static int nsp_getbounds_surf(NspGraphic *o,double *bounds);

#line 143 "./surf.h"
#endif /* NspSurf_Private */

