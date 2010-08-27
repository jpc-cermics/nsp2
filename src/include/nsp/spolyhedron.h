/* -*- Mode: C -*- */
#ifndef NSP_INC_NspSPolyhedron
#define NSP_INC_NspSPolyhedron

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

#line 4 "codegen/spolyhedron.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./spolyhedron.h"
/* NspSPolyhedron */

#include <nsp/graphic.h>

/*
 * NspSPolyhedron inherits from Graphic
 */

typedef struct _NspSPolyhedron NspSPolyhedron ;
typedef struct _NspTypeSPolyhedron NspTypeSPolyhedron ;

#line 41 "./spolyhedron.h"

struct _NspTypeSPolyhedron {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 48 "./spolyhedron.h"
};

typedef struct _nsp_spolyhedron nsp_spolyhedron;
struct _nsp_spolyhedron {
  NspMatrix* Mcoord;
  NspMatrix* Mface;
  NspMatrix* Mval;
  double vmin;
  double vmax;
  int colmin;
  int colmax;
  int coloutmin;
  int coloutmax;
  gboolean mesh;
  gboolean mesh_only;
  int back_color;
  gboolean shade;
  void* Mcoord_l;
  int* pos;  int pos_length;
  int* fill;  int fill_length;
  double* vlevel;  int vlevel_length;
  int coldef;
  int ref_count;
};

struct _NspSPolyhedron {
  /*< private >*/
  NspGraphic father;
  NspTypeSPolyhedron*type;
  /*< public >*/
  nsp_spolyhedron *obj;
};

extern int nsp_type_spolyhedron_id;
extern NspTypeSPolyhedron *nsp_type_spolyhedron;

/* type instances for graphic */

NspTypeSPolyhedron *new_type_spolyhedron(type_mode mode);

/* instance for NspSPolyhedron */

NspSPolyhedron *new_spolyhedron();

/*
 * Object methods redefined for spolyhedron 
 */


#define NULLSPOLYHEDRON (NspSPolyhedron*) 0

extern NspSPolyhedron *nsp_spolyhedron_create(char *name,NspMatrix* Mcoord,NspMatrix* Mface,NspMatrix* Mval,double vmin,double vmax,int colmin,int colmax,int coloutmin,int coloutmax,gboolean mesh,gboolean mesh_only,int back_color,gboolean shade,void* Mcoord_l,int* pos, int pos_length,int* fill, int fill_length,double* vlevel, int vlevel_length,int coldef,NspTypeBase *type);
extern NspSPolyhedron *nsp_spolyhedron_create_default(char *name);

/* from NspSPolyhedronObj.c */

extern NspSPolyhedron *nsp_spolyhedron_copy(NspSPolyhedron *H);
extern void nsp_spolyhedron_destroy(NspSPolyhedron *H);
extern int nsp_spolyhedron_info(NspSPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_spolyhedron_print(NspSPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_spolyhedron_latex(NspSPolyhedron *H, int indent,const char *name, int rec_level);
extern NspSPolyhedron *nsp_spolyhedron_object (NspObject *O);
extern int IsSPolyhedronObj (Stack stack, int i);
extern int IsSPolyhedron(NspObject *O);
extern NspSPolyhedron *GetSPolyhedronCopy (Stack stack, int i);
extern NspSPolyhedron *GetSPolyhedron (Stack stack, int i);
extern int nsp_spolyhedron_create_partial(NspSPolyhedron *H);
extern void nsp_spolyhedron_destroy_partial(NspSPolyhedron *H);
extern NspSPolyhedron * nsp_spolyhedron_copy_partial(NspSPolyhedron *H,NspSPolyhedron *self);
extern NspSPolyhedron * nsp_spolyhedron_full_copy_partial(NspSPolyhedron *H,NspSPolyhedron *self);
extern NspSPolyhedron * nsp_spolyhedron_full_copy(NspSPolyhedron *self);
extern int nsp_spolyhedron_check_values(NspSPolyhedron *H);
extern int int_spolyhedron_create(Stack stack, int rhs, int opt, int lhs);
extern NspSPolyhedron *nsp_spolyhedron_xdr_load_partial(XDR *xdrs, NspSPolyhedron *M);
extern int nsp_spolyhedron_xdr_save(XDR  *xdrs, NspSPolyhedron *M);

#line 9 "codegen/spolyhedron.override"
/* inserted at the end of public part of include file */

extern NspSPolyhedron *nsp_spolyhedron_create_from_facets(char *name,double *xx,double *yy,double *zz,int m,int n,int *colors, int ncol ,int cmap_ncol );
extern NspSPolyhedron *nsp_spolyhedron_create_from_triplet(char *name,double *x,double *y,double *z,int m,int n, double *col,int ncol);

#line 131 "./spolyhedron.h"
#endif /* NSP_INC_NspSPolyhedron */ 

#ifdef NspSPolyhedron_Private 
static int init_spolyhedron(NspSPolyhedron *o,NspTypeSPolyhedron *type);
static int nsp_spolyhedron_size(NspSPolyhedron *Mat, int flag);
static char *nsp_spolyhedron_type_as_string(void);
static char *nsp_spolyhedron_type_short_string(NspObject *v);
static int nsp_spolyhedron_eq(NspSPolyhedron *A, NspObject *B);
static int nsp_spolyhedron_neq(NspSPolyhedron *A, NspObject *B);
static NspSPolyhedron *nsp_spolyhedron_xdr_load(XDR *xdrs);
static AttrTab spolyhedron_attrs[];
static NspMethods *spolyhedron_get_methods(void);
/* static int int_spolyhedron_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspSPolyhedron *nsp_spolyhedron_create_void(char *name,NspTypeBase *type);
#line 16 "codegen/spolyhedron.override"
/* inserted in the private part of include file */
static void nsp_draw_spolyhedron(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_spolyhedron(NspGraphic *o,const double *tr);
static void nsp_rotate_spolyhedron(NspGraphic *o,double *R);
static void nsp_scale_spolyhedron(NspGraphic *o,double *alpha);
static int nsp_getbounds_spolyhedron(NspGraphic *o,double *bounds);
static void nsp_spolyhedron_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
static int nsp_spolyhedron_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_spolyhedron(BCG *Xgc,NspSPolyhedron *P);

static void draw_spolyhedron_ogl(BCG *Xgc,void *Ob);
static void draw_spolyhedron_face(BCG *Xgc,NspGraphic *Ob, int j);
static int zone(double val, double valmin, double valmax, int nv);
static void interp_color_triangle(BCG *Xgc,int *x, int *y, double *v, int *z, double *zlevel, int *fill);
static void permut_of_sort(int *tab, int *perm);
static void find_intersection(int *sx, int *sy, double *fxy, double z, 
			      int inda, int indb, int *xint, int *yint);

#line 165 "./spolyhedron.h"
#endif /* NspSPolyhedron_Private */

