/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMatrix
#define NSP_INC_NspGMatrix

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspGMatrix */

#include <nsp/graphic.h>

/*
 * NspGMatrix inherits from Graphic
 */

typedef struct _NspGMatrix NspGMatrix ;
typedef struct _NspTypeNspGMatrix NspTypeNspGMatrix ;

#line 22 "./gmatrix.h"

struct _NspTypeNspGMatrix {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./gmatrix.h"
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
  NspTypeNspGMatrix*type;
  /*< public >*/
  nsp_gmatrix *obj;
};

extern int nsp_type_gmatrix_id;
extern NspTypeNspGMatrix *nsp_type_gmatrix;

/* type instances for graphic */

NspTypeNspGMatrix *new_type_gmatrix(type_mode mode);

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

#line 4 "codegen/gmatrix.override"

/* inserted at the end of public part of include file */

#line 97 "./gmatrix.h"
#endif /* NSP_INC_NspGMatrix */ 

#ifdef NspGMatrix_Private 
static int init_gmatrix(NspGMatrix *o,NspTypeNspGMatrix *type);
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
#line 9 "codegen/gmatrix.override"

/* inserted in the private part of include file */
static void nsp_draw_gmatrix(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_gmatrix(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_gmatrix(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_gmatrix(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_gmatrix(BCG *Xgc,NspGraphic *o,double *bounds);

#line 121 "./gmatrix.h"
#endif /* NspGMatrix_Private */

