/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAxes
#define NSP_INC_NspAxes

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspAxes */

#include <nsp/graphic.h>

/*
 * NspAxes inherits from Graphic
 */

typedef struct _NspAxes NspAxes ;
typedef struct _NspTypeNspAxes NspTypeNspAxes ;

#line 22 "./axes.h"

struct _NspTypeNspAxes {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./axes.h"
};

typedef struct _nsp_axes nsp_axes;
struct _nsp_axes {
  NspMatrix* wrect;
  double rho;
  gboolean top;
  NspMatrix* bounds;
  NspMatrix* arect;
  NspMatrix* frect;
  char* title;
  char* x;
  char* y;
  NspList* children;
  gboolean fixed;
  gboolean iso;
  gboolean auto_axis;
  int grid;
  int axes;
  gboolean xlog;
  gboolean ylog;
  int lpos;
  NspMatrix* rect;
  gboolean zoom;
  NspMatrix* zrect;
  int ref_count;
};

struct _NspAxes {
  /*< private >*/
  NspGraphic father;
  NspTypeNspAxes*type;
  /*< public >*/
  nsp_axes *obj;
};

extern int nsp_type_axes_id;
extern NspTypeNspAxes *nsp_type_axes;

/* type instances for graphic */

NspTypeNspAxes *new_type_axes(type_mode mode);

/* instance for NspAxes */

NspAxes *new_axes();

/*
* Object methods redefined for axes 
*/


#define NULLAXES (NspAxes*) 0

extern NspAxes *nsp_axes_create(char *name,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,char* x,char* y,NspList* children,gboolean fixed,gboolean iso,gboolean auto_axis,int grid,int axes,gboolean xlog,gboolean ylog,int lpos,NspMatrix* rect,gboolean zoom,NspMatrix* zrect,NspTypeBase *type);
extern NspAxes *nsp_axes_create_default(char *name);

/* from NspAxesObj.c */

extern NspAxes *nsp_axes_copy(NspAxes *H);
extern void nsp_axes_destroy(NspAxes *H);
extern int nsp_axes_info(NspAxes *H, int indent,const char *name, int rec_level);
extern int nsp_axes_print(NspAxes *H, int indent,const char *name, int rec_level);
extern int nsp_axes_latex(NspAxes *H, int indent,const char *name, int rec_level);
extern NspAxes *nsp_axes_object (NspObject *O); 
extern int IsAxesObj (Stack stack, int i); 
extern int IsAxes(NspObject *O);
extern NspAxes *GetAxesCopy (Stack stack, int i); 
extern NspAxes *GetAxes (Stack stack, int i); 
extern int nsp_axes_create_partial(NspAxes *H);
extern void nsp_axes_destroy_partial(NspAxes *H);
extern NspAxes * nsp_axes_copy_partial(NspAxes *H,NspAxes *self);
extern NspAxes * nsp_axes_full_copy_partial(NspAxes *H,NspAxes *self);
extern NspAxes * nsp_axes_full_copy(NspAxes *self);
extern int nsp_axes_check_values(NspAxes *H);
extern int int_axes_create(Stack stack, int rhs, int opt, int lhs); 
extern NspAxes *nsp_axes_xdr_load_partial(XDR *xdrs, NspAxes *M);
extern int nsp_axes_xdr_save(XDR  *xdrs, NspAxes *M);

#endif /* NSP_INC_NspAxes */ 

#ifdef NspAxes_Private 
static int init_axes(NspAxes *o,NspTypeNspAxes *type);
static int nsp_axes_size(NspAxes *Mat, int flag);
static char *nsp_axes_type_as_string(void);
static char *nsp_axes_type_short_string(NspObject *v);
static int nsp_axes_eq(NspAxes *A, NspObject *B);
static int nsp_axes_neq(NspAxes *A, NspObject *B);
static NspAxes *nsp_axes_xdr_load(XDR *xdrs);
static AttrTab axes_attrs[];
static NspMethods *axes_get_methods(void);
/* static int int_axes_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAxes *nsp_axes_create_void(char *name,NspTypeBase *type);
#endif /* NspAxes_Private */

