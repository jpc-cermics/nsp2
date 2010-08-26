/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGrImage
#define NSP_INC_NspGrImage

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#line 4 "codegen/grimage.override"
/* inserted at the start of include file */
#include <nsp/figure.h>


#line 16 "./grimage.h"
/* NspGrImage */

#include <nsp/graphic.h>

/*
 * NspGrImage inherits from Graphic
 */

typedef struct _NspGrImage NspGrImage ;
typedef struct _NspTypeGrImage NspTypeGrImage ;

#line 28 "./grimage.h"

struct _NspTypeGrImage {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 35 "./grimage.h"
};

typedef struct _nsp_grimage nsp_grimage;
struct _nsp_grimage {
  double x;
  double y;
  double w;
  double h;
  gboolean border;
  int thickness;
  char* fname;
  void* image;
  int color;
  int ref_count;
};

struct _NspGrImage {
  /*< private >*/
  NspGraphic father;
  NspTypeGrImage*type;
  /*< public >*/
  nsp_grimage *obj;
};

extern int nsp_type_grimage_id;
extern NspTypeGrImage *nsp_type_grimage;

/* type instances for graphic */

NspTypeGrImage *new_type_grimage(type_mode mode);

/* instance for NspGrImage */

NspGrImage *new_grimage();

/*
 * Object methods redefined for grimage 
 */


#define NULLGRIMAGE (NspGrImage*) 0

extern NspGrImage *nsp_grimage_create(char *name,double x,double y,double w,double h,gboolean border,int thickness,char* fname,void* image,int color,NspTypeBase *type);
extern NspGrImage *nsp_grimage_create_default(char *name);

/* from NspGrImageObj.c */

extern NspGrImage *nsp_grimage_copy(NspGrImage *H);
extern void nsp_grimage_destroy(NspGrImage *H);
extern int nsp_grimage_info(NspGrImage *H, int indent,const char *name, int rec_level);
extern int nsp_grimage_print(NspGrImage *H, int indent,const char *name, int rec_level);
extern int nsp_grimage_latex(NspGrImage *H, int indent,const char *name, int rec_level);
extern NspGrImage *nsp_grimage_object (NspObject *O);
extern int IsGrImageObj (Stack stack, int i);
extern int IsGrImage(NspObject *O);
extern NspGrImage *GetGrImageCopy (Stack stack, int i);
extern NspGrImage *GetGrImage (Stack stack, int i);
extern int nsp_grimage_create_partial(NspGrImage *H);
extern void nsp_grimage_destroy_partial(NspGrImage *H);
extern NspGrImage * nsp_grimage_copy_partial(NspGrImage *H,NspGrImage *self);
extern NspGrImage * nsp_grimage_full_copy_partial(NspGrImage *H,NspGrImage *self);
extern NspGrImage * nsp_grimage_full_copy(NspGrImage *self);
extern int nsp_grimage_check_values(NspGrImage *H);
extern int int_grimage_create(Stack stack, int rhs, int opt, int lhs);
extern NspGrImage *nsp_grimage_xdr_load_partial(XDR *xdrs, NspGrImage *M);
extern int nsp_grimage_xdr_save(XDR  *xdrs, NspGrImage *M);

#line 10 "codegen/grimage.override"

/* inserted at the end of public part of include file */

#line 107 "./grimage.h"
#endif /* NSP_INC_NspGrImage */ 

#ifdef NspGrImage_Private 
static int init_grimage(NspGrImage *o,NspTypeGrImage *type);
static int nsp_grimage_size(NspGrImage *Mat, int flag);
static char *nsp_grimage_type_as_string(void);
static char *nsp_grimage_type_short_string(NspObject *v);
static int nsp_grimage_eq(NspGrImage *A, NspObject *B);
static int nsp_grimage_neq(NspGrImage *A, NspObject *B);
static NspGrImage *nsp_grimage_xdr_load(XDR *xdrs);
static AttrTab grimage_attrs[];
static NspMethods *grimage_get_methods(void);
/* static int int_grimage_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGrImage *nsp_grimage_create_void(char *name,NspTypeBase *type);
#line 15 "codegen/grimage.override"

/* inserted in the private part of include file */
static void nsp_draw_grimage(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_grimage(NspGraphic *o,const double *tr);
static void nsp_rotate_grimage(NspGraphic *o,double *R);
static void nsp_scale_grimage(NspGraphic *o,double *alpha);
static int nsp_getbounds_grimage(NspGraphic *o,double *bounds);

#line 131 "./grimage.h"
#endif /* NspGrImage_Private */

