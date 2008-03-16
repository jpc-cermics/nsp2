/* -*- Mode: C -*- */
#ifndef NSP_INC_Graphic
#define NSP_INC_Graphic

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Graphic */

#include "nsp/object.h"

/*
 * NspGraphic inherits from NspObject
 */

typedef struct _NspGraphic NspGraphic ;
typedef struct _NspTypeGraphic NspTypeGraphic ;


#line 27 "graphic.override"

typedef void draw_func(BCG *Xgc,NspGraphic *o);
typedef void translate_func(BCG *Xgc,NspGraphic *o,double *tr);
typedef void rotate_func(BCG *Xgc,NspGraphic *o,double *R);
typedef void scale_func(BCG *Xgc,NspGraphic *o,double *alpha);
typedef void bounds_func(BCG *Xgc,NspGraphic *o,double *bounds);


struct _NspTypeGraphic {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
  
#line 18 "graphic.override"

  draw_func *draw; 
  translate_func *translate;
  rotate_func *rotate;
  scale_func *scale;
  bounds_func *bounds;


};

typedef struct _nsp_graphic nsp_graphic;
struct _nsp_graphic {
  int color;
  int ref_count;
};

struct _NspGraphic {
  /*< private >*/
  NspObject father;
  NspTypeGraphic*type;
  /*< public >*/
  nsp_graphic *obj;
};

extern int nsp_type_graphic_id;
extern NspTypeGraphic *nsp_type_graphic;

/* type instances for object */

NspTypeGraphic *new_type_graphic(type_mode mode);

/* instance for Graphic */

NspGraphic *new_graphic();

/*
* Object methods redefined for graphic 
*/


#define NULLGRAPHIC (NspGraphic*) 0

extern NspGraphic *nsp_graphic_create(char *name,int color,NspTypeBase *type);

/* from GraphicObj.c */

extern NspGraphic *nsp_graphic_copy(NspGraphic *H);
extern void nsp_graphic_destroy(NspGraphic *H);
extern void nsp_graphic_info(NspGraphic *H, int indent,const char *name, int rec_level);
extern void nsp_graphic_print(NspGraphic *H, int indent,const char *name, int rec_level);
extern void nsp_graphic_latex(NspGraphic *H, int indent,const char *name, int rec_level);
extern NspGraphic *nsp_graphic_object (NspObject *O); 
extern int IsGraphicObj (Stack stack, int i); 
extern int IsGraphic(NspObject *O);
extern NspGraphic *GetGraphicCopy (Stack stack, int i); 
extern NspGraphic *GetGraphic (Stack stack, int i); 
extern int nsp_graphic_create_partial(NspGraphic *H);
extern void nsp_graphic_destroy_partial(NspGraphic *H);
extern NspGraphic * nsp_graphic_copy_partial(NspGraphic *H,NspGraphic *self);
extern int nsp_graphic_check_values(NspGraphic *H);
extern int int_graphic_create(Stack stack, int rhs, int opt, int lhs); 
extern NspGraphic *nsp_graphic_xdr_load_partial(XDR *xdrs, NspGraphic *M);
extern int nsp_graphic_xdr_save(XDR  *xdrs, NspGraphic *M);

#endif /* NSP_INC_Graphic */ 

#ifdef Graphic_Private 
static int init_graphic(NspGraphic *o,NspTypeGraphic *type);
static int nsp_graphic_size(NspGraphic *Mat, int flag);
static char *nsp_graphic_type_as_string(void);
static char *nsp_graphic_type_short_string(NspObject *v);
static int nsp_graphic_eq(NspGraphic *A, NspObject *B);
static int nsp_graphic_neq(NspGraphic *A, NspObject *B);
static NspGraphic *nsp_graphic_xdr_load(XDR *xdrs);
static AttrTab graphic_attrs[];
static NspMethods *graphic_get_methods(void);
/* static int int_graphic_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGraphic *nsp_graphic_create_void(char *name,NspTypeBase *type);
#endif /* Graphic_Private */

