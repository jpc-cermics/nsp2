#ifndef NSP_INC_Rect
#define NSP_INC_Rect

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* graphic rectangle */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"
#include "nsp/matrix.h"
#include "nsp/smatrix.h"
#include "nsp/graphics/Graphics.h"

/*
 * NspRect inherits from NspObject 
 */

typedef struct _NspRect NspRect;

typedef struct _NspTypeRect { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeRect;

struct _NspRect {
  /*< private >*/
  NspObject father; 
  NspTypeRect *type; 
  /*< public >*/
  BCG *Xgc;
  double r[4];
  int color; 
  int thickness;
  int background;
};

extern int nsp_type_rect_id;
extern NspTypeRect *nsp_type_rect;

NspTypeRect *new_type_rect(type_mode mode);

NspRect *new_rect();

/*
 * Object methods redefined for rect 
 */

#ifdef Rect_Private 
static int init_rect(NspRect *ob,NspTypeRect *type);
static int rect_size(NspRect *Mat, int flag);
static char *rect_type_as_string(void);
static char *rect_type_short_string(void);
static int rect_eq(NspRect *A, NspObject *B);
static int rect_neq(NspRect *A, NspObject *B);
static int rect_xdr_save(XDR  *F, NspRect *M);
static NspRect  *rect_xdr_load(XDR  *F);
static AttrTab rect_attrs[];
static NspRect *rect_object (NspObject *O); 
static NspRect *rect_copy (NspRect *H); 
static void rect_destroy (NspRect *H); 
static void rect_info (NspRect *H, int indent,char *name, int rec_level); 
static void rect_print (NspRect *H, int indent,char *name, int rec_level); 
static NspMethods *rect_get_methods(void);
#endif /* Rect_Private */

#define NULLRECT (NspRect*) 0

/* from RectObj.c */


extern int IsRectObj (Stack stack, int i); 
extern NspRect *GetRectCopy (Stack stack, int i); 
extern NspRect *GetRect (Stack stack, int i); 
extern int IsRect (NspObject *O); 

/* from rect.c */

extern NspRect *rect_create(char *name,BCG *Xgc, double *rect, int color, int thickness, int background,NspTypeBase *); 
extern void RectDraw(NspRect *R);
extern void RectTranslate(NspRect *R,const double pt[2]);
extern void RectResize(NspRect *R,const double size[2]);

#endif 



