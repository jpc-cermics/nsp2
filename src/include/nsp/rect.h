#ifndef INC_NSP_Rect
#define INC_NSP_Rect

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/
  
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

typedef struct _nsp_rect NspRect;

typedef int (*rect_save) (NspFile  *F, NspRect *M);

typedef struct _nsp_type_Rect { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  rect_save *save;
} NspTypeRect;

struct _nsp_rect {
  NspObject father; 
  NspTypeRect *type; 
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
static int rect_xdr_save(NspFile  *F, NspRect *M);
static NspRect  *rect_xdr_load(NspFile  *F);
static AttrTab rect_attrs[];
static NspRect *rect_object (NspObject *O); 
static NspRect *rect_copy (NspRect *H); 
static void rect_destroy (NspRect *H); 
static void rect_info (NspRect *H, int indent); 
static void rect_print (NspRect *H, int indent); 
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



