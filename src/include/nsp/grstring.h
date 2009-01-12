/* -*- Mode: C -*- */
#ifndef NSP_INC_Grstring
#define NSP_INC_Grstring

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Grstring */

#include "nsp/graphic.h"

/*
 * NspGrstring inherits from NspGraphic
 */

typedef struct _NspGrstring NspGrstring ;
typedef struct _NspTypeGrstring NspTypeGrstring ;

#line 22 "./grstring.h"

struct _NspTypeGrstring {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./grstring.h"
};

typedef struct _nsp_grstring nsp_grstring;
struct _nsp_grstring {
  double x;
  double y;
  char* font;
  NspSMatrix* text;
  int position;
  double angle;
  double w;
  double h;
  int fill;
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

/* instance for Grstring */

NspGrstring *new_grstring();

/*
* Object methods redefined for grstring 
*/


#define NULLGRSTRING (NspGrstring*) 0

extern NspGrstring *nsp_grstring_create(char *name,double x,double y,char* font,NspSMatrix* text,int position,double angle,double w,double h,int fill,NspTypeBase *type);

/* from GrstringObj.c */

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

#endif /* NSP_INC_Grstring */ 

#ifdef Grstring_Private 
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
static NspGrstring *nsp_grstring_create_void(char *name,NspTypeBase *type);
#endif /* Grstring_Private */

