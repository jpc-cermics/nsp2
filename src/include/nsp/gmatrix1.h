/* -*- Mode: C -*- */
#ifndef NSP_INC_GMatrix1
#define NSP_INC_GMatrix1

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* GMatrix1 */

#include "nsp/graphic.h"

/*
 * NspGMatrix1 inherits from NspGraphic
 */

typedef struct _NspGMatrix1 NspGMatrix1 ;
typedef struct _NspTypeGMatrix1 NspTypeGMatrix1 ;

#line 22 "./gmatrix1.h"

struct _NspTypeGMatrix1 {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./gmatrix1.h"
};

typedef struct _nsp_gmatrix1 nsp_gmatrix1;
struct _nsp_gmatrix1 {
  NspMatrix* data;
  gboolean remap;
  gboolean shade;
  NspMatrix* colminmax;
  NspMatrix* zminmax;
  NspMatrix* x;
  NspMatrix* y;
  int ref_count;
};

struct _NspGMatrix1 {
  /*< private >*/
  NspGraphic father;
  NspTypeGMatrix1*type;
  /*< public >*/
  nsp_gmatrix1 *obj;
};

extern int nsp_type_gmatrix1_id;
extern NspTypeGMatrix1 *nsp_type_gmatrix1;

/* type instances for graphic */

NspTypeGMatrix1 *new_type_gmatrix1(type_mode mode);

/* instance for GMatrix1 */

NspGMatrix1 *new_gmatrix1();

/*
* Object methods redefined for gmatrix1 
*/


#define NULLGMATRIX1 (NspGMatrix1*) 0

extern NspGMatrix1 *nsp_gmatrix1_create(char *name,NspMatrix* data,gboolean remap,gboolean shade,NspMatrix* colminmax,NspMatrix* zminmax,NspMatrix* x,NspMatrix* y,NspTypeBase *type);
extern NspGMatrix1 *nsp_gmatrix1_create_default(char *name);

/* from GMatrix1Obj.c */

extern NspGMatrix1 *nsp_gmatrix1_copy(NspGMatrix1 *H);
extern void nsp_gmatrix1_destroy(NspGMatrix1 *H);
extern int nsp_gmatrix1_info(NspGMatrix1 *H, int indent,const char *name, int rec_level);
extern int nsp_gmatrix1_print(NspGMatrix1 *H, int indent,const char *name, int rec_level);
extern int nsp_gmatrix1_latex(NspGMatrix1 *H, int indent,const char *name, int rec_level);
extern NspGMatrix1 *nsp_gmatrix1_object (NspObject *O); 
extern int IsGMatrix1Obj (Stack stack, int i); 
extern int IsGMatrix1(NspObject *O);
extern NspGMatrix1 *GetGMatrix1Copy (Stack stack, int i); 
extern NspGMatrix1 *GetGMatrix1 (Stack stack, int i); 
extern int nsp_gmatrix1_create_partial(NspGMatrix1 *H);
extern void nsp_gmatrix1_destroy_partial(NspGMatrix1 *H);
extern NspGMatrix1 * nsp_gmatrix1_copy_partial(NspGMatrix1 *H,NspGMatrix1 *self);
extern NspGMatrix1 * nsp_gmatrix1_full_copy_partial(NspGMatrix1 *H,NspGMatrix1 *self);
extern NspGMatrix1 * nsp_gmatrix1_full_copy(NspGMatrix1 *self);
extern int nsp_gmatrix1_check_values(NspGMatrix1 *H);
extern int int_gmatrix1_create(Stack stack, int rhs, int opt, int lhs); 
extern NspGMatrix1 *nsp_gmatrix1_xdr_load_partial(XDR *xdrs, NspGMatrix1 *M);
extern int nsp_gmatrix1_xdr_save(XDR  *xdrs, NspGMatrix1 *M);

#endif /* NSP_INC_GMatrix1 */ 

#ifdef GMatrix1_Private 
static int init_gmatrix1(NspGMatrix1 *o,NspTypeGMatrix1 *type);
static int nsp_gmatrix1_size(NspGMatrix1 *Mat, int flag);
static char *nsp_gmatrix1_type_as_string(void);
static char *nsp_gmatrix1_type_short_string(NspObject *v);
static int nsp_gmatrix1_eq(NspGMatrix1 *A, NspObject *B);
static int nsp_gmatrix1_neq(NspGMatrix1 *A, NspObject *B);
static NspGMatrix1 *nsp_gmatrix1_xdr_load(XDR *xdrs);
static AttrTab gmatrix1_attrs[];
static NspMethods *gmatrix1_get_methods(void);
/* static int int_gmatrix1_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGMatrix1 *nsp_gmatrix1_create_void(char *name,NspTypeBase *type);
#endif /* GMatrix1_Private */

