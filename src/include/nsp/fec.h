/* -*- Mode: C -*- */
#ifndef NSP_INC_NspFec
#define NSP_INC_NspFec

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspFec */

#include "nsp/graphic.h"

/*
 * NspFec inherits from Graphic
 */

typedef struct _NspFec NspFec ;
typedef struct _NspTypeNspFec NspTypeNspFec ;

#line 22 "./fec.h"

struct _NspTypeNspFec {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./fec.h"
};

typedef struct _nsp_fec nsp_fec;
struct _nsp_fec {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* triangles;
  NspMatrix* func;
  NspMatrix* colminmax;
  NspMatrix* zminmax;
  gboolean draw;
  NspMatrix* colout;
  int ref_count;
};

struct _NspFec {
  /*< private >*/
  NspGraphic father;
  NspTypeNspFec*type;
  /*< public >*/
  nsp_fec *obj;
};

extern int nsp_type_fec_id;
extern NspTypeNspFec *nsp_type_fec;

/* type instances for graphic */

NspTypeNspFec *new_type_fec(type_mode mode);

/* instance for NspFec */

NspFec *new_fec();

/*
* Object methods redefined for fec 
*/


#define NULLFEC (NspFec*) 0

extern NspFec *nsp_fec_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* triangles,NspMatrix* func,NspMatrix* colminmax,NspMatrix* zminmax,gboolean draw,NspMatrix* colout,NspTypeBase *type);
extern NspFec *nsp_fec_create_default(char *name);

/* from NspFecObj.c */

extern NspFec *nsp_fec_copy(NspFec *H);
extern void nsp_fec_destroy(NspFec *H);
extern int nsp_fec_info(NspFec *H, int indent,const char *name, int rec_level);
extern int nsp_fec_print(NspFec *H, int indent,const char *name, int rec_level);
extern int nsp_fec_latex(NspFec *H, int indent,const char *name, int rec_level);
extern NspFec *nsp_fec_object (NspObject *O); 
extern int IsFecObj (Stack stack, int i); 
extern int IsFec(NspObject *O);
extern NspFec *GetFecCopy (Stack stack, int i); 
extern NspFec *GetFec (Stack stack, int i); 
extern int nsp_fec_create_partial(NspFec *H);
extern void nsp_fec_destroy_partial(NspFec *H);
extern NspFec * nsp_fec_copy_partial(NspFec *H,NspFec *self);
extern NspFec * nsp_fec_full_copy_partial(NspFec *H,NspFec *self);
extern NspFec * nsp_fec_full_copy(NspFec *self);
extern int nsp_fec_check_values(NspFec *H);
extern int int_fec_create(Stack stack, int rhs, int opt, int lhs); 
extern NspFec *nsp_fec_xdr_load_partial(XDR *xdrs, NspFec *M);
extern int nsp_fec_xdr_save(XDR  *xdrs, NspFec *M);

#endif /* NSP_INC_NspFec */ 

#ifdef NspFec_Private 
static int init_fec(NspFec *o,NspTypeNspFec *type);
static int nsp_fec_size(NspFec *Mat, int flag);
static char *nsp_fec_type_as_string(void);
static char *nsp_fec_type_short_string(NspObject *v);
static int nsp_fec_eq(NspFec *A, NspObject *B);
static int nsp_fec_neq(NspFec *A, NspObject *B);
static NspFec *nsp_fec_xdr_load(XDR *xdrs);
static AttrTab fec_attrs[];
static NspMethods *fec_get_methods(void);
/* static int int_fec_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspFec *nsp_fec_create_void(char *name,NspTypeBase *type);
#endif /* NspFec_Private */

