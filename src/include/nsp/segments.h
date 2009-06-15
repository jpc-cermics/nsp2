/* -*- Mode: C -*- */
#ifndef NSP_INC_NspSegments
#define NSP_INC_NspSegments

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspSegments */

#include "nsp/graphic.h"

/*
 * NspSegments inherits from Graphic
 */

typedef struct _NspSegments NspSegments ;
typedef struct _NspTypeNspSegments NspTypeNspSegments ;

#line 22 "./segments.h"

struct _NspTypeNspSegments {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./segments.h"
};

typedef struct _nsp_segments nsp_segments;
struct _nsp_segments {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* color;
  int ref_count;
};

struct _NspSegments {
  /*< private >*/
  NspGraphic father;
  NspTypeNspSegments*type;
  /*< public >*/
  nsp_segments *obj;
};

extern int nsp_type_segments_id;
extern NspTypeNspSegments *nsp_type_segments;

/* type instances for graphic */

NspTypeNspSegments *new_type_segments(type_mode mode);

/* instance for NspSegments */

NspSegments *new_segments();

/*
* Object methods redefined for segments 
*/


#define NULLSEGMENTS (NspSegments*) 0

extern NspSegments *nsp_segments_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* color,NspTypeBase *type);
extern NspSegments *nsp_segments_create_default(char *name);

/* from NspSegmentsObj.c */

extern NspSegments *nsp_segments_copy(NspSegments *H);
extern void nsp_segments_destroy(NspSegments *H);
extern int nsp_segments_info(NspSegments *H, int indent,const char *name, int rec_level);
extern int nsp_segments_print(NspSegments *H, int indent,const char *name, int rec_level);
extern int nsp_segments_latex(NspSegments *H, int indent,const char *name, int rec_level);
extern NspSegments *nsp_segments_object (NspObject *O); 
extern int IsSegmentsObj (Stack stack, int i); 
extern int IsSegments(NspObject *O);
extern NspSegments *GetSegmentsCopy (Stack stack, int i); 
extern NspSegments *GetSegments (Stack stack, int i); 
extern int nsp_segments_create_partial(NspSegments *H);
extern void nsp_segments_destroy_partial(NspSegments *H);
extern NspSegments * nsp_segments_copy_partial(NspSegments *H,NspSegments *self);
extern NspSegments * nsp_segments_full_copy_partial(NspSegments *H,NspSegments *self);
extern NspSegments * nsp_segments_full_copy(NspSegments *self);
extern int nsp_segments_check_values(NspSegments *H);
extern int int_segments_create(Stack stack, int rhs, int opt, int lhs); 
extern NspSegments *nsp_segments_xdr_load_partial(XDR *xdrs, NspSegments *M);
extern int nsp_segments_xdr_save(XDR  *xdrs, NspSegments *M);

#endif /* NSP_INC_NspSegments */ 

#ifdef NspSegments_Private 
static int init_segments(NspSegments *o,NspTypeNspSegments *type);
static int nsp_segments_size(NspSegments *Mat, int flag);
static char *nsp_segments_type_as_string(void);
static char *nsp_segments_type_short_string(NspObject *v);
static int nsp_segments_eq(NspSegments *A, NspObject *B);
static int nsp_segments_neq(NspSegments *A, NspObject *B);
static NspSegments *nsp_segments_xdr_load(XDR *xdrs);
static AttrTab segments_attrs[];
static NspMethods *segments_get_methods(void);
/* static int int_segments_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspSegments *nsp_segments_create_void(char *name,NspTypeBase *type);
#endif /* NspSegments_Private */

