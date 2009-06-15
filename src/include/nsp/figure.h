/* -*- Mode: C -*- */
#ifndef NSP_INC_NspFigure
#define NSP_INC_NspFigure

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspFigure */

#include <nsp/graphic.h>

/*
 * NspFigure inherits from Graphic
 */

typedef struct _NspFigure NspFigure ;
typedef struct _NspTypeNspFigure NspTypeNspFigure ;

#line 22 "./figure.h"

struct _NspTypeNspFigure {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./figure.h"
};

typedef struct _nsp_figure nsp_figure;
struct _nsp_figure {
  char* fname;
  char* driver;
  int id;
  NspMatrix* dims;
  NspMatrix* viewport_dims;
  gboolean wresize;
  NspMatrix* position;
  NspList* children;
  gboolean draw_now;
  NspFigureData* data;
  int ref_count;
};

struct _NspFigure {
  /*< private >*/
  NspGraphic father;
  NspTypeNspFigure*type;
  /*< public >*/
  nsp_figure *obj;
};

extern int nsp_type_figure_id;
extern NspTypeNspFigure *nsp_type_figure;

/* type instances for graphic */

NspTypeNspFigure *new_type_figure(type_mode mode);

/* instance for NspFigure */

NspFigure *new_figure();

/*
* Object methods redefined for figure 
*/


#define NULLFIGURE (NspFigure*) 0

extern NspFigure *nsp_figure_create(char *name,char* fname,char* driver,int id,NspMatrix* dims,NspMatrix* viewport_dims,gboolean wresize,NspMatrix* position,NspList* children,gboolean draw_now,NspFigureData* data,NspTypeBase *type);
extern NspFigure *nsp_figure_create_default(char *name);

/* from NspFigureObj.c */

extern NspFigure *nsp_figure_copy(NspFigure *H);
extern void nsp_figure_destroy(NspFigure *H);
extern int nsp_figure_info(NspFigure *H, int indent,const char *name, int rec_level);
extern int nsp_figure_print(NspFigure *H, int indent,const char *name, int rec_level);
extern int nsp_figure_latex(NspFigure *H, int indent,const char *name, int rec_level);
extern NspFigure *nsp_figure_object (NspObject *O); 
extern int IsFigureObj (Stack stack, int i); 
extern int IsFigure(NspObject *O);
extern NspFigure *GetFigureCopy (Stack stack, int i); 
extern NspFigure *GetFigure (Stack stack, int i); 
extern int nsp_figure_create_partial(NspFigure *H);
extern void nsp_figure_destroy_partial(NspFigure *H);
extern NspFigure * nsp_figure_copy_partial(NspFigure *H,NspFigure *self);
extern NspFigure * nsp_figure_full_copy_partial(NspFigure *H,NspFigure *self);
extern NspFigure * nsp_figure_full_copy(NspFigure *self);
extern int nsp_figure_check_values(NspFigure *H);
extern int int_figure_create(Stack stack, int rhs, int opt, int lhs); 
extern NspFigure *nsp_figure_xdr_load_partial(XDR *xdrs, NspFigure *M);
extern int nsp_figure_xdr_save(XDR  *xdrs, NspFigure *M);

#endif /* NSP_INC_NspFigure */ 

#ifdef NspFigure_Private 
static int init_figure(NspFigure *o,NspTypeNspFigure *type);
static int nsp_figure_size(NspFigure *Mat, int flag);
static char *nsp_figure_type_as_string(void);
static char *nsp_figure_type_short_string(NspObject *v);
static int nsp_figure_eq(NspFigure *A, NspObject *B);
static int nsp_figure_neq(NspFigure *A, NspObject *B);
static NspFigure *nsp_figure_xdr_load(XDR *xdrs);
static AttrTab figure_attrs[];
static NspMethods *figure_get_methods(void);
/* static int int_figure_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspFigure *nsp_figure_create_void(char *name,NspTypeBase *type);
#endif /* NspFigure_Private */

