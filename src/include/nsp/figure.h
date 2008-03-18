/* -*- Mode: C -*- */
#ifndef NSP_INC_Figure
#define NSP_INC_Figure

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Figure */

#include "nsp/graphic.h"

/*
 * NspFigure inherits from NspGraphic
 */

typedef struct _NspFigure NspFigure ;
typedef struct _NspTypeFigure NspTypeFigure ;


struct _NspTypeFigure {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
  
};

typedef struct _nsp_figure nsp_figure;
struct _nsp_figure {
  NspList* children;
  char* fname;
  gboolean wresize;
  int id;
  int width;
  int height;
  int gr_width;
  int gr_height;
  int x;
  int y;
  int ref_count;
};

struct _NspFigure {
  /*< private >*/
  NspGraphic father;
  NspTypeFigure*type;
  /*< public >*/
  nsp_figure *obj;
};

extern int nsp_type_figure_id;
extern NspTypeFigure *nsp_type_figure;

/* type instances for graphic */

NspTypeFigure *new_type_figure(type_mode mode);

/* instance for Figure */

NspFigure *new_figure();

/*
* Object methods redefined for figure 
*/


#define NULLFIGURE (NspFigure*) 0

extern NspFigure *nsp_figure_create(char *name,NspList* children,char* fname,gboolean wresize,int id,int width,int height,int gr_width,int gr_height,int x,int y,NspTypeBase *type);

/* from FigureObj.c */

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
extern int nsp_figure_check_values(NspFigure *H);
extern int int_figure_create(Stack stack, int rhs, int opt, int lhs); 
extern NspFigure *nsp_figure_xdr_load_partial(XDR *xdrs, NspFigure *M);
extern int nsp_figure_xdr_save(XDR  *xdrs, NspFigure *M);

#endif /* NSP_INC_Figure */ 

#ifdef Figure_Private 
static int init_figure(NspFigure *o,NspTypeFigure *type);
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
#endif /* Figure_Private */

