/* -*- Mode: C -*- */
#ifndef NSP_INC_NspFigureData
#define NSP_INC_NspFigureData

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspFigureData */

#include <nsp/object.h>

/*
 * NspFigureData inherits from Object
 */

typedef struct _NspFigureData NspFigureData ;
typedef struct _NspTypeFigureData NspTypeFigureData ;

#line 22 "./figuredata.h"

struct _NspTypeFigureData {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./figuredata.h"
};

struct _NspFigureData {
  /*< private >*/
  NspObject father;
  NspTypeFigureData*type;
  /*< public >*/
    int color;
  int background;
  NspMatrix* colormap;
  int dashes;
  int font;
  int font_size;
  int foreground;
  int hidden3d;
  int line_mode;
  int line_style;
  int mark;
  int mark_size;
  int pattern;
  int pixmap;
  int thickness;
  int use_color;
  gboolean auto_clear;
};

extern int nsp_type_figuredata_id;
extern NspTypeFigureData *nsp_type_figuredata;

/* type instances for object */

NspTypeFigureData *new_type_figuredata(type_mode mode);

/* instance for NspFigureData */

NspFigureData *new_figuredata();

/*
* Object methods redefined for figuredata 
*/


#define NULLFIGUREDATA (NspFigureData*) 0

extern NspFigureData *nsp_figuredata_create(char *name,int color,int background,NspMatrix* colormap,int dashes,int font,int font_size,int foreground,int hidden3d,int line_mode,int line_style,int mark,int mark_size,int pattern,int pixmap,int thickness,int use_color,gboolean auto_clear,NspTypeBase *type);
extern NspFigureData *nsp_figuredata_create_default(char *name);

/* from NspFigureDataObj.c */

extern NspFigureData *nsp_figuredata_copy(NspFigureData *H);
extern void nsp_figuredata_destroy(NspFigureData *H);
extern int nsp_figuredata_info(NspFigureData *H, int indent,const char *name, int rec_level);
extern int nsp_figuredata_print(NspFigureData *H, int indent,const char *name, int rec_level);
extern int nsp_figuredata_latex(NspFigureData *H, int indent,const char *name, int rec_level);
extern NspFigureData *nsp_figuredata_object (NspObject *O);
extern int IsFigureDataObj (Stack stack, int i);
extern int IsFigureData(NspObject *O);
extern NspFigureData *GetFigureDataCopy (Stack stack, int i);
extern NspFigureData *GetFigureData (Stack stack, int i);
extern int nsp_figuredata_create_partial(NspFigureData *H);
extern void nsp_figuredata_destroy_partial(NspFigureData *H);
extern NspFigureData * nsp_figuredata_copy_partial(NspFigureData *H,NspFigureData *self);
extern NspFigureData * nsp_figuredata_full_copy_partial(NspFigureData *H,NspFigureData *self);
extern NspFigureData * nsp_figuredata_full_copy(NspFigureData *self);
extern int nsp_figuredata_check_values(NspFigureData *H);
extern int int_figuredata_create(Stack stack, int rhs, int opt, int lhs);
extern NspFigureData *nsp_figuredata_xdr_load_partial(XDR *xdrs, NspFigureData *M);
extern int nsp_figuredata_xdr_save(XDR  *xdrs, NspFigureData *M);

#endif /* NSP_INC_NspFigureData */ 

#ifdef NspFigureData_Private 
static int init_figuredata(NspFigureData *o,NspTypeFigureData *type);
static int nsp_figuredata_size(NspFigureData *Mat, int flag);
static char *nsp_figuredata_type_as_string(void);
static char *nsp_figuredata_type_short_string(NspObject *v);
static int nsp_figuredata_eq(NspFigureData *A, NspObject *B);
static int nsp_figuredata_neq(NspFigureData *A, NspObject *B);
static NspFigureData *nsp_figuredata_xdr_load(XDR *xdrs);
static AttrTab figuredata_attrs[];
static NspMethods *figuredata_get_methods(void);
/* static int int_figuredata_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspFigureData *nsp_figuredata_create_void(char *name,NspTypeBase *type);
#endif /* NspFigureData_Private */

