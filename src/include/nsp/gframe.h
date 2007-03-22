#ifndef NSP_INC_GFrame
#define NSP_INC_GFrame

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* graphic frame */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/matrix.h"
#include "nsp/smatrix.h"
#include "nsp/graphics/Graphics.h"

/*
 * NspGFrame inherits from NspObject 
 */

typedef struct _NspGframe NspGFrame;

typedef struct _NspTypeGFrame { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeGFrame;

typedef struct _nsp_gframe nsp_gframe;

struct _nsp_gframe {
  NspList *objs ;       /* Object of type list: list of internal objects */
  BCG *Xgc;
  double scale[4];  /* the scales */
  double r[4] ;     /* frame position in its father */
  int ref_count;
};


struct _NspGframe {
  /*< private >*/
  NspObject father; 
  NspTypeGFrame *type; 
  /*< public >*/
  nsp_gframe *obj;
};

extern int nsp_type_gframe_id;
extern NspTypeGFrame *nsp_type_gframe;

/* only useful when building a new class derived from gframe */

NspTypeGFrame *new_type_gframe(type_mode mode);

NspGFrame *new_gframe();

/*
 * Object methods redefined for gframe 
 */

#ifdef GFrame_Private 
static int init_gframe(NspGFrame *ob,NspTypeGFrame *type);
static int gframe_size(NspGFrame *Mat, int flag);
static char *gframe_type_as_string(void);
static char *gframe_type_short_string(NspObject *v);
static int gframe_eq(NspGFrame *A, NspObject *B);
static int gframe_neq(NspGFrame *A, NspObject *B);
static int gframe_xdr_save(XDR  *F, NspGFrame *M);
static NspGFrame  *gframe_xdr_load(XDR  *F);
static NspObject *gframe_path_extract(NspGFrame *H,int n, NspObject **Objs);
static AttrTab gframe_attrs[];
static NspGFrame *gframe_object (NspObject *O); 
static NspGFrame *gframe_copy (NspGFrame *H); 
static void gframe_destroy (NspGFrame *H); 
static void gframe_info (NspGFrame *H, int indent,char *name, int rec_level); 
static void gframe_print (NspGFrame *H, int indent,char *name, int rec_level); 
static NspMethods *gframe_get_methods(void);
static int int_gframe_create(Stack stack, int rhs, int opt, int lhs);
static NspGFrame *frame_full_copy( NspGFrame *F);

#endif /* GFrame_Private */

#define NULLGFRAME (NspGFrame*) 0

/* from GFrameObj.c */

extern NspGFrame *gframe_object(NspObject *O); 
extern int IsGFrameObj (Stack stack, int i); 
extern NspGFrame *GetGFrameCopy (Stack stack, int i); 
extern NspGFrame *GetGFrame (Stack stack, int i); 
extern int IsGFrame (NspObject *O); 

extern NspObject *gframe_path_extract (NspGFrame *H,int n, NspObject **Objs); 
extern NspGFrame *gframe_create(char *name,BCG *Xgc,int init_objs,const double scale[],double r[],NspTypeBase *type);
extern void gframe_draw (NspGFrame *R); 
extern int gframe_select_obj (NspGFrame *R, const double *pt, NspObject **Objs,NspObject *exclude); 
extern int gframe_select_lock(NspGFrame *F,double pt[2], NspObject **O, int *cp, int *lock_c) ;
extern int gframe_select_and_move (NspGFrame *R, const double *pt); 
extern int gframe_select_and_hilite(NspGFrame *R,const double pt[2]);

typedef enum { MOVE, MOVE_CONTROL } move_action ; 
extern int gframe_move_obj (NspGFrame *R,NspObject *O, const double *pt, int stop, int cp,move_action  action ); 
extern void gframe_unhilite_objs (NspGFrame *R, int draw); 
extern void gframe_delete_hilited (NspGFrame *R); 
extern NspObject* gframe_create_new_block(NspGFrame *R);
extern NspObject* gframe_create_new_connector(NspGFrame *R);
extern NspObject* gframe_create_new_link(NspGFrame *F);
extern int gframe_create_new_rect(NspGFrame *F);
extern int  gframe_hilite_near_pt(NspGFrame *R,const double pt[2]);

extern void gframe_locks_update(NspGFrame *R,NspObject *O);
extern int gframe_select_and_split(NspGFrame *R,const double pt[2]);
extern int gframe_select_link_and_add_control(NspGFrame *R,const double pt[2]);
extern int gframe_select_link_and_remove_control(NspGFrame *R,const double pt[2]);

extern NspObject * gframe_get_hilited(NspGFrame *R) ;
extern void gframe_set_frame_field(NspGFrame *F);


#endif

