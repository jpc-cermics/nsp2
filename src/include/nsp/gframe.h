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

/**
 * NspGFrame:
 * @obj: a pointer to a #nsp_gframe
 *
 * inherits from #NspObject and is used 
 * for scicos diagrams. 
 */

typedef struct _NspGFrame NspGFrame;

typedef struct _NspTypeGFrame { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeGFrame;

/**
 * nspgframe: 
 * @objs: a #NspList of objects contained in the frame
 * @Xgc: graphic context to be used for drawing,
 * @scale: scales to be used inside the frame as an array of size four.
 * @r: frame position in its father as an array of size four.
 * @ref_count: a reference counter
 *
 * internal structure which is used to keep a list of objects 
 * associated to a graphic context. #nspgframe object are 
 * used at nsp level through the use of #NspGFrame objets. 
 */

typedef struct _nspgframe nspgframe;

struct _nspgframe {
  NspList *objs ;   /* Object of type list: list of internal objects */
  BCG *Xgc;         /* graphic context to ne used */
  double scale[4];  /* the scales */
  double r[4] ;     /* frame position in its father as relative numbers */
  int top;
  int ref_count;
};

/**
 * NspGFrame: 
 * @obj: a #nspgframe. 
 *
 * inherits from #NspObject and is used 
 * for scicos diagrams. It contains a #nsp_gframe and the semantic of 
 * copy is by reference. 
 */

struct _NspGFrame {
  /*< private >*/
  NspObject father; 
  NspTypeGFrame *type; 
  /*< public >*/
  nspgframe *obj;
};

extern int nsp_type_gframe_id;
extern NspTypeGFrame *nsp_type_gframe;

/* only useful when building a new class derived from gframe */

NspTypeGFrame *new_type_gframe(type_mode mode);

NspGFrame *new_gframe();

/*
 * Object methods redefined for gframe 
 */

#define NULLGFRAME (NspGFrame*) 0

/* from GFrameObj.c */

/* extern NspGFrame *gframe_object(NspObject *O);  */
extern int IsGFrameObj (Stack stack, int i); 
extern NspGFrame *GetGFrameCopy (Stack stack, int i); 
extern NspGFrame *GetGFrame (Stack stack, int i); 
extern int IsGFrame (NspObject *O); 

/* extern NspObject *gframe_path_extract (NspGFrame *H,int n, NspObject **Objs);  */
extern NspGFrame *nsp_gframe_create(char *name,BCG *Xgc,int init_objs,const double scale[],double r[],NspTypeBase *type);
extern void nsp_gframe_draw (NspGFrame *R); 
extern int nsp_gframe_select_obj (NspGFrame *R, const double *pt, NspObject **Objs,NspObject *exclude); 
extern int nsp_gframe_select_lock(NspGFrame *F,double pt[2], NspObject **O, int *cp, int *lock_c) ;
extern int nsp_gframe_select_and_move (NspGFrame *R, const double *pt, int mask); 
extern int nsp_gframe_select_and_hilite(NspGFrame *R,const double pt[2]);
extern int nsp_gframe_select_and_toggle_hilite(NspGFrame *R,const double pt[2]);

typedef enum { MOVE, MOVE_CONTROL } move_action ; 
extern int nsp_gframe_move_obj (NspGFrame *R,NspObject *O, const double *pt, int stop, int cp,move_action  action ); 
extern void nsp_gframe_unhilite_objs (NspGFrame *R, int draw); 
extern void nsp_gframe_delete_hilited (NspGFrame *R); 
extern NspObject* nsp_gframe_create_new_block(NspGFrame *R);
extern NspObject* nsp_gframe_create_new_connector(NspGFrame *R);
extern NspObject* nsp_gframe_create_new_link(NspGFrame *F);
extern NspObject * nsp_gframe_create_new_gridblock(NspGFrame *F, int flag);
extern int nsp_gframe_create_new_rect(NspGFrame *F);
extern int  nsp_gframe_hilite_near_pt(NspGFrame *R,const double pt[2]);

extern void nsp_gframe_locks_update(NspGFrame *R,NspObject *O);
extern int nsp_gframe_select_and_split(NspGFrame *R,const double pt[2]);
extern int nsp_gframe_select_link_and_add_control(NspGFrame *R,const double pt[2]);
extern int nsp_gframe_select_link_and_remove_control(NspGFrame *R,const double pt[2]);

extern NspObject * nsp_gframe_get_hilited(NspGFrame *R) ;
extern void nsp_gframe_set_frame_field(NspGFrame *F);
extern NspGFrame *nsp_gframe_full_copy( NspGFrame *F);
extern NspGFrame *nsp_gframe_from_nspgframe(char *name,BCG *Xgc, nspgframe *gf);
extern void nsp_gframe_destroy (NspGFrame *H); 
extern NspList *nsp_gframe_get_hilited_list(nspgframe *gf, int full_copy);
extern int nsp_gframe_select_and_move_list(NspGFrame *R,NspObject *Obj,const double pt[2], int *click);
extern int nsp_gframe_move_list_obj(NspGFrame *F,NspList *L,const double pt[2],int stop,int cp,
				    move_action action, int *click);

/* functions for nspgframe */

extern void nspgframe_draw(nspgframe *gf);
extern void nspgframe_set_frame_field(nspgframe *gf);
extern void *nspgframe_get_adress(NspList *L,void *old );
extern nspgframe *nspgframe_full_copy(nspgframe *gf,int hilited_only);
extern NspGFrame *nsp_gframe_hilited_full_copy( NspGFrame *F);

#endif

#ifdef GFrame_Private 
/* private functions */
static int init_gframe(NspGFrame *ob,NspTypeGFrame *type);
static int nsp_gframe_size(NspGFrame *Mat, int flag);
static char *nsp_gframe_type_as_string(void);
static char *nsp_gframe_type_short_string(NspObject *v);
static int nsp_gframe_eq(NspGFrame *A, NspObject *B);
static int nsp_gframe_neq(NspGFrame *A, NspObject *B);
static int nsp_gframe_xdr_save(XDR  *F, NspGFrame *M);
static NspGFrame  *nsp_gframe_xdr_load(XDR  *F);
static NspObject *nsp_gframe_path_extract(NspGFrame *H,int n, NspObject **Objs, int *copy);
static AttrTab nsp_gframe_attrs[];
static NspGFrame *nsp_gframe_object (NspObject *O); 
static NspGFrame *nsp_gframe_copy (NspGFrame *H); 
static int nsp_gframe_info (NspGFrame *H, int indent,char *name, int rec_level); 
static int nsp_gframe_print (NspGFrame *H, int indent,char *name, int rec_level); 
static NspMethods *nsp_gframe_get_methods(void);
static int int_nsp_gframe_create(Stack stack, int rhs, int opt, int lhs);
static void nspgframe_recompute_pointers(nspgframe *gf);
static void nsp_gframe_list_recompute_pointers(NspList *L);



#endif /* GFrame_Private */

