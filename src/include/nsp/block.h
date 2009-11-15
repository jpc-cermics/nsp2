/* -*- Mode: C -*- */
#ifndef NSP_INC_NspBlock
#define NSP_INC_NspBlock

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#line 4 "codegen/block.override"
/* inserted at the start of include file */

#include <stdio.h>   /* for file declaration */
#include "nsp/grint.h" /* interface definition */

/**
 * NspBlock:
 * @obj: a #nsp_block pointer 
 *
 * inherits from #NspObject and implements Grint. 
 * Used for graphic blocks for a C implementation of scicos.
 */

/**
 * grb_lock: 
 *  @port: a #gr_port. 
 *  @pt: position of the lock point in the frame
 *  @ptr: relative position of the lock point in the block
 *  @type: type of the lock point.
 *
 * used for storing information about lock points of a block. 
 * 
 */

typedef struct b_lock grb_lock ; 

struct b_lock {
  gr_port port;  /* Only one port */
  double pt[2]; /* lock position in the frame*/
  double ptr[2]; /* lock position (relative position in the block)*/
  int type ; /* type of the lock point */
};

/**
 * nsp_block: 
 * @frame: a block must be in a frame to be drawn 
 * @object_sid: 
 * @r: the block enclosing rectangle 
 * @color: color of the block
 * @thickness: thickness of the enclosing rectangle 
 * @background: color of the background
 * @n_locks:  number of lock points 
 * @locks: array of lock points 
 * @hilited: is block hilited 
 * @show: is block to be visible 
 * @ref_count: a reference counter.
 *
 * used for storing information for a graphic block.
 */


#line 63 "./block.h"
/* NspBlock */

#include <nsp/graphic.h>

/*
 * NspBlock inherits from Graphic
 */

typedef struct _NspBlock NspBlock ;
typedef struct _NspTypeBlock NspTypeBlock ;

#line 75 "./block.h"

struct _NspTypeBlock {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 82 "./block.h"
};

typedef struct _nsp_block nsp_block;
struct _nsp_block {
  void* object_sid;
  double r[4];
  int color;
  int thickness;
  int background;
  int n_locks;
  grb_lock* locks;
  NspGraphic* icon;
  int draw_mode;
  int ref_count;
};

struct _NspBlock {
  /*< private >*/
  NspGraphic father;
  NspTypeBlock*type;
  /*< public >*/
  nsp_block *obj;
};

extern int nsp_type_block_id;
extern NspTypeBlock *nsp_type_block;

/* type instances for graphic */

NspTypeBlock *new_type_block(type_mode mode);

/* instance for NspBlock */

NspBlock *new_block();

/*
 * Object methods redefined for block 
 */


#define NULLBLOCK (NspBlock*) 0

extern NspBlock *nsp_block_create(char *name,void* object_sid,double* r,int color,int thickness,int background,int n_locks,grb_lock* locks,NspGraphic* icon,int draw_mode,NspTypeBase *type);
extern NspBlock *nsp_block_create_default(char *name);

/* from NspBlockObj.c */

extern NspBlock *nsp_block_copy(NspBlock *H);
extern void nsp_block_destroy(NspBlock *H);
extern int nsp_block_info(NspBlock *H, int indent,const char *name, int rec_level);
extern int nsp_block_print(NspBlock *H, int indent,const char *name, int rec_level);
extern int nsp_block_latex(NspBlock *H, int indent,const char *name, int rec_level);
extern NspBlock *nsp_block_object (NspObject *O);
extern int IsBlockObj (Stack stack, int i);
extern int IsBlock(NspObject *O);
extern NspBlock *GetBlockCopy (Stack stack, int i);
extern NspBlock *GetBlock (Stack stack, int i);
extern int nsp_block_create_partial(NspBlock *H);
extern void nsp_block_destroy_partial(NspBlock *H);
extern NspBlock * nsp_block_copy_partial(NspBlock *H,NspBlock *self);
extern NspBlock * nsp_block_full_copy_partial(NspBlock *H,NspBlock *self);
extern NspBlock * nsp_block_full_copy(NspBlock *self);
extern int nsp_block_check_values(NspBlock *H);
extern int int_block_create(Stack stack, int rhs, int opt, int lhs);
extern NspBlock *nsp_block_xdr_load_partial(XDR *xdrs, NspBlock *M);
extern int nsp_block_xdr_save(XDR  *xdrs, NspBlock *M);

#line 57 "codegen/block.override"

/* inserted at the end of public part of include file */

#line 154 "./block.h"
#endif /* NSP_INC_NspBlock */ 

#ifdef NspBlock_Private 
static int init_block(NspBlock *o,NspTypeBlock *type);
static int nsp_block_size(NspBlock *Mat, int flag);
static char *nsp_block_type_as_string(void);
static char *nsp_block_type_short_string(NspObject *v);
static int nsp_block_eq(NspBlock *A, NspObject *B);
static int nsp_block_neq(NspBlock *A, NspObject *B);
static NspBlock *nsp_block_xdr_load(XDR *xdrs);
static AttrTab block_attrs[];
static NspMethods *block_get_methods(void);
/* static int int_block_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspBlock *nsp_block_create_void(char *name,NspTypeBase *type);
#line 62 "codegen/block.override"

static double lock_size=1; /*  XXX a factoriser quelque part ... */ 
static int lock_color=10;

static int nsp_block_create_icon(BCG *Xgc,NspBlock *B);

/* set of method for parent class graphic  */
static void nsp_draw_block(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_block(NspGraphic *o,const double *tr);
static void nsp_rotate_block(NspGraphic *o,double *R);
static void nsp_scale_block(NspGraphic *o,double *alpha);
static int nsp_getbounds_block(NspGraphic *o,double *bounds);
static void nsp_block_link_figure(NspGraphic *G, void *F, void *A);
static void nsp_block_unlink_figure(NspGraphic *G, void *F);

/* set of methods for implementing Grint */
static int block_get_hilited (NspBlock *B); 
static void block_set_hilited (NspBlock *B, int val); 
static int block_get_show (NspBlock *B); 
static void block_set_show (NspBlock *B, int val); 
static int block_set_pos (NspBlock *R, const double *pt); 
static void block_get_pos (NspBlock *R, double *pt); 
static void block_resize (NspBlock *R, const double *size); 
static void block_update_locks (NspBlock *R); 
static int block_contains_pt (const NspBlock *B, const double *pt); 
static int block_control_near_pt (const NspBlock *B, const double *pt, int *cp); 
static int block_lock_near_pt (const NspBlock *B, double *pt, int *cp); 
static void block_move_control_init( NspBlock *B,int cp,double ptc[2]);
static void block_move_control (void *F,NspBlock *B, const double *pt, int cp,double ptc[2]); 

static int block_get_number_of_locks(const NspBlock *B) ;
static int block_get_number_of_ports(const NspBlock *B,int lp) ;
static int block_get_lock_connection(const NspBlock *B,int i,int port, gr_port *p );
static void block_get_lock_pos(const NspBlock *B,int i,double pt[]);
static lock_dir block_get_lock_dir(const NspBlock *B,int i);

static int block_set_lock_connection(NspBlock *B,int i,int prt,const gr_port *p);
static void block_unset_lock_connection(NspBlock *B,int i,int port);
static int block_is_lock_connectable(NspBlock *B,int i);
static int block_is_lock_connected(NspBlock *B,int i);
static void block_set_lock_pos(NspBlock *B, int i,const double pt[],int keep_angle,lock_dir dir);
static void block_set_lock_pos_rel(NspBlock *B, int i,const double pt[]);

static void block_unlock( NspBlock *B,int lp) ;
static int block_set_locks(NspBlock *B,NspMatrix *Pt);

/* requested for grb_lock */

static void nsp_destroy_grb_lock(grb_lock *locks,NspBlock *H);
static int nsp_save_grb_lock(XDR *xdrs,grb_lock *locks,NspBlock *M);
static int nsp_load_grb_lock(XDR *xdrs,grb_lock *locks,NspBlock *M);
static int nsp_print_grb_lock(int indent,grb_lock *locks,NspBlock *M);
static int nsp_check_grb_lock(grb_lock *locks,NspBlock *M);
static int nsp_grb_lock_full_copy(NspBlock *C,grb_lock *locks,NspBlock *M);

/* local */
static void lock_draw(BCG *Xgc,const double pt[2],lock_dir dir,lock_type typ,int locked);

#line 228 "./block.h"
#endif /* NspBlock_Private */

