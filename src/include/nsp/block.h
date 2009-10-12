#ifndef NSP_INC_Block
#define NSP_INC_Block

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* block: graphic bloc for scicos  */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/gframe.h"
#include "nsp/grint.h" /* interface definition */
#include "nsp/graphics-old/Graphics.h"

#include "nsp/graphic.h"

/**
 * NspBlock:
 * @obj: a #nsp_block pointer 
 *
 * inherits from #NspObject and implements Grint. 
 * Used for graphic blocks for a C implementation of scicos.
 */

typedef struct _NspBlock NspBlock;

typedef struct _NspTypeBlock NspTypeBlock;

struct _NspTypeBlock { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

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

typedef struct _nsp_block nsp_block;

struct _nsp_block {
  nspgframe *frame;/* a block must be in a frame to be drawn */
  void *object_sid;
  double r[4]; 		
  int color;
  int thickness;
  int background;
  int n_locks ; /* number of lock points */
  grb_lock *locks; 
  int hilited ; 
  int show    ;   
  int ref_count;
};



struct _NspBlock {
  /*< private >*/
  NspGraphic father;
  NspTypeBlock *type; 
  /*< public >*/
  nsp_block *obj;
};

extern int nsp_type_block_id;
extern NspTypeBlock *nsp_type_block;

NspTypeBlock *new_type_block(type_mode mode);
NspBlock *new_block();


/* extern NspBlock *block_object(NspObject *O);  */
extern int IsBlockObj (Stack stack, int i); 
extern NspBlock *GetBlockCopy (Stack stack, int i); 
extern NspBlock *GetBlock (Stack stack, int i); 
extern int IsBlock (NspObject *O); 
extern NspBlock *block_create(char *name,double rect[],int color,int thickness,int background, NspTypeBase *type );
extern NspBlock *nsp_block_create(NspBlock *H,double *rect,int color,int thickness,int background);

#define NULLBLOCK (NspBlock*) 0

#endif 

#ifdef NspBlock_Private /* Block_Private */
static int init_block(NspBlock *ob,NspTypeBlock *type);
static int nsp_block_size(NspBlock *Mat, int flag);
static char *nsp_block_type_as_string(void);
static char *nsp_block_type_short_string(NspObject *v);
static int nsp_block_eq(NspBlock *A, NspObject *B);
static int nsp_block_neq(NspBlock *A, NspObject *B);
static int nsp_block_xdr_save(XDR  *xdrs, NspBlock *M);
static NspBlock  *nsp_block_xdr_load(XDR  *F);
static AttrTab block_attrs[];
static NspBlock *nsp_block_object (NspObject *O); 
static NspBlock *nsp_block_copy (NspBlock *H); 
static void nsp_block_destroy (NspBlock *H); 
static int nsp_block_info (NspBlock *H, int indent,char *name, int rec_level); 
static int nsp_block_print (NspBlock *H, int indent,char *name, int rec_level); 
static int nsp_block_latex (NspBlock *H, int indent,char *name, int rec_level); 
static NspMethods *block_get_methods(void);
static int int_block_create( Stack stack, int rhs, int opt, int lhs); 
static NspBlock *nsp_block_create_void(char *name,NspTypeBase *type);

/* set of method for parent class graphic  */
static void nsp_draw_block(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_block(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_block(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_block(BCG *Xgc,NspGraphic *o,double *alpha);
static int nsp_getbounds_block(BCG *Xgc,NspGraphic *o,double *bounds);

/* set of methods for implementing Grint */
static int block_get_hilited (NspBlock *B); 
static void block_set_hilited (NspBlock *B, int val); 
static int block_get_show (NspBlock *B); 
static void block_set_show (NspBlock *B, int val); 
static void block_draw (NspBlock *R); 
static int block_translate (NspBlock *R, const double *pt); 
static int block_set_pos (NspBlock *R, const double *pt); 
static void block_get_pos (NspBlock *R, double *pt); 
static void block_get_rect (NspBlock *R, double *r); 
static void block_resize (NspBlock *R, const double *size); 
static void block_update_locks (NspBlock *R); 
static int block_contains_pt (const NspBlock *B, const double *pt); 
static int block_control_near_pt (const NspBlock *B, const double *pt, int *cp); 
static int block_lock_near_pt (const NspBlock *B, double *pt, int *cp); 
static void block_move_control_init( NspBlock *B,int cp,double ptc[2]);
static void block_move_control (NspGFrame *F,NspBlock *B, const double *pt, int cp,double ptc[2]); 

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
static NspBlock * block_full_copy( NspBlock *B);
static void block_set_frame( NspBlock *B, NspGFrame *Gf);

#endif 



