#ifndef INC_NSP_Block
#define INC_NSP_Block

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/
  
/* block */

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/gframe.h"
#include "nsp/grint.h" /* interface definition */

/*
 * NspBlock inherits from NspObject 
 * and implements GRint 
 */

typedef struct _nsp_block NspBlock;

typedef int (*block_save) (NspFile  *F, NspBlock *M);

typedef struct _nsp_type_Block { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  block_save *save;
} NspTypeBlock;

typedef struct b_lock {
  gr_port port;  /* Only one port */
  double pt[2]; /* lock position */
} grb_lock ; 

struct _nsp_block {
  NspObject father; 
  NspTypeBlock *type; 
  double r[4]; 		
  int color;
  int thickness;
  int background;
  int n_locks ; /* number of lock points */
  grb_lock *locks; 
  int hilited ; 
  int show    ;   
};

extern int nsp_type_block_id;
extern NspTypeBlock *nsp_type_block;

NspTypeBlock *new_type_block(type_mode mode);
NspBlock *new_block();

#ifdef Block_Private 
static int init_block(NspBlock *ob,NspTypeBlock *type);
static int block_size(NspBlock *Mat, int flag);
static char *block_type_as_string(void);
static char *block_type_short_string(void);
static int block_eq(NspBlock *A, NspObject *B);
static int block_neq(NspBlock *A, NspObject *B);
static int block_xdr_save(NspFile  *F, NspBlock *M);
static NspBlock  *block_xdr_load(NspFile  *F);
static AttrTab block_attrs[];
static NspBlock *block_object (NspObject *O); 
static NspBlock *block_copy (NspBlock *H); 
static void block_destroy (NspBlock *H); 
static void block_info (NspBlock *H, int indent); 
static void block_print (NspBlock *H, int indent); 
static NspMethods *block_get_methods(void);
static int int_block_create( Stack stack, int rhs, int opt, int lhs); 

/* set of methods for implementing Grint */
static int block_get_hilited (NspBlock *B); 
static void block_set_hilited (NspBlock *B, int val); 
static int block_get_show (NspBlock *B); 
static void block_set_show (NspBlock *B, int val); 
static void block_draw (NspBlock *R); 
static void block_translate (NspBlock *R, const double *pt); 
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
static int block_set_lock_connection(NspBlock *B,int i,const gr_port *p);
static void block_unset_lock_connection(NspBlock *B,int i,int port);
static int block_is_lock_connectable(NspBlock *B,int i);
static int block_is_lock_connected(NspBlock *B,int i);
static void block_set_lock_pos(NspBlock *B, int i,const double pt[]);

#endif /* Block_Private */

#define NULLBLOCK (NspBlock*) 0


extern NspBlock *block_object(NspObject *O); 
extern int IsBlockObj (Stack stack, int i); 
extern NspBlock *GetBlockCopy (Stack stack, int i); 
extern NspBlock *GetBlock (Stack stack, int i); 
extern int IsBlock (NspObject *O); 
extern NspBlock *block_create(char *name,double rect[],int color,int thickness,int background, NspTypeBase *type );

#endif

