#ifndef NSP_INC_GridBlock
#define NSP_INC_GridBlock

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* gridblock: a test  */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/gframe.h"
#include "nsp/grint.h" /* interface definition */
#include "nsp/graphics/Graphics.h"

/**
 * NspGridBlock:
 * @obj: a #nsp_gridblock pointer 
 *
 * inherits from #NspObject and implements Grint. 
 * Used for graphic gridblocks for a C implementation of scicos.
 */

typedef struct _NspGridBlock NspGridBlock;

typedef struct _NspTypeGridBlock NspTypeGridBlock;

struct _NspTypeGridBlock { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

/**
 * nsp_gridblock: 
 * @frame: a gridblock must be in a frame to be drawn 
 * @object_sid: 
 * @r: the gridblock enclosing rectangle 
 * @color: color of the gridblock
 * @thickness: thickness of the enclosing rectangle 
 * @background: color of the background
 * @n_locks:  number of lock points 
 * @locks: array of lock points 
 * @hilited: is gridblock hilited 
 * @show: is gridblock to be visible 
 * @ref_count: a reference counter.
 *
 * used for storing information for a graphic gridblock.
 */

#define WITH_GRID_FRAME 


#ifndef WITH_GRID_FRAME
typedef struct _nsp_gridblock nsp_gridblock;

struct _nsp_gridblock {
  int foo;
  int ref_count;
};
#endif 

struct _NspGridBlock {
  /*< private >*/
  NspBlock father; 
  NspTypeGridBlock *type; 
  /*< public >*/
#ifdef WITH_GRID_FRAME
  nspgframe *obj;
#else 
  nsp_gridblock *obj;
#endif
};

extern int nsp_type_gridblock_id;
extern NspTypeGridBlock *nsp_type_gridblock;

NspTypeGridBlock *new_type_gridblock(type_mode mode);
NspGridBlock *new_gridblock();


/* extern NspGridBlock *gridblock_object(NspObject *O);  */
extern int IsGridBlockObj (Stack stack, int i); 
extern NspGridBlock *GetGridBlockCopy (Stack stack, int i); 
extern NspGridBlock *GetGridBlock (Stack stack, int i); 
extern int IsGridBlock (NspObject *O); 
extern NspGridBlock *gridblock_create(char *name,double rect[],int color,int thickness,int background, NspTypeBase *type );
extern int gridblock_translate(NspGridBlock *B,const double tr[2]);
extern int gridblock_set_pos(NspGridBlock *B,const double tr[2]);
extern void gridblock_resize(NspGridBlock *B,const double size[2]);
extern void gridblock_move_control(NspGFrame *F, NspGridBlock *B,const double mpt[2], int cp,double ptc[2]);

#define NULLGRIDBLOCK (NspGridBlock*) 0

#endif 

#ifdef GridBlock_Private /* GridBlock_Private */
static int init_gridblock(NspGridBlock *ob,NspTypeGridBlock *type);
static int gridblock_size(NspGridBlock *Mat, int flag);
static char *gridblock_type_as_string(void);
static char *gridblock_type_short_string(NspObject *v);
static int gridblock_eq(NspGridBlock *A, NspObject *B);
static int gridblock_neq(NspGridBlock *A, NspObject *B);
static int gridblock_xdr_save(XDR  *xdrs, NspGridBlock *M);
static NspGridBlock  *gridblock_xdr_load(XDR  *F);
static AttrTab gridblock_attrs[];
static NspGridBlock *gridblock_object (NspObject *O); 
static NspGridBlock *gridblock_copy (NspGridBlock *H); 
static void gridblock_destroy (NspGridBlock *H); 
static void gridblock_info (NspGridBlock *H, int indent,char *name, int rec_level); 
static int gridblock_print (NspGridBlock *H, int indent,char *name, int rec_level); 
static NspMethods *gridblock_get_methods(void);
static int int_gridblock_create( Stack stack, int rhs, int opt, int lhs); 

/* set of methods for implementing Grint */
static void gridblock_draw (NspGridBlock *R); 
static NspGridBlock * gridblock_full_copy( NspGridBlock *B);

#endif 



