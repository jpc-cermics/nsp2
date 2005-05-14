#ifndef NSP_INC_Link
#define NSP_INC_Link

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* graphic links */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/gframe.h" 
#include "nsp/graphics/Graphics.h"

/*
 * NspLink inherits from NspObject 
 */

typedef struct _NspLink NspLink;

typedef struct _NspTypeLink { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeLink;

typedef struct l_lock {
  gr_port port;  /* Only one port */
} grl_lock ; 

struct _NspLink {
  /*< private >*/
  NspObject father; 
  NspTypeLink *type; 
  /*< public >*/
  /* specific*/
  BCG *Xgc;
  int color; 
  int thickness;
  NspMatrix *poly ;       /* the polyline */
  grl_lock locks[2];      /* two lock points */
  int hilited ; 
  int show    ; 
};

extern int nsp_type_link_id;
extern NspTypeLink *nsp_type_link;

NspTypeLink *new_type_link(type_mode mode);

NspLink *new_link();

/*
 * Object methods redefined for link 
 */

#ifdef Link_Private 
static int init_link(NspLink *ob,NspTypeLink *type);
static int link_size(NspLink *Mat, int flag);
static char *link_type_as_string(void);
static char *link_type_short_string(void);
static int link_eq(NspLink *A, NspObject *B);
static int link_neq(NspLink *A, NspObject *B);
static int link_xdr_save(NspFile  *F, NspLink *M);
static NspLink  *link_xdr_load(NspFile  *F);
static AttrTab link_attrs[];
static NspLink *link_object (NspObject *O); 
static NspLink *link_copy (NspLink *H); 
static void link_destroy (NspLink *H); 
static void link_info (NspLink *H, int indent); 
static void link_print (NspLink *H, int indent); 
static NspMethods *link_get_methods(void);

static int link_get_hilited (NspLink *B); 
static void link_set_hilited (NspLink *B, int val); 
static int link_get_show (NspLink *B); 
static void link_set_show (NspLink *B, int val); 
static void link_draw (NspLink *R); 
static void link_translate (NspLink *R, const double *pt); 
static void link_resize (NspLink *R, const double *size); 
static void link_update_locks (NspLink *R); 
static int link_contains_pt (const NspLink *B, const double *pt); 
static int link_control_near_pt (const NspLink *B, const double *pt, int *cp); 
static int link_lock_near_pt (const NspLink *B, const double *pt, int *cp); 
static void link_move_control_init( NspLink *B,int cp,double ptc[2]);
static void link_move_control (NspGFrame *F,NspLink *B, const double pt[2], int cp,double w[2]); 

static int link_get_number_of_locks(const NspLink *B) ;
static int link_get_number_of_ports(const NspLink *B,int lp) ;
static int link_get_lock_connection(const NspLink *B,int i,int port, gr_port *p );
static void link_get_lock_pos(const NspLink *B,int i,double pt[]);
static int link_set_lock_connection(NspLink *B,int i,const gr_port *p);
static void link_unset_lock_connection(NspLink *B,int i,int port);
static int link_is_lock_connectable(NspLink *B,int i);
static int link_is_lock_connected(NspLink *B,int i);
static void link_set_lock_pos(NspLink *B, int i,const double pt[]);

static int int_link_create(Stack stack, int rhs, int opt, int lhs);
#endif /* Link_Private */

#define NULLLINK (NspLink*) 0

/* from LinkObj.c */

extern int IsLinkObj (Stack stack, int i); 
extern NspLink *GetLinkCopy (Stack stack, int i); 
extern NspLink *GetLink (Stack stack, int i); 
extern NspLink *LinkCreateN(char *name,int n,int color,int thickness);
extern void link_lock_update(NspGFrame *F, NspLink *L,int lp,double ptnew[2]);
extern int link_split(NspGFrame *F,NspLink *L,NspLink **L1,const double pt[2]);
extern void link_check(NspGFrame *F,NspLink *L);
extern int IsLink(NspObject *O);
extern int link_add_control(NspLink *L,const double pt[2]);


#endif

