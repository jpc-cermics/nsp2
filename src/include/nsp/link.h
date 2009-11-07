/* -*- Mode: C -*- */
#ifndef NSP_INC_NspLink
#define NSP_INC_NspLink

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#line 4 "codegen/link.override"
/* inserted at the start of include file */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/grint.h" /* interface definition */

/**
 * NspLink:
 * @obj: a #nsp_link pointer 
 *
 * inherits from #NspObject and implements Grint. 
 * Used for graphic links for a C implementation of scicos.
 */

/**
 * grl_lock: 
 *  @port: a #gr_port. 
 *  @pt: position of the lock point in the frame
 *  @ptr: relative position of the lock point in the link
 *  @type: type of the lock point.
 *
 * used for storing information about lock points of a link. 
 * 
 */

typedef struct _grl_lock grl_lock ; 

struct _grl_lock {
  gr_port port;  /* Only one port */
};

/**
 * nsp_link: 
 * @frame: a link must be in a frame to be drawn 
 * @object_sid: 
 * @r: the link enclosing rectangle 
 * @color: color of the link
 * @thickness: thickness of the enclosing rectangle 
 * @background: color of the background
 * @n_locks:  number of lock points 
 * @locks: array of lock points 
 * @hilited: is link hilited 
 * @show: is link to be visible 
 * @ref_count: a reference counter.
 *
 * used for storing information for a graphic link.
 */


#line 62 "./link.h"
/* NspLink */

#include <nsp/graphic.h>

/*
 * NspLink inherits from Graphic
 */

typedef struct _NspLink NspLink ;
typedef struct _NspTypeLink NspTypeLink ;

#line 74 "./link.h"

struct _NspTypeLink {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 81 "./link.h"
};

typedef struct _nsp_link nsp_link;
struct _nsp_link {
  void* object_sid;
  int color;
  int thickness;
  int background;
  NspMatrix* poly;
  grl_lock lock1;
  grl_lock lock2;
  gboolean hilited;
  gboolean show;
  int ref_count;
};

struct _NspLink {
  /*< private >*/
  NspGraphic father;
  NspTypeLink*type;
  /*< public >*/
  nsp_link *obj;
};

extern int nsp_type_link_id;
extern NspTypeLink *nsp_type_link;

/* type instances for graphic */

NspTypeLink *new_type_link(type_mode mode);

/* instance for NspLink */

NspLink *new_link();

/*
 * Object methods redefined for link 
 */


#define NULLLINK (NspLink*) 0

extern NspLink *nsp_link_create(char *name,void* object_sid,int color,int thickness,int background,NspMatrix* poly,grl_lock lock1,grl_lock lock2,gboolean hilited,gboolean show,NspTypeBase *type);
extern NspLink *nsp_link_create_default(char *name);

/* from NspLinkObj.c */

extern NspLink *nsp_link_copy(NspLink *H);
extern void nsp_link_destroy(NspLink *H);
extern int nsp_link_info(NspLink *H, int indent,const char *name, int rec_level);
extern int nsp_link_print(NspLink *H, int indent,const char *name, int rec_level);
extern int nsp_link_latex(NspLink *H, int indent,const char *name, int rec_level);
extern NspLink *nsp_link_object (NspObject *O);
extern int IsLinkObj (Stack stack, int i);
extern int IsLink(NspObject *O);
extern NspLink *GetLinkCopy (Stack stack, int i);
extern NspLink *GetLink (Stack stack, int i);
extern int nsp_link_create_partial(NspLink *H);
extern void nsp_link_destroy_partial(NspLink *H);
extern NspLink * nsp_link_copy_partial(NspLink *H,NspLink *self);
extern NspLink * nsp_link_full_copy_partial(NspLink *H,NspLink *self);
extern NspLink * nsp_link_full_copy(NspLink *self);
extern int nsp_link_check_values(NspLink *H);
extern int int_link_create(Stack stack, int rhs, int opt, int lhs);
extern NspLink *nsp_link_xdr_load_partial(XDR *xdrs, NspLink *M);
extern int nsp_link_xdr_save(XDR  *xdrs, NspLink *M);

#line 56 "codegen/link.override"

/* inserted at the end of public part of include file */

#line 153 "./link.h"
#endif /* NSP_INC_NspLink */ 

#ifdef NspLink_Private 
static int init_link(NspLink *o,NspTypeLink *type);
static int nsp_link_size(NspLink *Mat, int flag);
static char *nsp_link_type_as_string(void);
static char *nsp_link_type_short_string(NspObject *v);
static int nsp_link_eq(NspLink *A, NspObject *B);
static int nsp_link_neq(NspLink *A, NspObject *B);
static NspLink *nsp_link_xdr_load(XDR *xdrs);
static AttrTab link_attrs[];
static NspMethods *link_get_methods(void);
/* static int int_link_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspLink *nsp_link_create_void(char *name,NspTypeBase *type);
#line 61 "codegen/link.override"

static double lock_size=1; /*  XXX a factoriser quelque part ... */ 
static int lock_color=10;

/* set of method for parent class graphic  */
static void nsp_draw_link(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data);
static void nsp_translate_link(NspGraphic *o,const double *tr);
static void nsp_rotate_link(NspGraphic *o,double *R);
static void nsp_scale_link(NspGraphic *o,double *alpha);
static int nsp_getbounds_link(NspGraphic *o,double *bounds);

/* set of methods for implementing Grint */
static int link_get_hilited (NspLink *B); 
static void link_set_hilited (NspLink *B, int val); 
static int link_get_show (NspLink *B); 
static void link_set_show (NspLink *B, int val); 
static int link_set_pos (NspLink *R, const double *pt); 
static void link_get_pos (NspLink *R, double *pt); 
static void link_resize (NspLink *R, const double *size); 
static void link_update_locks (NspLink *R); 
static int link_contains_pt (const NspLink *B, const double *pt); 
static int link_control_near_pt (const NspLink *B, const double *pt, int *cp); 
static int link_lock_near_pt (const NspLink *B,const double *pt, int *cp); 
static void link_move_control_init( NspLink *B,int cp,double ptc[2]);
static void link_move_control (void *F,NspLink *B, const double *pt, int cp,double ptc[2]); 

static int link_get_number_of_locks(const NspLink *B) ;
static int link_get_number_of_ports(const NspLink *B,int lp) ;
static int link_get_lock_connection(const NspLink *B,int i,int port, gr_port *p );
static void link_get_lock_pos(const NspLink *B,int i,double pt[]);
static lock_dir link_get_lock_dir(const NspLink *B,int i);

static int link_set_lock_connection(NspLink *B,int i,int prt,const gr_port *p);
static void link_unset_lock_connection(NspLink *B,int i,int port);
static int link_is_lock_connectable(NspLink *B,int i);
static int link_is_lock_connected(NspLink *B,int i);
static void link_set_lock_pos(NspLink *B, int i,const double pt[],int keep_angle,lock_dir dir);
static void link_unlock( NspLink *B,int lp) ;

/* requested for grl_lock */

static void nsp_destroy_grl_lock(grl_lock *locks,NspLink *H);
static int nsp_save_grl_lock(XDR *xdrs,grl_lock *locks,NspLink *M);
static int nsp_load_grl_lock(XDR *xdrs,grl_lock *locks,NspLink *M);
static int nsp_print_grl_lock(int indent,grl_lock *locks,NspLink *M);
static int nsp_check_grl_lock(grl_lock *locks,NspLink *M);
static int nsp_eq_grl_lock(grl_lock *lock1,grl_lock *lock2);
static void nsp_init_grl_lock(grl_lock *locks);
static int  nsp_grl_lock_full_copy(NspLink *C,grl_lock *Cl,NspLink *L);

#line 219 "./link.h"
#endif /* NspLink_Private */

