/* -*- Mode: C -*- */
#ifndef NSP_INC_NspConnector
#define NSP_INC_NspConnector

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#line 4 "codegen/connector.override"

/* inserted at the start of include file */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/grint.h" /* interface definition */

/**
 * NspConnector:
 * @obj: pointer to a #nsp_connector
 *
 * NspConnector inherits from NspObject 
 * and implements GRint. It is used for 
 * multiple link connections. A connector 
 * has just one lock point where many links can be locked 
 */

typedef struct _gr_lock gr_lock ; 
struct _gr_lock {
  int n_ports ;
  int fixed ;   /* flag: if fixed == TRUE the number of ports cannot be changed 
		 * after creation
		 */
  gr_port *ports; 
  double pt[2]; /* lock position */
};



#line 42 "./connector.h"
/* NspConnector */

#include <nsp/graphic.h>

/*
 * NspConnector inherits from Graphic
 */

typedef struct _NspConnector NspConnector ;
typedef struct _NspTypeConnector NspTypeConnector ;

#line 54 "./connector.h"

struct _NspTypeConnector {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 61 "./connector.h"
};

typedef struct _nsp_connector nsp_connector;
struct _nsp_connector {
  void* object_sid;
  double r[4];
  int color;
  int thickness;
  int background;
  gr_lock lock;
  gboolean hilited;
  gboolean show;
  int ref_count;
};

struct _NspConnector {
  /*< private >*/
  NspGraphic father;
  NspTypeConnector*type;
  /*< public >*/
  nsp_connector *obj;
};

extern int nsp_type_connector_id;
extern NspTypeConnector *nsp_type_connector;

/* type instances for graphic */

NspTypeConnector *new_type_connector(type_mode mode);

/* instance for NspConnector */

NspConnector *new_connector();

/*
 * Object methods redefined for connector 
 */


#define NULLCONNECTOR (NspConnector*) 0

extern NspConnector *nsp_connector_create(char *name,void* object_sid,double* r,int color,int thickness,int background,gr_lock lock,gboolean hilited,gboolean show,NspTypeBase *type);
extern NspConnector *nsp_connector_create_default(char *name);

/* from NspConnectorObj.c */

extern NspConnector *nsp_connector_copy(NspConnector *H);
extern void nsp_connector_destroy(NspConnector *H);
extern int nsp_connector_info(NspConnector *H, int indent,const char *name, int rec_level);
extern int nsp_connector_print(NspConnector *H, int indent,const char *name, int rec_level);
extern int nsp_connector_latex(NspConnector *H, int indent,const char *name, int rec_level);
extern NspConnector *nsp_connector_object (NspObject *O);
extern int IsConnectorObj (Stack stack, int i);
extern int IsConnector(NspObject *O);
extern NspConnector *GetConnectorCopy (Stack stack, int i);
extern NspConnector *GetConnector (Stack stack, int i);
extern int nsp_connector_create_partial(NspConnector *H);
extern void nsp_connector_destroy_partial(NspConnector *H);
extern NspConnector * nsp_connector_copy_partial(NspConnector *H,NspConnector *self);
extern NspConnector * nsp_connector_full_copy_partial(NspConnector *H,NspConnector *self);
extern NspConnector * nsp_connector_full_copy(NspConnector *self);
extern int nsp_connector_check_values(NspConnector *H);
extern int int_connector_create(Stack stack, int rhs, int opt, int lhs);
extern NspConnector *nsp_connector_xdr_load_partial(XDR *xdrs, NspConnector *M);
extern int nsp_connector_xdr_save(XDR  *xdrs, NspConnector *M);

#line 36 "codegen/connector.override"

/* inserted at the end of public part of include file */

#line 132 "./connector.h"
#endif /* NSP_INC_NspConnector */ 

#ifdef NspConnector_Private 
static int init_connector(NspConnector *o,NspTypeConnector *type);
static int nsp_connector_size(NspConnector *Mat, int flag);
static char *nsp_connector_type_as_string(void);
static char *nsp_connector_type_short_string(NspObject *v);
static int nsp_connector_eq(NspConnector *A, NspObject *B);
static int nsp_connector_neq(NspConnector *A, NspObject *B);
static NspConnector *nsp_connector_xdr_load(XDR *xdrs);
static AttrTab connector_attrs[];
static NspMethods *connector_get_methods(void);
/* static int int_connector_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspConnector *nsp_connector_create_void(char *name,NspTypeBase *type);
#line 41 "codegen/connector.override"

static double lock_size=1; /*  XXX a factoriser quelque part ... */ 
static int lock_color=10;

/* set of method for parent class graphic  */
static void nsp_draw_connector(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_connector(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_connector(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_connector(BCG *Xgc,NspGraphic *o,double *alpha);
static int nsp_getbounds_connector(BCG *Xgc,NspGraphic *o,double *bounds);

/* set of methods for implementing Grint */
static int connector_get_hilited (NspConnector *B); 
static void connector_set_hilited (NspConnector *B, int val); 
static int connector_get_show (NspConnector *B); 
static void connector_set_show (NspConnector *B, int val); 
static void connector_draw (NspConnector *R); 
static int connector_translate (NspConnector *R, const double *pt); 
static int connector_set_pos (NspConnector *R, const double *pt); 
static void connector_get_pos (NspConnector *R, double *pt); 
static void connector_get_rect (NspConnector *R, double *r); 
static void connector_resize (NspConnector *R, const double *size); 
static void connector_update_locks (NspConnector *R); 
static int connector_contains_pt (const NspConnector *B, const double *pt); 
static int connector_control_near_pt (const NspConnector *B, const double *pt, int *cp); 
static int connector_lock_near_pt (const NspConnector *B, double *pt, int *cp); 
static void connector_move_control_init( NspConnector *B,int cp,double ptc[2]);
static void connector_move_control (void *F,NspConnector *B, const double *pt, int cp,double ptc[2]); 

static int connector_get_number_of_locks(const NspConnector *B) ;
static int connector_get_number_of_ports(const NspConnector *B,int lp) ;
static int connector_get_lock_connection(const NspConnector *B,int i,int port, gr_port *p );
static void connector_get_lock_pos(const NspConnector *B,int i,double pt[]);
static lock_dir connector_get_lock_dir(const NspConnector *B,int i);

static int connector_set_lock_connection(NspConnector *B,int i,int prt,const gr_port *p);
static void connector_unset_lock_connection(NspConnector *B,int i,int port);
static int connector_is_lock_connectable(NspConnector *B,int i);
static int connector_is_lock_connected(const NspConnector *B,int i);
static void connector_set_lock_pos(NspConnector *B, int i,const double pt[],int keep_angle,lock_dir dir);
static void connector_unlock( NspConnector *B,int lp) ;

/* requested for grl_lock */

static void nsp_destroy_gr_lock(gr_lock *locks,NspConnector *H);
static int nsp_save_gr_lock(XDR *xdrs,gr_lock *locks,NspConnector *M);
static int nsp_load_gr_lock(XDR *xdrs,gr_lock *locks,NspConnector *M);
static int nsp_print_gr_lock(int indent,gr_lock *locks,NspConnector *M);
static int nsp_check_gr_lock(gr_lock *locks,NspConnector *M);
static int nsp_eq_gr_lock(gr_lock *lock1,gr_lock *lock2);
static void nsp_init_gr_lock(gr_lock *locks);
static int nsp_gr_lock_full_copy(gr_lock *lock_c,gr_lock *lock,NspConnector *M);

#line 201 "./connector.h"
#endif /* NspConnector_Private */

