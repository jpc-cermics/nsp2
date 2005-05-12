#ifndef INC_NSP_Connector
#define INC_NSP_Connector

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/
  
/* connector */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/gframe.h"
#include "nsp/grint.h"

/*
 * NspConnector inherits from NspObject 
 * and implements GRint 
 */

typedef struct _nsp_connector NspConnector;

typedef int (*connector_save) (NspFile  *F, NspConnector *M);

typedef struct _nsp_type_Connector { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  connector_save *save;
} NspTypeConnector;



typedef struct _lock {
  int n_ports ;
  int fixed ;   /* flag: if fixed == TRUE the number of ports cannot be changed 
		 * after creation
		 */
  gr_port *ports; 
  double pt[2]; /* lock position */
} gr_lock ; 

struct _nsp_connector {
  NspObject father; 
  NspTypeConnector *type; 
  double r[4];
  int color; 
  int thickness;
  int background;
  gr_lock lock; /* Only one lock point for a connector */
  int hilited ; 
  int show    ;   
};

extern int nsp_type_connector_id;
extern NspTypeConnector *nsp_type_connector;

NspTypeConnector *new_type_connector(type_mode mode);
NspConnector *new_connector();

#ifdef Connector_Private 
static int init_connector(NspConnector *ob,NspTypeConnector *type);
static int connector_size(NspConnector *Mat, int flag);
static char *connector_type_as_string(void);
static char *connector_type_short_string(void);
static int connector_eq(NspConnector *A, NspObject *B);
static int connector_neq(NspConnector *A, NspObject *B);
static int connector_xdr_save(NspFile  *F, NspConnector *M);
static NspConnector  *connector_xdr_load(NspFile  *F);
static AttrTab connector_attrs[];
static NspConnector *connector_object (NspObject *O); 
static NspConnector *connector_copy (NspConnector *H); 
static void connector_destroy (NspConnector *H); 
static void connector_info (NspConnector *H, int indent); 
static void connector_print (NspConnector *H, int indent); 
static NspMethods *connector_get_methods(void);
static int int_connector_create( Stack stack, int rhs, int opt, int lhs);

/* set of methods for implementing Grint */
static int connector_get_hilited (NspConnector *B); 
static void connector_set_hilited (NspConnector *B, int val); 
static int connector_get_show (NspConnector *B); 
static void connector_set_show (NspConnector *B, int val); 
static void connector_draw (NspConnector *R); 
static void connector_translate (NspConnector *R, const double *pt); 
static void connector_resize (NspConnector *R, const double *size); 
static void connector_update_locks (NspConnector *R); 
static int connector_contains_pt (const NspConnector *B, const double *pt); 
static int connector_control_near_pt (const NspConnector *B, const double *pt, int *cp); 
static int connector_lock_near_pt (const NspConnector *B, double *pt, int *cp); 
static void connector_move_control_init( NspConnector *B,int cp,double ptc[2]);
static void connector_move_control (NspGFrame *F,NspConnector *B, const double *pt, int cp,double ptc[2]); 

static int connector_get_number_of_locks(const NspConnector *B) ;
static int connector_get_number_of_ports(const NspConnector *B,int lp) ;
static int connector_get_lock_connection(const NspConnector *B,int i,int port, gr_port *p );
static void connector_get_lock_pos(const NspConnector *B,int i,double pt[]);
static int connector_set_lock_connection(NspConnector *B,int i,const gr_port *p);
static void connector_unset_lock_connection(NspConnector *B,int i,int port);
static int connector_is_lock_connectable(NspConnector *B,int i);
static int connector_is_lock_connected(NspConnector *B,int i);
static void connector_set_lock_pos(NspConnector *B, int i,const double pt[]);



#endif /* Connector_Private */

#define NULLCONNECTOR (NspConnector*) 0


extern NspConnector *connector_object(NspObject *O); 
extern int IsConnectorObj (Stack stack, int i); 
extern NspConnector *GetConnectorCopy (Stack stack, int i); 
extern NspConnector *GetConnector (Stack stack, int i); 
extern int IsConnector (NspObject *O); 

extern NspConnector *connector_create(char *name,double rect[],int color,int thickness,int background,
				      NspTypeBase *type );


#endif

