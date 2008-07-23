/* -*- Mode: C -*- */
#ifndef NSP_INC_Port
#define NSP_INC_Port

/*
 * This Software is GPL (Copyright ENPC 1998-2008) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Port */

#include "nsp/object.h"

/*
 * NspPort inherits from NspObject
 */

typedef struct _NspPort NspPort ;
typedef struct _NspTypePort NspTypePort ;

struct _NspTypePort {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_port nsp_port;

/* used to store information about the 
 * object connected on a port.
 */

struct _nsp_port {
  NspObject *object_id ; /* object connected to the associated port or NULL */  
  int lock;              /* object is connected through his lock point lock */
  int port;              /* and port number port */
  int ref_count;
};

struct _NspPort {
  /*< private >*/
  NspObject father;
  NspTypePort*type;
  /*< public >*/
  nsp_port *obj;
};

extern int nsp_type_port_id;
extern NspTypePort *nsp_type_port;

/* type instances for object */

NspTypePort *new_type_port(type_mode mode);

/* instance for Port */

NspPort *new_port();

/*
* Object methods redefined for port 
*/


#define NULLPORT (NspPort*) 0

extern NspPort *port_create(char *name,int lock,int port,NspObject* object_id,NspTypeBase *type);

/* from PortObj.c */

extern NspPort *nsp_port_copy(NspPort *H);
extern void nsp_port_destroy(NspPort *H);
extern void nsp_port_info(NspPort *H, int indent,char *name, int rec_level);
extern int nsp_port_print(NspPort *H, int indent,char *name, int rec_level);
extern NspPort *nsp_port_object (NspObject *O); 
extern int IsPortObj (Stack stack, int i); 
extern int IsPort(NspObject *O);
extern NspPort *GetPortCopy (Stack stack, int i); 
extern NspPort *GetPort (Stack stack, int i); 

#endif /* NSP_INC_Port */ 

#ifdef Port_Private 
static int init_port(NspPort *o,NspTypePort *type);
static int nsp_port_size(NspPort *Mat, int flag);
static char *nsp_port_type_as_string(void);
static char *nsp_port_type_short_string(NspObject *v);
static int nsp_port_eq(NspPort *A, NspObject *B);
static int nsp_port_neq(NspPort *A, NspObject *B);
static int nsp_port_xdr_save(XDR  *xdrs, NspPort *M);
static NspPort *nsp_port_xdr_load(XDR *xdrs);
static AttrTab port_attrs[];
static NspMethods *port_get_methods(void);
static int int_port_create(Stack stack, int rhs, int opt, int lhs);
static NspPort *port_create_void(char *name,NspTypeBase *type);
#endif /* Port_Private */

