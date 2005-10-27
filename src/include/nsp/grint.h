#ifndef NSP_INC_GRint
#define NSP_INC_GRint

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* graphic object interface */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

typedef int gr_get_hilited (void *B); 
typedef void gr_set_hilited (void *B, int val); 
typedef int gr_get_show (void *B); 
typedef void gr_set_show (void *B, int val); 
typedef void gr_draw (void *R); 
typedef void gr_translate (void *R, const double *pt); 
typedef void gr_resize (void *R, const double *size); 
typedef void gr_update_locks (void *R); 
typedef int gr_contains_pt (const void *B, const double *pt); 
typedef int gr_control_near_pt (const void *B, const double *pt, int *cp); 
typedef int gr_lock_near_pt (const void *B, const double *pt, int *cp); 
typedef void gr_move_control_init (void *B,  int cp,double ptc[2]); 
typedef void gr_move_control (NspGFrame *F,void *B, const double *pt, int cp,double ptc[2]); 


typedef struct _port {
  NspObject *object_id ; /* object connected to the associated port or NULL */  
  int lock;              /* object is connected through his lock point lock */
  int port;              /* and port number port */
} gr_port; 


typedef int gr_get_number_of_locks(const void *B) ;
typedef int gr_get_number_of_ports(const void *B,int lp) ;
typedef int gr_get_lock_connection(const void *B,int i,int port, gr_port *p );
typedef void gr_get_lock_pos(const void *B,int i,double pt[]);
typedef int gr_set_lock_connection(void *B,int i,const gr_port *p);
typedef void gr_unset_lock_connection(void *B,int i,int port);
typedef int gr_is_lock_connectable(void *B,int i);
typedef int gr_is_lock_connected(void *B,int i);
typedef void gr_set_lock_pos(void *B, int i,const double pt[],int keep_flag);

typedef struct _NspTypeGRint { 
  NSP_TYPE_OBJECT__ 

  /*< public >*/

  gr_get_hilited *get_hilited ;		     
  gr_set_hilited *set_hilited ;		     
  gr_get_show *get_show ;		     
  gr_set_show *set_show ;		     
  gr_draw *draw ;			     
  gr_translate *translate ;		     
  gr_resize *resize ;			     
  gr_update_locks *update_locks ;	     
  gr_contains_pt *contains_pt ;		     
  gr_control_near_pt *control_near_pt ;	     
  gr_lock_near_pt *lock_near_pt ;	     
  gr_move_control_init *move_control_init ;  
  gr_move_control *move_control ;	     

  gr_get_number_of_locks * get_number_of_locks;
  gr_get_number_of_ports * get_number_of_ports;
  gr_get_lock_connection * get_lock_connection;
  gr_get_lock_pos * get_lock_pos;
  gr_set_lock_connection * set_lock_connection;
  gr_unset_lock_connection * unset_lock_connection;
  gr_is_lock_connectable * is_lock_connectable;
  gr_is_lock_connected * is_lock_connected;
  gr_set_lock_pos * set_lock_pos;

} NspTypeGRint;


#define GR_INT(t) ((NspTypeGRint *) t)

extern int nsp_type_grint_id;
extern NspTypeGRint *nsp_type_grint;

NspTypeGRint *new_type_grint(type_mode mode);

#endif 

