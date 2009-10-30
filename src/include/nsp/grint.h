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

/* to code in a same var lock_dir and lock_type 
 * lock_dir = type & LOCK_DIR_FLAG
 * lock_type = type & LOCK_TYPE_FLAG
 * type = LNORTH | IN 
 */

#define LOCK_DIR_FLAG 0x0f 
#define LOCK_TYPE_FLAG 0xf0

typedef  enum { LD_NORTH=0, LD_SOUTH=1, LD_EAST=2, LD_WEST=3, LD_ANY=4 } lock_dir;
typedef  enum { L_IN=0 <<4 ,L_OUT=1 <<4 ,L_EVIN=2 << 4,L_EVOUT=3 <<4 , L_SQP=4 <<4, L_SQM=5 <<4  } lock_type;

typedef int gr_get_hilited (void *B); 
typedef void gr_set_hilited (void *B, int val); 
typedef int gr_get_show (void *B); 
typedef void gr_set_show (void *B, int val); 
typedef int gr_set_pos (void *R, const double *pt); 
typedef void gr_get_pos (void *R, double *pt); 
typedef void gr_resize (void *R, const double *size); 
typedef void gr_update_locks (void *R); 
typedef int gr_contains_pt (const void *B, const double *pt); 
typedef int gr_control_near_pt (const void *B, const double *pt, int *cp); 
typedef int gr_lock_near_pt (const void *B, const double *pt, int *cp); 
typedef void gr_move_control_init (void *B,  int cp,double ptc[2]); 
typedef void gr_move_control (void *F,void *B, const double *pt, int cp,double ptc[2]); 

typedef struct _gr_port gr_port;

struct _gr_port {
  NspObject *object_id ; /* object connected to the associated port or NULL */  
  void *object_sid;      /* used to keep track of connections while copying or saving */
  int lock;              /* object is connected through his lock point lock */
  int port;              /* and port number port */
};


typedef int gr_get_number_of_locks(const void *B) ;
typedef int gr_get_number_of_ports(const void *B,int lp) ;
typedef int gr_get_lock_connection(const void *B,int i,int port, gr_port *p );
typedef void gr_get_lock_pos(const void *B,int i,double pt[]);
typedef lock_dir gr_get_lock_dir(const void *B,int i);
typedef int gr_set_lock_connection(void *B,int i,int prt,const gr_port *p);
typedef void gr_unset_lock_connection(void *B,int i,int port);
typedef int gr_is_lock_connectable(void *B,int i);
typedef int gr_is_lock_connected(void *B,int i);
typedef void * gr_full_copy(void *B);
typedef void gr_set_lock_pos(void *B, int i,const double pt[],int keep_flag,lock_dir dir);
typedef void gr_unlock(void *B,int i);
typedef void gr_set_frame(void *B,const void *Gf);

typedef struct _NspTypeGRint NspTypeGRint ;

struct _NspTypeGRint { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
  gr_get_hilited *get_hilited ;		     
  gr_set_hilited *set_hilited ;		     
  gr_get_show *get_show ;		     
  gr_set_show *set_show ;		     
  gr_set_pos *set_pos ;		     
  gr_get_pos *get_pos ;		     
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
  gr_get_lock_dir * get_lock_dir;
  gr_set_lock_connection * set_lock_connection;
  gr_unset_lock_connection * unset_lock_connection;
  gr_is_lock_connectable * is_lock_connectable;
  gr_is_lock_connected * is_lock_connected;
  gr_set_lock_pos * set_lock_pos;
  gr_unlock *unlock ; 
};


#define GR_INT(t) ((NspTypeGRint *) t)

extern int nsp_type_grint_id;
extern NspTypeGRint *nsp_type_grint;

NspTypeGRint *new_type_grint(type_mode mode);
#endif 

#ifdef   GRint_Private 
static char *grint_type_as_string (void);
#endif 
