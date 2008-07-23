#ifndef INC_NSP_GRint
#define INC_NSP_GRint

/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
  
/* graphic object interface */

#include <stdio.h>   /** for file declaration **/
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

typedef int gr_get_n_locks(const void *B) ;
typedef int gr_get_lock_status(const void *B, int i);
typedef int gr_get_lock_link(const void *B, int i);
typedef void gr_get_lock_pos(const void *B, int i,double pt[]);
typedef int gr_get_lock_data(const void *B, int i) ;
typedef void gr_set_lock_status(void *B, int i, int val);
typedef void  gr_set_lock_link(void *B, int i,int val);
typedef void gr_set_lock_pos(void *B, int i,const double pt[]);
typedef void gr_set_lock_data(void *B, int i, int val);

typedef struct _nsp_type_GRint { 
  NSP_TYPE_OBJECT__ 

  /* rajouts */

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
					     
  gr_get_n_locks *   get_n_locks;	     
  gr_get_lock_status *   get_lock_status;    
  gr_get_lock_link *   get_lock_link;	     
  gr_get_lock_pos *   get_lock_pos;	     
  gr_get_lock_data *   get_lock_data;	     
  gr_set_lock_status *   set_lock_status;    
  gr_set_lock_link *   set_lock_link;	     
  gr_set_lock_pos *   set_lock_pos;	     
  gr_set_lock_data *   set_lock_data;        

} NspTypeGRint;


#define GR_INT(t) ((NspTypeGRint *) t)

extern int nsp_type_grint_id;
extern NspTypeGRint *nsp_type_grint;

NspTypeGRint *new_type_grint(type_mode mode);

#endif 

