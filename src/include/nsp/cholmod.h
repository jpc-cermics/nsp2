#ifndef NSP_INC_Cholmod
#define NSP_INC_Cholmod

/* Nsp
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * Interface with the cholmod library. 
 *
 */

/* Cholmod */
#include <nsp/objectf.h> 
#include <cholmod.h>

/*
 * NspCholmod inherits from NspObject
 */

/* typedef struct _NspCholmod NspCholmod ; */
typedef struct _NspTypeCholmod NspTypeCholmod ;

struct _NspTypeCholmod {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_cholmod nsp_cholmod;
struct _nsp_cholmod {
  cholmod_factor *L ;
  cholmod_common Common;
  int m,n;
  int ref_count;
};

struct _NspCholmod {
  /*< private >*/
  NspObject father;
  NspTypeCholmod*type;
  /*< public >*/
  nsp_cholmod *obj;
};

extern int nsp_type_cholmod_id;
extern NspTypeCholmod *nsp_type_cholmod;

/* type instances for object */

NspTypeCholmod *new_type_cholmod(type_mode mode);

/* instance for Cholmod */

NspCholmod *new_cholmod();

/*
* Object methods redefined for cholmod 
*/

#define NULLCHOLMOD (NspCholmod*) 0

extern NspCholmod *cholmod_create(char *name,NspTypeBase *type);

/* from CholmodObj.c */

extern NspCholmod *nsp_cholmod_copy(NspCholmod *H);
extern void nsp_cholmod_destroy(NspCholmod *H);
extern int nsp_cholmod_info(NspCholmod *M, int indent,const char *name, int rec_level);
extern int nsp_cholmod_print(NspCholmod *Mat, int indent,char *name, int rec_level);
extern NspCholmod *nsp_cholmod_object (NspObject *O); 
extern int IsCholmodObj (Stack stack, int i); 
extern int IsCholmod(NspObject *O);
extern NspCholmod *GetCholmodCopy (Stack stack, int i); 
extern NspCholmod *GetCholmod (Stack stack, int i); 
extern int int_cholmod_create(Stack stack, int rhs, int opt, int lhs);

#endif /* NSP_INC_Cholmod */ 

#ifdef Cholmod_Private 
static int init_cholmod(NspCholmod *o,NspTypeCholmod *type);
static int nsp_cholmod_size(NspCholmod *Mat, int flag);
static char *nsp_cholmod_type_as_string(void);
static char *nsp_cholmod_type_short_string(NspObject *v);
static int nsp_cholmod_eq(NspCholmod *A, NspObject *B);
static int nsp_cholmod_neq(NspCholmod *A, NspObject *B);
static int nsp_cholmod_xdr_save(XDR  *xdrs, NspCholmod *M);
static NspCholmod *nsp_cholmod_xdr_load(XDR *xdrs);
static AttrTab cholmod_attrs[];
static NspMethods *cholmod_get_methods(void);
static NspCholmod *cholmod_create_void(char *name,NspTypeBase *type);
#endif /* Cholmod_Private */

