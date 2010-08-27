#ifndef NSP_INC_Umfpack
#define NSP_INC_Umfpack

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
 * Interface with the umfpack library. 
 *
 */

/* Umfpack */

#include <nsp/object.h>
#include <nsp/spcolmatrix.h>

/*
 * NspUmfpack inherits from NspObject
 */

/* typedef struct _NspUmfpack NspUmfpack ; */
typedef struct _NspTypeUmfpack NspTypeUmfpack ;

struct _NspTypeUmfpack {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_umfpack nsp_umfpack;
struct _nsp_umfpack {
  char rc_type;
  char* data;
  nsp_sparse_triplet mtlb_T;
  int ref_count;
};

struct _NspUmfpack {
  /*< private >*/
  NspObject father;
  NspTypeUmfpack*type;
  /*< public >*/
  nsp_umfpack *obj;
};

extern int nsp_type_umfpack_id;
extern NspTypeUmfpack *nsp_type_umfpack;

/* type instances for object */

NspTypeUmfpack *new_type_umfpack(type_mode mode);

/* instance for Umfpack */

NspUmfpack *new_umfpack();

/*
* Object methods redefined for umfpack 
*/

#define NULLUMFPACK (NspUmfpack*) 0

extern NspUmfpack *umfpack_create(char *name,char rc_type,char* data,NspTypeBase *type);

/* from UmfpackObj.c */

extern NspUmfpack *nsp_umfpack_copy(NspUmfpack *H);
extern void nsp_umfpack_destroy(NspUmfpack *H);
extern int nsp_umfpack_info(NspUmfpack *M, int indent,const char *name, int rec_level);
extern int nsp_umfpack_print(NspUmfpack *Mat, int indent,char *name, int rec_level);
extern NspUmfpack *nsp_umfpack_object (NspObject *O); 
extern int IsUmfpackObj (Stack stack, int i); 
extern int IsUmfpack(NspObject *O);
extern NspUmfpack *GetUmfpackCopy (Stack stack, int i); 
extern NspUmfpack *GetUmfpack (Stack stack, int i); 

#endif /* NSP_INC_Umfpack */ 

#ifdef Umfpack_Private 
static int init_umfpack(NspUmfpack *o,NspTypeUmfpack *type);
static int nsp_umfpack_size(NspUmfpack *Mat, int flag);
static char *nsp_umfpack_type_as_string(void);
static char *nsp_umfpack_type_short_string(NspObject *v);
static int nsp_umfpack_eq(NspUmfpack *A, NspObject *B);
static int nsp_umfpack_neq(NspUmfpack *A, NspObject *B);
static int nsp_umfpack_xdr_save(XDR  *xdrs, NspUmfpack *M);
static NspUmfpack *nsp_umfpack_xdr_load(XDR *xdrs);
static AttrTab umfpack_attrs[];
static NspMethods *umfpack_get_methods(void);
static int int_umfpack_create(Stack stack, int rhs, int opt, int lhs);
static NspUmfpack *umfpack_create_void(char *name,NspTypeBase *type);
#endif /* Umfpack_Private */

