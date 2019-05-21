#ifndef NSP_INC_Spqr
#define NSP_INC_Spqr

/* Nsp
 * Copyright (C) 2015-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface with the spqr library. 
 *
 */

/* Spqr */
#include <nsp/objectf.h> 
#include <SuiteSparseQR_C.h>

/*
 * NspSpqr inherits from NspObject
 */

/* typedef struct _NspSpqr NspSpqr ; */
typedef struct _NspTypeSpqr NspTypeSpqr ;

struct _NspTypeSpqr {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_spqr nsp_spqr;

struct _nsp_spqr {

  SuiteSparseQR_C_factorization *QR;
  cholmod_sparse *A ;
  cholmod_common Common;
  int ordering;
  double tol;
  int ref_count;
};

struct _NspSpqr {
  /*< private >*/
  NspObject father;
  NspTypeSpqr*type;
  /*< public >*/
  nsp_spqr *obj;
};

extern int nsp_type_spqr_id;
extern NspTypeSpqr *nsp_type_spqr;

/* type instances for object */

NspTypeSpqr *new_type_spqr(type_mode mode);

/* instance for Spqr */

NspSpqr *new_spqr();

/*
* Object methods redefined for spqr 
*/

#define NULLSPQR (NspSpqr*) 0

extern NspSpqr *spqr_create(char *name,NspTypeBase *type);

/* from SpqrObj.c */

extern NspSpqr *nsp_spqr_copy(NspSpqr *H);
extern void nsp_spqr_destroy(NspSpqr *H);
extern int nsp_spqr_info(NspSpqr *M, int indent,const char *name, int rec_level);
extern int nsp_spqr_print(NspSpqr *Mat, int indent,char *name, int rec_level);
extern NspSpqr *nsp_spqr_object (NspObject *O); 
extern int IsSpqrObj (Stack stack, int i); 
extern int IsSpqr(NspObject *O);
extern NspSpqr *GetSpqrCopy (Stack stack, int i); 
extern NspSpqr *GetSpqr (Stack stack, int i); 
extern int int_spqr_create(Stack stack, int rhs, int opt, int lhs);

#endif /* NSP_INC_Spqr */ 

#ifdef Spqr_Private 
static int init_spqr(NspSpqr *o,NspTypeSpqr *type);
static int nsp_spqr_size(NspSpqr *Mat, int flag);
static char *nsp_spqr_type_as_string(void);
static char *nsp_spqr_type_short_string(NspObject *v);
static int nsp_spqr_eq(NspSpqr *A, NspObject *B);
static int nsp_spqr_neq(NspSpqr *A, NspObject *B);
static int nsp_spqr_xdr_save(XDR  *xdrs, NspSpqr *M);
static NspSpqr *nsp_spqr_xdr_load(XDR *xdrs);
static AttrTab spqr_attrs[];
static NspMethods *spqr_get_methods(void);
static NspSpqr *spqr_create_void(char *name,NspTypeBase *type);
#endif /* Spqr_Private */

