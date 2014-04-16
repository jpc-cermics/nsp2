/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgraph
#define NSP_INC_NspAgraph

/*
 * Copyright (C) 1998-2014 Jean-Philippe Chancelier Enpc/Cermics
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

#line 4 "codegen/agraph.override"

#line 27 "./agraph.h"
/* NspAgraph */

#include <nsp/object.h>

/*
 * NspAgraph inherits from Object
 */

typedef struct _NspAgraph NspAgraph ;
typedef struct _NspTypeAgraph NspTypeAgraph ;

struct _NspTypeAgraph {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_agraph nsp_agraph;
struct _nsp_agraph {
  void* graph;
  int ref_count;
};

struct _NspAgraph {
  /*< private >*/
  NspObject father;
  NspTypeAgraph*type;
  /*< public >*/
  nsp_agraph *obj;
};

extern int nsp_type_agraph_id;
extern NspTypeAgraph *nsp_type_agraph;

/* type instances for object */

NspTypeAgraph *new_type_agraph(type_mode mode);

/* instance for NspAgraph */

NspAgraph *new_agraph();

/*
 * Object methods redefined for agraph 
 */


#define NULLAGRAPH (NspAgraph*) 0

extern NspAgraph *nsp_agraph_create(const char *name,void* graph,NspTypeBase *type);
extern NspAgraph *nsp_agraph_create_default(const char *name);

/* from NspAgraphObj.c */

extern NspAgraph *nsp_agraph_copy(NspAgraph *H);
extern void nsp_agraph_destroy(NspAgraph *H);
extern int nsp_agraph_info(NspAgraph *H, int indent,const char *name, int rec_level);
extern int nsp_agraph_print(NspAgraph *H, int indent,const char *name, int rec_level);
extern int nsp_agraph_latex(NspAgraph *H, int indent,const char *name, int rec_level);
extern NspAgraph *nsp_agraph_object (NspObject *O);
extern int IsAgraphObj (Stack stack, int i);
extern int IsAgraph(NspObject *O);
extern NspAgraph *GetAgraphCopy (Stack stack, int i);
extern NspAgraph *GetAgraph (Stack stack, int i);
extern int nsp_agraph_create_partial(NspAgraph *H);
extern void nsp_agraph_destroy_partial(NspAgraph *H);
extern NspAgraph * nsp_agraph_copy_partial(NspAgraph *H,NspAgraph *self);
extern NspAgraph * nsp_agraph_full_copy_partial(NspAgraph *H,NspAgraph *self);
extern NspAgraph * nsp_agraph_full_copy(NspAgraph *self);
extern int nsp_agraph_check_values(NspAgraph *H);
extern int int_agraph_create(Stack stack, int rhs, int opt, int lhs);
extern NspAgraph *nsp_agraph_xdr_load_partial(XDR *xdrs, NspAgraph *M);
extern int nsp_agraph_xdr_save(XDR  *xdrs, NspAgraph *M);

#line 7 "codegen/agraph.override"
/* inserted at the end of public part of include file
 * of ldfaure.h
 */
#include <gvc.h> 
#include <nsp/agraph-nsp.h>

#line 109 "./agraph.h"
#endif /* NSP_INC_NspAgraph */ 

#ifdef NspAgraph_Private 
static int init_agraph(NspAgraph *o,NspTypeAgraph *type);
static int nsp_agraph_size(NspAgraph *Mat, int flag);
static char *nsp_agraph_type_as_string(void);
static char *nsp_agraph_type_short_string(NspObject *v);
static int nsp_agraph_eq(NspAgraph *A, NspObject *B);
static int nsp_agraph_neq(NspAgraph *A, NspObject *B);
static NspAgraph *nsp_agraph_xdr_load(XDR *xdrs);
static AttrTab agraph_attrs[];
static NspMethods *agraph_get_methods(void);
/* static int int_agraph_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAgraph *nsp_agraph_create_void(const char *name,NspTypeBase *type);
#line 15 "codegen/agraph.override"
/* inserted in the private part of include file
 * of ldfaure.h
 */
static int nsp_gv_add_nodes(NspAgraph *G, NspSMatrix *S);
static int nsp_gv_add_edges(NspAgraph *G, NspSMatrix *S);
#define nsp_gv_nnodes(G) agnnodes((G)->obj->graph)
#define nsp_gv_nedges(G) agnedges((G)->obj->graph)
static int nsp_gv_write(NspAgraph * g,void *chan);
static int nsp_gv_render(NspAgraph *G, char *mode, char *filename);
static int nsp_gv_layout(NspAgraph *G, char *mode);
static NspAgnode *nsp_gv_agfindnode(NspAgraph * g, char *name);
static NspAgnode *nsp_gv_agfstnode(NspAgraph * g);
static NspAgnode *nsp_gv_aglstnode(NspAgraph * g);
static NspAgnode *nsp_gv_agnxtnode(NspAgraph * g, NspAgnode *n);
static NspAgnode *nsp_gv_agprvnode(NspAgraph * g, NspAgnode *n);
static NspSMatrix *nsp_gv_nodeattrs(NspAgraph * g);
static NspSMatrix *nsp_gv_edgeattrs(NspAgraph * g);
static NspSMatrix *nsp_gv_graphattrs(NspAgraph * g);

static NspAgedge *nsp_gv_agfstout(NspAgraph * g, NspAgnode *n);
static NspAgedge *nsp_gv_agnxtout(NspAgraph * g, NspAgedge *e);
static NspAgedge *nsp_gv_agfstin(NspAgraph * g, NspAgnode *n);
static NspAgedge *nsp_gv_agnxtin(NspAgraph * g, NspAgedge *e);
static NspAgedge *nsp_gv_agfstedge(NspAgraph * g, NspAgnode *n);
static NspAgedge *nsp_gv_agnxtedge(NspAgraph * g, NspAgedge *e, NspAgnode *n);
static int _wrap_nsp_gv_agset_gen(void *obj,Stack stack,int rhs,int opt,int lhs);
static NspAgraph *nsp_gv_agsubg(NspAgraph * g, char *name);

#line 153 "./agraph.h"
#endif /* NspAgraph_Private */

