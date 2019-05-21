/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgraph
#define NSP_INC_NspAgraph

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
  void* gvc;
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

extern NspAgraph *nsp_agraph_create(const char *name,void* graph,void* gvc,NspTypeBase *type);
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
#line 14 "codegen/agraph.override"

/* graph methods */

static int nsp_agraph_fill_from_b(Agraph_t *g, NspBMatrix *B);

#define nsp_agnnodes(G) agnnodes((G)->obj->graph)
#define nsp_agnedges(G) agnedges((G)->obj->graph)

static NspAgraph *nsp_agparent(NspAgraph * G);
static NspAgraph *nsp_agroot(NspAgraph * G);

static int nsp_gv_write(NspAgraph * g,void *chan);
static int nsp_gv_render(NspAgraph *G, char *mode, char *filename);
static int nsp_gv_layout(NspAgraph *G, char *mode);

static NspSMatrix *nsp_agnodeattrs(NspAgraph * g);
static NspSMatrix *nsp_agedgeattrs(NspAgraph * g);
static NspSMatrix *nsp_agraphattrs(NspAgraph * g);

static NspAgedge *nsp_agfstout(NspAgraph * g, NspAgnode *n);
static NspAgedge *nsp_agnxtout(NspAgraph * g, NspAgedge *e);
static NspAgedge *nsp_agfstin(NspAgraph * g, NspAgnode *n);
static NspAgedge *nsp_agnxtin(NspAgraph * g, NspAgedge *e);
static NspAgedge *nsp_agfstedge(NspAgraph * g, NspAgnode *n);
static NspAgedge *nsp_agnxtedge(NspAgraph * g, NspAgedge *e, NspAgnode *n);

static int _wrap_nsp_agset_gen(void *obj,Stack stack,int rhs,int opt,int lhs);
static int nsp_agdelnode(NspAgraph *g, NspAgnode *n);
static int nsp_agdeledge(NspAgraph *g,NspAgedge *e);

/* graph methods related to subgraphs */

static NspAgraph *nsp_agsubg(NspAgraph * g, char *name);
static NspAgraph *nsp_agfstsubg(NspAgraph * G);
static NspAgraph *nsp_agnxtsubg(NspAgraph * G);
static int nsp_agdelsubg(NspAgraph * G,NspAgraph * Gsub);

/* graph methods related to nodes */

static NspAgnode *nsp_agfstnode(NspAgraph * g);
static NspAgnode *nsp_aglstnode(NspAgraph * g);
static NspAgnode *nsp_agnxtnode(NspAgraph * g, NspAgnode *n);
static NspAgnode *nsp_agprvnode(NspAgraph * g, NspAgnode *n);
static int nsp_agaddnodes(NspAgraph *G, NspSMatrix *S);
static int nsp_agaddedges(NspAgraph *G, NspSMatrix *S);
static NspAgnode *nsp_agfindnode_by_name(NspAgraph * g, char *name);

static NspAgraph *nsp_agread(void *chan);

/* common */

static char *nsp_agnameof_g(NspAgraph * G);
static char *nsp_agnameof_n(NspAgnode * N);
static char *nsp_agnameof_e(NspAgedge * E);

/* node methods */

static NspAgraph *nsp_agraphof(NspAgnode * N);

/* edge methods */

static NspAgnode *nsp_agtail(NspAgedge* E);
static NspAgnode *nsp_aghead(NspAgedge* E);

/* ref counts */
static int nsp_agattr_refcount_set(void *obj,int itype, int offset);
/* static int nsp_agattr_refcount_get(Agraph_t *obj,int itype); */

#line 193 "./agraph.h"
#endif /* NspAgraph_Private */

