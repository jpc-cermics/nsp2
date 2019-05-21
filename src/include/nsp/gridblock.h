/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGridBlock
#define NSP_INC_NspGridBlock

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

#line 4 "codegen/gridblock.override"
/* inserted at the start of include file */
#include <stdio.h>   /* for file declaration */
#include <nsp/figure.h>
#include <nsp/diagram.h>
#include <nsp/grint.h> /* interface definition */
#include <nsp/eval.h>/* interface definition */

/**
 * NspGridBlock:
 * @obj: a #nsp_block pointer 
 *
 * inherits from #NspObject and implements Grint. 
 * Used for graphic blocks for a C implementation of scicos.
 */

#line 41 "./gridblock.h"
/* NspGridBlock */

#include <nsp/block.h>

/*
 * NspGridBlock inherits from Block
 */

typedef struct _NspGridBlock NspGridBlock ;
typedef struct _NspTypeGridBlock NspTypeGridBlock ;

struct _NspTypeGridBlock {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gridblock nsp_gridblock;
struct _nsp_gridblock {
  void* diagram;
  int ref_count;
};

struct _NspGridBlock {
  /*< private >*/
  NspBlock father;
  NspTypeGridBlock*type;
  /*< public >*/
  nsp_gridblock *obj;
};

extern int nsp_type_gridblock_id;
extern NspTypeGridBlock *nsp_type_gridblock;

/* type instances for block */

NspTypeGridBlock *new_type_gridblock(type_mode mode);

/* instance for NspGridBlock */

NspGridBlock *new_gridblock();

/*
 * Object methods redefined for gridblock 
 */


#define NULLGRIDBLOCK (NspGridBlock*) 0

extern NspGridBlock *nsp_gridblock_create(const char *name,void* diagram,NspTypeBase *type);
extern NspGridBlock *nsp_gridblock_create_default(const char *name);

/* from NspGridBlockObj.c */

extern NspGridBlock *nsp_gridblock_copy(NspGridBlock *H);
extern void nsp_gridblock_destroy(NspGridBlock *H);
extern int nsp_gridblock_info(NspGridBlock *H, int indent,const char *name, int rec_level);
extern int nsp_gridblock_print(NspGridBlock *H, int indent,const char *name, int rec_level);
extern int nsp_gridblock_latex(NspGridBlock *H, int indent,const char *name, int rec_level);
extern NspGridBlock *nsp_gridblock_object (NspObject *O);
extern int IsGridBlockObj (Stack stack, int i);
extern int IsGridBlock(NspObject *O);
extern NspGridBlock *GetGridBlockCopy (Stack stack, int i);
extern NspGridBlock *GetGridBlock (Stack stack, int i);
extern int nsp_gridblock_create_partial(NspGridBlock *H);
extern void nsp_gridblock_destroy_partial(NspGridBlock *H);
extern NspGridBlock * nsp_gridblock_copy_partial(NspGridBlock *H,NspGridBlock *self);
extern NspGridBlock * nsp_gridblock_full_copy_partial(NspGridBlock *H,NspGridBlock *self);
extern NspGridBlock * nsp_gridblock_full_copy(NspGridBlock *self);
extern int nsp_gridblock_check_values(NspGridBlock *H);
extern int int_gridblock_create(Stack stack, int rhs, int opt, int lhs);
extern NspGridBlock *nsp_gridblock_xdr_load_partial(XDR *xdrs, NspGridBlock *M);
extern int nsp_gridblock_xdr_save(XDR  *xdrs, NspGridBlock *M);

#line 21 "codegen/gridblock.override"

/* inserted at the end of public part of include file */

NspGridBlock *nsp_gridblock_create_override(char *name,double *rect,int color,int thickness,int background);
NspGridBlock *nsp_gridblock_create_from_nsp_diagram(char *name,double *rect,int color,int thickness,int background, NspDiagram *D);
NspDiagram *nsp_gridblock_get_diagram(void *B1);

#line 124 "./gridblock.h"
#endif /* NSP_INC_NspGridBlock */ 

#ifdef NspGridBlock_Private 
static int init_gridblock(NspGridBlock *o,NspTypeGridBlock *type);
static int nsp_gridblock_size(NspGridBlock *Mat, int flag);
static char *nsp_gridblock_type_as_string(void);
static char *nsp_gridblock_type_short_string(NspObject *v);
static int nsp_gridblock_eq(NspGridBlock *A, NspObject *B);
static int nsp_gridblock_neq(NspGridBlock *A, NspObject *B);
static NspGridBlock *nsp_gridblock_xdr_load(XDR *xdrs);
static AttrTab gridblock_attrs[];
static NspMethods *gridblock_get_methods(void);
/* static int int_gridblock_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGridBlock *nsp_gridblock_create_void(const char *name,NspTypeBase *type);
#line 30 "codegen/gridblock.override"

/* local */

#line 143 "./gridblock.h"
#endif /* NspGridBlock_Private */

