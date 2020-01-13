/* -*- Mode: C -*- */
#ifndef NSP_INC_FRAME
#define NSP_INC_FRAME

/* Nsp
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

#include <nsp/object.h> 

/*
 * NspFrame inherits from NspObject
 */

typedef struct _NspFrame NspFrame;
typedef struct _NspTypeFrame NspTypeFrame;

typedef int (*frame_save) (XDR  *xdrs, NspFrame *M);

struct _NspTypeFrame { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

/* should be unique */

#define FRAME_AS_LIST

struct _NspFrame {
  /*< private >*/
  NspObject father; 
  NspTypeFrame *type; 
  /*< public >*/
  NspBHash *local_vars; /* used to make the link between local variables and their value */
  NspCells *locals; /* table of local variables */
#ifdef FRAME_AS_LIST
  NspList  *vars; /* struct to store non local variables */
#else 
  NspHash  *vars; /* struct to store non local variables */
#endif 
};

/* we use val to code the variable id in 0xffff and use 0xffff0000 
 * for tags 
 */

#define VAR_ID(val)  (val & 0xffff)
#define VAR_TAG(val) (val >> 16 )
#define VAR_GLOBAL   0x10000 
#define VAR_PERSISTENT   0x20000 
#define VAR_SET_PERSISTENT(val) (VAR_ID(val) + VAR_PERSISTENT) 
#define VAR_IS_PERSISTENT(val) (VAR_TAG(val) == 2)

extern const char nsp_frame_header[];

extern int nsp_type_frame_id;
extern NspTypeFrame *nsp_type_frame;

/* type instances for classa */

NspTypeFrame *new_type_frame(type_mode mode);

/* instance for Frame */

NspFrame *new_frame();

/*
 * Object methods redefined for frame 
 */

#define NULLFRAME (NspFrame*) 0

NspFrame *nsp_frame_create(const char *name, NspCells *C);
NspFrame *nsp_frame_copy(const NspFrame *H);
void nsp_frame_destroy(NspFrame *H);
void nsp_frame_info(NspFrame *H, int indent,const char *name, int rec_level);
int nsp_frame_print(NspFrame *H, int indent,const char *name, int rec_level);

/* from FrameObj.c */

typedef enum _nsp_frame_tag nsp_frame_tag;

enum _nsp_frame_tag {
  nsp_frame_local, nsp_frame_global, nsp_frame_caller, nsp_frame_constants};

extern NspObject *nsp_who(Stack *stack,const char *frame, int as_hash, int print_only, int *error);

extern NspFrame *nsp_frame_object (NspObject *O); 
extern int IsFrameObj (Stack stack, int i); 
extern int IsFrame(NspObject *O);
extern NspFrame *GetFrameCopy (Stack stack, int i); 
extern NspFrame *GetFrame (Stack stack, int i); 

extern NspObject *nsp_object_unframeize(const NspFrame *S);
extern NspObject * nsp_object_frameize(const NspObject *O);

extern NspMatrix *nsp_frame_to_matrix(const NspFrame *S);
extern NspFrame *nsp_matrix_to_frame(const NspMatrix *A);

extern NspObject *nsp_eframe_search_object(NspFrame *F,const char *name,int tag);
extern int nsp_eframe_replace_object(NspFrame *F, NspObject *A);
extern NspObject *nsp_eframe_search_and_remove_object(NspFrame *F,nsp_const_string str);
extern void nsp_eframe_remove_object(NspFrame *F,nsp_const_string str);
extern void nsp_eframe_remove_all_objects(NspFrame *F);
extern NspHash *nsp_eframe_to_hash(NspFrame *F);
extern NspSMatrix *nsp_eframe_to_smat(NspFrame *F);
extern int nsp_eframe_to_save( NspFile *file,NspFrame *F);
extern int nsp_eframe_set_persistent_value(NspFrame *F,NspObject *Obj, int *used);

#endif 

/* private part */

#ifdef Frame_Private 
static NspFrame *_nsp_frame_create(const char *name, NspCells *C,NspTypeBase *type);
static int init_frame(NspFrame *o,NspTypeFrame *type);
static int nsp_frame_size(NspFrame *Mat, int flag);
static char *nsp_frame_type_as_string(void);
static char *nsp_frame_type_short_string(NspObject *v);
static int nsp_frame_eq(NspFrame *A, NspObject *B);
static int nsp_frame_neq(NspFrame *A, NspObject *B);
/* 
static int nsp_frame_xdr_save(XDR *xdrs, NspFrame *M);
static NspFrame  *nsp_frame_xdr_load(XDR *xdrs);
*/
/* static AttrTab frame_attrs[]; */
static NspMethods *frame_get_methods(void); 
/* static int int_frame_create(Stack stack, int rhs, int opt, int lhs); */
#endif /* Frame_Private */
