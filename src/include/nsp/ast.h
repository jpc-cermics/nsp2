/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAst
#define NSP_INC_NspAst

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

#line 4 "codegen/ast.override"

#line 27 "./ast.h"
/* NspAst */

#include <nsp/object.h>

/*
 * NspAst inherits from Object
 */

typedef struct _NspAst NspAst ;
typedef struct _NspTypeAst NspTypeAst ;

struct _NspTypeAst {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspAst {
  /*< private >*/
  NspObject father;
  NspTypeAst*type;
  /*< public >*/
    int op;
  int arity;
  char* str;
  NspObject* xobj;
  NspList* args;
  NspObject* user_data;
  int line;
};

extern int nsp_type_ast_id;
extern NspTypeAst *nsp_type_ast;

/* type instances for object */

NspTypeAst *new_type_ast(type_mode mode);

/* instance for NspAst */

NspAst *new_ast();

/*
 * Object methods redefined for ast 
 */


#define NULLAST (NspAst*) 0

extern NspAst *nsp_ast_create(const char *name,int op,int arity,char* str,NspObject* xobj,NspList* args,NspObject* user_data,int line,NspTypeBase *type);
extern NspAst *nsp_ast_create_default(const char *name);

/* from NspAstObj.c */

extern NspAst *nsp_ast_copy(NspAst *H);
extern void nsp_ast_destroy(NspAst *H);
extern int nsp_ast_info(NspAst *H, int indent,const char *name, int rec_level);
extern int nsp_ast_print(NspAst *H, int indent,const char *name, int rec_level);
extern int nsp_ast_latex(NspAst *H, int indent,const char *name, int rec_level);
extern NspAst *nsp_ast_object (NspObject *O);
extern int IsAstObj (Stack stack, int i);
extern int IsAst(NspObject *O);
extern NspAst *GetAstCopy (Stack stack, int i);
extern NspAst *GetAst (Stack stack, int i);
extern int nsp_ast_create_partial(NspAst *H);
extern void nsp_ast_destroy_partial(NspAst *H);
extern NspAst * nsp_ast_copy_partial(NspAst *H,NspAst *self);
extern NspAst * nsp_ast_full_copy_partial(NspAst *H,NspAst *self);
extern NspAst * nsp_ast_full_copy(NspAst *self);
extern int nsp_ast_check_values(NspAst *H);
extern int int_ast_create(Stack stack, int rhs, int opt, int lhs);
extern NspAst *nsp_ast_xdr_load_partial(XDR *xdrs, NspAst *M);
extern int nsp_ast_xdr_save(XDR  *xdrs, NspAst *M);

#line 7 "codegen/ast.override"

/* inserted at the end of public part of class include file */
extern NspAst *nsp_plist_to_ast(const char *name,PList L);
extern NspAst* nsp_parse_file(char *Str);
extern NspAst * nsp_parse_from_smat(NspSMatrix *M);
extern int nsp_ast_set_str(NspAst *ast,const char *str);
extern int nsp_ast_check_args(NspList *L);
extern void nsp_ast_generic_pretty_printer(NspAst *ast, int indent, int color,int target, int space, int columns);
extern int nsp_ast_printlength(NspAst *ast, int indent);

#line 113 "./ast.h"
#endif /* NSP_INC_NspAst */ 

#ifdef NspAst_Private 
static int init_ast(NspAst *o,NspTypeAst *type);
static int nsp_ast_size(NspAst *Mat, int flag);
static char *nsp_ast_type_as_string(void);
static char *nsp_ast_type_short_string(NspObject *v);
static int nsp_ast_eq(NspAst *A, NspObject *B);
static int nsp_ast_neq(NspAst *A, NspObject *B);
static NspAst *nsp_ast_xdr_load(XDR *xdrs);
static AttrTab ast_attrs[];
static NspMethods *ast_get_methods(void);
/* static int int_ast_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAst *nsp_ast_create_void(const char *name,NspTypeBase *type);
#line 19 "codegen/ast.override"

/* inserted in the private part of include file
 * of classa.h
 */
static void nsp_ast_pprint(NspAst * L, int indent, int color,int target, int space, int columns);
static int meth_ast_print(NspAst *self,Stack stack, int rhs, int opt, int lhs);
static int meth_ast_sprint(NspAst *self,Stack stack, int rhs, int opt, int lhs);
static int meth_ast_fprint(NspAst *self,Stack stack, int rhs, int opt, int lhs);
static int nsp_ast_obj_equal(NspAst *ast1,NspAst *ast2);
static void nsp_ast_print_node(NspAst *ast);
static void nsp_ast_info_tree(NspAst *ast, int indent,const char *name,int rec_level);

#line 141 "./ast.h"
#endif /* NspAst_Private */

