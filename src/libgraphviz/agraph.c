/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
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





#line 73 "../types-test/codegen/agraph.override"
/* headers */

#line 31 "agraph.c"

/* ----------- NspAgraph ----------- */


#define  NspAgraph_Private 
#include <nsp/objects.h>
#include <nsp/agraph.h>
#include <nsp/interf.h>

/* 
 * NspAgraph inherits from Object 
 */

int nsp_type_agraph_id=0;
NspTypeAgraph *nsp_type_agraph=NULL;

/*
 * Type object for NspAgraph 
 * all the instance of NspTypeAgraph share the same id. 
 * nsp_type_agraph: is an instance of NspTypeAgraph 
 *    used for objects of NspAgraph type (i.e built with new_agraph) 
 * other instances are used for derived classes 
 */
NspTypeAgraph *new_type_agraph(type_mode mode)
{
  NspTypeAgraph *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_agraph != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_agraph;
    }
  if (( type =  malloc(sizeof(NspTypeAgraph))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = agraph_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = agraph_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_agraph;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for agraph */ 

  top->pr = (print_func *) nsp_agraph_print;
  top->dealloc = (dealloc_func *) nsp_agraph_destroy;
  top->copy  =  (copy_func *) nsp_agraph_copy;
  top->size  = (size_func *) nsp_agraph_size;
  top->s_type =  (s_type_func *) nsp_agraph_type_as_string;
  top->sh_type = (sh_type_func *) nsp_agraph_type_short_string;
  top->info = (info_func *) nsp_agraph_info;
  /* top->is_true = (is_true_func  *) nsp_agraph_is_true; */
  /* top->loop =(loop_func *) nsp_agraph_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_agraph_object;
  top->eq  = (eq_func *) nsp_agraph_eq;
  top->neq  = (eq_func *) nsp_agraph_neq;
  top->save  = (save_func *) nsp_agraph_xdr_save;
  top->load  = (load_func *) nsp_agraph_xdr_load;
  top->create = (create_func*) int_agraph_create;
  top->latex = (print_func *) nsp_agraph_latex;
  top->full_copy = (copy_func *) nsp_agraph_full_copy;

  /* specific methods for agraph */

  type->init = (init_func *) init_agraph;

  /* 
   * NspAgraph interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_agraph_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAgraph called nsp_type_agraph
       */
      type->id =  nsp_type_agraph_id = nsp_new_type_id();
      nsp_type_agraph = type;
      if ( nsp_register_type(nsp_type_agraph) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_agraph(mode);
    }
  else 
    {
      type->id = nsp_type_agraph_id;
      return type;
    }
}

/*
 * initialize NspAgraph instances 
 * locally and by calling initializer on parent class 
 */

static int init_agraph(NspAgraph *Obj,NspTypeAgraph *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspAgraph 
 */

NspAgraph *new_agraph() 
{
  NspAgraph *loc;
  /* type must exists */
  nsp_type_agraph = new_type_agraph(T_BASE);
  if ( (loc = malloc(sizeof(NspAgraph)))== NULLAGRAPH) return loc;
  /* initialize object */
  if ( init_agraph(loc,nsp_type_agraph) == FAIL) return NULLAGRAPH;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAgraph 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_agraph_size(NspAgraph *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char agraph_type_name[]="Agraph";
static char agraph_short_type_name[]="agraph";

static char *nsp_agraph_type_as_string(void)
{
  return(agraph_type_name);
}

static char *nsp_agraph_type_short_string(NspObject *v)
{
  return(agraph_short_type_name);
}

/*
 * A == B 
 */

static int nsp_agraph_eq(NspAgraph *A, NspObject *B)
{
  NspAgraph *loc = (NspAgraph *) B;
  if ( check_cast(B,nsp_type_agraph_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->graph != loc->obj->graph) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_agraph_neq(NspAgraph *A, NspObject *B)
{
  return ( nsp_agraph_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_agraph_xdr_save(XDR *xdrs, NspAgraph *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_agraph)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAgraph  *nsp_agraph_xdr_load_partial(XDR *xdrs, NspAgraph *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspAgraph  *nsp_agraph_xdr_load(XDR *xdrs)
{
  NspAgraph *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAGRAPH;
  if ((H  = nsp_agraph_create_void(name,(NspTypeBase *) nsp_type_agraph))== NULLAGRAPH) return H;
  if ( nsp_agraph_create_partial(H) == FAIL) return NULLAGRAPH;
  if ((H  = nsp_agraph_xdr_load_partial(xdrs,H))== NULLAGRAPH) return H;
  if ( nsp_agraph_check_values(H) == FAIL) return NULLAGRAPH;
  return H;
}

/*
 * delete 
 */

void nsp_agraph_destroy_partial(NspAgraph *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 142 "../types-test/codegen/agraph.override"
   /* verbatim in destroy */
/* agclose(H->obj->graph); */
   
#line 259 "agraph.c"
    FREE(H->obj);
   }
}

void nsp_agraph_destroy(NspAgraph *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_agraph_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_agraph_info(NspAgraph *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAGRAPH) 
    {
      Sciprintf("Null Pointer NspAgraph \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_agraph_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_agraph_print(NspAgraph *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAGRAPH) 
    {
      Sciprintf("Null Pointer NspAgraph \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_agraph_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_agraph_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"graph=0x%x\n",M->obj->graph);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_agraph_latex(NspAgraph *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_agraph_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"graph=0x%x\n",M->obj->graph);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAgraph objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAgraph   *nsp_agraph_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_agraph_id) == TRUE ) return ((NspAgraph *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_agraph));
  return NULL;
}

int IsAgraphObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_agraph_id);
}

int IsAgraph(NspObject *O)
{
  return nsp_object_type(O,nsp_type_agraph_id);
}

NspAgraph  *GetAgraphCopy(Stack stack, int i)
{
  if (  GetAgraph(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAgraph  *GetAgraph(Stack stack, int i)
{
  NspAgraph *M;
  if (( M = nsp_agraph_object(NthObj(i))) == NULLAGRAPH)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAgraph instance 
 *-----------------------------------------------------*/

static NspAgraph *nsp_agraph_create_void(const char *name,NspTypeBase *type)
{
 NspAgraph *H  = (type == NULL) ? new_agraph() : type->new();
 if ( H ==  NULLAGRAPH)
  {
   Sciprintf("No more memory\n");
   return NULLAGRAPH;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAGRAPH;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_agraph_create_partial(NspAgraph *H)
{
  if((H->obj = calloc(1,sizeof(nsp_agraph)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->graph = NULL;
  return OK;
}

int nsp_agraph_check_values(NspAgraph *H)
{
  return OK;
}

NspAgraph *nsp_agraph_create(const char *name,void* graph,NspTypeBase *type)
{
  NspAgraph *H  = nsp_agraph_create_void(name,type);
  if ( H ==  NULLAGRAPH) return NULLAGRAPH;
  if ( nsp_agraph_create_partial(H) == FAIL) return NULLAGRAPH;
  H->obj->graph = graph;
  if ( nsp_agraph_check_values(H) == FAIL) return NULLAGRAPH;
  return H;
}


NspAgraph *nsp_agraph_create_default(const char *name)
{
 NspAgraph *H  = nsp_agraph_create_void(name,NULL);
 if ( H ==  NULLAGRAPH) return NULLAGRAPH;
  if ( nsp_agraph_create_partial(H) == FAIL) return NULLAGRAPH;
 if ( nsp_agraph_check_values(H) == FAIL) return NULLAGRAPH;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAgraph *nsp_agraph_copy_partial(NspAgraph *H,NspAgraph *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAgraph *nsp_agraph_copy(NspAgraph *self)
{
  NspAgraph *H  =nsp_agraph_create_void(NVOID,(NspTypeBase *) nsp_type_agraph);
  if ( H ==  NULLAGRAPH) return NULLAGRAPH;
  if ( nsp_agraph_copy_partial(H,self)== NULL) return NULLAGRAPH;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAgraph *nsp_agraph_full_copy_partial(NspAgraph *H,NspAgraph *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_agraph))) == NULL) return NULLAGRAPH;
  H->obj->ref_count=1;
  H->obj->graph = self->obj->graph;
  return H;
}

NspAgraph *nsp_agraph_full_copy(NspAgraph *self)
{
  NspAgraph *H  =nsp_agraph_create_void(NVOID,(NspTypeBase *) nsp_type_agraph);
  if ( H ==  NULLAGRAPH) return NULLAGRAPH;
  if ( nsp_agraph_full_copy_partial(H,self)== NULL) return NULLAGRAPH;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAgraph
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 89 "../types-test/codegen/agraph.override"

/* describing the type of graph to be created. 
 * A graph can be directed or undirected. In addition, a graph can
 * be strict, i.e., have at most one edge between any pair of nodes, 
 * or non-strict, allowing an arbitrary number
 * of edges between two nodes. 
 Agundirected: Non-strict, undirected graph
 Agstrictundirected: Strict, undirected graph
 Agdirected: Non-strict, directed graph
 Agstrictdirected: Strict, directed graph
*/

int int_agraph_create(Stack stack, int rhs, int opt, int lhs)
{
  Agraph_t *g;
  NspAgraph *H;
  char *name = "G";
  char *type = "graph";
  nsp_option opts[] ={{ "type",string,NULLOBJ,-1},
		      { "name",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int rep;
  Agdesc_t itype = Agundirected;
  const char *t_choices[]={ "graph", "graphstrict","digraph","digraphstrict",  NULL };
  Agdesc_t t_itype[]={ Agundirected,Agstrictundirected, Agdirected , Agstrictdirected};
  CheckStdRhs(0,0);
  /* aginit(); can be called multiple times */
  if ( get_optional_args(stack,rhs,opt,opts,&type,&name) == FAIL) 
    return RET_BUG;
  rep = is_string_in_array(type, t_choices, 1);
  if ( rep < 0 )
    {
      string_not_in_array(stack, type, t_choices, "optional argument type");
      return RET_BUG;
    }
  itype = t_itype[rep];
  if (( g = agopen(name,itype,0))== NULL) 
    {
      Scierror("Error: agopen failed to create a graph\n");
      return RET_BUG;
    }
  if ((H = nsp_agraph_create(NVOID,g, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a graph\n");
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 


#line 523 "agraph.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_nsp_agnnodes(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = nsp_agnnodes(self);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_nsp_agnedges(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = nsp_agnedges(self);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_nsp_agparent(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgraph *ret;
  CheckRhs(0,0);
  ret = nsp_agparent(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agroot(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgraph *ret;
  CheckRhs(0,0);
  ret = nsp_agroot(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agnameof_g(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
  ret = nsp_agnameof_g(self);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

#line 326 "../types-test/codegen/agraph.override"

static int _wrap_nsp_agaddnodes(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {smat,t_end};
  NspSMatrix *nodes;
  int ret;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&nodes) == FAIL) return RET_BUG;
  ret = nsp_agaddnodes(self, nodes);
  if ( lhs == 1) 
    {
      if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
      return 1;
    }
  if ( ret != -1 ) 
    {
      Scierror("Error: failed to add node %s to the graph\n",nodes->S[ret]);
      return RET_BUG;
    }
  return 0;
}

#line 598 "agraph.c"


#line 350 "../types-test/codegen/agraph.override"

static int _wrap_nsp_agaddedges(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {smat,t_end};
  NspSMatrix *nodes;
  int ret;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&nodes) == FAIL) return RET_BUG;
  ret = nsp_agaddedges(self, nodes);
  if ( ret == FALSE ) 
    {
      return RET_BUG;
    }
  return 0;
}


#line 619 "agraph.c"


#line 268 "../types-test/codegen/agraph.override"

/* fix an attribute of a node */
static int _wrap_nsp_gv_agset_g(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  return _wrap_nsp_gv_agset_gen(self->obj->graph,stack,rhs,opt,lhs);
} 

#line 630 "agraph.c"


#line 253 "../types-test/codegen/agraph.override"

static int _wrap_nsp_gv_agget(NspAgnode *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *attr;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&attr) == FAIL) return RET_BUG;
  ret = agget(self->obj->node, attr);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}


#line 647 "agraph.c"


static int _wrap_nsp_gv_layout(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *type;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
  nsp_gv_layout(self, type);
  return 0;
}

static int _wrap_nsp_gv_render(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, string,t_end};
  char *type, *fname;
  if ( GetArgs(stack,rhs,opt,T,&type, &fname) == FAIL) return RET_BUG;
  nsp_gv_render(self, type, fname);
  return 0;
}

#line 147 "../types-test/codegen/agraph.override"

/* generic function used to set a graph attribute */

static int _wrap_nsp_gv_gattr_gen(NspAgraph *self,Stack stack,int rhs,int opt,int lhs,
				  int itype,const char *type)
{
  Agsym_t *a = NULL;
  int i;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  for ( i = 1 ; i <= rhs ; i++) 
    {
      char *value,*agstr;
      const char *attr;
      NspObject *O;
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: %s of method %s should be a named optional argument \n",
		   ArgPosition(i),NspFname(stack));
	  return RET_BUG;
	}
      /* A copy of object is added in the hash table **/
      /* GetObj takes care of Hobj pointers **/
      attr = nsp_object_get_name(NthObj(i));
      O = nsp_get_object(stack,i);
      if ( IsString(O) == FALSE )
	{
	  Scierror("Error: %s of method  %s should be a string\n",
		   ArgPosition(i),NspFname(stack));
	  return RET_BUG;
	}
      value =  ((NspSMatrix *) O)->S[0];
      agstr = nsp_string_copy(attr);
      if ( agstr == NULL) return RET_BUG;
      a = agattr( ((Agraph_t *) self->obj->graph)->root,itype, agstr,value);
      nsp_string_destroy(&agstr);
      if ( a == NULL) 
	{
	  Scierror("Error: failed to add %s attribute %s=%s\n",
		   type,  attr,((NspSMatrix *) O)->S[0]);
	  return RET_BUG;
	}
    }
  return 0;
} 

static int _wrap_nsp_gv_gattr(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  return _wrap_nsp_gv_gattr_gen(self,stack,rhs,opt,lhs,AGRAPH,"graph");
}


#line 721 "agraph.c"


static int _wrap_nsp_gv_graphattrs(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *ret;
  CheckRhs(0,0);
  ret = nsp_gv_graphattrs(self);
  if ( ret == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 201 "../types-test/codegen/agraph.override"

static int _wrap_nsp_gv_nattr(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  return _wrap_nsp_gv_gattr_gen(self,stack,rhs,opt,lhs,AGNODE,"node");
}

#line 741 "agraph.c"


static int _wrap_nsp_gv_nodeattrs(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *ret;
  CheckRhs(0,0);
  ret = nsp_gv_nodeattrs(self);
  if ( ret == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 209 "../types-test/codegen/agraph.override"

static int _wrap_nsp_gv_eattr(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  return _wrap_nsp_gv_gattr_gen(self,stack,rhs,opt,lhs,AGEDGE,"edge");
}

#line 761 "agraph.c"


static int _wrap_nsp_gv_edgeattrs(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *ret;
  CheckRhs(0,0);
  ret = nsp_gv_edgeattrs(self);
  if ( ret == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_write(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *chan;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&chan) == FAIL) return RET_BUG;
  ret = nsp_gv_write(self, chan);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 217 "../types-test/codegen/agraph.override"
static int _wrap_nsp_gv_isundirected(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  CheckLhs(0,1);
  ret = agisundirected(self->obj->graph);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 796 "agraph.c"


#line 229 "../types-test/codegen/agraph.override"
static int _wrap_nsp_gv_isdirected(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  CheckLhs(0,1);
  ret = agisdirected(self->obj->graph);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 810 "agraph.c"


#line 241 "../types-test/codegen/agraph.override"
static int _wrap_nsp_gv_isstrict(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  CheckLhs(0,1);
  ret = agisstrict(self->obj->graph);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 824 "agraph.c"


static int _wrap_nsp_agfstnode(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgnode *ret;
  CheckRhs(0,0);
  ret = nsp_agfstnode(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agnxtnode(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *n;
  NspAgnode *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &n) == FAIL) return RET_BUG;
  ret = nsp_agnxtnode(self, ((NspAgnode *) n));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_aglstnode(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgnode *ret;
  CheckRhs(0,0);
  ret = nsp_aglstnode(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agprvnode(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *n;
  NspAgnode *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &n) == FAIL) return RET_BUG;
  ret = nsp_agprvnode(self, ((NspAgnode *) n));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agfindnode_by_name(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *name;
  NspAgnode *ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  ret = nsp_agfindnode_by_name(self, name);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_agfstedge(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *n;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &n) == FAIL) return RET_BUG;
  ret = nsp_gv_agfstedge(self, ((NspAgnode *) n));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_agnxtedge(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check,t_end};
  NspObject *e, *n;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agedge, &e, &nsp_type_agnode, &n) == FAIL) return RET_BUG;
  ret = nsp_gv_agnxtedge(self, ((NspAgedge *) e), ((NspAgnode *) n));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_agfstin(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *n;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &n) == FAIL) return RET_BUG;
  ret = nsp_gv_agfstin(self, ((NspAgnode *) n));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_agnxtin(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *e;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agedge, &e) == FAIL) return RET_BUG;
  ret = nsp_gv_agnxtin(self, ((NspAgedge *) e));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_agfstout(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *n;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &n) == FAIL) return RET_BUG;
  ret = nsp_gv_agfstout(self, ((NspAgnode *) n));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_gv_agnxtout(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *e;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agedge, &e) == FAIL) return RET_BUG;
  ret = nsp_gv_agnxtout(self, ((NspAgedge *) e));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agsubg(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *name;
  NspAgraph *ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  ret = nsp_agsubg(self, name);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agfstsubg(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgraph *ret;
  CheckRhs(0,0);
  ret = nsp_agfstsubg(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agnxtsubg(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgraph *ret;
  CheckRhs(0,0);
  ret = nsp_agnxtsubg(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agdelsubg(NspAgraph *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *sub;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agraph, &sub) == FAIL) return RET_BUG;
  nsp_agdelsubg(self, ((NspAgraph *) sub));
  return 0;
}

static NspMethods agraph_methods[] = {
  {"nnodes",(nsp_method *) _wrap_nsp_agnnodes},
  {"nedges",(nsp_method *) _wrap_nsp_agnedges},
  {"parent",(nsp_method *) _wrap_nsp_agparent},
  {"root",(nsp_method *) _wrap_nsp_agroot},
  {"nameof",(nsp_method *) _wrap_nsp_agnameof_g},
  {"add_nodes",(nsp_method *) _wrap_nsp_agaddnodes},
  {"add_edges",(nsp_method *) _wrap_nsp_agaddedges},
  {"agset",(nsp_method *) _wrap_nsp_gv_agset_g},
  {"agget",(nsp_method *) _wrap_nsp_gv_agget},
  {"layout",(nsp_method *) _wrap_nsp_gv_layout},
  {"render",(nsp_method *) _wrap_nsp_gv_render},
  {"graphattr",(nsp_method *) _wrap_nsp_gv_gattr},
  {"graphattrs",(nsp_method *) _wrap_nsp_gv_graphattrs},
  {"nodeattr",(nsp_method *) _wrap_nsp_gv_nattr},
  {"nodeattrs",(nsp_method *) _wrap_nsp_gv_nodeattrs},
  {"edgeattr",(nsp_method *) _wrap_nsp_gv_eattr},
  {"edgeattrs",(nsp_method *) _wrap_nsp_gv_edgeattrs},
  {"write",(nsp_method *) _wrap_nsp_gv_write},
  {"isundirected",(nsp_method *) _wrap_nsp_gv_isundirected},
  {"isdirected",(nsp_method *) _wrap_nsp_gv_isdirected},
  {"isstrict",(nsp_method *) _wrap_nsp_gv_isstrict},
  {"fstnode",(nsp_method *) _wrap_nsp_agfstnode},
  {"nxtnode",(nsp_method *) _wrap_nsp_agnxtnode},
  {"lstnode",(nsp_method *) _wrap_nsp_aglstnode},
  {"prvnode",(nsp_method *) _wrap_nsp_agprvnode},
  {"findnode",(nsp_method *) _wrap_nsp_agfindnode_by_name},
  {"fstedge",(nsp_method *) _wrap_nsp_gv_agfstedge},
  {"nxtedge",(nsp_method *) _wrap_nsp_gv_agnxtedge},
  {"fstin",(nsp_method *) _wrap_nsp_gv_agfstin},
  {"nxtin",(nsp_method *) _wrap_nsp_gv_agnxtin},
  {"fstout",(nsp_method *) _wrap_nsp_gv_agfstout},
  {"nxtout",(nsp_method *) _wrap_nsp_gv_agnxtout},
  {"subg",(nsp_method *) _wrap_nsp_agsubg},
  {"fstsubg",(nsp_method *) _wrap_nsp_agfstsubg},
  {"nxtsubg",(nsp_method *) _wrap_nsp_agnxtsubg},
  {"delsubg",(nsp_method *) _wrap_nsp_agdelsubg},
  { NULL, NULL}
};

static NspMethods *agraph_get_methods(void) { return agraph_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab agraph_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;


/* ----------- NspAgnode ----------- */


#define  NspAgnode_Private 
#include <nsp/objects.h>
#include <nsp/agnode.h>
#include <nsp/interf.h>

/* 
 * NspAgnode inherits from Object 
 */

int nsp_type_agnode_id=0;
NspTypeAgnode *nsp_type_agnode=NULL;

/*
 * Type object for NspAgnode 
 * all the instance of NspTypeAgnode share the same id. 
 * nsp_type_agnode: is an instance of NspTypeAgnode 
 *    used for objects of NspAgnode type (i.e built with new_agnode) 
 * other instances are used for derived classes 
 */
NspTypeAgnode *new_type_agnode(type_mode mode)
{
  NspTypeAgnode *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_agnode != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_agnode;
    }
  if (( type =  malloc(sizeof(NspTypeAgnode))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = agnode_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = agnode_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_agnode;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for agnode */ 

  top->pr = (print_func *) nsp_agnode_print;
  top->dealloc = (dealloc_func *) nsp_agnode_destroy;
  top->copy  =  (copy_func *) nsp_agnode_copy;
  top->size  = (size_func *) nsp_agnode_size;
  top->s_type =  (s_type_func *) nsp_agnode_type_as_string;
  top->sh_type = (sh_type_func *) nsp_agnode_type_short_string;
  top->info = (info_func *) nsp_agnode_info;
  /* top->is_true = (is_true_func  *) nsp_agnode_is_true; */
  /* top->loop =(loop_func *) nsp_agnode_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_agnode_object;
  top->eq  = (eq_func *) nsp_agnode_eq;
  top->neq  = (eq_func *) nsp_agnode_neq;
  top->save  = (save_func *) nsp_agnode_xdr_save;
  top->load  = (load_func *) nsp_agnode_xdr_load;
  top->create = (create_func*) int_agnode_create;
  top->latex = (print_func *) nsp_agnode_latex;
  top->full_copy = (copy_func *) nsp_agnode_full_copy;

  /* specific methods for agnode */

  type->init = (init_func *) init_agnode;

  /* 
   * NspAgnode interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_agnode_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAgnode called nsp_type_agnode
       */
      type->id =  nsp_type_agnode_id = nsp_new_type_id();
      nsp_type_agnode = type;
      if ( nsp_register_type(nsp_type_agnode) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_agnode(mode);
    }
  else 
    {
      type->id = nsp_type_agnode_id;
      return type;
    }
}

/*
 * initialize NspAgnode instances 
 * locally and by calling initializer on parent class 
 */

static int init_agnode(NspAgnode *Obj,NspTypeAgnode *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspAgnode 
 */

NspAgnode *new_agnode() 
{
  NspAgnode *loc;
  /* type must exists */
  nsp_type_agnode = new_type_agnode(T_BASE);
  if ( (loc = malloc(sizeof(NspAgnode)))== NULLAGNODE) return loc;
  /* initialize object */
  if ( init_agnode(loc,nsp_type_agnode) == FAIL) return NULLAGNODE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAgnode 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_agnode_size(NspAgnode *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char agnode_type_name[]="Agnode";
static char agnode_short_type_name[]="agnode";

static char *nsp_agnode_type_as_string(void)
{
  return(agnode_type_name);
}

static char *nsp_agnode_type_short_string(NspObject *v)
{
  return(agnode_short_type_name);
}

/*
 * A == B 
 */

static int nsp_agnode_eq(NspAgnode *A, NspObject *B)
{
  NspAgnode *loc = (NspAgnode *) B;
  if ( check_cast(B,nsp_type_agnode_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->node != loc->obj->node) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_agnode_neq(NspAgnode *A, NspObject *B)
{
  return ( nsp_agnode_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_agnode_xdr_save(XDR *xdrs, NspAgnode *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_agnode)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAgnode  *nsp_agnode_xdr_load_partial(XDR *xdrs, NspAgnode *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspAgnode  *nsp_agnode_xdr_load(XDR *xdrs)
{
  NspAgnode *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAGNODE;
  if ((H  = nsp_agnode_create_void(name,(NspTypeBase *) nsp_type_agnode))== NULLAGNODE) return H;
  if ( nsp_agnode_create_partial(H) == FAIL) return NULLAGNODE;
  if ((H  = nsp_agnode_xdr_load_partial(xdrs,H))== NULLAGNODE) return H;
  if ( nsp_agnode_check_values(H) == FAIL) return NULLAGNODE;
  return H;
}

/*
 * delete 
 */

void nsp_agnode_destroy_partial(NspAgnode *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_agnode_destroy(NspAgnode *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_agnode_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_agnode_info(NspAgnode *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAGNODE) 
    {
      Sciprintf("Null Pointer NspAgnode \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_agnode_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_agnode_print(NspAgnode *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAGNODE) 
    {
      Sciprintf("Null Pointer NspAgnode \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_agnode_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_agnode_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"node=0x%x\n",M->obj->node);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_agnode_latex(NspAgnode *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_agnode_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"node=0x%x\n",M->obj->node);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAgnode objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAgnode   *nsp_agnode_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_agnode_id) == TRUE ) return ((NspAgnode *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_agnode));
  return NULL;
}

int IsAgnodeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_agnode_id);
}

int IsAgnode(NspObject *O)
{
  return nsp_object_type(O,nsp_type_agnode_id);
}

NspAgnode  *GetAgnodeCopy(Stack stack, int i)
{
  if (  GetAgnode(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAgnode  *GetAgnode(Stack stack, int i)
{
  NspAgnode *M;
  if (( M = nsp_agnode_object(NthObj(i))) == NULLAGNODE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAgnode instance 
 *-----------------------------------------------------*/

static NspAgnode *nsp_agnode_create_void(const char *name,NspTypeBase *type)
{
 NspAgnode *H  = (type == NULL) ? new_agnode() : type->new();
 if ( H ==  NULLAGNODE)
  {
   Sciprintf("No more memory\n");
   return NULLAGNODE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAGNODE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_agnode_create_partial(NspAgnode *H)
{
  if((H->obj = calloc(1,sizeof(nsp_agnode)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->node = NULL;
  return OK;
}

int nsp_agnode_check_values(NspAgnode *H)
{
  return OK;
}

NspAgnode *nsp_agnode_create(const char *name,void* node,NspTypeBase *type)
{
  NspAgnode *H  = nsp_agnode_create_void(name,type);
  if ( H ==  NULLAGNODE) return NULLAGNODE;
  if ( nsp_agnode_create_partial(H) == FAIL) return NULLAGNODE;
  H->obj->node = node;
  if ( nsp_agnode_check_values(H) == FAIL) return NULLAGNODE;
  return H;
}


NspAgnode *nsp_agnode_create_default(const char *name)
{
 NspAgnode *H  = nsp_agnode_create_void(name,NULL);
 if ( H ==  NULLAGNODE) return NULLAGNODE;
  if ( nsp_agnode_create_partial(H) == FAIL) return NULLAGNODE;
 if ( nsp_agnode_check_values(H) == FAIL) return NULLAGNODE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAgnode *nsp_agnode_copy_partial(NspAgnode *H,NspAgnode *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAgnode *nsp_agnode_copy(NspAgnode *self)
{
  NspAgnode *H  =nsp_agnode_create_void(NVOID,(NspTypeBase *) nsp_type_agnode);
  if ( H ==  NULLAGNODE) return NULLAGNODE;
  if ( nsp_agnode_copy_partial(H,self)== NULL) return NULLAGNODE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAgnode *nsp_agnode_full_copy_partial(NspAgnode *H,NspAgnode *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_agnode))) == NULL) return NULLAGNODE;
  H->obj->ref_count=1;
  H->obj->node = self->obj->node;
  return H;
}

NspAgnode *nsp_agnode_full_copy(NspAgnode *self)
{
  NspAgnode *H  =nsp_agnode_create_void(NVOID,(NspTypeBase *) nsp_type_agnode);
  if ( H ==  NULLAGNODE) return NULLAGNODE;
  if ( nsp_agnode_full_copy_partial(H,self)== NULL) return NULLAGNODE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAgnode
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_agnode_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAgnode *H;
  CheckStdRhs(0,0);
  /* want to be sure that type agnode is initialized */
  nsp_type_agnode = new_type_agnode(T_BASE);
  if(( H = nsp_agnode_create_void(NVOID,(NspTypeBase *) nsp_type_agnode)) == NULLAGNODE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_agnode_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_agnode_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_nsp_agnameof_n(NspAgnode *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
  ret = nsp_agnameof_n(self);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_nsp_agraphof(NspAgnode *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgraph *ret;
  CheckRhs(0,0);
  ret = nsp_agraphof(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 277 "../types-test/codegen/agraph.override"

/* fix an attribute of a node */
static int _wrap_nsp_gv_agset_n(NspAgnode *self,Stack stack,int rhs,int opt,int lhs)
{
  return _wrap_nsp_gv_agset_gen(self->obj->node,stack,rhs,opt,lhs);
} 

#line 1523 "agraph.c"


static NspMethods agnode_methods[] = {
  {"nameof",(nsp_method *) _wrap_nsp_agnameof_n},
  {"agraphof",(nsp_method *) _wrap_nsp_agraphof},
  {"agget",(nsp_method *) _wrap_nsp_gv_agget},
  {"agset",(nsp_method *) _wrap_nsp_gv_agset_n},
  { NULL, NULL}
};

static NspMethods *agnode_get_methods(void) { return agnode_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab agnode_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;


/* ----------- NspAgedge ----------- */


#define  NspAgedge_Private 
#include <nsp/objects.h>
#include <nsp/agedge.h>
#include <nsp/interf.h>

/* 
 * NspAgedge inherits from Object 
 */

int nsp_type_agedge_id=0;
NspTypeAgedge *nsp_type_agedge=NULL;

/*
 * Type object for NspAgedge 
 * all the instance of NspTypeAgedge share the same id. 
 * nsp_type_agedge: is an instance of NspTypeAgedge 
 *    used for objects of NspAgedge type (i.e built with new_agedge) 
 * other instances are used for derived classes 
 */
NspTypeAgedge *new_type_agedge(type_mode mode)
{
  NspTypeAgedge *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_agedge != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_agedge;
    }
  if (( type =  malloc(sizeof(NspTypeAgedge))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = agedge_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = agedge_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_agedge;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for agedge */ 

  top->pr = (print_func *) nsp_agedge_print;
  top->dealloc = (dealloc_func *) nsp_agedge_destroy;
  top->copy  =  (copy_func *) nsp_agedge_copy;
  top->size  = (size_func *) nsp_agedge_size;
  top->s_type =  (s_type_func *) nsp_agedge_type_as_string;
  top->sh_type = (sh_type_func *) nsp_agedge_type_short_string;
  top->info = (info_func *) nsp_agedge_info;
  /* top->is_true = (is_true_func  *) nsp_agedge_is_true; */
  /* top->loop =(loop_func *) nsp_agedge_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_agedge_object;
  top->eq  = (eq_func *) nsp_agedge_eq;
  top->neq  = (eq_func *) nsp_agedge_neq;
  top->save  = (save_func *) nsp_agedge_xdr_save;
  top->load  = (load_func *) nsp_agedge_xdr_load;
  top->create = (create_func*) int_agedge_create;
  top->latex = (print_func *) nsp_agedge_latex;
  top->full_copy = (copy_func *) nsp_agedge_full_copy;

  /* specific methods for agedge */

  type->init = (init_func *) init_agedge;

  /* 
   * NspAgedge interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_agedge_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAgedge called nsp_type_agedge
       */
      type->id =  nsp_type_agedge_id = nsp_new_type_id();
      nsp_type_agedge = type;
      if ( nsp_register_type(nsp_type_agedge) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_agedge(mode);
    }
  else 
    {
      type->id = nsp_type_agedge_id;
      return type;
    }
}

/*
 * initialize NspAgedge instances 
 * locally and by calling initializer on parent class 
 */

static int init_agedge(NspAgedge *Obj,NspTypeAgedge *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspAgedge 
 */

NspAgedge *new_agedge() 
{
  NspAgedge *loc;
  /* type must exists */
  nsp_type_agedge = new_type_agedge(T_BASE);
  if ( (loc = malloc(sizeof(NspAgedge)))== NULLAGEDGE) return loc;
  /* initialize object */
  if ( init_agedge(loc,nsp_type_agedge) == FAIL) return NULLAGEDGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAgedge 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_agedge_size(NspAgedge *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char agedge_type_name[]="Agedge";
static char agedge_short_type_name[]="agedge";

static char *nsp_agedge_type_as_string(void)
{
  return(agedge_type_name);
}

static char *nsp_agedge_type_short_string(NspObject *v)
{
  return(agedge_short_type_name);
}

/*
 * A == B 
 */

static int nsp_agedge_eq(NspAgedge *A, NspObject *B)
{
  NspAgedge *loc = (NspAgedge *) B;
  if ( check_cast(B,nsp_type_agedge_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->edge != loc->obj->edge) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_agedge_neq(NspAgedge *A, NspObject *B)
{
  return ( nsp_agedge_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_agedge_xdr_save(XDR *xdrs, NspAgedge *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_agedge)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAgedge  *nsp_agedge_xdr_load_partial(XDR *xdrs, NspAgedge *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspAgedge  *nsp_agedge_xdr_load(XDR *xdrs)
{
  NspAgedge *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAGEDGE;
  if ((H  = nsp_agedge_create_void(name,(NspTypeBase *) nsp_type_agedge))== NULLAGEDGE) return H;
  if ( nsp_agedge_create_partial(H) == FAIL) return NULLAGEDGE;
  if ((H  = nsp_agedge_xdr_load_partial(xdrs,H))== NULLAGEDGE) return H;
  if ( nsp_agedge_check_values(H) == FAIL) return NULLAGEDGE;
  return H;
}

/*
 * delete 
 */

void nsp_agedge_destroy_partial(NspAgedge *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_agedge_destroy(NspAgedge *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_agedge_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_agedge_info(NspAgedge *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAGEDGE) 
    {
      Sciprintf("Null Pointer NspAgedge \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_agedge_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_agedge_print(NspAgedge *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAGEDGE) 
    {
      Sciprintf("Null Pointer NspAgedge \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_agedge_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_agedge_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"edge=0x%x\n",M->obj->edge);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_agedge_latex(NspAgedge *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_agedge_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"edge=0x%x\n",M->obj->edge);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAgedge objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAgedge   *nsp_agedge_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_agedge_id) == TRUE ) return ((NspAgedge *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_agedge));
  return NULL;
}

int IsAgedgeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_agedge_id);
}

int IsAgedge(NspObject *O)
{
  return nsp_object_type(O,nsp_type_agedge_id);
}

NspAgedge  *GetAgedgeCopy(Stack stack, int i)
{
  if (  GetAgedge(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAgedge  *GetAgedge(Stack stack, int i)
{
  NspAgedge *M;
  if (( M = nsp_agedge_object(NthObj(i))) == NULLAGEDGE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAgedge instance 
 *-----------------------------------------------------*/

static NspAgedge *nsp_agedge_create_void(const char *name,NspTypeBase *type)
{
 NspAgedge *H  = (type == NULL) ? new_agedge() : type->new();
 if ( H ==  NULLAGEDGE)
  {
   Sciprintf("No more memory\n");
   return NULLAGEDGE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAGEDGE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_agedge_create_partial(NspAgedge *H)
{
  if((H->obj = calloc(1,sizeof(nsp_agedge)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->edge = NULL;
  return OK;
}

int nsp_agedge_check_values(NspAgedge *H)
{
  return OK;
}

NspAgedge *nsp_agedge_create(const char *name,void* edge,NspTypeBase *type)
{
  NspAgedge *H  = nsp_agedge_create_void(name,type);
  if ( H ==  NULLAGEDGE) return NULLAGEDGE;
  if ( nsp_agedge_create_partial(H) == FAIL) return NULLAGEDGE;
  H->obj->edge = edge;
  if ( nsp_agedge_check_values(H) == FAIL) return NULLAGEDGE;
  return H;
}


NspAgedge *nsp_agedge_create_default(const char *name)
{
 NspAgedge *H  = nsp_agedge_create_void(name,NULL);
 if ( H ==  NULLAGEDGE) return NULLAGEDGE;
  if ( nsp_agedge_create_partial(H) == FAIL) return NULLAGEDGE;
 if ( nsp_agedge_check_values(H) == FAIL) return NULLAGEDGE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAgedge *nsp_agedge_copy_partial(NspAgedge *H,NspAgedge *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAgedge *nsp_agedge_copy(NspAgedge *self)
{
  NspAgedge *H  =nsp_agedge_create_void(NVOID,(NspTypeBase *) nsp_type_agedge);
  if ( H ==  NULLAGEDGE) return NULLAGEDGE;
  if ( nsp_agedge_copy_partial(H,self)== NULL) return NULLAGEDGE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAgedge *nsp_agedge_full_copy_partial(NspAgedge *H,NspAgedge *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_agedge))) == NULL) return NULLAGEDGE;
  H->obj->ref_count=1;
  H->obj->edge = self->obj->edge;
  return H;
}

NspAgedge *nsp_agedge_full_copy(NspAgedge *self)
{
  NspAgedge *H  =nsp_agedge_create_void(NVOID,(NspTypeBase *) nsp_type_agedge);
  if ( H ==  NULLAGEDGE) return NULLAGEDGE;
  if ( nsp_agedge_full_copy_partial(H,self)== NULL) return NULLAGEDGE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAgedge
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_agedge_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAgedge *H;
  CheckStdRhs(0,0);
  /* want to be sure that type agedge is initialized */
  nsp_type_agedge = new_type_agedge(T_BASE);
  if(( H = nsp_agedge_create_void(NVOID,(NspTypeBase *) nsp_type_agedge)) == NULLAGEDGE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_agedge_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_agedge_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_nsp_agnameof_e(NspAgedge *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
  ret = nsp_agnameof_e(self);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_nsp_aghead(NspAgedge *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgnode *ret;
  CheckRhs(0,0);
  ret = nsp_aghead(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_agtail(NspAgedge *self,Stack stack,int rhs,int opt,int lhs)
{
  NspAgnode *ret;
  CheckRhs(0,0);
  ret = nsp_agtail(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 286 "../types-test/codegen/agraph.override"

/* fix an attribute of a node */
static int _wrap_nsp_gv_agset_e(NspAgedge *self,Stack stack,int rhs,int opt,int lhs)
{
  return _wrap_nsp_gv_agset_gen(self->obj->edge,stack,rhs,opt,lhs);
} 

#line 2031 "agraph.c"


static NspMethods agedge_methods[] = {
  {"nameof",(nsp_method *) _wrap_nsp_agnameof_e},
  {"head",(nsp_method *) _wrap_nsp_aghead},
  {"tail",(nsp_method *) _wrap_nsp_agtail},
  {"agget",(nsp_method *) _wrap_nsp_gv_agget},
  {"agset",(nsp_method *) _wrap_nsp_gv_agset_e},
  { NULL, NULL}
};

static NspMethods *agedge_get_methods(void) { return agedge_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab agedge_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;


/* ----------- NspAgsym ----------- */


#define  NspAgsym_Private 
#include <nsp/objects.h>
#include <nsp/agsym.h>
#include <nsp/interf.h>

/* 
 * NspAgsym inherits from Object 
 */

int nsp_type_agsym_id=0;
NspTypeAgsym *nsp_type_agsym=NULL;

/*
 * Type object for NspAgsym 
 * all the instance of NspTypeAgsym share the same id. 
 * nsp_type_agsym: is an instance of NspTypeAgsym 
 *    used for objects of NspAgsym type (i.e built with new_agsym) 
 * other instances are used for derived classes 
 */
NspTypeAgsym *new_type_agsym(type_mode mode)
{
  NspTypeAgsym *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_agsym != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_agsym;
    }
  if (( type =  malloc(sizeof(NspTypeAgsym))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = agsym_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = agsym_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_agsym;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for agsym */ 

  top->pr = (print_func *) nsp_agsym_print;
  top->dealloc = (dealloc_func *) nsp_agsym_destroy;
  top->copy  =  (copy_func *) nsp_agsym_copy;
  top->size  = (size_func *) nsp_agsym_size;
  top->s_type =  (s_type_func *) nsp_agsym_type_as_string;
  top->sh_type = (sh_type_func *) nsp_agsym_type_short_string;
  top->info = (info_func *) nsp_agsym_info;
  /* top->is_true = (is_true_func  *) nsp_agsym_is_true; */
  /* top->loop =(loop_func *) nsp_agsym_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_agsym_object;
  top->eq  = (eq_func *) nsp_agsym_eq;
  top->neq  = (eq_func *) nsp_agsym_neq;
  top->save  = (save_func *) nsp_agsym_xdr_save;
  top->load  = (load_func *) nsp_agsym_xdr_load;
  top->create = (create_func*) int_agsym_create;
  top->latex = (print_func *) nsp_agsym_latex;
  top->full_copy = (copy_func *) nsp_agsym_full_copy;

  /* specific methods for agsym */

  type->init = (init_func *) init_agsym;

  /* 
   * NspAgsym interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_agsym_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAgsym called nsp_type_agsym
       */
      type->id =  nsp_type_agsym_id = nsp_new_type_id();
      nsp_type_agsym = type;
      if ( nsp_register_type(nsp_type_agsym) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_agsym(mode);
    }
  else 
    {
      type->id = nsp_type_agsym_id;
      return type;
    }
}

/*
 * initialize NspAgsym instances 
 * locally and by calling initializer on parent class 
 */

static int init_agsym(NspAgsym *Obj,NspTypeAgsym *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspAgsym 
 */

NspAgsym *new_agsym() 
{
  NspAgsym *loc;
  /* type must exists */
  nsp_type_agsym = new_type_agsym(T_BASE);
  if ( (loc = malloc(sizeof(NspAgsym)))== NULLAGSYM) return loc;
  /* initialize object */
  if ( init_agsym(loc,nsp_type_agsym) == FAIL) return NULLAGSYM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAgsym 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_agsym_size(NspAgsym *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char agsym_type_name[]="Agsym";
static char agsym_short_type_name[]="agsym";

static char *nsp_agsym_type_as_string(void)
{
  return(agsym_type_name);
}

static char *nsp_agsym_type_short_string(NspObject *v)
{
  return(agsym_short_type_name);
}

/*
 * A == B 
 */

static int nsp_agsym_eq(NspAgsym *A, NspObject *B)
{
  NspAgsym *loc = (NspAgsym *) B;
  if ( check_cast(B,nsp_type_agsym_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_agsym_neq(NspAgsym *A, NspObject *B)
{
  return ( nsp_agsym_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_agsym_xdr_save(XDR *xdrs, NspAgsym *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_agsym)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAgsym  *nsp_agsym_xdr_load_partial(XDR *xdrs, NspAgsym *M)
{
  M->obj->ref_count=1;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
 return M;
}

static NspAgsym  *nsp_agsym_xdr_load(XDR *xdrs)
{
  NspAgsym *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAGSYM;
  if ((H  = nsp_agsym_create_void(name,(NspTypeBase *) nsp_type_agsym))== NULLAGSYM) return H;
  if ( nsp_agsym_create_partial(H) == FAIL) return NULLAGSYM;
  if ((H  = nsp_agsym_xdr_load_partial(xdrs,H))== NULLAGSYM) return H;
  if ( nsp_agsym_check_values(H) == FAIL) return NULLAGSYM;
  return H;
}

/*
 * delete 
 */

void nsp_agsym_destroy_partial(NspAgsym *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->Mcoord != NULL ) 
      nsp_matrix_destroy(H->obj->Mcoord);
    FREE(H->obj);
   }
}

void nsp_agsym_destroy(NspAgsym *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_agsym_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_agsym_info(NspAgsym *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAGSYM) 
    {
      Sciprintf("Null Pointer NspAgsym \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_agsym_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_agsym_print(NspAgsym *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAGSYM) 
    {
      Sciprintf("Null Pointer NspAgsym \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_agsym_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_agsym_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_agsym_latex(NspAgsym *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_agsym_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAgsym objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAgsym   *nsp_agsym_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_agsym_id) == TRUE ) return ((NspAgsym *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_agsym));
  return NULL;
}

int IsAgsymObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_agsym_id);
}

int IsAgsym(NspObject *O)
{
  return nsp_object_type(O,nsp_type_agsym_id);
}

NspAgsym  *GetAgsymCopy(Stack stack, int i)
{
  if (  GetAgsym(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAgsym  *GetAgsym(Stack stack, int i)
{
  NspAgsym *M;
  if (( M = nsp_agsym_object(NthObj(i))) == NULLAGSYM)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAgsym instance 
 *-----------------------------------------------------*/

static NspAgsym *nsp_agsym_create_void(const char *name,NspTypeBase *type)
{
 NspAgsym *H  = (type == NULL) ? new_agsym() : type->new();
 if ( H ==  NULLAGSYM)
  {
   Sciprintf("No more memory\n");
   return NULLAGSYM;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAGSYM;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_agsym_create_partial(NspAgsym *H)
{
  if((H->obj = calloc(1,sizeof(nsp_agsym)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  return OK;
}

int nsp_agsym_check_values(NspAgsym *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  return OK;
}

NspAgsym *nsp_agsym_create(const char *name,NspMatrix* Mcoord,NspTypeBase *type)
{
  NspAgsym *H  = nsp_agsym_create_void(name,type);
  if ( H ==  NULLAGSYM) return NULLAGSYM;
  if ( nsp_agsym_create_partial(H) == FAIL) return NULLAGSYM;
  H->obj->Mcoord= Mcoord;
  if ( nsp_agsym_check_values(H) == FAIL) return NULLAGSYM;
  return H;
}


NspAgsym *nsp_agsym_create_default(const char *name)
{
 NspAgsym *H  = nsp_agsym_create_void(name,NULL);
 if ( H ==  NULLAGSYM) return NULLAGSYM;
  if ( nsp_agsym_create_partial(H) == FAIL) return NULLAGSYM;
 if ( nsp_agsym_check_values(H) == FAIL) return NULLAGSYM;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAgsym *nsp_agsym_copy_partial(NspAgsym *H,NspAgsym *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAgsym *nsp_agsym_copy(NspAgsym *self)
{
  NspAgsym *H  =nsp_agsym_create_void(NVOID,(NspTypeBase *) nsp_type_agsym);
  if ( H ==  NULLAGSYM) return NULLAGSYM;
  if ( nsp_agsym_copy_partial(H,self)== NULL) return NULLAGSYM;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAgsym *nsp_agsym_full_copy_partial(NspAgsym *H,NspAgsym *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_agsym))) == NULL) return NULLAGSYM;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_full_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  return H;
}

NspAgsym *nsp_agsym_full_copy(NspAgsym *self)
{
  NspAgsym *H  =nsp_agsym_create_void(NVOID,(NspTypeBase *) nsp_type_agsym);
  if ( H ==  NULLAGSYM) return NULLAGSYM;
  if ( nsp_agsym_full_copy_partial(H,self)== NULL) return NULLAGSYM;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAgsym
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_agsym_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAgsym *H;
  CheckStdRhs(0,0);
  /* want to be sure that type agsym is initialized */
  nsp_type_agsym = new_type_agsym(T_BASE);
  if(( H = nsp_agsym_create_void(NVOID,(NspTypeBase *) nsp_type_agsym)) == NULLAGSYM) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_agsym_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_agsym_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *agsym_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_agsym_get_Mcoord(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAgsym *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_agsym_get_obj_Mcoord(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAgsym *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_agsym_set_Mcoord(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcoord;
  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAgsym *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspAgsym *) self)->obj->Mcoord);
  ((NspAgsym *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static AttrTab agsym_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_agsym_get_Mcoord, (attr_set_function *)_wrap_agsym_set_Mcoord,(attr_get_object_function *)_wrap_agsym_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};



/* ----------- NspAgdisc ----------- */


#define  NspAgdisc_Private 
#include <nsp/objects.h>
#include <nsp/agdisc.h>
#include <nsp/interf.h>

/* 
 * NspAgdisc inherits from Object 
 */

int nsp_type_agdisc_id=0;
NspTypeAgdisc *nsp_type_agdisc=NULL;

/*
 * Type object for NspAgdisc 
 * all the instance of NspTypeAgdisc share the same id. 
 * nsp_type_agdisc: is an instance of NspTypeAgdisc 
 *    used for objects of NspAgdisc type (i.e built with new_agdisc) 
 * other instances are used for derived classes 
 */
NspTypeAgdisc *new_type_agdisc(type_mode mode)
{
  NspTypeAgdisc *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_agdisc != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_agdisc;
    }
  if (( type =  malloc(sizeof(NspTypeAgdisc))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = agdisc_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = agdisc_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_agdisc;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for agdisc */ 

  top->pr = (print_func *) nsp_agdisc_print;
  top->dealloc = (dealloc_func *) nsp_agdisc_destroy;
  top->copy  =  (copy_func *) nsp_agdisc_copy;
  top->size  = (size_func *) nsp_agdisc_size;
  top->s_type =  (s_type_func *) nsp_agdisc_type_as_string;
  top->sh_type = (sh_type_func *) nsp_agdisc_type_short_string;
  top->info = (info_func *) nsp_agdisc_info;
  /* top->is_true = (is_true_func  *) nsp_agdisc_is_true; */
  /* top->loop =(loop_func *) nsp_agdisc_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_agdisc_object;
  top->eq  = (eq_func *) nsp_agdisc_eq;
  top->neq  = (eq_func *) nsp_agdisc_neq;
  top->save  = (save_func *) nsp_agdisc_xdr_save;
  top->load  = (load_func *) nsp_agdisc_xdr_load;
  top->create = (create_func*) int_agdisc_create;
  top->latex = (print_func *) nsp_agdisc_latex;
  top->full_copy = (copy_func *) nsp_agdisc_full_copy;

  /* specific methods for agdisc */

  type->init = (init_func *) init_agdisc;

  /* 
   * NspAgdisc interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_agdisc_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAgdisc called nsp_type_agdisc
       */
      type->id =  nsp_type_agdisc_id = nsp_new_type_id();
      nsp_type_agdisc = type;
      if ( nsp_register_type(nsp_type_agdisc) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_agdisc(mode);
    }
  else 
    {
      type->id = nsp_type_agdisc_id;
      return type;
    }
}

/*
 * initialize NspAgdisc instances 
 * locally and by calling initializer on parent class 
 */

static int init_agdisc(NspAgdisc *Obj,NspTypeAgdisc *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspAgdisc 
 */

NspAgdisc *new_agdisc() 
{
  NspAgdisc *loc;
  /* type must exists */
  nsp_type_agdisc = new_type_agdisc(T_BASE);
  if ( (loc = malloc(sizeof(NspAgdisc)))== NULLAGDISC) return loc;
  /* initialize object */
  if ( init_agdisc(loc,nsp_type_agdisc) == FAIL) return NULLAGDISC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAgdisc 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_agdisc_size(NspAgdisc *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char agdisc_type_name[]="Agdisc";
static char agdisc_short_type_name[]="agdisc";

static char *nsp_agdisc_type_as_string(void)
{
  return(agdisc_type_name);
}

static char *nsp_agdisc_type_short_string(NspObject *v)
{
  return(agdisc_short_type_name);
}

/*
 * A == B 
 */

static int nsp_agdisc_eq(NspAgdisc *A, NspObject *B)
{
  NspAgdisc *loc = (NspAgdisc *) B;
  if ( check_cast(B,nsp_type_agdisc_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_agdisc_neq(NspAgdisc *A, NspObject *B)
{
  return ( nsp_agdisc_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_agdisc_xdr_save(XDR *xdrs, NspAgdisc *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_agdisc)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAgdisc  *nsp_agdisc_xdr_load_partial(XDR *xdrs, NspAgdisc *M)
{
  M->obj->ref_count=1;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
 return M;
}

static NspAgdisc  *nsp_agdisc_xdr_load(XDR *xdrs)
{
  NspAgdisc *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAGDISC;
  if ((H  = nsp_agdisc_create_void(name,(NspTypeBase *) nsp_type_agdisc))== NULLAGDISC) return H;
  if ( nsp_agdisc_create_partial(H) == FAIL) return NULLAGDISC;
  if ((H  = nsp_agdisc_xdr_load_partial(xdrs,H))== NULLAGDISC) return H;
  if ( nsp_agdisc_check_values(H) == FAIL) return NULLAGDISC;
  return H;
}

/*
 * delete 
 */

void nsp_agdisc_destroy_partial(NspAgdisc *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->Mcoord != NULL ) 
      nsp_matrix_destroy(H->obj->Mcoord);
    FREE(H->obj);
   }
}

void nsp_agdisc_destroy(NspAgdisc *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_agdisc_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_agdisc_info(NspAgdisc *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAGDISC) 
    {
      Sciprintf("Null Pointer NspAgdisc \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_agdisc_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_agdisc_print(NspAgdisc *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAGDISC) 
    {
      Sciprintf("Null Pointer NspAgdisc \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_agdisc_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_agdisc_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_agdisc_latex(NspAgdisc *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_agdisc_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAgdisc objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAgdisc   *nsp_agdisc_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_agdisc_id) == TRUE ) return ((NspAgdisc *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_agdisc));
  return NULL;
}

int IsAgdiscObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_agdisc_id);
}

int IsAgdisc(NspObject *O)
{
  return nsp_object_type(O,nsp_type_agdisc_id);
}

NspAgdisc  *GetAgdiscCopy(Stack stack, int i)
{
  if (  GetAgdisc(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAgdisc  *GetAgdisc(Stack stack, int i)
{
  NspAgdisc *M;
  if (( M = nsp_agdisc_object(NthObj(i))) == NULLAGDISC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAgdisc instance 
 *-----------------------------------------------------*/

static NspAgdisc *nsp_agdisc_create_void(const char *name,NspTypeBase *type)
{
 NspAgdisc *H  = (type == NULL) ? new_agdisc() : type->new();
 if ( H ==  NULLAGDISC)
  {
   Sciprintf("No more memory\n");
   return NULLAGDISC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAGDISC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_agdisc_create_partial(NspAgdisc *H)
{
  if((H->obj = calloc(1,sizeof(nsp_agdisc)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  return OK;
}

int nsp_agdisc_check_values(NspAgdisc *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  return OK;
}

NspAgdisc *nsp_agdisc_create(const char *name,NspMatrix* Mcoord,NspTypeBase *type)
{
  NspAgdisc *H  = nsp_agdisc_create_void(name,type);
  if ( H ==  NULLAGDISC) return NULLAGDISC;
  if ( nsp_agdisc_create_partial(H) == FAIL) return NULLAGDISC;
  H->obj->Mcoord= Mcoord;
  if ( nsp_agdisc_check_values(H) == FAIL) return NULLAGDISC;
  return H;
}


NspAgdisc *nsp_agdisc_create_default(const char *name)
{
 NspAgdisc *H  = nsp_agdisc_create_void(name,NULL);
 if ( H ==  NULLAGDISC) return NULLAGDISC;
  if ( nsp_agdisc_create_partial(H) == FAIL) return NULLAGDISC;
 if ( nsp_agdisc_check_values(H) == FAIL) return NULLAGDISC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAgdisc *nsp_agdisc_copy_partial(NspAgdisc *H,NspAgdisc *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAgdisc *nsp_agdisc_copy(NspAgdisc *self)
{
  NspAgdisc *H  =nsp_agdisc_create_void(NVOID,(NspTypeBase *) nsp_type_agdisc);
  if ( H ==  NULLAGDISC) return NULLAGDISC;
  if ( nsp_agdisc_copy_partial(H,self)== NULL) return NULLAGDISC;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAgdisc *nsp_agdisc_full_copy_partial(NspAgdisc *H,NspAgdisc *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_agdisc))) == NULL) return NULLAGDISC;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_full_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  return H;
}

NspAgdisc *nsp_agdisc_full_copy(NspAgdisc *self)
{
  NspAgdisc *H  =nsp_agdisc_create_void(NVOID,(NspTypeBase *) nsp_type_agdisc);
  if ( H ==  NULLAGDISC) return NULLAGDISC;
  if ( nsp_agdisc_full_copy_partial(H,self)== NULL) return NULLAGDISC;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAgdisc
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_agdisc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAgdisc *H;
  CheckStdRhs(0,0);
  /* want to be sure that type agdisc is initialized */
  nsp_type_agdisc = new_type_agdisc(T_BASE);
  if(( H = nsp_agdisc_create_void(NVOID,(NspTypeBase *) nsp_type_agdisc)) == NULLAGDISC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_agdisc_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_agdisc_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *agdisc_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_agdisc_get_Mcoord(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAgdisc *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_agdisc_get_obj_Mcoord(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAgdisc *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_agdisc_set_Mcoord(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcoord;
  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAgdisc *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspAgdisc *) self)->obj->Mcoord);
  ((NspAgdisc *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static AttrTab agdisc_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_agdisc_get_Mcoord, (attr_set_function *)_wrap_agdisc_set_Mcoord,(attr_get_object_function *)_wrap_agdisc_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_nsp_agread(Stack stack, int rhs, int opt, int lhs) /* agread */
{
  int_types T[] = {string,t_end};
  char *filename;
  NspAgraph *ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
    ret = nsp_agread(filename);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

int _wrap_nsp_agdegree(Stack stack, int rhs, int opt, int lhs) /* agdegree */
{
  int_types T[] = {obj_check, s_int, s_int,t_end};
  NspObject *n;
  int in, out, ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &n, &in, &out) == FAIL) return RET_BUG;
    ret = nsp_agdegree(((NspAgnode *) n), in, out);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_nsp_agdelnode(Stack stack, int rhs, int opt, int lhs) /* agdelnode */
{
  int_types T[] = {obj_check,t_end};
  NspObject *arg_n;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agnode, &arg_n) == FAIL) return RET_BUG;
    ret = nsp_agdelnode(((NspAgnode *) arg_n));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_nsp_agdeledge(Stack stack, int rhs, int opt, int lhs) /* agdeledge */
{
  int_types T[] = {obj_check,t_end};
  NspObject *arg_e;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agedge, &arg_e) == FAIL) return RET_BUG;
    ret = nsp_agdeledge(((NspAgedge *) arg_e));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_nsp_agsubedge(Stack stack, int rhs, int opt, int lhs) /* agsubedge */
{
  int_types T[] = {obj_check, obj_check, s_int,t_end};
  NspObject *g, *e;
  int createflag;
  NspAgedge *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agraph, &g, &nsp_type_agedge, &e, &createflag) == FAIL) return RET_BUG;
    ret = nsp_agsubedge(((NspAgraph *) g), ((NspAgedge *) e), createflag);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

int _wrap_nsp_agnxtattr(Stack stack, int rhs, int opt, int lhs) /* agnxtattr */
{
  int_types T[] = {obj_check, s_int, obj_check,t_end};
  NspObject *g, *attr;
  int kind;
  NspAgsym *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_agraph, &g, &kind, &nsp_type_agsym, &attr) == FAIL) return RET_BUG;
    ret = nsp_agnxtattr(((NspAgraph *) g), kind, ((NspAgsym *) attr));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Agraph_func[]={
  {"agread", _wrap_nsp_agread},
  {"agdegree", _wrap_nsp_agdegree},
  {"agdelnode", _wrap_nsp_agdelnode},
  {"agdeledge", _wrap_nsp_agdeledge},
  {"agsubedge", _wrap_nsp_agsubedge},
  {"agnxtattr", _wrap_nsp_agnxtattr},
  { "agraph_create", int_agraph_create},
  { NULL, NULL}
};

/* call ith function in the Agraph interface */

int Agraph_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Agraph_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Agraph_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Agraph_func[i].name;
  *f = Agraph_func[i].fonc;
}

#line 369 "../types-test/codegen/agraph.override"

/* set of nsp function to facilitate methods or functions for graphviz functions */

NspAgraph *nsp_agread(void *chan)
{ 
  FILE* file = fopen(chan, "r"); 
  Agraph_t *dotGraph = NULL;
  if ( file == NULL ) return NULL;
  /* aginit(); */ /* can be called multiple times */
  if ((dotGraph = agread(file,NULL))== NULL) 
    return NULL;
  return nsp_agraph_create(NVOID,dotGraph, NULL);
}

static int nsp_gv_write(NspAgraph * g,void *chan)
{ 
  FILE* file = fopen(chan, "w"); 
  if ( file == NULL ) return FALSE;
  agwrite(g->obj->graph,file);
  fclose(file);
  return TRUE;
}

void nsp_agflatten(NspAgraph * g, int flag){ };
int nsp_agisflattened(NspAgraph * g){return FAIL;}
int nsp_agisdirected(NspAgraph * g){return FAIL;}
int nsp_agisundirected(NspAgraph * g){return FAIL;}
int nsp_agisstrict(NspAgraph * g){return FAIL;}

/* nodes */
/* 
NspAgnode  *nsp_agidnode(NspAgraph * g, unsigned long id,int createflag){ return NULL;}
*/


/* edges */

NspAgedge *nsp_agidedge(NspAgnode * t, NspAgnode * h, unsigned long id, int createflag){ return NULL;}
NspAgedge *nsp_agsubedge(NspAgraph * g, NspAgedge * e, int createflag){ return NULL;}

/* generic */
/* NspAgraph *nsp_agraphof(void *){ return NULL;} */
/* char *nsp_agnameof(void *){ return NULL;} */

int nsp_agrelabel(void *obj, char *name){return FAIL;}	/* scary */
int nsp_agrelabel_node(NspAgnode * n, char *newname){return FAIL;}
int nsp_agdelete(NspAgraph * g, void *obj){return FAIL;}
int nsp_agdelnode(NspAgnode * arg_n){return FAIL;}
int nsp_agdeledge(NspAgedge * arg_e){return FAIL;}
/* int nsp_agisarootobj(void *){ return NULL;} */
/* strings */
/* char *nsp_agstrdup(NspAgraph *, char *){ return NULL;} */
/* char *nsp_agstrdup_html(NspAgraph *, char *){ return NULL;} */
/* int nsp_aghtmlstr(char *){ return NULL;} */
/* char *nsp_agstrbind(NspAgraph * g, char *){ return NULL;}*/
/*   int nsp_agstrfree(NspAgraph *, char *){ return NULL;} */
/*char *nsp_agcanonstr(char *, char *){ return NULL;}*/
/*char *nsp_agcanonStr(char*){ return NULL;}*/

NspAgsym *nsp_agattrsym(void *obj, char *name){ return NULL;}
NspAgsym *nsp_agnxtattr(NspAgraph * g, int kind, NspAgsym * attr){ return NULL;}
void *nsp_agbindrec(void *obj, char *name, unsigned int size,
		       int move_to_front){ return NULL;}
int nsp_agdelrec(void *obj, char *name){return FAIL;}
void nsp_aginit(NspAgraph * g, int kind, char *rec_name,
		int rec_size, int move_to_front){ }
void nsp_agclean(NspAgraph * g, int kind, char *rec_name){}
char *nsp_agget(void *obj, char *name){ return NULL;}
char *nsp_agxget(void *obj, NspAgsym * sym){ return NULL;}
int nsp_agset(void *obj, char *name, char *value){return FAIL;}
int nsp_agxset(void *obj, NspAgsym * sym, char *value){return FAIL;}

/* set cardinality */
int nsp_agdegree(NspAgnode * n, int in, int out){return FAIL;}


/* XXX changer l'interface de add edges 
 * a changer pour que la méthode retourne une erreur en cas de FALSE 
 */


static GVC_t *gvc=NULL;

static int nsp_gv_layout(NspAgraph *G, char *mode)
{
  if ( gvc == NULL ) 
    {
      gvc = gvContext();
    }
  gvLayout(gvc, G->obj->graph, mode);
  return TRUE;
}

static int nsp_gv_render(NspAgraph *G, char *mode, char *filename)
{
  FILE* file;
  if ( gvc == NULL ) 
    {
      Scierror("render: you should call layout first\n");
      return FALSE;
    }
  if (( file = fopen(filename, "w"))== NULL) 
    return FALSE;
  gvRender(gvc, G->obj->graph, mode, file);
  fclose(file);
  return TRUE;
}

/* list attribute names */ 

static NspSMatrix *nsp_gv_objattrs(NspAgraph * g,int tag )
{
  Agsym_t *a = NULL;
  NspSMatrix *S= nsp_smatrix_create(NVOID,0,0, NULL,0);
  while ((a = agnxtattr(g->obj->graph, tag , a))) 
    {
      if ( nsp_row_smatrix_append_string(S, a->name) == FAIL) 
	goto fail;
    }
  return S;
 fail:
  if ( S != NULL) nsp_smatrix_destroy(S);
  return NULL;
}

static NspSMatrix *nsp_gv_graphattrs(NspAgraph * g)
{
  return nsp_gv_objattrs(g,AGRAPH);
}

static NspSMatrix *nsp_gv_nodeattrs(NspAgraph * g)
{
  return nsp_gv_objattrs(g,AGNODE);
}

static NspSMatrix *nsp_gv_edgeattrs(NspAgraph * g)
{
  return nsp_gv_objattrs(g,AGEDGE);
}

static NspAgedge *nsp_gv_agfstout(NspAgraph * g, NspAgnode *n)
{
  Agedge_t *e1 ;
  if ((e1 = agfstout(g->obj->graph,n->obj->node))== NULL)
    {
      Scierror("Error: first out edge was not found\n");
      return NULL;
    }
  return nsp_agedge_create(NVOID,e1,NULL);
}

static NspAgedge *nsp_gv_agnxtout(NspAgraph * g, NspAgedge *e)
{
  Agedge_t *e1 ;
  if ((e1 = agnxtout(g->obj->graph,e->obj->edge))== NULL)
    {
      Scierror("Error: next out edge node was not found\n");
      return NULL;
    }
  return nsp_agedge_create(NVOID,e1,NULL);
}

static NspAgedge *nsp_gv_agfstin(NspAgraph * g, NspAgnode *n)
{
  Agedge_t *e1 ;
  if ((e1 = agfstin(g->obj->graph,n->obj->node))== NULL)
    {
      Scierror("Error: first in edge was not found\n");
      return NULL;
    }
  return nsp_agedge_create(NVOID,e1,NULL);
}

static NspAgedge *nsp_gv_agnxtin(NspAgraph * g, NspAgedge *e)
{
  Agedge_t *e1 ;
  if ((e1 = agnxtin(g->obj->graph,e->obj->edge))== NULL)
    {
      Scierror("Error: next in edge was not found\n");
      return NULL;
    }
  return nsp_agedge_create(NVOID,e1,NULL);
}

static NspAgedge *nsp_gv_agfstedge(NspAgraph * g, NspAgnode *n)
{
  Agedge_t *e1 ;
  if ((e1 = agfstedge(g->obj->graph,n->obj->node))== NULL)
    {
      Scierror("Error: first edge node was not found\n");
      return NULL;
    }
  return nsp_agedge_create(NVOID,e1,NULL);
}

static NspAgedge *nsp_gv_agnxtedge(NspAgraph * g, NspAgedge *e, NspAgnode *n)
{
  Agedge_t *e1 ;
  if ((e1 = agnxtedge(g->obj->graph,e->obj->edge,n->obj->node))== NULL)
    {
      Scierror("Error: next edge was not found\n");
      return NULL;
    }
  return nsp_agedge_create(NVOID,e1,NULL);
}

static int _wrap_nsp_gv_agset_gen(void *obj,Stack stack,int rhs,int opt,int lhs)
{
  /* int a; */
  int i;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  for ( i = 1 ; i <= rhs ; i++) 
    {
      char *value, *agstr;
      const char *attr;
      NspObject *O;
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: %s of method %s should be a named optional argument \n",
		   ArgPosition(i),NspFname(stack));
	  return RET_BUG;
	}
      attr = nsp_object_get_name(NthObj(i));
      O = nsp_get_object(stack,i);
      if ( IsString(O) == FALSE )
	{
	  Scierror("Error: %s of method  %s should be a string\n",
		   ArgPosition(i),NspFname(stack));
	  return RET_BUG;
	}
      value =  ((NspSMatrix *) O)->S[0];

      agstr = nsp_string_copy(attr);
      if ( agstr == NULL) return RET_BUG;
      /* a =*/ agsafeset(obj,agstr,value,"");
      nsp_string_destroy(&agstr);
      /*
	if ( a == FALSE) 
	{
	  Scierror("Error: failed to add a node attribute %s=%s\n",
		   attr,((NspSMatrix *) O)->S[0]);
	  return RET_BUG;
	}
      */
    }
  return 0;
} 

/*-------------------------------------------------
 *  utilities for writing graph methods
 *-------------------------------------------------*/

static NspAgraph *nsp_agparent(NspAgraph * G)
{
  Agraph_t *g = ((Agraph_t *) G->obj->graph);
  Agraph_t *p;
  NspAgraph *P;
  if (( p = agparent(g))== NULL) 
    {
      Scierror("Error: failed to obtain parent graph\n");
      return NULL;
    }
  if ((P = nsp_agraph_create(NVOID,p, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a graph\n");
      return NULL;
    }
  return P;
}

static NspAgraph *nsp_agroot(NspAgraph * G)
{
  Agraph_t *g = ((Agraph_t *) G->obj->graph);
  Agraph_t *r;
  NspAgraph *R;
  if (( r = agroot(g))== NULL) 
    {
      Scierror("Error: failed to obtain parent graph\n");
      return NULL;
    }
  if (( R = nsp_agraph_create(NVOID,r, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a graph\n");
      return NULL;
    }
  return R;
}


static NspAgnode *nsp_agfstnode(NspAgraph * g)
{
  Agnode_t *n ;
  if ((n = agfstnode(g->obj->graph))== NULL)
    {
      Scierror("Error: first node was not found\n");
      return NULL;
    }
  return nsp_agnode_create(NVOID,n,NULL);
}

static NspAgnode *nsp_aglstnode(NspAgraph * g)
{
  Agnode_t *n ;
  if ((n = aglstnode(g->obj->graph))== NULL)
    {
      Scierror("Error: last node was not found\n");
      return NULL;
    }
  return nsp_agnode_create(NVOID,n,NULL);
}

static NspAgnode *nsp_agnxtnode(NspAgraph * g, NspAgnode *n)
{
  Agnode_t *n1 ;
  if ((n1 = agnxtnode(g->obj->graph,n->obj->node))== NULL)
    {
      Scierror("Error: next node was not found\n");
      return NULL;
    }
  return nsp_agnode_create(NVOID,n1,NULL);
}

static NspAgnode *nsp_agprvnode(NspAgraph * g, NspAgnode *n)
{
  Agnode_t *n1 ;
  if ((n1 = agprvnode(g->obj->graph,n->obj->node))== NULL)
    {
      Scierror("Error: previous node was not found\n");
      return NULL;
    }
  return nsp_agnode_create(NVOID,n1,NULL);
}

static NspAgnode *nsp_agfindnode_by_name(NspAgraph * g, char *name)
{
  Agnode_t *n ;
  if ((n = agfindnode(g->obj->graph,name) )== NULL)
    {
      Scierror("Error: node with name=%s was not found\n",name);
      return NULL;
    }
  return nsp_agnode_create(NVOID,n,NULL);
}

/* add nodes to the graph given their names 
 */

static int nsp_agaddnodes(NspAgraph *G, NspSMatrix *S)
{
  int i;
  for ( i = 0 ; i < S->mn ; i++) 
    if ( agnode(G->obj->graph, S->S[i],1) == NULL) 
      {
	return i;
      }
  return -1; /* success */
}

/* add edges to the graph given their names 
 */

static int nsp_agaddedges(NspAgraph *G, NspSMatrix *S)
{
  int i;
  if ( S->n != 2 ) 
    {
      Scierror("add_deges: argument should have two columns\n");
      return FALSE;
    }
  for ( i = 0 ; i < S->m ; i++) 
    {
      Agnode_t *t, *h;
      if ((t = agfindnode(G->obj->graph, S->S[i]))== NULL) 
	{
	  Scierror("Error: in add_deges tail %s of arc %d is not a node name\n",S->S[i],i);
	  return FALSE;
	}
      if ((h = agfindnode(G->obj->graph, S->S[i+S->m]))== NULL) 
	{
	  Scierror("Error: in add_deges head %s of arc %d is not a node name\n",S->S[i+S->m],i);
	  return FALSE;
	}
      if ( agedge(G->obj->graph, t,h,0,1)  == NULL) 
	{
	  Scierror("add_deges: failed to add arc %s->%s\n",t,h);
	  return FALSE;
	}
    }
  return TRUE;
}


static char *nsp_agnameof_g(NspAgraph * G)
{
  char *str = agnameof(G->obj->graph);
  if ( str == NULL) return NULL;
  return nsp_new_string(str,-1);
}

static char *nsp_agnameof_n(NspAgnode * N)
{
  char *str= agnameof(N->obj->node);
  if ( str == NULL) return NULL;
  return nsp_new_string(str,-1);
}

static char *nsp_agnameof_e(NspAgedge * E)
{
  char *str=agnameof(E->obj->edge);
  if ( str == NULL) return NULL;
  return nsp_new_string(str,-1);
}

/*---------------------------------------------------------
 *  utilities for writing graph methods related to subgraph 
 *---------------------------------------------------------*/

static NspAgraph *nsp_agsubg(NspAgraph * G, char *name) 
{
  Agraph_t *g = ((Agraph_t *) G->obj->graph);
  Agraph_t *s;
  NspAgraph *S;
  if (( s = agsubg(g,name,1))== NULL) 
    {
      Scierror("Error: failed to create a subgraph\n");
      return NULL;
    }
  if (( S = nsp_agraph_create(NVOID,s, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a subgraph\n");
      return NULL;
    }
  return S;
}

static NspAgraph *nsp_agfstsubg(NspAgraph * G)
{
  Agraph_t *g = ((Agraph_t *) G->obj->graph);
  Agraph_t *s;
  NspAgraph *S;
  if (( s = agfstsubg(g))== NULL) 
    {
      Scierror("Error: failed to find a subgraph\n");
      return NULL;
    }
  if (( S = nsp_agraph_create(NVOID,s, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a graph\n");
      return NULL;
    }
  return S;
}

static NspAgraph *nsp_agnxtsubg(NspAgraph * G)
{
  Agraph_t *g = ((Agraph_t *) G->obj->graph);
  Agraph_t *s;
  NspAgraph *S;
  if (( s = agnxtsubg(g))== NULL) 
    {
      Scierror("Error: failed to find next subgraph\n");
      return NULL;
    }
  if (( S = nsp_agraph_create(NVOID,s, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a graph\n");
      return NULL;
    }
  return S;
}

static int nsp_agdelsubg(NspAgraph * G,NspAgraph * Gsub)
{
  Agraph_t *g = ((Agraph_t *) G->obj->graph);
  Agraph_t *gsub = ((Agraph_t *) Gsub->obj->graph);
  agdelsubg(g,gsub);
  return OK;
}

/*-------------------------------------------------
 *  utilities for writing node methods
 *-------------------------------------------------*/

static NspAgraph *nsp_agraphof(NspAgnode * N)
{
  NspAgraph *G;
  Agraph_t *g = agraphof(N->obj->node);
  if ( g == NULL) return NULL;
  if ((G = nsp_agraph_create(NVOID,g, NULL)) == NULL) 
    {
      Scierror("Error: failed to create a graph\n");
      return NULL;
    }
  return G;
}

/*-------------------------------------------------
 *  utilities for writing edge methods
 *-------------------------------------------------*/

static NspAgnode *nsp_aghead(NspAgedge* E)
{
  Agnode_t *n ;
  Agedge_t *e = (Agedge_t *) E->obj->edge ;
  if ((n = aghead(e)) == NULL)
    {
      char * str = agnameof(e);
      if ( str != NULL) 
	Scierror("Error: failed to get the head of edge %s\n",agnameof(e));
      else
	Scierror("Error: failed to get the head of an edge \n");
      return NULL;
    }
  return nsp_agnode_create(NVOID,n,NULL);
}

static NspAgnode *nsp_agtail(NspAgedge* E)
{
  Agnode_t *n ;
  Agedge_t *e = (Agedge_t *) E->obj->edge ;
  if ((n = agtail(e)) == NULL)
    {
      char * str = agnameof(e);
      if ( str != NULL) 
	Scierror("Error: failed to get the tail of edge %s\n",agnameof(e));
      else
	Scierror("Error: failed to get the tail of an edge \n");
    }
  return nsp_agnode_create(NVOID,n,NULL);
}



#line 3707 "agraph.c"
