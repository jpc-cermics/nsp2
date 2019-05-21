/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
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





#line 31 "codegen/compound.override"

#line 30 "compound.c"

/* -----------NspCompound ----------- */


#define  NspCompound_Private 
#include <nsp/objects.h>
#include <nsp/compound.h>
#include <nsp/interf.h>

/* 
 * NspCompound inherits from Graphic 
 */

int nsp_type_compound_id=0;
NspTypeCompound *nsp_type_compound=NULL;

/*
 * Type object for NspCompound 
 * all the instance of NspTypeCompound share the same id. 
 * nsp_type_compound: is an instance of NspTypeCompound 
 *    used for objects of NspCompound type (i.e built with new_compound) 
 * other instances are used for derived classes 
 */
NspTypeCompound *new_type_compound(type_mode mode)
{
  NspTypeCompound *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_compound != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_compound;
    }
  if (( type =  malloc(sizeof(NspTypeCompound))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = compound_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = compound_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_compound;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for compound */ 

  top->pr = (print_func *) nsp_compound_print;
  top->dealloc = (dealloc_func *) nsp_compound_destroy;
  top->copy  =  (copy_func *) nsp_compound_copy;
  top->size  = (size_func *) nsp_compound_size;
  top->s_type =  (s_type_func *) nsp_compound_type_as_string;
  top->sh_type = (sh_type_func *) nsp_compound_type_short_string;
  top->info = (info_func *) nsp_compound_info;
  /* top->is_true = (is_true_func  *) nsp_compound_is_true; */
  /* top->loop =(loop_func *) nsp_compound_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_compound_object;
  top->eq  = (eq_func *) nsp_compound_eq;
  top->neq  = (eq_func *) nsp_compound_neq;
  top->save  = (save_func *) nsp_compound_xdr_save;
  top->load  = (load_func *) nsp_compound_xdr_load;
  top->create = (create_func*) int_compound_create;
  top->latex = (print_func *) nsp_compound_latex;
  top->full_copy = (copy_func *) nsp_compound_full_copy;

  /* specific methods for compound */

  type->init = (init_func *) init_compound;

#line 39 "codegen/compound.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_compound;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_compound ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_compound  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_compound  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_compound  ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_compound_link_figure; 
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_compound_unlink_figure; 
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_compound_children ;
#line 115 "compound.c"
  /* 
   * NspCompound interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_compound_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCompound called nsp_type_compound
       */
      type->id =  nsp_type_compound_id = nsp_new_type_id();
      nsp_type_compound = type;
      if ( nsp_register_type(nsp_type_compound) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_compound(mode);
    }
  else 
    {
      type->id = nsp_type_compound_id;
      return type;
    }
}

/*
 * initialize NspCompound instances 
 * locally and by calling initializer on parent class 
 */

static int init_compound(NspCompound *Obj,NspTypeCompound *type)
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
 * new instance of NspCompound 
 */

NspCompound *new_compound() 
{
  NspCompound *loc;
  /* type must exists */
  nsp_type_compound = new_type_compound(T_BASE);
  if ( (loc = malloc(sizeof(NspCompound)))== NULLCOMPOUND) return loc;
  /* initialize object */
  if ( init_compound(loc,nsp_type_compound) == FAIL) return NULLCOMPOUND;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspCompound 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_compound_size(NspCompound *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char compound_type_name[]="Compound";
static char compound_short_type_name[]="compound";

static char *nsp_compound_type_as_string(void)
{
  return(compound_type_name);
}

static char *nsp_compound_type_short_string(NspObject *v)
{
  return(compound_short_type_name);
}

/*
 * A == B 
 */

static int nsp_compound_eq(NspCompound *A, NspObject *B)
{
  NspCompound *loc = (NspCompound *) B;
  if ( check_cast(B,nsp_type_compound_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->bounds)->type->eq(A->obj->bounds,loc->obj->bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  if ( A->obj->hilite_type != loc->obj->hilite_type) return FALSE;
  if ( A->obj->hilite_size != loc->obj->hilite_size) return FALSE;
  if ( A->obj->hilite_color != loc->obj->hilite_color) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_compound_neq(NspCompound *A, NspObject *B)
{
  return ( nsp_compound_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_compound_xdr_save(XDR *xdrs, NspCompound *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_compound)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->hilite_type) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->hilite_size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->hilite_color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCompound  *nsp_compound_xdr_load_partial(XDR *xdrs, NspCompound *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->hilite_type) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->hilite_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->hilite_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspCompound  *nsp_compound_xdr_load(XDR *xdrs)
{
  NspCompound *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCOMPOUND;
  if ((H  = nsp_compound_create_void(name,(NspTypeBase *) nsp_type_compound))== NULLCOMPOUND) return H;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
  if ((H  = nsp_compound_xdr_load_partial(xdrs,H))== NULLCOMPOUND) return H;
  if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
  return H;
}

/*
 * delete 
 */

void nsp_compound_destroy_partial(NspCompound *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->bounds != NULL ) 
      nsp_matrix_destroy(H->obj->bounds);
    if ( H->obj->children != NULL ) 
      nsp_list_destroy(H->obj->children);
    FREE(H->obj);
   }
}

void nsp_compound_destroy(NspCompound *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_compound_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_compound_info(NspCompound *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer NspCompound \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_compound_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_compound_print(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer NspCompound \n");
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
          nsp_compound_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->bounds),indent+2,"bounds", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"hilite_type=%d\n", M->obj->hilite_type);
  Sciprintf1(indent+2,"hilite_size=%d\n", M->obj->hilite_size);
  Sciprintf1(indent+2,"hilite_color=%d\n", M->obj->hilite_color);
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_compound_latex(NspCompound *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  if ( M->obj->bounds != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->bounds),FALSE,"bounds", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),FALSE,"children", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|hilite_type|= \\numprint{%d}\n",M->obj->hilite_type);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|hilite_size|= \\numprint{%d}\n",M->obj->hilite_size);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|hilite_color|= \\numprint{%d}\n",M->obj->hilite_color);
  Sciprintf1(2,"\\\\\n");
  nsp_graphic_latex((NspGraphic * ) M, FALSE,NULL,rec_level);
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspCompound objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspCompound   *nsp_compound_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_compound_id)  == TRUE  ) return ((NspCompound *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_compound));
  return NULL;
}

int IsCompoundObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_compound_id);
}

int IsCompound(NspObject *O)
{
  return nsp_object_type(O,nsp_type_compound_id);
}

NspCompound  *GetCompoundCopy(Stack stack, int i)
{
  if (  GetCompound(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCompound  *GetCompound(Stack stack, int i)
{
  NspCompound *M;
  if (( M = nsp_compound_object(NthObj(i))) == NULLCOMPOUND)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspCompound instance 
 *-----------------------------------------------------*/

static NspCompound *nsp_compound_create_void(const char *name,NspTypeBase *type)
{
 NspCompound *H  = (type == NULL) ? new_compound() : type->new();
 if ( H ==  NULLCOMPOUND)
  {
   Sciprintf("No more memory\n");
   return NULLCOMPOUND;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCOMPOUND;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_compound_create_partial(NspCompound *H)
{
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_compound)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->bounds = NULLMAT;
  H->obj->children = NULLLIST;
  H->obj->hilite_type = -1;
  H->obj->hilite_size = -1;
  H->obj->hilite_color = -1;
  return OK;
}

int nsp_compound_check_values(NspCompound *H)
{
  if ( H->obj->bounds == NULLMAT) 
    {
     double x_def[4]={0};
     if (( H->obj->bounds = nsp_matrix_create("bounds",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->bounds->R,x_def,4*sizeof(double));
  }
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspCompound *nsp_compound_create(const char *name,NspMatrix* bounds,NspList* children,int hilite_type,int hilite_size,int hilite_color,NspTypeBase *type)
{
  NspCompound *H  = nsp_compound_create_void(name,type);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
  H->obj->bounds= bounds;
  H->obj->children= children;
  H->obj->hilite_type=hilite_type;
  H->obj->hilite_size=hilite_size;
  H->obj->hilite_color=hilite_color;
  if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
  return H;
}


NspCompound *nsp_compound_create_default(const char *name)
{
 NspCompound *H  = nsp_compound_create_void(name,NULL);
 if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
  if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCompound *nsp_compound_copy_partial(NspCompound *H,NspCompound *self)
{
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLCOMPOUND;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspCompound *nsp_compound_copy(NspCompound *self)
{
  NspCompound *H  =nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_copy_partial(H,self)== NULL) return NULLCOMPOUND;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspCompound *nsp_compound_full_copy_partial(NspCompound *H,NspCompound *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLCOMPOUND;
  if ((H->obj = calloc(1,sizeof(nsp_compound))) == NULL) return NULLCOMPOUND;
  H->obj->ref_count=1;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_full_copy_and_name("bounds", NSP_OBJECT(self->obj->bounds))) == NULLMAT) return NULL;
    }
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_full_copy_and_name("children", NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  H->obj->hilite_type=self->obj->hilite_type;
  H->obj->hilite_size=self->obj->hilite_size;
  H->obj->hilite_color=self->obj->hilite_color;
  return H;
}

NspCompound *nsp_compound_full_copy(NspCompound *self)
{
  NspCompound *H  =nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_full_copy_partial(H,self)== NULL) return NULLCOMPOUND;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspCompound
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_compound_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCompound *H;
  CheckStdRhs(0,0);
  /* want to be sure that type compound is initialized */
  nsp_type_compound = new_type_compound(T_BASE);
  if(( H = nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound)) == NULLCOMPOUND) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_compound_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_compound_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *compound_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 90 "codegen/compound.override"

/* here we override get_obj  and set_obj 
 * we want get to be followed by a set to check that 
 * inserted value is correct thus we use copy = TRUE.
 */

static NspObject *_wrap_compound_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE; 
  ret = ((NspList*) ((NspCompound *) self)->obj->children);
  return (NspObject *) ret;
}

/* in this function we can check that val is correct before 
 * setting the field with val. return FAIL if val is incorrect.
 */

static int _wrap_compound_set_obj_children(void *self,NspObject *val)
{
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (((NspCompound *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspCompound *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspCompound *) self)->obj->children);
    }
  ((NspCompound *) self)->obj->children =  (NspList *) val;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig,((NspGraphic *) self)->obj->Axe);
  nsp_compound_compute_inside_bounds(self);
  return OK;
}

static int _wrap_compound_set_children(void *self, char *attr, NspObject *O)
{
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspCompound *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspCompound *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspCompound *) self)->obj->children);
    }
  ((NspCompound *) self)->obj->children= children;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig,((NspGraphic *) self)->obj->Axe);
  nsp_compound_compute_inside_bounds(self);
  return OK;
}


#line 660 "compound.c"
static NspObject *_wrap_compound_get_children(void *self,const char *attr)
{
  NspList *ret;
  ret = ((NspCompound *) self)->obj->children;
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_hilite_type(void *self,const char *attr)
{
  int ret;
  ret = ((NspCompound *) self)->obj->hilite_type;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_compound_set_hilite_type(void *self,const char *attr, NspObject *O)
{
  int hilite_type;
  if ( IntScalar(O,&hilite_type) == FAIL) return FAIL;
  ((NspCompound *) self)->obj->hilite_type= hilite_type;
  return OK;
}

static NspObject *_wrap_compound_get_hilite_size(void *self,const char *attr)
{
  int ret;
  ret = ((NspCompound *) self)->obj->hilite_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_compound_set_hilite_size(void *self,const char *attr, NspObject *O)
{
  int hilite_size;
  if ( IntScalar(O,&hilite_size) == FAIL) return FAIL;
  ((NspCompound *) self)->obj->hilite_size= hilite_size;
  return OK;
}

static NspObject *_wrap_compound_get_hilite_color(void *self,const char *attr)
{
  int ret;
  ret = ((NspCompound *) self)->obj->hilite_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_compound_set_hilite_color(void *self,const char *attr, NspObject *O)
{
  int hilite_color;
  if ( IntScalar(O,&hilite_color) == FAIL) return FAIL;
  ((NspCompound *) self)->obj->hilite_color= hilite_color;
  return OK;
}

static AttrTab compound_attrs[] = {
  { "children", (attr_get_function * )_wrap_compound_get_children, (attr_set_function * )_wrap_compound_set_children, (attr_get_object_function * )_wrap_compound_get_obj_children, (attr_set_object_function * )_wrap_compound_set_obj_children },
  { "hilite_type", (attr_get_function * )_wrap_compound_get_hilite_type, (attr_set_function * )_wrap_compound_set_hilite_type, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "hilite_size", (attr_get_function * )_wrap_compound_get_hilite_size, (attr_set_function * )_wrap_compound_set_hilite_size, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "hilite_color", (attr_get_function * )_wrap_compound_get_hilite_color, (attr_set_function * )_wrap_compound_set_hilite_color, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 146 "codegen/compound.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_compound(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 734 "compound.c"


#line 156 "codegen/compound.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_compound(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 747 "compound.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Compound_func[]={
  { "extractelts_compound", _wrap_nsp_extractelts_compound},
  { "setrowscols_compound", _wrap_nsp_setrowscols_compound},
  { "compound_create", int_compound_create},
  { NULL, NULL}
};

/* call ith function in the Compound interface */

int Compound_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Compound_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Compound_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Compound_func[i].name;
  *f = Compound_func[i].fonc;
}
void nsp_initialize_Compound_types(void)
{
  new_type_compound(T_BASE);
}

#line 167 "codegen/compound.override"

/* inserted verbatim at the end */

static const int lock_size=6;

static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  NspCompound *P = (NspCompound *) Obj;
  NspGraphic *G =  (NspGraphic *) Obj;
  NspList *L = P->obj->children;
  Cell *cloc = L->first;
  
  if ( Obj->obj->show == FALSE ) return ;

  /* check if the block is inside drawing rectangle
   */

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  /* draw elements */
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G,rect,data);
	}
      cloc = cloc->next;
    }

  /* draw rectangle if hilited */
  if ( G->obj->hilited == TRUE ) nsp_draw_default_mark_compound(Xgc,Obj);
}

/**
 * nsp_draw_default_mark_compound:
 * @Xgc: 
 * @Obj: 
 * 
 * mark<0 no rectangle 
 * mark>=0 rectangle 
 * abs(mark)>=1 two squares 
 * abs(mark)>=2 four squares 
 * mark_size<0 lock_size for square 
 * 
 * mark_size=0 0 no square 
 * mark_size>0 mark_size*2 for square/mark_size for rectangle *
 * 
 **/

static void nsp_draw_default_mark_compound(BCG *Xgc,NspGraphic *Obj)
{
  NspCompound *P = (NspCompound *) Obj;
  NspGraphic *G =  (NspGraphic *) Obj;
  int color = Xgc->graphic_engine->xset_color(Xgc,P->obj->hilite_color);
  int x=lock_size,y=lock_size;
  double xd,yd, xdd=0.0,ydd=0.0, rect[4], bounds[4];
  
  /* already take into accound the fact that object is 
   * hilited in its bounds 
   */
  if ( nsp_getbounds_compound(G,bounds)== FALSE ) return;
  
  length_scale_i2f(Xgc->scales,&xd,&yd,&x,&y,1);
  
  /* square size */
  if (P->obj->hilite_size>=0) {
    int sz;
    sz=P->obj->hilite_size;
    if (P->obj->hilite_type >0 ) sz=sz*2;
    length_scale_i2f(Xgc->scales,&xdd,&ydd,&sz,&sz,1);
  } else {
    xdd=xd; ydd=yd;
  }
  
  rect[2]=xdd;
  rect[3]=ydd;
  
  /* draw squares */
  switch(Abs(P->obj->hilite_type))
  {
  case 2 :
    /* four squares */
    rect[0]=bounds[2] - xd/2 - xdd/2;
    rect[1]=bounds[3] + ydd/2 - yd/2;
    Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
    rect[0]=bounds[0] - xdd/2 + xd/2;
    rect[1]=bounds[1] + yd/2 + ydd/2;
    Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
    /* use 1 for the next squares */
  case 1 :
    /* two squares */
    rect[0]=bounds[0] - xdd/2 + xd/2;
    rect[1]=bounds[3] + ydd/2 - yd/2;
    Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
    rect[0]=bounds[2] - xd/2 - xdd/2;
    rect[1]=bounds[1] + yd/2 + ydd/2;
    Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
    break;
  default :
    break;
  }
  /* draw surrounding rectangle */
  if (P->obj->hilite_type >=0) 
    {
      int cwidth=Xgc->graphic_engine->xget_thickness(Xgc);
      rect[0]=bounds[0] + xd/2.0; 
      rect[1]=bounds[3] - yd/2.0;
      rect[2]=bounds[2] - bounds[0] - xd;
      rect[3]=bounds[3] - bounds[1] - yd;
      /* rectangle thickness */
      if (P->obj->hilite_size>0) {
	Xgc->graphic_engine->xset_thickness(Xgc,P->obj->hilite_size);
      }
      Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
      /* restore current thickness */
      if (P->obj->hilite_size>0) 
	{
	  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
	}
    }
  /* restore current color */
  Xgc->graphic_engine->xset_color(Xgc,color);
}

/**
 * nsp_grlist_compute_inside_bounds:
 * @L: a #NspList 
 * @bounds: pointer to an array of double of size 4.
 * 
 * compute the bounds of the set of objects countained in the 
 * list. returns %FALSE when the list is empty.
 * 
 * Returns: %TRUE or %FALSE
 **/

int nsp_grlist_compute_inside_bounds(NspList *L,double *bounds)
{
  double l_bounds[4];
  Cell *cloc = L->first ;
  if ( cloc == NULLCELL) 
    {
      bounds[0]=bounds[1]=0;
      bounds[2]=bounds[3]=0;
      return FALSE;
    }
  
  bounds[0]=bounds[1]=LARGEST_REAL;
  bounds[2]=bounds[3]=-LARGEST_REAL;

  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->bounds(G,l_bounds);
	  if ( l_bounds[0] < bounds[0] ) 
	    bounds[0]= l_bounds[0];
	  if (  l_bounds[2] > bounds[2])
	    bounds[2]= l_bounds[2];
	  if ( l_bounds[1] < bounds[1] ) 
	    bounds[1]= l_bounds[1];
	  if (  l_bounds[3] > bounds[3])
	    bounds[3]= l_bounds[3];
	}
      cloc = cloc->next;
    }
  return TRUE;
}

/**
 * nsp_compound_compute_inside_bounds:
 * @Obj: a #NspCompound object 
 * 
 * Computes the bounds of objects contained in the compound and
 * store the result in the #NspCompound object.
 * If the compound is empty %FALSE is returned and  #NspCompound object
 * is not changed. 
 * This function should only be called when children of compound are 
 * modified.
 * 
 * Returns: %TRUE or %FALSE 
 **/

static int nsp_compound_compute_inside_bounds(NspGraphic *Obj)
{
  NspCompound *P = (NspCompound *) Obj;
  double bounds[4];
  if ( nsp_grlist_compute_inside_bounds(P->obj->children,bounds) == TRUE ) 
    {
      memcpy(P->obj->bounds->R,bounds,4*sizeof(double));
      return TRUE;
    }
  return FALSE;
}

/* Note that the bounds should be changed here
 */

static void nsp_translate_compound(NspGraphic *Obj,const double *tr)
{
  NspCompound *P = (NspCompound *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* XXX */
  nsp_graphic_invalidate((NspGraphic *) Obj);
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->translate(G,tr);
	}
      cloc = cloc->next;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_compound(NspGraphic *Obj,double *R)
{
  NspCompound *P = (NspCompound *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->rotate(G,R);
	}
      cloc = cloc->next;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_compound(NspGraphic *Obj,double *alpha)
{
  NspCompound *P = (NspCompound *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->scale(G,alpha);
	}
      cloc = cloc->next;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/**
 * nsp_getbounds_compound:
 * @Obj: 
 * @bounds: 
 * 
 * returns in @bounds the bounds of the graphic object, 
 * taking into account the fact that the object might be hilited.
 * 
 * Returns: %TRUE or %FALSE 
 **/

static int nsp_getbounds_compound(NspGraphic *G,double *bounds)
{
  double xd=0.0,yd=0.0;
  int x=lock_size,y=lock_size;
  NspCompound *P = (NspCompound *) G;
  nsp_figure *F = G->obj->Fig;
  BCG *Xgc = ( F != NULL) ? F->Xgc : NULL;
  /* nsp_compound_compute_inside_bounds should not always
   * be called since P->obj->bounds->R should contain the 
   * result. 
   */
  if ( nsp_compound_compute_inside_bounds(G) == FALSE ) 
    return FALSE;
  /* copy internal bounds in bounds */
  memcpy(bounds,P->obj->bounds->R,4*sizeof(double));  
  /* add  the hilite boundaries to the bounds */
  if ( Xgc!= NULL) length_scale_i2f(Xgc->scales,&xd,&yd,&x,&y,1);
  bounds[0] -=  xd/2.0;/* xmin*/
  bounds[1] -=  yd/2.0;/* ymin*/
  bounds[2] +=  xd/2.0;/* xmax*/
  bounds[3] +=  yd/2.0;/* ymax*/
  return TRUE;
}

static void nsp_compound_link_figure(NspGraphic *G, void *F, void *A)
{
  /* link toplevel, take care to use nsp_graphic field */
  nsp_graphic_link_figure(G, F,A);
  /* link children */
  nsp_list_link_figure(((NspCompound *) G)->obj->children,F,A);
}


static void nsp_compound_unlink_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_unlink_figure(G,  F);
  /* link children */
  nsp_list_unlink_figure(((NspCompound *) G)->obj->children,F);
}

static NspList *nsp_compound_children(NspGraphic *Obj)
{
  return  ((NspCompound *) Obj)->obj->children;
}



#line 1099 "compound.c"
