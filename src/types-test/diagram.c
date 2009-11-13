/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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





#line 90 "codegen/diagram.override"

#include <gdk/gdk.h>
#include "nsp/link.h"
#include "nsp/block.h"
#include "nsp/connector.h"
#include "nsp/figuredata.h"
#include "nsp/figure.h"
#include "nsp/axes.h"
#include "nsp/diagram.h"

#line 39 "diagram.c"

/* ----------- NspDiagram ----------- */


#define  NspDiagram_Private 
#include <nsp/object.h>
#include <nsp/diagram.h>
#include <nsp/interf.h>

/* 
 * NspDiagram inherits from Graphic 
 */

int nsp_type_diagram_id=0;
NspTypeDiagram *nsp_type_diagram=NULL;

/*
 * Type object for NspDiagram 
 * all the instance of NspTypeDiagram share the same id. 
 * nsp_type_diagram: is an instance of NspTypeDiagram 
 *    used for objects of NspDiagram type (i.e built with new_diagram) 
 * other instances are used for derived classes 
 */
NspTypeDiagram *new_type_diagram(type_mode mode)
{
  NspTypeDiagram *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_diagram != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_diagram;
    }
  if (( type =  malloc(sizeof(NspTypeDiagram))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = diagram_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = diagram_get_methods;
  type->new = (new_func *) new_diagram;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for diagram */ 

  top->pr = (print_func *) nsp_diagram_print;
  top->dealloc = (dealloc_func *) nsp_diagram_destroy;
  top->copy  =  (copy_func *) nsp_diagram_copy;
  top->size  = (size_func *) nsp_diagram_size;
  top->s_type =  (s_type_func *) nsp_diagram_type_as_string;
  top->sh_type = (sh_type_func *) nsp_diagram_type_short_string;
  top->info = (info_func *) nsp_diagram_info;
  /* top->is_true = (is_true_func  *) nsp_diagram_is_true; */
  /* top->loop =(loop_func *) nsp_diagram_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_diagram_object;
  top->eq  = (eq_func *) nsp_diagram_eq;
  top->neq  = (eq_func *) nsp_diagram_neq;
  top->save  = (save_func *) nsp_diagram_xdr_save;
  top->load  = (load_func *) nsp_diagram_xdr_load;
  top->create = (create_func*) int_diagram_create;
  top->latex = (print_func *) nsp_diagram_latex;
  top->full_copy = (copy_func *) nsp_diagram_full_copy;

  /* specific methods for diagram */

  type->init = (init_func *) init_diagram;

#line 107 "codegen/diagram.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_diagram;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_diagram ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_diagram  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_diagram  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_diagram  ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_diagram_link_figure; 
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_diagram_unlink_figure; 
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_diagram_children ;

#line 122 "diagram.c"
  /* 
   * NspDiagram interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_diagram_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeDiagram called nsp_type_diagram
       */
      type->id =  nsp_type_diagram_id = nsp_new_type_id();
      nsp_type_diagram = type;
      if ( nsp_register_type(nsp_type_diagram) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_diagram(mode);
    }
  else 
    {
      type->id = nsp_type_diagram_id;
      return type;
    }
}

/*
 * initialize NspDiagram instances 
 * locally and by calling initializer on parent class 
 */

static int init_diagram(NspDiagram *Obj,NspTypeDiagram *type)
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
 * new instance of NspDiagram 
 */

NspDiagram *new_diagram() 
{
  NspDiagram *loc;
  /* type must exists */
  nsp_type_diagram = new_type_diagram(T_BASE);
  if ( (loc = malloc(sizeof(NspDiagram)))== NULLDIAGRAM) return loc;
  /* initialize object */
  if ( init_diagram(loc,nsp_type_diagram) == FAIL) return NULLDIAGRAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspDiagram 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_diagram_size(NspDiagram *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char diagram_type_name[]="Diagram";
static char diagram_short_type_name[]="diagram";

static char *nsp_diagram_type_as_string(void)
{
  return(diagram_type_name);
}

static char *nsp_diagram_type_short_string(NspObject *v)
{
  return(diagram_short_type_name);
}

/*
 * A == B 
 */

static int nsp_diagram_eq(NspDiagram *A, NspObject *B)
{
  NspDiagram *loc = (NspDiagram *) B;
  if ( check_cast(B,nsp_type_diagram_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->bounds)->type->eq(A->obj->bounds,loc->obj->bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_diagram_neq(NspDiagram *A, NspObject *B)
{
  return ( nsp_diagram_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_diagram_xdr_save(XDR *xdrs, NspDiagram *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_diagram)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspDiagram  *nsp_diagram_xdr_load_partial(XDR *xdrs, NspDiagram *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspDiagram  *nsp_diagram_xdr_load(XDR *xdrs)
{
  NspDiagram *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLDIAGRAM;
  if ((H  = nsp_diagram_create_void(name,(NspTypeBase *) nsp_type_diagram))== NULLDIAGRAM) return H;
  if ( nsp_diagram_create_partial(H) == FAIL) return NULLDIAGRAM;
  if ((H  = nsp_diagram_xdr_load_partial(xdrs,H))== NULLDIAGRAM) return H;
  if ( nsp_diagram_check_values(H) == FAIL) return NULLDIAGRAM;
#line 119 "codegen/diagram.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
  nspdiagram_recompute_pointers(H->obj);

#line 279 "diagram.c"
  return H;
}

/*
 * delete 
 */

void nsp_diagram_destroy_partial(NspDiagram *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
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

void nsp_diagram_destroy(NspDiagram *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_diagram_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_diagram_info(NspDiagram *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLDIAGRAM) 
    {
      Sciprintf("Null Pointer NspDiagram \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_diagram_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_diagram_print(NspDiagram *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLDIAGRAM) 
    {
      Sciprintf("Null Pointer NspDiagram \n");
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
          nsp_diagram_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_diagram_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_diagram_latex(NspDiagram *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_diagram_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspDiagram objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspDiagram   *nsp_diagram_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_diagram_id) == TRUE ) return ((NspDiagram *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_diagram));
  return NULL;
}

int IsDiagramObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_diagram_id);
}

int IsDiagram(NspObject *O)
{
  return nsp_object_type(O,nsp_type_diagram_id);
}

NspDiagram  *GetDiagramCopy(Stack stack, int i)
{
  if (  GetDiagram(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspDiagram  *GetDiagram(Stack stack, int i)
{
  NspDiagram *M;
  if (( M = nsp_diagram_object(NthObj(i))) == NULLDIAGRAM)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspDiagram instance 
 *-----------------------------------------------------*/

static NspDiagram *nsp_diagram_create_void(char *name,NspTypeBase *type)
{
 NspDiagram *H  = (type == NULL) ? new_diagram() : type->new();
 if ( H ==  NULLDIAGRAM)
  {
   Sciprintf("No more memory\n");
   return NULLDIAGRAM;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLDIAGRAM;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_diagram_create_partial(NspDiagram *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_diagram)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->bounds = NULLMAT;
  H->obj->children = NULLLIST;
  return OK;
}

int nsp_diagram_check_values(NspDiagram *H)
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
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspDiagram *nsp_diagram_create(char *name,NspMatrix* bounds,NspList* children,NspTypeBase *type)
{
  NspDiagram *H  = nsp_diagram_create_void(name,type);
  if ( H ==  NULLDIAGRAM) return NULLDIAGRAM;
  if ( nsp_diagram_create_partial(H) == FAIL) return NULLDIAGRAM;
  H->obj->bounds= bounds;
  H->obj->children= children;
  if ( nsp_diagram_check_values(H) == FAIL) return NULLDIAGRAM;
#line 119 "codegen/diagram.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
  nspdiagram_recompute_pointers(H->obj);

#line 484 "diagram.c"
  return H;
}


NspDiagram *nsp_diagram_create_default(char *name)
{
 NspDiagram *H  = nsp_diagram_create_void(name,NULL);
 if ( H ==  NULLDIAGRAM) return NULLDIAGRAM;
  if ( nsp_diagram_create_partial(H) == FAIL) return NULLDIAGRAM;
 if ( nsp_diagram_check_values(H) == FAIL) return NULLDIAGRAM;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspDiagram *nsp_diagram_copy_partial(NspDiagram *H,NspDiagram *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspDiagram *nsp_diagram_copy(NspDiagram *self)
{
  NspDiagram *H  =nsp_diagram_create_void(NVOID,(NspTypeBase *) nsp_type_diagram);
  if ( H ==  NULLDIAGRAM) return NULLDIAGRAM;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLDIAGRAM;
  if ( nsp_diagram_copy_partial(H,self)== NULL) return NULLDIAGRAM;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspDiagram *nsp_diagram_full_copy_partial(NspDiagram *H,NspDiagram *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_diagram))) == NULL) return NULLDIAGRAM;
  H->obj->ref_count=1;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_full_copy_and_name("bounds",NSP_OBJECT(self->obj->bounds))) == NULLMAT) return NULL;
    }
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_full_copy_and_name("children",NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  return H;
}

NspDiagram *nsp_diagram_full_copy(NspDiagram *self)
{
  NspDiagram *H  =nsp_diagram_create_void(NVOID,(NspTypeBase *) nsp_type_diagram);
  if ( H ==  NULLDIAGRAM) return NULLDIAGRAM;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLDIAGRAM;
  if ( nsp_diagram_full_copy_partial(H,self)== NULL) return NULLDIAGRAM;
#line 119 "codegen/diagram.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
  nspdiagram_recompute_pointers(H->obj);

#line 550 "diagram.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspDiagram
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_diagram_create(Stack stack, int rhs, int opt, int lhs)
{
  NspDiagram *H;
  CheckStdRhs(0,0);
  /* want to be sure that type diagram is initialized */
  nsp_type_diagram = new_type_diagram(T_BASE);
  if(( H = nsp_diagram_create_void(NVOID,(NspTypeBase *) nsp_type_diagram)) == NULLDIAGRAM) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_diagram_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_diagram_check_values(H) == FAIL) return RET_BUG;
#line 119 "codegen/diagram.override"
  /* verbatim in create/load/full_copy interface use RET_BUG for returned value */
  nspdiagram_recompute_pointers(H->obj);

#line 574 "diagram.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 496 "codegen/diagram.override"

static int _wrap_diagram_new_link(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  obj =  nsp_diagram_create_new_link(((NspDiagram *) self));
  /* since obj is kept on the frame we must return a copy 
   * link can be null because the new link may possibly have been aborted.
   */
  if (obj != NULL && lhs == 1) 
    {
      if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
      MoveObj(stack,1,obj);
      return 1;
    }
  return 0;
}

#line 602 "diagram.c"


#line 404 "codegen/diagram.override"

static int _wrap_diagram_new_block(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  int mode=0;
  int ix,iy;
  double pt1[2]={0,100};
  NspObject *obj;
  CheckRhs(0,2);
  CheckLhs(-1,1);
  if ( rhs == 1 ) 
    {
      if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      CheckLength(NspFname(stack),1,pt,2);
      /* pt is in pixel here */
      ix=pt->R[0];iy=pt->R[1];
      nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
    }
  if ( rhs == 2 )
    {
      if (GetScalarInt(stack,2,&mode) == FAIL) return RET_BUG;
    }
  if ((obj = nsp_diagram_create_new_block((NspDiagram *) self,pt1))== NULL)
    return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}


#line 637 "diagram.c"


#line 437 "codegen/diagram.override"
/* a super block 
 */

static int _wrap_diagram_new_gridblock(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj = nsp_diagram_create_new_gridblock(((NspDiagram *) self),TRUE))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

#line 656 "diagram.c"


#line 454 "codegen/diagram.override"

static int _wrap_diagram_new_gridblock_from_selection(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj = nsp_diagram_create_new_gridblock(((NspDiagram *) self),FALSE))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}


#line 674 "diagram.c"


#line 470 "codegen/diagram.override"

static int _wrap_diagram_new_connector(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj = nsp_diagram_create_new_connector(((NspDiagram *) self)))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

#line 691 "diagram.c"


#line 485 "codegen/diagram.override"

static int _wrap_diagram_new_rect(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  /* nsp_diagram_create_new_rect(((NspDiagram *) self)); */
  return 0;
}

#line 704 "diagram.c"


#line 385 "codegen/diagram.override"

static int _wrap_diagram_hilite_near_pt(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  nsp_diagram_hilite_near_pt(((NspDiagram *) self),pt1);
  return 0;
}

#line 725 "diagram.c"


#line 229 "codegen/diagram.override"

/* select_and_move select current unhilite others and move current */

static int _wrap_diagram_select_and_move(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  nsp_diagram_select_and_move(((NspDiagram *) self),pt1, 0);
  return 0;
}

#line 748 "diagram.c"


#line 250 "codegen/diagram.override"

/* select_and_move_list: select current and move all hilited in group */

static int _wrap_diagram_select_and_move_list(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  int ix,iy;
  double pt1[2];
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  nsp_diagram_select_and_move(((NspDiagram *) self),pt1, 1);
  return 0;
}


#line 772 "diagram.c"


#line 272 "codegen/diagram.override"
/* select_and_hilite */

static int _wrap_diagram_select_and_hilite(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  int rep;
  NspObject *bool;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  rep= nsp_diagram_select_and_hilite(((NspDiagram *) self),pt1);
  if ((bool = nsp_create_boolean_object(NVOID,(rep == OK) ? TRUE : FALSE))
      == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,bool);
  return 1;
}


#line 800 "diagram.c"


#line 298 "codegen/diagram.override"

static int _wrap_diagram_select_and_toggle_hilite(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep,ix,iy;
  double pt1[2];
  NspObject *bool;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  rep= nsp_diagram_select_and_toggle_hilite(((NspDiagram *) self),pt1);
  if ((bool = nsp_create_boolean_object(NVOID,(rep == OK) ? TRUE : FALSE))
      == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,bool);
  return 1;
}


#line 826 "diagram.c"


#line 322 "codegen/diagram.override"
/* split link */

static int _wrap_diagram_select_and_split(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  nsp_diagram_select_and_split(((NspDiagram *) self),pt1);
  return 0;
}


#line 849 "diagram.c"


#line 343 "codegen/diagram.override"
/* split link */

static int _wrap_diagram_select_link_and_add_control(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  nsp_diagram_select_link_and_add_control(((NspDiagram *) self),pt1);
  return 0;
}



#line 873 "diagram.c"


#line 365 "codegen/diagram.override"
/* shorten link */
static int _wrap_diagram_select_link_and_remove_control(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  nsp_diagram_select_link_and_remove_control(((NspDiagram *) self),pt1);
  return 0;
}


#line 895 "diagram.c"


#line 517 "codegen/diagram.override"

static int _wrap_diagram_delete_hilited(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  nsp_diagram_delete_hilited(((NspDiagram *) self));
  return 0;
}


#line 909 "diagram.c"


#line 595 "codegen/diagram.override"
/* insert an object in a frame. 
 *
 */

static int _wrap_diagram_insert(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspBlock  *B; 
  NspLink *L;
  NspConnector *C;
  NspObject *obj=NULL;
  int flag = TRUE;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ( IsBlockObj(stack,1) )
    { 
      if ((B=GetBlockCopy(stack,1)) == NULLBLOCK) return RET_BUG;
      /* B->obj->frame = ((NspDiagram *) self)->obj; */
      obj = NSP_OBJECT(B);
    }
  else if ( IsLinkObj(stack,1))
    { 
      if ((L=GetLinkCopy(stack,1)) == NULLLINK) return RET_BUG;
      /* L->obj->frame = ((NspDiagram *) self)->obj; */
      obj = NSP_OBJECT(L);
      flag = FALSE;
    }
  else if ( IsConnectorObj(stack,1))
    { 
      if ((C=GetConnectorCopy(stack,1)) == NULLCONNECTOR) return RET_BUG;
      /* C->obj->frame = ((NspDiagram *) self)->obj; */
      obj = NSP_OBJECT(C);
    }

  if (nsp_object_set_name(obj,"lel") == FAIL) return RET_BUG;
  if ( flag ) 
    {
      if (nsp_list_end_insert(((NspDiagram *) self)->obj->children,obj) == FAIL ) return RET_BUG;
    }
  else 
    {
      if (nsp_list_insert(((NspDiagram *) self)->obj->children,obj,0) == FAIL ) return RET_BUG;
    }
  return 0;
}


#line 959 "diagram.c"


#line 643 "codegen/diagram.override"
/* used for the paste of a multiselection 
 * insert a list of objects which are in a diagram. 
 */

static int _wrap_diagram_insert_diagram(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspTypeGRint *bf;
  NspObject *Obj;
  NspMatrix *pt;
  int rep,click;
  double pt1[2]= {5,-10};
  NspDiagram *D = self;
  Cell *C;
  NspDiagram *Di;
  NspGraphic *GDi;
  CheckRhs(2,2);
  CheckLhs(-1,0);
  if ((Di=GetDiagram(stack,1)) == NULLDIAGRAM ) return RET_BUG;
  if ((pt = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),2,pt,2);
  /* now we loop on objects and insert them 
   * this could be turned into a new function 
   */
  if ((Di = nsp_diagram_full_copy(Di))== NULLDIAGRAM)   return RET_BUG;
  /* take care that full copy preserve the figure */
  GDi = (NspGraphic *) Di;
  GDi->type->unlink_figure(GDi,GDi->obj->Fig);
  /* 
   * pt is the mouse position 
   * we have to translate or move the upper-left rectangle enclosing the 
   * selection to pt. Thus we need to now this enclosing rectangle.
   * XXX: we start here by using the first object position as a position 
   */
  Obj = nsp_list_get_element(Di->obj->children,1);
  if ( Obj != NULLOBJ ) 
    {
      double pt2[2];
      bf = GR_INT(Obj->basetype->interface);
      bf->get_pos(Obj,pt2);
      pt1[0] = pt->R[0]-pt2[0];
      pt1[1] = pt->R[1]-pt2[1];
    }
  nsp_diagram_list_obj_action(Di,Di->obj->children,pt1,L_TRANSLATE);
  nsp_diagram_list_obj_action(Di,Di->obj->children,pt1, L_LOCK_UPDATE);
  /* unselect the objects */
  nsp_diagram_unhilite_objs(D,FALSE);
  /* insert each object 
   * 
   */
  C = Di->obj->children->first; 
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  NspGraphic *G = (NspGraphic *) C->O;
	  /* set the frame field of each object */
	  /* NspBlock *B = (NspBlock *) C->O;  */
	  /* B->obj->frame = D->obj; */
	  /* hilite inserted */
	  bf = GR_INT(C->O->basetype->interface);
	  /* add the object */
	  if ( nsp_list_end_insert(D->obj->children,C->O) == FAIL )
	    return RET_BUG; 
	  /* be sure that the object Figure is OK */
	  bf->set_hilited(C->O,TRUE); 
	  G->type->link_figure(G,
			       ((NspGraphic *) D)->obj->Fig,
			       ((NspGraphic *) D)->obj->Axe);
	  C->O= NULLOBJ;
	}
      C = C->next ;
    }
  /* we can now destroy Di */
  nsp_diagram_destroy(Di);
  /* and we can enter a move_selection */
  rep = nsp_diagram_select_and_move_list(D,NULLOBJ,pt->R,&click);
  return 0;
}


#line 1043 "diagram.c"


#line 529 "codegen/diagram.override"
/* get the first hilited object */

static int _wrap_diagram_get_selection(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj,*bool;
  CheckRhs(0,0);
  CheckLhs(2,2);
  if ((obj = nsp_diagram_get_hilited(((NspDiagram *) self)))== NULL) 
    {
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,1,bool);
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,2,bool);
      return 2;
    }
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  if ((bool = nsp_create_boolean_object(NVOID,TRUE)));
  MoveObj(stack,1,bool);
  MoveObj(stack,2,obj);
  return 2;
}



#line 1072 "diagram.c"


#line 556 "codegen/diagram.override"
/* get a full copy of the first hilited object */
static int _wrap_diagram_get_selection_copy(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj,*bool;
  CheckRhs(0,0);
  CheckLhs(2,2);
  if ((obj = nsp_diagram_get_hilited(((NspDiagram *) self)))== NULL) 
    {
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,1,bool);
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,2,bool);
      return 2;
    }
  if ((obj = nsp_object_full_copy(obj))== NULLOBJ)  return RET_BUG;
  if ((bool = nsp_create_boolean_object(NVOID,TRUE)));
  MoveObj(stack,1,bool);
  MoveObj(stack,2,obj);
  return 2;
}


#line 1098 "diagram.c"


#line 580 "codegen/diagram.override"
/* get the hilited objects as a list with or without full copy */

static int _wrap_diagram_get_selection_as_diagram(void *self,Stack stack, int rhs, int opt, int lhs) 
{
  NspDiagram *obj;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((obj =nsp_diagram_hilited_full_copy((NspDiagram *) self)) == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(obj));
  return 1;
}


#line 1115 "diagram.c"


#line 739 "codegen/diagram.override"
/* check if we are over an object */

static int _wrap_diagram_check_pointer(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ix,iy;
  double pt1[2];
  int k, hilited = FALSE;
  NspObject *Obj;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(0,2);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  /* pt is in pixel here */
  ix=pt->R[0];iy=pt->R[1];
  nsp_axes_i2f(((NspGraphic *) self)->obj->Axe,ix,iy,pt1);
  k = nsp_diagram_select_obj(((NspDiagram *) self),pt1,&Obj,NULL);
  if ( k !=0 )
    {
      NspTypeGRint *bf = GR_INT(Obj->basetype->interface);
      hilited= bf->get_hilited(Obj);
    }
  if ( nsp_move_boolean(stack,1, ( k== 0) ? FALSE : TRUE) == FAIL)  return RET_BUG;
  if ( lhs == 2 ) 
    {
      if ( nsp_move_boolean(stack,2, hilited) == FAIL)  return RET_BUG;
    }
  return Max(lhs,1);
}

#line 1149 "diagram.c"


#line 725 "codegen/diagram.override"

static int _wrap_diagram_get_nobjs(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int length;
  CheckRhs(0,0);
  CheckLhs(0,1);
  length =nsp_list_length( ((NspDiagram *) self)->obj->children);
  if ( nsp_move_double(stack,1,(double) length) == FAIL)  return RET_BUG;
  return 1;
}


#line 1165 "diagram.c"


static NspMethods diagram_methods[] = {
  {"new_link",(nsp_method *) _wrap_diagram_new_link},
  {"new_block",(nsp_method *) _wrap_diagram_new_block},
  {"new_gridblock",(nsp_method *) _wrap_diagram_new_gridblock},
  {"new_gridblock_from_selection",(nsp_method *) _wrap_diagram_new_gridblock_from_selection},
  {"new_connector",(nsp_method *) _wrap_diagram_new_connector},
  {"new_rect",(nsp_method *) _wrap_diagram_new_rect},
  {"hilite_near_pt",(nsp_method *) _wrap_diagram_hilite_near_pt},
  {"select_and_move",(nsp_method *) _wrap_diagram_select_and_move},
  {"select_and_move_list",(nsp_method *) _wrap_diagram_select_and_move_list},
  {"select_and_hilite",(nsp_method *) _wrap_diagram_select_and_hilite},
  {"select_and_toggle_hilite",(nsp_method *) _wrap_diagram_select_and_toggle_hilite},
  {"select_and_split",(nsp_method *) _wrap_diagram_select_and_split},
  {"select_link_and_add_control",(nsp_method *) _wrap_diagram_select_link_and_add_control},
  {"select_link_and_remove_control",(nsp_method *) _wrap_diagram_select_link_and_remove_control},
  {"delete_hilited",(nsp_method *) _wrap_diagram_delete_hilited},
  {"insert",(nsp_method *) _wrap_diagram_insert},
  {"insert_diagram",(nsp_method *) _wrap_diagram_insert_diagram},
  {"get_selection",(nsp_method *) _wrap_diagram_get_selection},
  {"get_selection_copy",(nsp_method *) _wrap_diagram_get_selection_copy},
  {"get_selection_as_diagram",(nsp_method *) _wrap_diagram_get_selection_as_diagram},
  {"check_pointer",(nsp_method *) _wrap_diagram_check_pointer},
  {"get_nobjs",(nsp_method *) _wrap_diagram_get_nobjs},
  { NULL, NULL}
};

static NspMethods *diagram_get_methods(void) { return diagram_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 149 "codegen/diagram.override"

/* here we override get_obj  and set_obj 
 * we want get to be followed by a set to check that 
 * inserted value is correct thus we use copy = TRUE.
 */

static NspObject *_wrap_diagram_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE; 
  ret = ((NspList*) ((NspDiagram *) self)->obj->children);
  return (NspObject *) ret;
}

/* in this function we can check that val is correct before 
 * setting the field with val. return FAIL if val is incorrect.
 */

static int _wrap_diagram_set_obj_children(void *self,NspObject *val)
{
  NspDiagram *D = self;
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (D->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(D->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(D->obj->children);
    }
  D->obj->children =  (NspList *) val;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig,
			 ((NspGraphic *) self)->obj->Axe);
  nsp_diagram_compute_inside_bounds(self);
  return OK;
}

static int _wrap_diagram_set_children(void *self, char *attr, NspObject *O)
{
  NspDiagram *D = self;
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (D->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(D->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(D->obj->children);
    }
  D->obj->children= children;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig,
			 ((NspGraphic *) self)->obj->Axe);
  nsp_diagram_compute_inside_bounds(self);
  return OK;
}


#line 1258 "diagram.c"
static NspObject *_wrap_diagram_get_children(void *self,const char *attr)
{
  NspList *ret;

  ret = ((NspDiagram *) self)->obj->children;
  return (NspObject *) ret;
}

static AttrTab diagram_attrs[] = {
  { "children", (attr_get_function *)_wrap_diagram_get_children, (attr_set_function *)_wrap_diagram_set_children,(attr_get_object_function *)_wrap_diagram_get_obj_children, (attr_set_object_function *)_wrap_diagram_set_obj_children },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 209 "codegen/diagram.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_diagram(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1285 "diagram.c"


#line 219 "codegen/diagram.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_diagram(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 1297 "diagram.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Diagram_func[]={
  {"extractelts_diagram", _wrap_nsp_extractelts_diagram},
  {"setrowscols_diagram", _wrap_nsp_setrowscols_diagram},
  { "diagram_create", int_diagram_create},
  { NULL, NULL}
};

/* call ith function in the Diagram interface */

int Diagram_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Diagram_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Diagram_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Diagram_func[i].name;
  *f = Diagram_func[i].fonc;
}

#line 771 "codegen/diagram.override"

/* inserted verbatim at the end */

static void nsp_draw_diagram(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data)
{
  NspDiagram *P = (NspDiagram *) Obj;
  NspList *L = P->obj->children;
  Cell *cloc = L->first;
  
  if ( Obj->obj->show == FALSE ) return ;
  /*
   * we do not check here the bound of the diagram since 
   * they are not relevant here (see getbounds). 
   * proper bounds should be given at creation or we just 
   * use here the fact that each block will do the proper 
   * job 
   if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
   {
   return ;
   }
   */

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
}

/* compute the bounds of the set of objects countained in the 
 * diagram. This function is to be called when contained 
 * objects are changed.
 */

static void nsp_diagram_compute_inside_bounds(NspGraphic *Obj)
{
  double l_bounds[4],bounds[4];
  Cell *cloc;
  NspList *L;
  NspDiagram *P = (NspDiagram *) Obj;
  L = P->obj->children;
  cloc = L->first ;
  if ( cloc == NULLCELL) 
    {
      bounds[0]=bounds[1]=0;
      bounds[2]=bounds[3]=0;
      return;
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
  memcpy(P->obj->bounds->R,bounds,4*sizeof(double));
}

/* Note that the bounds should be changed here
 */

static void nsp_translate_diagram(NspGraphic *Obj,const double *tr)
{
  NspDiagram *P = (NspDiagram *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
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

static void nsp_rotate_diagram(NspGraphic *Obj,double *R)
{
  NspDiagram *P = (NspDiagram *) Obj;
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

static void nsp_scale_diagram(NspGraphic *Obj,double *alpha)
{
  NspDiagram *P = (NspDiagram *) Obj;
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

/* compute in bounds the enclosing rectangle of diagram 
 *
 */

static int nsp_getbounds_diagram(NspGraphic *Obj,double *bounds)
{
  NspDiagram *P = (NspDiagram *) Obj;
  if ( 0 )
    {
      nsp_diagram_compute_inside_bounds(Obj);
      memcpy(bounds,P->obj->bounds->R,4*sizeof(double));
      return TRUE;
    }
  else
    {
      bounds[0]=0;/* xmin */
      bounds[1]=0;/* ymin */
      bounds[2]=200;/* xmax */
      bounds[3]=200;/* ymax */
    }
  return TRUE;
}

static void nsp_diagram_link_figure(NspGraphic *G, void *F, void *A)
{
  /* link toplevel */
  nsp_graphic_link_figure(G, F, A);
  /* link children */
  nsp_list_link_figure(((NspDiagram *) G)->obj->children,F,A);
}


static void nsp_diagram_unlink_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_unlink_figure(G, F);
  /* link children */
  nsp_list_unlink_figure(((NspDiagram *) G)->obj->children,F);
}

static NspList *nsp_diagram_children(NspGraphic *Obj)
{
  return  ((NspDiagram *) Obj)->obj->children;
}


/*********************************************************************
 * Diagram Object in Scilab : a graphic Diagram
 *********************************************************************/

/**
 * nsp_diagram_select_obj:
 * @R: a graphic frame  
 * @pt: point coordinates.
 * @Objs: an array of objects.
 * @exclude: an object to be excluded from search.
 * 
 * selects the first object of @R which contains the point @pt and returns 
 * the result in @Objs. @exclude can be used to exclude an object from the search.
 * 
 * Return value: 0 or the position of the object found in the list.
 *
 **/

int nsp_diagram_select_obj(NspDiagram *R,const double pt[2], NspObject **Objs, NspObject *exclude) 
{
  int count = 1;
  Cell *C = R->obj->children->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && C->O != exclude )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->contains_pt(C->O,pt) ) 
	    {
	      *Objs = C->O;
	      return count;
	    }
	}
      C = C->next ;
      count++;
    }
  return 0;
}


/**
 * nsp_diagram_select_lock:
 * @F: a graphic frame  
 * @pt: point coordinates.
 * @O: an object 
 * @cp: lock point id 
 * @lock_c: is lock connectable.
 *
 * If @pt is close enough to an object lock point, then the object is 
 * returned in @O, the lock point id in @cp and the connectable status in @lock_c.
 *  
 * return value: a non null integer in case of success
 **/


int nsp_diagram_select_lock(NspDiagram *F,double pt[2], NspObject **O, int *cp, int *lock_c) 
{
  int count = 1;
  Cell *C = F->obj->children->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->lock_near_pt(C->O,pt,cp) == TRUE ) 
	    {
	      *O = C->O;
	      *lock_c = bf->is_lock_connectable(C->O,*cp);
	      return count;
	    }
	}
      C = C->next ;
      count++;
    }
  return 0;
}


/**
 * nsp_diagram_get_adress:
 * @F: a #NspDiagram 
 * @old: a void pointer 
 * 
 * This function is used to get the new adress in @F of an 
 * object which was previouly stored at adress @old. The old 
 * adresses are stored in objects in the field @sid. 
 * This is used when performing full copy of objects to restore 
 * new crossed references in the copy. If the new adress is not 
 * found then %NULL is returned. This can happen if a full copy 
 * was performed but on a subset of the objects (for example 
 * just the hilited objects), then in the full copy reference 
 * unfound objects are to be set to %NULL. Thus %NULL can 
 * be a correct answer. 
 * 
 * Returns: a pointer as a void pointer 
 **/

void *nsp_diagram_get_adress(NspDiagram *F,void *old )
{
  return nspdiagram_get_adress(F->obj->children,old);
}

/**
 * nspdiagram_get_adress:
 * @L: a #NspList
 * @old: a void pointer 
 * 
 * This function is used to get the new adress in @L of an 
 * object which was previouly stored at adress @old. The old 
 * adresses are stored in objects in the field @sid. 
 * This is used when performing full copy of objects to restore 
 * new crossed references in the copy. The list @L must contains 
 * objects implementing grint interface
 * 
 * 
 * Returns: a pointer as a void pointer 
 **/

static void *nspdiagram_get_adress(NspList *L,void *old )
{
  int count = 1;
  Cell *C = L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  NspBlock *B = (NspBlock *) C->O; 
	  if ( B->obj->object_sid  == old) 
	    return C->O;
	}
      C = C->next ;
      count++;
    }
  return NULL;
}


/**
 * nsp_diagram_locks_set_show:
 * @F:  a #NspDiagram 
 * @O: a #NspObject 
 * @val: a flag as an integer
 * 
 * sets the show attribute to value @val for all the objects which 
 * are connected to object @O by lock connections.
 **/
#if 0
static void nsp_diagram_locks_set_show(NspDiagram *F,NspObject *O,int val)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK && p.object_id != NULL) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  bf1->set_show(O1,val);
		}
	    }
	}
    }
}
#endif 

static void nsp_diagram_rectangle_select_objs(NspDiagram *R,const double pt[2],double *rect)
{
  int zrect1[4], xof[4]={-5,-5,10,10},i;
  nsp_axes *axe;
  int th,color,style,fg,ix,iy;
  int ibutton=-1,imask,iwait=FALSE;
  double mpt[2],x,y;
  nsp_figure *Fig = (((NspGraphic *) R)->obj->Fig);
  BCG *Xgc= Fig->Xgc; /*  window_list_search_new( Fig->id);  */
  if ( Xgc == NULL ) return; 
  axe = ((NspGraphic *) R)->obj->Axe;

  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  th = Xgc->graphic_engine->xget_thickness(Xgc);
  color= Xgc->graphic_engine->xget_pattern(Xgc);
  style = Xgc->graphic_engine->xget_dash(Xgc);
  fg    = Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_thickness(Xgc,1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,fg);
  x=pt[0];y=pt[1];
  rect[0]= x;
  rect[1]= y;
  rect[2]= 0;
  rect[3]= 0;
  while ( ibutton == -1 ) 
    {
      int ok_changed; 
      Cell *C;
      rect2d_f2i(&axe->scale,rect,Xgc->zrect,1);
      for ( i = 0 ; i < 4 ; i++) zrect1[i] = xof[i]+ Xgc->zrect[i];
      Xgc->graphic_engine->invalidate(Xgc,zrect1);
      rect[0]= Min(pt[0],x);
      rect[1]= Max(pt[1],y);
      rect[2]= Abs(pt[0]-x);
      rect[3]= Abs(pt[1]-y);
      /* invalidate new zone */
      rect2d_f2i(&axe->scale,rect,Xgc->zrect,1);
      for ( i = 0 ; i < 4 ; i++) zrect1[i] = xof[i]+ Xgc->zrect[i];
      Xgc->graphic_engine->invalidate(Xgc,zrect1);
      nsp_set_cursor(Xgc,GDK_BOTTOM_RIGHT_CORNER);
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix, &iy,iwait,TRUE,TRUE,FALSE);
      nsp_axes_i2f(axe,ix,iy,mpt);
      x=mpt[0];y=mpt[1];
      /* hilite objects which are contained in bbox 
       */ 
      C = R->obj->children->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ )
	    {
	      NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	      double o_rect[4]; 
	      NspGraphic *G = (NspGraphic *) C->O;
	      if ( G->type->bounds(G,o_rect) == TRUE 
		   && o_rect[0] >= rect[0]  && o_rect[2] <= rect[0]+rect[2] 
		   && o_rect[1] >= rect[1]-rect[3] && o_rect[3] <= rect[1] )
		{
		  ok_changed  = TRUE;
		  bf->set_hilited(C->O,TRUE);
		}
	      else 
		{
		  bf->set_hilited(C->O,FALSE);
		}
	    }
	  C = C->next ;
	}
    }
  nsp_set_cursor(Xgc,-1);
  for ( i = 0 ; i < 4 ; i++) zrect1[i] = xof[i]+ Xgc->zrect[i];
  /* disable zrect */
  Xgc->zrect[2]=   Xgc->zrect[3]=0;
  /* last draw without rectangle selection */
  Xgc->graphic_engine->invalidate(Xgc,zrect1);
  Xgc->graphic_engine->xset_thickness(Xgc,th);
  Xgc->graphic_engine->xset_dash(Xgc,style);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  Xgc->graphic_engine->xinfo(Xgc," ");
}

/**
 * nsp_diagram_select_and_move:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and move it with the mouse.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_diagram_select_and_move(NspDiagram *R,const double pt[2],int mask)
{
  int k1, cp;
  NspTypeGRint *bf;
  NspObject *O;
  int k = nsp_diagram_select_obj(R,pt,&O,NULL);
  if ( k==0 )
    {
      double bbox[4];
      /* acquire a rectangle and hilite objects inside */
      nsp_diagram_unhilite_objs(R,TRUE);
      nsp_diagram_rectangle_select_objs(R,pt,bbox);
      return OK;
    }
  bf = GR_INT(O->basetype->interface);
  /* are we inside a control point ? */
  k1 = bf->control_near_pt(O,pt,&cp);
  /* is the object already hilited */
  if ( bf->get_hilited(O) == TRUE || mask == 1 ) 
    {
      if ( k1 == FALSE ) 
	{
	  int rep,click;
	  /* we enter a move selection 
	   */
	  rep = nsp_diagram_select_and_move_list(R,O, pt,&click);
	  if ( rep == -100) return OK;
	  if ( click == TRUE && mask == 0 ) 
	    {
	      /* it was a click not a move */
	      nsp_diagram_unhilite_objs(R,FALSE); 
	      bf->set_hilited(O,TRUE);
	    }
	  return OK;
	}
      /* here we keep the selection active but we continue 
       * here with a MOVE_CONTROL
       */
    }
  else 
    {
      /* here the object is newly selected thus we have 
       * to unhilite others except if we are in a shift move 
       */
      nsp_diagram_unhilite_objs(R,FALSE); 
    }
  /*
   * hilite the moving object 
   */
  bf->set_hilited(O,TRUE);
  if ( k1 == FALSE ) 
    {
      if ( nsp_diagram_move_obj(R,O, pt, -5,cp,MOVE ) == -100) 
	return OK;
    }
  else
    {
      if ( nsp_diagram_move_obj(R,O, pt, -5,cp,MOVE_CONTROL ) == -100) 
	return OK;
    }
  return OK;
}


/**
 * nsp_diagram_get_hilited_list:
 * @gf: a #nsp_diagram 
 * @full_copy: %TRUE for a full copy.
 * 
 * returns in a list a copy of the hilited objects of #nsp_diagram.
 * Depending on the parameter @full_copy, we perform a copy or a full copy.
 * Note that when a full copy is performed cross-references within the copy 
 * are updated.
 * 
 * Return value: %OK or %FAIL
 **/

NspList *nsp_diagram_get_hilited_list(nsp_diagram *gf, int full_copy)
{
  NspObject *obj=NULL;
  NspList *Loc;
  Cell *cloc= gf->children->first ;
  if ( (Loc = nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspTypeGRint *bf= GR_INT(cloc->O->basetype->interface);
	  if ( bf->get_hilited(cloc->O) == TRUE) 
	    {
	      if ( full_copy == TRUE ) 
		{
		  if ((obj = nsp_object_full_copy_and_name("lel",cloc->O)) == NULLOBJ)
		      goto err;
		}
	      else
		{
		  if ((obj= nsp_object_copy_with_name(cloc->O)) == NULLOBJ)  goto err;
		}
	      if ( nsp_list_end_insert(Loc, obj) == FAIL ) goto err;
	    }
	}
      cloc = cloc->next;
    }
  /* update the cross references in the copy */
  nsp_diagram_list_recompute_pointers(Loc); 
  return Loc;
 err:
  nsp_list_destroy(Loc);
  return NULLLIST;
} 


/**
 * nsp_diagram_select_and_move_list:
 * @R: a #NspDiagram 
 * @Obj: a #NspObject (which was selected to move the selection)
 * @pt: a point position
 * @click: %TRUE or %FALSE 
 * 
 * move a selection with the mouse (hilited blocks). @pt is the initial mouse position. 
 * and @Obj the object which was selected to initialize the move. @Obj can be NULL.
 * We return in @click a boolean which is %TRUE if the list of object was in fact unmoved
 *  (press-release at the same position)
 * 
 * Return value: %OK or %FAIL XXXXX A revoir
 **/

int nsp_diagram_select_and_move_list(NspDiagram *R,NspObject *Obj,const double pt[2], int *click)
{
  int rep, cp=0; /* cp unused */
  NspTypeGRint *bf;
  NspList *L;
  if ( Obj != NULLOBJ) 
    {
      bf = GR_INT(Obj->basetype->interface);
      bf->set_hilited(Obj,TRUE);
    }
  L= nsp_diagram_get_hilited_list(R->obj,FALSE);
  if ( L== NULLLIST) return OK;
  rep = nsp_diagram_move_list_obj(R,L, pt, -5,cp,MOVE, click );
  nsp_list_destroy(L);
  if ( rep == -100) return rep;
  return OK;
}


/**
 * nsp_diagram_select_and_hilite:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and hilite the object. Other hilite objects are 
 * unhilited.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_diagram_select_and_hilite(NspDiagram *R,const double pt[2])
{
  NspTypeGRint *bf;
  NspObject *O;
  int k = nsp_diagram_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  bf = GR_INT(O->basetype->interface);
  nsp_diagram_unhilite_objs(R,FALSE);
  bf->set_hilited(O,TRUE);
  return OK;
}


/**
 * nsp_diagram_select_and_toggle_hilite:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and changes its hilite status the object.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_diagram_select_and_toggle_hilite(NspDiagram *R,const double pt[2])
{
  NspTypeGRint *bf;
  NspObject *O;
  int k = nsp_diagram_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  bf = GR_INT(O->basetype->interface);
  if ( bf->get_hilited(O) == TRUE) 
    bf->set_hilited(O,FALSE);
  else 
    bf->set_hilited(O,TRUE);
  return OK;
}



/**
 * nsp_diagram_select_and_split:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and if this object is a link the link is splited.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_diagram_select_and_split(NspDiagram *R,const double pt[2])
{
  int rep=OK;
  NspObject *Ob;
  int k = nsp_diagram_select_obj(R,pt,&Ob,NULL);
  if ( k==0 ) return FAIL;
  if ( IsLink(Ob) ) 
    {
      NspLink *link;
      rep= link_split(R,(NspLink *) Ob,&link,pt);
    }
  return rep;
}

/**
 * nsp_diagram_select_link_and_add_control:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and if this object is a link a control point is added to the link.
 * 
 * Return value: %OK or %FAIL
 * FIXME: are we also supposed to highlight the link ? 
 **/

int nsp_diagram_select_link_and_add_control(NspDiagram *R,const double pt[2])
{
  int rep=OK;
  NspObject *O;
  int k = nsp_diagram_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  if ( IsLink(O) ) 
    {
      rep= link_add_control((NspLink *)O,pt);
    }
  return rep;
}

/**
 * nsp_diagram_select_link_and_remove_control:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * selects the object which is near the point @pt 
 * and if this object is a link a control point is added to the link.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_diagram_select_link_and_remove_control(NspDiagram *R,const double pt[2])
{
  int rep=OK;
  NspObject *O;
  int k = nsp_diagram_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  if ( IsLink(O) ) 
    {
      rep= link_remove_control((NspLink *)O,pt);
    }
  return rep;
}

/**
 * nsp_diagram_hilite_near_pt:
 * @R: a #NspDiagram 
 * @pt: a point position 
 * 
 * highlights the object which is near the point @pt.
 * 
 * Return value: 
 **/

int  nsp_diagram_hilite_near_pt(NspDiagram *R,const double pt[2])
{
  NspObject *O;
  int k = nsp_diagram_select_obj(R,pt,&O,NULL);
  if ( k==0 )
    {
      nsp_diagram_unhilite_objs(R,TRUE);
    }
  else 
    {
      NspTypeGRint *bf = GR_INT(O->basetype->interface);
      nsp_diagram_unhilite_objs(R,FALSE);
      bf->set_hilited(O,TRUE);
    }
  return OK;
}

/**
 * nsp_diagram_locks_invalidate:
 * @R: a #NspDiagram 
 * @O: a #NspObject. 
 * 
 * calls the invalidate method on the objects which are connected
 * to object @O by lock points.
 **/

static void nsp_diagram_locks_invalidate(NspDiagram *R,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  /* Scierror("XXX diagram_locks_draw lock= %d ports=%d\n",i,np); */
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK && p.object_id != NULL) 
		{
		  NspGraphic *G = (NspGraphic *) p.object_id ;
		  nsp_graphic_invalidate(G);
		  /* G->type->draw(G->obj->Fig,G,NULL); */
		}
	    }
	}
    }
}


/**
 * nsp_diagram_locks_update:
 * @R: a #NspDiagram 
 * @O: a #NspObject. 
 *
 * Updates the position of the control points of 
 * objects which are locked to object @O. 
 * this is usefull when moving  block to update links 
 * positions.
 * 
 **/

void nsp_diagram_locks_update(NspDiagram *R,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      lock_dir dir;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK && p.object_id != NULL) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  double pt[2];
		  bf->get_lock_pos(O,i,pt);
		  /* updates links acordingly and try to keep 
		   * prefered direction 
		   */
		  dir = bf->get_lock_dir(O,i);		  
		  bf1->set_lock_pos(O1,p.lock,pt,TRUE,dir);
		}
	    }
	}
    }

}

/**
 * nspdiagram_recompute_obj_pointers:
 * @gf: a #nsp_diagram 
 * @O: a #NspObject
 * 
 * This function updates the cross references to other objects 
 * for object @O. Object @O contains in the sid field the old 
 * adresses of objects to be searched. And the new adresses are 
 * used to update the id field. 
 * 
 **/

static void nspdiagram_recompute_obj_pointers(NspList *L,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      int np = bf->get_number_of_ports(O,i);
      int j;
      for ( j= 0 ; j < np ; j++) 
	{
	  gr_port p;
	  if ( bf->get_lock_connection(O,i,j,&p)== OK ) 
	    {
	      if ( p.object_sid != NULL) 
		{
		  void *new= nspdiagram_get_adress(L,p.object_sid );
		  p.object_id = new;
		  p.object_sid = NULL;
		  /* A uniformiser */
		  bf->set_lock_connection(O,i,j,&p);
		}
	    }
	}
    }
}

/**
 * nspdiagram_recompute_obj_pointers:
 * @gf: a #nsp_diagram 
 * 
 * This function updates all the cross references contained 
 * in objects stored in @gf. This is used after a full copy
 * and works even if only a subset of @gf was full copied.
 * 
 **/

static void nspdiagram_recompute_pointers(nsp_diagram *gf)
{
  nsp_diagram_list_recompute_pointers(gf->children);
}

/**
 * nsp_diagram_list_recompute_obj_pointers:
 * @L: a #NspList 
 * 
 * This function updates all the cross references contained 
 * in objects stored in @L. Note that cross references refering 
 * to objects not in @L are set to NULL. Thus this function can 
 * be used when a full copy of a subset of a #nsp_diagram is done.
 *
 **/

static void nsp_diagram_list_recompute_pointers(NspList *L)
{
  int count = 1;
  Cell *C = L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  nspdiagram_recompute_obj_pointers(L,C->O);
	}
      C = C->next ;
      count++;
    }
}




/**
 * nsp_diagram_move_obj:
 * @F: : a #NspDiagram 
 * @O: the #NspObject to be moved. 
 * @pt: the initial position of the mouse.
 * @stop: an integer giving the mouse code to accept for ending the move 
 * @cp: the id of the control point to be moved
 * @action: %MOVE or %MOVE_CONTROL
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_diagram_move_obj(NspDiagram *D,NspObject *O,const double pt[2],int stop,
			 int cp,move_action action)
{
  int ix,iy;
  int rep;
  nsp_figure *Fig = (((NspGraphic *) D)->obj->Fig);
  BCG *Xgc= Fig->Xgc; /*  window_list_search_new( Fig->id);  */
  int wstop = 0, ibutton,imask, iwait=FALSE;
  double mpt[2],pt1[2]= {pt[0],pt[1]},ptwork[2];
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  NspGraphic *G;
  switch ( action ) 
    {
    case MOVE : 
      nsp_set_cursor(Xgc,GDK_FLEUR);
      break;
    case MOVE_CONTROL :
      if ( IsBlock(O) )
	nsp_set_cursor(Xgc,GDK_BOTTOM_RIGHT_CORNER);
      else 
	nsp_set_cursor(Xgc,GDK_FLEUR);
      bf->move_control_init(O,cp,ptwork);
      break;
    }

  /* invalidate the moving object */
  nsp_graphic_invalidate((NspGraphic *) O);
  nsp_diagram_locks_invalidate(D,O);

  /*
   * mpt is the mouse position, 
   * ptwork is the control point position 
   */
  while ( wstop==0 ) 
    {
      /* get new mouse position in pixel */
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix,&iy,iwait,TRUE,TRUE,FALSE);
      nsp_axes_i2f(((NspGraphic *) D)->obj->Axe,ix,iy,mpt);
      if ( ibutton == -100 ) 
	{
	  /* window is destroyed */
	  return ibutton;
	}
      if ( ibutton == stop ) wstop= 1;
      Xgc->graphic_engine->xinfo(Xgc,"ibutton=%d",ibutton);
      /* move object 
       * note that the translate operation will invalidate the 
       * moving object
       */
      switch ( action ) 
	{
	case MOVE : 
	  G = (NspGraphic *) O;
	  G->type->translate(G,(pt1[0]= mpt[0] -pt1[0],pt1[1]=mpt[1] -pt1[1],pt1));
	  nsp_diagram_locks_invalidate(D,(NspObject *)G);
	  rep = OK; /* translate was returning an int in previous version */
	  if ( rep == FAIL) wstop=1; /* quit untranslatable objects */
	  break;
	case MOVE_CONTROL :
	  bf->move_control(D,O,mpt,cp, ptwork);
	  nsp_diagram_locks_invalidate(D,O);
	}
      /* update locks positions for objects locked to objects  */ 
      nsp_diagram_locks_update(D,O);
      pt1[0] = mpt[0];
      pt1[1] = mpt[1];
    }
  nsp_set_cursor(Xgc,-1);
  if ( IsLink(O)) link_check(D,(NspLink *)O);

  return ibutton;
}

/**
 * nsp_diagram_move_list_obj:
 * @F: : a #NspDiagram 
 * @L: the #NspList of Objects to be moved. 
 * @pt: the initial position of the mouse.
 * @stop: an integer giving the mouse code to accept for ending the move 
 * @cp: the id of the control point to be moved
 * @action: %MOVE
 * @click: %TRUE or %FALSE 
 * 
 * move a list of objects, The only action for a list of objects is %MOVE
 * (%MOVE_CONTROL has no sense). We return in @click a boolean which is %TRUE 
 * if the list of object was in fact unmove (press-release at the same position)
 * 
 * Return value: an integer 
 **/

static int nsp_diagram_list_obj_action(NspDiagram *F,NspList *L,const double pt[2],list_move_action action)
{
  int rep = OK;
  Cell *C = L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  switch ( action )
	    {
	    case L_INVALIDATE : 
	      if ( IsBlock(C->O)  || IsConnector(C->O))
		{
		  NspGraphic *G = (NspGraphic *) C->O;
		  nsp_graphic_invalidate(G);
		  /*   G->type->draw(G->obj->Fig,G,NULL); */
		  nsp_diagram_locks_invalidate(F,C->O);
		}
	      break;
	    case L_TRANSLATE : 
	      ((NspGraphic *) C->O)->type->translate((NspGraphic *) C->O,pt);
	      rep= OK;
	      if ( rep == FAIL) 
		{
		  return rep;
		}

	      break;
	    case L_LOCK_UPDATE:
	      if ( IsBlock(C->O)  || IsConnector(C->O))
		nsp_diagram_locks_update(F,C->O);
	      break;
	    case L_LINK_CHECK:
	      if ( IsLink(C->O)) link_check(F,(NspLink *) (C->O));
	      break;
	    }
	}
      C = C->next ;
    }
  return OK;
}

int nsp_diagram_move_list_obj(NspDiagram *F,NspList *L,const double pt[2],int stop,int cp,move_action action, int *click)
{
  int rep,ix,iy;
  BCG *Xgc= ((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->Xgc; 
  int wstop = 0, ibutton,imask, iwait=FALSE;
  double mpt[2],pt1[2]= {pt[0],pt[1]};

  nsp_set_cursor(Xgc,GDK_FLEUR);

  if ( action == MOVE_CONTROL) 
    {
      /* nothing to do */
    }
  /*
   * mpt is the mouse position, 
   * ptwork is the control point position 
   */
  /* invalidate the moving list */
  nsp_diagram_list_obj_action(F,L,pt,L_INVALIDATE);

  while ( wstop==0 ) 
    {
      /* draw the frame 
       * we could here record and use a fixed part.
       */
      /* get new mouse position */
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix,&iy,iwait,TRUE,TRUE,FALSE);
      nsp_axes_i2f(((NspGraphic *) F)->obj->Axe,ix,iy,mpt);
      /* nsp_get_point_axes(Xgc,ix,iy,mpt); */
      if ( ibutton == -100 ) 
	{
	  return ibutton;
	}
      if ( ibutton == stop ) wstop= 1;
      Xgc->graphic_engine->xinfo(Xgc,"ibutton=%d",ibutton);
      /* move object */
      switch ( action ) 
	{
	case MOVE : 
	  rep=  nsp_diagram_list_obj_action(F,L,(pt1[0]= mpt[0] -pt1[0],pt1[1]=mpt[1] -pt1[1],pt1),L_TRANSLATE);
	  if ( rep == FAIL) wstop=1; /* quit untranslatable objects */
	  break;
	case MOVE_CONTROL :
	  /* unused */
	  break;
	}
      /* update locks positions for objects locked to objects  */ 
      nsp_diagram_list_obj_action(F,L,pt, L_LOCK_UPDATE);
      /* invalidate the moving list 
       * note that invalidate was also performed by L_TRANSLATE
       */
      nsp_diagram_list_obj_action(F,L,pt,L_INVALIDATE);
      pt1[0] = mpt[0];
      pt1[1] = mpt[1];
    }
  /* was it a click ? */
  *click =  ( pt1[0]== pt[0] && pt1[1] == pt[1] ) ? TRUE : FALSE;
  /* we return the last activated button code */

  nsp_set_cursor(Xgc,-1);

  return ibutton;
}



/**
 * nsp_diagram_unhilite_objs:
 * @R:  a #NspDiagram 
 * @draw: an integer 
 * 
 * unhighlight the highlighted objects of @R if 
 * @draw is equal to %TRUE the objects are redrawn.
 * 
 **/

void nsp_diagram_unhilite_objs(NspDiagram *R,int draw )
{
  Cell *C = R->obj->children->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE) 
	    {
	      bf->set_hilited(C->O,FALSE);
	    }
	}
      C = C->next ;
    }
}

/**
 * nsp_diagram_delete_hilited:
 * @R: : a #NspDiagram 
 * 
 * delete hilited objects of @F.
 **/

void nsp_diagram_delete_hilited(NspDiagram *R) 
{
  Cell *C = R->obj->children->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* grint interface */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE ) 
	    {
	      /* we change the hilited property to invalidate the 
	       * object (drawing will be updated).
	       */
	      bf->set_hilited(C->O,FALSE);
	      nsp_object_destroy(&C->O);
	      C->O = NULLOBJ;
	    }
	}
      C = C->next ;
    }
  /* 
   * here we must compact the list which can have holes 
   * but it implies that lock number are to be properly updated 
   * FIXME ? 
   */
}
/**
 * nsp_diagram_get_hilited:
 * @R: : a #NspDiagram 
 * 
 * return the first hilited object of the list of objects 
 * contained in @R.
 * 
 **/

NspObject * nsp_diagram_get_hilited(NspDiagram *R) 
{
  Cell *C = R->obj->children->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* grint interface */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE ) 
	    {
	      return C->O;
	    }
	}
      C = C->next ;
    }
  return NULL;
}

/**
 * nsp_diagram_create_new_block:
 * @F: a #NspDiagram 
 * 
 * creates a new block which is positioned interactively and 
 * inserted in @F.
 * 
 * Return value: %OK or %FALSE.
 **/

NspObject * nsp_diagram_create_new_block(NspDiagram *F,const double pt[2])
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={pt[0],pt[1],10,10};
  NspBlock *B;
  NspGraphic *G;
  BCG *Xgc;
  /* unhilite all */
  nsp_diagram_unhilite_objs(F,FALSE);
  B = nsp_block_create("fe",NULL,rect,color,thickness,background,0,NULL,NULL,NULL);
  if ( B == NULLBLOCK) return NULLOBJ;
  G = (NspGraphic *) B;
  G->obj->hilited = TRUE;
  G->type->link_figure(G,((NspGraphic *) F)->obj->Fig,((NspGraphic *) F)->obj->Axe);
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return NULLOBJ;
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  Xgc= ((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->Xgc; 
  return NSP_OBJECT(B);
}

/**
 * nsp_diagram_create_new_gridblock:
 * @F: a #NspDiagram 
 * 
 * creates a new super block which is positioned interactively and 
 * inserted in @F. The super block is filled with current hilited objects 
 * which are then removed from the frame 
 * 
 * Return value: %OK or %FALSE.
 **/

NspObject * nsp_diagram_create_new_gridblock(NspDiagram *F, int flag )
{
#if 0 
  /* XXXX à revoir */
  NspDiagram *F1;
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,10,10}, pt[]={0,100};
  NspGridBlock *B;
  /* create the gridblock */
  if ( flag == TRUE) 
    {
      /* empty super block */
      B=gridblock_create("fe",rect,color,thickness,background,NULL);
      if ( B == NULLGRIDBLOCK) return NULLOBJ;
    }
  else 
    {
      if ((F1 = nsp_diagram_hilited_full_copy(F)) == NULLDIAGRAM) return NULLOBJ;
      B=gridblock_create_from_nsp_diagram("fe",rect,color,thickness,background,F1);
      nsp_diagram_destroy(F1);
      if ( B == NULLGRIDBLOCK) return NULLOBJ;
      /* unhilite all */
      nsp_diagram_delete_hilited(F);
    }
  ((NspBlock *)B)->obj->frame = F->obj;
  ((NspBlock *)B)->obj->hilited = TRUE;
  B->obj->Xgc = F->obj->Xgc;
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return NULLOBJ;
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  return NSP_OBJECT(B);
#else
  return NULL;
#endif 
}


/**
 * nsp_diagram_create_new_connector:
 * @F: a #NspDiagram 
 * 
 * create a new connector which is positioned interactively and 
 * inserted in @F.
 * 
 * Return value: %OK or %FALSE
 **/

NspObject * nsp_diagram_create_new_connector(NspDiagram *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,4,4}, pt[]={0,100};
  NspConnector *B;
  NspGraphic *G;
  BCG *Xgc;
  gr_lock l;
  /* unhilite all */
  nsp_diagram_unhilite_objs(F,FALSE);
  B=nsp_connector_create("fe",NULL,rect,color,thickness,background,l,NULL);
  if ( B == NULL) return NULLOBJ;
  G = (NspGraphic *) B;
  G->obj->hilited = TRUE;
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return NULLOBJ;
  G->type->link_figure(G,((NspGraphic *) F)->obj->Fig,((NspGraphic *) F)->obj->Axe);
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  Xgc= ((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->Xgc; 
  return NSP_OBJECT(B);
}

/**
 * nsp_diagram_create_new_rect:
 * @F: a #NspDiagram 
 * 
 * create a new rectangle which is positioned interactively and 
 * inserted in @F.
 * 
 * Return value: %OK or %FALSE
 **/
#if 0 
int nsp_diagram_create_new_rect(NspDiagram *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,4,4}, pt[]={0,100};
  NspRect *B;
  BCG *Xgc;
  /* unhilite all */
  nsp_diagram_unhilite_objs(F,FALSE);
  Xgc= ((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->Xgc;

  B=rect_create("fe",Xgc,rect,color,thickness,background,NULL);
  if ( B == NULL) return FAIL;
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return FAIL;
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return FAIL;
  /* XXXX block_draw(B); */
  return OK;
}
#endif 

/**
 * nsp_diagram_create_new_link:
 * @F: a #NspDiagram 
 * 
 * Interactively creates a new link (#NspLink) and inserts 
 * the link in object @F.
 * 
 * Return value: %FAIL or %OK.
 *
 **/

static double lock_size=1;

NspObject * nsp_diagram_create_new_link(NspDiagram *F)
{
  NspGraphic *G;
  int ix,iy;
  BCG *Xgc= ((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->Xgc;
  NspObject *Ob;
  int cp1;
  double mpt[2],pt[2];
  int wstop = 0,stop=2, ibutton, imask, iwait=FALSE;
  int color=4,thickness=1,hvfactor,count=0;
  NspLink *L;
  NspTypeGRint *bf;
  /* unhilite all */
  nsp_diagram_unhilite_objs(F,FALSE);
  hvfactor=lock_size*2;/* magnetism toward horizontal and vertical line  */
  Xgc->graphic_engine->xinfo(Xgc,"Enter polyline, Right click to stop");
  
  /* prepare a link with 1 points */
  L= link_create_n("fe",1,color,thickness);
  bf = GR_INT(((NspObject *) L)->basetype->interface);
  if ( L == NULLLINK) return NULLOBJ;
  G = (NspGraphic *) L;
  G->obj->hilited = TRUE;
  L->obj->poly->R[0]=mpt[0];
  L->obj->poly->R[1]=mpt[0];
  /* insert link in diagram at the start 
   */
  if (nsp_list_insert(F->obj->children,(NspObject  *) L,0) == FAIL) return NULLOBJ;
  
  G->type->link_figure(G,((NspGraphic *) F)->obj->Fig,((NspGraphic *) F)->obj->Axe);
  nsp_graphic_invalidate(G);

  while ( wstop==0 ) 
    {
      /* get new mouse position */
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix,&iy,iwait,TRUE,TRUE,FALSE);
      nsp_axes_i2f(((NspGraphic *) F)->obj->Axe,ix,iy,mpt);
      nsp_graphic_invalidate(G);
      if ( ibutton == -100 ) 
	{
	  /* we stop : window was killed */
	  return NULLOBJ;
	}
      if ( ibutton == stop ) 
	{
	  /* here we stop with a right click 
	   */
	  if (  count >= 2  ) 
	    {
	      double *x = L->obj->poly->R, *y = L->obj->poly->R + L->obj->poly->m;
	      /* try to improve angles before quit */
	      /*  magnetism toward horizontal or vertival lines */
	      if ( Abs( x[count] - x[count-1] ) < hvfactor ) x[count-1] = x[count];
	      if ( Abs( y[count] - y[count-1] ) < hvfactor ) y[count-1] = y[count];
	    }              
	}
      if ( ibutton == stop ) 
	{
	  break; 
	}
      else if ( ibutton == 0 ) 
	{
	  /* this is a left click click 
	   * If the left click is near a lock point we stop 
	   * 
	   */
	  int lock_c, rep;
	  /* are we near a lock point ? if true mpt is changed  */
	  pt[0]=mpt[0]; pt[1]=mpt[1];
	  rep = nsp_diagram_select_lock(F,mpt, &Ob, &cp1,&lock_c);
	  /* link_check will check if the lock point is already in use */
	  if ( rep != 0 )
	    {
	      /* set last point to lock position and stop if it's not the first point*/
	      L->obj->poly->R[count]= mpt[0];
	      L->obj->poly->R[count+L->obj->poly->m]= mpt[1];
	      if ( count != 0) 
		{
		  if ( count >= 2 ) 
		    {
		      double *x = L->obj->poly->R, *y = L->obj->poly->R + L->obj->poly->m;
		      /* try to improve angles before quit */
		      /*  magnetism toward horizontal or vertival lines */
		      if ( Abs( x[count] - x[count-1] ) < hvfactor ) x[count-1] = x[count];
		      if ( Abs( y[count] - y[count-1] ) < hvfactor ) y[count-1] = y[count];
		    }		  
		  /* we have finished */
		  break;
		}
	    }
	  if ( nsp_matrix_add_rows(L->obj->poly,1)== FAIL ) return NULLOBJ;	  
	  count ++;
	  L->obj->poly->R[count]= mpt[0];
	  L->obj->poly->R[count+L->obj->poly->m]= mpt[1];
	}
      else 
	{
	  int lock_c;
	  /* just moving */
	  /* are we near a lock point ? if true mpt is changed  */
	  int rep = nsp_diagram_select_lock(F,mpt, &Ob, &cp1,&lock_c) ;
	  if ( rep == 0 && count != 0 ) 
	    {
	      /*  try to keep horizontal and vertical lines */
	      if ( Abs( L->obj->poly->R[count-1] - mpt[0]) < hvfactor ) mpt[0]=L->obj->poly->R[count-1];
	      if ( Abs( L->obj->poly->R[count-1+L->obj->poly->m] - mpt[1]) < hvfactor ) 
		mpt[1]=L->obj->poly->R[count-1+L->obj->poly->m];
	    }              
	  L->obj->poly->R[count]= mpt[0];
	  L->obj->poly->R[count+L->obj->poly->m]= mpt[1];
	}
      nsp_graphic_invalidate(G);
    }
  /*
   * do not keep a Link with one point 
   */ 
  if ( L->obj->poly->m == 1)
    {
      nsp_graphic_invalidate(G);
      nsp_list_remove_first(F->obj->children);
      return NULLOBJ;
    }
  /* check if first and last points are locked 
   * if true update locks 
   */
  mpt[0]=L->obj->poly->R[0];
  mpt[1]=L->obj->poly->R[L->obj->poly->m];
  link_lock_update(F,L,0,mpt);
  mpt[0]=L->obj->poly->R[L->obj->poly->m-1];
  mpt[1]=L->obj->poly->R[2*L->obj->poly->m-1];
  link_lock_update(F,L,1,mpt);
  /* check if we are locked */
  link_check(F,L);
  nsp_graphic_invalidate(G);
  return NSP_OBJECT(L);
}


/**
 * nsp_diagram_hilited_full_copy:
 * @self: a #NspDiagram
 * 
 * Make a full copy of a @self but only for hilited objects. 
 * Since @self contains a list of objects which are themselves objects with 
 * references, the full copy must be performed on these
 * objects and cross references are to be updates.
 * 
 * Returns: a new #NspDiagram or %NULLDIAGRAM
 **/

static NspDiagram *nsp_diagram_hilited_full_copy_partial(NspDiagram *H,NspDiagram *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_diagram))) == NULL) return NULLDIAGRAM;
  H->obj->ref_count=1;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_full_copy_and_name("bounds",NSP_OBJECT(self->obj->bounds)))
	  == NULLMAT) return NULL;
    }
  H->obj->children =  self->obj->children;
  if ( H->obj->children != NULL) 
    {
      H->obj->children = nsp_diagram_list_full_copy(H->obj->children, TRUE);
      if (  H->obj->children == NULL) return NULL;
    }
  return H;
}

NspDiagram *nsp_diagram_hilited_full_copy(NspDiagram *self)
{
  NspDiagram *H  =nsp_diagram_create_void(NVOID,(NspTypeBase *) nsp_type_diagram);
  if ( H ==  NULLDIAGRAM) return NULLDIAGRAM;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLDIAGRAM;
  if ( nsp_diagram_hilited_full_copy_partial(H,self)== NULL) return NULLDIAGRAM;
  nspdiagram_recompute_pointers(H->obj);
  return H;
}

/**
 * nsp_diagram_list_full_copy:
 * @L: a #NspList 
 * @hilited_only: %TRUE or %FALSE
 * 
 * Make a full copy of a list of objects which all 
 * implements the Grint interface and are all to be full copied. 
 * Note that, after the copy the cross references in the objects 
 * are wrong and are to be restored by nspdiagram_recompute_pointers(). 
 * If @hilited_only is %TRUE, only hilited objects are copied.
 * 
 * Returns: a new #NspList
 **/

static NspList * nsp_diagram_list_full_copy(NspList *L,int hilited_only) 
{
  NspObject *obj=NULL;
  NspList *Loc;
  Cell *cloc;
  if ( (Loc = nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspTypeGRint *bf= GR_INT(cloc->O->basetype->interface);
	  if ( hilited_only == FALSE || bf->get_hilited(cloc->O) == TRUE) 
	    {
	      if ((obj = nsp_object_full_copy_and_name(nsp_object_get_name(cloc->O),cloc->O))
		  == NULLOBJ )  goto err;
	      if ( nsp_object_set_name(obj,nsp_object_get_name(cloc->O)) == FAIL ) goto err;
	      if ( nsp_list_end_insert(Loc, obj) == FAIL ) goto err;
	    }
	}
      cloc = cloc->next;
    }
  return Loc;
 err:
  nsp_list_destroy(Loc);
  return NULLLIST;
} 



#line 2920 "diagram.c"
