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





#line 89 "codegen/diagram.override"

#include "nsp/link.h"
#include "nsp/block.h"
#include "nsp/connector.h"
#include "nsp/figuredata.h"
#include "nsp/figure.h"
#include "nsp/diagram.h"

#line 37 "diagram.c"

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

#line 104 "codegen/diagram.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_diagram;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_diagram ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_diagram  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_diagram  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_diagram  ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_diagram_link_figure; 
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_diagram_unlink_figure; 
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_diagram_children ;
#line 119 "diagram.c"
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
  if ((M->obj = calloc(1,sizeof(nsp_diagram))) == NULL) return NULL;
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
  if ((H  = nsp_diagram_xdr_load_partial(xdrs,H))== NULLDIAGRAM) return H;
  if ( nsp_diagram_check_values(H) == FAIL) return NULLDIAGRAM;
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
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 217 "codegen/diagram.override"
/* draw */

static int _wrap_diagram_draw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  nsp_diagram_draw(self);
  MoveObj(stack,1,self);
  return 1;
}

#line 570 "diagram.c"


#line 703 "codegen/diagram.override"
/* XXX */
extern void nsp_diagram_tops(NspDiagram *R,char *fname);

static int _wrap_diagram_tops(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep=1,color=-1;
  char *filename= NULL, *mode = NULL;
  static char *Table[] = {"d", "l", "n", "p", "k", NULL};
  int_types T[] = {string, new_opts, t_end} ;
  nsp_option opts[] ={{ "color",s_bool,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&filename,&opts,&color,&mode) == FAIL) return RET_BUG;
  if ( mode != NULL) 
    {
      rep = is_string_in_array(mode,Table,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,mode,Table,"optional argument mode");
	  return RET_BUG;
	}
    }
  /* XXXXXXXXX nsp_diagram_tops(self,filename); */
  return 0;
}


#line 601 "diagram.c"


#line 440 "codegen/diagram.override"

static int _wrap_diagram_new_link(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj =   nsp_diagram_create_new_link(((NspDiagram *) self)))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

#line 618 "diagram.c"


#line 365 "codegen/diagram.override"

static int _wrap_diagram_new_block(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj = nsp_diagram_create_new_block(((NspDiagram *) self)))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}


#line 636 "diagram.c"


#line 381 "codegen/diagram.override"
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

#line 655 "diagram.c"


#line 398 "codegen/diagram.override"

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


#line 673 "diagram.c"


#line 414 "codegen/diagram.override"

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

#line 690 "diagram.c"


#line 429 "codegen/diagram.override"

static int _wrap_diagram_new_rect(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  /* nsp_diagram_create_new_rect(((NspDiagram *) self)); */
  return 0;
}

#line 703 "diagram.c"


#line 351 "codegen/diagram.override"

static int _wrap_diagram_hilite_near_pt(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  nsp_diagram_hilite_near_pt(((NspDiagram *) self),pt->R);
  return 0;
}

#line 719 "diagram.c"


#line 229 "codegen/diagram.override"

/* select_and_move select current unhilite others and move current */

static int _wrap_diagram_select_and_move(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  nsp_diagram_select_and_move(((NspDiagram *) self),pt->R, 0);
  return 0;
}

#line 737 "diagram.c"


#line 245 "codegen/diagram.override"

/* select_and_move_list: select current and move all hilited in group */

static int _wrap_diagram_select_and_move_list(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  nsp_diagram_select_and_move(((NspDiagram *) self),pt->R, 1);
  return 0;
}


#line 756 "diagram.c"


#line 262 "codegen/diagram.override"
/* select_and_hilite */

static int _wrap_diagram_select_and_hilite(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  NspObject *bool;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  rep= nsp_diagram_select_and_hilite(((NspDiagram *) self),pt->R);
  if ((bool = nsp_create_boolean_object(NVOID,(rep == OK) ? TRUE : FALSE))
      == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,bool);
  return 1;
}


#line 779 "diagram.c"


#line 283 "codegen/diagram.override"

static int _wrap_diagram_select_and_toggle_hilite(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  NspObject *bool;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  rep= nsp_diagram_select_and_toggle_hilite(((NspDiagram *) self),pt->R);
  if ((bool = nsp_create_boolean_object(NVOID,(rep == OK) ? TRUE : FALSE))
      == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,bool);
  return 1;
}


#line 801 "diagram.c"


#line 303 "codegen/diagram.override"
/* split link */

static int _wrap_diagram_select_and_split(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  nsp_diagram_select_and_split(((NspDiagram *) self),pt->R);
  return 0;
}


#line 819 "diagram.c"


#line 319 "codegen/diagram.override"
/* split link */

static int _wrap_diagram_select_link_and_add_control(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  nsp_diagram_select_link_and_add_control(((NspDiagram *) self),pt->R);
  return 0;
}



#line 838 "diagram.c"


#line 336 "codegen/diagram.override"
/* shorten link */
static int _wrap_diagram_select_link_and_remove_control(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  nsp_diagram_select_link_and_remove_control(((NspDiagram *) self),pt->R);
  return 0;
}


#line 855 "diagram.c"


#line 455 "codegen/diagram.override"

static int _wrap_diagram_delete_hilited(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  nsp_diagram_delete_hilited(((NspDiagram *) self));
  return 0;
}


#line 869 "diagram.c"


#line 538 "codegen/diagram.override"
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


#line 919 "diagram.c"


#line 586 "codegen/diagram.override"
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
  NspDiagram *F = self;
  Cell *C;
  NspDiagram *GF;
  CheckRhs(2,2);
  CheckLhs(-1,0);
  if ((GF=GetDiagram(stack,1)) == NULLDIAGRAM ) return RET_BUG;
  if ((pt = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),2,pt,2);
  /* now we loop on objects and insert them 
   * this could be turned into a new function 
   */
  if ((GF = nsp_diagram_full_copy(GF))== NULLDIAGRAM)   return RET_BUG;
  /* 
   * pt is the mouse position 
   * we have to translate or move the upper-left rectangle enclosing the 
   * selection to pt. Thus we need to now this enclosing rectangle.
   * XXX: we start here by using the first object position as a position 
   */
  Obj = nsp_list_get_element(GF->obj->children,1);
  if ( Obj != NULLOBJ ) 
    {
      double pt2[2];
      bf = GR_INT(Obj->basetype->interface);
      bf->get_pos(Obj,pt2);
      pt1[0] = pt->R[0]-pt2[0];
      pt1[1] = pt->R[1]-pt2[1];
    }
  nsp_diagram_list_obj_action(GF,GF->obj->children,pt1,L_TRANSLATE);
  nsp_diagram_list_obj_action(GF,GF->obj->children,pt1, L_LOCK_UPDATE);
  /* unselect the objects */
  nsp_diagram_unhilite_objs(F,FALSE);
  /* insert each object 
   * 
   */
  C = GF->obj->children->first; 
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* set the frame field of each object */
	  /* NspBlock *B = (NspBlock *) C->O;  */
	  /* B->obj->frame = F->obj; */
	  /* hilite inserted */
	  bf = GR_INT(C->O->basetype->interface);
	  bf->set_hilited(C->O,TRUE);
	  /* add the object */
	  if ( nsp_list_end_insert(F->obj->children,C->O) == FAIL )
	    return RET_BUG; 
	  C->O= NULLOBJ;
	}
      C = C->next ;
    }
  /* we can now destroy GF */
  nsp_diagram_destroy(GF);
  /* and we can enter a move_selection */
  rep = nsp_diagram_select_and_move_list(F,NULLOBJ,pt->R,&click);
  nsp_diagram_draw(F);
  return 0;
}


#line 995 "diagram.c"


#line 467 "codegen/diagram.override"
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



#line 1024 "diagram.c"


#line 494 "codegen/diagram.override"
/* get a full copy of the first hilited object */
static int _wrap_diagram_get_selection_copy(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj,*bool;
  NspTypeGRint *bf;
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
  /* we want here a full copy 
   * we assume that obj implements grint interface 
   */
  bf = GR_INT(obj->basetype->interface);
  if ((obj = bf->full_copy(obj))== NULLOBJ)  return RET_BUG;
  if ((bool = nsp_create_boolean_object(NVOID,TRUE)));
  MoveObj(stack,1,bool);
  MoveObj(stack,2,obj);
  return 2;
}


#line 1055 "diagram.c"


#line 523 "codegen/diagram.override"
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


#line 1072 "diagram.c"


#line 660 "codegen/diagram.override"
/* connect a diagram to a physical window 
 *
 */

static int _wrap_diagram_attach_to_window(void *self,Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int winid;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ( GetScalarInt(stack,1,&winid) == FAIL) return RET_BUG;
  Xgc =  window_list_search_new(winid);
  if ( Xgc == NULL ) 
    {
      Scierror("Error: Graphic window %d does not exists\n",winid);
      return RET_BUG;
    }
  /* ((NspDiagram *) self)->obj->Xgc = Xgc; */
  /* ((NspDiagram *) self)->obj->top = TRUE; */
  return 0;
}

#line 1098 "diagram.c"


#line 746 "codegen/diagram.override"
/* check if we are over an object */

static int _wrap_diagram_check_pointer(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int k, hilited = FALSE;
  NspObject *Obj;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(0,2);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  k = nsp_diagram_select_obj(((NspDiagram *) self),pt->R,&Obj,NULL);
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

#line 1127 "diagram.c"


#line 684 "codegen/diagram.override"

static int _wrap_diagram_full_copy(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspDiagram *F;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((F = nsp_diagram_full_copy((NspDiagram *) self))== NULLDIAGRAM) 
    {
      Scierror("Error: copy failed\n");
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(F));
  return 1;
}



#line 1148 "diagram.c"


#line 732 "codegen/diagram.override"

static int _wrap_diagram_get_nobjs(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int length;
  CheckRhs(0,0);
  CheckLhs(0,1);
  length =nsp_list_length( ((NspDiagram *) self)->obj->children);
  if ( nsp_move_double(stack,1,(double) length) == FAIL)  return RET_BUG;
  return 1;
}


#line 1164 "diagram.c"


static NspMethods diagram_methods[] = {
  {"draw",(nsp_method *) _wrap_diagram_draw},
  {"tops",(nsp_method *) _wrap_diagram_tops},
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
  {"attach_to_window",(nsp_method *) _wrap_diagram_attach_to_window},
  {"check_pointer",(nsp_method *) _wrap_diagram_check_pointer},
  {"full_copy",(nsp_method *) _wrap_diagram_full_copy},
  {"get_nobjs",(nsp_method *) _wrap_diagram_get_nobjs},
  { NULL, NULL}
};

static NspMethods *diagram_get_methods(void) { return diagram_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 140 "codegen/diagram.override"

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
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (((NspDiagram *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspDiagram *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspDiagram *) self)->obj->children);
    }
  ((NspDiagram *) self)->obj->children =  (NspList *) val;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig);
  nsp_diagram_compute_inside_bounds(NULL,self);
  return OK;
}

static int _wrap_diagram_set_children(void *self, char *attr, NspObject *O)
{
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspDiagram *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspDiagram *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspDiagram *) self)->obj->children);
    }
  ((NspDiagram *) self)->obj->children= children;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig);
  nsp_diagram_compute_inside_bounds(NULL,self);
  return OK;
}


#line 1257 "diagram.c"
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
#line 196 "codegen/diagram.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_diagram(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1284 "diagram.c"


#line 206 "codegen/diagram.override"

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

#line 773 "codegen/diagram.override"

/* inserted verbatim at the end */

static void nsp_draw_diagram(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspDiagram *P = (NspDiagram *) Obj;
  NspList *L = P->obj->children;
  Cell *cloc = L->first;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  /* draw elements */
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G,NULL);
	}
      cloc = cloc->next;
    }
}

/* compute the bounds of the set of objects countained in the 
 * diagram. This function is to be called when contained 
 * objects are changed.
 */

static void nsp_diagram_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj)
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
	  G->type->bounds(Xgc,G,l_bounds);
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

static void nsp_translate_diagram(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  int draw_now;
  NspDiagram *P = (NspDiagram *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* just in case we inihibit the draw during the 
   * while 
   */
  draw_now = ((nsp_figure *) Obj->obj->Fig)->draw_now;
  ((nsp_figure *) Obj->obj->Fig)->draw_now =  FALSE;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->translate(Xgc,G,tr);
	}
      cloc = cloc->next;
    }
  ((nsp_figure *) Obj->obj->Fig)->draw_now = draw_now;
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_diagram(BCG *Xgc,NspGraphic *Obj,double *R)
{
  int draw_now;
  NspDiagram *P = (NspDiagram *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* just in case we inihibit the draw during the 
   * while 
   */
  draw_now = ((nsp_figure *) Obj->obj->Fig)->draw_now;
  ((nsp_figure *) Obj->obj->Fig)->draw_now =  FALSE;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->rotate(Xgc,G,R);
	}
      cloc = cloc->next;
    }
  ((nsp_figure *) Obj->obj->Fig)->draw_now = draw_now;
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_diagram(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int draw_now;
  NspDiagram *P = (NspDiagram *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* just in case we inihibit the draw during the 
   * while 
   */
  draw_now = ((nsp_figure *) Obj->obj->Fig)->draw_now;
  ((nsp_figure *) Obj->obj->Fig)->draw_now =  FALSE;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->scale(Xgc,G,alpha);
	}
      cloc = cloc->next;
    }
  ((nsp_figure *) Obj->obj->Fig)->draw_now = draw_now;
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of diagram 
 *
 */

static int nsp_getbounds_diagram(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspDiagram *P = (NspDiagram *) Obj;
  if ( 0 )
    {
      nsp_diagram_compute_inside_bounds(Xgc,Obj);
      memcpy(bounds,P->obj->bounds->R,4*sizeof(double));
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

static void nsp_diagram_link_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_link_figure(G, F);
  /* link children */
  nsp_list_link_figure(((NspDiagram *) G)->obj->children,F);
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

static int pixmap = FALSE ; /* XXXXX */

/**
 * nsp_diagram_draw:
 * @R: a graphic frame  
 * 
 * draw the objects contained in frame @R.
 * 
 **/

void nsp_diagram_draw(NspDiagram *D)
{
  nsp_figure *Fig = (((NspGraphic *) D)->obj->Fig);
  nsp_figure_force_redraw(Fig);
}

/**
 * nsp_diagram_draw:
 * @R: a graphic frame  
 * 
 * draw the objects contained in frame @R.
 * 
 **/

extern BCG ScilabGCPos ; /* Postscript */
extern Gengine Pos_gengine_old;

void nsp_diagram_tops(NspDiagram *R,char *fname)
{
#if 0
  int wdim[2],*wdim_p=NULL;
  int zero=0,un=1,colored=TRUE;
  BCG *Xgc = R->obj->Xgc;
  if ( Xgc == NULL) return;
  R->obj->Xgc->graphic_engine->xget_windowdim(R->obj->Xgc,wdim,wdim+1);
  wdim_p = wdim;
  ScilabGCPos.graphic_engine = &Pos_gengine_old;
  ScilabGCPos.graphic_engine->initgraphic(fname,&Xgc->CurWindow,wdim_p,NULL,NULL,NULL,'k',NULL);
  if (colored == TRUE ) 
    ScilabGCPos.graphic_engine->xset_usecolor(&ScilabGCPos,un);
  else
    ScilabGCPos.graphic_engine->xset_usecolor(&ScilabGCPos,zero);
  R->obj->Xgc = &ScilabGCPos;
  nsp_diagram_draw(R);
  ScilabGCPos.graphic_engine->xend(&ScilabGCPos);
  R->obj->Xgc = Xgc;
#endif 

}


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

/*
 * set the frame field for all objects 
 * Note that all object can be casted to block for accessing that field
 * XXXX this should be turned into o GR_INT method 
 **/

void nsp_diagram_set_frame_field(NspDiagram *F)
{
  nspdiagram_set_frame_field(F->obj);
}

static void nspdiagram_set_frame_field(nsp_diagram *gf)
{
  int count = 1;
  Cell *C = gf->children->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* 
	     NspBlock *B = (NspBlock *) C->O; 
	     B->obj->frame = gf;
	  */
	}
      C = C->next ;
      count++;
    }
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


static void nsp_diagram_zoom_get_rectangle(NspDiagram *R,const double pt[2],double *rect)
{
#if 0
  int th,pixmode,color,style,fg;
  int ibutton=-1,imask,iwait=FALSE;
  double x,y;
  BCG *Xgc = R->obj->Xgc;
  if ( Xgc == NULL ) return; 
  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  th = Xgc->graphic_engine->xget_thickness(Xgc);
  color= Xgc->graphic_engine->xget_pattern(Xgc);
  style = Xgc->graphic_engine->xget_dash(Xgc);
  fg    = Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_thickness(Xgc,1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,fg);
  x=pt[0];y=pt[1];
  while ( ibutton == -1 ) 
    {
      int ok_changed; 
      Cell *C;
      rect[0]= Min(pt[0],x);
      rect[1]= Max(pt[1],y);
      rect[2]= Abs(pt[0]-x);
      rect[3]= Abs(pt[1]-y);
      Xgc->graphic_engine->scale->xgetmouse(Xgc,"one",&ibutton,&imask,&x, &y,iwait,TRUE,TRUE,FALSE);
      /* hilite objects which are contained in bbox 
       */ 
      C = R->obj->children->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ )
	    {
	      double o_rect[4]; 
	      NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	      bf->get_rect(C->O,o_rect);
	      /* check if rect is inside rect */
	      if ( o_rect[0] >= rect[0] && o_rect[1] <= rect[1] 
		   && o_rect[0]+o_rect[2] <= rect[0]+rect[2] 
		   && o_rect[1]-o_rect[3] >= rect[1]-rect[3]  ) 
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
      nsp_diagram_draw(R);
      Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
    }
  Xgc->graphic_engine->xset_thickness(Xgc,th);
  Xgc->graphic_engine->xset_dash(Xgc,style);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  Xgc->graphic_engine->xinfo(Xgc," ");
  nsp_diagram_draw(R);
#endif 
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
      nsp_diagram_zoom_get_rectangle(R,pt,bbox);
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
	      nsp_diagram_draw(R);
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
  /* hide the moving object and its locked objects */
  bf->set_show(O,FALSE);
  if ( IsBlock(O)|| IsConnector(O) )  nsp_diagram_locks_set_show(R,O,FALSE);
  bf->set_hilited(O,TRUE);
  /* global draw of all but the moving object and linked objects 
   * we could here record the state to redraw faster 
   * since during the move this part will be kept constant.
   */
  nsp_diagram_draw(R);
  /*  */
  bf->set_show(O,TRUE);
  if ( IsBlock(O) || IsConnector(O) )  nsp_diagram_locks_set_show(R,O,TRUE);
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
  nsp_diagram_draw(R);
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
		  if ((obj = bf->full_copy(cloc->O))== NULLOBJ)  goto err;
		  if ( nsp_object_set_name(obj,"lel") == FAIL)goto err;

		}
	      else
		{
		  if ((obj=nsp_object_copy_with_name(cloc->O)) == NULLOBJ)  goto err;
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
      /* hide the moving object and its locked objects */
      bf->set_show(Obj,FALSE);
      if ( IsBlock(Obj)|| IsConnector(Obj) )  nsp_diagram_locks_set_show(R,Obj,FALSE);
      /* nsp_diagram_unhilite_objs(R,FALSE); */
      bf->set_hilited(Obj,TRUE);
      /* global draw of all but the moving object and linked objects 
       * we could here record the state to redraw faster 
       * since during the move this part will be kept constant.
       */
      nsp_diagram_draw(R);
      /* */
      bf->set_show(Obj,TRUE);
      if ( IsBlock(Obj) || IsConnector(Obj) )  nsp_diagram_locks_set_show(R,Obj,TRUE);
    }
  L= nsp_diagram_get_hilited_list(R->obj,FALSE);
  if ( L== NULLLIST) return OK;
  rep = nsp_diagram_move_list_obj(R,L, pt, -5,cp,MOVE, click );
  nsp_list_destroy(L);
  if ( rep == -100) return rep;
  nsp_diagram_draw(R);
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
      nsp_diagram_draw(R);
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
      nsp_diagram_draw(R);
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
      nsp_diagram_draw(R);
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
      nsp_diagram_draw(R);
    }
  return OK;
}

/**
 * nsp_diagram_locks_draw:
 * @R: a #NspDiagram 
 * @O: a #NspObject. 
 * 
 * calls the draw method on the objects which are connected
 * to object @O by lock points.
 **/

static void nsp_diagram_locks_draw(NspDiagram *R,NspObject *O)
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
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  bf1->draw(O1);
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

int nsp_diagram_move_obj(NspDiagram *D,NspObject *O,const double pt[2],int stop,int cp,move_action action)
{
  int ix,iy;
  int rep;
  nsp_figure *Fig = (((NspGraphic *) D)->obj->Fig);
  BCG *Xgc= window_list_search_new( Fig->id); 
  int wstop = 0, ibutton,imask, iwait=FALSE;
  double mpt[2],pt1[2]= {pt[0],pt[1]},ptwork[2];
  NspTypeGRint *bf = GR_INT(O->basetype->interface);

  if ( action == MOVE_CONTROL) 
    {
      bf->move_control_init(O,cp,ptwork);
    }
  /*
   * mpt is the mouse position, 
   * ptwork is the control point position 
   */
  
  while ( wstop==0 ) 
    {
      /* draw the frame 
       * we could here record and use a fixed part.
       */
      nsp_diagram_draw(D);
      if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
      /* get new mouse position 
       * XXX this code should be simplified not to search 
       * each time for an axes and changing the scales 
       * i.e nsp_get_point_axes should not be used all times 
       * or its returned argument could be used 
       */
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix,&iy,iwait,TRUE,TRUE,FALSE);
      nsp_get_point_axes(Xgc,ix,iy,mpt);
      if ( ibutton == -100 ) 
	{
	  return ibutton;
	}
      if ( ibutton == stop ) wstop= 1;
      Xgc->graphic_engine->xinfo(Xgc,"ibutton=%d",ibutton);
      /* clear block shape using redraw */
      /* if ( pixmap ) Xgc->graphic_engine->xset_show(); */
      /* move object */
      switch ( action ) 
	{
	case MOVE : 
	  rep= bf->translate(O,(pt1[0]= mpt[0] -pt1[0],pt1[1]=mpt[1] -pt1[1],pt1));
	  if ( rep == FAIL) wstop=1; /* quit untranslatable objects */
	  break;
	case MOVE_CONTROL :
	  bf->move_control(D,O,mpt,cp, ptwork);
	}
      /* update locks positions for objects locked to objects  */ 
      nsp_diagram_locks_update(D,O);
      pt1[0] = mpt[0];
      pt1[1] = mpt[1];
    }
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

/* utiliy function */ 


static int nsp_diagram_list_obj_action(NspDiagram *F,NspList *L,const double pt[2],list_move_action action)
{
  int rep = OK;
  Cell *C = L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  switch ( action )
	    {
	    case L_DRAW : 
	      if ( IsBlock(C->O)  || IsConnector(C->O))
		{
		  bf->draw(C->O);
		  if ( IsBlock(C->O)  || IsConnector(C->O))  nsp_diagram_locks_draw(F,C->O);
		}
	      break;
	    case L_TRANSLATE : 
	      rep= bf->translate(C->O,pt);
	      if ( rep == FAIL) return rep;
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
  BCG *Xgc= window_list_search_new(((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->id); 
  int wstop = 0, ibutton,imask, iwait=FALSE;
  double mpt[2],pt1[2]= {pt[0],pt[1]};

  if ( action == MOVE_CONTROL) 
    {
      /* nothing to do */
    }
  /*
   * mpt is the mouse position, 
   * ptwork is the control point position 
   */

  while ( wstop==0 ) 
    {
      /* draw the frame 
       * we could here record and use a fixed part.
       */
      nsp_diagram_draw(F);
      if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
      /* get new mouse position */
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix,&iy,iwait,TRUE,TRUE,FALSE);
      nsp_get_point_axes(Xgc,ix,iy,mpt);
      if ( ibutton == -100 ) 
	{
	  return ibutton;
	}
      if ( ibutton == stop ) wstop= 1;
      Xgc->graphic_engine->xinfo(Xgc,"ibutton=%d",ibutton);
      /* clear block shape using redraw */
      /* if ( pixmap ) Xgc->graphic_engine->xset_show(); */
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
      pt1[0] = mpt[0];
      pt1[1] = mpt[1];
    }
  /* was it a click ? */
  *click =  ( pt1[0]== pt[0] && pt1[1] == pt[1] ) ? TRUE : FALSE;
  /* we return the last activated button code */
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
  int ok = FALSE;
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
	      ok = TRUE;
	    }
	}
      C = C->next ;
    }
  if ( ok == TRUE && draw == TRUE )  nsp_diagram_draw(R);
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

NspObject * nsp_diagram_create_new_block(NspDiagram *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,10,10}, pt[]={0,100};
  NspBlock *B;
  NspGraphic *G;
  BCG *Xgc;
  /* unhilite all */
  nsp_diagram_unhilite_objs(F,FALSE);
  B = nsp_block_create("fe",NULL,rect,color,thickness,background,0,NULL,FALSE,TRUE,NULL,NULL);
  if ( B == NULLBLOCK) return NULLOBJ;
  B->obj->hilited = TRUE;
  G = (NspGraphic *) B;
  G->type->link_figure(G,((NspGraphic *) F)->obj->Fig);
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return NULLOBJ;
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  Xgc= window_list_search_new(((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->id); 
  
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
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
  if ( pixmap ) F->obj->Xgc->graphic_engine->xset_show(F->obj->Xgc);
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
  B=nsp_connector_create("fe",NULL,rect,color,thickness,background,l,
			 FALSE,TRUE,NULL);
  if ( B == NULL) return NULLOBJ;
  B->obj->hilited = TRUE;
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return NULLOBJ;
  G = (NspGraphic *) B;
  G->type->link_figure(G,((NspGraphic *) F)->obj->Fig);
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  Xgc= window_list_search_new(((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->id); 
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
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
  Xgc= window_list_search_new(((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->id); 

  B=rect_create("fe",Xgc,rect,color,thickness,background,NULL);
  if ( B == NULL) return FAIL;
  if (nsp_list_end_insert(F->obj->children,(NspObject  *) B) == FAIL) return FAIL;
  rep= nsp_diagram_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return FAIL;
  /* XXXX block_draw(B); */
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
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
  BCG *Xgc= window_list_search_new(((nsp_figure*) (((NspGraphic *) F)->obj->Fig))->id); 
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
  L->obj->hilited = TRUE;
  L->obj->poly->R[0]=mpt[0];
  L->obj->poly->R[1]=mpt[0];
  while ( wstop==0 ) 
    {
      NspGraphic *G= (NspGraphic *) L;
      nsp_diagram_draw(F);
      /* draw the link */
      G->type->draw(Xgc,G,NULL);
      if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
      /* get new mouse position */
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ix,&iy,iwait,TRUE,TRUE,FALSE);
      nsp_get_point_axes(Xgc,ix,iy,mpt);
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
      /* clear link shape using redraw */
      /* bf->draw(L); */
      
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
	  rep = nsp_diagram_select_lock(F,mpt, &Ob, &cp1,&lock_c) ;
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
    }
  /* insert link in frame 
   * at the start 
   */
  if (nsp_list_insert(F->obj->children,(NspObject  *) L,0) == FAIL) return NULLOBJ;
  /* check if first and last points are locked 
   * if true update locks 
   */
  G = (NspGraphic *) L;
  G->type->link_figure(G,((NspGraphic *) F)->obj->Fig);

  mpt[0]=L->obj->poly->R[0];
  mpt[1]=L->obj->poly->R[L->obj->poly->m];
  link_lock_update(F,L,0,mpt);
  mpt[0]=L->obj->poly->R[L->obj->poly->m-1];
  mpt[1]=L->obj->poly->R[2*L->obj->poly->m-1];
  link_lock_update(F,L,1,mpt);
  link_check(F,L);
  nsp_diagram_draw(F);
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
  return NSP_OBJECT(L);
}


/**
 * nsp_diagram_full_copy:
 * @F: a #NspDiagram
 * 
 * Make a full copy of a @F. Since @F contains 
 * a list of objects which are themselves objects with 
 * references, the full copy must be performed on these
 * objects and cross references are to be updates.
 * 
 * Returns: a new #NspDiagram or %NULLDIAGRAM
 **/

static NspList * nsp_diagram_list_full_copy(NspList *L,int hilited_only);

NspDiagram *zz_nsp_diagram_full_copy( NspDiagram *F)
{
  NspDiagram *H  = nsp_diagram_create_void(NVOID,NULL);
  if ( H ==  NULLDIAGRAM ) return NULLDIAGRAM;
  if ((H->obj = nspdiagram_full_copy(F->obj,FALSE))  == NULL) return NULLDIAGRAM;
  return H;
}

/**
 * nsp_diagram_hilited_full_copy:
 * @F: a #NspDiagram
 * 
 * Make a full copy of a @F but only for hilited objects. 
 * Since @F contains a list of objects which are themselves objects with 
 * references, the full copy must be performed on these
 * objects and cross references are to be updates.
 * 
 * Returns: a new #NspDiagram or %NULLDIAGRAM
 **/

NspDiagram *nsp_diagram_hilited_full_copy( NspDiagram *F)
{
  NspDiagram *H  = nsp_diagram_create_void(NVOID,NULL);
  if ( H ==  NULLDIAGRAM ) return NULLDIAGRAM;
  if ((H->obj = nspdiagram_full_copy(F->obj,TRUE))  == NULL) return NULLDIAGRAM;
  return H;
}

/**
 * nspdiagram_full_copy:
 * @gf: a #nsp_diagram 
 * @hilited_only: %TRUE or %FALSE
 * 
 * Make a full copy of a @gf. Since @gf contains 
 * a list of objects which are themselves objects with 
 * references, the full copy must be performed on these
 * objects and cross references are to be updates.
 * If @hilited_only is %TRUE, only hilited objects are copied.
 * 
 * Returns: a new #nsp_diagram or %NULL 
 **/

nsp_diagram *nspdiagram_full_copy(nsp_diagram *gf,int hilited_only)
{
  /* int i; */
  nsp_diagram *loc; 
  if ((loc = malloc(sizeof(nsp_diagram))) == NULL) return NULL;
  loc->ref_count=1;
  if ((loc->children = nsp_diagram_list_full_copy(gf->children, hilited_only))== NULL) 
    return NULL;
  /* restore lost pointers 
   */
  nspdiagram_set_frame_field(loc);
  /*
   * restore interconnections 
   */
  nspdiagram_recompute_pointers(loc);
  /* copy scales 
   *
   */
  /* 
     for ( i=0; i < 4 ; i++) loc->r[i]=gf->r[i];
     for ( i=0; i < 4 ; i++) loc->scale[i]=gf->scale[i];
     loc->Xgc = gf->Xgc;
     loc->top = gf->top;
  */
  return loc;
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
	      if ( (obj = bf->full_copy(cloc->O)) == NULLOBJ )  goto err;
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



#line 3035 "diagram.c"
