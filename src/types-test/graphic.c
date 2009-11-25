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





#line 20 "codegen/graphic.override"
#include <gdk/gdk.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h>
#include <nsp/axes.h>
#include <nsp/objs3d.h>
#include "../interp/Eval.h"

#line 36 "graphic.c"

/* ----------- NspGraphic ----------- */


#define  NspGraphic_Private 
#include <nsp/object.h>
#include <nsp/graphic.h>
#include <nsp/interf.h>

/* 
 * NspGraphic inherits from Object 
 */

int nsp_type_graphic_id=0;
NspTypeGraphic *nsp_type_graphic=NULL;

/*
 * Type object for NspGraphic 
 * all the instance of NspTypeGraphic share the same id. 
 * nsp_type_graphic: is an instance of NspTypeGraphic 
 *    used for objects of NspGraphic type (i.e built with new_graphic) 
 * other instances are used for derived classes 
 */
NspTypeGraphic *new_type_graphic(type_mode mode)
{
  NspTypeGraphic *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_graphic != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_graphic;
    }
  if (( type =  malloc(sizeof(NspTypeGraphic))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = graphic_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = graphic_get_methods;
  type->new = (new_func *) new_graphic;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for graphic */ 

  top->pr = (print_func *) nsp_graphic_print;
  top->dealloc = (dealloc_func *) nsp_graphic_destroy;
  top->copy  =  (copy_func *) nsp_graphic_copy;
  top->size  = (size_func *) nsp_graphic_size;
  top->s_type =  (s_type_func *) nsp_graphic_type_as_string;
  top->sh_type = (sh_type_func *) nsp_graphic_type_short_string;
  top->info = (info_func *) nsp_graphic_info;
  /* top->is_true = (is_true_func  *) nsp_graphic_is_true; */
  /* top->loop =(loop_func *) nsp_graphic_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_graphic_object;
  top->eq  = (eq_func *) nsp_graphic_eq;
  top->neq  = (eq_func *) nsp_graphic_neq;
  top->save  = (save_func *) nsp_graphic_xdr_save;
  top->load  = (load_func *) nsp_graphic_xdr_load;
  top->create = (create_func*) int_graphic_create;
  top->latex = (print_func *) nsp_graphic_latex;
  top->full_copy = (copy_func *) nsp_graphic_full_copy;

  /* specific methods for graphic */

  type->init = (init_func *) init_graphic;

#line 72 "codegen/graphic.override"

  /* inserted verbatim in the type definition 
   * here we define the default values for graphic methods 
   * these methods of class Graphic are to be re-defined by subclasses 
   * if necessary.
   */
  type->draw = NULL;
  type->translate = NULL;
  type->rotate = NULL;
  type->scale = NULL;
  type->bounds = NULL;
  type->link_figure = nsp_graphic_link_figure;
  type->unlink_figure = nsp_graphic_unlink_figure;
  type->children = NULL;
  type->zmean = NULL; 
  type->n_faces = NULL;
  type->invalidate = nsp_graphic_invalidate;

#line 127 "graphic.c"
  /* 
   * NspGraphic interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_graphic_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGraphic called nsp_type_graphic
       */
      type->id =  nsp_type_graphic_id = nsp_new_type_id();
      nsp_type_graphic = type;
      if ( nsp_register_type(nsp_type_graphic) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_graphic(mode);
    }
  else 
    {
      type->id = nsp_type_graphic_id;
      return type;
    }
}

/*
 * initialize NspGraphic instances 
 * locally and by calling initializer on parent class 
 */

static int init_graphic(NspGraphic *Obj,NspTypeGraphic *type)
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
 * new instance of NspGraphic 
 */

NspGraphic *new_graphic() 
{
  NspGraphic *loc;
  /* type must exists */
  nsp_type_graphic = new_type_graphic(T_BASE);
  if ( (loc = malloc(sizeof(NspGraphic)))== NULLGRAPHIC) return loc;
  /* initialize object */
  if ( init_graphic(loc,nsp_type_graphic) == FAIL) return NULLGRAPHIC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGraphic 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_graphic_size(NspGraphic *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char graphic_type_name[]="Graphic";
static char graphic_short_type_name[]="graphic";

static char *nsp_graphic_type_as_string(void)
{
  return(graphic_type_name);
}

static char *nsp_graphic_type_short_string(NspObject *v)
{
  return(graphic_short_type_name);
}

/*
 * A == B 
 */

static int nsp_graphic_eq(NspGraphic *A, NspObject *B)
{
  NspGraphic *loc = (NspGraphic *) B;
  if ( check_cast(B,nsp_type_graphic_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->hilited != loc->obj->hilited) return FALSE;
  if ( A->obj->show != loc->obj->show) return FALSE;
  if ( A->obj->Fig != loc->obj->Fig) return FALSE;
  if ( A->obj->Axe != loc->obj->Axe) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_graphic_neq(NspGraphic *A, NspObject *B)
{
  return ( nsp_graphic_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_graphic_xdr_save(XDR *xdrs, NspGraphic *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_graphic)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->hilited) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->show) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGraphic  *nsp_graphic_xdr_load_partial(XDR *xdrs, NspGraphic *M)
{
  M->obj->ref_count=1;
  if (nsp_xdr_load_i(xdrs, &M->obj->hilited) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->show) == FAIL) return NULL;
 return M;
}

static NspGraphic  *nsp_graphic_xdr_load(XDR *xdrs)
{
  NspGraphic *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRAPHIC;
  if ((H  = nsp_graphic_create_void(name,(NspTypeBase *) nsp_type_graphic))== NULLGRAPHIC) return H;
  if ( nsp_graphic_create_partial(H) == FAIL) return NULLGRAPHIC;
  if ((H  = nsp_graphic_xdr_load_partial(xdrs,H))== NULLGRAPHIC) return H;
  if ( nsp_graphic_check_values(H) == FAIL) return NULLGRAPHIC;
  return H;
}

/*
 * delete 
 */

void nsp_graphic_destroy_partial(NspGraphic *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_graphic_destroy(NspGraphic *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_graphic_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_graphic_info(NspGraphic *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRAPHIC) 
    {
      Sciprintf("Null Pointer NspGraphic \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_graphic_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_graphic_print(NspGraphic *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRAPHIC) 
    {
      Sciprintf("Null Pointer NspGraphic \n");
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
          nsp_graphic_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_graphic_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"hilited	= %s\n", ( M->obj->hilited == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"show	= %s\n", ( M->obj->show == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"Fig=%xl\n",M->obj->Fig);
  Sciprintf1(indent+2,"Axe=%xl\n",M->obj->Axe);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_graphic_latex(NspGraphic *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_graphic_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"hilited	= %s\n", ( M->obj->hilited == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"show	= %s\n", ( M->obj->show == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"Fig=%xl\n",M->obj->Fig);
  Sciprintf1(indent+2,"Axe=%xl\n",M->obj->Axe);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGraphic objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGraphic   *nsp_graphic_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_graphic_id) == TRUE ) return ((NspGraphic *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_graphic));
  return NULL;
}

int IsGraphicObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_graphic_id);
}

int IsGraphic(NspObject *O)
{
  return nsp_object_type(O,nsp_type_graphic_id);
}

NspGraphic  *GetGraphicCopy(Stack stack, int i)
{
  if (  GetGraphic(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGraphic  *GetGraphic(Stack stack, int i)
{
  NspGraphic *M;
  if (( M = nsp_graphic_object(NthObj(i))) == NULLGRAPHIC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGraphic instance 
 *-----------------------------------------------------*/

static NspGraphic *nsp_graphic_create_void(char *name,NspTypeBase *type)
{
 NspGraphic *H  = (type == NULL) ? new_graphic() : type->new();
 if ( H ==  NULLGRAPHIC)
  {
   Sciprintf("No more memory\n");
   return NULLGRAPHIC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRAPHIC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_graphic_create_partial(NspGraphic *H)
{
  if((H->obj = calloc(1,sizeof(nsp_graphic)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->hilited = FALSE;
  H->obj->show = TRUE;
  H->obj->Fig = NULL;
  H->obj->Axe = NULL;
  return OK;
}

int nsp_graphic_check_values(NspGraphic *H)
{
  return OK;
}

NspGraphic *nsp_graphic_create(char *name,gboolean hilited,gboolean show,void* Fig,void* Axe,NspTypeBase *type)
{
  NspGraphic *H  = nsp_graphic_create_void(name,type);
  if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_create_partial(H) == FAIL) return NULLGRAPHIC;
  H->obj->hilited=hilited;
  H->obj->show=show;
  H->obj->Fig = Fig;
  H->obj->Axe = Axe;
  if ( nsp_graphic_check_values(H) == FAIL) return NULLGRAPHIC;
  return H;
}


NspGraphic *nsp_graphic_create_default(char *name)
{
 NspGraphic *H  = nsp_graphic_create_void(name,NULL);
 if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_create_partial(H) == FAIL) return NULLGRAPHIC;
 if ( nsp_graphic_check_values(H) == FAIL) return NULLGRAPHIC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGraphic *nsp_graphic_copy_partial(NspGraphic *H,NspGraphic *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGraphic *nsp_graphic_copy(NspGraphic *self)
{
  NspGraphic *H  =nsp_graphic_create_void(NVOID,(NspTypeBase *) nsp_type_graphic);
  if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_copy_partial(H,self)== NULL) return NULLGRAPHIC;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGraphic *nsp_graphic_full_copy_partial(NspGraphic *H,NspGraphic *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_graphic))) == NULL) return NULLGRAPHIC;
  H->obj->ref_count=1;
  H->obj->hilited=self->obj->hilited;
  H->obj->show=self->obj->show;
  H->obj->Fig = self->obj->Fig;
  H->obj->Axe = self->obj->Axe;
  return H;
}

NspGraphic *nsp_graphic_full_copy(NspGraphic *self)
{
  NspGraphic *H  =nsp_graphic_create_void(NVOID,(NspTypeBase *) nsp_type_graphic);
  if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_full_copy_partial(H,self)== NULL) return NULLGRAPHIC;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGraphic
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_graphic_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGraphic *H;
  CheckStdRhs(0,0);
  /* want to be sure that type graphic is initialized */
  nsp_type_graphic = new_type_graphic(T_BASE);
  if(( H = nsp_graphic_create_void(NVOID,(NspTypeBase *) nsp_type_graphic)) == NULLGRAPHIC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_graphic_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_graphic_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 92 "codegen/graphic.override"
/* take care that the name to give for override is the c-name of 
 * the method 
 */
static int _wrap_graphic_translate(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,t_end};
  NspMatrix *tr;
  if ( GetArgs(stack,rhs,opt,T,&tr) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,tr,2);
  self->type->translate(self, tr->R);
  return 0;
}

#line 544 "graphic.c"


#line 107 "codegen/graphic.override"
static int _wrap_graphic_scale(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,t_end};
  NspMatrix *sc;

  if ( GetArgs(stack,rhs,opt,T,&sc) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,sc,2);
  self->type->scale(self, sc->R);
  return 0;
}

#line 559 "graphic.c"


#line 120 "codegen/graphic.override"
static int _wrap_graphic_rotate(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,t_end};
  NspMatrix *R;
  if ( GetArgs(stack,rhs,opt,T,&R) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,R,2);
  self->type->rotate(self, R->R);
  return 0;
}

#line 573 "graphic.c"


#line 142 "codegen/graphic.override"
static int _wrap_graphic_unlink(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,0);
  self->type->unlink_figure(self, self->obj->Fig);
  return 0;
}

#line 585 "graphic.c"


#line 132 "codegen/graphic.override"
static int _wrap_graphic_invalidate(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,0);
  self->type->invalidate(self);
  return 0;
}

#line 597 "graphic.c"


static NspMethods graphic_methods[] = {
  {"translate",(nsp_method *) _wrap_graphic_translate},
  {"scale",(nsp_method *) _wrap_graphic_scale},
  {"rotate",(nsp_method *) _wrap_graphic_rotate},
  {"unlink",(nsp_method *) _wrap_graphic_unlink},
  {"invalidate",(nsp_method *) _wrap_graphic_invalidate},
  { NULL, NULL}
};

static NspMethods *graphic_get_methods(void) { return graphic_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_graphic_get_hilited(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGraphic *) self)->obj->hilited;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_graphic_set_hilited(void *self,const char *attr, NspObject *O)
{
  int hilited;

  if ( BoolScalar(O,&hilited) == FAIL) return FAIL;
  ((NspGraphic *) self)->obj->hilited= hilited;
  return OK;
}

static NspObject *_wrap_graphic_get_show(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGraphic *) self)->obj->show;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_graphic_set_show(void *self,const char *attr, NspObject *O)
{
  int show;

  if ( BoolScalar(O,&show) == FAIL) return FAIL;
  ((NspGraphic *) self)->obj->show= show;
  return OK;
}

static AttrTab graphic_attrs[] = {
  { "hilited", (attr_get_function *)_wrap_graphic_get_hilited, (attr_set_function *)_wrap_graphic_set_hilited,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "show", (attr_get_function *)_wrap_graphic_get_show, (attr_set_function *)_wrap_graphic_set_show,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Graphic_func[]={
  { "graphic_create", int_graphic_create},
  { NULL, NULL}
};

/* call ith function in the Graphic interface */

int Graphic_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Graphic_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Graphic_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Graphic_func[i].name;
  *f = Graphic_func[i].fonc;
}

#line 152 "codegen/graphic.override"

/* verbatim at the end */
/* default methods in graphic */

/**
 * nsp_graphic_link_figure:
 * @G: a #NspGraphic object 
 * @F: a void pointer which is to be a #nsp_figure 
 * 
 * set the Fig field of @G.
 * 
 **/

void nsp_graphic_link_figure(NspGraphic *G,void *F, void *A)
{
  nsp_figure *Fi = F;
  G->obj->Fig = Fi;
  G->obj->Axe = A;
}

/**
 * nsp_graphic_unlink_figure:
 * @G: a #NspGraphic object 
 * @F: a void pointer which is to be a #nsp_figure 
 *
 * if @G Fig field is equal to @F then Fig field 
 * if set to %NULL.
 * 
 **/

void nsp_graphic_unlink_figure(NspGraphic *G, void *F)
{
  /* NspFigure *Fi = F;*/
  if ( G->obj->Fig == F ) 
    {
      G->obj->Fig = NULL ;
      G->obj->Axe = NULL;	
    }
}

/* interface shared by all graphic objects */

/*
 * Extract requested child of a graphicobject 
 * the returned object is not copied.
 */

static int int_nspgraphic_extract_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Elts;
  int i, nret, L_name;
  NspObject *O;
  NspList *L;
  NspGraphic *Gr;
  
  CheckRhs (2,2);
  if ((Gr=GetGraphic(stack,1)) == NULLGRAPHIC) return RET_BUG;
  if ((Elts = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( Gr->type->children == NULL) return RET_BUG;
  if ((L = Gr->type->children(Gr)) == NULLLIST) return RET_BUG;
  
  nret = Elts->mn;
  L_name = Ocheckname(L,NVOID);

  for ( i = 0 ; i < nret ; i++ )
    {
      if ( (O=nsp_list_get_element(L,((int) Elts->R[i]))) ==  NULLOBJ )
	return RET_BUG;  /* error message done in nsp_list_get_element */
      /* If NspList has no name or list element has no name we must copy */
      if ( L_name || Ocheckname(O,NVOID) ) 
	if ( (O=nsp_object_copy(O)) == NULLOBJ )  return RET_BUG;
      NthObj(rhs+i+1) = O;
    }

  nsp_void_object_destroy(&NthObj(1));
  nsp_void_object_destroy(&NthObj(2));
  for ( i = 0 ; i < nret ; i++) 
    {
      NthObj(i+1)= NthObj(i+rhs+1);
      NSP_OBJECT(NthObj(i+1))->ret_pos = i+1;
      NthObj(i+rhs+1)= NULLOBJ;
    }
  return nret;
}

/* extract a field  grobject('field') <=> grobject.field
 * or grobject.get['field']
 *
 */

static int int_nspgraphic_extract_s(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Gr;
  NspObject *Ret;
  NspSMatrix *S;
  int i,j,count=0;
  CheckRhs(2,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  if ((Gr= (NspObject *) GetGraphic(stack,1)) == NULLOBJ) return RET_BUG;
  for ( j = 2 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  Ret = nsp_get_attribute_util(Gr,Gr->basetype,S->S[i]);
	  if ( Ret == NULL) return RET_BUG;
	  NthObj(rhs+ ++count) = Ret ;
	  NSP_OBJECT(Ret)->ret_pos = count;
	  if (count == lhs) break;
	}
      if (count == lhs) break;
    }
  return count;
}

/* extraction part when argument is a list  */ 

static int int_nspgraphic_extract_l(Stack stack, int rhs, int opt, int lhs)
{
  char name[NAME_MAXL];
  int rep,n ;
  if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
  if ( rep == 3 ) 
    {
      /* last extraction : here O can be anything */ 
      nsp_build_funcname("extractelts",&stack,stack.first+1,1,name);
      if ((n=nsp_eval_func(NULLOBJ,name,2,stack,stack.first+1,2,0,1)) < 0) 
	{
	  return RET_BUG;
	}
    }
  nsp_void_object_destroy(&NthObj(1));
  NSP_OBJECT(NthObj(2))->ret_pos = 1;
  return 1;
}

/* extraction part */

int int_nspgraphic_extract(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(2,2);
  if (IsMatObj(stack,2)) 
    {
      return int_nspgraphic_extract_m(stack,rhs,opt,lhs);
    }
  else if (IsSMatObj(stack,2)) 
    {
      return int_nspgraphic_extract_s(stack,rhs,opt,lhs);
    }
  else if ( IsListObj(stack,2) ) 
    {
      return int_nspgraphic_extract_l(stack,rhs,opt,lhs);
    }
  else 
    {
      Scierror("Error: Wrong type for argument in list extraction int or list required\n");
      return RET_BUG;
    }
  return 1;
}

/*
 * set an attribute value. 
 */

int int_graphic_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Gr;
  NspObject *Obj;
  const char *name;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((Gr= (NspObject *) GetGraphic(stack,1)) == NULLOBJ) return RET_BUG;
  if ( IsSMatObj(stack,2) )
    {
      if ((name = GetString(stack,2)) == (char*)0) return RET_BUG;  
    }
  else 
    {
      Scierror("%s: indice for extraction should be a string\n",NspFname(stack));
      return RET_BUG;
    }
  if ((  Obj = nsp_get_object(stack,3)) == NULLOBJ ) return RET_BUG;
  if ( nsp_set_attribute_util(Gr,Gr->basetype,name,Obj) == FAIL) return RET_BUG;
  NSP_OBJECT(Gr)->ret_pos = 1;
  return 1;
}

/* invalidate the drawing region associated to a graphic object. 
 * This function is to be called when the 
 * graphic object has changed and need to be drawn.
 * 
 */

void nsp_graphic_invalidate(NspGraphic *G)
{
  double bounds[4];/* xmin,ymin,xmax, ymax */
  nsp_figure *F = G->obj->Fig;
  nsp_axes *A = G->obj->Axe;
  BCG *Xgc;
  if ( F == NULL ) return ;
  if ((Xgc= F->Xgc) == NULL) return ;
  if ( F->draw_now== FALSE) return;
  if ( G->obj->show == FALSE ) return;
  if ( G->type->bounds(G,bounds)== TRUE) 
    {
      gint rect[4]; /* like a GdkRectangle */
      int xmin,xmax,ymin,ymax;
      scale_f2i(&A->scale,bounds,bounds+1,&xmin,&ymin,1);
      scale_f2i(&A->scale,bounds+2,bounds+3,&xmax,&ymax,1);
      rect[0]=xmin-10;rect[1]=ymax-10;rect[2]=xmax-xmin+20;rect[3]=ymin-ymax+20;
      /* fprintf(stderr,"invalidate [%d,%d,%d,%d]\n",rect[0],rect[1],rect[2],rect[3]);*/
      Xgc->graphic_engine->invalidate(Xgc,rect);
    }
  else
    {
      NspObject *obj = nsp_check_for_axes_or_objs3d_from_pointer(F,G->obj->Axe);
      if ( obj != NULL)
	{
	  if (IsAxes(obj) )
	    {
	      nsp_axes_invalidate((NspGraphic *) obj);
	    }
	  else 
	    {
	      nsp_objs3d_invalidate((NspGraphic *) obj);
	    }
	}
      else
	{
	  Xgc->graphic_engine->invalidate(Xgc,NULL);
	}
    }
}

/**
 * nsp_graphic_intersect_rectangle:
 * @G: 
 * @rect: 
 * 
 * 
 * 
 * Returns: 
 **/
 
int nsp_graphic_intersect_rectangle(NspGraphic *G,const GdkRectangle *rect)
{
  nsp_axes *axe = ((NspGraphic *) G)->obj->Axe;
  GdkRectangle r1, r2 = *rect;
  int xmin,ymin,xmax,ymax;
  double bounds[4];
  if ( rect == NULL ) return TRUE;
  if ( G->type->bounds(G,bounds) == FALSE ) return TRUE;
  scale_f2i(&axe->scale,bounds,bounds+1,&xmin,&ymin,1);
  scale_f2i(&axe->scale,bounds+2,bounds+3,&xmax,&ymax,1);
  r1.x = xmin-10; 
  r1.y = ymax-10;
  r1.width = xmax - xmin +20;
  r1.height = ymin - ymax +20 ;
  return  gdk_rectangle_intersect(&r2,&r1,&r1);
}


#line 953 "graphic.c"
