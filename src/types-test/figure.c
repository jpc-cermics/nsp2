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





#line 62 "codegen/figure.override"
#include <gdk/gdk.h>
#include <nsp/figure.h>
#include <nsp/axes.h>
#include <nsp/objs3d.h>
#include <nsp/compound.h>

#line 35 "figure.c"

/* ----------- NspFigure ----------- */


#define  NspFigure_Private 
#include <nsp/object.h>
#include <nsp/figure.h>
#include <nsp/interf.h>

/* 
 * NspFigure inherits from Graphic 
 */

int nsp_type_figure_id=0;
NspTypeFigure *nsp_type_figure=NULL;

/*
 * Type object for NspFigure 
 * all the instance of NspTypeFigure share the same id. 
 * nsp_type_figure: is an instance of NspTypeFigure 
 *    used for objects of NspFigure type (i.e built with new_figure) 
 * other instances are used for derived classes 
 */
NspTypeFigure *new_type_figure(type_mode mode)
{
  NspTypeFigure *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_figure != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_figure;
    }
  if (( type =  malloc(sizeof(NspTypeFigure))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = figure_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = figure_get_methods;
  type->new = (new_func *) new_figure;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for figure */ 

  top->pr = (print_func *) nsp_figure_print;
  top->dealloc = (dealloc_func *) nsp_figure_destroy;
  top->copy  =  (copy_func *) nsp_figure_copy;
  top->size  = (size_func *) nsp_figure_size;
  top->s_type =  (s_type_func *) nsp_figure_type_as_string;
  top->sh_type = (sh_type_func *) nsp_figure_type_short_string;
  top->info = (info_func *) nsp_figure_info;
  /* top->is_true = (is_true_func  *) nsp_figure_is_true; */
  /* top->loop =(loop_func *) nsp_figure_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_figure_object;
  top->eq  = (eq_func *) nsp_figure_eq;
  top->neq  = (eq_func *) nsp_figure_neq;
  top->save  = (save_func *) nsp_figure_xdr_save;
  top->load  = (load_func *) nsp_figure_xdr_load;
  top->create = (create_func*) int_figure_create;
  top->latex = (print_func *) nsp_figure_latex;
  top->full_copy = (copy_func *) nsp_figure_full_copy;

  /* specific methods for figure */

  type->init = (init_func *) init_figure;

#line 75 "codegen/figure.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_figure;
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_figure_children ;
  ((NspTypeGraphic *) type->surtype)->invalidate = nsp_figure_invalidate;

#line 113 "figure.c"
  /* 
   * NspFigure interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_figure_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeFigure called nsp_type_figure
       */
      type->id =  nsp_type_figure_id = nsp_new_type_id();
      nsp_type_figure = type;
      if ( nsp_register_type(nsp_type_figure) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_figure(mode);
    }
  else 
    {
      type->id = nsp_type_figure_id;
      return type;
    }
}

/*
 * initialize NspFigure instances 
 * locally and by calling initializer on parent class 
 */

static int init_figure(NspFigure *Obj,NspTypeFigure *type)
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
 * new instance of NspFigure 
 */

NspFigure *new_figure() 
{
  NspFigure *loc;
  /* type must exists */
  nsp_type_figure = new_type_figure(T_BASE);
  if ( (loc = malloc(sizeof(NspFigure)))== NULLFIGURE) return loc;
  /* initialize object */
  if ( init_figure(loc,nsp_type_figure) == FAIL) return NULLFIGURE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspFigure 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_figure_size(NspFigure *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char figure_type_name[]="Figure";
static char figure_short_type_name[]="figure";

static char *nsp_figure_type_as_string(void)
{
  return(figure_type_name);
}

static char *nsp_figure_type_short_string(NspObject *v)
{
  return(figure_short_type_name);
}

/*
 * A == B 
 */

static int nsp_figure_eq(NspFigure *A, NspObject *B)
{
  NspFigure *loc = (NspFigure *) B;
  if ( check_cast(B,nsp_type_figure_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( strcmp(A->obj->fname,loc->obj->fname) != 0) return FALSE;
  if ( strcmp(A->obj->driver,loc->obj->driver) != 0) return FALSE;
  if ( A->obj->id != loc->obj->id) return FALSE;
  if ( NSP_OBJECT(A->obj->dims)->type->eq(A->obj->dims,loc->obj->dims) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->viewport_dims)->type->eq(A->obj->viewport_dims,loc->obj->viewport_dims) == FALSE ) return FALSE;
  if ( A->obj->wresize != loc->obj->wresize) return FALSE;
  if ( NSP_OBJECT(A->obj->position)->type->eq(A->obj->position,loc->obj->position) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  if ( A->obj->draw_now != loc->obj->draw_now) return FALSE;
if ( NSP_OBJECT(A->obj->gc)->type->eq(A->obj->gc,loc->obj->gc) == FALSE ) return FALSE;
  if ( A->obj->Xgc != loc->obj->Xgc) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_figure_neq(NspFigure *A, NspObject *B)
{
  return ( nsp_figure_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_figure_xdr_save(XDR *xdrs, NspFigure *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_figure)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->fname) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->driver) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->id) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->dims)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->viewport_dims)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->wresize) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->position)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->gc)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspFigure  *nsp_figure_xdr_load_partial(XDR *xdrs, NspFigure *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->fname)) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->driver)) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->id) == FAIL) return NULL;
  if ((M->obj->dims =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->viewport_dims =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->wresize) == FAIL) return NULL;
  if ((M->obj->position =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if ((M->obj->gc= (NspFigureData *) nsp_object_xdr_load(xdrs))== NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspFigure  *nsp_figure_xdr_load(XDR *xdrs)
{
  NspFigure *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLFIGURE;
  if ((H  = nsp_figure_create_void(name,(NspTypeBase *) nsp_type_figure))== NULLFIGURE) return H;
  if ( nsp_figure_create_partial(H) == FAIL) return NULLFIGURE;
  if ((H  = nsp_figure_xdr_load_partial(xdrs,H))== NULLFIGURE) return H;
  if ( nsp_figure_check_values(H) == FAIL) return NULLFIGURE;
#line 91 "codegen/figure.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
  nsp_figure_children_link_figure(H);
  nsp_figure_initialize_gc(H);
#line 295 "figure.c"
  return H;
}

/*
 * delete 
 */

void nsp_figure_destroy_partial(NspFigure *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 86 "codegen/figure.override"
  /* inserted verbatim at the begining of destroy */
  nsp_figure_children_unlink_figure(H);

#line 313 "figure.c"
  nsp_string_destroy(&(H->obj->fname));
  nsp_string_destroy(&(H->obj->driver));
    if ( H->obj->dims != NULL ) 
      nsp_matrix_destroy(H->obj->dims);
    if ( H->obj->viewport_dims != NULL ) 
      nsp_matrix_destroy(H->obj->viewport_dims);
    if ( H->obj->position != NULL ) 
      nsp_matrix_destroy(H->obj->position);
    if ( H->obj->children != NULL ) 
      nsp_list_destroy(H->obj->children);
    if (H->obj->gc != NULL)
    nsp_object_destroy((NspObject **)&H->obj->gc);
    FREE(H->obj);
   }
}

void nsp_figure_destroy(NspFigure *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_figure_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_figure_info(NspFigure *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLFIGURE) 
    {
      Sciprintf("Null Pointer NspFigure \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_figure_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_figure_print(NspFigure *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLFIGURE) 
    {
      Sciprintf("Null Pointer NspFigure \n");
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
          nsp_figure_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_figure_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"driver=%s\n",M->obj->driver);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  if ( M->obj->dims != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->dims),indent+2,"dims",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->viewport_dims != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->viewport_dims),indent+2,"viewport_dims",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"wresize	= %s\n", ( M->obj->wresize == TRUE) ? "T" : "F" );
  if ( M->obj->position != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->position),indent+2,"position",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"draw_now	= %s\n", ( M->obj->draw_now == TRUE) ? "T" : "F" );
  if ( M->obj->gc != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->gc),indent+2,"gc",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Xgc=%xl\n",M->obj->Xgc);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_figure_latex(NspFigure *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_figure_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"driver=%s\n",M->obj->driver);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  if ( M->obj->dims != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->dims),indent+2,"dims",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->viewport_dims != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->viewport_dims),indent+2,"viewport_dims",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"wresize	= %s\n", ( M->obj->wresize == TRUE) ? "T" : "F" );
  if ( M->obj->position != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->position),indent+2,"position",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"draw_now	= %s\n", ( M->obj->draw_now == TRUE) ? "T" : "F" );
  if ( M->obj->gc != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->gc),indent+2,"gc",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Xgc=%xl\n",M->obj->Xgc);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspFigure objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspFigure   *nsp_figure_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_figure_id) == TRUE ) return ((NspFigure *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_figure));
  return NULL;
}

int IsFigureObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_figure_id);
}

int IsFigure(NspObject *O)
{
  return nsp_object_type(O,nsp_type_figure_id);
}

NspFigure  *GetFigureCopy(Stack stack, int i)
{
  if (  GetFigure(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFigure  *GetFigure(Stack stack, int i)
{
  NspFigure *M;
  if (( M = nsp_figure_object(NthObj(i))) == NULLFIGURE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspFigure instance 
 *-----------------------------------------------------*/

static NspFigure *nsp_figure_create_void(char *name,NspTypeBase *type)
{
 NspFigure *H  = (type == NULL) ? new_figure() : type->new();
 if ( H ==  NULLFIGURE)
  {
   Sciprintf("No more memory\n");
   return NULLFIGURE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLFIGURE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_figure_create_partial(NspFigure *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_figure)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->fname = NULL;
  H->obj->driver = NULL;
  H->obj->id = 0;
  H->obj->dims = NULLMAT;
  H->obj->viewport_dims = NULLMAT;
  H->obj->wresize = TRUE;
  H->obj->position = NULLMAT;
  H->obj->children = NULLLIST;
  H->obj->draw_now = TRUE;
  H->obj->gc = NULL;
  H->obj->Xgc = NULL;
  return OK;
}

int nsp_figure_check_values(NspFigure *H)
{
  if ( H->obj->fname == NULL) 
    {
     if (( H->obj->fname = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->driver == NULL) 
    {
     if (( H->obj->driver = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->dims == NULLMAT) 
    {
       if (( H->obj->dims = nsp_matrix_create("dims",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->viewport_dims == NULLMAT) 
    {
       if (( H->obj->viewport_dims = nsp_matrix_create("viewport_dims",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->position == NULLMAT) 
    {
       if (( H->obj->position = nsp_matrix_create("position",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  if ( H->obj->gc == NULL) 
    {
     if (( H->obj->gc = nsp_figuredata_create_default("gc")) == NULL)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspFigure *nsp_figure_create(char *name,char* fname,char* driver,int id,NspMatrix* dims,NspMatrix* viewport_dims,gboolean wresize,NspMatrix* position,NspList* children,gboolean draw_now,NspFigureData* gc,void* Xgc,NspTypeBase *type)
{
  NspFigure *H  = nsp_figure_create_void(name,type);
  if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_figure_create_partial(H) == FAIL) return NULLFIGURE;
  H->obj->fname = fname;
  H->obj->driver = driver;
  H->obj->id=id;
  H->obj->dims= dims;
  H->obj->viewport_dims= viewport_dims;
  H->obj->wresize=wresize;
  H->obj->position= position;
  H->obj->children= children;
  H->obj->draw_now=draw_now;
  H->obj->gc= gc;
  H->obj->Xgc = Xgc;
  if ( nsp_figure_check_values(H) == FAIL) return NULLFIGURE;
#line 91 "codegen/figure.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
  nsp_figure_children_link_figure(H);
  nsp_figure_initialize_gc(H);
#line 587 "figure.c"
  return H;
}


NspFigure *nsp_figure_create_default(char *name)
{
 NspFigure *H  = nsp_figure_create_void(name,NULL);
 if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_figure_create_partial(H) == FAIL) return NULLFIGURE;
 if ( nsp_figure_check_values(H) == FAIL) return NULLFIGURE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspFigure *nsp_figure_copy_partial(NspFigure *H,NspFigure *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspFigure *nsp_figure_copy(NspFigure *self)
{
  NspFigure *H  =nsp_figure_create_void(NVOID,(NspTypeBase *) nsp_type_figure);
  if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLFIGURE;
  if ( nsp_figure_copy_partial(H,self)== NULL) return NULLFIGURE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspFigure *nsp_figure_full_copy_partial(NspFigure *H,NspFigure *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_figure))) == NULL) return NULLFIGURE;
  H->obj->ref_count=1;
  if ((H->obj->fname = nsp_string_copy(self->obj->fname)) == NULL) return NULL;
  if ((H->obj->driver = nsp_string_copy(self->obj->driver)) == NULL) return NULL;
  H->obj->id=self->obj->id;
  if ( self->obj->dims == NULL )
    { H->obj->dims = NULL;}
  else
    {
      if ((H->obj->dims = (NspMatrix *) nsp_object_full_copy_and_name("dims",NSP_OBJECT(self->obj->dims))) == NULLMAT) return NULL;
    }
  if ( self->obj->viewport_dims == NULL )
    { H->obj->viewport_dims = NULL;}
  else
    {
      if ((H->obj->viewport_dims = (NspMatrix *) nsp_object_full_copy_and_name("viewport_dims",NSP_OBJECT(self->obj->viewport_dims))) == NULLMAT) return NULL;
    }
  H->obj->wresize=self->obj->wresize;
  if ( self->obj->position == NULL )
    { H->obj->position = NULL;}
  else
    {
      if ((H->obj->position = (NspMatrix *) nsp_object_full_copy_and_name("position",NSP_OBJECT(self->obj->position))) == NULLMAT) return NULL;
    }
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_full_copy_and_name("children",NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  H->obj->draw_now=self->obj->draw_now;
  if ( self->obj->gc == NULL )
    { H->obj->gc = NULL;}
  else
    {
      if ((H->obj->gc = (NspFigureData *) nsp_object_full_copy_and_name("gc",NSP_OBJECT(self->obj->gc))) == NULL) return NULL;
    }
  H->obj->Xgc = self->obj->Xgc;
  return H;
}

NspFigure *nsp_figure_full_copy(NspFigure *self)
{
  NspFigure *H  =nsp_figure_create_void(NVOID,(NspTypeBase *) nsp_type_figure);
  if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLFIGURE;
  if ( nsp_figure_full_copy_partial(H,self)== NULL) return NULLFIGURE;
#line 91 "codegen/figure.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
  nsp_figure_children_link_figure(H);
  nsp_figure_initialize_gc(H);
#line 677 "figure.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspFigure
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_figure_create(Stack stack, int rhs, int opt, int lhs)
{
  NspFigure *H;
  CheckStdRhs(0,0);
  /* want to be sure that type figure is initialized */
  nsp_type_figure = new_type_figure(T_BASE);
  if(( H = nsp_figure_create_void(NVOID,(NspTypeBase *) nsp_type_figure)) == NULLFIGURE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_figure_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_figure_check_values(H) == FAIL) return RET_BUG;
#line 91 "codegen/figure.override"
  /* verbatim in create/load/full_copy interface use RET_BUG for returned value */
  nsp_figure_children_link_figure(H);
  nsp_figure_initialize_gc(H);
#line 701 "figure.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_nsp_figure_connect(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_connect(self);
  return 0;
}

static int _wrap_nsp_figure_unconnect(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_unconnect(self);
  return 0;
}

static int _wrap_nsp_figure_draw_latter(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_draw_latter(self);
  return 0;
}

static int _wrap_nsp_figure_draw_now(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_draw_now(self);
  return 0;
}

static int _wrap_nsp_figure_process_updates(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_process_updates(self);
  return 0;
}

static int _wrap_nsp_set_current_figure(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_set_current_figure(self);
  return 0;
}

#line 176 "codegen/figure.override"

static int _wrap_nsp_figure_extract(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  NspCompound *C=nsp_figure_get_axe_elts_as_compound(NVOID,self);
  if ( C == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(C));
  return 1;
}

#line 755 "figure.c"


#line 187 "codegen/figure.override"

static int _wrap_nsp_figure_start_compound(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  if (  nsp_figure_start_compound(self) == FAIL) return RET_BUG;
  return 0;
}


#line 767 "figure.c"


#line 197 "codegen/figure.override"

static int _wrap_nsp_figure_end_compound(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  NspCompound *C;
  if ((C = nsp_figure_end_compound("compound",self))==NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(C));
  return 1;
}


#line 781 "figure.c"


static int _wrap_nsp_figure_remove_element(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *g;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_graphic, &g) == FAIL) return RET_BUG;
  nsp_figure_remove_element(self, ((NspGraphic *) g));
  return 0;
}

static NspMethods figure_methods[] = {
  {"connect",(nsp_method *) _wrap_nsp_figure_connect},
  {"unconnect",(nsp_method *) _wrap_nsp_figure_unconnect},
  {"draw_latter",(nsp_method *) _wrap_nsp_figure_draw_latter},
  {"draw_now",(nsp_method *) _wrap_nsp_figure_draw_now},
  {"process_updates",(nsp_method *) _wrap_nsp_figure_process_updates},
  {"set_current",(nsp_method *) _wrap_nsp_set_current_figure},
  {"extract",(nsp_method *) _wrap_nsp_figure_extract},
  {"start_compound",(nsp_method *) _wrap_nsp_figure_start_compound},
  {"end_compound",(nsp_method *) _wrap_nsp_figure_end_compound},
  {"remove",(nsp_method *) _wrap_nsp_figure_remove_element},
  { NULL, NULL}
};

static NspMethods *figure_get_methods(void) { return figure_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_figure_get_fname(void *self,const char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspFigure *) self)->obj->fname;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_figure_set_fname(void *self,const char *attr, NspObject *O)
{
  char *fname;

  if ((fname = nsp_string_object(O))==NULL) return FAIL;
  if ((fname = nsp_string_copy(fname)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspFigure *) self)->obj->fname);
  ((NspFigure *) self)->obj->fname= fname;
  return OK;
}

static NspObject *_wrap_figure_get_driver(void *self,const char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspFigure *) self)->obj->driver;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_figure_set_driver(void *self,const char *attr, NspObject *O)
{
  char *driver;

  if ((driver = nsp_string_object(O))==NULL) return FAIL;
  if ((driver = nsp_string_copy(driver)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspFigure *) self)->obj->driver);
  ((NspFigure *) self)->obj->driver= driver;
  return OK;
}

static NspObject *_wrap_figure_get_id(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigure *) self)->obj->id;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_id(void *self,const char *attr, NspObject *O)
{
  int id;

  if ( IntScalar(O,&id) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->id= id;
  return OK;
}

static NspObject *_wrap_figure_get_dims(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspFigure *) self)->obj->dims;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_dims(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFigure *) self)->obj->dims);
  return (NspObject *) ret;
}

static int _wrap_figure_set_dims(void *self,const char *attr, NspObject *O)
{
  NspMatrix *dims;

  if ( ! IsMat(O) ) return FAIL;
  if ((dims = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigure *) self)->obj->dims != NULL ) 
    nsp_matrix_destroy(((NspFigure *) self)->obj->dims);
  ((NspFigure *) self)->obj->dims= dims;
  return OK;
}

static NspObject *_wrap_figure_get_viewport_dims(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspFigure *) self)->obj->viewport_dims;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_viewport_dims(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFigure *) self)->obj->viewport_dims);
  return (NspObject *) ret;
}

static int _wrap_figure_set_viewport_dims(void *self,const char *attr, NspObject *O)
{
  NspMatrix *viewport_dims;

  if ( ! IsMat(O) ) return FAIL;
  if ((viewport_dims = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigure *) self)->obj->viewport_dims != NULL ) 
    nsp_matrix_destroy(((NspFigure *) self)->obj->viewport_dims);
  ((NspFigure *) self)->obj->viewport_dims= viewport_dims;
  return OK;
}

static NspObject *_wrap_figure_get_wresize(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspFigure *) self)->obj->wresize;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_figure_set_wresize(void *self,const char *attr, NspObject *O)
{
  int wresize;

  if ( BoolScalar(O,&wresize) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->wresize= wresize;
  return OK;
}

static NspObject *_wrap_figure_get_position(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspFigure *) self)->obj->position;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_position(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFigure *) self)->obj->position);
  return (NspObject *) ret;
}

static int _wrap_figure_set_position(void *self,const char *attr, NspObject *O)
{
  NspMatrix *position;

  if ( ! IsMat(O) ) return FAIL;
  if ((position = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigure *) self)->obj->position != NULL ) 
    nsp_matrix_destroy(((NspFigure *) self)->obj->position);
  ((NspFigure *) self)->obj->position= position;
  return OK;
}

#line 112 "codegen/figure.override"

static NspObject *_wrap_figure_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE;
  ret = ((NspList*) ((NspFigure *) self)->obj->children);
  return (NspObject *) ret;
}

static int _wrap_figure_set_children(void *self, char *attr, NspObject *O)
{
  NspList *children;

  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspFigure *) self)->obj->children != NULL ) 
    {
      nsp_figure_children_unlink_figure(self);
      nsp_list_destroy(((NspFigure *) self)->obj->children);
    }
  ((NspFigure *) self)->obj->children= children;
  nsp_figure_children_link_figure(self);
  return OK;
}

static int _wrap_figure_set_obj_children(void *self,NspObject *val)
{
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_figure_check_children(self,(NspList *) val )== FAIL) 
    {
      return FAIL;
    }
  if (((NspFigure *) self)->obj->children != NULL ) 
    {
      nsp_figure_children_unlink_figure(self);
      nsp_list_destroy(((NspFigure *) self)->obj->children);
    }
  ((NspFigure *) self)->obj->children=(NspList *) val ;
  nsp_figure_children_link_figure(self);
  return OK;
}

#line 1021 "figure.c"
static NspObject *_wrap_figure_get_children(void *self,const char *attr)
{
  NspList *ret;

  ret = ((NspFigure *) self)->obj->children;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_gc(void *self,const char *attr)
{
  NspFigureData *ret;

  ret = ((NspFigure *) self)->obj->gc;
  return NSP_OBJECT(ret);
}

static int _wrap_figure_set_gc(void *self,const char *attr, NspObject *O)
{
  NspFigureData *gc;

  if ( ! IsFigureData(O) ) return FAIL;
  if ((gc = (NspFigureData *) nsp_object_copy_and_name(attr,O)) == NULLFIGUREDATA) return FAIL;
  if (((NspFigure *) self)->obj->gc != NULL ) 
    nsp_figuredata_destroy(((NspFigure *) self)->obj->gc);
  ((NspFigure *) self)->obj->gc= gc;
  return OK;
}

static AttrTab figure_attrs[] = {
  { "fname", (attr_get_function *)_wrap_figure_get_fname, (attr_set_function *)_wrap_figure_set_fname,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "driver", (attr_get_function *)_wrap_figure_get_driver, (attr_set_function *)_wrap_figure_set_driver,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "id", (attr_get_function *)_wrap_figure_get_id, (attr_set_function *)_wrap_figure_set_id,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "dims", (attr_get_function *)_wrap_figure_get_dims, (attr_set_function *)_wrap_figure_set_dims,(attr_get_object_function *)_wrap_figure_get_obj_dims, (attr_set_object_function *)int_set_object_failed },
  { "viewport_dims", (attr_get_function *)_wrap_figure_get_viewport_dims, (attr_set_function *)_wrap_figure_set_viewport_dims,(attr_get_object_function *)_wrap_figure_get_obj_viewport_dims, (attr_set_object_function *)int_set_object_failed },
  { "wresize", (attr_get_function *)_wrap_figure_get_wresize, (attr_set_function *)_wrap_figure_set_wresize,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "position", (attr_get_function *)_wrap_figure_get_position, (attr_set_function *)_wrap_figure_set_position,(attr_get_object_function *)_wrap_figure_get_obj_position, (attr_set_object_function *)int_set_object_failed },
  { "children", (attr_get_function *)_wrap_figure_get_children, (attr_set_function *)_wrap_figure_set_children,(attr_get_object_function *)_wrap_figure_get_obj_children, (attr_set_object_function *)_wrap_figure_set_obj_children },
  { "gc", (attr_get_function *)_wrap_figure_get_gc, (attr_set_function *)_wrap_figure_set_gc,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};



/* ----------- NspFigureData ----------- */


#define  NspFigureData_Private 
#include <nsp/object.h>
#include <nsp/figuredata.h>
#include <nsp/interf.h>

/* 
 * NspFigureData inherits from Object 
 */

int nsp_type_figuredata_id=0;
NspTypeFigureData *nsp_type_figuredata=NULL;

/*
 * Type object for NspFigureData 
 * all the instance of NspTypeFigureData share the same id. 
 * nsp_type_figuredata: is an instance of NspTypeFigureData 
 *    used for objects of NspFigureData type (i.e built with new_figuredata) 
 * other instances are used for derived classes 
 */
NspTypeFigureData *new_type_figuredata(type_mode mode)
{
  NspTypeFigureData *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_figuredata != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_figuredata;
    }
  if (( type =  malloc(sizeof(NspTypeFigureData))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = figuredata_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = figuredata_get_methods;
  type->new = (new_func *) new_figuredata;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for figuredata */ 

  top->pr = (print_func *) nsp_figuredata_print;
  top->dealloc = (dealloc_func *) nsp_figuredata_destroy;
  top->copy  =  (copy_func *) nsp_figuredata_copy;
  top->size  = (size_func *) nsp_figuredata_size;
  top->s_type =  (s_type_func *) nsp_figuredata_type_as_string;
  top->sh_type = (sh_type_func *) nsp_figuredata_type_short_string;
  top->info = (info_func *) nsp_figuredata_info;
  /* top->is_true = (is_true_func  *) nsp_figuredata_is_true; */
  /* top->loop =(loop_func *) nsp_figuredata_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_figuredata_object;
  top->eq  = (eq_func *) nsp_figuredata_eq;
  top->neq  = (eq_func *) nsp_figuredata_neq;
  top->save  = (save_func *) nsp_figuredata_xdr_save;
  top->load  = (load_func *) nsp_figuredata_xdr_load;
  top->create = (create_func*) int_figuredata_create;
  top->latex = (print_func *) nsp_figuredata_latex;
  top->full_copy = (copy_func *) nsp_figuredata_full_copy;

  /* specific methods for figuredata */

  type->init = (init_func *) init_figuredata;

  /* 
   * NspFigureData interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_figuredata_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeFigureData called nsp_type_figuredata
       */
      type->id =  nsp_type_figuredata_id = nsp_new_type_id();
      nsp_type_figuredata = type;
      if ( nsp_register_type(nsp_type_figuredata) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_figuredata(mode);
    }
  else 
    {
      type->id = nsp_type_figuredata_id;
      return type;
    }
}

/*
 * initialize NspFigureData instances 
 * locally and by calling initializer on parent class 
 */

static int init_figuredata(NspFigureData *Obj,NspTypeFigureData *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->color = 1;
  Obj->background = -1;
  Obj->colormap = NULLMAT;
  Obj->dashes = 1;
  Obj->font = 2;
  Obj->font_size = 1;
  Obj->foreground = -1;
  Obj->hidden3d = 4;
  Obj->line_mode = 0;
  Obj->line_style = 1;
  Obj->mark = 0;
  Obj->mark_size = 0;
  Obj->pattern = 1;
  Obj->pixmap = 0;
  Obj->thickness = 1;
  Obj->use_color = 1;
  Obj->auto_clear = FALSE;
  return OK;
}

/*
 * new instance of NspFigureData 
 */

NspFigureData *new_figuredata() 
{
  NspFigureData *loc;
  /* type must exists */
  nsp_type_figuredata = new_type_figuredata(T_BASE);
  if ( (loc = malloc(sizeof(NspFigureData)))== NULLFIGUREDATA) return loc;
  /* initialize object */
  if ( init_figuredata(loc,nsp_type_figuredata) == FAIL) return NULLFIGUREDATA;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspFigureData 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_figuredata_size(NspFigureData *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char figuredata_type_name[]="FigureData";
static char figuredata_short_type_name[]="figuredata";

static char *nsp_figuredata_type_as_string(void)
{
  return(figuredata_type_name);
}

static char *nsp_figuredata_type_short_string(NspObject *v)
{
  return(figuredata_short_type_name);
}

/*
 * A == B 
 */

static int nsp_figuredata_eq(NspFigureData *A, NspObject *B)
{
  NspFigureData *loc = (NspFigureData *) B;
  if ( check_cast(B,nsp_type_figuredata_id) == FALSE) return FALSE ;
  if ( A->color != loc->color) return FALSE;
  if ( A->background != loc->background) return FALSE;
  if ( NSP_OBJECT(A->colormap)->type->eq(A->colormap,loc->colormap) == FALSE ) return FALSE;
  if ( A->dashes != loc->dashes) return FALSE;
  if ( A->font != loc->font) return FALSE;
  if ( A->font_size != loc->font_size) return FALSE;
  if ( A->foreground != loc->foreground) return FALSE;
  if ( A->hidden3d != loc->hidden3d) return FALSE;
  if ( A->line_mode != loc->line_mode) return FALSE;
  if ( A->line_style != loc->line_style) return FALSE;
  if ( A->mark != loc->mark) return FALSE;
  if ( A->mark_size != loc->mark_size) return FALSE;
  if ( A->pattern != loc->pattern) return FALSE;
  if ( A->pixmap != loc->pixmap) return FALSE;
  if ( A->thickness != loc->thickness) return FALSE;
  if ( A->use_color != loc->use_color) return FALSE;
  if ( A->auto_clear != loc->auto_clear) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_figuredata_neq(NspFigureData *A, NspObject *B)
{
  return ( nsp_figuredata_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_figuredata_xdr_save(XDR *xdrs, NspFigureData *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_figuredata)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->background) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->colormap)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->dashes) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->font) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->font_size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->foreground) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->hidden3d) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->line_mode) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->line_style) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->mark) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->mark_size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->pattern) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->pixmap) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->use_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->auto_clear) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspFigureData  *nsp_figuredata_xdr_load_partial(XDR *xdrs, NspFigureData *M)
{
  if (nsp_xdr_load_i(xdrs, &M->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->background) == FAIL) return NULL;
  if ((M->colormap =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->dashes) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->font) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->font_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->foreground) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->hidden3d) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->line_mode) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->line_style) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->mark) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->mark_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->pattern) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->pixmap) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->use_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->auto_clear) == FAIL) return NULL;
 return M;
}

static NspFigureData  *nsp_figuredata_xdr_load(XDR *xdrs)
{
  NspFigureData *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLFIGUREDATA;
  if ((H  = nsp_figuredata_create_void(name,(NspTypeBase *) nsp_type_figuredata))== NULLFIGUREDATA) return H;
  if ( nsp_figuredata_create_partial(H) == FAIL) return NULLFIGUREDATA;
  if ((H  = nsp_figuredata_xdr_load_partial(xdrs,H))== NULLFIGUREDATA) return H;
  if ( nsp_figuredata_check_values(H) == FAIL) return NULLFIGUREDATA;
  return H;
}

/*
 * delete 
 */

void nsp_figuredata_destroy_partial(NspFigureData *H)
{
  if ( H->colormap != NULL ) 
    nsp_matrix_destroy(H->colormap);
}

void nsp_figuredata_destroy(NspFigureData *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_figuredata_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_figuredata_info(NspFigureData *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLFIGUREDATA) 
    {
      Sciprintf("Null Pointer NspFigureData \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_figuredata_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_figuredata_print(NspFigureData *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLFIGUREDATA) 
    {
      Sciprintf("Null Pointer NspFigureData \n");
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
          nsp_figuredata_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_figuredata_type_short_string(NSP_OBJECT(M)) );
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"color=%d\n",M->color);
  Sciprintf1(indent+2,"background=%d\n",M->background);
  if ( M->colormap != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->colormap),indent+2,"colormap",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"dashes=%d\n",M->dashes);
  Sciprintf1(indent+2,"font=%d\n",M->font);
  Sciprintf1(indent+2,"font_size=%d\n",M->font_size);
  Sciprintf1(indent+2,"foreground=%d\n",M->foreground);
  Sciprintf1(indent+2,"hidden3d=%d\n",M->hidden3d);
  Sciprintf1(indent+2,"line_mode=%d\n",M->line_mode);
  Sciprintf1(indent+2,"line_style=%d\n",M->line_style);
  Sciprintf1(indent+2,"mark=%d\n",M->mark);
  Sciprintf1(indent+2,"mark_size=%d\n",M->mark_size);
  Sciprintf1(indent+2,"pattern=%d\n",M->pattern);
  Sciprintf1(indent+2,"pixmap=%d\n",M->pixmap);
  Sciprintf1(indent+2,"thickness=%d\n",M->thickness);
  Sciprintf1(indent+2,"use_color=%d\n",M->use_color);
  Sciprintf1(indent+2,"auto_clear	= %s\n", ( M->auto_clear == TRUE) ? "T" : "F" );
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_figuredata_latex(NspFigureData *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_figuredata_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"color=%d\n",M->color);
  Sciprintf1(indent+2,"background=%d\n",M->background);
  if ( M->colormap != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->colormap),indent+2,"colormap",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"dashes=%d\n",M->dashes);
  Sciprintf1(indent+2,"font=%d\n",M->font);
  Sciprintf1(indent+2,"font_size=%d\n",M->font_size);
  Sciprintf1(indent+2,"foreground=%d\n",M->foreground);
  Sciprintf1(indent+2,"hidden3d=%d\n",M->hidden3d);
  Sciprintf1(indent+2,"line_mode=%d\n",M->line_mode);
  Sciprintf1(indent+2,"line_style=%d\n",M->line_style);
  Sciprintf1(indent+2,"mark=%d\n",M->mark);
  Sciprintf1(indent+2,"mark_size=%d\n",M->mark_size);
  Sciprintf1(indent+2,"pattern=%d\n",M->pattern);
  Sciprintf1(indent+2,"pixmap=%d\n",M->pixmap);
  Sciprintf1(indent+2,"thickness=%d\n",M->thickness);
  Sciprintf1(indent+2,"use_color=%d\n",M->use_color);
  Sciprintf1(indent+2,"auto_clear	= %s\n", ( M->auto_clear == TRUE) ? "T" : "F" );
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspFigureData objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspFigureData   *nsp_figuredata_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_figuredata_id) == TRUE ) return ((NspFigureData *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_figuredata));
  return NULL;
}

int IsFigureDataObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_figuredata_id);
}

int IsFigureData(NspObject *O)
{
  return nsp_object_type(O,nsp_type_figuredata_id);
}

NspFigureData  *GetFigureDataCopy(Stack stack, int i)
{
  if (  GetFigureData(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFigureData  *GetFigureData(Stack stack, int i)
{
  NspFigureData *M;
  if (( M = nsp_figuredata_object(NthObj(i))) == NULLFIGUREDATA)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspFigureData instance 
 *-----------------------------------------------------*/

static NspFigureData *nsp_figuredata_create_void(char *name,NspTypeBase *type)
{
 NspFigureData *H  = (type == NULL) ? new_figuredata() : type->new();
 if ( H ==  NULLFIGUREDATA)
  {
   Sciprintf("No more memory\n");
   return NULLFIGUREDATA;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLFIGUREDATA;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_figuredata_create_partial(NspFigureData *H)
{
  return OK;
}

int nsp_figuredata_check_values(NspFigureData *H)
{
  if ( H->colormap == NULLMAT) 
    {
       if (( H->colormap = nsp_matrix_create("colormap",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  return OK;
}

NspFigureData *nsp_figuredata_create(char *name,int color,int background,NspMatrix* colormap,int dashes,int font,int font_size,int foreground,int hidden3d,int line_mode,int line_style,int mark,int mark_size,int pattern,int pixmap,int thickness,int use_color,gboolean auto_clear,NspTypeBase *type)
{
  NspFigureData *H  = nsp_figuredata_create_void(name,type);
  if ( H ==  NULLFIGUREDATA) return NULLFIGUREDATA;
  H->color=color;
  H->background=background;
  H->colormap= colormap;
  H->dashes=dashes;
  H->font=font;
  H->font_size=font_size;
  H->foreground=foreground;
  H->hidden3d=hidden3d;
  H->line_mode=line_mode;
  H->line_style=line_style;
  H->mark=mark;
  H->mark_size=mark_size;
  H->pattern=pattern;
  H->pixmap=pixmap;
  H->thickness=thickness;
  H->use_color=use_color;
  H->auto_clear=auto_clear;
  if ( nsp_figuredata_check_values(H) == FAIL) return NULLFIGUREDATA;
  return H;
}


NspFigureData *nsp_figuredata_create_default(char *name)
{
 NspFigureData *H  = nsp_figuredata_create_void(name,NULL);
 if ( H ==  NULLFIGUREDATA) return NULLFIGUREDATA;
 if ( nsp_figuredata_check_values(H) == FAIL) return NULLFIGUREDATA;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspFigureData *nsp_figuredata_copy_partial(NspFigureData *H,NspFigureData *self)
{
  H->color=self->color;
  H->background=self->background;
  if ( self->colormap == NULL )
    { H->colormap = NULL;}
  else
    {
      if ((H->colormap = (NspMatrix *) nsp_object_copy_and_name("colormap",NSP_OBJECT(self->colormap))) == NULLMAT) return NULL;
    }
  H->dashes=self->dashes;
  H->font=self->font;
  H->font_size=self->font_size;
  H->foreground=self->foreground;
  H->hidden3d=self->hidden3d;
  H->line_mode=self->line_mode;
  H->line_style=self->line_style;
  H->mark=self->mark;
  H->mark_size=self->mark_size;
  H->pattern=self->pattern;
  H->pixmap=self->pixmap;
  H->thickness=self->thickness;
  H->use_color=self->use_color;
  H->auto_clear=self->auto_clear;
  return H;
}

NspFigureData *nsp_figuredata_copy(NspFigureData *self)
{
  NspFigureData *H  =nsp_figuredata_create_void(NVOID,(NspTypeBase *) nsp_type_figuredata);
  if ( H ==  NULLFIGUREDATA) return NULLFIGUREDATA;
  if ( nsp_figuredata_copy_partial(H,self)== NULL) return NULLFIGUREDATA;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspFigureData *nsp_figuredata_full_copy_partial(NspFigureData *H,NspFigureData *self)
{
  H->color=self->color;
  H->background=self->background;
  if ( self->colormap == NULL )
    { H->colormap = NULL;}
  else
    {
      if ((H->colormap = (NspMatrix *) nsp_object_full_copy_and_name("colormap",NSP_OBJECT(self->colormap))) == NULLMAT) return NULL;
    }
  H->dashes=self->dashes;
  H->font=self->font;
  H->font_size=self->font_size;
  H->foreground=self->foreground;
  H->hidden3d=self->hidden3d;
  H->line_mode=self->line_mode;
  H->line_style=self->line_style;
  H->mark=self->mark;
  H->mark_size=self->mark_size;
  H->pattern=self->pattern;
  H->pixmap=self->pixmap;
  H->thickness=self->thickness;
  H->use_color=self->use_color;
  H->auto_clear=self->auto_clear;
  return H;
}

NspFigureData *nsp_figuredata_full_copy(NspFigureData *self)
{
  NspFigureData *H  =nsp_figuredata_create_void(NVOID,(NspTypeBase *) nsp_type_figuredata);
  if ( H ==  NULLFIGUREDATA) return NULLFIGUREDATA;
  if ( nsp_figuredata_full_copy_partial(H,self)== NULL) return NULLFIGUREDATA;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspFigureData
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_figuredata_create(Stack stack, int rhs, int opt, int lhs)
{
  NspFigureData *H;
  CheckStdRhs(0,0);
  /* want to be sure that type figuredata is initialized */
  nsp_type_figuredata = new_type_figuredata(T_BASE);
  if(( H = nsp_figuredata_create_void(NVOID,(NspTypeBase *) nsp_type_figuredata)) == NULLFIGUREDATA) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_figuredata_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *figuredata_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_figuredata_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspFigureData *) self)->color= color;
  return OK;
}

static NspObject *_wrap_figuredata_get_background(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->background;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_background(void *self,const char *attr, NspObject *O)
{
  int background;

  if ( IntScalar(O,&background) == FAIL) return FAIL;
  ((NspFigureData *) self)->background= background;
  return OK;
}

#line 209 "codegen/figure.override"

static NspObject *_wrap_figuredata_get_colormap(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspFigureData *) self)->colormap;
  return (NspObject *) ret;
}

static NspObject *_wrap_figuredata_get_obj_colormap(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = TRUE;
  ret = ((NspMatrix*) ((NspFigureData *) self)->colormap);
  return (NspObject *) ret;
}

static int _wrap_figuredata_set_obj_colormap(void *self, NspObject *O)
{
  NspMatrix *colormap;
  if ( ! IsMat(O) ) return FAIL;
  if ((colormap = (NspMatrix *) nsp_object_copy_and_name("cmap",O)) == NULLMAT) return FAIL;
  if (((NspFigureData *) self)->colormap != NULL ) 
    nsp_matrix_destroy(((NspFigureData *) self)->colormap);
  ((NspFigureData *) self)->colormap= colormap;
  return OK;
}

/**
 * _wrap_figuredata_set_colormap:
 * @self: 
 * @attr: 
 * @O: 
 * 
 * 
 * 
 * Returns: 
 **/

static int _wrap_figuredata_set_colormap(void *self,const char *attr, NspObject *O)
{
  NspMatrix *colormap;
  if ( ! IsMat(O) ) return FAIL;
  if ((colormap = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigureData *) self)->colormap != NULL ) 
    nsp_matrix_destroy(((NspFigureData *) self)->colormap);
  ((NspFigureData *) self)->colormap= colormap;
  return OK;
}


#line 1759 "figure.c"
static NspObject *_wrap_figuredata_get_dashes(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->dashes;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_dashes(void *self,const char *attr, NspObject *O)
{
  int dashes;

  if ( IntScalar(O,&dashes) == FAIL) return FAIL;
  ((NspFigureData *) self)->dashes= dashes;
  return OK;
}

static NspObject *_wrap_figuredata_get_font(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->font;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_font(void *self,const char *attr, NspObject *O)
{
  int font;

  if ( IntScalar(O,&font) == FAIL) return FAIL;
  ((NspFigureData *) self)->font= font;
  return OK;
}

static NspObject *_wrap_figuredata_get_font_size(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->font_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_font_size(void *self,const char *attr, NspObject *O)
{
  int font_size;

  if ( IntScalar(O,&font_size) == FAIL) return FAIL;
  ((NspFigureData *) self)->font_size= font_size;
  return OK;
}

static NspObject *_wrap_figuredata_get_foreground(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->foreground;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_foreground(void *self,const char *attr, NspObject *O)
{
  int foreground;

  if ( IntScalar(O,&foreground) == FAIL) return FAIL;
  ((NspFigureData *) self)->foreground= foreground;
  return OK;
}

static NspObject *_wrap_figuredata_get_hidden3d(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->hidden3d;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_hidden3d(void *self,const char *attr, NspObject *O)
{
  int hidden3d;

  if ( IntScalar(O,&hidden3d) == FAIL) return FAIL;
  ((NspFigureData *) self)->hidden3d= hidden3d;
  return OK;
}

static NspObject *_wrap_figuredata_get_line_mode(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->line_mode;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_line_mode(void *self,const char *attr, NspObject *O)
{
  int line_mode;

  if ( IntScalar(O,&line_mode) == FAIL) return FAIL;
  ((NspFigureData *) self)->line_mode= line_mode;
  return OK;
}

static NspObject *_wrap_figuredata_get_line_style(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->line_style;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_line_style(void *self,const char *attr, NspObject *O)
{
  int line_style;

  if ( IntScalar(O,&line_style) == FAIL) return FAIL;
  ((NspFigureData *) self)->line_style= line_style;
  return OK;
}

static NspObject *_wrap_figuredata_get_mark(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->mark;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_mark(void *self,const char *attr, NspObject *O)
{
  int mark;

  if ( IntScalar(O,&mark) == FAIL) return FAIL;
  ((NspFigureData *) self)->mark= mark;
  return OK;
}

static NspObject *_wrap_figuredata_get_mark_size(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->mark_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_mark_size(void *self,const char *attr, NspObject *O)
{
  int mark_size;

  if ( IntScalar(O,&mark_size) == FAIL) return FAIL;
  ((NspFigureData *) self)->mark_size= mark_size;
  return OK;
}

static NspObject *_wrap_figuredata_get_pattern(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->pattern;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_pattern(void *self,const char *attr, NspObject *O)
{
  int pattern;

  if ( IntScalar(O,&pattern) == FAIL) return FAIL;
  ((NspFigureData *) self)->pattern= pattern;
  return OK;
}

static NspObject *_wrap_figuredata_get_pixmap(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->pixmap;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_pixmap(void *self,const char *attr, NspObject *O)
{
  int pixmap;

  if ( IntScalar(O,&pixmap) == FAIL) return FAIL;
  ((NspFigureData *) self)->pixmap= pixmap;
  return OK;
}

static NspObject *_wrap_figuredata_get_thickness(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspFigureData *) self)->thickness= thickness;
  return OK;
}

static NspObject *_wrap_figuredata_get_use_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspFigureData *) self)->use_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figuredata_set_use_color(void *self,const char *attr, NspObject *O)
{
  int use_color;

  if ( IntScalar(O,&use_color) == FAIL) return FAIL;
  ((NspFigureData *) self)->use_color= use_color;
  return OK;
}

static NspObject *_wrap_figuredata_get_auto_clear(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspFigureData *) self)->auto_clear;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_figuredata_set_auto_clear(void *self,const char *attr, NspObject *O)
{
  int auto_clear;

  if ( BoolScalar(O,&auto_clear) == FAIL) return FAIL;
  ((NspFigureData *) self)->auto_clear= auto_clear;
  return OK;
}

static AttrTab figuredata_attrs[] = {
  { "color", (attr_get_function *)_wrap_figuredata_get_color, (attr_set_function *)_wrap_figuredata_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "background", (attr_get_function *)_wrap_figuredata_get_background, (attr_set_function *)_wrap_figuredata_set_background,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "colormap", (attr_get_function *)_wrap_figuredata_get_colormap, (attr_set_function *)_wrap_figuredata_set_colormap,(attr_get_object_function *)_wrap_figuredata_get_obj_colormap, (attr_set_object_function *)_wrap_figuredata_set_obj_colormap },
  { "dashes", (attr_get_function *)_wrap_figuredata_get_dashes, (attr_set_function *)_wrap_figuredata_set_dashes,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "font", (attr_get_function *)_wrap_figuredata_get_font, (attr_set_function *)_wrap_figuredata_set_font,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "font_size", (attr_get_function *)_wrap_figuredata_get_font_size, (attr_set_function *)_wrap_figuredata_set_font_size,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "foreground", (attr_get_function *)_wrap_figuredata_get_foreground, (attr_set_function *)_wrap_figuredata_set_foreground,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "hidden3d", (attr_get_function *)_wrap_figuredata_get_hidden3d, (attr_set_function *)_wrap_figuredata_set_hidden3d,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "line_mode", (attr_get_function *)_wrap_figuredata_get_line_mode, (attr_set_function *)_wrap_figuredata_set_line_mode,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "line_style", (attr_get_function *)_wrap_figuredata_get_line_style, (attr_set_function *)_wrap_figuredata_set_line_style,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark", (attr_get_function *)_wrap_figuredata_get_mark, (attr_set_function *)_wrap_figuredata_set_mark,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark_size", (attr_get_function *)_wrap_figuredata_get_mark_size, (attr_set_function *)_wrap_figuredata_set_mark_size,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "pattern", (attr_get_function *)_wrap_figuredata_get_pattern, (attr_set_function *)_wrap_figuredata_set_pattern,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "pixmap", (attr_get_function *)_wrap_figuredata_get_pixmap, (attr_set_function *)_wrap_figuredata_set_pixmap,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_figuredata_get_thickness, (attr_set_function *)_wrap_figuredata_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "use_color", (attr_get_function *)_wrap_figuredata_get_use_color, (attr_set_function *)_wrap_figuredata_set_use_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "auto_clear", (attr_get_function *)_wrap_figuredata_get_auto_clear, (attr_set_function *)_wrap_figuredata_set_auto_clear,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_nsp_check_for_current_figure(Stack stack, int rhs, int opt, int lhs) /* get_current_figure */
{
  NspFigure *ret;

    ret = nsp_check_for_current_figure();
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

int _wrap_nsp_get_current_axes(Stack stack, int rhs, int opt, int lhs) /* get_current_axes */
{
  NspAxes *ret;

    ret = nsp_get_current_axes();
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 156 "codegen/figure.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_figure(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 2054 "figure.c"


#line 166 "codegen/figure.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_figure(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 2066 "figure.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Figure_func[]={
  {"get_current_figure", _wrap_nsp_check_for_current_figure},
  {"get_current_axes", _wrap_nsp_get_current_axes},
  {"extractelts_figure", _wrap_nsp_extractelts_figure},
  {"setrowscols_figure", _wrap_nsp_setrowscols_figure},
  { "figure_create", int_figure_create},
  { NULL, NULL}
};

/* call ith function in the Figure interface */

int Figure_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Figure_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Figure_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Figure_func[i].name;
  *f = Figure_func[i].fonc;
}

#line 261 "codegen/figure.override"

/**
 * nsp_draw_figure:
 * @Xgc: 
 * @Obj: 
 * @rect: 
 * @data: 
 * 
 * 
 **/

static void nsp_draw_figure(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  int rep;
  Cell *cloc;
  NspList *L;
  NspFigure *F = (NspFigure *) Obj;
  /* set up values according to figure */
  nsp_figure_set_gc_values(F);
  rep = Xgc->graphic_engine->xpush_colormap(Xgc, F->obj->gc->colormap);

  /* clear proper zone */
  if ( rect != NULL)
    {
      Xgc->graphic_engine->cleararea(Xgc,rect);
      Xgc->graphic_engine->xset_clip(Xgc,rect);
    }
  else 
    {
      Xgc->graphic_engine->clearwindow(Xgc);
    }
  
  /* draw elements */
  L = F->obj->children;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G,rect,data);
	}
      cloc = cloc->next;
    }
  /* reset Xgc colors */
  if ( rep == OK)  Xgc->graphic_engine->xpop_colormap(Xgc);
  /* unclip */
  Xgc->graphic_engine->xset_unclip(Xgc);
}


#ifdef WITH_GTKGLEXT 
extern Gengine GL_gengine; 
#endif 
#ifdef WITH_CAIRO
extern Gengine Cairo_gengine; 
#endif 
extern Gengine XFig_gengine, Pos_gengine, Gtk_gengine; 

/**
 * nsp_figure_connect:
 * @F: 
 * 
 * creates a graphic window, sets this window as the current one 
 * then add figure to the Xgc associated to the 
 * window. The type of the window depends on the 
 * driver field of the figure @F.
 *
 * Returns: %OK or %FAIL
 **/

static int nsp_figure_connect(NspFigure *F)
{
  driver_initgraphic *initg = Gtk_gengine.initgraphic;
  int v1=-1, wdim[2], wpdim[2],  wpos[2];
  BCG *Xgc;
  
  if (F->obj->Xgc != NULL ) 
    {
      /* Figure is already connected */
      return OK;
    }
 
  if ( F->obj->dims != NULL && F->obj->dims->mn == 2 )
    { 
      wdim[0] = F->obj->dims->R[0];
      wdim[1] = F->obj->dims->R[1];
    }
  
  if ( F->obj->viewport_dims != NULL &&  F->obj->viewport_dims->mn == 2 ) 
    { 
      wpdim[0] = F->obj->viewport_dims->R[0];
      wpdim[1] = F->obj->viewport_dims->R[1];
    }

  /* A FAIRE c'est les offsets du viewport */
  /* 
     if (viewport != NULL && viewport->mn != 2 ) 
     {
     viewport[0]= F->obj->viewport_pos->R[0];
     viewport[1]= F->obj->viewport_pos->R[1];
     }
  */
  if ( F->obj->position != NULL && F->obj->position->mn == 2 )
    { 
      wpos[0] = F->obj->position->R[0];
      wpos[1] = F->obj->position->R[1];
    }
  
  if ( strcmp(F->obj->driver,"Gtk") == 0) initg = Gtk_gengine.initgraphic;
  else if ( strcmp(F->obj->driver,"OpenGl") == 0) 
    {
#ifdef WITH_GTKGLEXT 
      initg = GL_gengine.initgraphic;
#else 
      Sciprintf("No opengl support in this version\n");
#endif 
    }
  else if ( strcmp(F->obj->driver,"Cairo") == 0) 
    {
#ifdef WITH_CAIRO 
      initg = Cairo_gengine.initgraphic;
#else 
      Sciprintf("No cairo support in this version\n");
#endif
    }
  else 
    {
      initg = Gtk_gengine.initgraphic;
    }
  
  v1 = -1; 

  if ((F = (NspFigure *) nsp_object_copy_and_name("fig",NSP_OBJECT(F))) == NULL )
    return FAIL;
  
  initg("",&v1, 
	(F->obj->dims != NULL  && F->obj->dims->mn == 2 ) ? wdim :NULL, 
	(F->obj->viewport_dims  != NULL &&  F->obj->viewport_dims->mn == 2) ? wpdim : NULL , 
	NULL, 
	( F->obj->position != NULL && F->obj->position->mn == 2 ) ? wpos: NULL , 
	'e',NULL,F);
  
  if ( F->obj->Xgc == NULL) 
    {
      Sciprintf("failed to connect figure\n");
      return FAIL;
    }
  
  Xgc->graphic_engine->xset_wresize(Xgc,F->obj->wresize);
  if ( F->obj->fname != NULL && strcmp(F->obj->fname,"") != 0 )
    Xgc->graphic_engine->setpopupname(Xgc,F->obj->fname);

  return OK;
}


/**
 * nsp_figure_unconnect:
 * @F: a #NspFigure 
 * 
 * delete graphic window associated to @F. 
 * @F is not deleted.
 * 
 * Returns: %OK
 **/

static int nsp_figure_unconnect(NspFigure *F)
{
  BCG *Xgc =  F->obj->Xgc;
  if ( Xgc != NULL)  Xgc->actions->delete(Xgc);
  F->obj->Xgc = NULL;
  return OK ;
}

/**
 * nsp_list_link_figure:
 * @L: 
 * @F: 
 * @A: 
 * 
 * recursively call the link_figure method for each element of 
 * the list.
 * 
 **/

void nsp_list_link_figure(NspList *L, nsp_figure *F, void *A)
{
  /* A should be a nsp_axes */
  Cell *cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->link_figure(G,F,A);
	}
      cloc = cloc->next;
    }
}

/**
 * nsp_list_unlink_figure:
 * @L: 
 * @F: 
 * 
 * recursively call the unlink_figure method for each element of 
 * the list.
 * 
 **/

void nsp_list_unlink_figure(NspList *L, nsp_figure *F)
{
  Cell *cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->unlink_figure(G,F);
	}
      cloc = cloc->next;
    }
}

/**
 * nsp_list_check_figure:
 * @L: 
 * @F: 
 * 
 * checks that all the children of the Figure @F have a field Fif 
 * set to #nsp_figure @F.
 *
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_list_check_figure(NspList *L, nsp_figure *F)
{
  Cell *cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  if ( ! IsGraphic( cloc->O))
	    {
	      Scierror("Error: List should only contain graphic objects\n");
	      return FAIL;
	    }
	  if ( ((NspGraphic *) cloc->O)->obj->Fig != NULL && ((NspGraphic *) cloc->O)->obj->Fig != F ) 
	    {
	      Scierror("Error: A graphic object already belongs to an other figure\n");
	      return FAIL;
	    }
	}
      cloc = cloc->next;
    }
  return OK;
}

/**
 * nsp_figure_children_link_figure:
 * @F: a #NspFigure 
 * 
 * call link_figure method on children of @F.
 * 
 **/
static void nsp_figure_children_link_figure(NspFigure *F)
{
  nsp_list_link_figure(F->obj->children, F->obj, NULL);
}


/**
 * nsp_figure_children_unlink_figure:
 * @F: a #NspFigure 
 * 
 * 
 * call unlink_figure method on children of @F.
 **/
static void nsp_figure_children_unlink_figure(NspFigure *F)
{
  nsp_list_unlink_figure(F->obj->children,F->obj);
}


/**
 * nsp_figure_check_children:
 * @F: 
 * @L: 
 * 
 * 
 * cheks that all the children of @F have proper Fig field
 * 
 * Returns: 
 **/
static int nsp_figure_check_children(NspFigure *F,NspList *L)
{
  return  nsp_list_check_figure(F->obj->children,F->obj);
}

/**
 * nsp_get_current_figure:
 * @void: 
 * 
 * returns the current figure (not a copy).
 * 
 * Returns: a #NspFigure or %NULL
 **/

static NspFigure *nsp_current_figure=NULL;

NspFigure *nsp_get_current_figure(void)
{
  BCG *Xgc;
  NspObject  *F = NULL;
  if ( nsp_current_figure != NULL )
    {
      if ( nsp_current_figure->obj->ref_count > 1) 
	return nsp_current_figure;
      /* no more ref to the current_figure */
      nsp_figure_destroy(nsp_current_figure);
      nsp_current_figure = NULL;
    }
  /* check if we have a graphic window with 
   * a figure non recorded as current 
   */
  if ((Xgc = window_list_get_first()) == NULL) return NULL;
  if ((F = (NspObject *) Xgc->figure)== NULL) return NULL;
  if (F == NULL || ! IsFigure(F)) return NULL;
  nsp_current_figure = (NspFigure *) nsp_object_copy_and_name("Fig",(NspObject *) F);
  return nsp_current_figure;
}

/**
 * nsp_set_current_figure:
 * @F: 
 * 
 * sets nsp_current_figure to @F
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_set_current_figure(NspFigure *F)
{
  NspFigure *loc;
  if ((loc =(NspFigure *) nsp_object_copy_and_name("Fig",(NspObject *) F)) == NULL ) 
    return FAIL;
  if ( nsp_current_figure != NULL) 
    nsp_figure_destroy(nsp_current_figure);
  nsp_current_figure =loc ;
  return OK;
}

/**
 * nsp_figure_children:
 * @Obj: a #NspGraphic which is to be a #NspFigure.
 * 
 * returns the children of a @Figure
 * 
 * Returns: 
 **/

static NspList *nsp_figure_children(NspGraphic *Obj)
{
  return  ((NspFigure *) Obj)->obj->children;
}

/**
 * nsp_get_current_axes:
 * @void: 
 * 
 * returns the current axe
 * 
 * Returns: a #NspAxes or %NULL
 **/

static NspAxes *nsp_current_axes=NULL;

static NspAxes *nsp_get_current_axes(void)
{
  NspObject *Obj;
  NspFigure *cf;
  NspList *L;
  if ( nsp_current_axes != NULL) 
    {
      if ( nsp_current_axes->obj->ref_count > 1 )
	return nsp_current_axes;
    }
  cf = nsp_get_current_figure();
  if ( cf == NULL) return NULL;
  L= cf->obj->children;
  /* return the first axes */
  if ( (Obj = nsp_list_get_element(L,1)) ==  NULLOBJ )
    {
      /* maybe we could here build a current axes */
      return NULL;
    }
  nsp_current_axes= (NspAxes *) Obj;
  return nsp_current_axes;
}

/**
 * nsp_create_default_figure:
 * @Xgc: a graphic context 
 * 
 * create and return a #NspFigure connected to @Xgc.
 * 
 * Returns: a new #NspFigure.
 **/

static NspFigure *nsp_create_default_figure(BCG *Xgc)
{
  NspFigure *Fig;
  char *name,*driver;
  /* take care that nsp_figure_create won't allocate its fields 
   * we have to make the copies here 
   */
  if (( name =new_nsp_string("Graphic window")) == NULLSTRING)
    return NULL;
  if (( driver =new_nsp_string("Gtk")) == NULLSTRING)
    return NULL;
  Fig = nsp_figure_create("fig",name,driver,Xgc->CurWindow,NULL,NULL,TRUE,NULL,NULL,
			  TRUE,NULL,Xgc,NULL);
  /* insert in Xgc */
  if ( Fig != NULL) 
    {
      NspObject *obj;
      if ((obj = nsp_object_copy_and_name("Obj",NSP_OBJECT(Fig))) == NULLOBJ ) 
	return NULL;
      Xgc->figure = obj;
    }
  return Fig;
}




/**
 * nsp_check_for_axes:
 * @Xgc: 
 * @wrect: 
 * 
 * 
 * checks for a figure and an axes in Xgc 
 * create one if not present. 
 * Xgc should not be null here. 
 * this is not a definitive function just a hack.
 * since graphic should be driven by Figure Not by Xgc 
 * 
 * Returns: 
 **/

NspAxes * nsp_check_for_axes(BCG *Xgc,const double *wrect)
{
  int created=FALSE;
  NspAxes *Axes= NULL;
  NspFigure  *F = Xgc->figure; 
  if ( F == NULL) 
    {
      /* create a new figure and store it in Xgc */
      F = nsp_create_default_figure(Xgc);
      if ( F == NULL) return NULL;
      created=TRUE;
    }
  if ( ! IsFigure((NspObject *) F)) return NULL;
  Axes = nsp_check_for_axes_in_figure(F,wrect);
  if ( created==TRUE) nsp_figure_destroy(F);
  return Axes;
}


/**
 * nsp_check_for_axes_in_figure:
 * @Xgc: 
 * @wrect: 
 * 
 * 
 * checks for a figure and an axes in Xgc 
 * create one if not present. 
 * Xgc should not be null here. 
 * this is not a definitive function just a hack.
 * since graphic should be driven by Figure Not by Xgc 
 * 
 * Returns: 
 **/

NspAxes * nsp_check_for_axes_in_figure(NspFigure *F,const double *wrect)
{
  int i,l;
  NspObject *Obj=NULLOBJ,*Axes=NULLOBJ;
  NspList *L;
  if ( F == NULL) return NULL;
  L= F->obj->children;
  /* return the first axes found 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && IsAxes(Obj) )
	{
	  Axes= Obj;
	  if ( wrect == NULL) 
	    {
	      break;
	    }
	  if ( Abs(((NspAxes *)Axes)->obj->wrect->R[0]-wrect[0])< 1.e-4 
	       && Abs(((NspAxes *)Axes)->obj->wrect->R[1]-wrect[1])< 1.e-4 
	       && Abs(((NspAxes *)Axes)->obj->wrect->R[2]-wrect[2])< 1.e-4 
	       && Abs(((NspAxes *)Axes)->obj->wrect->R[3]-wrect[3])< 1.e-4 )
	    {
	      break;
	    }
	  Axes = NULL;
	}
    }
  if ( Axes == NULLOBJ) 
    {
      /* create a new axes */
      NspAxes *axe= nsp_axes_create_default("axe");
      if ( axe == NULL) return NULL;
      /* store in Figure */
      if ( nsp_list_begin_insert(L,(NspObject *) axe)== FAIL) 
	{
	  nsp_axes_destroy(axe);
	  return NULL;
	}
      /* set figure informations in axe */
      nsp_figure_children_link_figure(F);
      Axes =(NspObject *) axe;
    }
  return (NspAxes *) Axes;
}


/**
 * nsp_check_for_objs3d:
 * @Xgc: 
 * @wrect: 
 * 
 * 
 * checks for a figure and a 3dobj-axes in Xgc 
 * create one if not present. 
 * Xgc should not be null here. 
 * this is not a definitive function just a hack.
 * since graphic should be driven by Figure Not by Xgc 
 * 
 * Returns: 
 **/

NspObjs3d * nsp_check_for_objs3d(BCG *Xgc,const double *wrect)
{
  int  created=FALSE;
  NspObjs3d *Objs3d=NULL;
  NspFigure  *F = Xgc->figure; 
  if ( F == NULL) 
    {
      /* create a new figure and store it in Xgc */
      F = nsp_create_default_figure(Xgc);
      if ( F == NULL) return NULL;
      created = TRUE;
    }
  if ( ! IsFigure((NspObject *) F)) return NULL;
  Objs3d = nsp_check_for_objs3d_in_figure(F,wrect);
  if ( created==TRUE) nsp_figure_destroy(F);
  return  Objs3d;
}


/**
 * nsp_check_for_objs3d_in_figure:
 * @Xgc: 
 * @wrect: 
 * 
 * 
 * checks for a figure and a 3dobj-axes in Xgc 
 * create one if not present. 
 * Xgc should not be null here. 
 * this is not a definitive function just a hack.
 * since graphic should be driven by Figure Not by Xgc 
 * 
 * Returns: 
 **/

NspObjs3d * nsp_check_for_objs3d_in_figure(NspFigure *F,const double *wrect)
{
  int i,l ;
  NspObject *Obj=NULLOBJ,*Objs3d=NULLOBJ;
  NspList *L;
  if ( F == NULL) return NULL;
  L= F->obj->children;
  /* return the first axes found 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ &&  IsObjs3d(Obj))
	{
	  Objs3d=Obj;
	  if ( wrect == NULL) 
	    {
	      break;
	    }
	  if ( Abs(((NspObjs3d *)Objs3d)->obj->wrect->R[0]-wrect[0])< 1.e-4 
	       && Abs(((NspObjs3d *)Objs3d)->obj->wrect->R[1]-wrect[1])< 1.e-4 
	       && Abs(((NspObjs3d *)Objs3d)->obj->wrect->R[2]-wrect[2])< 1.e-4 
	       && Abs(((NspObjs3d *)Objs3d)->obj->wrect->R[3]-wrect[3])< 1.e-4 )
	    {
	      break;
	    }
	  Objs3d = NULL;
	  break;
	}
    }
  if ( Objs3d == NULLOBJ) 
    {
      /* create a new obj3d */
      NspObjs3d *obj3d= nsp_objs3d_create_default("axe3d");
      if ( obj3d == NULL) return NULL;
      /* store in Figure */
      if ( nsp_list_begin_insert(L,(NspObject *) obj3d)== FAIL) 
	{
	  nsp_objs3d_destroy(obj3d);
	  return NULL;
	}
      /* set figure informations in axe */
      nsp_figure_children_link_figure(F);
      Objs3d =(NspObject *) obj3d;
    }
  return (NspObjs3d *) Objs3d;
}


/**
 * nsp_check_for_axes_or_objs3d:
 * @Xgc: 
 * @wrect: 
 * 
 * 
 * 
 * Returns: 
 **/
NspObject * nsp_check_for_axes_or_objs3d(BCG *Xgc,const double *wrect)
{
  int i,l, created=FALSE;
  NspObject *Obj=NULLOBJ,*Res=NULLOBJ;
  NspList *L;
  NspFigure  *F = (NspFigure *) Xgc->figure;
  if ( F == NULL) 
    {
      /* create a new figure and store it in Xgc */
      F = nsp_create_default_figure(Xgc);
      if ( F == NULL) return NULL;
      created=TRUE;
    }
  if ( ! IsFigure((NspObject *) F)) return NULL;
  L= F->obj->children;
  /* return the first axes or objs3d found 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && IsAxes(Obj))
	{
	  Res =Obj;
	  if ( wrect == NULL) 
	    {
	      break;
	    }
	  if ( Abs(((NspAxes *)Res)->obj->wrect->R[0]-wrect[0])< 1.e-4 
	       && Abs(((NspAxes *)Res)->obj->wrect->R[1]-wrect[1])< 1.e-4 
	       && Abs(((NspAxes *)Res)->obj->wrect->R[2]-wrect[2])< 1.e-4 
	       && Abs(((NspAxes *)Res)->obj->wrect->R[3]-wrect[3])< 1.e-4 )
	    {
	      break;
	    }
	  Res = NULL;
	}
      else if ( Obj != NULLOBJ &&  IsObjs3d(Obj))
	{
	  Res=Obj;
	  if ( wrect == NULL) 
	    {
	      break;
	    }
	  if ( Abs(((NspObjs3d *)Res)->obj->wrect->R[0]-wrect[0])< 1.e-4 
	       && Abs(((NspObjs3d *)Res)->obj->wrect->R[1]-wrect[1])< 1.e-4 
	       && Abs(((NspObjs3d *)Res)->obj->wrect->R[2]-wrect[2])< 1.e-4 
	       && Abs(((NspObjs3d *)Res)->obj->wrect->R[3]-wrect[3])< 1.e-4 )
	    {
	      break;
	    }
	  Res = NULL;
	  break;
	}
	       
    }
  if ( Res == NULLOBJ) 
    {
      /* create a new axes */
      NspAxes *axe= nsp_axes_create_default("axe");
      if ( axe == NULL) return NULL;
      /* store in Figure */
      if ( nsp_list_begin_insert(L,(NspObject *) axe)== FAIL) 
	{
	  nsp_axes_destroy(axe);
	  return NULL;
	}
      /* set figure informations in axe */
      nsp_figure_children_link_figure(F);
      Res =(NspObject *) axe;
    }
  if ( created==TRUE) nsp_figure_destroy(F);
  return Res;
}


/**
 * nsp_check_pt_axes_or_objs3d:
 * @Xgc: 
 * @pt: 
 * 
 * 
 * check if a point is inside an axes or objs3d 
 * 
 * Returns: 
 **/

NspObject * nsp_check_pt_axes_or_objs3d(BCG *Xgc,const int *pt)
{
  int wdim[]= {Xgc->CWindowWidth,  Xgc->CWindowHeight};
  int i,l,  Irect[4];
  NspObject *Obj=NULLOBJ,*Res=NULLOBJ;
  NspList *L;
  NspFigure  *F = (NspFigure *) Xgc->figure;
  if ( F== NULL ||  ! IsFigure((NspObject *) F)) return NULL;

  L= F->obj->children;
  /* return the first axes or objs3d found which contains pt 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && IsAxes(Obj))
	{
	  Res =Obj;
	  /* the figure rectangle in int values */
	  Irect[0]=((NspAxes *) Res)->obj->wrect->R[0]*wdim[0];
	  Irect[1]=((NspAxes *) Res)->obj->wrect->R[1]*wdim[1];
	  Irect[2]=((NspAxes *) Res)->obj->wrect->R[2]*wdim[0];
	  Irect[3]=((NspAxes *) Res)->obj->wrect->R[3]*wdim[1];
	  
	  if ( Irect[0] <= pt[0] && pt[0] <= Irect[0]+Irect[2] 
	       && Irect[1] <= pt[1] && pt[1] <= Irect[1]+Irect[3] )
	    {
	      break;
	    }
	  Res = NULL;
	}
      else if ( Obj != NULLOBJ &&  IsObjs3d(Obj))
	{
	  Res=Obj;
	  /* the figure rectangle in int values */
	  Irect[0]=((NspObjs3d *) Res)->obj->wrect->R[0]*wdim[0];
	  Irect[1]=((NspObjs3d *) Res)->obj->wrect->R[1]*wdim[1];
	  Irect[2]=((NspObjs3d *) Res)->obj->wrect->R[2]*wdim[0];
	  Irect[3]=((NspObjs3d *) Res)->obj->wrect->R[3]*wdim[1];
	  
	  if ( Irect[0] <= pt[0] && pt[0] <= Irect[0]+Irect[2] 
	       && Irect[1] <= pt[1] && pt[1] <= Irect[1]+Irect[3] )
	    {
	      break;
	    }
	  Res = NULL;
	}
	       
    }
  return Res;
}

/**
 * nsp_check_for_axes_or_objs3d_from_pointer:
 * @F: 
 * @obj: 
 * 
 * 
 * 
 * Returns: 
 **/

NspObject *nsp_check_for_axes_or_objs3d_from_pointer(nsp_figure *F,void *obj)
{
  int i,l; 
  NspObject *Obj=NULLOBJ;
  NspList *L;
  if ( F == NULL) return NULL;
  L= F->children;
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && IsAxes(Obj))
	{
	  NspAxes *A = (NspAxes *) Obj;
	  if ( A->obj == obj ) return (NspObject *) A;
	}
      else if ( Obj != NULLOBJ &&  IsObjs3d(Obj))
	{
	  NspObjs3d *A = (NspObjs3d *) Obj;
	  if ( A->obj == obj ) return (NspObject *) A;
	}
    }
  return NULL;
}


/**
 * nsp_figure_draw_latter:
 * @F: 
 * 
 * 
 * 
 * Returns: 
 **/
static int nsp_figure_draw_latter(NspFigure *F)
{
  F->obj->draw_now=FALSE; 
  return OK ;
}

/**
 * nsp_figure_draw_now:
 * @F: 
 * 
 * 
 * 
 * Returns: 
 **/

static int nsp_figure_draw_now(NspFigure *F)
{
  BCG *Xgc;
  if ( F->obj->draw_now == FALSE ) 
    {
      F->obj->draw_now= TRUE;
      /* we were not in draw_now mode 
       * thus all the invalidate have been 
       * lost 
       */
      nsp_figure_invalidate((NspGraphic *) F);
    }
  if ((Xgc= F->obj->Xgc) == NULL) return FAIL;
  /* flush the accumulated expose events */
  Xgc->graphic_engine->process_updates(Xgc);
  return OK ;
}

/**
 * nsp_get_point_axes:
 * @Xgc: 
 * @px: 
 * @py: 
 * @dp: 
 * 
 * get the axes which contains a point (px,py) 
 * and use the axes scale to convert point to double dp[2].
 * This function has the side effect that the axe scale is 
 * set in Xgc.
 * 
 * 
 * Returns: 
 **/

NspGraphic *nsp_get_point_axes(BCG *Xgc,int px,int py,double *dp)
{
  NspGraphic *gr=NULL;
  NspFigure *F=nsp_get_figure(Xgc);
  int ww= Xgc->CWindowWidth, wh = Xgc->CWindowHeight;
  NspList *L;
  Cell *cloc;
  if ( F == NULL) return NULL;
  L= F->obj->children;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  if ( IsAxes(NSP_OBJECT(G)))
	    { 
	      NspAxes *A = (NspAxes *) G;
	      int w = ww*A->obj->wrect->R[2];
	      int h = wh*A->obj->wrect->R[3];
	      int x = ww*A->obj->wrect->R[0];
	      int y = ww*A->obj->wrect->R[1];
	      if (cloc->next==NULL || ( px >= x && px <= x+w && py >= y && py <= y +h))
		{
		  gr = G;
		  nsp_axes_i2f(A->obj,px,py,dp);
		}
	    }
	  else if ( IsObjs3d(NSP_OBJECT(G)))
	    {
	      NspObjs3d *A = (NspObjs3d *) G;
	      int w = ww*A->obj->wrect->R[2];
	      int h = wh*A->obj->wrect->R[3];
	      int x = ww*A->obj->wrect->R[0];
	      int y = ww*A->obj->wrect->R[1];
	      if ( px >= x && px <= x+w && py >= y && py <= y +h)
		{
		  gr = G;
		  /* a revoir XXXXX */
		  set_scale(Xgc->scales,NULL,A->obj->wrect->R,A->obj->frect->R,NULL,NULL,A->obj->arect->R);
		  scale_i2f(Xgc->scales,dp,dp+1,&px,&py,1);
		}
	    }
	}
      cloc = cloc->next;
    }
  return gr;
}


/**
 * nsp_get_figure:
 * @Xgc: 
 * 
 * 
 * 
 * Returns: 
 **/

NspFigure *nsp_get_figure(BCG *Xgc)
{
  NspObject  *F = NULL;
  if ( Xgc == NULL) return NULL;
  F = Xgc->figure;
  if ( F== NULL || ! IsFigure(F)) return NULL;
  return (NspFigure *) F;
}

/**
 * nsp_figure_get_axe_elts_as_compound:
 * @name: 
 * @F: 
 * 
 * get the elements of the first axe as a compound 
 * and remove the elements. 
 * 
 * 
 * Returns: 
 **/
NspCompound *nsp_figure_get_axe_elts_as_compound(char *name,NspFigure *F)
{
  NspAxes *A; 
  NspList *L;
  NspCompound *C;
  if ((C= nsp_compound_create(name,NULL,NULL,NULL))== NULL) return NULL;
  /* unlink the children from the figure */
  /* return the first axes XXX  */
  if ( (A =(NspAxes *) nsp_list_get_element(F->obj->children,1)) ==  NULL )
    return NULL;
  if ( IsAxes(NSP_OBJECT(A)) == FALSE) return NULL;
  nsp_list_unlink_figure(A->obj->children,F->obj);
  /* swap respective children */
  L = C->obj->children;
  C->obj->children = A->obj->children;
  A->obj->children = L;
  return C;
}



/**
 * nsp_figure_start_compound:
 * @F: 
 * 
 * creates a new axe which is inserted in a figure 
 * as the first axe. This axe also becomes the 
 * default axe and its show attribute is set to 
 * %FALSE. Subsequent graphics are collected in 
 * this axe. Using #nsp_figure_end_compound it is 
 * possible to collect all the graphics contained in this
 * axe and store them in a compound.
 *
 * Returns: %OK or %FAIL
 **/

static int count = 0;

static int nsp_figure_start_compound(NspFigure *F)
{
  NspAxes *axe= nsp_axes_create_default("axe");
  if ( axe == NULL) return FAIL;
  /* store in Figure */

  if (count == 0 )
    {
      count = 1;
    }
  else
    {
      Sciprintf("Recursive call to nsp_figure_start_compound\n");
      return FAIL;
    }
  
  if ( nsp_list_begin_insert(F->obj->children,(NspObject *) axe)== FAIL) 
    {
      nsp_axes_destroy(axe);
      return FAIL;
    }
  /* set figure informations in axe */
  ((NspGraphic *) axe)->type->link_figure((NspGraphic *) axe,F->obj, axe->obj);
  /* set the axe drawing mode to false */
  ((NspGraphic *) axe)->obj->show = FALSE;
  return OK;
}

/**
 * nsp_figure_end_compound:
 * @name: 
 * @F: 
 * 
 * 
 * get the elements in the first axe 
 * put them in a compound. 
 * Then: 
 *  if the axe is unique store the compound in the axe
 *  if an other axe exists store the compound on the next 
 *  axe and remove the first one. 
 *
 * 
 * Returns: 
 **/

static NspCompound *nsp_figure_end_compound(char *name,NspFigure *F)
{
  NspAxes *A1,*A2;
  NspList *L;
  NspCompound *C;

  if ( count == 0 )
    {
      Sciprintf("ending a non started compound\n");
    }
  else
    {
      count = 0;
    }

  if ((C= nsp_compound_create(name,NULL,NULL,NULL))== NULL) return NULL;
  /*
   * the first child of figure is the axe that must be converted to 
   * compound.
   */
  if ( (A1 =(NspAxes *) nsp_list_get_element(F->obj->children,1)) ==  NULL )
    return NULL;
  if ( IsAxes(NSP_OBJECT(A1)) == FALSE) return NULL;
  /*
   * mv the axe children in the compound.
   */
  L = C->obj->children;
  C->obj->children = A1->obj->children;
  A1->obj->children = L;
  if ( (A2 =(NspAxes *) nsp_list_get_element(F->obj->children,2)) ==  NULL )
    {
      /* if we just have one axe we insert the new compound in 
       * the same axe.
       */
      if ( nsp_axes_insert_child(A1, (NspGraphic *) C, TRUE) == FAIL) 
	return NULL;
    }
  else
    {
      /* insert the compound in A2 */
      if ( nsp_axes_insert_child(A2, (NspGraphic *) C, TRUE) == FAIL) 
	return NULL;
      /* remove A1 */
      nsp_list_remove_first(F->obj->children);
    }
  return C;
}



/**
 * nsp_list_delete_graphic_obj:
 * @F: 
 * @L: 
 * @Obj: 
 * 
 * Remove a graphic object in a Figure 
 * by exploring the axes
 * 
 * 
 * Returns: 
 **/

static int nsp_list_delete_graphic_obj(NspFigure *F,NspList *L, NspGraphic *Obj)
{
  Cell *Loc = L->first;
  while ( Loc != NULLCELL ) 
    {
      if ( Loc->O != NULLOBJ )
	{
	  NspGraphic *G1= (NspGraphic *) Loc->O;
	  if ( G1->obj == Obj->obj ) 
	    {
	      G1->type->unlink_figure(G1,F->obj);
	      nsp_remove_cell_from_list(L, Loc);
	      L->icurrent = 0;
	      L->current = NULLCELL;
	      nsp_cell_destroy(&Loc);
	      return OK;
	    }
	}
      Loc = Loc->next;
    }
  return FAIL;
}

/**
 * nsp_figure_remove_element:
 * @F: 
 * @Obj: 
 * 
 * 
 * 
 * Returns: 
 **/

static int nsp_figure_remove_element(NspFigure *F,NspGraphic *Obj)
{
  NspList *L= F->obj->children;
  Cell *cloc  = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  if ( IsAxes(NSP_OBJECT(G)))
	    { 
	      NspAxes *A = (NspAxes *) G;
	      NspList *L1=  A->obj->children;
	      if (nsp_list_delete_graphic_obj(F,L1, Obj)==OK) return OK;
	    }
	  else if ( IsObjs3d(NSP_OBJECT(G)))
	    {
	      NspObjs3d *A = (NspObjs3d *) G;
	      NspList *L1=  A->obj->children;
	      if ( nsp_list_delete_graphic_obj(F,L1, Obj)==OK) return OK;
	    }
	}
      cloc = cloc->next;
    }
  return FAIL;
}

/**
 * nsp_figure_invalidate:
 * @G: 
 * 
 * invalidate the drawing region associated to a
 * figure. 
 * 
 **/

void nsp_figure_invalidate(NspGraphic *G)
{
  NspFigure *F = (NspFigure *) G;
  BCG *Xgc;
  if ( F == NULL ) return ;
  if ((Xgc= F->obj->Xgc) == NULL) return ;
  if ( F->obj->draw_now== FALSE) return;
  Xgc->graphic_engine->invalidate(Xgc,NULL);
}



/**
 * nsp_figure_set_gc_values:
 * @F: 
 * 
 * used FigureData to set up values in 
 * Xgc 
 **/

static void nsp_figure_set_gc_values(NspFigure *F)
{
  NspFigureData *Gc = F->obj->gc;
  BCG *Xgc =  F->obj->Xgc;  
  Gc = F->obj->gc;
  if ( Xgc == NULL) return;
  Xgc->graphic_engine->xset_pattern(Xgc,Gc->color);
  if ( Gc->background != -1 ) 
    Xgc->graphic_engine->xset_background(Xgc,Gc->background);
  else
    Gc->background= Xgc->graphic_engine->xget_background(Xgc);
  if ( Gc->foreground != -1 ) 
    Xgc->graphic_engine->xset_foreground(Xgc,Gc->foreground);
  else
    Gc->foreground= Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_dash(Xgc,Gc->dashes);
  Xgc->graphic_engine->xset_font(Xgc,Gc->font,Gc->font_size, FALSE);
  Xgc->graphic_engine->xset_hidden3d(Xgc,Gc->hidden3d);
  Xgc->graphic_engine->xset_absourel(Xgc,Gc->line_mode);
  Xgc->graphic_engine->xset_dash(Xgc,Gc->line_style);
  Xgc->graphic_engine->xset_mark(Xgc,Gc->mark,Gc->mark_size);
  Xgc->graphic_engine->xset_pixmapOn(Xgc,Gc->pixmap);
  Xgc->graphic_engine->xset_thickness(Xgc,Gc->thickness);
  Xgc->graphic_engine->xset_usecolor(Xgc,Gc->use_color);
  Xgc->graphic_engine->xset_autoclear(Xgc,Gc->auto_clear);
}

/**
 * nsp_figure_initialize_gc:
 * @F: 
 * 
 * initialize some values in FigureData 
 * which are not properly initialized at creation.
 **/

void nsp_figure_initialize_gc(NspFigure *F)
{
  NspFigureData *Gc = F->obj->gc;
  BCG *Xgc =  F->obj->Xgc;  
  Gc = F->obj->gc;
  if ( Xgc != NULL) 
    {
      if ( Gc->background == -1 ) 
	Gc->background = Xgc->graphic_engine->xget_background(Xgc);
      if ( Gc->foreground == -1 ) 
	Gc->foreground = Xgc->graphic_engine->xget_foreground(Xgc);
    }
}

/**
 * nsp_figure_data_set_colormap:
 * @Xgc: 
 * @Mc: 
 * 
 * @Mc is not copied here and should contains a colormap + 3 colors.
 **/

void nsp_figure_data_set_colormap(NspFigure *F,NspMatrix *Mc) 
{
  NspFigureData *Gc = F->obj->gc;
  BCG *Xgc =  F->obj->Xgc;  
  int m = Mc->m;
  if ( Gc->colormap != NULL) nsp_matrix_destroy(Gc->colormap);
  Gc->colormap = Mc; 
  Gc->foreground = m+1;
  Gc->background = m+2;
  if ( Xgc != NULL ) 
    {
      Xgc->graphic_engine->xset_usecolor(Xgc,1);
      Xgc->graphic_engine->xset_foreground(Xgc,-1);
      Xgc->graphic_engine->xset_foreground(Xgc,Gc->foreground);
      Xgc->graphic_engine->xset_background(Xgc,-1);
      Xgc->graphic_engine->xset_background(Xgc,Gc->background);
      Xgc->graphic_engine->xset_pattern(Xgc,-1);
      Xgc->graphic_engine->xset_pattern(Xgc,Gc->foreground);
    }
}

/**
 * nsp_figure_data_reset:
 * @F: a #NspFigure
 * 
 * reset Figure data to default values 
 **/

void nsp_figure_data_reset(NspFigure *F)
{
  NspFigureData *Gc = F->obj->gc;
  if ( Gc->colormap != NULL) 
    nsp_matrix_destroy(Gc->colormap);
  /* in order to use default colormap */
  Gc->colormap = nsp_matrix_create("colormap",'r',0,0);
  Gc->foreground = -1;
  Gc->background = -1;
  Gc->color= 1;
  Gc->dashes=  1;
  Gc->font=  2;
  Gc->font_size=  1;
  Gc->hidden3d=  4;
  Gc->line_mode=  0;
  Gc->line_style=  1;
  Gc->mark=  0;
  Gc->mark_size=  0;
  Gc->pattern=  1;
  Gc->pixmap=  0;
  Gc->thickness=  1;
  Gc->use_color=  1;
  Gc->auto_clear= FALSE;
}


/**
 * nsp_figure_process_updates:
 * @F: a #NspFigure 
 * 
 * call process_updates on the graphic window if any 
 * associated to figure @F.
 *
 **/
static void nsp_figure_process_updates(NspFigure *F)
{
  BCG *Xgc =  F->obj->Xgc;  
  if ( Xgc == NULL ) return ;
  Xgc->graphic_engine->process_updates(Xgc);
}

/**
 * nsp_check_for_figure :
 * @Xgc: 
 * 
 * check if a figure is present in @Xgc.
 * if not figure is created and set as current if 
 * set_current is %TRUE.
 * 
 * Returns: a new Figure or %NULL
 **/

NspFigure *nsp_check_for_figure(BCG *Xgc,int set_current)
{
  NspFigure *F;
  if ( Xgc == NULL ) return NULL;
  if ( Xgc->figure != NULL) 
    {
      if ( set_current == TRUE )
	{
	  if ( nsp_set_current_figure(Xgc->figure) == FAIL) return NULL;
	}
      return Xgc->figure; 
    }
  F = nsp_create_default_figure(Xgc);
  if ( F == NULL) return NULL;
  if ( set_current == TRUE )
    {
      if (  nsp_set_current_figure(F) == FAIL) return NULL;
    }
  /* destroy the local copy */
  nsp_figure_destroy(F);
  F = nsp_get_current_figure();
  return F;
}

/**
 * nsp_check_for_current_figure :
 * @void: 
 * 
 * returns the current figure, creating 
 * the figure if necessary.
 * 
 * Returns: 
 **/

NspFigure *nsp_check_for_current_figure(void)
{
  NspFigure *F;
  F = nsp_get_current_figure(); 
  if ( F == NULL) 
    {
      /* create a new figure and store it in Xgc */
      BCG *Xgc=nsp_check_graphic_context();
      F = nsp_check_for_figure(Xgc,TRUE);
    }
  return F;
}

/**
 * nsp_check_for_current_axes:
 * @void: 
 * 
 * returns the current axe and create one 
 * if necessary.
 * 
 * Returns: a new #NspAxes or %NULL.
 **/

NspAxes *nsp_check_for_current_axes(void)
{
  int l,i;
  NspObject *Obj;
  NspList *L;
  NspFigure *F;
  NspAxes *Axes= NULLAXES;
  F = nsp_check_for_current_figure(); 
  if ( F == NULL) return NULL;
  L= F->obj->children;
  /* return the first axes found 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && IsAxes(Obj) )
	{
	  Axes=(NspAxes *) Obj;
	  break;
	}
    }
  if ( Axes == NULLAXES ) 
    {
      /* create a new axes */
      Axes= nsp_axes_create_default("axe");
      if ( Axes == NULL) return NULL;
      /* store in Figure */
      if ( nsp_list_begin_insert(L,(NspObject *) Axes)== FAIL) 
	{
	  nsp_axes_destroy(Axes);
	  return NULL;
	}
      /* set figure informations in axe */
      nsp_figure_children_link_figure(F);
    }
  return Axes;
}

/**
 * nsp_check_for_current_objs3d:
 * @void: 
 * 
 * returns the current objs3d and create one 
 * if necessary.
 * 
 * Returns: a new #NspObjs3d or %NULL.
 **/

NspObjs3d *nsp_check_for_current_objs3d(void)
{
  int l,i;
  NspObject *Obj;
  NspList *L;
  NspFigure *F;
  NspObjs3d *Objs3d = NULL;
  F = nsp_check_for_current_figure(); 
  if ( F == NULL) return NULL;
  L= F->obj->children;
  /* return the first axes found 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && IsObjs3d(Obj) )
	{
	  Objs3d=(NspObjs3d *) Obj;
	  break;
	}
    }
  if ( Objs3d == NULLOBJS3D ) 
    {
      /* create a new axes */
      Objs3d= nsp_objs3d_create_default("objs3d");
      if ( Objs3d == NULL) return NULL;
      /* store in Figure */
      if ( nsp_list_begin_insert(L,(NspObject *) Objs3d)== FAIL) 
	{
	  nsp_objs3d_destroy(Objs3d);
	  return NULL;
	}
      /* set figure informations in axe */
      nsp_figure_children_link_figure(F);
    }
  return Objs3d;
}
/**
 * nsp_check_for_current_axes_or_objs3d:
 * @void: 
 * 
 * returns the current objs3d and create one 
 * if necessary.
 * 
 * Returns: a new #NspObjs3d or %NULL.
 **/

NspObject *nsp_check_for_current_axes_or_objs3d(void)
{
  int l,i;
  NspObject *Obj;
  NspList *L;
  NspFigure *F;
  NspObject *Ret = NULL;
  F = nsp_check_for_current_figure(); 
  if ( F == NULL) return NULL;
  L= F->obj->children;
  /* return the first axes found 
   * Note that the children of a Figure should be axes or 3daxes (obj3d)
   */
  l= nsp_list_length(L);
  for ( i= 1; i <= l ; i++) 
    {
      Obj = nsp_list_get_element(L,i);
      if ( Obj != NULLOBJ && ( IsObjs3d(Obj) || IsAxes(Obj)) )
	{
	  Ret = Obj;
	  break;
	}
    }
  if ( Ret == NULLOBJ ) 
    {
      /* create a new axes */
      NspAxes *Axes =  nsp_axes_create_default("axe");
      if ( Axes == NULL) return NULL;
      /* store in Figure */
      if ( nsp_list_begin_insert(L,(NspObject *) Axes)== FAIL) 
	{
	  nsp_axes_destroy(Axes);
	  return NULL;
	}
      /* set figure informations in axe */
      nsp_figure_children_link_figure(F);
      Ret = (NspObject *) Axes;
    }
  return Ret;
}


void nsp_send_scale_2D_to_opengl(BCG *Xgc)
{
#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
    }
#endif
}

void nsp_send_scale_3D_to_opengl(BCG *Xgc)
{
#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_3dview(Xgc);
    }
#endif
}


/**
 * nsp_figure_remove_children:
 * @F: 
 * 
 * 
 * 
 * Returns: 
 **/

int nsp_figure_remove_children(NspFigure *F)
{
  if (F->obj->children != NULL ) 
    {
      nsp_figure_children_unlink_figure(F);
      nsp_list_destroy(F->obj->children);
    }
  if ((F->obj->children = nsp_list_create("children")) == NULLLIST)
    return FAIL;  
  else 
    return OK;
}


#line 3676 "figure.c"
