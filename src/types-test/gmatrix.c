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





#line 19 "codegen/gmatrix.override"

#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include <nsp/axes.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 38 "gmatrix.c"

/* ----------- NspGMatrix ----------- */


#define  NspGMatrix_Private 
#include <nsp/object.h>
#include <nsp/gmatrix.h>
#include <nsp/interf.h>

/* 
 * NspGMatrix inherits from Graphic 
 */

int nsp_type_gmatrix_id=0;
NspTypeGMatrix *nsp_type_gmatrix=NULL;

/*
 * Type object for NspGMatrix 
 * all the instance of NspTypeGMatrix share the same id. 
 * nsp_type_gmatrix: is an instance of NspTypeGMatrix 
 *    used for objects of NspGMatrix type (i.e built with new_gmatrix) 
 * other instances are used for derived classes 
 */
NspTypeGMatrix *new_type_gmatrix(type_mode mode)
{
  NspTypeGMatrix *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmatrix;
    }
  if (( type =  malloc(sizeof(NspTypeGMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmatrix_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmatrix_get_methods;
  type->new = (new_func *) new_gmatrix;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmatrix */ 

  top->pr = (print_func *) nsp_gmatrix_print;
  top->dealloc = (dealloc_func *) nsp_gmatrix_destroy;
  top->copy  =  (copy_func *) nsp_gmatrix_copy;
  top->size  = (size_func *) nsp_gmatrix_size;
  top->s_type =  (s_type_func *) nsp_gmatrix_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmatrix_type_short_string;
  top->info = (info_func *) nsp_gmatrix_info;
  /* top->is_true = (is_true_func  *) nsp_gmatrix_is_true; */
  /* top->loop =(loop_func *) nsp_gmatrix_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gmatrix_object;
  top->eq  = (eq_func *) nsp_gmatrix_eq;
  top->neq  = (eq_func *) nsp_gmatrix_neq;
  top->save  = (save_func *) nsp_gmatrix_xdr_save;
  top->load  = (load_func *) nsp_gmatrix_xdr_load;
  top->create = (create_func*) int_gmatrix_create;
  top->latex = (print_func *) nsp_gmatrix_latex;
  top->full_copy = (copy_func *) nsp_gmatrix_full_copy;

  /* specific methods for gmatrix */

  type->init = (init_func *) init_gmatrix;

#line 35 "codegen/gmatrix.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_gmatrix;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_gmatrix ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_gmatrix  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_gmatrix  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_gmatrix  ;
  /* next method are defined in NspGraphic and need not be chnaged here for GMatrix */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 121 "gmatrix.c"
  /* 
   * NspGMatrix interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMatrix called nsp_type_gmatrix
       */
      type->id =  nsp_type_gmatrix_id = nsp_new_type_id();
      nsp_type_gmatrix = type;
      if ( nsp_register_type(nsp_type_gmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gmatrix(mode);
    }
  else 
    {
      type->id = nsp_type_gmatrix_id;
      return type;
    }
}

/*
 * initialize NspGMatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmatrix(NspGMatrix *Obj,NspTypeGMatrix *type)
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
 * new instance of NspGMatrix 
 */

NspGMatrix *new_gmatrix() 
{
  NspGMatrix *loc;
  /* type must exists */
  nsp_type_gmatrix = new_type_gmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspGMatrix)))== NULLGMATRIX) return loc;
  /* initialize object */
  if ( init_gmatrix(loc,nsp_type_gmatrix) == FAIL) return NULLGMATRIX;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMatrix 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gmatrix_size(NspGMatrix *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gmatrix_type_name[]="GMatrix";
static char gmatrix_short_type_name[]="gmatrix";

static char *nsp_gmatrix_type_as_string(void)
{
  return(gmatrix_type_name);
}

static char *nsp_gmatrix_type_short_string(NspObject *v)
{
  return(gmatrix_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gmatrix_eq(NspGMatrix *A, NspObject *B)
{
  NspGMatrix *loc = (NspGMatrix *) B;
  if ( check_cast(B,nsp_type_gmatrix_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->data)->type->eq(A->obj->data,loc->obj->data) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->rect)->type->eq(A->obj->rect,loc->obj->rect) == FALSE ) return FALSE;
  if ( A->obj->remap != loc->obj->remap) return FALSE;
  if ( NSP_OBJECT(A->obj->colminmax)->type->eq(A->obj->colminmax,loc->obj->colminmax) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->zminmax)->type->eq(A->obj->zminmax,loc->obj->zminmax) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_gmatrix_neq(NspGMatrix *A, NspObject *B)
{
  return ( nsp_gmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gmatrix_xdr_save(XDR *xdrs, NspGMatrix *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gmatrix)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->data)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->rect)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->remap) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colminmax)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->zminmax)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGMatrix  *nsp_gmatrix_xdr_load_partial(XDR *xdrs, NspGMatrix *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_gmatrix))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->data =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->rect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->remap) == FAIL) return NULL;
  if ((M->obj->colminmax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->zminmax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspGMatrix  *nsp_gmatrix_xdr_load(XDR *xdrs)
{
  NspGMatrix *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGMATRIX;
  if ((H  = nsp_gmatrix_create_void(name,(NspTypeBase *) nsp_type_gmatrix))== NULLGMATRIX) return H;
  if ((H  = nsp_gmatrix_xdr_load_partial(xdrs,H))== NULLGMATRIX) return H;
  if ( nsp_gmatrix_check_values(H) == FAIL) return NULLGMATRIX;
  return H;
}

/*
 * delete 
 */

void nsp_gmatrix_destroy_partial(NspGMatrix *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->data != NULL ) 
      nsp_matrix_destroy(H->obj->data);
    if ( H->obj->rect != NULL ) 
      nsp_matrix_destroy(H->obj->rect);
    if ( H->obj->colminmax != NULL ) 
      nsp_matrix_destroy(H->obj->colminmax);
    if ( H->obj->zminmax != NULL ) 
      nsp_matrix_destroy(H->obj->zminmax);
    FREE(H->obj);
   }
}

void nsp_gmatrix_destroy(NspGMatrix *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gmatrix_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gmatrix_info(NspGMatrix *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGMATRIX) 
    {
      Sciprintf("Null Pointer NspGMatrix \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gmatrix_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gmatrix_print(NspGMatrix *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGMATRIX) 
    {
      Sciprintf("Null Pointer NspGMatrix \n");
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
          nsp_gmatrix_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gmatrix_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->data != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->data),indent+2,"data",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->rect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->rect),indent+2,"rect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"remap	= %s\n", ( M->obj->remap == TRUE) ? "T" : "F" );
  if ( M->obj->colminmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colminmax),indent+2,"colminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zminmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->zminmax),indent+2,"zminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gmatrix_latex(NspGMatrix *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gmatrix_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->data != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->data),indent+2,"data",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->rect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->rect),indent+2,"rect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"remap	= %s\n", ( M->obj->remap == TRUE) ? "T" : "F" );
  if ( M->obj->colminmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colminmax),indent+2,"colminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zminmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->zminmax),indent+2,"zminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMatrix objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMatrix   *nsp_gmatrix_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gmatrix_id) == TRUE ) return ((NspGMatrix *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmatrix));
  return NULL;
}

int IsGMatrixObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmatrix_id);
}

int IsGMatrix(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmatrix_id);
}

NspGMatrix  *GetGMatrixCopy(Stack stack, int i)
{
  if (  GetGMatrix(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMatrix  *GetGMatrix(Stack stack, int i)
{
  NspGMatrix *M;
  if (( M = nsp_gmatrix_object(NthObj(i))) == NULLGMATRIX)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGMatrix instance 
 *-----------------------------------------------------*/

static NspGMatrix *nsp_gmatrix_create_void(char *name,NspTypeBase *type)
{
 NspGMatrix *H  = (type == NULL) ? new_gmatrix() : type->new();
 if ( H ==  NULLGMATRIX)
  {
   Sciprintf("No more memory\n");
   return NULLGMATRIX;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGMATRIX;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gmatrix_create_partial(NspGMatrix *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_gmatrix)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->data = NULLMAT;
  H->obj->rect = NULLMAT;
  H->obj->remap = TRUE;
  H->obj->colminmax = NULLMAT;
  H->obj->zminmax = NULLMAT;
  return OK;
}

int nsp_gmatrix_check_values(NspGMatrix *H)
{
  if ( H->obj->data == NULLMAT) 
    {
       if (( H->obj->data = nsp_matrix_create("data",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->rect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->rect = nsp_matrix_create("rect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->rect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->colminmax == NULLMAT) 
    {
       if (( H->obj->colminmax = nsp_matrix_create("colminmax",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->zminmax == NULLMAT) 
    {
       if (( H->obj->zminmax = nsp_matrix_create("zminmax",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGMatrix *nsp_gmatrix_create(char *name,NspMatrix* data,NspMatrix* rect,gboolean remap,NspMatrix* colminmax,NspMatrix* zminmax,NspTypeBase *type)
{
 NspGMatrix *H  = nsp_gmatrix_create_void(name,type);
 if ( H ==  NULLGMATRIX) return NULLGMATRIX;
  if ( nsp_gmatrix_create_partial(H) == FAIL) return NULLGMATRIX;
  H->obj->data= data;
  H->obj->rect= rect;
  H->obj->remap=remap;
  H->obj->colminmax= colminmax;
  H->obj->zminmax= zminmax;
 if ( nsp_gmatrix_check_values(H) == FAIL) return NULLGMATRIX;
 return H;
}


NspGMatrix *nsp_gmatrix_create_default(char *name)
{
 NspGMatrix *H  = nsp_gmatrix_create_void(name,NULL);
 if ( H ==  NULLGMATRIX) return NULLGMATRIX;
  if ( nsp_gmatrix_create_partial(H) == FAIL) return NULLGMATRIX;
 if ( nsp_gmatrix_check_values(H) == FAIL) return NULLGMATRIX;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGMatrix *nsp_gmatrix_copy_partial(NspGMatrix *H,NspGMatrix *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGMatrix *nsp_gmatrix_copy(NspGMatrix *self)
{
  NspGMatrix *H  =nsp_gmatrix_create_void(NVOID,(NspTypeBase *) nsp_type_gmatrix);
  if ( H ==  NULLGMATRIX) return NULLGMATRIX;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGMATRIX;
  if ( nsp_gmatrix_copy_partial(H,self)== NULL) return NULLGMATRIX;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGMatrix *nsp_gmatrix_full_copy_partial(NspGMatrix *H,NspGMatrix *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_gmatrix))) == NULL) return NULLGMATRIX;
  H->obj->ref_count=1;
  if ( self->obj->data == NULL )
    { H->obj->data = NULL;}
  else
    {
      if ((H->obj->data = (NspMatrix *) nsp_object_full_copy_and_name("data",NSP_OBJECT(self->obj->data))) == NULLMAT) return NULL;
    }
  if ( self->obj->rect == NULL )
    { H->obj->rect = NULL;}
  else
    {
      if ((H->obj->rect = (NspMatrix *) nsp_object_full_copy_and_name("rect",NSP_OBJECT(self->obj->rect))) == NULLMAT) return NULL;
    }
  H->obj->remap=self->obj->remap;
  if ( self->obj->colminmax == NULL )
    { H->obj->colminmax = NULL;}
  else
    {
      if ((H->obj->colminmax = (NspMatrix *) nsp_object_full_copy_and_name("colminmax",NSP_OBJECT(self->obj->colminmax))) == NULLMAT) return NULL;
    }
  if ( self->obj->zminmax == NULL )
    { H->obj->zminmax = NULL;}
  else
    {
      if ((H->obj->zminmax = (NspMatrix *) nsp_object_full_copy_and_name("zminmax",NSP_OBJECT(self->obj->zminmax))) == NULLMAT) return NULL;
    }
  return H;
}

NspGMatrix *nsp_gmatrix_full_copy(NspGMatrix *self)
{
  NspGMatrix *H  =nsp_gmatrix_create_void(NVOID,(NspTypeBase *) nsp_type_gmatrix);
  if ( H ==  NULLGMATRIX) return NULLGMATRIX;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGMATRIX;
  if ( nsp_gmatrix_full_copy_partial(H,self)== NULL) return NULLGMATRIX;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGMatrix
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gmatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGMatrix *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gmatrix is initialized */
  nsp_type_gmatrix = new_type_gmatrix(T_BASE);
  if(( H = nsp_gmatrix_create_void(NVOID,(NspTypeBase *) nsp_type_gmatrix)) == NULLGMATRIX) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_gmatrix_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gmatrix_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gmatrix_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gmatrix_get_data(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix *) self)->obj->data;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix_get_obj_data(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix *) self)->obj->data);
  return (NspObject *) ret;
}

static int _wrap_gmatrix_set_data(void *self,const char *attr, NspObject *O)
{
  NspMatrix *data;

  if ( ! IsMat(O) ) return FAIL;
  if ((data = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix *) self)->obj->data != NULL ) 
    nsp_matrix_destroy(((NspGMatrix *) self)->obj->data);
  ((NspGMatrix *) self)->obj->data= data;
  return OK;
}

static NspObject *_wrap_gmatrix_get_rect(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix *) self)->obj->rect;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix_get_obj_rect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix *) self)->obj->rect);
  return (NspObject *) ret;
}

static int _wrap_gmatrix_set_rect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *rect;

  if ( ! IsMat(O) ) return FAIL;
  if ((rect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix *) self)->obj->rect != NULL ) 
    nsp_matrix_destroy(((NspGMatrix *) self)->obj->rect);
  ((NspGMatrix *) self)->obj->rect= rect;
  return OK;
}

static NspObject *_wrap_gmatrix_get_remap(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGMatrix *) self)->obj->remap;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_gmatrix_set_remap(void *self,const char *attr, NspObject *O)
{
  int remap;

  if ( BoolScalar(O,&remap) == FAIL) return FAIL;
  ((NspGMatrix *) self)->obj->remap= remap;
  return OK;
}

static NspObject *_wrap_gmatrix_get_colminmax(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix *) self)->obj->colminmax;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix_get_obj_colminmax(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix *) self)->obj->colminmax);
  return (NspObject *) ret;
}

static int _wrap_gmatrix_set_colminmax(void *self,const char *attr, NspObject *O)
{
  NspMatrix *colminmax;

  if ( ! IsMat(O) ) return FAIL;
  if ((colminmax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix *) self)->obj->colminmax != NULL ) 
    nsp_matrix_destroy(((NspGMatrix *) self)->obj->colminmax);
  ((NspGMatrix *) self)->obj->colminmax= colminmax;
  return OK;
}

static NspObject *_wrap_gmatrix_get_zminmax(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix *) self)->obj->zminmax;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix_get_obj_zminmax(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix *) self)->obj->zminmax);
  return (NspObject *) ret;
}

static int _wrap_gmatrix_set_zminmax(void *self,const char *attr, NspObject *O)
{
  NspMatrix *zminmax;

  if ( ! IsMat(O) ) return FAIL;
  if ((zminmax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix *) self)->obj->zminmax != NULL ) 
    nsp_matrix_destroy(((NspGMatrix *) self)->obj->zminmax);
  ((NspGMatrix *) self)->obj->zminmax= zminmax;
  return OK;
}

static AttrTab gmatrix_attrs[] = {
  { "data", (attr_get_function *)_wrap_gmatrix_get_data, (attr_set_function *)_wrap_gmatrix_set_data,(attr_get_object_function *)_wrap_gmatrix_get_obj_data, (attr_set_object_function *)int_set_object_failed },
  { "rect", (attr_get_function *)_wrap_gmatrix_get_rect, (attr_set_function *)_wrap_gmatrix_set_rect,(attr_get_object_function *)_wrap_gmatrix_get_obj_rect, (attr_set_object_function *)int_set_object_failed },
  { "remap", (attr_get_function *)_wrap_gmatrix_get_remap, (attr_set_function *)_wrap_gmatrix_set_remap,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "colminmax", (attr_get_function *)_wrap_gmatrix_get_colminmax, (attr_set_function *)_wrap_gmatrix_set_colminmax,(attr_get_object_function *)_wrap_gmatrix_get_obj_colminmax, (attr_set_object_function *)int_set_object_failed },
  { "zminmax", (attr_get_function *)_wrap_gmatrix_get_zminmax, (attr_set_function *)_wrap_gmatrix_set_zminmax,(attr_get_object_function *)_wrap_gmatrix_get_obj_zminmax, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 55 "codegen/gmatrix.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_gmatrix(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 784 "gmatrix.c"


#line 65 "codegen/gmatrix.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_gmatrix(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 796 "gmatrix.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GMatrix_func[]={
  {"extractelts_gmatrix", _wrap_nsp_extractelts_gmatrix},
  {"setrowscols_gmatrix", _wrap_nsp_setrowscols_gmatrix},
  { "gmatrix_create", int_gmatrix_create},
  { NULL, NULL}
};

/* call ith function in the GMatrix interface */

int GMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GMatrix_func[i].name;
  *f = GMatrix_func[i].fonc;
}

#line 75 "codegen/gmatrix.override"

/* inserted verbatim at the end */

static void nsp_draw_gmatrix(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspGMatrix *P = (NspGMatrix *) Obj;
  int remap = P->obj->remap; 
  double xx[]={ P->obj->rect->R[0],P->obj->rect->R[2]};
  double yy[]={ P->obj->rect->R[1],P->obj->rect->R[3]};
  int xx1[2],yy1[2];
  int *xm,*ym,  j;
  int colminmax[2];
  double *zminmax = NULL;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  if ( P->obj->colminmax->mn == 2 ) 
    {
      colminmax[0] = P->obj->colminmax->R[0];
      colminmax[1] = P->obj->colminmax->R[1];
    }
  if ( P->obj->zminmax->mn == 2 ) 
    zminmax = P->obj->zminmax->R;
  else
    remap = FALSE;
  
  if  (  Xgc->scales->cosa==1.0 ) 
    {
      /* Boundaries of the matrix rectangle in pixel */
      scale_f2i(Xgc,xx,yy,xx1,yy1,2);
      xm = graphic_alloc(0,P->obj->data->n+1,sizeof(int));
      ym = graphic_alloc(1,P->obj->data->m+1,sizeof(int));
      if ( xm == 0 || ym == 0 )
	{
	  Scistring("Xgray: running out of memory\n");
	  return ; 
	}
      for ( j =0 ; j < (P->obj->data->n+1) ; j++)	 
	xm[j]= (int) (( xx1[1]*j + xx1[0]*(P->obj->data->n-j) )/((double) P->obj->data->n));
      for ( j =0 ; j < (P->obj->data->m+1) ; j++)	 
	ym[j]= (int) (( yy1[0]*j + yy1[1]*(P->obj->data->m-j) )/((double) P->obj->data->m));
      Xgc->graphic_engine->fill_grid_rectangles1(Xgc,xm,ym,P->obj->data->R,
						 P->obj->data->m, 
						 P->obj->data->n,
						 remap,
						 colminmax,
						 zminmax);
    }
  else
    {
      double xp[4],yp[4];
      const double *z =P->obj->data->R;
      int nr =P->obj->data->m , nc=P->obj->data->n;
      int colmin,colmax;
      double zmin,zmax,coeff;
      int i,j,fill[1],cpat,xz[2];
      cpat = Xgc->graphic_engine->xget_pattern(Xgc);
      Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
      nsp_remap_colors(Xgc,remap,&colmin,&colmax,&zmin,&zmax,&coeff,colminmax,zminmax,z,nr*nc);
      
      for (i = 0 ; i < nc-1 ; i++)
	for (j = 0 ; j < nr-1 ; j++)
	  {
	    fill[0]= (remap == FALSE) ? rint(z[i+nr*j]) : rint((colmax-colmin)*(z[i+nr*j] - zmin)*coeff + colmin);
	    /* do not draw rectangles which are outside the colormap range */
	    if ( fill[0] < colmin || fill[0] > colmax ) continue ;
	    Xgc->graphic_engine->xset_pattern(Xgc,fill[0]);
	    xp[0]= (( xx[1]*i + xx[0]*(P->obj->data->n-i) )/((double) P->obj->data->n));
	    yp[0]= (( yy[0]*j + yy[1]*(P->obj->data->m-j) )/((double) P->obj->data->m));
	    xp[1]= xp[0];
	    yp[1]= (( yy[0]*(j+1) + yy[1]*(P->obj->data->m-(j+1)) )/((double) P->obj->data->m));
	    xp[2]= (( xx[1]*(i+1) + xx[0]*(P->obj->data->n-(i+1)) )/((double) P->obj->data->n));
	    yp[2]= yp[1];
	    xp[3]= xp[2];
	    yp[3]= yp[0];
	    Xgc->graphic_engine->scale->fillpolyline(Xgc,xp,yp,4,1);
	  }
      Xgc->graphic_engine->xset_pattern(Xgc,cpat);
    }
  
}



static void nsp_translate_gmatrix(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspGMatrix *P = (NspGMatrix *) Obj;
  P->obj->rect->R[0] += tr[0];
  P->obj->rect->R[2] += tr[0];
  P->obj->rect->R[1] += tr[1];
  P->obj->rect->R[3] += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_gmatrix(BCG *Xgc,NspGraphic *Obj,double *R)
{
  /* NspGMatrix *P = (NspGMatrix *) Obj; */
  Sciprintf("we should get a double here for alpha\n");
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_gmatrix(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  NspGMatrix *P = (NspGMatrix *) Obj;
  P->obj->rect->R[0] *= alpha[0];
  P->obj->rect->R[2] *= alpha[0];
  P->obj->rect->R[3] *= alpha[1];
  P->obj->rect->R[1] *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of gmatrix 
 *
 */

static int nsp_getbounds_gmatrix (BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspGMatrix *P = (NspGMatrix *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  bounds[0]=P->obj->rect->R[0]; /* xmin */
  bounds[1]=P->obj->rect->R[1]; /* ymin */
  bounds[2]=P->obj->rect->R[2]; /* xmax */
  bounds[3]=P->obj->rect->R[3]; /* ymax */
  return TRUE;
}


#line 953 "gmatrix.c"
