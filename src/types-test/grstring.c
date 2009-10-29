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





#line 19 "codegen/grstring.override"
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

#line 32 "grstring.c"

/* ----------- NspGrstring ----------- */


#define  NspGrstring_Private 
#include <nsp/object.h>
#include <nsp/grstring.h>
#include <nsp/interf.h>

/* 
 * NspGrstring inherits from Graphic 
 */

int nsp_type_grstring_id=0;
NspTypeGrstring *nsp_type_grstring=NULL;

/*
 * Type object for NspGrstring 
 * all the instance of NspTypeGrstring share the same id. 
 * nsp_type_grstring: is an instance of NspTypeGrstring 
 *    used for objects of NspGrstring type (i.e built with new_grstring) 
 * other instances are used for derived classes 
 */
NspTypeGrstring *new_type_grstring(type_mode mode)
{
  NspTypeGrstring *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_grstring != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grstring;
    }
  if (( type =  malloc(sizeof(NspTypeGrstring))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = grstring_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = grstring_get_methods;
  type->new = (new_func *) new_grstring;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for grstring */ 

  top->pr = (print_func *) nsp_grstring_print;
  top->dealloc = (dealloc_func *) nsp_grstring_destroy;
  top->copy  =  (copy_func *) nsp_grstring_copy;
  top->size  = (size_func *) nsp_grstring_size;
  top->s_type =  (s_type_func *) nsp_grstring_type_as_string;
  top->sh_type = (sh_type_func *) nsp_grstring_type_short_string;
  top->info = (info_func *) nsp_grstring_info;
  /* top->is_true = (is_true_func  *) nsp_grstring_is_true; */
  /* top->loop =(loop_func *) nsp_grstring_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_grstring_object;
  top->eq  = (eq_func *) nsp_grstring_eq;
  top->neq  = (eq_func *) nsp_grstring_neq;
  top->save  = (save_func *) nsp_grstring_xdr_save;
  top->load  = (load_func *) nsp_grstring_xdr_load;
  top->create = (create_func*) int_grstring_create;
  top->latex = (print_func *) nsp_grstring_latex;
  top->full_copy = (copy_func *) nsp_grstring_full_copy;

  /* specific methods for grstring */

  type->init = (init_func *) init_grstring;

#line 29 "codegen/grstring.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grstring;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grstring ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grstring  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grstring  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grstring  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Grstring */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 117 "grstring.c"
  /* 
   * NspGrstring interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_grstring_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGrstring called nsp_type_grstring
       */
      type->id =  nsp_type_grstring_id = nsp_new_type_id();
      nsp_type_grstring = type;
      if ( nsp_register_type(nsp_type_grstring) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grstring(mode);
    }
  else 
    {
      type->id = nsp_type_grstring_id;
      return type;
    }
}

/*
 * initialize NspGrstring instances 
 * locally and by calling initializer on parent class 
 */

static int init_grstring(NspGrstring *Obj,NspTypeGrstring *type)
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
 * new instance of NspGrstring 
 */

NspGrstring *new_grstring() 
{
  NspGrstring *loc;
  /* type must exists */
  nsp_type_grstring = new_type_grstring(T_BASE);
  if ( (loc = malloc(sizeof(NspGrstring)))== NULLGRSTRING) return loc;
  /* initialize object */
  if ( init_grstring(loc,nsp_type_grstring) == FAIL) return NULLGRSTRING;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGrstring 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_grstring_size(NspGrstring *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char grstring_type_name[]="Grstring";
static char grstring_short_type_name[]="grstring";

static char *nsp_grstring_type_as_string(void)
{
  return(grstring_type_name);
}

static char *nsp_grstring_type_short_string(NspObject *v)
{
  return(grstring_short_type_name);
}

/*
 * A == B 
 */

static int nsp_grstring_eq(NspGrstring *A, NspObject *B)
{
  NspGrstring *loc = (NspGrstring *) B;
  if ( check_cast(B,nsp_type_grstring_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
  if ( strcmp(A->obj->font,loc->obj->font) != 0) return FALSE;
  if ( NSP_OBJECT(A->obj->text)->type->eq(A->obj->text,loc->obj->text) == FALSE ) return FALSE;
  if ( A->obj->position != loc->obj->position) return FALSE;
  if ( A->obj->angle != loc->obj->angle) return FALSE;
  if ( A->obj->w != loc->obj->w) return FALSE;
  if ( A->obj->h != loc->obj->h) return FALSE;
  if ( A->obj->fill != loc->obj->fill) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_grstring_neq(NspGrstring *A, NspObject *B)
{
  return ( nsp_grstring_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_grstring_xdr_save(XDR *xdrs, NspGrstring *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_grstring)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->y) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->font) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->text)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->position) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->angle) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->w) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->h) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fill) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrstring  *nsp_grstring_xdr_load_partial(XDR *xdrs, NspGrstring *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_grstring))) == NULL) return NULL;
  M->obj->ref_count=1;
  if (nsp_xdr_load_d(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->font)) == FAIL) return NULL;
  if ((M->obj->text =(NspSMatrix *) nsp_object_xdr_load(xdrs))== NULLSMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->position) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->angle) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->w) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->h) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->fill) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspGrstring  *nsp_grstring_xdr_load(XDR *xdrs)
{
  NspGrstring *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRSTRING;
  if ((H  = nsp_grstring_create_void(name,(NspTypeBase *) nsp_type_grstring))== NULLGRSTRING) return H;
  if ((H  = nsp_grstring_xdr_load_partial(xdrs,H))== NULLGRSTRING) return H;
  if ( nsp_grstring_check_values(H) == FAIL) return NULLGRSTRING;
  return H;
}

/*
 * delete 
 */

void nsp_grstring_destroy_partial(NspGrstring *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_string_destroy(&(H->obj->font));
    if ( H->obj->text != NULL ) 
      nsp_smatrix_destroy(H->obj->text);
    FREE(H->obj);
   }
}

void nsp_grstring_destroy(NspGrstring *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_grstring_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_grstring_info(NspGrstring *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRSTRING) 
    {
      Sciprintf("Null Pointer NspGrstring \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_grstring_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_grstring_print(NspGrstring *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRSTRING) 
    {
      Sciprintf("Null Pointer NspGrstring \n");
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
          nsp_grstring_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grstring_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"font=%s\n",M->obj->font);
  if ( M->obj->text != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->text),indent+2,"text",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"position=%d\n",M->obj->position);
  Sciprintf1(indent+2,"angle=%f\n",M->obj->angle);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
  Sciprintf1(indent+2,"fill=%d\n",M->obj->fill);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_grstring_latex(NspGrstring *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grstring_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"font=%s\n",M->obj->font);
  if ( M->obj->text != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->text),indent+2,"text",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"position=%d\n",M->obj->position);
  Sciprintf1(indent+2,"angle=%f\n",M->obj->angle);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
  Sciprintf1(indent+2,"fill=%d\n",M->obj->fill);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGrstring objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGrstring   *nsp_grstring_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grstring_id) == TRUE ) return ((NspGrstring *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_grstring));
  return NULL;
}

int IsGrstringObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_grstring_id);
}

int IsGrstring(NspObject *O)
{
  return nsp_object_type(O,nsp_type_grstring_id);
}

NspGrstring  *GetGrstringCopy(Stack stack, int i)
{
  if (  GetGrstring(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGrstring  *GetGrstring(Stack stack, int i)
{
  NspGrstring *M;
  if (( M = nsp_grstring_object(NthObj(i))) == NULLGRSTRING)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGrstring instance 
 *-----------------------------------------------------*/

static NspGrstring *nsp_grstring_create_void(char *name,NspTypeBase *type)
{
 NspGrstring *H  = (type == NULL) ? new_grstring() : type->new();
 if ( H ==  NULLGRSTRING)
  {
   Sciprintf("No more memory\n");
   return NULLGRSTRING;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRSTRING;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_grstring_create_partial(NspGrstring *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grstring)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0.0;
  H->obj->y = 0.0;
  H->obj->font = NULL;
  H->obj->text = NULLSMAT;
  H->obj->position = 0;
  H->obj->angle = 0.0;
  H->obj->w = 0.0;
  H->obj->h = 0.0;
  H->obj->fill = 0;
  return OK;
}

int nsp_grstring_check_values(NspGrstring *H)
{
  if ( H->obj->font == NULL) 
    {
     if (( H->obj->font = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->text == NULLSMAT) 
    {
     if (( H->obj->text = nsp_smatrix_create("text",0,0,"v",0)) == NULLSMAT)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGrstring *nsp_grstring_create(char *name,double x,double y,char* font,NspSMatrix* text,int position,double angle,double w,double h,int fill,NspTypeBase *type)
{
 NspGrstring *H  = nsp_grstring_create_void(name,type);
 if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_grstring_create_partial(H) == FAIL) return NULLGRSTRING;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->font = font;
  H->obj->text= text;
  H->obj->position=position;
  H->obj->angle=angle;
  H->obj->w=w;
  H->obj->h=h;
  H->obj->fill=fill;
 if ( nsp_grstring_check_values(H) == FAIL) return NULLGRSTRING;
 return H;
}


NspGrstring *nsp_grstring_create_default(char *name)
{
 NspGrstring *H  = nsp_grstring_create_void(name,NULL);
 if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_grstring_create_partial(H) == FAIL) return NULLGRSTRING;
 if ( nsp_grstring_check_values(H) == FAIL) return NULLGRSTRING;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGrstring *nsp_grstring_copy_partial(NspGrstring *H,NspGrstring *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrstring *nsp_grstring_copy(NspGrstring *self)
{
  NspGrstring *H  =nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRSTRING;
  if ( nsp_grstring_copy_partial(H,self)== NULL) return NULLGRSTRING;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGrstring *nsp_grstring_full_copy_partial(NspGrstring *H,NspGrstring *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_grstring))) == NULL) return NULLGRSTRING;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  if ((H->obj->font = nsp_string_copy(self->obj->font)) == NULL) return NULL;
  if ( self->obj->text == NULL )
    { H->obj->text = NULL;}
  else
    {
      if ((H->obj->text = (NspSMatrix *) nsp_object_full_copy_and_name("text",NSP_OBJECT(self->obj->text))) == NULLSMAT) return NULL;
    }
  H->obj->position=self->obj->position;
  H->obj->angle=self->obj->angle;
  H->obj->w=self->obj->w;
  H->obj->h=self->obj->h;
  H->obj->fill=self->obj->fill;
  return H;
}

NspGrstring *nsp_grstring_full_copy(NspGrstring *self)
{
  NspGrstring *H  =nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRSTRING;
  if ( nsp_grstring_full_copy_partial(H,self)== NULL) return NULLGRSTRING;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGrstring
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_grstring_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGrstring *H;
  CheckStdRhs(0,0);
  /* want to be sure that type grstring is initialized */
  nsp_type_grstring = new_type_grstring(T_BASE);
  if(( H = nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring)) == NULLGRSTRING) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_grstring_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_grstring_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *grstring_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_grstring_get_x(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->x;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grstring_set_x(void *self,const char *attr, NspObject *O)
{
  double x;

  if ( DoubleScalar(O,&x) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_grstring_get_y(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->y;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grstring_set_y(void *self,const char *attr, NspObject *O)
{
  double y;

  if ( DoubleScalar(O,&y) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_grstring_get_font(void *self,const char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->font;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_grstring_set_font(void *self,const char *attr, NspObject *O)
{
  char *font;

  if ((font = nsp_string_object(O))==NULL) return FAIL;
  if ((font = nsp_string_copy(font)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspGrstring *) self)->obj->font);
  ((NspGrstring *) self)->obj->font= font;
  return OK;
}

static NspObject *_wrap_grstring_get_text(void *self,const char *attr)
{
  NspSMatrix *ret;

  ret = ((NspGrstring *) self)->obj->text;
  return (NspObject *) ret;
}

static NspObject *_wrap_grstring_get_obj_text(void *self,const char *attr, int *copy)
{
  NspSMatrix *ret;

  *copy = FALSE;
  ret = ((NspSMatrix*) ((NspGrstring *) self)->obj->text);
  return (NspObject *) ret;
}

static int _wrap_grstring_set_text(void *self,const char *attr, NspObject *O)
{
  NspSMatrix *text;

  if ( ! IsSMat(O) ) return FAIL;
  if ((text = (NspSMatrix *) nsp_object_copy_and_name(attr,O)) == NULLSMAT) return FAIL;
  if (((NspGrstring *) self)->obj->text != NULL ) 
    nsp_smatrix_destroy(((NspGrstring *) self)->obj->text);
  ((NspGrstring *) self)->obj->text= text;
  return OK;
}

static NspObject *_wrap_grstring_get_position(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrstring *) self)->obj->position;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_position(void *self,const char *attr, NspObject *O)
{
  int position;

  if ( IntScalar(O,&position) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->position= position;
  return OK;
}

static NspObject *_wrap_grstring_get_angle(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->angle;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grstring_set_angle(void *self,const char *attr, NspObject *O)
{
  double angle;

  if ( DoubleScalar(O,&angle) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->angle= angle;
  return OK;
}

static NspObject *_wrap_grstring_get_w(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->w;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grstring_set_w(void *self,const char *attr, NspObject *O)
{
  double w;

  if ( DoubleScalar(O,&w) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->w= w;
  return OK;
}

static NspObject *_wrap_grstring_get_h(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->h;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grstring_set_h(void *self,const char *attr, NspObject *O)
{
  double h;

  if ( DoubleScalar(O,&h) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->h= h;
  return OK;
}

static NspObject *_wrap_grstring_get_fill(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrstring *) self)->obj->fill;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_fill(void *self,const char *attr, NspObject *O)
{
  int fill;

  if ( IntScalar(O,&fill) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->fill= fill;
  return OK;
}

static AttrTab grstring_attrs[] = {
  { "x", (attr_get_function *)_wrap_grstring_get_x, (attr_set_function *)_wrap_grstring_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_grstring_get_y, (attr_set_function *)_wrap_grstring_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "font", (attr_get_function *)_wrap_grstring_get_font, (attr_set_function *)_wrap_grstring_set_font,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "text", (attr_get_function *)_wrap_grstring_get_text, (attr_set_function *)_wrap_grstring_set_text,(attr_get_object_function *)_wrap_grstring_get_obj_text, (attr_set_object_function *)int_set_object_failed },
  { "position", (attr_get_function *)_wrap_grstring_get_position, (attr_set_function *)_wrap_grstring_set_position,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "angle", (attr_get_function *)_wrap_grstring_get_angle, (attr_set_function *)_wrap_grstring_set_angle,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "w", (attr_get_function *)_wrap_grstring_get_w, (attr_set_function *)_wrap_grstring_set_w,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "h", (attr_get_function *)_wrap_grstring_get_h, (attr_set_function *)_wrap_grstring_set_h,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "fill", (attr_get_function *)_wrap_grstring_get_fill, (attr_set_function *)_wrap_grstring_set_fill,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 51 "codegen/grstring.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grstring(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 813 "grstring.c"


#line 61 "codegen/grstring.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grstring(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 826 "grstring.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Grstring_func[]={
  {"extractelts_grstring", _wrap_nsp_extractelts_grstring},
  {"setrowscols_grstring", _wrap_nsp_setrowscols_grstring},
  { "grstring_create", int_grstring_create},
  { NULL, NULL}
};

/* call ith function in the Grstring interface */

int Grstring_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Grstring_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Grstring_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Grstring_func[i].name;
  *f = Grstring_func[i].fonc;
}

#line 72 "codegen/grstring.override"

/* inserted verbatim at the end */

static void nsp_draw_grstring(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int i,flagx=0;
  double rect[4],wc,x,y,yi;
  NspGrstring *P = (NspGrstring *) Obj;
  NspSMatrix *S = P->obj->text;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  /*     to keep the size of the largest line */
  wc = 0.;
  x= P->obj->x;
  y= P->obj->y;
  /* S->n should be equal to 1 here 
   * This is to be done at creation
   */
  if ( S->n == 0 ) return;

  if ( P->obj->position == 10) /* !!*/ 
    {
      double w=P->obj->w, h = P->obj->h;
      nsp_string str;
      if ( S->mn != 1) 
	{
	  if (( str =nsp_smatrix_elts_concat(S,"\n",1," ",1))== NULL) return ;
	}
      else
	{
	  str = S->S[0];
	}
      Xgc->graphic_engine->scale->xstringb(Xgc,str,&P->obj->fill,&x,&y,&w,&h);
      if ( S->mn != 1 ) FREE(str);
    }
  else
    {
      if ( S->m == 1 )
	{
	  /* one rotated string */
	  Xgc->graphic_engine->scale->displaystring(Xgc,S->S[0],x,y,0,P->obj->angle);
	}
      else 
	{
	  for (i = S->m -1 ; i >= 0; --i) 
	    {
	      Xgc->graphic_engine->scale->displaystring(Xgc,S->S[i],x,y,0,P->obj->angle);
	      Xgc->graphic_engine->scale->boundingbox(Xgc,S->S[i],x,y,rect);
	      wc = Max(wc,rect[2]);
	      if (i != 0 ) 
		y += rect[3] * 1.2;
	      else 
		y += rect[3];
	    }
	  if (flagx == 1) {
	    double rect[]={x,y,wc, y - yi};
	    Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
	  }
	}
    }
}

static void nsp_translate_grstring(NspGraphic *Obj,const double *tr)
{
  NspGrstring *P = (NspGrstring *) Obj;
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_grstring(NspGraphic *Obj,double *R)
{
  NspGrstring *P = (NspGrstring *) Obj;
  double x1;
  x1 = R[0]*(P->obj->x) -R[1]*(P->obj->y);
  P->obj->y = R[1]*(P->obj->x) +R[0]*(P->obj->y);
  P->obj->x = x1;
  /* Il faut aussi changer l'angle */
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_grstring(NspGraphic *Obj,double *alpha)
{
  NspGrstring *P = (NspGrstring *) Obj;
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of grstring 
 *
 */

static int nsp_getbounds_grstring(NspGraphic *Obj,double *bounds)
{
  return FALSE;
  /* NspGrstring *P = (NspGrstring *) Obj; */
  bounds[0]= bounds[1] = bounds[2]= bounds[3]=0;
  /*   bounds[0]=*x;/\* xmin *\/ */
  /*   bounds[1]=*y;/\* ymin *\/ */
  /*   bounds[2]=*x;/\* xmax *\/ */
  /*   bounds[3]=*y;/\* ymax *\/ */
}


#line 963 "grstring.c"
