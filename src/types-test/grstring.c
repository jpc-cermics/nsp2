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





#line 24 "codegen/grstring.override"
#include <gdk/gdk.h>
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/figuredata.h>
#include <nsp/figure.h>
#include <nsp/axes.h>

#line 36 "grstring.c"

/* -----------NspGrstring ----------- */


#define  NspGrstring_Private 
#include <nsp/objects.h>
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
  type->gtk_methods = FALSE;
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

#line 38 "codegen/grstring.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grstring;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grstring ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grstring  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grstring  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grstring  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Grstring */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */

#line 122 "grstring.c"
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
  return 1;
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
  if ( A->obj->angle != loc->obj->angle) return FALSE;
  if ( A->obj->w != loc->obj->w) return FALSE;
  if ( A->obj->h != loc->obj->h) return FALSE;
  if ( A->obj->fill != loc->obj->fill) return FALSE;
  if ( A->obj->posx != loc->obj->posx) return FALSE;
  if ( A->obj->posy != loc->obj->posy) return FALSE;
  if ( A->obj->size != loc->obj->size) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
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
  if (nsp_xdr_save_d(xdrs, M->obj->angle) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->w) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->h) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fill) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->posx) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->posy) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrstring  *nsp_grstring_xdr_load_partial(XDR *xdrs, NspGrstring *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_d(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->font)) == FAIL) return NULL;
  if ((M->obj->text =(NspSMatrix *) nsp_object_xdr_load(xdrs))== NULLSMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->angle) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->w) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->h) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->fill) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->posx) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->posy) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspGrstring  *nsp_grstring_xdr_load(XDR *xdrs)
{
  NspGrstring *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRSTRING;
  if ((H  = nsp_grstring_create_void(name,(NspTypeBase *) nsp_type_grstring))== NULLGRSTRING) return H;
  if ( nsp_grstring_create_partial(H) == FAIL) return NULLGRSTRING;
  if ((H  = nsp_grstring_xdr_load_partial(xdrs,H))== NULLGRSTRING) return H;
  if ( nsp_grstring_check_values(H) == FAIL) return NULLGRSTRING;
  return H;
}

/*
 * delete 
 */

void nsp_grstring_destroy_partial(NspGrstring *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
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
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grstring_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n", M->obj->x);
  Sciprintf1(indent+2,"y=%f\n", M->obj->y);
  Sciprintf1(indent+2,"font=%s\n",M->obj->font);
  if ( M->obj->text != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->text),indent+2,"text", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"angle=%f\n", M->obj->angle);
  Sciprintf1(indent+2,"w=%f\n", M->obj->w);
  Sciprintf1(indent+2,"h=%f\n", M->obj->h);
  Sciprintf1(indent+2,"fill=%d\n", M->obj->fill);
  Sciprintf1(indent+2,"posx=%d\n", M->obj->posx);
  Sciprintf1(indent+2,"posy=%d\n", M->obj->posy);
  Sciprintf1(indent+2,"size=%d\n", M->obj->size);
  Sciprintf1(indent+2,"color=%d\n", M->obj->color);
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_grstring_latex(NspGrstring *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grstring_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|x| = \\numprint{%f}\n", M->obj->x);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|y| = \\numprint{%f}\n", M->obj->y);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|font|=\\verb@\"%s\"@\n",(M->obj->font==NULL) ? "NULL": M->obj->font);
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->text != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->text),FALSE,"text", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|angle| = \\numprint{%f}\n", M->obj->angle);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|w| = \\numprint{%f}\n", M->obj->w);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|h| = \\numprint{%f}\n", M->obj->h);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|fill|= \\numprint{%d}\n",M->obj->fill);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|posx|= \\numprint{%d}\n",M->obj->posx);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|posy|= \\numprint{%d}\n",M->obj->posy);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|size|= \\numprint{%d}\n",M->obj->size);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|color|= \\numprint{%d}\n",M->obj->color);
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
 * for NspGrstring objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGrstring   *nsp_grstring_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grstring_id)  == TRUE  ) return ((NspGrstring *) O);
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

static NspGrstring *nsp_grstring_create_void(const char *name,NspTypeBase *type)
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
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grstring)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0.0;
  H->obj->y = 0.0;
  H->obj->font = NULL;
  H->obj->text = NULLSMAT;
  H->obj->angle = 0.0;
  H->obj->w = 0.0;
  H->obj->h = 0.0;
  H->obj->fill = 0;
  H->obj->posx = 0;
  H->obj->posy = 0;
  H->obj->size = 0;
  H->obj->color = -1;
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
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspGrstring *nsp_grstring_create(const char *name,double x,double y,char* font,NspSMatrix* text,double angle,double w,double h,int fill,int posx,int posy,int size,int color,NspTypeBase *type)
{
  NspGrstring *H  = nsp_grstring_create_void(name,type);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_grstring_create_partial(H) == FAIL) return NULLGRSTRING;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->font = font;
  H->obj->text= text;
  H->obj->angle=angle;
  H->obj->w=w;
  H->obj->h=h;
  H->obj->fill=fill;
  H->obj->posx=posx;
  H->obj->posy=posy;
  H->obj->size=size;
  H->obj->color=color;
  if ( nsp_grstring_check_values(H) == FAIL) return NULLGRSTRING;
  return H;
}


NspGrstring *nsp_grstring_create_default(const char *name)
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
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLGRSTRING;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrstring *nsp_grstring_copy(NspGrstring *self)
{
  NspGrstring *H  =nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_grstring_copy_partial(H,self)== NULL) return NULLGRSTRING;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGrstring *nsp_grstring_full_copy_partial(NspGrstring *H,NspGrstring *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLGRSTRING;
  if ((H->obj = calloc(1,sizeof(nsp_grstring))) == NULL) return NULLGRSTRING;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  if ((H->obj->font = nsp_string_copy(self->obj->font)) == NULL) return NULL;
  if ( self->obj->text == NULL )
    { H->obj->text = NULL;}
  else
    {
      if ((H->obj->text = (NspSMatrix *) nsp_object_full_copy_and_name("text", NSP_OBJECT(self->obj->text))) == NULLSMAT) return NULL;
    }
  H->obj->angle=self->obj->angle;
  H->obj->w=self->obj->w;
  H->obj->h=self->obj->h;
  H->obj->fill=self->obj->fill;
  H->obj->posx=self->obj->posx;
  H->obj->posy=self->obj->posy;
  H->obj->size=self->obj->size;
  H->obj->color=self->obj->color;
  return H;
}

NspGrstring *nsp_grstring_full_copy(NspGrstring *self)
{
  NspGrstring *H  =nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
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
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
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
  NspObject *nsp_ret;
  const gchar *ret;
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

static NspObject *_wrap_grstring_get_posx(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrstring *) self)->obj->posx;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_posx(void *self,const char *attr, NspObject *O)
{
  int posx;
  if ( IntScalar(O,&posx) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->posx= posx;
  return OK;
}

static NspObject *_wrap_grstring_get_posy(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrstring *) self)->obj->posy;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_posy(void *self,const char *attr, NspObject *O)
{
  int posy;
  if ( IntScalar(O,&posy) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->posy= posy;
  return OK;
}

static NspObject *_wrap_grstring_get_size(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrstring *) self)->obj->size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_size(void *self,const char *attr, NspObject *O)
{
  int size;
  if ( IntScalar(O,&size) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->size= size;
  return OK;
}

static NspObject *_wrap_grstring_get_color(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrstring *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_color(void *self,const char *attr, NspObject *O)
{
  int color;
  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->color= color;
  return OK;
}

static AttrTab grstring_attrs[] = {
  { "x", (attr_get_function * )_wrap_grstring_get_x, (attr_set_function * )_wrap_grstring_set_x, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "y", (attr_get_function * )_wrap_grstring_get_y, (attr_set_function * )_wrap_grstring_set_y, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "font", (attr_get_function * )_wrap_grstring_get_font, (attr_set_function * )_wrap_grstring_set_font, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "text", (attr_get_function * )_wrap_grstring_get_text, (attr_set_function * )_wrap_grstring_set_text, (attr_get_object_function * )_wrap_grstring_get_obj_text, (attr_set_object_function * )int_set_object_failed },
  { "angle", (attr_get_function * )_wrap_grstring_get_angle, (attr_set_function * )_wrap_grstring_set_angle, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "w", (attr_get_function * )_wrap_grstring_get_w, (attr_set_function * )_wrap_grstring_set_w, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "h", (attr_get_function * )_wrap_grstring_get_h, (attr_set_function * )_wrap_grstring_set_h, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "fill", (attr_get_function * )_wrap_grstring_get_fill, (attr_set_function * )_wrap_grstring_set_fill, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "posx", (attr_get_function * )_wrap_grstring_get_posx, (attr_set_function * )_wrap_grstring_set_posx, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "posy", (attr_get_function * )_wrap_grstring_get_posy, (attr_set_function * )_wrap_grstring_set_posy, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "size", (attr_get_function * )_wrap_grstring_get_size, (attr_set_function * )_wrap_grstring_set_size, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "color", (attr_get_function * )_wrap_grstring_get_color, (attr_set_function * )_wrap_grstring_set_color, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 60 "codegen/grstring.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grstring(Stack stack, int rhs, int opt, int lhs)
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 899 "grstring.c"


#line 70 "codegen/grstring.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grstring(Stack stack, int rhs, int opt, int lhs)
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 912 "grstring.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Grstring_func[]={
  { "extractelts_grstring", _wrap_nsp_extractelts_grstring},
  { "setrowscols_grstring", _wrap_nsp_setrowscols_grstring},
  { "grstring_create", int_grstring_create},
  { NULL, NULL}
};

/* call ith function in the Grstring interface */

int Grstring_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Grstring_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Grstring_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Grstring_func[i].name;
  *f = Grstring_func[i].fonc;
}
void nsp_initialize_Grstring_types(void)
{
  new_type_grstring(T_BASE);
}

#line 81 "codegen/grstring.override"

/* inserted verbatim at the end */
static void nsp_draw_grstring_in_box(BCG *Xgc,NspGrstring *P, const char *str);

static void nsp_draw_grstring(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  int current_color=-1;
  int fontid[2],box;
  double xd1, yd1;
  nsp_string str;
  NspGrstring *P = (NspGrstring *) Obj;
  NspSMatrix *S = P->obj->text;
  double x=P->obj->x,y=P->obj->y;

  if ( Obj->obj->show == FALSE ) return ;

  /* check if the block is inside drawing rectangle
   */

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  /* S->n should be equal to 1 or 0 here
   * This is to be done at creation
   */
  if ( S->n == 0 ) return;
  if ( S->mn != 1)
    {
      if (( str =nsp_smatrix_elts_concat(S,"\n",1," ",1))== NULL) return ;
    }
  else
    {
      str = S->S[0];
    }
  box = 0; /* ( P->obj->box_color != -2 ); *//* we draw a box */

  current_color = Xgc->graphic_engine->xget_color(Xgc);
  if ( P->obj->color >= 0) Xgc->graphic_engine->xset_color(Xgc,P->obj->color);

  switch ( P->obj->fill )
    {
    case GR_fill_box:
      /* draw the string inside a box:
       * the font size is adapted so as to fill the box.
       */
      nsp_draw_grstring_in_box(Xgc,P,str);
      break;
    case GR_in_box :
      /* draw a string centered in a box */
      xd1 = XDouble2Pixel_d(Xgc->scales,x + P->obj->w/2.0);
      yd1 = YDouble2Pixel_d(Xgc->scales,y + P->obj->h/2.0);
      if ( P->obj->size != -1 )
	{
	  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0], Max(P->obj->size,1),TRUE);
	}
      Xgc->graphic_engine->displaystring(Xgc,str,xd1,yd1,box,P->obj->angle,
					 GR_STR_XCENTER,GR_STR_YCENTER);
      if ( P->obj->size != -1 )
	Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1],FALSE);
      break;
    case GR_no_box :
      /* display a string */
      xd1 = XDouble2Pixel_d(Xgc->scales,x + P->obj->w/2.0);
      yd1 = YDouble2Pixel_d(Xgc->scales,y + P->obj->h/2.0);
      if ( P->obj->size != -1 )
	{
	  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0], Max(P->obj->size,1),TRUE);
	}
      Xgc->graphic_engine->displaystring(Xgc,str,xd1,yd1,box,P->obj->angle,
					 P->obj->posx, P->obj->posy);
      if ( P->obj->size != -1 )
	Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1],FALSE);
      break;
    }

  if ( S->mn != 1 ) FREE(str);
  if ( P->obj->color >= 0) Xgc->graphic_engine->xset_color(Xgc,current_color);
}

static void nsp_draw_grstring_in_box(BCG *Xgc,NspGrstring *P, const char *str)
{
  int box = 0 ; /* ( P->obj->box_color != -2 );*/ /* we draw a box */
  double xd1,yd1;
  int iw,ih,size_in= 1, size_out=100,size=-1, count=0;
  int fontid[2],check= TRUE;
  double logrect[4];
  length_scale_f2i(Xgc->scales,&P->obj->w,&P->obj->h,&iw,&ih,1);
  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
  /* try to detect if current value is OK. */
  size = (P->obj->size == -1) ? fontid[1] : P->obj->size;
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size,TRUE);
  Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
  if ( logrect[2] > iw || logrect[3] > ih )
    {
      size_out = size;
    }
  else
    {
      size_in = size;
      /* is size_out enough ?
       * we first try size + 1
       */
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],size+1,TRUE);
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
      if ( logrect[2] > iw || logrect[3] > ih )
	{
	  size_out = size+1;
	  check = FALSE;
	}
      /* loop to increase upper bound */
      while (1)
	{
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size_out,TRUE);
	  Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
	  if ( !( logrect[2] > iw || logrect[3] > ih) )
	    {
	      size_out *= 2;
	    }
	  else
	    break;
	}
    }
  size= (size_in + size_out)/2;
  /* dichotomic search */
  while ( check  )
    {
      /* Sciprintf("Search with [%d,%d] \n",size_in,size_out); */
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],size,TRUE);
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
      count++;
      if ( logrect[2] > iw || logrect[3] > ih )
	{
	  size_out = size;
	}
      else
	{
	  size_in = size;
	}

      size = (size_in + size_out)/2;
      if ( size_out - size_in <= 1 ) break;
    }
  /* Sciprintf("We quit with %d\n",size); */
  P->obj->size = size ;
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size,TRUE);
  xd1 = XDouble2Pixel_d(Xgc->scales,P->obj->x + P->obj->w/2.0);
  yd1 = YDouble2Pixel_d(Xgc->scales,P->obj->y + P->obj->h/2.0);
  Xgc->graphic_engine->displaystring(Xgc,str,xd1,yd1,box, P->obj->angle,
				     GR_STR_XCENTER,GR_STR_YCENTER);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], FALSE);
}


static void nsp_translate_grstring(NspGraphic *Obj,const double *tr)
{
  NspGrstring *P = (NspGrstring *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_grstring(NspGraphic *Obj,double *R)
{
  NspGrstring *P = (NspGrstring *) Obj;
  double x1,y1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  /* rotate (x,y) */
  switch ( P->obj->fill )
    {
    case GR_fill_box:
    case GR_in_box:
      /* rotate the center */
      x1 = R[0]*(P->obj->x+ P->obj->w/2) -R[1]*(P->obj->y + P->obj->h/2);
      y1 = R[1]*(P->obj->x+ P->obj->w/2) +R[0]*(P->obj->y + P->obj->h/2);
      P->obj->x = x1 - P->obj->w/2;
      P->obj->y = y1 - P->obj->h/2;
      P->obj->angle += - atan2(R[1],R[0])*180/M_PI;
      break;
    case GR_no_box :
      /* rotate (x,y) */
      x1 = R[0]*(P->obj->x) -R[1]*(P->obj->y);
      P->obj->y = R[1]*(P->obj->x) +R[0]*(P->obj->y);
      P->obj->x = x1;
      /* change the angle */
      P->obj->angle += - atan2(R[1],R[0])*180/M_PI;
      break;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_grstring(NspGraphic *Obj,double *alpha)
{
  NspGrstring *P = (NspGrstring *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in @bounds the enclosing rectangle of grstring
 * Note that it only works when the grstring is inside a
 * figure.
 */

static int nsp_getbounds_grstring(NspGraphic *Obj,double *bounds)
{
  nsp_string str = NULL;
  NspGrstring *P = (NspGrstring *) Obj;
  NspSMatrix *S = P->obj->text;
  int fontid[2];
  double rect1[4];
  double width, height;
  nsp_axes *axe = Obj->obj->Axe;
  nsp_figure *F = Obj->obj->Fig;
  BCG *Xgc;
  if ( S->mn != 1)
    {
      if (( str =nsp_smatrix_elts_concat(S,"\n",1," ",1))== NULL) goto false;
    }
  else
    {
      str = S->S[0];
    }

  switch ( P->obj->fill )
    {
    case GR_fill_box:
      bounds[0]= P->obj->x ;
      bounds[2]= P->obj->x + P->obj->w;
      bounds[1]= P->obj->y;
      bounds[3]= P->obj->y+  P->obj->h;;
      break;
    case GR_in_box :
      /* here we need a Figure */
      if ( F == NULL || (Xgc = F->Xgc) == NULL) goto false;
      /* centered in a box  */
      if ( P->obj->size != -1 )
	{
	  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0], Max(P->obj->size,1),TRUE);
	}
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,rect1);
      if ( P->obj->size != -1 )
	Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1],FALSE);
      if ( S->mn != 1 ) FREE(str);
      length_scale_pixels_to_double(&axe->scale,&width,&height,rect1+2,rect1+3,1);
      /* Sciprintf("Taille de la boite [%f,%f] fontid=%d\n",width,height,P->obj->size); */
      bounds[0]= P->obj->x + P->obj->w/2 -width/2;
      bounds[2]= P->obj->x + P->obj->w/2 +width/2;
      bounds[1]= P->obj->y + P->obj->h/2 -height/2;
      bounds[3]= P->obj->y + P->obj->h/2 + height/2;
      break;
    case GR_no_box :
      /* no box case */
      if ( F == NULL || (Xgc = F->Xgc) == NULL) goto false;
      if ( P->obj->size != -1 )
	{
	  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0], Max(P->obj->size,1),TRUE);
	}
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,rect1);
      if ( P->obj->size != -1 )
	Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1],FALSE);
      if ( S->mn != 1 ) FREE(str);
      length_scale_pixels_to_double(&axe->scale,&width,&height,rect1+2,rect1+3,1);
      switch( P->obj->posx )
	{
	case GR_STR_XLEFT: bounds[0]= P->obj->x; bounds[2]= P->obj->x+width; break;
	case GR_STR_XCENTER: bounds[0]= P->obj->x -width/2; bounds[2]= P->obj->x+width/2; break;
	case GR_STR_XRIGHT: bounds[0]= P->obj->x - width; bounds[2]= P->obj->x; break;
	}
      switch( P->obj->posy )
	{
	case GR_STR_YBOTTOM:bounds[1]= P->obj->y; bounds[3]= P->obj->y + height; break;
	case GR_STR_YCENTER:bounds[1]= P->obj->y -height/2; bounds[3]= P->obj->y + height/2; break;
	case GR_STR_YBASELINE:
	  /* TO BE IMPROVED we give something bigger */
	  bounds[1]= P->obj->y -height ; bounds[3]= P->obj->y + height; break;
	case GR_STR_YUP:   bounds[1]= P->obj->y-height ; bounds[3]= P->obj->y; break;
	}
      break;
    }

  if (P->obj->angle!=0.)
    {
      /* rotate the bounds */
      double diff[2],  bb[8], center[2]={0,0};
      double cosa = cos(-P->obj->angle*M_PI/180);
      double sina = sin(-P->obj->angle*M_PI/180);

      switch ( P->obj->fill )
	{
	case GR_fill_box:
	case GR_in_box:
	  center[0]=bounds[0]+(bounds[2]-bounds[0])/2;
	  center[1]=bounds[1]+(bounds[3]-bounds[1])/2;
	  break;
	case GR_no_box:
	  switch( P->obj->posx )
	    {
	    case GR_STR_XLEFT: center[0] = bounds[0]; break;
	    case GR_STR_XCENTER: center[0]  = bounds[0] + width/2; break;
	    case GR_STR_XRIGHT: center[0]  = bounds[0] + width; break;
	    }
	  switch( P->obj->posy )
	    {
	    case GR_STR_YBOTTOM: center[1]  =bounds[1] ; break;
	    case GR_STR_YCENTER:  center[1]  = bounds[1] + height/2; break;
	    case GR_STR_YBASELINE: center[1] =bounds[1] ; break; /* wrong */
	    case GR_STR_YUP:  center[1]  = bounds[1] +height; break;
	    }
	  break;
	}
      /* rotate/translate the points */
      diff[0]=bounds[0]-center[0];
      diff[1]=bounds[1]-center[1];
      bb[0]=diff[0]*cosa-diff[1]*sina+center[0];
      bb[1]=diff[0]*sina+diff[1]*cosa+center[1];

      diff[0]=bounds[0]-center[0];
      diff[1]=bounds[3]-center[1];
      bb[2]=diff[0]*cosa-diff[1]*sina+center[0];
      bb[3]=diff[0]*sina+diff[1]*cosa+center[1];

      diff[0]=bounds[2]-center[0];
      diff[1]=bounds[3]-center[1];
      bb[4]=diff[0]*cosa-diff[1]*sina+center[0];
      bb[5]=diff[0]*sina+diff[1]*cosa+center[1];

      diff[0]=bounds[2]-center[0];
      diff[1]=bounds[1]-center[1];
      bb[6]=diff[0]*cosa-diff[1]*sina+center[0];
      bb[7]=diff[0]*sina+diff[1]*cosa+center[1];

      /* compute bounds */
      bounds[0]=Min(bb[0],bb[2]);
      bounds[0]=Min(bounds[0],bb[4]);
      bounds[0]=Min(bounds[0],bb[6]);

      bounds[2]=Max(bb[0],bb[2]);
      bounds[2]=Max(bounds[2],bb[4]);
      bounds[2]=Max(bounds[2],bb[6]);

      bounds[1]=Min(bb[1],bb[3]);
      bounds[1]=Min(bounds[1],bb[5]);
      bounds[1]=Min(bounds[1],bb[7]);

      bounds[3]=Max(bb[1],bb[3]);
      bounds[3]=Max(bounds[3],bb[5]);
      bounds[3]=Max(bounds[3],bb[7]);
    }

  if ( FALSE && F != NULL && (Xgc = F->Xgc) != NULL)
    {
      /* draw the bounds, if requested  */
      double xm[4],ym[4];
      double x[4],y[4];
      x[0]=bounds[0];
      y[0]=bounds[1];
      x[1]=bounds[2];
      y[1]=bounds[1];
      x[2]=bounds[2];
      y[2]=bounds[3];
      x[3]=bounds[0];
      y[3]=bounds[3];
      scale_double_to_pixels(Xgc->scales,x,y,xm,ym,4);
      Xgc->graphic_engine->drawpolyline(Xgc,xm,ym,4,TRUE);
    }
  
  if ( S->mn != 1) nsp_string_destroy(&str) ;
  return TRUE;
  false:
    if ( S->mn != 1) nsp_string_destroy(&str) ;
  return FALSE;
}

#line 1329 "grstring.c"
