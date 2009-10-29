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





#line 30 "codegen/string3d.override"
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 36 "string3d.c"

/* ----------- NspString3d ----------- */


#define  NspString3d_Private 
#include <nsp/object.h>
#include <nsp/string3d.h>
#include <nsp/interf.h>

/* 
 * NspString3d inherits from Graphic 
 */

int nsp_type_string3d_id=0;
NspTypeString3d *nsp_type_string3d=NULL;

/*
 * Type object for NspString3d 
 * all the instance of NspTypeString3d share the same id. 
 * nsp_type_string3d: is an instance of NspTypeString3d 
 *    used for objects of NspString3d type (i.e built with new_string3d) 
 * other instances are used for derived classes 
 */
NspTypeString3d *new_type_string3d(type_mode mode)
{
  NspTypeString3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_string3d != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_string3d;
    }
  if (( type =  malloc(sizeof(NspTypeString3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = string3d_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = string3d_get_methods;
  type->new = (new_func *) new_string3d;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for string3d */ 

  top->pr = (print_func *) nsp_string3d_print;
  top->dealloc = (dealloc_func *) nsp_string3d_destroy;
  top->copy  =  (copy_func *) nsp_string3d_copy;
  top->size  = (size_func *) nsp_string3d_size;
  top->s_type =  (s_type_func *) nsp_string3d_type_as_string;
  top->sh_type = (sh_type_func *) nsp_string3d_type_short_string;
  top->info = (info_func *) nsp_string3d_info;
  /* top->is_true = (is_true_func  *) nsp_string3d_is_true; */
  /* top->loop =(loop_func *) nsp_string3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_string3d_object;
  top->eq  = (eq_func *) nsp_string3d_eq;
  top->neq  = (eq_func *) nsp_string3d_neq;
  top->save  = (save_func *) nsp_string3d_xdr_save;
  top->load  = (load_func *) nsp_string3d_xdr_load;
  top->create = (create_func*) int_string3d_create;
  top->latex = (print_func *) nsp_string3d_latex;
  top->full_copy = (copy_func *) nsp_string3d_full_copy;

  /* specific methods for string3d */

  type->init = (init_func *) init_string3d;

#line 44 "codegen/string3d.override"
  /* inserted verbatim in the type definition 
   * here we override the method of its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_string3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_string3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_string3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_string3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_string3d  ;
  /* next method are defined in NspGraphic and need not be chnaged here for String3d */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 
  ((NspTypeGraphic *) type->surtype)->zmean = nsp_string3d_zmean;
  ((NspTypeGraphic *) type->surtype)->n_faces = nsp_string3d_n_faces;
#line 122 "string3d.c"
  /* 
   * NspString3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_string3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeString3d called nsp_type_string3d
       */
      type->id =  nsp_type_string3d_id = nsp_new_type_id();
      nsp_type_string3d = type;
      if ( nsp_register_type(nsp_type_string3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_string3d(mode);
    }
  else 
    {
      type->id = nsp_type_string3d_id;
      return type;
    }
}

/*
 * initialize NspString3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_string3d(NspString3d *Obj,NspTypeString3d *type)
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
 * new instance of NspString3d 
 */

NspString3d *new_string3d() 
{
  NspString3d *loc;
  /* type must exists */
  nsp_type_string3d = new_type_string3d(T_BASE);
  if ( (loc = malloc(sizeof(NspString3d)))== NULLSTRING3D) return loc;
  /* initialize object */
  if ( init_string3d(loc,nsp_type_string3d) == FAIL) return NULLSTRING3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspString3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_string3d_size(NspString3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char string3d_type_name[]="String3d";
static char string3d_short_type_name[]="string3d";

static char *nsp_string3d_type_as_string(void)
{
  return(string3d_type_name);
}

static char *nsp_string3d_type_short_string(NspObject *v)
{
  return(string3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_string3d_eq(NspString3d *A, NspObject *B)
{
  NspString3d *loc = (NspString3d *) B;
  if ( check_cast(B,nsp_type_string3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  if ( A->obj->Mcoord_l != loc->obj->Mcoord_l) return FALSE;
  if ( strcmp(A->obj->str,loc->obj->str) != 0) return FALSE;
  if ( A->obj->font_type != loc->obj->font_type) return FALSE;
  if ( A->obj->font_size != loc->obj->font_size) return FALSE;
  {int i;
    for ( i = 0 ; i < A->obj->pos_length ; i++)
      if ( A->obj->pos[i] != loc->obj->pos[i]) return FALSE;
  }
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_string3d_neq(NspString3d *A, NspObject *B)
{
  return ( nsp_string3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_string3d_xdr_save(XDR *xdrs, NspString3d *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_string3d)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->str) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->font_type) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->font_size) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspString3d  *nsp_string3d_xdr_load_partial(XDR *xdrs, NspString3d *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_string3d))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->str)) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->font_type) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->font_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspString3d  *nsp_string3d_xdr_load(XDR *xdrs)
{
  NspString3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSTRING3D;
  if ((H  = nsp_string3d_create_void(name,(NspTypeBase *) nsp_type_string3d))== NULLSTRING3D) return H;
  if ((H  = nsp_string3d_xdr_load_partial(xdrs,H))== NULLSTRING3D) return H;
  if ( nsp_string3d_check_values(H) == FAIL) return NULLSTRING3D;

#line 67 "codegen/string3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_string3d(H)== FAIL) return NULL; 

#line 293 "string3d.c"
  return H;
}

/*
 * delete 
 */

void nsp_string3d_destroy_partial(NspString3d *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 72 "codegen/string3d.override"
  /* verbatim in destroy */
  nsp_matrix_destroy(H->obj->Mcoord_l);

#line 311 "string3d.c"
    if ( H->obj->Mcoord != NULL ) 
      nsp_matrix_destroy(H->obj->Mcoord);
  nsp_string_destroy(&(H->obj->str));
    FREE(H->obj->pos);
    FREE(H->obj);
   }
}

void nsp_string3d_destroy(NspString3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_string3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_string3d_info(NspString3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSTRING3D) 
    {
      Sciprintf("Null Pointer NspString3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_string3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_string3d_print(NspString3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSTRING3D) 
    {
      Sciprintf("Null Pointer NspString3d \n");
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
          nsp_string3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_string3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  Sciprintf1(indent+2,"str=%s\n",M->obj->str);
  Sciprintf1(indent+2,"font_type=%d\n",M->obj->font_type);
  Sciprintf1(indent+2,"font_size=%d\n",M->obj->font_size);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_string3d_latex(NspString3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_string3d_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  Sciprintf1(indent+2,"str=%s\n",M->obj->str);
  Sciprintf1(indent+2,"font_type=%d\n",M->obj->font_type);
  Sciprintf1(indent+2,"font_size=%d\n",M->obj->font_size);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspString3d objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspString3d   *nsp_string3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_string3d_id) == TRUE ) return ((NspString3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_string3d));
  return NULL;
}

int IsString3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_string3d_id);
}

int IsString3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_string3d_id);
}

NspString3d  *GetString3dCopy(Stack stack, int i)
{
  if (  GetString3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspString3d  *GetString3d(Stack stack, int i)
{
  NspString3d *M;
  if (( M = nsp_string3d_object(NthObj(i))) == NULLSTRING3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspString3d instance 
 *-----------------------------------------------------*/

static NspString3d *nsp_string3d_create_void(char *name,NspTypeBase *type)
{
 NspString3d *H  = (type == NULL) ? new_string3d() : type->new();
 if ( H ==  NULLSTRING3D)
  {
   Sciprintf("No more memory\n");
   return NULLSTRING3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSTRING3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_string3d_create_partial(NspString3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_string3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  H->obj->Mcoord_l = NULL;
  H->obj->str = NULL;
  H->obj->font_type = -1;
  H->obj->font_size = -1;
  H->obj->pos = NULL; H->obj->pos_length = 0; 
  return OK;
}

int nsp_string3d_check_values(NspString3d *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->str == NULL) 
    {
     if (( H->obj->str = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspString3d *nsp_string3d_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,char* str,int font_type,int font_size,int* pos, int pos_length,NspTypeBase *type)
{
 NspString3d *H  = nsp_string3d_create_void(name,type);
 if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_string3d_create_partial(H) == FAIL) return NULLSTRING3D;
  H->obj->Mcoord= Mcoord;
  H->obj->Mcoord_l = Mcoord_l;
  H->obj->str = str;
  H->obj->font_type=font_type;
  H->obj->font_size=font_size;
  H->obj->pos = pos;
  H->obj->pos_length = pos_length;
 if ( nsp_string3d_check_values(H) == FAIL) return NULLSTRING3D;
 return H;
}


NspString3d *nsp_string3d_create_default(char *name)
{
 NspString3d *H  = nsp_string3d_create_void(name,NULL);
 if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_string3d_create_partial(H) == FAIL) return NULLSTRING3D;
 if ( nsp_string3d_check_values(H) == FAIL) return NULLSTRING3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspString3d *nsp_string3d_copy_partial(NspString3d *H,NspString3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspString3d *nsp_string3d_copy(NspString3d *self)
{
  NspString3d *H  =nsp_string3d_create_void(NVOID,(NspTypeBase *) nsp_type_string3d);
  if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSTRING3D;
  if ( nsp_string3d_copy_partial(H,self)== NULL) return NULLSTRING3D;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspString3d *nsp_string3d_full_copy_partial(NspString3d *H,NspString3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_string3d))) == NULL) return NULLSTRING3D;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_full_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  H->obj->Mcoord_l = self->obj->Mcoord_l;
  if ((H->obj->str = nsp_string_copy(self->obj->str)) == NULL) return NULL;
  H->obj->font_type=self->obj->font_type;
  H->obj->font_size=self->obj->font_size;
  if ((H->obj->pos = malloc(self->obj->pos_length*sizeof(int)))== NULL) return NULL;
  H->obj->pos_length = self->obj->pos_length;
  memcpy(H->obj->pos,self->obj->pos,self->obj->pos_length*sizeof(int));
  return H;
}

NspString3d *nsp_string3d_full_copy(NspString3d *self)
{
  NspString3d *H  =nsp_string3d_create_void(NVOID,(NspTypeBase *) nsp_type_string3d);
  if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSTRING3D;
  if ( nsp_string3d_full_copy_partial(H,self)== NULL) return NULLSTRING3D;

#line 67 "codegen/string3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_string3d(H)== FAIL) return NULL; 

#line 576 "string3d.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspString3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_string3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspString3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type string3d is initialized */
  nsp_type_string3d = new_type_string3d(T_BASE);
  if(( H = nsp_string3d_create_void(NVOID,(NspTypeBase *) nsp_type_string3d)) == NULLSTRING3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_string3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_string3d_check_values(H) == FAIL) return RET_BUG;

#line 67 "codegen/string3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_string3d(H)== FAIL) return RET_BUG; 

#line 601 "string3d.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *string3d_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_string3d_get_Mcoord(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspString3d *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_string3d_get_obj_Mcoord(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspString3d *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_string3d_set_Mcoord(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcoord;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspString3d *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspString3d *) self)->obj->Mcoord);
  ((NspString3d *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static NspObject *_wrap_string3d_get_str(void *self,const char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspString3d *) self)->obj->str;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_string3d_set_str(void *self,const char *attr, NspObject *O)
{
  char *str;

  if ((str = nsp_string_object(O))==NULL) return FAIL;
  if ((str = nsp_string_copy(str)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspString3d *) self)->obj->str);
  ((NspString3d *) self)->obj->str= str;
  return OK;
}

static NspObject *_wrap_string3d_get_font_type(void *self,const char *attr)
{
  int ret;

  ret = ((NspString3d *) self)->obj->font_type;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_string3d_set_font_type(void *self,const char *attr, NspObject *O)
{
  int font_type;

  if ( IntScalar(O,&font_type) == FAIL) return FAIL;
  ((NspString3d *) self)->obj->font_type= font_type;
  return OK;
}

static NspObject *_wrap_string3d_get_font_size(void *self,const char *attr)
{
  int ret;

  ret = ((NspString3d *) self)->obj->font_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_string3d_set_font_size(void *self,const char *attr, NspObject *O)
{
  int font_size;

  if ( IntScalar(O,&font_size) == FAIL) return FAIL;
  ((NspString3d *) self)->obj->font_size= font_size;
  return OK;
}

static AttrTab string3d_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_string3d_get_Mcoord, (attr_set_function *)_wrap_string3d_set_Mcoord,(attr_get_object_function *)_wrap_string3d_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { "str", (attr_get_function *)_wrap_string3d_get_str, (attr_set_function *)_wrap_string3d_set_str,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "font_type", (attr_get_function *)_wrap_string3d_get_font_type, (attr_set_function *)_wrap_string3d_set_font_type,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "font_size", (attr_get_function *)_wrap_string3d_get_font_size, (attr_set_function *)_wrap_string3d_set_font_size,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 77 "codegen/string3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_string3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 719 "string3d.c"


#line 87 "codegen/string3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_string3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 732 "string3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab String3d_func[]={
  {"extractelts_string3d", _wrap_nsp_extractelts_string3d},
  {"setrowscols_string3d", _wrap_nsp_setrowscols_string3d},
  { "string3d_create", int_string3d_create},
  { NULL, NULL}
};

/* call ith function in the String3d interface */

int String3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(String3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void String3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = String3d_func[i].name;
  *f = String3d_func[i].fonc;
}

#line 98 "codegen/string3d.override"

/* inserted verbatim at the end */

static void nsp_draw_string3d(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int face; 
  if ( Obj->obj->hidden == TRUE ) return ;
  nsp_check_string3d((NspString3d *) Obj);
#ifdef  WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* if we are using OpenGl we make a full draw of 
       * object and return 
       */
      draw_string3d_ogl(Xgc,Obj);
      nsp_ogl_set_2dview(Xgc); 
      return; 
    }
#endif 
  if ( data != NULL) 
    {
      face = *((int *) data);
      draw_string3d_face(Xgc,Obj,face);
    }
  else 
    {
      int i;
      /* draw all the faces: this is not really used  
       * since the face order is computed and sequenced in upper object.
       */
      for ( i= 0 ; i < ((NspString3d *) Obj)->obj->Mcoord->n; i++) 
	draw_string3d_face(Xgc,Obj,i);
    }
}

static void nsp_translate_string3d(NspGraphic *Obj,const double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_string3d(NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_string3d(NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of string3d 
 *
 */
extern void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) ;

static int nsp_getbounds_string3d(NspGraphic *Obj,double *bounds)
{
  int i;
  /* this should be stored in a cache and recomputed when necessary 
   *
   */
  nsp_string3d *Q= ((NspString3d *) Obj)->obj;
  nsp_check_string3d((NspString3d *) Obj);
  if ( Q->Mcoord->mn == 0) return FALSE;
  for ( i = 0 ; i < Q->Mcoord->m ; i++) 
    nsp_gr_bounds_min_max(Q->Mcoord->n,Q->Mcoord->R+i,3,&bounds[2*i],&bounds[2*i+1]);
  return TRUE;
}

int nsp_check_string3d( NspString3d *P)
{
  nsp_string3d *S = P->obj;
  int S_nb_coords = S->Mcoord->n;

  if ( S->Mcoord->m != 3 || S->Mcoord->n != 1 ) 
    {
      Scierror("Error: bad coord for string3d\n");
      return FAIL;
    }

  /* create extra data for qpos declared int* 
   * Q->pos id only usefull for non opengl driver 
   */
  if ( S->pos == NULL) S->pos = malloc( S_nb_coords * sizeof(VisionPos));
  S->pos_length = S_nb_coords;
  
  /* create extra data for Mcoord_l declared void* */
  if ( S->Mcoord_l == NULL) 
    {
      S->Mcoord_l = nsp_matrix_create("local",'r',S->Mcoord->m, S->Mcoord->n);
    }
  return OK;
}


static void draw_string3d_face(BCG *Xgc,NspGraphic *Ob, int j);
static void draw_justified_string3d(BCG *Xgc,NspGraphic *V, int xj, int yj);
static void draw_justified_string(BCG *Xgc,char *str, double x, double y, int xj, int yj);
static void draw_string3d_ogl(BCG *Xgc,void *Ob);
static void draw_justified_string3d_ogl(BCG *Xgc,void *Obj, int xj, int yj);

static void draw_string3d_face(BCG *Xgc,NspGraphic *Ob, int j)
{
  draw_justified_string3d(Xgc,Ob,CENTER,CENTER);
}

static void draw_justified_string3d(BCG *Xgc,NspGraphic *Obj, int xj, int yj)
{
  int fontid[2],current_fontid[2];
  double x,y;
  nsp_string3d *V = ((NspString3d *) Obj)->obj;
  double *V_coord = ((NspMatrix *)V->Mcoord_l)->R;

  Xgc->graphic_engine->xget_font(Xgc,current_fontid);
  fontid[0]= ( V->font_type < 0 ) ? current_fontid[0] : V->font_type;
  fontid[1]= ( V->font_size < 0 ) ? current_fontid[1] : V->font_size;
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
  x = XScale(V_coord[0]);
  y = YScale(V_coord[1]);
  draw_justified_string(Xgc,V->str,x,y, xj, yj);
  Xgc->graphic_engine->xset_font(Xgc,current_fontid[0],current_fontid[1]);
}

static void draw_justified_string(BCG *Xgc,char *str, double x, double y, int xj, int yj)
{
  int flag=0, rect[4], w, h;
  double angle=0.0; 
  Xgc->graphic_engine->boundingbox(Xgc,str,x,y, rect);
  w = rect[2]; h = rect[3];
  if ( xj == CENTER ) 
    x -= w/2;
  else if ( xj == RIGHT )
    x -= w;
  if ( yj == CENTER )
    y += h/2;
  else if ( yj == DOWN )
    y += h;
  Xgc->graphic_engine->displaystring(Xgc,str,x,y, flag,angle);
}

static void draw_string3d_ogl(BCG *Xgc,void *Ob)
{
  draw_justified_string3d_ogl(Xgc,Ob,CENTER,CENTER);
}

static void draw_justified_string3d_ogl(BCG *Xgc,void *Obj, int xj, int yj)
{
#ifdef  WITH_GTKGLEXT 
  int fontid[2],current_fontid[2];
  nsp_string3d *S = ((NspString3d *) Obj)->obj;
  const double lim[] ={ 1.e+10,  1.e+10, - 1.e+10};
  /* we move to 2d scale */
  double Tcoord[3];
  double *S_coord = S->Mcoord->R;

  apply_transforms_new(Xgc,Tcoord,S_coord,S->pos,lim,1); 
  Tcoord[0] = XScale(Tcoord[0]);
  Tcoord[1] = YScale(Tcoord[1]);
  nsp_ogl_set_2dview(Xgc);
  Xgc->graphic_engine->xget_font(Xgc,current_fontid);
  fontid[0]= ( S->font_type < 0 ) ? current_fontid[0] : S->font_type;
  fontid[1]= ( S->font_size < 0 ) ? current_fontid[1] : S->font_size;
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
  draw_justified_string(Xgc,S->str,Tcoord[0],Tcoord[1], xj, yj);
  Xgc->graphic_engine->xset_font(Xgc,current_fontid[0],current_fontid[1]);
  nsp_ogl_set_3dview(Xgc);
#endif 
}


static void zmean_faces_for_String3d(void *Obj, double z[], HFstruct HF[], int *n, int k)
{
  int j;
  nsp_string3d *S = ((NspString3d *) Obj)->obj;
  int S_nb_coords = S->Mcoord->n;
  double *S_coord = ((NspMatrix *)S->Mcoord_l)->R;

  for ( j = 0 ; j < S_nb_coords ; j++)
    if (S->pos[j] == VIN)
      {
	z[*n] = S_coord[3*j+2]; 
	HF[*n].num_obj = k; 
	HF[*n].num_in_obj = j;
	(*n)++; 
      }
}


/*
 * requested method for 3d objects.
 */

static void nsp_string3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim)
{
  nsp_string3d *Q= ((NspString3d *) Obj)->obj;
  apply_transforms_new(Xgc,((NspMatrix *) Q->Mcoord_l)->R,Q->Mcoord->R,Q->pos, lim, Q->Mcoord->n);
  zmean_faces_for_String3d(Obj, z,  HF, n, k);
}

/* requested method for 3d objects.
 *
 */

static int nsp_string3d_n_faces(BCG *Xgc,NspGraphic *Obj)
{
  return ((NspString3d *) Obj)->obj->Mcoord->n;
}



#line 975 "string3d.c"
