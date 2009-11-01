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





#line 28 "codegen/gmatrix1.override"

#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include <nsp/axes.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 38 "gmatrix1.c"

/* ----------- NspGMatrix1 ----------- */


#define  NspGMatrix1_Private 
#include <nsp/object.h>
#include <nsp/gmatrix1.h>
#include <nsp/interf.h>

/* 
 * NspGMatrix1 inherits from Graphic 
 */

int nsp_type_gmatrix1_id=0;
NspTypeGMatrix1 *nsp_type_gmatrix1=NULL;

/*
 * Type object for NspGMatrix1 
 * all the instance of NspTypeGMatrix1 share the same id. 
 * nsp_type_gmatrix1: is an instance of NspTypeGMatrix1 
 *    used for objects of NspGMatrix1 type (i.e built with new_gmatrix1) 
 * other instances are used for derived classes 
 */
NspTypeGMatrix1 *new_type_gmatrix1(type_mode mode)
{
  NspTypeGMatrix1 *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmatrix1 != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmatrix1;
    }
  if (( type =  malloc(sizeof(NspTypeGMatrix1))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmatrix1_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmatrix1_get_methods;
  type->new = (new_func *) new_gmatrix1;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmatrix1 */ 

  top->pr = (print_func *) nsp_gmatrix1_print;
  top->dealloc = (dealloc_func *) nsp_gmatrix1_destroy;
  top->copy  =  (copy_func *) nsp_gmatrix1_copy;
  top->size  = (size_func *) nsp_gmatrix1_size;
  top->s_type =  (s_type_func *) nsp_gmatrix1_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmatrix1_type_short_string;
  top->info = (info_func *) nsp_gmatrix1_info;
  /* top->is_true = (is_true_func  *) nsp_gmatrix1_is_true; */
  /* top->loop =(loop_func *) nsp_gmatrix1_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gmatrix1_object;
  top->eq  = (eq_func *) nsp_gmatrix1_eq;
  top->neq  = (eq_func *) nsp_gmatrix1_neq;
  top->save  = (save_func *) nsp_gmatrix1_xdr_save;
  top->load  = (load_func *) nsp_gmatrix1_xdr_load;
  top->create = (create_func*) int_gmatrix1_create;
  top->latex = (print_func *) nsp_gmatrix1_latex;
  top->full_copy = (copy_func *) nsp_gmatrix1_full_copy;

  /* specific methods for gmatrix1 */

  type->init = (init_func *) init_gmatrix1;

#line 44 "codegen/gmatrix1.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_gmatrix1;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_gmatrix1 ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_gmatrix1  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_gmatrix1  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_gmatrix1  ;
  /* next method are defined in NspGraphic and need not be chnaged here for GMatrix1 */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 121 "gmatrix1.c"
  /* 
   * NspGMatrix1 interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmatrix1_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMatrix1 called nsp_type_gmatrix1
       */
      type->id =  nsp_type_gmatrix1_id = nsp_new_type_id();
      nsp_type_gmatrix1 = type;
      if ( nsp_register_type(nsp_type_gmatrix1) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gmatrix1(mode);
    }
  else 
    {
      type->id = nsp_type_gmatrix1_id;
      return type;
    }
}

/*
 * initialize NspGMatrix1 instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmatrix1(NspGMatrix1 *Obj,NspTypeGMatrix1 *type)
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
 * new instance of NspGMatrix1 
 */

NspGMatrix1 *new_gmatrix1() 
{
  NspGMatrix1 *loc;
  /* type must exists */
  nsp_type_gmatrix1 = new_type_gmatrix1(T_BASE);
  if ( (loc = malloc(sizeof(NspGMatrix1)))== NULLGMATRIX1) return loc;
  /* initialize object */
  if ( init_gmatrix1(loc,nsp_type_gmatrix1) == FAIL) return NULLGMATRIX1;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMatrix1 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gmatrix1_size(NspGMatrix1 *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gmatrix1_type_name[]="GMatrix1";
static char gmatrix1_short_type_name[]="gmatrix1";

static char *nsp_gmatrix1_type_as_string(void)
{
  return(gmatrix1_type_name);
}

static char *nsp_gmatrix1_type_short_string(NspObject *v)
{
  return(gmatrix1_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gmatrix1_eq(NspGMatrix1 *A, NspObject *B)
{
  NspGMatrix1 *loc = (NspGMatrix1 *) B;
  if ( check_cast(B,nsp_type_gmatrix1_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->data)->type->eq(A->obj->data,loc->obj->data) == FALSE ) return FALSE;
  if ( A->obj->remap != loc->obj->remap) return FALSE;
  if ( A->obj->shade != loc->obj->shade) return FALSE;
  if ( NSP_OBJECT(A->obj->colminmax)->type->eq(A->obj->colminmax,loc->obj->colminmax) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->zminmax)->type->eq(A->obj->zminmax,loc->obj->zminmax) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->colout)->type->eq(A->obj->colout,loc->obj->colout) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_gmatrix1_neq(NspGMatrix1 *A, NspObject *B)
{
  return ( nsp_gmatrix1_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gmatrix1_xdr_save(XDR *xdrs, NspGMatrix1 *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gmatrix1)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->data)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->remap) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->shade) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colminmax)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->zminmax)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colout)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGMatrix1  *nsp_gmatrix1_xdr_load_partial(XDR *xdrs, NspGMatrix1 *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_gmatrix1))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->data =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->remap) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->shade) == FAIL) return NULL;
  if ((M->obj->colminmax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->zminmax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->colout =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspGMatrix1  *nsp_gmatrix1_xdr_load(XDR *xdrs)
{
  NspGMatrix1 *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGMATRIX1;
  if ((H  = nsp_gmatrix1_create_void(name,(NspTypeBase *) nsp_type_gmatrix1))== NULLGMATRIX1) return H;
  if ((H  = nsp_gmatrix1_xdr_load_partial(xdrs,H))== NULLGMATRIX1) return H;
  if ( nsp_gmatrix1_check_values(H) == FAIL) return NULLGMATRIX1;
  return H;
}

/*
 * delete 
 */

void nsp_gmatrix1_destroy_partial(NspGMatrix1 *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->data != NULL ) 
      nsp_matrix_destroy(H->obj->data);
    if ( H->obj->colminmax != NULL ) 
      nsp_matrix_destroy(H->obj->colminmax);
    if ( H->obj->zminmax != NULL ) 
      nsp_matrix_destroy(H->obj->zminmax);
    if ( H->obj->colout != NULL ) 
      nsp_matrix_destroy(H->obj->colout);
    if ( H->obj->x != NULL ) 
      nsp_matrix_destroy(H->obj->x);
    if ( H->obj->y != NULL ) 
      nsp_matrix_destroy(H->obj->y);
    FREE(H->obj);
   }
}

void nsp_gmatrix1_destroy(NspGMatrix1 *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gmatrix1_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gmatrix1_info(NspGMatrix1 *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGMATRIX1) 
    {
      Sciprintf("Null Pointer NspGMatrix1 \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gmatrix1_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gmatrix1_print(NspGMatrix1 *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGMATRIX1) 
    {
      Sciprintf("Null Pointer NspGMatrix1 \n");
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
          nsp_gmatrix1_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gmatrix1_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->data != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->data),indent+2,"data",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"remap	= %s\n", ( M->obj->remap == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"shade	= %s\n", ( M->obj->shade == TRUE) ? "T" : "F" );
  if ( M->obj->colminmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colminmax),indent+2,"colminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zminmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->zminmax),indent+2,"zminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colout != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colout),indent+2,"colout",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gmatrix1_latex(NspGMatrix1 *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gmatrix1_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->data != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->data),indent+2,"data",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"remap	= %s\n", ( M->obj->remap == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"shade	= %s\n", ( M->obj->shade == TRUE) ? "T" : "F" );
  if ( M->obj->colminmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colminmax),indent+2,"colminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zminmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->zminmax),indent+2,"zminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colout != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colout),indent+2,"colout",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMatrix1 objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMatrix1   *nsp_gmatrix1_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gmatrix1_id) == TRUE ) return ((NspGMatrix1 *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmatrix1));
  return NULL;
}

int IsGMatrix1Obj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmatrix1_id);
}

int IsGMatrix1(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmatrix1_id);
}

NspGMatrix1  *GetGMatrix1Copy(Stack stack, int i)
{
  if (  GetGMatrix1(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMatrix1  *GetGMatrix1(Stack stack, int i)
{
  NspGMatrix1 *M;
  if (( M = nsp_gmatrix1_object(NthObj(i))) == NULLGMATRIX1)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGMatrix1 instance 
 *-----------------------------------------------------*/

static NspGMatrix1 *nsp_gmatrix1_create_void(char *name,NspTypeBase *type)
{
 NspGMatrix1 *H  = (type == NULL) ? new_gmatrix1() : type->new();
 if ( H ==  NULLGMATRIX1)
  {
   Sciprintf("No more memory\n");
   return NULLGMATRIX1;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGMATRIX1;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gmatrix1_create_partial(NspGMatrix1 *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_gmatrix1)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->data = NULLMAT;
  H->obj->remap = TRUE;
  H->obj->shade = TRUE;
  H->obj->colminmax = NULLMAT;
  H->obj->zminmax = NULLMAT;
  H->obj->colout = NULLMAT;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  return OK;
}

int nsp_gmatrix1_check_values(NspGMatrix1 *H)
{
  if ( H->obj->data == NULLMAT) 
    {
       if (( H->obj->data = nsp_matrix_create("data",'r',0,0)) == NULLMAT)
       return FAIL;

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
  if ( H->obj->colout == NULLMAT) 
    {
       if (( H->obj->colout = nsp_matrix_create("colout",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->x == NULLMAT) 
    {
       if (( H->obj->x = nsp_matrix_create("x",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->y == NULLMAT) 
    {
       if (( H->obj->y = nsp_matrix_create("y",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGMatrix1 *nsp_gmatrix1_create(char *name,NspMatrix* data,gboolean remap,gboolean shade,NspMatrix* colminmax,NspMatrix* zminmax,NspMatrix* colout,NspMatrix* x,NspMatrix* y,NspTypeBase *type)
{
 NspGMatrix1 *H  = nsp_gmatrix1_create_void(name,type);
 if ( H ==  NULLGMATRIX1) return NULLGMATRIX1;
  if ( nsp_gmatrix1_create_partial(H) == FAIL) return NULLGMATRIX1;
  H->obj->data= data;
  H->obj->remap=remap;
  H->obj->shade=shade;
  H->obj->colminmax= colminmax;
  H->obj->zminmax= zminmax;
  H->obj->colout= colout;
  H->obj->x= x;
  H->obj->y= y;
 if ( nsp_gmatrix1_check_values(H) == FAIL) return NULLGMATRIX1;
 return H;
}


NspGMatrix1 *nsp_gmatrix1_create_default(char *name)
{
 NspGMatrix1 *H  = nsp_gmatrix1_create_void(name,NULL);
 if ( H ==  NULLGMATRIX1) return NULLGMATRIX1;
  if ( nsp_gmatrix1_create_partial(H) == FAIL) return NULLGMATRIX1;
 if ( nsp_gmatrix1_check_values(H) == FAIL) return NULLGMATRIX1;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGMatrix1 *nsp_gmatrix1_copy_partial(NspGMatrix1 *H,NspGMatrix1 *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGMatrix1 *nsp_gmatrix1_copy(NspGMatrix1 *self)
{
  NspGMatrix1 *H  =nsp_gmatrix1_create_void(NVOID,(NspTypeBase *) nsp_type_gmatrix1);
  if ( H ==  NULLGMATRIX1) return NULLGMATRIX1;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGMATRIX1;
  if ( nsp_gmatrix1_copy_partial(H,self)== NULL) return NULLGMATRIX1;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGMatrix1 *nsp_gmatrix1_full_copy_partial(NspGMatrix1 *H,NspGMatrix1 *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_gmatrix1))) == NULL) return NULLGMATRIX1;
  H->obj->ref_count=1;
  if ( self->obj->data == NULL )
    { H->obj->data = NULL;}
  else
    {
      if ((H->obj->data = (NspMatrix *) nsp_object_full_copy_and_name("data",NSP_OBJECT(self->obj->data))) == NULLMAT) return NULL;
    }
  H->obj->remap=self->obj->remap;
  H->obj->shade=self->obj->shade;
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
  if ( self->obj->colout == NULL )
    { H->obj->colout = NULL;}
  else
    {
      if ((H->obj->colout = (NspMatrix *) nsp_object_full_copy_and_name("colout",NSP_OBJECT(self->obj->colout))) == NULLMAT) return NULL;
    }
  if ( self->obj->x == NULL )
    { H->obj->x = NULL;}
  else
    {
      if ((H->obj->x = (NspMatrix *) nsp_object_full_copy_and_name("x",NSP_OBJECT(self->obj->x))) == NULLMAT) return NULL;
    }
  if ( self->obj->y == NULL )
    { H->obj->y = NULL;}
  else
    {
      if ((H->obj->y = (NspMatrix *) nsp_object_full_copy_and_name("y",NSP_OBJECT(self->obj->y))) == NULLMAT) return NULL;
    }
  return H;
}

NspGMatrix1 *nsp_gmatrix1_full_copy(NspGMatrix1 *self)
{
  NspGMatrix1 *H  =nsp_gmatrix1_create_void(NVOID,(NspTypeBase *) nsp_type_gmatrix1);
  if ( H ==  NULLGMATRIX1) return NULLGMATRIX1;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGMATRIX1;
  if ( nsp_gmatrix1_full_copy_partial(H,self)== NULL) return NULLGMATRIX1;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGMatrix1
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gmatrix1_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGMatrix1 *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gmatrix1 is initialized */
  nsp_type_gmatrix1 = new_type_gmatrix1(T_BASE);
  if(( H = nsp_gmatrix1_create_void(NVOID,(NspTypeBase *) nsp_type_gmatrix1)) == NULLGMATRIX1) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_gmatrix1_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gmatrix1_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gmatrix1_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gmatrix1_get_data(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix1 *) self)->obj->data;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix1_get_obj_data(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix1 *) self)->obj->data);
  return (NspObject *) ret;
}

static int _wrap_gmatrix1_set_data(void *self,const char *attr, NspObject *O)
{
  NspMatrix *data;

  if ( ! IsMat(O) ) return FAIL;
  if ((data = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix1 *) self)->obj->data != NULL ) 
    nsp_matrix_destroy(((NspGMatrix1 *) self)->obj->data);
  ((NspGMatrix1 *) self)->obj->data= data;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_remap(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGMatrix1 *) self)->obj->remap;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_gmatrix1_set_remap(void *self,const char *attr, NspObject *O)
{
  int remap;

  if ( BoolScalar(O,&remap) == FAIL) return FAIL;
  ((NspGMatrix1 *) self)->obj->remap= remap;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_shade(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGMatrix1 *) self)->obj->shade;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_gmatrix1_set_shade(void *self,const char *attr, NspObject *O)
{
  int shade;

  if ( BoolScalar(O,&shade) == FAIL) return FAIL;
  ((NspGMatrix1 *) self)->obj->shade= shade;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_colminmax(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix1 *) self)->obj->colminmax;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix1_get_obj_colminmax(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix1 *) self)->obj->colminmax);
  return (NspObject *) ret;
}

static int _wrap_gmatrix1_set_colminmax(void *self,const char *attr, NspObject *O)
{
  NspMatrix *colminmax;

  if ( ! IsMat(O) ) return FAIL;
  if ((colminmax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix1 *) self)->obj->colminmax != NULL ) 
    nsp_matrix_destroy(((NspGMatrix1 *) self)->obj->colminmax);
  ((NspGMatrix1 *) self)->obj->colminmax= colminmax;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_zminmax(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix1 *) self)->obj->zminmax;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix1_get_obj_zminmax(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix1 *) self)->obj->zminmax);
  return (NspObject *) ret;
}

static int _wrap_gmatrix1_set_zminmax(void *self,const char *attr, NspObject *O)
{
  NspMatrix *zminmax;

  if ( ! IsMat(O) ) return FAIL;
  if ((zminmax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix1 *) self)->obj->zminmax != NULL ) 
    nsp_matrix_destroy(((NspGMatrix1 *) self)->obj->zminmax);
  ((NspGMatrix1 *) self)->obj->zminmax= zminmax;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_colout(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix1 *) self)->obj->colout;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix1_get_obj_colout(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix1 *) self)->obj->colout);
  return (NspObject *) ret;
}

static int _wrap_gmatrix1_set_colout(void *self,const char *attr, NspObject *O)
{
  NspMatrix *colout;

  if ( ! IsMat(O) ) return FAIL;
  if ((colout = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix1 *) self)->obj->colout != NULL ) 
    nsp_matrix_destroy(((NspGMatrix1 *) self)->obj->colout);
  ((NspGMatrix1 *) self)->obj->colout= colout;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_x(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix1 *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix1_get_obj_x(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix1 *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_gmatrix1_set_x(void *self,const char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix1 *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspGMatrix1 *) self)->obj->x);
  ((NspGMatrix1 *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_gmatrix1_get_y(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspGMatrix1 *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_gmatrix1_get_obj_y(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspGMatrix1 *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_gmatrix1_set_y(void *self,const char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspGMatrix1 *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspGMatrix1 *) self)->obj->y);
  ((NspGMatrix1 *) self)->obj->y= y;
  return OK;
}

static AttrTab gmatrix1_attrs[] = {
  { "data", (attr_get_function *)_wrap_gmatrix1_get_data, (attr_set_function *)_wrap_gmatrix1_set_data,(attr_get_object_function *)_wrap_gmatrix1_get_obj_data, (attr_set_object_function *)int_set_object_failed },
  { "remap", (attr_get_function *)_wrap_gmatrix1_get_remap, (attr_set_function *)_wrap_gmatrix1_set_remap,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "shade", (attr_get_function *)_wrap_gmatrix1_get_shade, (attr_set_function *)_wrap_gmatrix1_set_shade,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "colminmax", (attr_get_function *)_wrap_gmatrix1_get_colminmax, (attr_set_function *)_wrap_gmatrix1_set_colminmax,(attr_get_object_function *)_wrap_gmatrix1_get_obj_colminmax, (attr_set_object_function *)int_set_object_failed },
  { "zminmax", (attr_get_function *)_wrap_gmatrix1_get_zminmax, (attr_set_function *)_wrap_gmatrix1_set_zminmax,(attr_get_object_function *)_wrap_gmatrix1_get_obj_zminmax, (attr_set_object_function *)int_set_object_failed },
  { "colout", (attr_get_function *)_wrap_gmatrix1_get_colout, (attr_set_function *)_wrap_gmatrix1_set_colout,(attr_get_object_function *)_wrap_gmatrix1_get_obj_colout, (attr_set_object_function *)int_set_object_failed },
  { "x", (attr_get_function *)_wrap_gmatrix1_get_x, (attr_set_function *)_wrap_gmatrix1_set_x,(attr_get_object_function *)_wrap_gmatrix1_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_gmatrix1_get_y, (attr_set_function *)_wrap_gmatrix1_set_y,(attr_get_object_function *)_wrap_gmatrix1_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 65 "codegen/gmatrix1.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_gmatrix1(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 921 "gmatrix1.c"


#line 75 "codegen/gmatrix1.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_gmatrix1(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 933 "gmatrix1.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GMatrix1_func[]={
  {"extractelts_gmatrix1", _wrap_nsp_extractelts_gmatrix1},
  {"setrowscols_gmatrix1", _wrap_nsp_setrowscols_gmatrix1},
  { "gmatrix1_create", int_gmatrix1_create},
  { NULL, NULL}
};

/* call ith function in the GMatrix1 interface */

int GMatrix1_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GMatrix1_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GMatrix1_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GMatrix1_func[i].name;
  *f = GMatrix1_func[i].fonc;
}

#line 85 "codegen/gmatrix1.override"

/* inserted verbatim at the end */

/*
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z, zmoy, is computed 
 *  and the rectangle is painted with a color which depends on 
 *  the value of zmoy 
 *  the z values, or values from zminmax=[zmin,zmax] if @zminmax is non NULL
 *  are linearly remapped to the min and max values of clors in 
 *  the current colormap or to [colormin,colormax] if @colminmax is non NULL.
 *  if remap is set to false @z values are directly casted to color values. 
 *  colout can be used to give the colors for z values which are below 
 *  zminmax[0] or above zminmax[1] (a zero value is interpreted as no painting).
 */

static void nsp_draw_gmatrix1(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspGMatrix1 *P = (NspGMatrix1 *) Obj;
  if ( P->obj->shade==TRUE) 
    nsp_draw_matrix_shade(Xgc,Obj,data);
  else
    nsp_draw_matrix_zmoy(Xgc,Obj,data);
      
}


static void nsp_translate_gmatrix1(NspGraphic *Obj,const double *tr)
{
  int i;
  NspGMatrix1 *P = (NspGMatrix1 *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) += tr[0];
    }
  for ( i=0; i < P->obj->y->mn ; i++) 
    {
      *(y++) += tr[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_gmatrix1(NspGraphic *Obj,double *R)
{
  /* nsp_figure_force_redraw(Obj->obj->Fig); */
}

static void nsp_scale_gmatrix1(NspGraphic *Obj,double *alpha)
{
  int i;
  NspGMatrix1 *P = (NspGMatrix1 *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) *= alpha[0];
    }
  for ( i=0; i < P->obj->y->mn ; i++) 
    {
      *(y++) *= alpha[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of gmatrix1 
 *
 */

static int nsp_getbounds_gmatrix1 (NspGraphic *Obj,double *bounds)
{
  NspGMatrix1 *P = (NspGMatrix1 *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  if ( P->obj->x->mn == 0 || P->obj->y->mn == 0) return FALSE;
  bounds[0]=P->obj->x->R[0]; /* xmin */
  bounds[1]=P->obj->y->R[0]; /* ymin */
  bounds[2]=P->obj->x->R[P->obj->x->mn-1] ;/* xmax */
  bounds[3]=P->obj->y->R[P->obj->y->mn-1];/* ymax */
  return TRUE;
}



static void nsp_draw_matrix_zmoy(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspGMatrix1 *P = (NspGMatrix1 *) Obj;
  int remap = P->obj->remap; 
  int *xm,*ym,  j;
  int  *colminmax = NULL, *colout=NULL;
  double zminmax[2];
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  if ( P->obj->colminmax->mn == 2 ) 
    {
      /* colminmax is supposed to be converted to int */
      colminmax =  P->obj->colminmax->I;
    }
  if ( P->obj->colout->mn == 2 ) 
    {
      /* colout is supposed to be converted to int */
      colout =  P->obj->colout->I;
    }

  if ( P->obj->zminmax->mn == 2 ) 
    {
      zminmax[0]= P->obj->zminmax->R[0];
      zminmax[1]= P->obj->zminmax->R[1];
    }
  else
    {
      /* it should be better not to compute max/min 
       * for each redraw.
       */
      zminmax[0]= Mini(P->obj->data->R,P->obj->data->mn);
      zminmax[1]= Maxi(P->obj->data->R,P->obj->data->mn);
    }
  
  xm = graphic_alloc(0,P->obj->x->mn,sizeof(int));
  ym = graphic_alloc(1,P->obj->y->mn,sizeof(int));
  
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return ;
    }      
  /* Drawing the curves */
  for ( j =0 ; j < P->obj->x->mn ; j++)	 xm[j]= XScale(Xgc->scales,P->obj->x->R[j]);
  for ( j =0 ; j < P->obj->y->mn ; j++)	 ym[j]= YScale(Xgc->scales,P->obj->y->R[j]); 

  Xgc->graphic_engine->fill_grid_rectangles(Xgc,xm,ym,P->obj->data->R,
					    P->obj->data->m, 
					    P->obj->data->n,
					    remap,
					    colminmax,
					    zminmax,
					    colout);
}


/**
 * nsp_draw_matrix_shade:
 * @Xgc: 
 * @x: 
 * @y: 
 * @z: 
 * @n1: 
 * @n2: 
 * @strflag: 
 * @brect: 
 * @aaint: 
 * @l1: 
 * 
 * similar to @nsp_draw_matrix but the we assume that the 
 * function is piecewise linear on triangles and we use 
 * code similar to code used in nsp_fec to linearly interpolate 
 * colors in triangles. 
 * The x,y grid is decomposed as follows 
 *  ______ 
 *  | /| /|
 *  |/_|/_|  
 *  | /| /|
 *  |/_|/_|
 * 
 */

#if 1
/* FIXME */
extern void fillpolyline2D_shade(BCG *Xgc,int *vx, int *vy, int *colors, int n,int closeflag);
extern Gengine GL_gengine;
#endif 


static void nsp_draw_matrix_shade(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspGMatrix1 *P = (NspGMatrix1 *) Obj;
  int remap = P->obj->remap; 
  int nx = P->obj->x->mn;
  int ny = P->obj->y->mn;
  int *xm,*ym,i,  j, k;
  double zminmax[2];
  double *func= P->obj->data->R;
  int Nnode= nx*ny;
  int *colminmax = NULL, *colout = NULL;

  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;

  if ( P->obj->colminmax->mn == 2 ) 
    {
      colminmax = P->obj->colminmax->I;
    }

  if ( P->obj->colout->mn == 2 ) 
    {
      /* colout is supposed to be converted to int */
      colout =  P->obj->colout->I;
    }

  if ( P->obj->zminmax->mn == 2 ) 
    {
      zminmax[0]= P->obj->zminmax->R[0];
      zminmax[1]= P->obj->zminmax->R[1];
    }
  else
    {
      /* it should be better not to compute max/min 
       * for each redraw.
       */
      zminmax[0]= Mini(P->obj->data->R,P->obj->data->mn);
      zminmax[1]= Maxi(P->obj->data->R,P->obj->data->mn);
    }
  
  /* Allocation */
  xm = graphic_alloc(0,Nnode,sizeof(int));
  ym = graphic_alloc(1,Nnode,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
    }      
  
  for ( i = 0 ; i < nx  ; i++ )
    for ( j = 0 ; j < ny  ; j++) 
      {
	double xp=P->obj->x->R[i],yp=P->obj->y->R[j];
	scale_f2i(Xgc->scales,&xp,&yp,xm+i+nx*j,ym+i+nx*j,1);
      }

  /* Fec code */
  {
    int nz;
    int whiteid;
    
    double *zlevel, dz, zmin, zmax, fxy[3], sx[3], sy[3];
    int *zone, *fill, kp, perm[3], zxy[3], color_min;

    /* choice between zmin and zmax given by the user or computed
     * with the min and max z values.
     */

    if ( zminmax == NULL  ) { 
      zmin=(double) Mini(func,Nnode);
      zmax=(double) Maxi(func,Nnode);
    } 
    else {
      zmin = Min( zminmax[0] , zminmax[1] );
      zmax = Max( zminmax[0] , zminmax[1] );
    };
      
    whiteid= Xgc->graphic_engine->xget_last(Xgc);
    nz=whiteid;
    
    /* choice for the colormap (in case of a user 's choice 
     * verify the parameter). For the automatic choice I have
     * put colminmax[0]=colominmax[1]=1 in matdes.c  
     */

    if ( colminmax == NULL  )  /* automatic choice (see matdes.c) */
      color_min=1; 
    else if ( colminmax[0] < 1 || colminmax[1] > nz || colminmax[0] > colminmax[1] ) {
      /* ici on pourrait plutot forcer les choses en imposant 1<= colmin < colmax <= nz */
      sciprint("Error: colminmax badly choosen it should be in [0,%d]\n",nz);
      return; 
    }
    else {
      color_min = colminmax[0];
      nz = colminmax[1] - colminmax[0] + 1;
    };
      
    /* 
     *  1/ the purpose of the first part is to to compute the "zone" of each point :
     *    
     *    - the array zlevel are the boundaries between the differents zones :
     *        zlevel[0] = zmin, zlevel[nz] = zmax 
     *     and zlevel[i] = zmin + i*(zmax-zmin)/nz
     *  
     *     - if  zlevel[j-1] <= func[i] < zlevel[j]  then zone[i] = j
     *       if func[i] > zmax  then zone[i] = nz+1
     *       if func[i] < zmin  then zone[i] = 0
     *     - the zone j is filled with color fill[j] with
     *       fill[j] = -(j-1 + color_min) if 1 <= j <= nz
     *     - if colout == NULL
     *        fill[0] = color attributed for fill[1]     ---> this behavior may be changed ...
     *        fill[nz+1] = color attributed for fill[nz] --/
     *       else 
     *        fill[0]=- colout[0];
     *        fill[1]=- colout[1];
     */
 
    /* allocations for some arrays ... */

    zone = graphic_alloc(2,Nnode,sizeof(int));
    zlevel = graphic_alloc(3,nz+1,sizeof(double));
    fill  = graphic_alloc(4,nz+2,sizeof(int));
    if ( (zone == NULL) || (zlevel == NULL) || (fill  == NULL)) 
      {
	Scistring("fec: malloc No more Place\n");
	return ;
      }
    /* compute the fill array (fill = - num color) */
    fill[1] = - color_min;
    for ( i = 2 ; i <= nz ; i++ ) fill[i] = fill[i-1] - 1;
    if ( colout == NULL) 
      {
	fill[0] =  fill[1] ; fill[nz+1] = fill[nz];
      }
    else 
      {
	fill[0] = - Max(colout[0],0) ; fill[nz+1] = - Max(colout[1],0);
      }


    /* finaly compute the zone of each point */
    
    if (remap == FALSE && colminmax == NULL && P->obj->zminmax->mn != 2 )
      {
	for (i = 0 ; i <= nz ; i++) zlevel[i] = i;
	for ( i = 0 ; i < Nnode ; i++ ) zone[i] = Min(Max(func[i],0),nz+1);
      }
    else
      {
	/* compute the zlevels */
	dz = (zmax - zmin)/nz;
	for (i = 0 ; i < nz ; i++) zlevel[i] = zmin + i*dz;
	zlevel[nz] = zmax;
	for ( i = 0 ; i < Nnode ; i++ ) {
	  if ( func[i] > zmax )
	    zone[i] = nz+1;
	  else if ( func[i] < zmin )
	    zone[i] = 0;
	  else
	    zone[i] = floor( (func[i] - zmin)/dz ) + 1;
	}
      }

    /* 
     * 2/ loop on the triangles : each triangle is finally decomposed 
     *    into its differents zones (polygons) by the function PaintTriangle   
     */

#ifdef  WITH_GTKGLEXT 
    if (  Xgc->graphic_engine == &GL_gengine ) 
      {
	for ( i = 0 ; i < nx -1 ; i++ )
	  for ( j = 0 ; j < ny -1 ; j++) 
	    {
	      int pos[4],colors[4],xp[4],yp[4];
	      pos[0]=i+nx*j; pos[1]=pos[0]+nx;  pos[2]=pos[0]+nx+1, pos[3]=pos[0]+1;
	      for ( k = 0 ; k < 4 ; k++ ) 
		{
		  colors[k]= fill[zone[pos[k]]];
		  xp[k]= xm[pos[k]];
		  yp[k]= ym[pos[k]];
		}
	      fillpolyline2D_shade(Xgc,xp,yp,colors,4,1);
	    }
      }
    else 
#endif
      {
	for ( i = 0 ; i < nx -1 ; i++ )
	  for ( j = 0 ; j < ny -1 ; j++) 
	    {
	      int pos[3],tr;
	      for ( tr = 0 ; tr < 2 ; tr ++ )
		{
		  if ((i +j) % 2 == 0 )
		    {
		      if ( tr == 0 ) 
			{ 
			  /* upper triangle 
			   *  ___
			   *  | /
			   *  |/
			   */
			  pos[0]=i+nx*j; pos[1]=pos[0]+nx;  pos[2]=pos[0]+nx+1;
			}
		      else 
			{
			  /* lower triangle 
			   * 
			   *   /|
			   *  /_|
			   */
			  pos[0]=i+nx*j;  pos[1]=pos[0]+1;   pos[2]=pos[0]+nx+1;
			}
		    }
		  else
		    {
		      if ( tr == 0 ) 
			{ 
			  /* upper triangle 
			   *  
			   *  |\
			   *  |_\
			   */
			  pos[0]=i+nx*j; pos[1]=pos[0]+1;  pos[2]=pos[0]+nx;
			}
		      else 
			{
			  /* lower triangle 
			   *  ___
			   *  \ |
			   *   \|
			   */
			  pos[0]=i+nx*j+1;  pos[1]=pos[0]+nx;   pos[2]=pos[0]+nx-1;
			}
		    }

		  /* retrieve node numbers and functions values */
		  for ( k = 0 ; k < 3 ; k++ ) zxy[k]= zone[pos[k]];
		  /* get the permutation perm so as zxy[perm] is sorted */
		  PermutOfSort(zxy, perm); 
		  /* apply the permutation to get the triangle 's vertices
		   * in increasing zone (zxy[0] <= zxy[1] <= zxy[2]) 
		   */
		  for ( k = 0 ; k < 3 ; k++ ) {
		    kp = perm[k];
		    zxy[k]= zone[pos[kp]];
		    fxy[k]= func[pos[kp]];
		    sx[k] = xm[pos[kp]];
		    sy[k] = ym[pos[kp]];
		  }
		  /* call the "painting" function */
		  PaintTriangle(Xgc,sx, sy, fxy, zxy, zlevel, fill);
		}
	    }
      }
  }
}



/**
 * PermutOfSort:
 * @tab: 
 * @perm: 
 * 
 * functions used above (Bruno 01/02/2001)
 * 
 **/

void PermutOfSort (const int *tab, int *perm)
{
  /* 
   * get the permutation perm[3] which sort the array tab[3] in increasing order 
   */
  perm[0]=0; perm[1] = 1; perm[2] = 2;
  if ( tab[1] < tab[0] ) {
    perm[1]=0 ; perm[0] = 1;
  };
  if ( tab[2] < tab[perm[1]] ) {   /* sort not finish */
    if ( tab[2] < tab[perm[0]] ) {
      perm[2] = perm[1]; perm[1] = perm[0]; perm[0] = 2; 
    }
    else {
      perm[2] = perm[1] ; perm[1] = 2;
    };
  };
}


/**
 * PaintTriangle:
 * @Xgc: 
 * @sx: 
 * @sy: vertices coordinates of a triangle (Pi=(sx[i],sy[i]) i=0,1,2)
 * @fxy: fxy[i], (i=0,1,2) value of an affine function on the vertex Pi
 * @zxy: zone of Pi : zxy[i]=j if  zlevel[j-1] <= fxy[i] < zlevel[j]
 * @zlevel: a (0..nz) vector given the boundaries for color filling
 * @fill: fill[j] is the color pattern associated with zone[j] when fill[j]=0 the 
 * zone is not painted.
 * 
 * decomposes the triangle into its different zones (which gives polygones) and
 * send them to the graphic driver. This is something like the shade function
 * (see Plo3d.c) but a little different as in shade a color is directly
 * associated with each vertex.
 *
 **/

void PaintTriangle (BCG *Xgc,const double *sx,const double *sy,const  double *fxy, 
		    const int *zxy, const double *zlevel,const int *fill)
{
  int nb0, edge, izone, color;
  int nr, resx[5],resy[5];
  int xEdge2, yEdge2, xEdge, yEdge; 

  /* 
   * case of only one color for the triangle : 
   */

  if ( zxy[0] == zxy[2] ) {
    resx[0]=inint(sx[0]); resx[1]=inint(sx[1]);  resx[2]=inint(sx[2]);
    resy[0]=inint(sy[0]); resy[1]=inint(sy[1]);  resy[2]=inint(sy[2]);
    color = fill[zxy[0]]; nr = 3;
    if ( color != 0 ) 
      {
	Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
      }
    return;
  }

  /* 
   *  at least 2 colors for painting the triangle : it is divided in elementary
   **  polygons. The number of polygons is npolys = zxy[2]-zxy[0]+1.
   *
   *  P2           as zxy[0] <= zxy[1] <  zxy[2] or 
   *  Notations/Hints :       /\           zxy[0] <  zxy[1] <= zxy[2]
   *                  edge2  /  \ edge1    from a previus sort. All the polygons
   *                        /    \         have 2 points on edge2, the others points
   *                       /______\        are on edge0 and/or edge1. I name the 2 ends
   *                     P0        P1      points on each poly PEdge2 and Pedge, they are 
   *                         edge0         the 2 first points of the next poly. I start
   *  from P0 to form the first poly (a triangle or
   *  a 4 sides depending if zxy[0]=zxy[1]), then the 2, 3, .., npolys - 1 (if they exist)
   *  and finally the last one which comprise the P2 vertex.  In some special cases
   *  we can have a degenerate poly but it doesn't matter ! 				  
   */
  
  nb0 = zxy[1]-zxy[0]; /* number of intersection points on edge 0 */

  /*
   *    compute the first poly    
   */
  
  resx[0]=inint(sx[0]); resy[0]=inint(sy[0]); nr = 1; edge = 0;
  if ( nb0 == 0 ) {    /* the intersection point is on Edge1 but the next point
                          of the poly is P1 */
    resx[1]=inint(sx[1]); resy[1]=inint(sy[1]); nr++;
    edge = 1;          /* the next intersection points will be on edge1 */
  } else nb0--;
  /* the intersection point on edge (0 or 1) : */
  FindIntersection(sx, sy, fxy, zlevel[zxy[0]], edge, edge+1, &xEdge, &yEdge);
  resx[nr]=xEdge; resy[nr]=yEdge; nr++;
  /* the last point of the first poly (edge 2) : */
  FindIntersection(sx, sy, fxy, zlevel[zxy[0]], 0, 2, &xEdge2, &yEdge2);
  resx[nr]=xEdge2; resy[nr]=yEdge2; nr++;
  color = fill[zxy[0]];
  if ( color != 0 )   Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);

  /*
   *  compute the intermediary polygon(s) 
   */

  for ( izone = zxy[0]+1 ; izone < zxy[2] ; izone++ ) 
    {
      resx[0] = xEdge2; resy[0] = yEdge2;          /* the 2 first points are known */
      resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
      if ( edge == 0 )
	{
	  /* the intersection point is perhaps on edge 0 */
	  if (nb0 == 0 ) 
	    {
	      /* no it is on edge 1 but the next point of the poly is P1 */
	      resx[2]=inint(sx[1]); resy[2]=inint(sy[1]); nr++;
	      edge = 1;          /* the next intersection points will be on edge1 */
	    } 
	  else 
	    {
	      nb0--;
	    }
	}
      /* the intersection point on edge (0 or 1) : */
      FindIntersection(sx, sy, fxy, zlevel[izone], edge, edge+1, &xEdge, &yEdge);
      resx[nr]=xEdge; resy[nr]=yEdge; nr++;
      /* the last point of the first poly (edge 2) : */
      FindIntersection(sx, sy, fxy, zlevel[izone], 0, 2, &xEdge2, &yEdge2);
      resx[nr]=xEdge2; resy[nr]=yEdge2; nr++;
      color = fill[izone];
      if ( color != 0 ) 
	Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
    }

  /*
   *  compute the last poly  
   */

  resx[0] = xEdge2; resy[0] = yEdge2;         /* the 2 first points are known */
  resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
  if ( edge == 0 ) 
    {
      /* the next point of the poly is P1 */
      resx[2]=inint(sx[1]); resy[2]=inint(sy[1]); nr++;
    }

  /* the last point is P2 */
  resx[nr] = inint(sx[2]); resy[nr] = inint(sy[2]); nr++;
  color = fill[zxy[2]];
  if ( color != 0 )  Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);


}


void FindIntersection(const double *sx,const double *sy,const double *fxy,double z,int inda, int indb,
		      int *xint, int *yint)
{
  double alpha =  (z - fxy[inda])/(fxy[indb] - fxy[inda]);
  *xint = inint((1 - alpha)*sx[inda] + alpha*sx[indb]);
  *yint = inint((1 - alpha)*sy[inda] + alpha*sy[indb]);
}



#line 1565 "gmatrix1.c"
