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





#line 25 "codegen/segments.override"

#line 30 "segments.c"

/* -----------NspSegments ----------- */


#define  NspSegments_Private 
#include <nsp/objects.h>
#include <nsp/segments.h>
#include <nsp/interf.h>

/* 
 * NspSegments inherits from Graphic 
 */

int nsp_type_segments_id=0;
NspTypeSegments *nsp_type_segments=NULL;

/*
 * Type object for NspSegments 
 * all the instance of NspTypeSegments share the same id. 
 * nsp_type_segments: is an instance of NspTypeSegments 
 *    used for objects of NspSegments type (i.e built with new_segments) 
 * other instances are used for derived classes 
 */
NspTypeSegments *new_type_segments(type_mode mode)
{
  NspTypeSegments *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_segments != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_segments;
    }
  if (( type =  malloc(sizeof(NspTypeSegments))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = segments_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = segments_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_segments;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for segments */ 

  top->pr = (print_func *) nsp_segments_print;
  top->dealloc = (dealloc_func *) nsp_segments_destroy;
  top->copy  =  (copy_func *) nsp_segments_copy;
  top->size  = (size_func *) nsp_segments_size;
  top->s_type =  (s_type_func *) nsp_segments_type_as_string;
  top->sh_type = (sh_type_func *) nsp_segments_type_short_string;
  top->info = (info_func *) nsp_segments_info;
  /* top->is_true = (is_true_func  *) nsp_segments_is_true; */
  /* top->loop =(loop_func *) nsp_segments_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_segments_object;
  top->eq  = (eq_func *) nsp_segments_eq;
  top->neq  = (eq_func *) nsp_segments_neq;
  top->save  = (save_func *) nsp_segments_xdr_save;
  top->load  = (load_func *) nsp_segments_xdr_load;
  top->create = (create_func*) int_segments_create;
  top->latex = (print_func *) nsp_segments_latex;
  top->full_copy = (copy_func *) nsp_segments_full_copy;

  /* specific methods for segments */

  type->init = (init_func *) init_segments;

#line 33 "codegen/segments.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_segments;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_segments ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_segments  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_segments  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_segments  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Segments */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 116 "segments.c"
  /* 
   * NspSegments interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_segments_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSegments called nsp_type_segments
       */
      type->id =  nsp_type_segments_id = nsp_new_type_id();
      nsp_type_segments = type;
      if ( nsp_register_type(nsp_type_segments) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_segments(mode);
    }
  else 
    {
      type->id = nsp_type_segments_id;
      return type;
    }
}

/*
 * initialize NspSegments instances 
 * locally and by calling initializer on parent class 
 */

static int init_segments(NspSegments *Obj,NspTypeSegments *type)
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
 * new instance of NspSegments 
 */

NspSegments *new_segments() 
{
  NspSegments *loc;
  /* type must exists */
  nsp_type_segments = new_type_segments(T_BASE);
  if ( (loc = malloc(sizeof(NspSegments)))== NULLSEGMENTS) return loc;
  /* initialize object */
  if ( init_segments(loc,nsp_type_segments) == FAIL) return NULLSEGMENTS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspSegments 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_segments_size(NspSegments *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char segments_type_name[]="Segments";
static char segments_short_type_name[]="segments";

static char *nsp_segments_type_as_string(void)
{
  return(segments_type_name);
}

static char *nsp_segments_type_short_string(NspObject *v)
{
  return(segments_short_type_name);
}

/*
 * A == B 
 */

static int nsp_segments_eq(NspSegments *A, NspObject *B)
{
  NspSegments *loc = (NspSegments *) B;
  if ( check_cast(B,nsp_type_segments_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->color)->type->eq(A->obj->color,loc->obj->color) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->thickness)->type->eq(A->obj->thickness,loc->obj->thickness) == FALSE ) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_segments_neq(NspSegments *A, NspObject *B)
{
  return ( nsp_segments_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_segments_xdr_save(XDR *xdrs, NspSegments *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_segments)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->color)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->thickness)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspSegments  *nsp_segments_xdr_load_partial(XDR *xdrs, NspSegments *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->color =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->thickness =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspSegments  *nsp_segments_xdr_load(XDR *xdrs)
{
  NspSegments *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSEGMENTS;
  if ((H  = nsp_segments_create_void(name,(NspTypeBase *) nsp_type_segments))== NULLSEGMENTS) return H;
  if ( nsp_segments_create_partial(H) == FAIL) return NULLSEGMENTS;
  if ((H  = nsp_segments_xdr_load_partial(xdrs,H))== NULLSEGMENTS) return H;
  if ( nsp_segments_check_values(H) == FAIL) return NULLSEGMENTS;
  return H;
}

/*
 * delete 
 */

void nsp_segments_destroy_partial(NspSegments *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->x != NULL ) 
      nsp_matrix_destroy(H->obj->x);
    if ( H->obj->y != NULL ) 
      nsp_matrix_destroy(H->obj->y);
    if ( H->obj->color != NULL ) 
      nsp_matrix_destroy(H->obj->color);
    if ( H->obj->thickness != NULL ) 
      nsp_matrix_destroy(H->obj->thickness);
    FREE(H->obj);
   }
}

void nsp_segments_destroy(NspSegments *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_segments_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_segments_info(NspSegments *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSEGMENTS) 
    {
      Sciprintf("Null Pointer NspSegments \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_segments_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_segments_print(NspSegments *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSEGMENTS) 
    {
      Sciprintf("Null Pointer NspSegments \n");
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
          nsp_segments_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_segments_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->color != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->color),indent+2,"color", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->thickness != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->thickness),indent+2,"thickness", rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_segments_latex(NspSegments *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_segments_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),FALSE,"x", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),FALSE,"y", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->color != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->color),FALSE,"color", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->thickness != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->thickness),FALSE,"thickness", rec_level+1)== FALSE ) return FALSE ;
    }
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
 * for NspSegments objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspSegments   *nsp_segments_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_segments_id)  == TRUE  ) return ((NspSegments *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_segments));
  return NULL;
}

int IsSegmentsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_segments_id);
}

int IsSegments(NspObject *O)
{
  return nsp_object_type(O,nsp_type_segments_id);
}

NspSegments  *GetSegmentsCopy(Stack stack, int i)
{
  if (  GetSegments(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSegments  *GetSegments(Stack stack, int i)
{
  NspSegments *M;
  if (( M = nsp_segments_object(NthObj(i))) == NULLSEGMENTS)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspSegments instance 
 *-----------------------------------------------------*/

static NspSegments *nsp_segments_create_void(const char *name,NspTypeBase *type)
{
 NspSegments *H  = (type == NULL) ? new_segments() : type->new();
 if ( H ==  NULLSEGMENTS)
  {
   Sciprintf("No more memory\n");
   return NULLSEGMENTS;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSEGMENTS;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_segments_create_partial(NspSegments *H)
{
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_segments)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->color = NULLMAT;
  H->obj->thickness = NULLMAT;
  return OK;
}

int nsp_segments_check_values(NspSegments *H)
{
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
  if ( H->obj->color == NULLMAT) 
    {
       if (( H->obj->color = nsp_matrix_create("color",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->thickness == NULLMAT) 
    {
       if (( H->obj->thickness = nsp_matrix_create("thickness",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspSegments *nsp_segments_create(const char *name,NspMatrix* x,NspMatrix* y,NspMatrix* color,NspMatrix* thickness,NspTypeBase *type)
{
  NspSegments *H  = nsp_segments_create_void(name,type);
  if ( H ==  NULLSEGMENTS) return NULLSEGMENTS;
  if ( nsp_segments_create_partial(H) == FAIL) return NULLSEGMENTS;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->color= color;
  H->obj->thickness= thickness;
  if ( nsp_segments_check_values(H) == FAIL) return NULLSEGMENTS;
  return H;
}


NspSegments *nsp_segments_create_default(const char *name)
{
 NspSegments *H  = nsp_segments_create_void(name,NULL);
 if ( H ==  NULLSEGMENTS) return NULLSEGMENTS;
  if ( nsp_segments_create_partial(H) == FAIL) return NULLSEGMENTS;
  if ( nsp_segments_check_values(H) == FAIL) return NULLSEGMENTS;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspSegments *nsp_segments_copy_partial(NspSegments *H,NspSegments *self)
{
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLSEGMENTS;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspSegments *nsp_segments_copy(NspSegments *self)
{
  NspSegments *H  =nsp_segments_create_void(NVOID,(NspTypeBase *) nsp_type_segments);
  if ( H ==  NULLSEGMENTS) return NULLSEGMENTS;
  if ( nsp_segments_copy_partial(H,self)== NULL) return NULLSEGMENTS;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspSegments *nsp_segments_full_copy_partial(NspSegments *H,NspSegments *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLSEGMENTS;
  if ((H->obj = calloc(1,sizeof(nsp_segments))) == NULL) return NULLSEGMENTS;
  H->obj->ref_count=1;
  if ( self->obj->x == NULL )
    { H->obj->x = NULL;}
  else
    {
      if ((H->obj->x = (NspMatrix *) nsp_object_full_copy_and_name("x", NSP_OBJECT(self->obj->x))) == NULLMAT) return NULL;
    }
  if ( self->obj->y == NULL )
    { H->obj->y = NULL;}
  else
    {
      if ((H->obj->y = (NspMatrix *) nsp_object_full_copy_and_name("y", NSP_OBJECT(self->obj->y))) == NULLMAT) return NULL;
    }
  if ( self->obj->color == NULL )
    { H->obj->color = NULL;}
  else
    {
      if ((H->obj->color = (NspMatrix *) nsp_object_full_copy_and_name("color", NSP_OBJECT(self->obj->color))) == NULLMAT) return NULL;
    }
  if ( self->obj->thickness == NULL )
    { H->obj->thickness = NULL;}
  else
    {
      if ((H->obj->thickness = (NspMatrix *) nsp_object_full_copy_and_name("thickness", NSP_OBJECT(self->obj->thickness))) == NULLMAT) return NULL;
    }
  return H;
}

NspSegments *nsp_segments_full_copy(NspSegments *self)
{
  NspSegments *H  =nsp_segments_create_void(NVOID,(NspTypeBase *) nsp_type_segments);
  if ( H ==  NULLSEGMENTS) return NULLSEGMENTS;
  if ( nsp_segments_full_copy_partial(H,self)== NULL) return NULLSEGMENTS;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspSegments
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_segments_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSegments *H;
  CheckStdRhs(0,0);
  /* want to be sure that type segments is initialized */
  nsp_type_segments = new_type_segments(T_BASE);
  if(( H = nsp_segments_create_void(NVOID,(NspTypeBase *) nsp_type_segments)) == NULLSEGMENTS) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_segments_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_segments_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *segments_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_segments_get_x(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspSegments *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_segments_get_obj_x(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSegments *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_segments_set_x(void *self,const char *attr, NspObject *O)
{
  NspMatrix *x;
  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSegments *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspSegments *) self)->obj->x);
  ((NspSegments *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_segments_get_y(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspSegments *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_segments_get_obj_y(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSegments *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_segments_set_y(void *self,const char *attr, NspObject *O)
{
  NspMatrix *y;
  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSegments *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspSegments *) self)->obj->y);
  ((NspSegments *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_segments_get_color(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspSegments *) self)->obj->color;
  return (NspObject *) ret;
}

static NspObject *_wrap_segments_get_obj_color(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSegments *) self)->obj->color);
  return (NspObject *) ret;
}

static int _wrap_segments_set_color(void *self,const char *attr, NspObject *O)
{
  NspMatrix *color;
  if ( ! IsMat(O) ) return FAIL;
  if ((color = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSegments *) self)->obj->color != NULL ) 
    nsp_matrix_destroy(((NspSegments *) self)->obj->color);
  ((NspSegments *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_segments_get_thickness(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspSegments *) self)->obj->thickness;
  return (NspObject *) ret;
}

static NspObject *_wrap_segments_get_obj_thickness(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSegments *) self)->obj->thickness);
  return (NspObject *) ret;
}

static int _wrap_segments_set_thickness(void *self,const char *attr, NspObject *O)
{
  NspMatrix *thickness;
  if ( ! IsMat(O) ) return FAIL;
  if ((thickness = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSegments *) self)->obj->thickness != NULL ) 
    nsp_matrix_destroy(((NspSegments *) self)->obj->thickness);
  ((NspSegments *) self)->obj->thickness= thickness;
  return OK;
}

static AttrTab segments_attrs[] = {
  { "x", (attr_get_function * )_wrap_segments_get_x, (attr_set_function * )_wrap_segments_set_x, (attr_get_object_function * )_wrap_segments_get_obj_x, (attr_set_object_function * )int_set_object_failed },
  { "y", (attr_get_function * )_wrap_segments_get_y, (attr_set_function * )_wrap_segments_set_y, (attr_get_object_function * )_wrap_segments_get_obj_y, (attr_set_object_function * )int_set_object_failed },
  { "color", (attr_get_function * )_wrap_segments_get_color, (attr_set_function * )_wrap_segments_set_color, (attr_get_object_function * )_wrap_segments_get_obj_color, (attr_set_object_function * )int_set_object_failed },
  { "thickness", (attr_get_function * )_wrap_segments_get_thickness, (attr_set_function * )_wrap_segments_set_thickness, (attr_get_object_function * )_wrap_segments_get_obj_thickness, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 56 "codegen/segments.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_segments(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 758 "segments.c"


#line 66 "codegen/segments.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_segments(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 771 "segments.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Segments_func[]={
  { "extractelts_segments", _wrap_nsp_extractelts_segments},
  { "setrowscols_segments", _wrap_nsp_setrowscols_segments},
  { "segments_create", int_segments_create},
  { NULL, NULL}
};

/* call ith function in the Segments interface */

int Segments_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Segments_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Segments_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Segments_func[i].name;
  *f = Segments_func[i].fonc;
}
void nsp_initialize_Segments_types(void)
{
  new_type_segments(T_BASE);
}

#line 77 "codegen/segments.override"

/* inserted verbatim at the end */

static void nsp_draw_segments(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  int c_width, c_color;
  int width, color;
  int *colors=NULL, *widths=NULL;
  int i;
  
  NspSegments *P = (NspSegments *) Obj;
  if ( Obj->obj->show == FALSE ) return;
  if ( P->obj->x->mn == 0 )  return;
  if ( ! nsp_graphic_intersect_rectangle(Obj, rect)) return;
  
  width = c_width = Xgc->graphic_engine->xget_thickness(Xgc);
  color = c_color = Xgc->graphic_engine->xget_color(Xgc);
  
  if ( P->obj->color != NULLMAT && P->obj->color->mn != 0 ) 
    {
      colors= ( P->obj->color->mn == 1) ? NULL : P->obj->color->I;
      color = ( P->obj->color->mn == 1) ? P->obj->color->I[0] : c_color ;
    }
  if ( P->obj->thickness != NULLMAT && P->obj->thickness->mn != 0 )
    {
      widths =  ( P->obj->thickness->mn == 1) ? NULL : P->obj->thickness->I;
      width  =  ( P->obj->thickness->mn == 1) ? P->obj->thickness->I[0] : c_width ;
    }
  for (i=0 ; i < P->obj->x->mn/2 ; i++)
    {
      int s_col = Min(i,P->obj->color->mn-1);
      int s_thick = Min(i,P->obj->thickness->mn-1);
      int segment_color= (colors != NULL) ? P->obj->color->I[s_col] : color;
      int segment_width= (widths != NULL) ? P->obj->thickness->I[s_thick]: width;
      double cx[]={ P->obj->x->R[2*i], P->obj->x->R[2*i+1]};
      double cy[]={ P->obj->y->R[2*i],P->obj->y->R[2*i+1]};
      double vx[2],vy[2];
      scale_double_to_pixels(Xgc->scales,cx,cy,vx,vy,2);
      Xgc->graphic_engine->xset_thickness(Xgc,segment_width);
      Xgc->graphic_engine->xset_line_style(Xgc,segment_color);
      Xgc->graphic_engine->drawline(Xgc,vx[0],vy[0],vx[1],vy[1]);
    }
  width = Xgc->graphic_engine->xget_thickness(Xgc);
  color = Xgc->graphic_engine->xget_color(Xgc);
  /* back to default */
  if ( c_width != width)  Xgc->graphic_engine->xset_thickness(Xgc, c_width);
  if ( c_color != color)  Xgc->graphic_engine->xset_color(Xgc, c_color);
}

static void nsp_translate_segments(NspGraphic *Obj,const double *tr)
{
  int i; 
  NspSegments *P = (NspSegments *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) += tr[0];
      *(y++) += tr[1];
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_segments(NspGraphic *Obj,double *R)
{
  int i;
  NspSegments *P = (NspSegments *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R,x1,y1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      x1 = R[0]*(*x) -R[1]*(*y);
      y1 = R[1]*(*x) +R[0]*(*y);
      *(x++) =x1;
      *(y++) =y1;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_segments(NspGraphic *Obj,double *alpha)
{
  int i;
  NspSegments *P = (NspSegments *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) *= alpha[0];
      *(y++) *= alpha[1];
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of segments 
 *
 */

static int nsp_getbounds_segments(NspGraphic *Obj,double *bounds)
{
  int i;
  NspSegments *P = (NspSegments *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R,dval;
  if ( P->obj->x->mn == 0) return FALSE;

  bounds[0]=*x;/* xmin */
  bounds[1]=*y;/* ymin */
  bounds[2]=*x;/* xmax */
  bounds[3]=*y;/* ymax */
  for (i = 1; i < P->obj->x->mn; i++)
    {
      dval = x[i];
      if ( dval > bounds[2] )
	bounds[2] = dval;
      else if ( dval < bounds[0] )
	bounds[0] = dval;
      dval = y[i];
      if ( dval > bounds[3] )
	bounds[3] = dval;
      else if ( dval < bounds[1] )
	bounds[1] = dval;
    }
  return TRUE;
}


#line 932 "segments.c"
