/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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





#line 119 "codegen/link.override"

#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/parse.h"


#line 37 "link.c"

/* ----------- NspLink ----------- */


#define  NspLink_Private 
#include <nsp/object.h>
#include <nsp/link.h>
#include <nsp/interf.h>

/* 
 * NspLink inherits from Graphic 
 * and implements  GRint
 */

int nsp_type_link_id=0;
NspTypeLink *nsp_type_link=NULL;

/*
 * Type object for NspLink 
 * all the instance of NspTypeLink share the same id. 
 * nsp_type_link: is an instance of NspTypeLink 
 *    used for objects of NspLink type (i.e built with new_link) 
 * other instances are used for derived classes 
 */
NspTypeLink *new_type_link(type_mode mode)
{
  NspTypeGRint *t_grint;
  NspTypeLink *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_link != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_link;
    }
  if (( type =  malloc(sizeof(NspTypeLink))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = link_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = link_get_methods;
  type->new = (new_func *) new_link;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for link */ 

  top->pr = (print_func *) nsp_link_print;
  top->dealloc = (dealloc_func *) nsp_link_destroy;
  top->copy  =  (copy_func *) nsp_link_copy;
  top->size  = (size_func *) nsp_link_size;
  top->s_type =  (s_type_func *) nsp_link_type_as_string;
  top->sh_type = (sh_type_func *) nsp_link_type_short_string;
  top->info = (info_func *) nsp_link_info;
  /* top->is_true = (is_true_func  *) nsp_link_is_true; */
  /* top->loop =(loop_func *) nsp_link_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_link_object;
  top->eq  = (eq_func *) nsp_link_eq;
  top->neq  = (eq_func *) nsp_link_neq;
  top->save  = (save_func *) nsp_link_xdr_save;
  top->load  = (load_func *) nsp_link_xdr_load;
  top->create = (create_func*) int_link_create;
  top->latex = (print_func *) nsp_link_latex;

  /* specific methods for link */

  type->init = (init_func *) init_link;

#line 134 "codegen/link.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_link;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_link ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_link  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_link  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_link  ;
  /* next method are defined in NspGraphic and need not be changed here for Link */
  /* ((NspTypeNspGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeNspGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 121 "link.c"
  /* 
   * NspLink interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_grint = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) t_grint;
#line 146 "codegen/link.override"

  t_grint->get_hilited 	=(gr_get_hilited *) link_get_hilited;
  t_grint->set_hilited 	=(gr_set_hilited *) link_set_hilited;
  t_grint->get_show    	=(gr_get_show *) link_get_show;
  t_grint->set_show		=(gr_set_show *) link_set_show;
  t_grint->draw    		=(gr_draw *) link_draw;
  t_grint->translate 	=(gr_translate *) link_translate;
  t_grint->set_pos  	=(gr_set_pos *) link_set_pos;
  t_grint->get_pos  	=(gr_get_pos *) link_get_pos;
  t_grint->get_rect  	=(gr_get_rect *) link_get_rect;
  t_grint->resize 		=(gr_resize *) link_resize;
  t_grint->update_locks 	=(gr_update_locks *) link_update_locks;
  t_grint->contains_pt 	=(gr_contains_pt *) link_contains_pt;
  t_grint->control_near_pt 	=(gr_control_near_pt *) link_control_near_pt;
  t_grint->lock_near_pt 	=(gr_lock_near_pt *) link_lock_near_pt;
  t_grint->move_control_init=(gr_move_control_init *) link_move_control_init;
  t_grint->move_control 	=(gr_move_control *) link_move_control;

  t_grint->get_number_of_locks =(gr_get_number_of_locks *) link_get_number_of_locks;
  t_grint->get_number_of_ports =(gr_get_number_of_ports *) link_get_number_of_ports;
  t_grint->get_lock_connection =(gr_get_lock_connection *) link_get_lock_connection;
  t_grint->get_lock_pos =(gr_get_lock_pos *) link_get_lock_pos;
  t_grint->get_lock_dir =(gr_get_lock_dir *) link_get_lock_dir;
  t_grint->set_lock_connection =(gr_set_lock_connection *) link_set_lock_connection;
  t_grint->unset_lock_connection =(gr_unset_lock_connection *) link_unset_lock_connection;
  t_grint->is_lock_connectable =(gr_is_lock_connectable *) link_is_lock_connectable;
  t_grint->is_lock_connected =(gr_is_lock_connected *) link_is_lock_connected;
  t_grint->set_lock_pos =(gr_set_lock_pos *) link_set_lock_pos;
  t_grint->full_copy =(gr_full_copy *) link_full_copy;
  t_grint->unlock =(gr_unlock *) link_unlock;
  t_grint->set_frame =(gr_set_frame *) link_set_frame;

#line 163 "link.c"
  if ( nsp_type_link_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeLink called nsp_type_link
       */
      type->id =  nsp_type_link_id = nsp_new_type_id();
      nsp_type_link = type;
      if ( nsp_register_type(nsp_type_link) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_link(mode);
    }
  else 
    {
      type->id = nsp_type_link_id;
      return type;
    }
}

/*
 * initialize NspLink instances 
 * locally and by calling initializer on parent class 
 */

static int init_link(NspLink *Obj,NspTypeLink *type)
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
 * new instance of NspLink 
 */

NspLink *new_link() 
{
  NspLink *loc;
  /* type must exists */
  nsp_type_link = new_type_link(T_BASE);
  if ( (loc = malloc(sizeof(NspLink)))== NULLLINK) return loc;
  /* initialize object */
  if ( init_link(loc,nsp_type_link) == FAIL) return NULLLINK;
  return loc;
}


/*----------------------------------------------
 * Object method redefined for NspLink 
 *-----------------------------------------------*/
/*----------------------------------------------
 * Object method redefined for NspLink 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_link_size(NspLink *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char link_type_name[]="Link";
static char link_short_type_name[]="link";

static char *nsp_link_type_as_string(void)
{
  return(link_type_name);
}

static char *nsp_link_type_short_string(NspObject *v)
{
  return(link_short_type_name);
}

/*
 * A == B 
 */

static int nsp_link_eq(NspLink *A, NspObject *B)
{
  NspLink *loc = (NspLink *) B;
  if ( check_cast(B,nsp_type_link_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->frame != loc->obj->frame) return FALSE;
  if ( A->obj->object_sid != loc->obj->object_sid) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( A->obj->background != loc->obj->background) return FALSE;
  if ( NSP_OBJECT(A->obj->poly)->type->eq(A->obj->poly,loc->obj->poly) == FALSE ) return FALSE;
  if ( nsp_eq_grl_lock(&A->obj->lock1,&loc->obj->lock1)== FALSE) return FALSE;
  if ( nsp_eq_grl_lock(&A->obj->lock2,&loc->obj->lock2)== FALSE) return FALSE;
  if ( A->obj->hilited != loc->obj->hilited) return FALSE;
  if ( A->obj->show != loc->obj->show) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_link_neq(NspLink *A, NspObject *B)
{
  return ( nsp_link_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_link_xdr_save(XDR *xdrs, NspLink *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_link)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(M)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->background) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->poly)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->hilited) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->show) == FAIL) return FAIL;
  if ( nsp_save_grl_lock(xdrs,&M->obj->lock1,M) == FAIL ) return FAIL;
  if ( nsp_save_grl_lock(xdrs,&M->obj->lock2,M) == FAIL ) return FAIL;
  if (nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspLink  *nsp_link_xdr_load_partial(XDR *xdrs, NspLink *M)
{
  int id,fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_link))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  NULLLINK;
  M->obj->object_sid = NSP_INT_TO_POINTER(id);
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->background) == FAIL) return NULL;
  if ((M->obj->poly =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->hilited) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->show) == FAIL) return NULL;
  if ( nsp_load_grl_lock(xdrs,&M->obj->lock1,M) == FAIL ) return NULL;
  if ( nsp_load_grl_lock(xdrs,&M->obj->lock2,M) == FAIL ) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspLink  *nsp_link_xdr_load(XDR *xdrs)
{
  NspLink *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLLINK;
  if ((H  = nsp_link_create_void(name,(NspTypeBase *) nsp_type_link))== NULLLINK) return H;
  if ((H  = nsp_link_xdr_load_partial(xdrs,H))== NULLLINK) return H;
  if ( nsp_link_check_values(H) == FAIL) return NULLLINK;
  return H;
}



/*
 * delete 
 */

void nsp_link_destroy_partial(NspLink *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     nsp_matrix_destroy(H->obj->poly);
     nsp_destroy_grl_lock(&H->obj->lock1,H); 
     nsp_destroy_grl_lock(&H->obj->lock2,H); 
     FREE(H->obj);
   }
}

void nsp_link_destroy(NspLink *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_link_destroy_partial(H);
  FREE(H);
}


/*
 * info 
 */

int nsp_link_info(NspLink *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLLINK) 
    {
      Sciprintf("Null Pointer NspLink \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_link_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_link_print(NspLink *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLLINK) 
    {
      Sciprintf("Null Pointer NspLink \n");
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
          nsp_link_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_link_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"frame=%xl\n",M->obj->frame);
  Sciprintf1(indent+2,"object_sid=%xl\n",M->obj->object_sid);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"background=%d\n",M->obj->background);
  if ( M->obj->poly != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->poly),indent+2,"poly",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_print_grl_lock(indent+2,&M->obj->lock1,M);
  nsp_print_grl_lock(indent+2,&M->obj->lock2,M);
  Sciprintf1(indent+2,"hilited	= %s\n", ( M->obj->hilited == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"show	= %s\n", ( M->obj->show == TRUE) ? "T" : "F" );
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_link_latex(NspLink *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_link_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"frame=%xl\n",M->obj->frame);
  Sciprintf1(indent+2,"object_sid=%xl\n",M->obj->object_sid);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"background=%d\n",M->obj->background);
  if ( M->obj->poly != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->poly),indent+2,"poly",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_print_grl_lock(indent+2,&M->obj->lock1,M);
  nsp_print_grl_lock(indent+2,&M->obj->lock2,M);
  Sciprintf1(indent+2,"hilited	= %s\n", ( M->obj->hilited == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"show	= %s\n", ( M->obj->show == TRUE) ? "T" : "F" );
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}


/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspLink objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspLink   *nsp_link_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_link_id) == TRUE ) return ((NspLink *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_link));
  return NULL;
}

int IsLinkObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_link_id);
}

int IsLink(NspObject *O)
{
  return nsp_object_type(O , nsp_type_link_id);
}

NspLink  *GetLinkCopy(Stack stack, int i)
{
  if (  GetLink(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspLink  *GetLink(Stack stack, int i)
{
  NspLink *M;
  if (( M = nsp_link_object(NthObj(i))) == NULLLINK)
     ArgMessage(stack,i);
  return M;
}


/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspLink instance 
 *-----------------------------------------------------*/
/* override the code for link creation */

static NspLink *nsp_link_create_void(char *name,NspTypeBase *type)
{
 NspLink *H  = (type == NULL) ? new_link() : type->new();
 if ( H ==  NULLLINK)
  {
   Sciprintf("No more memory\n");
   return NULLLINK;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLLINK;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_link_create_partial(NspLink *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_link)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->frame = NULL;
  H->obj->object_sid = NULL;
  H->obj->color = 0;
  H->obj->thickness = 0;
  H->obj->background = 0;
  H->obj->poly = NULLMAT;
  nsp_init_grl_lock(&H->obj->lock1);
  nsp_init_grl_lock(&H->obj->lock2);
  H->obj->hilited = FALSE;
  H->obj->show = TRUE;
  return OK;
}

int nsp_link_check_values(NspLink *H)
{
  if ( H->obj->poly == NULLMAT) 
    {
      if (( H->obj->poly = nsp_matrix_create("poly",'r',0,0)) == NULLMAT)
	return FAIL;
    }
  if ( nsp_check_grl_lock(&H->obj->lock1,H) == FAIL ) return FAIL;
  if ( nsp_check_grl_lock(&H->obj->lock2,H) == FAIL ) return FAIL;
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspLink *nsp_link_create(char *name,nspgframe* frame,void* object_sid,int color,int thickness,int background,NspMatrix* poly,grl_lock lock1,grl_lock lock2,gboolean hilited,gboolean show,NspTypeBase *type)
{
 NspLink *H  = nsp_link_create_void(name,type);
 if ( H ==  NULLLINK) return NULLLINK;
  if ( nsp_link_create_partial(H) == FAIL) return NULLLINK;
  H->obj->frame = frame;
  H->obj->object_sid = object_sid;
  H->obj->color=color;
  H->obj->thickness=thickness;
  H->obj->background=background;
  if ( poly != NULL) 
    {
      if (( H->obj->poly = nsp_matrix_copy(poly))== NULLMAT) return NULLLINK;
      if (nsp_object_set_name(NSP_OBJECT(H->obj->poly),"lpt") == FAIL) return NULLLINK;
    }
  H->obj->lock1 = lock1;
  H->obj->lock2 = lock2;
  H->obj->lock1.port.object_id = NULL;
  H->obj->lock2.port.object_id = NULL;
  H->obj->hilited=hilited;
  H->obj->show=show;
  if ( nsp_link_check_values(H) == FAIL) return NULLLINK;
  return H;
}

NspLink *nsp_link_create_default(char *name)
{
 NspLink *H  = nsp_link_create_void(name,NULL);
 if ( H ==  NULLLINK) return NULLLINK;
  if ( nsp_link_create_partial(H) == FAIL) return NULLLINK;
 if ( nsp_link_check_values(H) == FAIL) return NULLLINK;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspLink *nsp_link_copy_partial(NspLink *H,NspLink *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspLink *nsp_link_copy(NspLink *self)
{
  NspLink *H  =nsp_link_create_void(NVOID,(NspTypeBase *) nsp_type_link);
  if ( H ==  NULLLINK) return NULLLINK;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLLINK;
  if ( nsp_link_copy_partial(H,self)== NULL) return NULLLINK;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspLink *nsp_link_full_copy_partial(NspLink *H,NspLink *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_link))) == NULL) return NULLLINK;
  H->obj->ref_count=1;
  H->obj->frame = self->obj->frame;
  H->obj->object_sid = self->obj->object_sid;
  H->obj->color=self->obj->color;
  H->obj->thickness=self->obj->thickness;
  H->obj->background=self->obj->background;
  if ( self->obj->poly == NULL )
    { H->obj->poly = NULL;}
  else
    {
      if ((H->obj->poly = (NspMatrix *) nsp_object_copy_and_name("poly",NSP_OBJECT(self->obj->poly))) == NULLMAT) return NULL;
    }
  H->obj->lock1 = self->obj->lock1;
  H->obj->lock2 = self->obj->lock2;
  H->obj->hilited=self->obj->hilited;
  H->obj->show=self->obj->show;
  return H;
}

NspLink *nsp_link_full_copy(NspLink *self)
{
  NspLink *H  =nsp_link_create_void(NVOID,(NspTypeBase *) nsp_type_link);
  if ( H ==  NULLLINK) return NULLLINK;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLLINK;
  if ( nsp_link_full_copy_partial(H,self)== NULL) return NULLLINK;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspLink
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_link_create(Stack stack, int rhs, int opt, int lhs)
{
  NspLink *H;
  NspMatrix *M1;
  int color=-1,thickness=-1;
  grl_lock l;
  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
  CheckCols(NspFname(stack),1,M1,2);
  if ( get_optional_args(stack,rhs,opt,opts,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = nsp_link_create(NVOID,NULL,NULL,color,thickness,0,M1,l,l,FALSE,TRUE,NULL)) 
    == NULLLINK) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 


/*------------------------------------------------------
 * Methods
 *------------------------------------------------------*/

/* draw */

static int _wrap_link_draw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  link_draw(self);
  return 0;
}

/* translate */

static int _wrap_link_translate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  link_translate(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}
/* translate */

static int _wrap_link_set_pos(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  link_set_pos(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/* resize */ 

static int _wrap_link_resize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),2,M,2);
  link_resize(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}


/*
 * connect 
 */

static int link_connect(NspLink *L,int lock, NspObject *Obj,int obj_lock,int obj_port);

static int _wrap_link_connect(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  int lock,lock_dest;
  NspBlock *L;
  CheckRhs(3,4);
  CheckLhs(-1,1);
  if ( GetScalarInt(stack,1,&lock) == FAIL) return RET_BUG;
  if ( (L=GetBlock(stack,2))== NULLBLOCK) return RET_BUG;
  if ( GetScalarInt(stack,3,&lock_dest) == FAIL) return RET_BUG;
  if ( link_connect(self,lock,NSP_OBJECT(L),lock_dest,0) == FAIL) return RET_BUG;
  MoveObj(stack,1,self);
  return 1;
}


static NspMethods link_methods[] = {
  { "translate", _wrap_link_translate},
  { "set_pos", _wrap_link_set_pos},
  { "resize",   _wrap_link_resize},
  { "draw",   _wrap_link_draw},
  { "connect", _wrap_link_connect},
  { (char *) 0, NULL}
};
                                                                                                      
static NspMethods *link_get_methods(void) { return link_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_link_get_color(void *self,const char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspLink *) self)->obj->color);
}

static int _wrap_link_set_color(void *self,const  char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspLink *)self)->obj->color = color;
  return OK ;
}

static NspObject * _wrap_link_get_thickness(void *self,const char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspLink *) self)->obj->thickness);
}
                                                                                                      
static int _wrap_link_set_thickness(void *self,const  char *attr, NspObject *O)
{
  int thickness;
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspLink *)self)->obj->thickness = thickness;
  return OK ;
}

static NspObject * _wrap_link_get_hilited(void *self,const char *attr)
{
  return nsp_new_boolean_obj(((NspLink *) self)->obj->hilited);
}
                                                                                                      
static int _wrap_link_set_hilited(void *self,const  char *attr, NspObject *O)
{
  int hilited;
  if (  BoolScalar(O,&hilited) == FAIL) return FAIL;
  ((NspLink *)self)->obj->hilited = hilited;
  return OK ;
}

static NspObject * _wrap_link_get_show(void *self,const char *attr)
{
  return nsp_new_boolean_obj(((NspLink *) self)->obj->show);
}
                                                                                                      
static int _wrap_link_set_show(void *self,const  char *attr, NspObject *O)
{
  int show;
  if (  BoolScalar(O,&show) == FAIL) return FAIL;
  ((NspLink *)self)->obj->show = show;
  return OK ;
}

static AttrTab link_attrs[] = {
  { "color",        _wrap_link_get_color ,     _wrap_link_set_color ,     NULL, NULL },
  { "thickness",    _wrap_link_get_thickness,  _wrap_link_set_thickness,  NULL, NULL },
  { "hilited",   _wrap_link_get_hilited, _wrap_link_set_hilited, NULL, NULL  },
  { "show",   _wrap_link_get_show, _wrap_link_set_show, NULL, NULL  },
  { (char *) 0, NULL, NULL, NULL  }
};


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Link_func[]={
  {"setrowscols_gl",int_set_attributes}, 
  {(char *) 0, NULL}
};

/* call ith function in the Link interface */

int Link_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Link_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Link_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Link_func[i].name;
  *f = Link_func[i].fonc;
}


static void nsp_draw_link(BCG *Xgc,NspGraphic *Obj, void *data)
{
  
}

static void nsp_translate_link(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  /* NspLink *P = (NspLink *) Obj;*/
}

static void nsp_rotate_link(BCG *Xgc,NspGraphic *Obj,double *R)
{
  
}

static void nsp_scale_link(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  /*   NspLink *P = (NspLink *) Obj; */
}

/* compute in bounds the enclosing rectangle of link 
 *
 */

static int nsp_getbounds_link (BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspLink *B = (NspLink *) Obj;
  link_get_rect(B,bounds);
  return TRUE;
}


static void dist_2_polyline(const NspMatrix *poly,const double pt[2],
			    double pt_proj[2],int *kmin,double *pmin,double *d);

/*********************************************************************
 * Create a graphic link
 *********************************************************************/

NspLink *link_create_n(char *name,int n,int color,int thickness)
{
  NspLink *H  = nsp_link_create_void(name,NULL);
  if ( H ==  NULLLINK) return NULLLINK;
  if ((H->obj = malloc(sizeof(nsp_link))) == NULL) return NULL;
  H->obj->ref_count=1;
  H->obj->frame = NULL; 
  /* fields */
  if (( H->obj->poly =nsp_mat_zeros(n,2))== NULLMAT) return NULLLINK;
  /* need a name here to be able to save object */
  if (nsp_object_set_name(NSP_OBJECT(H->obj->poly),"lpt") == FAIL) return NULLLINK;
  H->obj->color = color;
  H->obj->thickness = thickness;
  H->obj->hilited = FALSE ; 
  H->obj->show = TRUE   ; 
  H->obj->lock1.port.object_id = NULL;
  H->obj->lock2.port.object_id = NULL;
  return H;
}

/*---------------------------------------------------------
 * GRint interface implementation 
 *---------------------------------------------------------*/

/**************************************************
 * change or get attributes 
 **************************************************/

int link_get_hilited(NspLink *B) {  return B->obj->hilited; } 
void link_set_hilited(NspLink *B,int val) {  B->obj->hilited = val; } 
int link_get_show(NspLink *B) {  return B->obj->show; } 
void link_set_show(NspLink *B,int val) {  B->obj->show = val; } 

/**************************************************
 * Draw 
 **************************************************/

/* static double lock_size=1;
   static int lock_color=10;
*/
static int link_unconnected_color=10;

void link_draw(NspLink *L)
{
  BCG *Xgc;
  double loc[4];
  int cpat, cwidth;
  /* only draw block which are in a frame */
  if ( L->obj->frame == NULL) return;
  if ( L->obj->show == FALSE ) return ;

  Xgc=L->obj->frame->Xgc;

  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
  /* draw polyline */
  if ( link_is_lock_connected(L,0)== TRUE && 
       link_is_lock_connected(L,1)== TRUE ) 
    Xgc->graphic_engine->xset_pattern(Xgc,L->obj->color);
  else 
    Xgc->graphic_engine->xset_pattern(Xgc,link_unconnected_color);
  Xgc->graphic_engine->scale->drawpolyline(Xgc,L->obj->poly->R, L->obj->poly->R + L->obj->poly->m,
					   L->obj->poly->m,0);
  /* add hilited */ 
  Xgc->graphic_engine->xset_pattern(Xgc,lock_color);
  if ( L->obj->hilited == TRUE ) 
    {
      int i,m= L->obj->poly->m;
      double *x= L->obj->poly->R, *y = x + m; 
      /* link points except first and last */
      for ( i=1 ; i < m -1; i++) 
	{
	  loc[0]=x[i]-lock_size/2; loc[1]=y[i]+lock_size/2;loc[2]=loc[3]= lock_size;
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
	}
      /* firts and last link points which are lock points */
      for ( i=0 ; i <= 1; i++) 
	{
	  if ( i== 1 && m == 1) break; /* just one point in the link */
	  if ( link_is_lock_connected(L,i)== TRUE)
	    Xgc->graphic_engine->xset_pattern(Xgc,lock_color); 
	  else 
	    Xgc->graphic_engine->xset_pattern(Xgc,1); 
	  link_get_lock_pos(L,i,loc);
	  loc[0] += -lock_size/2; loc[1] += lock_size/2;loc[2]=loc[3]= lock_size;
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
	}
    }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}

/**************************************************
 * translate 
 **************************************************/

/* XXXXX */
extern int nsp_message_(char *message,char **buttons,int n_buttons);

int link_translate(NspLink *L,const double pt[2])
{
  int i,m= L->obj->poly->m,min=0,max=m;
  double *x= L->obj->poly->R, *y = x + m; 
  /* cannot translate locked link */
  if ( link_is_lock_connected(L,0) ) min=1;
  if ( link_is_lock_connected(L,1) ) max=m-1;
  for ( i= min ; i < max ; i++) 
    {
      x[i] += pt[0] ;
      y[i] += pt[1] ;
    }
  return OK;
}

int link_set_pos(NspLink *L,const double pt[2])
{
  double tr[2];
  int m= L->obj->poly->m;
  double *x= L->obj->poly->R, *y = x + m; 
  tr[0]=pt[0]-x[m/2];
  tr[1]=pt[1]-y[m/2];
  return link_translate(L,tr);
}

void link_get_pos(NspLink *L, double pt[2])
{
  int m= L->obj->poly->m;
  double *x= L->obj->poly->R, *y = x + m; 
  pt[0]= x[m/2];
  pt[1]=y[m/2];
}

void link_get_rect(NspLink *L, double r[4])
{
  double dr[2];
  int m= L->obj->poly->m, i;
  double *x= L->obj->poly->R, *y = x + m; 
  if ( m == 0) 
    {
      for ( i=0; i < 4 ; i++) r[i]=0;
      return;
    }
  r[0]=dr[0]=x[0];
  r[1]=dr[1]=y[0];
  for ( i = 0 ; i < m ; i++) 
    {
      if ( x[i] < r[0] ) r[0]= x[i];
      else if ( x[i] > dr[0]) dr[0]= x[i];
      if ( y[i] < dr[1] ) dr[1]= y[i];
      else if ( y[i] > r[1]) r[1]= y[i];
    }
  r[2]= dr[0]-r[0]; /* width */
  r[3]= r[1]- dr[1]; /* height */
}


/**************************************************
 * resize 
 **************************************************/

void link_resize(NspLink *R,const double size[2])
{
  
}

/**************************************************
 * Compute locks points and update R
 **************************************************/

void link_update_locks(NspLink *R)
{
  
}

/**************************************************
 * pt is inside NspLink 
 **************************************************/

int link_contains_pt(const NspLink *B,const double pt[2])
{
  double pt_proj[2],pmin,d;
  int kmin=-1;
  dist_2_polyline(B->obj->poly,pt,pt_proj,&kmin,&pmin,&d);
  if ( kmin != -1 && d <  lock_size/2 ) 
    return TRUE;
  else 
    return FALSE;
}
    
/**************************************************
 * utility function 
 * distance from a point to a polyline 
 * the point is on the segment [kmin,kmin+1] (note that 
 * kmin is < size(xp,'*'))
 * and its projection is at point 
 * pt = [ xp(kmin)+ pmin*(xp(kmin+1)-xp(kmin)) ;
 *        yp(kmin)+ pmin*(yp(kmin+1)-yp(kmin)) 
 * the distance is dmin 
 **************************************************/

static void dist_2_polyline(const NspMatrix *poly,const double pt[2],
			    double pt_proj[2],int *kmin,double *pmin,
			    double *dmin)
{
  double ux,uy,wx,wy,un,gx,gy;
  int n= poly->m;
  double *xp= poly->R;
  double *yp= poly->R + n;
  double p,d;
  int i;
  *dmin = 1.0+10; /* XXXXX max_double */
  for ( i = 0 ; i < n-1 ; i++) 
    {
      ux = xp[i+1]-xp[i];
      uy = yp[i+1]-yp[i];
      wx = pt[0] - xp[i];
      wy = pt[1] - yp[i];
      un= Max(ux*ux + uy*uy,1.e-10); /* XXXX */
      p = Max(Min((ux*wx+ uy*wy)/un,1),0);
      /* the projection of pt on each segment */
      gx= wx -  p * ux;
      gy= wy -  p * uy;
      d = Max(Abs(gx),Abs(gy));
      if ( d < *dmin ) 
	{
	  *dmin = d;
	  *pmin = p;
	  *kmin = i;
	  pt_proj[0]= xp[i]+ p*ux;
	  pt_proj[1]= yp[i]+ p*uy;
	}
    }
}

/**************************************************
 * pt is inside a control point 
 **************************************************/

int link_control_near_pt(const NspLink *B,const double pt[2], int *cp)
{
  int n= B->obj->poly->m;
  double *xp= B->obj->poly->R;
  double *yp= B->obj->poly->R + n;
  double d; 
  int i;
  for ( i = 0 ; i < n ; i++) 
    {
      d = Max(Abs( xp[i] -pt[0]),Abs( yp[i] -pt[1])) ;
      if ( d < lock_size ) 
	{ 
	  *cp = i ;
	  return TRUE;
	}
    }
  return FALSE;
}

/**************************************************
 * pt is inside a lock point 
 **************************************************/

int link_lock_near_pt(const NspLink *B,const double pt[2], int *cp)
{
  return FALSE;
}

/**************************************************
 * used when moving a control point 
 **************************************************/

void link_move_control_init( NspLink *B,int cp,double pt[2])
{
  pt[0]= B->obj->poly->R[cp];
  pt[1]= B->obj->poly->R[cp + B->obj->poly->m];
}

/*
 * link_get_number: unused 
 */

int link_get_number(NspGFrame *F, NspLink *B) 
{
  int count = 1;
  Cell *C = F->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O == (NspObject  *) B) return count;
      C = C->next ;
      count++;
    }
  return 0;
}

/* unlock, lock point lp = 0 or 1 */

static void link_unlock( NspLink *L,int lp) 
{
  NspObject *O1;
  gr_port p; 
  /* just test if unlock is necessary */
  if ( link_is_lock_connected(L,lp)==FALSE ) return; 
  if ( link_get_lock_connection(L,lp,0,&p)==FAIL) return;
  /* we are locked to a block, unlock it */
  O1 = p.object_id;
  if ( O1 != NULLOBJ ) 
    {
      /* propagate unlock to the locked object */
      GR_INT(O1->basetype->interface)->unset_lock_connection(O1,p.lock,p.port);
    }
  /* unset the lock on the link */
  link_unset_lock_connection(L,lp,0);
}


/**
 * link_set_frame:
 * @Gf: a #NspGFrame 
 * 
 * attach the link frame reference to @GF
 **/

static void link_set_frame( NspLink *B, NspGFrame *Gf)
{
  B->obj->frame = Gf->obj;
}


/**
 * link_lock: 
 * @F: a graphic frame  
 * @L: a link 
 * @lp: lock point of the link ie 0 or 1 
 * @p: an object to be connected to lp 
 * 
 * This function first unlock the lock point of Link @L. 
 * Then the lock point of the link is connected to the object/lock point 
 * stored in @p. @p is thus updated to contain a valid port number. 
 * Then @lp lock point of @L is locked to @p.
 * When calling this function, we assume that the object/lock point 
 * described by @p has an available port for connection. 
 * 
 **/

static void link_lock(NspGFrame *F, NspLink *L,int lp,gr_port *p)
{
  int port;
  NspObject *O1;
  gr_port p1 = {(NspObject *) L,NULL,lp,0};
  link_unlock( L,lp); /* unlock lp */
  /* we first update the Object we want to lock */
  O1 = p->object_id;
  port=GR_INT(O1->basetype->interface)->set_lock_connection(O1,p->lock,-1,&p1);
  /* should not get through port == -1 */
  if ( port == -1 ) Scierror("Unable to connect link to object \n");
  p->port = port; 
  /* Now we lock the link loc to p */
  link_set_lock_connection(L,lp,0,p);
}

/**
 * link_lock_update: 
 * @F: a graphic frame  
 * @L: a link 
 * @lp: lock point of the link ie 0 or 1 
 * @ptnew: point coordinates.
 * 
 * If @ptnew is near an object lock point and the lock point has an available port, 
 * then the link lock point @lp is unlocked and locked to this new lock point. 
 * @ptnew is also changed. 
 * 
 **/

void link_lock_update(NspGFrame *F, NspLink *L,int lp,double ptnew[2]) 
{
  int lock_c;
  /* double pt[]={ptnew[0],ptnew[1]};*/
  if ( F != NULLGFRAME) 
    {
      NspObject *Ob;
      int cp1;
      int rep = nsp_gframe_select_lock(F,ptnew, &Ob, &cp1,&lock_c) ;
      if ( rep != 0 ) 
	{
	  if ( lock_c == TRUE)
	    {
	      gr_port p ={ Ob,NULL,cp1,0};/* link_lock will find the available port */
	      link_lock(F,L,lp,&p);
	    }
	}
      else 
	{
	  link_unlock(L,lp);
	}
    }
}

/**
 * link_move_control:
 * @F: 
 * @L: 
 * @mpt: mouse position  
 * @cp: control point id
 * @ptc: the control point position 
 * 
 * moves the control point @cp of a link toward the mouse position @mpt.
 * @mpt is the mouse position and @ptc the control point position 
 * which is to be updated by the routine.
 * 
 **/

void link_move_control(NspGFrame *F, NspLink *L,const double mpt[2], int cp,double ptc[2])
{
  double ptb[2],ptn[2];
  /* move a control point : here the point cp of the polyline */
  int n = L->obj->poly->m;
  double *xp= L->obj->poly->R;
  double *yp= L->obj->poly->R + n;
  /* 
   * ptc is where we should move ptc but due to magnetism 
   * it can be different.
   */
  ptc[0]  = mpt[0];
  ptc[1]  = mpt[1];

  if ( cp >= 1 && cp < n -1 ) 
    {
      int hvfactor=2*lock_size;
      /*  magnetism toward horizontal or vertival lines */
      ptb[0] = xp[cp-1]; ptb[1] = yp[cp-1];
      ptn[0] = xp[cp+1]; ptn[1] = yp[cp+1];
      if ( Abs( ptb[0] - ptc[0]) < hvfactor ) ptc[0]= ptb[0];
      if ( Abs( ptn[0] - ptc[0]) < hvfactor ) ptc[0]= ptn[0];
      if ( Abs( ptb[1] - ptc[1]) < hvfactor ) ptc[1]= ptb[1];
      if ( Abs( ptn[1] - ptc[1]) < hvfactor ) ptc[1]= ptn[1];
    }
  else if ( cp == 0 || cp == n-1 )
    {
      /* try to check if we are in the vivinity of 
       * a lock point lock of a Block 
       * if true this will change ptc due to block magnetism.
       */
      int lp= (cp == 0) ? 0 : 1 ;
      link_lock_update(F,L,lp,ptc);
      if ( L->obj->poly->m >= 3 && link_is_lock_connected(L,lp)== TRUE)
	{
	  /* magnetize previous point toward horiwontal or vertical line */
	  int next = ( cp == 0) ? 1 : n-2;
	  int hvfactor=2*lock_size;
	  double *x = L->obj->poly->R, *y = L->obj->poly->R + L->obj->poly->m;
	  /*  magnetism toward horizontal or vertival lines */
	  if ( Abs( x[cp] - x[next] ) < hvfactor ) x[next] = x[cp];
	  if ( Abs( y[cp] - y[next] ) < hvfactor ) y[next] = y[cp];
	}
    }
  xp[cp]=ptc[0];
  yp[cp]=ptc[1];
  
}

/**
 * link_split:
 * @F: a #NspGFrame 
 * @L: a link 
 * @L1: a pointer to a link 
 * @pt: a point.
 * 
 * splits the graphic link @L, the new link is returned in @L1 and 
 * inserted in the graphic frame @F. The split point is the polyline 
 * point nearest to @pt. 
 * 
 * Return value: %OK or %FAIL
 **/

int link_split(NspGFrame *F,NspLink *L,NspLink **L1,const double pt[2])
{
  int kmin,i,n,n1;
  gr_port p;
  double proj[2], pmin,dmin;
  dist_2_polyline(L->obj->poly,pt,proj,&kmin,&pmin,&dmin);
  n = L->obj->poly->m;
  /* for proj to end of link */
  n1 = L->obj->poly->m-kmin;
  if ((*L1= link_create_n("fe",n1,L->obj->color,L->obj->thickness))==NULL) return FAIL;
  (*L1)->obj->frame = F->obj;
  (*L1)->obj->poly->R[0]=proj[0];
  (*L1)->obj->poly->R[n1]=proj[1];
  for ( i=1; i < n1 ; i++) 
    {
      (*L1)->obj->poly->R[i]= L->obj->poly->R[i+kmin];
      (*L1)->obj->poly->R[i+n1]=L->obj->poly->R[i+kmin+n];
    }
  /* change L */ 
  L->obj->poly->R[kmin+1]=proj[0];
  L->obj->poly->R[kmin+1+n]= proj[1];
  for ( i=0; i < kmin +2 ; i++) 
    {
      L->obj->poly->R[i+ kmin+2]= L->obj->poly->R[i+n];
    }
  if (  nsp_matrix_resize(L->obj->poly,kmin+2,2)== FAIL) return FAIL;
  /* now change links */ 
  if ( link_is_lock_connected(L,1)== TRUE)
    {
      link_get_lock_connection(L,1,0,&p);
      link_unlock(L,1);  
      link_lock(F,(*L1),1,&p); 
    }
  /* add L1 in the frame at start */ 
  if (nsp_list_insert(F->obj->objs,(NspObject  *) (*L1),0) == FAIL) return FAIL;
  return OK;
}



/** 
 * link_add_control_point: 
 * 
 * XXXX 
 * 
 */

int link_add_control(NspLink *L,const double pt[2])
{
  int kmin,i,n;
  double proj[2], pmin,dmin;
  n = L->obj->poly->m;
  /* the point is to be inserted after kmin */
  dist_2_polyline(L->obj->poly,pt,proj,&kmin,&pmin,&dmin);
  if (  nsp_matrix_resize(L->obj->poly,n+1,L->obj->poly->n)== FAIL) return FAIL;
  /* insert point in resized matrix */
  for ( i= L->obj->poly->mn -1 ; i > L->obj->poly->m+kmin+1 ; i--) 
    L->obj->poly->R[i]= L->obj->poly->R[i-2];
  L->obj->poly->R[L->obj->poly->m+kmin+1]=pt[1];
  for ( i= L->obj->poly->m+kmin ; i> kmin+1 ; i--) 
    L->obj->poly->R[i]= L->obj->poly->R[i-1];
  L->obj->poly->R[kmin+1]=pt[0];
  return OK;
}

/** 
 * link_remove_control_point: 
 * 
 * XXXX 
 * 
 */

int link_remove_control(NspLink *L,const double pt[2])
{
  int cp;
  index_vector index = {&cp, cp+1,cp+1,1, FALSE,0};
  if ( link_control_near_pt(L,pt,&cp) == FALSE ) return OK;
  if ( cp == 0 || cp == L->obj->poly->m -1 ) return OK;
  /* remove point in matrix */
  return nsp_matint_delete_rows( (NspObject *) (L->obj->poly), &index);
}


/**
 * link_check:
 * @F: a graphic frame 
 * @L: a graphic link 
 * 
 * Checks if the lock points of @L are not over an other object 
 * of the graphic frame @F. 
 * and lock is not set. If true the lock points are moved. 
 * 
 **/

void link_check(NspGFrame *F,NspLink *L)
{
  NspObject *obj;
  double pt[2];
  int i,cp,lock_c;
  /* checks if the lock points are not over an other object lock point 
   * and lock is not set 
   */
  for ( i=0; i < 2 ; i++) 
    {
      link_get_lock_pos(L,i,pt);
      /* checks if pt is a lock point of */
      if ( nsp_gframe_select_lock(F,pt,&obj,&cp,&lock_c) != 0) 
	{
	  /* pt is near a lock point */ 
	  if ( link_is_lock_connected(L,i)== FALSE) 
	    {
	      int m= L->obj->poly->m;
	      double *x= L->obj->poly->R, *y = x + m; 
	      /* Scierror("Link lock point is over an object lock and lock is not active\n"); */
	      if ( i==0 && m >= 2 ) 
		{
		  pt[0] += (x[1]-pt[0])/2;
		  pt[1] += (y[1]-pt[1])/2;
		}
	      else if ( i == 1 && m >= 2)
		{
		  pt[0] += (x[m-2]-pt[0])/2;
		  pt[1] += (y[m-2]-pt[1])/2;
		}
	      link_set_lock_pos(L,i,pt,FALSE,LD_ANY);
	    }
	}
      /* checks if lock point is over a link */
      if ( nsp_gframe_select_obj(F,pt,&obj,(NspObject *)L) != 0) 
	{
	  if ( link_is_lock_connected(L,i)== FALSE) 
	    {
	      Scierror("Link lock point [%5.2f,%5.2f]is over an object \n",pt[0],pt[1]);
	      if ( (obj != (NspObject *) L) && IsLink(obj))
		{
		  NspLink *link;
		  Scierror("I split the link \n");
		  if ( link_split(F,(NspLink *)obj,&link,pt) == OK)
		    {
		      /* create a connector */
		      int color=4,thickness=1, background=9;
		      double rect[]={pt[0]-lock_size,pt[1]+lock_size,lock_size*3,lock_size*3}; 
		      NspConnector *C;
		      gr_port p;
		      C=connector_create("fe",rect,color,thickness,background,NULL);
		      if ( C == NULL) return;
		      /* herits the frame graphic context */
		      C->obj->frame = F->obj;
		      if (nsp_list_end_insert(F->obj->objs,NSP_OBJECT(C)) == FAIL) return ; 
		      /* and link obj,link and L to the connector */
		      p.object_id =NSP_OBJECT(C); 
		      p.lock = 0; 
		      p.port = 0; /* not used */
		      link_lock(F,(NspLink *)obj,1,&p); 
		      link_lock(F,link,0,&p); 
		      link_lock(F,L,i,&p); 
		      nsp_gframe_locks_update(F,NSP_OBJECT(C));/* align the locks */
		      GR_INT(((NspObject *)C)->basetype->interface)->draw(C);

		    }
		}
	    }
	}
    }

  /* 
   * checks if the link polyline control points are not over lock points 
   */
  for ( i=1; i <  L->obj->poly->m-1 ; i++) 
    {
      pt[0]= L->obj->poly->R[i]; 
      pt[1]= L->obj->poly->R[i+L->obj->poly->m]; 
      if ( nsp_gframe_select_lock(F,pt,&obj,&cp,&lock_c) != 0) 
	{
	  L->obj->poly->R[i]+= 2*lock_size;
	}
    }

}

/*----------------------------------------------------
 * methods of GRint 
 *----------------------------------------------------*/

/**
 * link_get_number_of_locks: 
 * @B: a link 
 * 
 * Returns the number of lock points of the link 
 * 
 * Return value: the number of lock points
 **/

int link_get_number_of_locks(const NspLink *B) 
{
  return 2;
}

/**
 * link_get_number_of_ports: 
 * @B: a link 
 * @lp: a lock point 
 * 
 * Returns the number of ports of lock points lp;
 * 
 * Return value: the number of ports
 **/

int link_get_number_of_ports(const NspLink *B,int lp) 
{
  return 1;
}


/**
 * link_get_lock_connection: 
 * @B: a link 
 * @i: a lock point id. 
 * @port: a port of the lock point @i;
 * 
 * Returns in a gr_port structure information about the object 
 * connected to the port @port of lock point @i. 
 * 
 * Return value: #TRUE if lock point and port number exists or #FALSE. 
 **/

int link_get_lock_connection(const NspLink *B,int i,int port, gr_port *p )
{
  if ( port == 0)
    {
      switch (i){
      case 0:  *p = B->obj->lock1.port; return OK;
      case 1:  *p = B->obj->lock2.port; return OK;
      }
    }
  return FAIL;
}

/**
 * link_get_lock_pos:
 * @B: a link 
 * @i: a lock point id. 
 * @pt: point coordinates.
 *
 * @pt is filled with the position of lock point @i. 
 **/

void link_get_lock_pos(const NspLink *B, int i,double pt[])
{
  if ( i ==  0 ) 
    {
      pt[0]= B->obj->poly->R[0];
      pt[1]= B->obj->poly->R[B->obj->poly->m];
    }
  else if ( i == 1 ) 
    {
      pt[0]= B->obj->poly->R[B->obj->poly->m-1];
      pt[1]= B->obj->poly->R[2*B->obj->poly->m-1];

    }
}


/**
 * link_get_lock_dir:
 * @B: 
 * @i: 
 * 
 * Return value: 
 **/

lock_dir link_get_lock_dir(const NspLink *L, int i)
{
  return LD_ANY;
}


/**
 * link_set_lock_connection: 
 * @B: a link 
 * @i: a lock point id. 
 * @prt: port id (0 for links)
 * @p: information to be connected to one port of lock point i;
 * 
 * the port described by @p is connected to a port of lock point i; 
 * return value: -1 or the port number used for connection.
 **/

int link_set_lock_connection(NspLink *B,int i,int prt,const gr_port *p)
{
  if ( i ==  0 || i == 1 ) 
    {
      gr_port *port;
      if ( i == 0 ) 
	port= &B->obj->lock1.port;
      else
	port= &B->obj->lock2.port;
      if ( port->object_id != NULL) return -2;
      *port = *p;
      return 0;
    }
  return -1;
}

/**
 * link_unset_lock_connection: 
 * @B: a link 
 * @i: a lock point id. 
 * @prt: a lock point port. 
 * 
 * unconect the object locked to port @port of lock point @i.
 * 
 **/

void link_unset_lock_connection(NspLink *B,int i,int port)
{
  if (( i == 0 || i ==1) && port == 0)
    {
      /* XXXX : faut-il aussi propager l'info sur l'object lock� ? */
      if ( i== 0)
	B->obj->lock1.port.object_id = NULL;
      else
	B->obj->lock2.port.object_id = NULL;
    }
}

/**
 * link_is_lock_connectable
 * @B: a link 
 * @i: a lock point id. 
 * 
 * Checks if there's a free port in lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int link_is_lock_connectable(NspLink *B,int i)
{
  switch (i) 
    {
    case 0: if ( B->obj->lock1.port.object_id == NULL) return TRUE;
    case 1: if ( B->obj->lock2.port.object_id == NULL) return TRUE;
    }
  return FALSE;
}

/**
 * link_is_lock_connected 
 * @B: a link 
 * @i: a lock point id. 
 * 
 * Checks if there's a locked port for lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int link_is_lock_connected(NspLink *B,int i)
{
  switch (i ) {
  case 0 :  if ( B->obj->lock1.port.object_id != NULL) return TRUE; break;
  case 1 :  if ( B->obj->lock2.port.object_id != NULL) return TRUE; break;
  }
  return FALSE;
}

/**
 * link_set_lock_pos: 
 * @B: a link 
 * @i: a lock point id. 
 * @pt: a point coordinates 
 * @keep_angle: an integer 
 * 
 * Sets the lock point @i position to @pt. 
 * XXXX : keep_angle should be replaced in order 
 *      to impose pref for horizontal or vertical direction.
 *      according to the port position of the block 
 *      
 **/

static void link_set_lock_pos(NspLink *B, int i,const double pt[],int  keep_angle,lock_dir dir)
{
  double ptl[2];
  int hvfactor = 2*lock_size;
  NspMatrix *M = B->obj->poly;
  int m= B->obj->poly->m;
  int xp = ( i== 0) ? 0 : m-1;
  int yp = xp + m;
  int next = ( i==0 ) ? 1 : -1;

  if ( keep_angle == TRUE && dir != LD_ANY && m >= 3 ) 
    {
      ptl[0]=M->R[xp];
      ptl[1]=M->R[yp];
      switch ( dir ) 
	{
	case LD_NORTH: 
	case LD_SOUTH: 
	  if ( Abs( M->R[xp] - M->R[xp+next]) < hvfactor )  M->R[xp+next]= pt[0];break;
	case LD_WEST:
	case LD_EAST: 
	  if ( Abs( M->R[yp] - M->R[yp+next]) < hvfactor )   M->R[yp+next]= pt[1];break;
	case LD_ANY: 
	  break;
	}
      M->R[xp]=pt[0];
      M->R[yp]=pt[1];
    }
  else 
    {
      M->R[xp]=pt[0];
      M->R[yp]=pt[1];
    }
}


/*
 * load 
 */

static NspLink  *link_full_copy(NspLink *L)
{
  NspLink *L1=NULLLINK;

  extern NspLink *nsp_link_create(char *name,nspgframe* frame,void* object_sid,int color,int thickness,int background,NspMatrix* poly,grl_lock lock1,grl_lock lock2,gboolean hilited,gboolean show,NspTypeBase *type);

  if (( L1 = nsp_link_create(NVOID,NULL,NULL,L->obj->color,L->obj->thickness,L->obj->background,
			     L->obj->poly,L1->obj->lock1,L1->obj->lock2,L1->obj->hilited,
			     L1->obj->show,    NULL)) == NULLLINK) return NULLLINK;
  L1->obj->object_sid = L;
  /* the lock points */
  L1->obj->lock1 = L->obj->lock1;
  L1->obj->lock1.port.object_id = NULLOBJ;
  L1->obj->lock1.port.object_sid = L->obj->lock1.port.object_id;
  /* */
  L1->obj->lock2 = L->obj->lock2;
  L1->obj->lock2.port.object_id = NULLOBJ;
  L1->obj->lock2.port.object_sid = L->obj->lock2.port.object_id;
  return L1;
}

/*
 * Lock the port of Link L 
 * to object Obj at obj_lock, obj_port.
 */

static int link_connect(NspLink *L,int lock, NspObject *Obj,int obj_lock,int obj_port)
{
  double pt[2];
  gr_port p;
  NspTypeGRint *bf,*obj_bf;
  if ( L == NULLLINK) return FAIL;
  bf = GR_INT(((NspObject *) L)->basetype->interface);
  obj_bf = GR_INT(((NspObject *) Obj)->basetype->interface);
  if ( obj_bf->get_lock_connection(Obj,obj_lock,obj_port,&p)== FAIL) return FAIL;
  if ( p.object_id != NULLOBJ ) return FAIL;
  /* first unlock */
  link_unlock( L,lock) ;
  /* relock to new object */
  p.object_id = Obj;
  p.lock = obj_lock;
  p.port = 0;
  link_set_lock_connection(L,lock,0,&p);
  /* relock Obj to L */
  p.object_id = (NspObject *) L;
  p.lock = lock;
  p.port = 0;
  obj_bf->set_lock_connection(Obj,obj_lock,0,&p);
  obj_bf->get_lock_pos(Obj,obj_lock,pt);
  link_set_lock_pos(L,lock,pt,FALSE,LD_ANY);
  return OK;
}


/* requested for grl_lock
 *
 */

static void  nsp_destroy_grl_lock(grl_lock *locks,NspLink *H)
{
  if ( locks == &H->obj->lock1 ) 
    link_unlock(H,0);
  else
    link_unlock(H,1);
}

static int nsp_save_grl_lock(XDR *xdrs,grl_lock *locks,NspLink *M)
{
  grl_lock *lock= &M->obj->lock1;
  /* the port */
  if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(lock->port.object_id)) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,lock->port.lock) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,lock->port.port) == FAIL) return FAIL;
  lock= &M->obj->lock2;
  if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(lock->port.object_id)) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,lock->port.lock) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,lock->port.port) == FAIL) return FAIL;
  return OK;
}

static int nsp_eq_grl_lock(grl_lock *lock1,grl_lock *lock2)
{
  /* ZZZZ */
  return OK;
}

  
static int nsp_load_grl_lock(XDR *xdrs,grl_lock *locks,NspLink *L)
{
  int id;
  /* the lock points */
  grl_lock *lock= &L->obj->lock1;
  /* the port */
  lock->port.object_id = NULLOBJ;
  if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  FAIL;
  lock->port.object_sid = NSP_INT_TO_POINTER(id);
  if ( nsp_xdr_load_i(xdrs,&lock->port.lock) == FAIL) return FAIL;
  if ( nsp_xdr_load_i(xdrs,&lock->port.port) == FAIL) return FAIL;
  lock= &L->obj->lock2;
  /* the port */
  lock->port.object_id = NULLOBJ;
  if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  FAIL;
  lock->port.object_sid = NSP_INT_TO_POINTER(id);
  if ( nsp_xdr_load_i(xdrs,&lock->port.lock) == FAIL) return FAIL;
  if ( nsp_xdr_load_i(xdrs,&lock->port.port) == FAIL) return FAIL;
  return OK;
}

static int nsp_print_grl_lock(int indent,grl_lock *locks,NspLink *M)
{
  return OK;
}

static int nsp_check_grl_lock(grl_lock *locks,NspLink *M)
{
  return OK;
}


