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

#define  NspBlock_Private 
#include <nsp/nsp.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/object.h>
#include <nsp/gframe.h>
#include <nsp/block.h>
#include <nsp/link.h>
#include <nsp/connector.h>
#include <nsp/grint.h> /* interface definition */
#include <nsp/pr-output.h> 
#include <nsp/interf.h>
#include <nsp/matutil.h>
#include <nsp/plist.h>
#include <nsp/parse.h>


/* 
 * NspBlock inherits from Graphic 
 * and implements  GRint
 */

int nsp_type_block_id=0;
NspTypeBlock *nsp_type_block=NULL;

/*
 * Type object for NspBlock 
 * all the instance of NspTypeBlock share the same id. 
 * nsp_type_block: is an instance of NspTypeBlock 
 *    used for objects of NspBlock type (i.e built with new_block) 
 * other instances are used for derived classes 
 */
NspTypeBlock *new_type_block(type_mode mode)
{
  NspTypeGRint *t_grint;
  NspTypeBlock *type= NULL;
  NspTypeObject *top;

  if (  nsp_type_block != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_block;
    }
  if (( type =  malloc(sizeof(NspTypeBlock))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = block_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = block_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_block;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for block */ 

  top->pr = (print_func *) nsp_block_print;
  top->dealloc = (dealloc_func *) nsp_block_destroy;
  top->copy  =  (copy_func *) nsp_block_copy;
  top->size  = (size_func *) nsp_block_size;
  top->s_type =  (s_type_func *) nsp_block_type_as_string;
  top->sh_type = (sh_type_func *) nsp_block_type_short_string;
  top->info = (info_func *) nsp_block_info;
  /* top->is_true = (is_true_func  *) nsp_block_is_true; */
  /* top->loop =(loop_func *) nsp_block_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_block_object;
  top->eq  = (eq_func *) nsp_block_eq;
  top->neq  = (eq_func *) nsp_block_neq;
  top->save  = (save_func *) nsp_block_xdr_save;
  top->load  = (load_func *) nsp_block_xdr_load;
  top->create = (create_func*) int_block_create;
  top->latex = (print_func *) nsp_block_latex;

  /* specific methods for block */

  type->init = (init_func *) init_block;

  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_block;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_block ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_block  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_block  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_block  ;
  /* next method are defined in NspGraphic and need not be changed here for Block */
  /* ((NspTypeNspGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeNspGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

  /* 
   * NspBlock interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_grint = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) t_grint;

  t_grint->get_hilited 	=(gr_get_hilited *) block_get_hilited;
  t_grint->set_hilited 	=(gr_set_hilited *) block_set_hilited;
  t_grint->get_show    	=(gr_get_show *) block_get_show;
  t_grint->set_show	=(gr_set_show *) block_set_show;
  t_grint->draw    	=(gr_draw *) block_draw;
  t_grint->translate 	=(gr_translate *) block_translate;
  t_grint->set_pos  	=(gr_set_pos *) block_set_pos;
  t_grint->get_pos  	=(gr_get_pos *) block_get_pos;
  t_grint->get_rect  	=(gr_get_rect *) block_get_rect;
  t_grint->resize 	=(gr_resize *) block_resize;
  t_grint->update_locks	=(gr_update_locks *) block_update_locks;
  t_grint->contains_pt 	=(gr_contains_pt *) block_contains_pt;
  t_grint->control_near_pt 	=(gr_control_near_pt *) block_control_near_pt;
  t_grint->lock_near_pt 	=(gr_lock_near_pt *) block_lock_near_pt;
  t_grint->move_control_init    =(gr_move_control_init *) block_move_control_init;
  t_grint->move_control 	=(gr_move_control *) block_move_control;

  t_grint->get_number_of_locks =(gr_get_number_of_locks *) block_get_number_of_locks;
  t_grint->get_number_of_ports =(gr_get_number_of_ports *) block_get_number_of_ports;
  t_grint->get_lock_connection =(gr_get_lock_connection *) block_get_lock_connection;
  t_grint->get_lock_pos =(gr_get_lock_pos *) block_get_lock_pos;
  t_grint->get_lock_dir =(gr_get_lock_dir *) block_get_lock_dir;
  t_grint->set_lock_connection =(gr_set_lock_connection *) block_set_lock_connection;
  t_grint->unset_lock_connection =(gr_unset_lock_connection *) block_unset_lock_connection;
  t_grint->is_lock_connectable =(gr_is_lock_connectable *) block_is_lock_connectable;
  t_grint->is_lock_connected =(gr_is_lock_connected *) block_is_lock_connected;
  t_grint->set_lock_pos =(gr_set_lock_pos *) block_set_lock_pos;
  t_grint->full_copy =(gr_full_copy *) block_full_copy;
  t_grint->unlock =(gr_unlock *) block_unlock;
  t_grint->set_frame =(gr_set_frame *) block_set_frame;

  if ( nsp_type_block_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeBlock called nsp_type_block
       */
      type->id =  nsp_type_block_id = nsp_new_type_id();
      nsp_type_block = type;
      if ( nsp_register_type(nsp_type_block) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_block(mode);
    }
  else 
    {
      type->id = nsp_type_block_id;
      return type;
    }
}

/*
 * initialize NspBlock instances 
 * locally and by calling initializer on parent class 
 */

static int init_block(NspBlock *Obj,NspTypeBlock *type)
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
 * new instance of NspBlock 
 */

NspBlock *new_block() 
{
  NspBlock *loc;
  /* type must exists */
  nsp_type_block = new_type_block(T_BASE);
  if ( (loc = malloc(sizeof(NspBlock)))== NULLBLOCK) return loc;
  /* initialize object */
  if ( init_block(loc,nsp_type_block) == FAIL) return NULLBLOCK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspBlock 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_block_size(NspBlock *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char block_type_name[]="Block";
static char block_short_type_name[]="block";

static char *nsp_block_type_as_string(void)
{
  return(block_type_name);
}

static char *nsp_block_type_short_string(NspObject *v)
{
  return(block_short_type_name);
}

/*
 * A == B 
 */

static int nsp_block_eq(NspBlock *A, NspObject *B)
{
  NspBlock *loc = (NspBlock *) B;
  if ( check_cast(B,nsp_type_block_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->frame != loc->obj->frame) return FALSE;
  if ( A->obj->object_sid != loc->obj->object_sid) return FALSE;
  {
    int i;
    for ( i = 0 ; i < 4 ; i++ )
      if ( A->obj->r[i] != loc->obj->r[i] ) return FALSE;
  }
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( A->obj->background != loc->obj->background) return FALSE;
  if ( A->obj->n_locks != loc->obj->n_locks) return FALSE;
  if ( A->obj->locks != loc->obj->locks) return FALSE;
  if ( A->obj->hilited != loc->obj->hilited) return FALSE;
  if ( A->obj->show != loc->obj->show) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_block_neq(NspBlock *A, NspObject *B)
{
  return ( nsp_block_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_block_xdr_save(XDR  *xdrs, NspBlock *M)
{
  int i;
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_block)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  /* the block */
  if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(M)) == FAIL) return FAIL;
  if ( nsp_xdr_save_array_d(xdrs,M->obj->r,4) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,M->obj->color) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,M->obj->thickness) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,M->obj->background) == FAIL) return FAIL;
  /* the lock points */
  if ( nsp_xdr_save_i(xdrs,M->obj->n_locks) == FAIL) return FAIL;
  for ( i = 0 ; i < M->obj->n_locks ; i++) 
    {
      grb_lock *lock= M->obj->locks+i;
      if ( nsp_xdr_save_array_d(xdrs,lock->pt,2) == FAIL) return FAIL;
      if ( nsp_xdr_save_array_d(xdrs,lock->ptr,2) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,lock->type) == FAIL) return FAIL;
      /* the port */
      if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(lock->port.object_id)) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,lock->port.lock) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,lock->port.port) == FAIL) return FAIL;
    }
  /* the upper class */
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspBlock  *nsp_block_xdr_load_partial(XDR *xdrs, NspBlock *M)
{
  int fid,id,i;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_block))) == NULL) return NULL;
  M->obj->ref_count=1;

  if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  NULLBLOCK;
  if ( nsp_xdr_load_array_d(xdrs,M->obj->r,4) == FAIL) return NULLBLOCK;
  if ( nsp_xdr_load_i(xdrs,&M->obj->color) == FAIL) return NULLBLOCK;
  if ( nsp_xdr_load_i(xdrs,&M->obj->thickness) == FAIL) return NULLBLOCK;
  if ( nsp_xdr_load_i(xdrs,&M->obj->background) == FAIL) return NULLBLOCK;
  M->obj->object_sid = NSP_INT_TO_POINTER(id);
  /* the lock points */
  if ( nsp_xdr_load_i(xdrs,&M->obj->n_locks) == FAIL) return NULLBLOCK;
  if ( M->obj->locks != NULL) FREE(M->obj->locks);
  if (( M->obj->locks = malloc(M->obj->n_locks*sizeof(grb_lock))) == NULL ) 
    return NULLBLOCK;
  for ( i = 0 ; i < M->obj->n_locks ; i++) 
    {
      grb_lock *lock= M->obj->locks+i;
      if ( nsp_xdr_load_array_d(xdrs,lock->pt,2) == FAIL) return NULLBLOCK;
      if ( nsp_xdr_load_array_d(xdrs,lock->ptr,2) == FAIL) return NULLBLOCK;
      if ( nsp_xdr_load_i(xdrs,&lock->type) == FAIL) return NULLBLOCK;
      /* the port */
      lock->port.object_id = NULLOBJ;
      if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  NULLBLOCK;
      lock->port.object_sid = NSP_INT_TO_POINTER(id);
      if ( nsp_xdr_load_i(xdrs,&lock->port.lock) == FAIL) return NULLBLOCK;
      if ( nsp_xdr_load_i(xdrs,&lock->port.port) == FAIL) return NULLBLOCK;
    }
  
  /* the upper class */
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspBlock  *nsp_block_xdr_load(XDR *xdrs)
{
  NspBlock *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBLOCK;
  if ((H  = nsp_block_create_void(name,(NspTypeBase *) nsp_type_block))== NULLBLOCK) return H;
  if ((H  = nsp_block_xdr_load_partial(xdrs,H))== NULLBLOCK) return H;
  if ( nsp_block_check_values(H) == FAIL) return NULLBLOCK;
  return H;
}

/*
 * delete 
 */

void nsp_block_destroy_partial(NspBlock *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     int i;
     for ( i = 0 ; i <  H->obj->n_locks ;i++)
       {
	 block_unlock(H,i);
       }
     FREE(H->obj->locks);
     FREE(H->obj);
   }
}

void nsp_block_destroy(NspBlock *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_block_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

static int nsp_block_info(NspBlock *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLBLOCK) 
    {
      Sciprintf("Null Pointer NspBlock \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_block_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_block_print(NspBlock *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLBLOCK) 
    {
      Sciprintf("Null Pointer NspBlock \n");
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
          nsp_block_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_block_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"frame=%xl\n",M->obj->frame);
  Sciprintf1(indent+2,"object_sid=%xl\n",M->obj->object_sid);
  if ( nsp_print_array_double(indent+2,"r",M->obj->r,4,rec_level) == FALSE ) return FALSE ;
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"background=%d\n",M->obj->background);
  Sciprintf1(indent+2,"n_locks=%d\n",M->obj->n_locks);
  Sciprintf1(indent+2,"locks=%xl\n",M->obj->locks);
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

int nsp_block_latex(NspBlock *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_block_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"frame=%xl\n",M->obj->frame);
  Sciprintf1(indent+2,"object_sid=%xl\n",M->obj->object_sid);
  if ( nsp_print_latex_array_double(indent+2,"r",M->obj->r,4,rec_level) == FALSE ) return FALSE ;
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"background=%d\n",M->obj->background);
  Sciprintf1(indent+2,"n_locks=%d\n",M->obj->n_locks);
  Sciprintf1(indent+2,"locks=%xl\n",M->obj->locks);
  Sciprintf1(indent+2,"hilited	= %s\n", ( M->obj->hilited == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"show	= %s\n", ( M->obj->show == TRUE) ? "T" : "F" );
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspBlock objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspBlock   *nsp_block_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_block_id) == TRUE ) return ((NspBlock *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_block));
  return NULL;
}

int IsBlockObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_block_id);
}

int IsBlock(NspObject *O)
{
  return nsp_object_type(O,nsp_type_block_id);
}

NspBlock  *GetBlockCopy(Stack stack, int i)
{
  if (  GetBlock(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBlock  *GetBlock(Stack stack, int i)
{
  NspBlock *M;
  if (( M = nsp_block_object(NthObj(i))) == NULLBLOCK)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspBlock instance 
 *-----------------------------------------------------*/

static NspBlock *nsp_block_create_void(char *name,NspTypeBase *type)
{
 NspBlock *H  = (type == NULL) ? new_block() : type->new();
 if ( H ==  NULLBLOCK)
  {
   Sciprintf("No more memory\n");
   return NULLBLOCK;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLBLOCK;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_block_create_partial(NspBlock *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_block)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->frame = NULL;
  H->obj->object_sid = NULL;
  {
    double x_def[4]={0,0,0,0};
    memcpy(H->obj->r,x_def,4*sizeof(double));
  }
  H->obj->color = 0;
  H->obj->thickness = 0;
  H->obj->background = 0;
  H->obj->n_locks = 0;
  H->obj->locks = NULL;
  H->obj->hilited = FALSE;
  H->obj->show = TRUE;
  return OK;
}

int nsp_block_check_values(NspBlock *H)
{
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

static double lock_size=1; /*  XXX a factoriser quelque part ... */ 
static int lock_color=10;

NspBlock *nsp_block_create(char *name,nspgframe* frame,void* object_sid,double* r,int color,int thickness,int background,int n_locks,grb_lock* locks,int hilited,gboolean show,NspTypeBase *type)
{
  double pt[2];
  int i;
  NspBlock *H  = nsp_block_create_void(name,type);
  if ( H ==  NULLBLOCK) return NULLBLOCK;
  if ( nsp_block_create_partial(H) == FAIL) return NULLBLOCK;
  H->obj->frame = frame;
  H->obj->object_sid = object_sid;
  memcpy(H->obj->r,r,4*sizeof(double));
  H->obj->color=color;
  H->obj->thickness=thickness;
  H->obj->background=background;
  H->obj->n_locks=n_locks;
  H->obj->locks = locks;
  H->obj->hilited=hilited;
  H->obj->show=show;
  /* initial lock points */
  H->obj->n_locks = 4 ;
  if (( H->obj->locks = malloc(H->obj->n_locks*sizeof(grb_lock))) == NULL ) return NULLBLOCK;
  for (i=0; i < H->obj->n_locks ; i++) 
    {
      H->obj->locks[i].port.object_id = NULL; 
      H->obj->locks[i].port.object_sid = NULL; 
    }
  /* fix the relative position of the four initial locks */

  /* 
   * move the lock pos to the triangle head XXX !! 
   */
  H->obj->locks[0].type = LD_NORTH | L_EVIN ;
  H->obj->locks[1].type = LD_SOUTH | L_EVOUT ;
  H->obj->locks[2].type = LD_WEST | L_IN ;
  H->obj->locks[3].type = LD_EAST | L_OUT ;
  block_set_lock_pos_rel(H,0,(pt[0]=0.5,pt[1]=0,pt));
  block_set_lock_pos_rel(H,1,(pt[0]=0.5,pt[1]=1,pt));
  block_set_lock_pos_rel(H,2,(pt[0]=0.0,pt[1]=0.5,pt));
  block_set_lock_pos_rel(H,3,(pt[0]=1,pt[1]=0.5,pt));
  if ( nsp_block_check_values(H) == FAIL) return NULLBLOCK;
  return H;
}

NspBlock *nsp_block_create_default(char *name)
{
 NspBlock *H  = nsp_block_create_void(name,NULL);
 if ( H ==  NULLBLOCK) return NULLBLOCK;
  if ( nsp_block_create_partial(H) == FAIL) return NULLBLOCK;
 if ( nsp_block_check_values(H) == FAIL) return NULLBLOCK;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspBlock *nsp_block_copy_partial(NspBlock *H,NspBlock *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspBlock *nsp_block_copy(NspBlock *self)
{
  NspBlock *H  =nsp_block_create_void(NVOID,(NspTypeBase *) nsp_type_block);
  if ( H ==  NULLBLOCK) return NULLBLOCK;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLBLOCK;
  if ( nsp_block_copy_partial(H,self)== NULL) return NULLBLOCK;

  return H;
}

/*
 * full copy for gobject derived class
 */

NspBlock *nsp_block_full_copy_partial(NspBlock *H,NspBlock *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_block))) == NULL) return NULLBLOCK;
  H->obj->ref_count=1;
  H->obj->frame = self->obj->frame;
  H->obj->object_sid = self->obj->object_sid;
  memcpy(H->obj->r,self->obj->r,4*sizeof(double));
  H->obj->color=self->obj->color;
  H->obj->thickness=self->obj->thickness;
  H->obj->background=self->obj->background;
  H->obj->n_locks=self->obj->n_locks;
  H->obj->locks = self->obj->locks;
  H->obj->hilited=self->obj->hilited;
  H->obj->show=self->obj->show;
  return H;
}

NspBlock *nsp_block_full_copy(NspBlock *self)
{
  NspBlock *H  =nsp_block_create_void(NVOID,(NspTypeBase *) nsp_type_block);
  if ( H ==  NULLBLOCK) return NULLBLOCK;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLBLOCK;
  if ( nsp_block_full_copy_partial(H,self)== NULL) return NULLBLOCK;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspBlock
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val)
{
  NspMatrix *M1;
  int i;
  static double l[4];
  switch ( rhs - opt ) 
    {
    case 1 :
      if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
      CheckLength(NspFname(stack),1,M1,4);
      *val = M1->R;
      break;
    case 4 :
      for ( i = 1 ; i <= 4 ; i++) 
	{
	  if (GetScalarDouble(stack,i,l+i-1) == FAIL) return FAIL;
	}
      *val = l;
      break;
    default :
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 4\r\n",NspFname(stack),rhs-opt);
      return FAIL;
    }
  return OK;
}

static int int_block_create(Stack stack, int rhs, int opt, int lhs)
{
  NspBlock *H;
  double *val=NULL;
  int back=-1,color=-1,thickness=-1;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = nsp_block_create(NVOID,NULL,NULL,val,color,thickness,back,0,NULL,FALSE,TRUE,NULL))
     == NULLBLOCK) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/

/* draw */

static int _wrap_block_draw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  block_draw(self);
  return 0;
}

/* translate */

static int _wrap_block_translate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  block_translate(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/* set_position */

static int _wrap_block_set_pos(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  block_set_pos(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/* resize */ 

static int _wrap_block_resize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  block_resize(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/* fix a lock point position 
 * in relative coordinates 
 */

static int _wrap_block_set_lock_pos(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  int lock;
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ( GetScalarInt(stack,1,&lock) == FAIL) return RET_BUG;
  if ((M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  block_set_lock_pos_rel(self,lock,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/*
 * reset the locks pos
 */

static int _wrap_block_set_locks_pos(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((Pt= GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ( Pt->m != 3 ) 
    {
      Scierror("Error: wrong dimensions should have 3 rows\n");
      return RET_BUG;
    }
  if ( block_set_locks(self,Pt) == FAIL) return RET_BUG;
  MoveObj(stack,1,self);
  return 1;
}


static NspMethods block_methods[] = {
  { "translate", _wrap_block_translate},
  { "set_pos", _wrap_block_set_pos},
  { "resize",   _wrap_block_resize},
  { "draw",   _wrap_block_draw},
  { "set_lock_pos", _wrap_block_set_lock_pos},
  { "set_locks_pos", _wrap_block_set_locks_pos},
  { (char *) 0, NULL}
};

static NspMethods *block_get_methods(void) { return block_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_block_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspBlock *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_block_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspBlock *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_block_get_thickness(void *self,const char *attr)
{
  int ret;

  ret = ((NspBlock *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_block_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspBlock *) self)->obj->thickness= thickness;
  return OK;
}

static NspObject *_wrap_block_get_background(void *self,const char *attr)
{
  int ret;

  ret = ((NspBlock *) self)->obj->background;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_block_set_background(void *self,const char *attr, NspObject *O)
{
  int background;

  if ( IntScalar(O,&background) == FAIL) return FAIL;
  ((NspBlock *) self)->obj->background= background;
  return OK;
}

static NspObject *_wrap_block_get_hilited(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspBlock *) self)->obj->hilited;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_block_set_hilited(void *self,const char *attr, NspObject *O)
{
  int hilited;

  if ( BoolScalar(O,&hilited) == FAIL) return FAIL;
  ((NspBlock *) self)->obj->hilited= hilited;
  return OK;
}

static NspObject *_wrap_block_get_show(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspBlock *) self)->obj->show;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_block_set_show(void *self,const char *attr, NspObject *O)
{
  int show;

  if ( BoolScalar(O,&show) == FAIL) return FAIL;
  ((NspBlock *) self)->obj->show= show;
  return OK;
}

static AttrTab block_attrs[] = {
  { "color", (attr_get_function *)_wrap_block_get_color, (attr_set_function *)_wrap_block_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_block_get_thickness, (attr_set_function *)_wrap_block_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "background", (attr_get_function *)_wrap_block_get_background, (attr_set_function *)_wrap_block_set_background,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "hilited", (attr_get_function *)_wrap_block_get_hilited, (attr_set_function *)_wrap_block_set_hilited,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "show", (attr_get_function *)_wrap_block_get_show, (attr_set_function *)_wrap_block_set_show,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_block(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_block(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Block_func[]={
  {"extractelts_block", _wrap_nsp_extractelts_block},
  {"setrowscols_block", _wrap_nsp_setrowscols_block},
  { "block_create", int_block_create},
  { NULL, NULL}
};

/* call ith function in the Block interface */

int Block_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Block_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Block_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Block_func[i].name;
  *f = Block_func[i].fonc;
}

/* inserted verbatim at the end */

/* methods for the graphic class 
 *
 *
 */

/* see below 
static void nsp_draw_block(BCG *Xgc,NspGraphic *Obj, void *data)
{

}
*/

static void nsp_translate_block(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  /* NspBlock *P = (NspBlock *) Obj;*/
}

static void nsp_rotate_block(BCG *Xgc,NspGraphic *Obj,double *R)
{
  
}

static void nsp_scale_block(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  /*   NspBlock *P = (NspBlock *) Obj; */
}

/* compute in bounds the enclosing rectangle of block 
 *
 */

static int nsp_getbounds_block (BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspBlock *B = (NspBlock *) Obj;
  block_get_rect(B,bounds);
  return TRUE;
}

/*
 * implementation of the GRint interface 
 * for a block 
 */ 

/**
 * block_get_hilited:
 * @B: a block 
 *
 * Returns: the value of the hilited attribute of object @B.
 *
 **/

int block_get_hilited(NspBlock *B) {  return B->obj->hilited; } 

/**
 * block_set_hilited:
 * @B: a block 
 * @val: %True or %False. 
 * 
 * Sets the hilited status of the block @B.
 *
 **/

void block_set_hilited(NspBlock *B,int val) {  B->obj->hilited = val; } 

/**
 * block_get_show:
 * @B: a block 
 * @val:  %True or %False. 
 * 
 * Returns: the value of the show attribute of object @B.
 *
 **/

int block_get_show(NspBlock *B) {  return B->obj->show; } 

/**
 * block_set_show:
 * @B: a block 
 *
 * Sets the show status of the given Block.
 *
 **/

void block_set_show(NspBlock *B,int val) {  B->obj->show = val; } 

/**
 * lock_draw:
 * @Xgc: a #BCG context 
 * @pt: a point 
 * @dir: direction of lock point #lock_dir
 * @typ: a #lock_type 
 * @locked: is lock point locked ?
 * 
 * draw a lock point at lock position @pt 
 * It a lock point in thus the lock point is a the 
 * arraw end 
 * 
 * Return value: 
 **/

static void lock_draw(BCG *Xgc,const double pt[2],lock_dir dir,lock_type typ,int locked)
{
  /* angle according to dir */
  /*  LD_NORTH=0, LD_SOUTH=1, LD_EAST=2, LD_WEST=3, LD_ANY=4 */
  const double alpha[]= {0,180,-90,90,0};
  const double lock_triangle_x[]={-lock_size/2,lock_size/2,0};
  const double lock_triangle_yout[]={-lock_size,-lock_size,0};
  const double lock_triangle_yin[]={0,0,-lock_size}; 
  const double lock_square_x[]={-lock_size/2,lock_size/2,lock_size/2,-lock_size/2};
  const double lock_square_y[]={-lock_size,-lock_size,0,0};
  const double *ly=NULL,*lx=NULL;
  double cosa,sina;
  double x[4];
  double y[4];
  int npt=3 , i;
  switch (typ) 
    {
    case L_SQP:
    case L_SQM: 
      npt=4;
      lx=  lock_square_x;
      ly=  lock_square_y;
      break;
    case L_IN:
    case L_EVIN: 
      ly = lock_triangle_yin; 
      lx = lock_triangle_x; 
      break;
    case L_OUT:
    case L_EVOUT: 
      ly = lock_triangle_yout;
      lx = lock_triangle_x; 
      break;
    }
  cosa= cos(alpha[dir]*M_PI/180);
  sina= sin(alpha[dir]*M_PI/180);
  for ( i = 0 ; i < npt ; i++) 
    {
      x[i] = cosa*lx[i] -sina*ly[i]+pt[0];
      y[i] = sina*lx[i] +cosa*ly[i]+pt[1];
    }
  if ( locked ) 
    Xgc->graphic_engine->scale->fillpolyline(Xgc,x,y,npt,TRUE);
  Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,npt,TRUE);
}


/**
 * block_draw:
 * @B: a block 
 *
 * Draws a block given the current graphic driver. 
 *
 **/

static void draw_3d(BCG *Xgc,double r[]);

void block_draw(NspBlock *B)
{
  Sciprintf("Error: to be updated with nsp_draw_block\n");
}

static void nsp_draw_block(BCG *Xgc,NspGraphic *Obj, void *data)
{
  BCG *Xgc1;
  NspBlock *B = (NspBlock *) Obj;
  /* take care of the fact that str1 must be writable */
  char str1[] = "my\nblock";
  char str[256];
  double loc[4];
  int cpat, cwidth,i, draw_script, fill=FALSE;
  
  /* check the show attribute */
  if ( B->obj->show == FALSE ) return ;
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
  
  /* first draw inside */
  /* just a test we draw a Matrix inside the block */
  draw_script = 2;
  switch (draw_script)
    {
    case 0: 
      Xgc1 = window_list_get_first(); 
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,TRUE);
      sprintf(str,"Matplot1(rand(10,10)*32,[%5.2f,%5.2f,%5.2f,%5.2f]);",B->obj->r[0],B->obj->r[1]-B->obj->r[3],
	      B->obj->r[0]+B->obj->r[2],B->obj->r[1]);
      nsp_parse_eval_from_string(str,FALSE,FALSE,FALSE,TRUE);
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc1->CurWindow,TRUE);
      break;
    case 1: 
      Xgc1 = window_list_get_first(); 
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,TRUE);
      sprintf(str,"draw_vanne([%5.2f,%5.2f,%5.2f,%5.2f]);",B->obj->r[0],B->obj->r[1],B->obj->r[2],B->obj->r[3]);
      nsp_parse_eval_from_string(str,FALSE,FALSE,FALSE,TRUE);
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc1->CurWindow,TRUE);
      break;
    case 2:
      /* filling with white */
      Xgc->graphic_engine->xset_pattern(Xgc,8);
      Xgc->graphic_engine->scale->fillrectangle(Xgc,B->obj->r);
      /* drawing a string */
      Xgc->graphic_engine->xset_pattern(Xgc,0);
      loc[0] = B->obj->r[1] - B->obj->r[3];
      Xgc->graphic_engine->scale->xstringb(Xgc,str1,&fill,
					   B->obj->r,loc,B->obj->r+2,B->obj->r+3);
      break;
    case 3: 
      Xgc1 = window_list_get_first(); 
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,TRUE);
      sprintf(str,"draw_plot3d([%5.2f,%5.2f,%5.2f,%5.2f]);",B->obj->r[0],B->obj->r[1],B->obj->r[2],B->obj->r[3]);
      nsp_parse_eval_from_string(str,FALSE,FALSE,FALSE,TRUE);
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc1->CurWindow,TRUE);
      break;
    case 4: 
      Xgc1 = window_list_get_first(); 
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc->CurWindow,TRUE);
      sprintf(str,"draw_gtk_logo([%5.2f,%5.2f,%5.2f,%5.2f]);",B->obj->r[0],B->obj->r[1],B->obj->r[2],B->obj->r[3]);
      nsp_parse_eval_from_string(str,FALSE,FALSE,FALSE,TRUE);
      if (Xgc1 != Xgc ) Xgc->graphic_engine->xset_curwin(Xgc1->CurWindow,TRUE);
      break;
    default: 
      /* fill rectangle */
      Xgc->graphic_engine->xset_pattern(Xgc,B->obj->background);
      Xgc->graphic_engine->scale->fillrectangle(Xgc,B->obj->r);
      break;
    }
  /* draw frame rectangle */
  Xgc->graphic_engine->xset_pattern(Xgc,B->obj->color);
  draw_3d(Xgc,B->obj->r);
  Xgc->graphic_engine->scale->drawrectangle(Xgc,B->obj->r);
  /* add the control points if block is hilited */ 
  Xgc->graphic_engine->xset_pattern(Xgc,lock_color);
  if ( B->obj->hilited == TRUE ) 
    {
      loc[0]=B->obj->r[0]; loc[1]=B->obj->r[1];loc[2]=loc[3]= lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
      loc[0]+= B->obj->r[2] -lock_size; loc[1] -= B->obj->r[3] -lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
    }
  for ( i=0 ; i < B->obj->n_locks  ; i++ ) 
    {
      int locked,type;
      if ( block_is_lock_connected(B,i)== TRUE)
	{
	  locked = TRUE;
	  Xgc->graphic_engine->xset_pattern(Xgc,lock_color); 
	}
      else 
	{
	  locked = FALSE;
	  Xgc->graphic_engine->xset_pattern(Xgc,1); 
	}
      block_get_lock_pos(B,i,loc);
      /* need a method here ? */
      type = B->obj->locks[i].type;
      lock_draw(Xgc,loc,type & LOCK_DIR_FLAG ,type &LOCK_TYPE_FLAG ,locked);
      /* loc[0] += -1; loc[1] += 1;loc[2]=loc[3]= lock_size;
       *  Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
       */
    }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}

static void draw_3d(BCG *Xgc,double r[])
{
  int npt=4;
  double size3d=0.5;
  double x[]={r[0],r[0],r[0]- size3d,r[0]-size3d};
  double y[]={r[1],r[1]-r[3],r[1]-r[3]-size3d,r[1]-size3d};
  double x1[]={r[0],r[0]+r[2],r[0]+r[2]- size3d,r[0]-size3d};
  double y1[]={r[1]-r[3],r[1]-r[3],r[1]-r[3]-size3d,r[1]-r[3]-size3d};
  Xgc->graphic_engine->scale->fillpolyline(Xgc,x,y,npt,TRUE);
  Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,npt,TRUE);
  Xgc->graphic_engine->scale->fillpolyline(Xgc,x1,y1,npt,TRUE);
  Xgc->graphic_engine->scale->drawpolyline(Xgc,x1,y1,npt,TRUE);
  
}


/**
 * block_tranlate:
 * @B: a block 
 * @tr: translation vector 
 * 
 * Tranlates the block origin (upper left point) using the 
 * value of vector @tr. 
 *
 **/

int block_translate(NspBlock *B,const double tr[2])
{
  B->obj->r[0] += tr[0] ;
  B->obj->r[1] += tr[1] ;
  block_update_locks(B);
  return OK;
}

int block_set_pos(NspBlock *B,const double tr[2])
{
  B->obj->r[0] = tr[0] ;
  B->obj->r[1] = tr[1] ;
  block_update_locks(B);
  return OK;
}

void block_get_pos(NspBlock *B, double tr[2])
{
  tr[0] = B->obj->r[0]; 
  tr[1] = B->obj->r[1];
}

void block_get_rect(NspBlock *B, double r[4])
{
  memcpy(r,B->obj->r, 4*sizeof(double));
}


/**
 * block_resize: 
 * @B: a block 
 * @size: new width and height of the block given in an array of double.
 * 
 * Resize the block using the value of vector @size. 
 *
 **/

void block_resize(NspBlock *B,const double size[2])
{
  /* double pt[2]; */
  B->obj->r[2] = Max(size[0],3*lock_size) ;
  B->obj->r[3] = Max(size[1],3*lock_size) ;
  /* if resized the lock relative positions should change to XXXXX */
  block_update_locks(B);
}


/**
 * block_update_locks:
 * @B: a block 
 * 
 * Recomputes the positions of the locks point of the block.
 *
 **/

static void block_set_lock_pos_from_rel(NspBlock *B, int i);

void block_update_locks(NspBlock *B)
{
  int i;
  for (i=0; i < B->obj->n_locks ; i++) 
    {
      block_set_lock_pos_from_rel(B,i);
    }
}

/**
 * block_contains_pt
 * @B: a block 
 * @pt: a point position 
 * 
 * Checks if the given point is inside the block enclosing rectangle but not 
 * in a lock point of block @B.
 * 
 * Return value: %True or %False.
 **/

int block_contains_pt(const NspBlock *B,const double pt[2])
{
  int rep = B->obj->r[0] <= pt[0] && B->obj->r[1] >= pt[1] && B->obj->r[0]+B->obj->r[2] >= pt[0] && B->obj->r[1]- B->obj->r[3] <= pt[1];
  if (rep == TRUE )
    {
      int i;
      for ( i=0 ; i < B->obj->n_locks ; i++ ) 
	{
	  double d= Max(Abs( B->obj->locks[i].pt[0] -pt[0]),Abs( B->obj->locks[i].pt[1] -pt[1])) ;
	  if ( d < lock_size/2 ) 
	    return FALSE;
	}
    }
  return rep;
}

/**
 * block_control_near_pt:
 * @B: a block 
 * @pt: a point position 
 * @cp: the control point id in case of success.
 * 
 * Checks if the given point is near a block control point. Note 
 * that a block just have one control point which is the (down,right) 
 * corner used for resizing. 
 * 
 * Return value: %True or %False.
 **/

int block_control_near_pt(const NspBlock *B,const double pt[2], int *cp)
{
  double d = Max(Abs( B->obj->r[0]+B->obj->r[2] -pt[0]),Abs( B->obj->r[1]-B->obj->r[3] -pt[1])) ;
  if ( d < lock_size ) 
    { 
      *cp = 0 ;
      return TRUE;
    }
  return FALSE;
}

/**
 * block_lock_near_pt:
 * @B: a block 
 * @pt: a point position 
 * @cp: the control point id in case of success.
 * 
 * Checks if the given point is near a block lock point. 
 * If %True the given point is changed so as to contains 
 * the lock point coordinates and @cp is filled with the control point id. 
 * 
 * Return value: %True or %False.
 **/

int block_lock_near_pt(const NspBlock *B,double pt[2], int *cp)
{
  int i;
  for ( i=0 ; i < B->obj->n_locks ; i++ ) 
    {
      double d= Max(Abs( B->obj->locks[i].pt[0] -pt[0]),Abs( B->obj->locks[i].pt[1] -pt[1])) ;
      if ( d < lock_size ) 
	{ 
	  *cp = i;
	  pt[0]=B->obj->locks[i].pt[0];
	  pt[1]=B->obj->locks[i].pt[1];
	  return TRUE;
	}
    }
  return FALSE;
}

/**
 * block_move_control_init:
 * @B: a block 
 * @ct: a control point id
 * @pts: point coordinates 
 * 
 * Used to init a control point interactive move. 
 * This function is empty for Blocks. 
 **/

void block_move_control_init( NspBlock *B,int cp,double ptc[2])
{
}


/**
 * block_move_control:
 * @F: a graphic frame 
 * @B: a block 
 * mpt: point coordinates 
 * @ct: a control point id
 * @ptc: point coordinates 
 * 
 * Updates the block structure when a control point (there's just one control point 
 * for blocks at down right corner) is moved.
 * @mpt gives the mouse position where the control point is to be moved.translation vector which is to be applied to the control point.
 **/

void block_move_control(NspGFrame *F, NspBlock *B,const double mpt[2], int cp,double ptc[2])
{
  ptc[0]  =  Max(  mpt[0] - B->obj->r[0] ,0);
  ptc[1]  =  Max(  B->obj->r[1] -mpt[1] ,0);
  block_resize(B,ptc);
  /* return a copy of mpt */
  ptc[0]=mpt[0];
  ptc[1]=mpt[1];
}

/**
 * block_get_number_of_locks: 
 * @B: a block 
 * 
 * Returns the number of lock points of the block 
 * 
 * Return value: the number of lock points
 **/

int block_get_number_of_locks(const NspBlock *B) 
{
  return B->obj->n_locks;
}

/**
 * block_get_number_of_ports: 
 * @B: a block 
 * @lp: a lock point 
 * 
 * Returns the number of ports of lock points lp;
 * 
 * Return value: the number of ports
 **/

int block_get_number_of_ports(const NspBlock *B,int lp) 
{
  return 1;
}

/**
 * block_get_lock_connection: 
 * @B: a block 
 * @i: a lock point id. 
 * @port: a port of the lock point @i;
 * 
 * Returns in a gr_port structure information about the object 
 * connected to the port @port of lock point @i. 
 * 
 * Return value: #OK if lock point and port number exists or #FAIL
 **/

int block_get_lock_connection(const NspBlock *B,int i,int port, gr_port *p )
{
  if ( i >= 0 && i < B->obj->n_locks && port == 0  ) 
    {
      *p = B->obj->locks[i].port;
      return OK;
    }
  return FAIL;
}

/**
 * block_get_lock_pos:
 * @B: a block 
 * @i: a lock point id. 
 * @pt: point coordinates.
 *
 * @pt is filled with  the position of lock point @i. 
 **/

void block_get_lock_pos(const NspBlock *B, int i,double pt[])
{
  if ( i >=  0 && i < B->obj->n_locks )
    {
      pt[0]= B->obj->locks[i].pt[0];
      pt[1]= B->obj->locks[i].pt[1];
    }
}

/**
 * block_get_lock_dir:
 * @B: a #NspBlock 
 * @i: a lock point id. 
 * 
 * returns the lock direction of selected lock point.
 * 
 * Return value: a #lock_dir 
 **/

lock_dir block_get_lock_dir(const NspBlock *B, int i)
{
  if ( i >=  0 && i < B->obj->n_locks )
    {
      return B->obj->locks[i].type & LOCK_DIR_FLAG ;
    }
  return LD_ANY;
}



/**
 * block_set_lock_connection: 
 * @B: a block 
 * @i: a lock point id. 
 * @prt: a port id (0 for block).
 * @p: information to be connected to one port of lock point i;
 * 
 * the port described by @p is connected to a port of lock point i; 
 * return value: -2 or -1 or the port number used for connection. 
 **/

int block_set_lock_connection(NspBlock *B,int i,int prt,const gr_port *p)
{
  if ( i >=  0  && i < B->obj->n_locks ) 
    {
      gr_port *port= &B->obj->locks[i].port;
      if ( port->object_id != NULL) return -2;
      *port = *p;
      return 0;
    }
  return -1;
}

/**
 * block_unset_lock_connection: 
 * @B: a block 
 * @i: a lock point id. 
 * @prt: a lock point port. 
 * 
 * unconect the object locked to port @port of lock point @i.
 * 
 **/

void block_unset_lock_connection(NspBlock *B,int i,int port)
{
  if ( i >= 0 && i < B->obj->n_locks && port == 0)
    {
      B->obj->locks[i].port.object_id = NULL;
    }
}

/**
 * block_is_lock_connectable: 
 * @B: a block 
 * @i: a lock point id. 
 * 
 * Checks if there's a free port in lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int block_is_lock_connectable(NspBlock *B,int i)
{
  if ( i >=  0 && i < B->obj->n_locks )
    {
      if ( B->obj->locks[i].port.object_id == NULL) return TRUE; 
    }
  return FALSE;
}

/**
 * block_is_lock_connected 
 * @B: a block 
 * @i: a lock point id. 
 * 
 * Checks if there's a locked port for lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int block_is_lock_connected(NspBlock *B,int i)
{
  if ( i >=  0  && i < B->obj->n_locks ) 
    {
      if ( B->obj->locks[i].port.object_id != NULL) return TRUE; 
    }
  return FALSE;
}

/**
 * block_set_lock_pos: 
 * @B: a block 
 * @i: a lock point id. 
 * @pt: a point coordinates 
 * 
 * Sets the lock point @i position to @pt. 
 * XXXX : not supposed to call this function since 
 *        relative positions should be moved !!
 *        But it is maybe only called for links.
 **/

static void block_set_lock_pos(NspBlock *B, int i,const double pt[],int keep_angle,lock_dir dir)
{
  if ( i >= 0 && i < B->obj->n_locks )
    {
      B->obj->locks[i].pt[0] = pt[0];
      B->obj->locks[i].pt[1] = pt[1];
    }
}

/**
 * block_set_lock_pos_rel: 
 * @B: a block 
 * @i: a lock point id. 
 * @pt: a point coordinates 
 * 
 * Sets the lock point @i relative position (i.e in % of the 
 * block rect) to @pt. The absolute value is updated accordingly.
 **/

static void block_set_lock_pos_rel(NspBlock *B, int i,const double pt[])
{
  if ( i >= 0 && i < B->obj->n_locks )
    {
      B->obj->locks[i].ptr[0] = pt[0];
      B->obj->locks[i].ptr[1] = pt[1];
      block_set_lock_pos_from_rel(B,i);
    }
}

/* utility
 *
 */

static void block_set_lock_pos_from_rel(NspBlock *B, int i)
{
  const double xoffset[]={0,0,lock_size,-lock_size,0};
  const double yoffset[]={lock_size,-lock_size,0,0,0};
  if ( i >= 0 && i < B->obj->n_locks )
    {
      int dir = B->obj->locks[i].type & LOCK_DIR_FLAG ;
      double xof = xoffset[dir];
      double yof = yoffset[dir];
      B->obj->locks[i].pt[0]=B->obj->r[0]+B->obj->r[2]*B->obj->locks[i].ptr[0]+xof; 
      B->obj->locks[i].pt[1]=B->obj->r[1]-B->obj->r[3]*B->obj->locks[i].ptr[1]+yof;
    }
}



/**
 * block_set_locks:
 * @B: a #NspBlock 
 * @Pt: 3xn  #NspMatrix
 * 
 * uses the @Pt matrix as the definition of the lock points 
 * of the block @B. Each lock point is defined by a column of 
 * @Pt which gives the lock type and its (x,y) position defined 
 * by its relative position inside the block enclosing rectangle. 
 * x and y are in the range (0,1). (0,0) is the upper left point 
 * and (0,1) the lower right point. 
 *
 * Return value: %OK or %FAIL
 **/

static int block_set_locks(NspBlock *B,NspMatrix *Pt)
{
  void *lock;
  int i;
  /* first unlock the locks */
  for ( i = 0 ; i <  B->obj->n_locks ;i++)
    {
      block_unlock(B,i);
    }
  /* now create new lock */
  B->obj->n_locks = Pt->n ;
  
  if (( lock  = malloc(B->obj->n_locks*sizeof(grb_lock))) == NULL ) 
    {
      Scierror("Error: no more memory\n");
      return FAIL;
    }
  B->obj->locks = lock;
  for (i=0; i < B->obj->n_locks ; i++) 
    {
      B->obj->locks[i].port.object_id = NULL; 
      B->obj->locks[i].type = (int) *(Pt->R+2*i+2);
      block_set_lock_pos_rel(B,i,Pt->R+2*i);
    }
  return OK;
}


/**
 * block_unlock:
 * @L: a #NspBlock 
 * @lp: an integer
 * 
 * unlock the lock point of the block with id @lp.
 **/

static void block_unlock( NspBlock *B,int lp) 
{
  NspObject *O1;
  gr_port p; 
  /* just test if unlock is necessary */
  if ( block_is_lock_connected(B,lp)==FALSE ) return; 
  if ( block_get_lock_connection(B,lp,0,&p)==FAIL) return;
  /* we are locked to a block, unlock it */
  O1 = p.object_id;
  if ( O1 != NULLOBJ ) 
    {
      /* propagate unlock to the locked object */
      GR_INT(O1->basetype->interface)->unset_lock_connection(O1,p.lock,p.port);
    }
  /* unset the lock on the block */
  block_unset_lock_connection(B,lp,0);
}

/**
 * block_set_frame:
 * @Gf: a #NspGFrame 
 * 
 * attach the block frame reference to @GF
 **/

static void block_set_frame( NspBlock *B, NspGFrame *Gf)
{
  B->obj->frame = Gf->obj;
}


/**
 * block_full_copy:
 * @B: a #NspBlock 
 * 
 * returns a full copy of a a #NspBlock object. 
 * 
 * Returns: a new #NspBlock  or %NULLBLOCK
 **/

static NspBlock * block_full_copy( NspBlock *B)
{
  int i;
  NspBlock *M=NULLBLOCK;
  if (( M = nsp_block_create(NVOID,NULL,NULL,B->obj->r,B->obj->color,B->obj->thickness,B->obj->background,0,NULL,FALSE,TRUE,NULL))
      == NULLBLOCK) return NULLBLOCK;
  /* keep old address */
  M->obj->object_sid = B;
  /* the lock points */
  if ( M->obj->locks != NULL) FREE(M->obj->locks);
  M->obj->n_locks =   B->obj->n_locks;
  if (( M->obj->locks = malloc(M->obj->n_locks*sizeof(grb_lock))) == NULL ) return NULLBLOCK;
  for ( i = 0 ; i < M->obj->n_locks ; i++) 
    {
      M->obj->locks[i]= B->obj->locks[i];
      M->obj->locks[i].port.object_id = NULLOBJ;
      M->obj->locks[i].port.object_sid = B->obj->locks[i].port.object_id;
    }
  return M;
}

