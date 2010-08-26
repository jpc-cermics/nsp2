/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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





#line 90 "codegen/connector.override"

#line 30 "connector.c"

/* ----------- NspConnector ----------- */


#define  NspConnector_Private 
#include <nsp/objects.h>
#include <nsp/connector.h>
#include <nsp/interf.h>

/* 
 * NspConnector inherits from Graphic 
 * and implements  GRint
 */

int nsp_type_connector_id=0;
NspTypeConnector *nsp_type_connector=NULL;

/*
 * Type object for NspConnector 
 * all the instance of NspTypeConnector share the same id. 
 * nsp_type_connector: is an instance of NspTypeConnector 
 *    used for objects of NspConnector type (i.e built with new_connector) 
 * other instances are used for derived classes 
 */
NspTypeConnector *new_type_connector(type_mode mode)
{
  NspTypeGRint *t_grint;
  NspTypeConnector *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_connector != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_connector;
    }
  if (( type =  malloc(sizeof(NspTypeConnector))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = connector_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = connector_get_methods;
  type->new = (new_func *) new_connector;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for connector */ 

  top->pr = (print_func *) nsp_connector_print;
  top->dealloc = (dealloc_func *) nsp_connector_destroy;
  top->copy  =  (copy_func *) nsp_connector_copy;
  top->size  = (size_func *) nsp_connector_size;
  top->s_type =  (s_type_func *) nsp_connector_type_as_string;
  top->sh_type = (sh_type_func *) nsp_connector_type_short_string;
  top->info = (info_func *) nsp_connector_info;
  /* top->is_true = (is_true_func  *) nsp_connector_is_true; */
  /* top->loop =(loop_func *) nsp_connector_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_connector_object;
  top->eq  = (eq_func *) nsp_connector_eq;
  top->neq  = (eq_func *) nsp_connector_neq;
  top->save  = (save_func *) nsp_connector_xdr_save;
  top->load  = (load_func *) nsp_connector_xdr_load;
  top->create = (create_func*) int_connector_create;
  top->latex = (print_func *) nsp_connector_latex;
  top->full_copy = (copy_func *) nsp_connector_full_copy;

  /* specific methods for connector */

  type->init = (init_func *) init_connector;

#line 98 "codegen/connector.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_connector;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_connector ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_connector  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_connector  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_connector  ;
  /* next method are defined in NspGraphic and need not be changed here for Connector */
  /* ((NspTypeNspGraphic *) type->surtype)->connector_figure = nsp_graphic_connector_figure; */ 
  /* ((NspTypeNspGraphic *) type->surtype)->unconnector_figure = nsp_graphic_unconnector_figure; */ 

#line 115 "connector.c"
  /* 
   * NspConnector interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_grint = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) t_grint;
#line 110 "codegen/connector.override"

  t_grint->get_hilited 	=(gr_get_hilited *) connector_get_hilited;
  t_grint->set_hilited 	=(gr_set_hilited *) connector_set_hilited;
  t_grint->get_show    	=(gr_get_show *) connector_get_show;
  t_grint->set_show		=(gr_set_show *) connector_set_show;
  t_grint->set_pos  	=(gr_set_pos *) connector_set_pos;
  t_grint->get_pos  	=(gr_get_pos *) connector_get_pos;
  t_grint->resize 		=(gr_resize *) connector_resize;
  t_grint->update_locks 	=(gr_update_locks *) connector_update_locks;
  t_grint->contains_pt 	=(gr_contains_pt *) connector_contains_pt;
  t_grint->control_near_pt 	=(gr_control_near_pt *) connector_control_near_pt;
  t_grint->lock_near_pt 	=(gr_lock_near_pt *) connector_lock_near_pt;
  t_grint->move_control_init=(gr_move_control_init *) connector_move_control_init;
  t_grint->move_control 	=(gr_move_control *) connector_move_control;

  t_grint->get_number_of_locks =(gr_get_number_of_locks *) connector_get_number_of_locks;
  t_grint->get_number_of_ports =(gr_get_number_of_ports *) connector_get_number_of_ports;
  t_grint->get_lock_connection =(gr_get_lock_connection *) connector_get_lock_connection;
  t_grint->get_lock_pos =(gr_get_lock_pos *) connector_get_lock_pos;
  t_grint->get_lock_dir =(gr_get_lock_dir *) connector_get_lock_dir;
  t_grint->set_lock_connection =(gr_set_lock_connection *) connector_set_lock_connection;
  t_grint->unset_lock_connection =(gr_unset_lock_connection *) connector_unset_lock_connection;
  t_grint->is_lock_connectable =(gr_is_lock_connectable *) connector_is_lock_connectable;
  t_grint->is_lock_connected =(gr_is_lock_connected *) connector_is_lock_connected;
  t_grint->set_lock_pos =(gr_set_lock_pos *) connector_set_lock_pos;
  t_grint->unlock =(gr_unlock *) connector_unlock;

#line 152 "connector.c"
  if ( nsp_type_connector_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeConnector called nsp_type_connector
       */
      type->id =  nsp_type_connector_id = nsp_new_type_id();
      nsp_type_connector = type;
      if ( nsp_register_type(nsp_type_connector) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_connector(mode);
    }
  else 
    {
      type->id = nsp_type_connector_id;
      return type;
    }
}

/*
 * initialize NspConnector instances 
 * locally and by calling initializer on parent class 
 */

static int init_connector(NspConnector *Obj,NspTypeConnector *type)
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
 * new instance of NspConnector 
 */

NspConnector *new_connector() 
{
  NspConnector *loc;
  /* type must exists */
  nsp_type_connector = new_type_connector(T_BASE);
  if ( (loc = malloc(sizeof(NspConnector)))== NULLCONNECTOR) return loc;
  /* initialize object */
  if ( init_connector(loc,nsp_type_connector) == FAIL) return NULLCONNECTOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspConnector 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_connector_size(NspConnector *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char connector_type_name[]="Connector";
static char connector_short_type_name[]="connector";

static char *nsp_connector_type_as_string(void)
{
  return(connector_type_name);
}

static char *nsp_connector_type_short_string(NspObject *v)
{
  return(connector_short_type_name);
}

/*
 * A == B 
 */

static int nsp_connector_eq(NspConnector *A, NspObject *B)
{
  NspConnector *loc = (NspConnector *) B;
  if ( check_cast(B,nsp_type_connector_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->object_sid != loc->obj->object_sid) return FALSE;
  {
    int i;
    for ( i = 0 ; i < 4 ; i++ )
      if ( A->obj->r[i] != loc->obj->r[i] ) return FALSE;
  }
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( A->obj->background != loc->obj->background) return FALSE;
  if ( nsp_eq_gr_lock(&A->obj->lock,&loc->obj->lock)== FALSE) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_connector_neq(NspConnector *A, NspObject *B)
{
  return ( nsp_connector_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

#line 139 "codegen/connector.override"

/*
 * save 
 */

int nsp_connector_xdr_save(XDR *xdrs, NspConnector *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_connector)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(M)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->background) == FAIL) return FAIL;
  if (nsp_save_gr_lock(xdrs,&M->obj->lock,M) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspConnector  *nsp_connector_xdr_load_partial(XDR *xdrs, NspConnector *M)
{
  int id,fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  NULLCONNECTOR;
  M->obj->object_sid = NSP_INT_TO_POINTER(id);
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->background) == FAIL) return NULL;
  if (nsp_load_gr_lock(xdrs,&M->obj->lock,M) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspConnector  *nsp_connector_xdr_load(XDR *xdrs)
{
  NspConnector *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCONNECTOR;
  if ((H  = nsp_connector_create_void(name,(NspTypeBase *) nsp_type_connector))== NULLCONNECTOR) return H;
  if ( nsp_connector_create_partial(H) == FAIL) return NULLCONNECTOR;
  if ((H  = nsp_connector_xdr_load_partial(xdrs,H))== NULLCONNECTOR) return H;
  if ( nsp_connector_check_values(H) == FAIL) return NULLCONNECTOR;
  return H;
}

#line 325 "connector.c"
/*
 * delete 
 */

void nsp_connector_destroy_partial(NspConnector *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_destroy_gr_lock(&H->obj->lock,H); 
    FREE(H->obj);
   }
}

void nsp_connector_destroy(NspConnector *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_connector_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_connector_info(NspConnector *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCONNECTOR) 
    {
      Sciprintf("Null Pointer NspConnector \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_connector_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_connector_print(NspConnector *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCONNECTOR) 
    {
      Sciprintf("Null Pointer NspConnector \n");
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
          nsp_connector_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_connector_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"object_sid=%xl\n",M->obj->object_sid);
  if ( nsp_print_array_double(indent+2,"r",M->obj->r,4,rec_level) == FALSE ) return FALSE ;
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"background=%d\n",M->obj->background);
  nsp_print_gr_lock(indent+2,&M->obj->lock,M);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_connector_latex(NspConnector *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_connector_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"object_sid=%xl\n",M->obj->object_sid);
  if ( nsp_print_latex_array_double(indent+2,"r",M->obj->r,4,rec_level) == FALSE ) return FALSE ;
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"background=%d\n",M->obj->background);
  nsp_print_gr_lock(indent+2,&M->obj->lock,M);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspConnector objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspConnector   *nsp_connector_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_connector_id) == TRUE ) return ((NspConnector *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_connector));
  return NULL;
}

int IsConnectorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_connector_id);
}

int IsConnector(NspObject *O)
{
  return nsp_object_type(O,nsp_type_connector_id);
}

NspConnector  *GetConnectorCopy(Stack stack, int i)
{
  if (  GetConnector(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspConnector  *GetConnector(Stack stack, int i)
{
  NspConnector *M;
  if (( M = nsp_connector_object(NthObj(i))) == NULLCONNECTOR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspConnector instance 
 *-----------------------------------------------------*/
#line 199 "codegen/connector.override"
/* override the code for connector creation */


static NspConnector *nsp_connector_create_void(char *name,NspTypeBase *type)
{
 NspConnector *H  = (type == NULL) ? new_connector() : type->new();
 if ( H ==  NULLCONNECTOR)
  {
   Sciprintf("No more memory\n");
   return NULLCONNECTOR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCONNECTOR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_connector_create_partial(NspConnector *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_connector)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->object_sid = NULL;
  {
    double x_def[4]={0,0,0,0};
    memcpy(H->obj->r,x_def,4*sizeof(double));
  }
  H->obj->color = 0;
  H->obj->thickness = 0;
  H->obj->background = 0;
  nsp_init_gr_lock(&H->obj->lock);
  return OK;
}

int nsp_connector_check_values(NspConnector *H)
{
  if ( nsp_check_gr_lock(&H->obj->lock,H) == FAIL ) return FAIL;
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspConnector *nsp_connector_create(char *name,void* object_sid,double* r,int color,int thickness,int background,gr_lock lock,NspTypeBase *type)
{
 NspConnector *H  = nsp_connector_create_void(name,type);
 if ( H ==  NULLCONNECTOR) return NULLCONNECTOR;
  if ( nsp_connector_create_partial(H) == FAIL) return NULLCONNECTOR;
  H->obj->object_sid = object_sid;
  memcpy(H->obj->r,r,4*sizeof(double));
  H->obj->color=color;
  H->obj->thickness=thickness;
  H->obj->background=background;
  /* H->obj->lock = lock; */
  connector_update_locks(H);
 if ( nsp_connector_check_values(H) == FAIL) return NULLCONNECTOR;
 return H;
}


NspConnector *nsp_connector_create_default(char *name)
{
 NspConnector *H  = nsp_connector_create_void(name,NULL);
 if ( H ==  NULLCONNECTOR) return NULLCONNECTOR;
  if ( nsp_connector_create_partial(H) == FAIL) return NULLCONNECTOR;
 if ( nsp_connector_check_values(H) == FAIL) return NULLCONNECTOR;
 return H;
}

#line 537 "connector.c"
/*
 * copy for gobject derived class  
 */

NspConnector *nsp_connector_copy_partial(NspConnector *H,NspConnector *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspConnector *nsp_connector_copy(NspConnector *self)
{
  NspConnector *H  =nsp_connector_create_void(NVOID,(NspTypeBase *) nsp_type_connector);
  if ( H ==  NULLCONNECTOR) return NULLCONNECTOR;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONNECTOR;
  if ( nsp_connector_copy_partial(H,self)== NULL) return NULLCONNECTOR;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspConnector *nsp_connector_full_copy_partial(NspConnector *H,NspConnector *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_connector))) == NULL) return NULLCONNECTOR;
  H->obj->ref_count=1;
  H->obj->object_sid = self->obj->object_sid;
  memcpy(H->obj->r,self->obj->r,4*sizeof(double));
  H->obj->color=self->obj->color;
  H->obj->thickness=self->obj->thickness;
  H->obj->background=self->obj->background;
  if( nsp_gr_lock_full_copy(H,&H->obj->lock,self)== FAIL) return NULL;
  return H;
}

NspConnector *nsp_connector_full_copy(NspConnector *self)
{
  NspConnector *H  =nsp_connector_create_void(NVOID,(NspTypeBase *) nsp_type_connector);
  if ( H ==  NULLCONNECTOR) return NULLCONNECTOR;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONNECTOR;
  if ( nsp_connector_full_copy_partial(H,self)== NULL) return NULLCONNECTOR;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspConnector
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 271 "codegen/connector.override"

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);

int int_connector_create(Stack stack, int rhs, int opt, int lhs)
{
  gr_lock l;
  NspConnector *H;
  double *val=NULL;
  int back=-1,color=-1,thickness=-1;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = nsp_connector_create(NVOID,NULL,val,color,thickness,back,l,NULL)) == NULLCONNECTOR) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

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




#line 642 "connector.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 355 "codegen/connector.override"

/* translate */

static int _wrap_connector_translate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  nsp_translate_connector(self,M->R);
  MoveObj(stack,1,self);
  return 1;

}

#line 663 "connector.c"


#line 373 "codegen/connector.override"
/* set_position */

static int _wrap_connector_set_pos(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  connector_set_pos(self,M->R);
  MoveObj(stack,1,self);
  return 1;

}

#line 682 "connector.c"


#line 390 "codegen/connector.override"
/* resize */ 

static int _wrap_connector_resize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,2);
  connector_resize(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

#line 700 "connector.c"


static NspMethods connector_methods[] = {
  {"translate",(nsp_method *) _wrap_connector_translate},
  {"set_pos",(nsp_method *) _wrap_connector_set_pos},
  {"resize",(nsp_method *) _wrap_connector_resize},
  { NULL, NULL}
};

static NspMethods *connector_get_methods(void) { return connector_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_connector_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspConnector *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_connector_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspConnector *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_connector_get_thickness(void *self,const char *attr)
{
  int ret;

  ret = ((NspConnector *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_connector_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspConnector *) self)->obj->thickness= thickness;
  return OK;
}

static NspObject *_wrap_connector_get_background(void *self,const char *attr)
{
  int ret;

  ret = ((NspConnector *) self)->obj->background;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_connector_set_background(void *self,const char *attr, NspObject *O)
{
  int background;

  if ( IntScalar(O,&background) == FAIL) return FAIL;
  ((NspConnector *) self)->obj->background= background;
  return OK;
}

static AttrTab connector_attrs[] = {
  { "color", (attr_get_function *)_wrap_connector_get_color, (attr_set_function *)_wrap_connector_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_connector_get_thickness, (attr_set_function *)_wrap_connector_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "background", (attr_get_function *)_wrap_connector_get_background, (attr_set_function *)_wrap_connector_set_background,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Connector_func[]={
  { "connector_create", int_connector_create},
  { NULL, NULL}
};

/* call ith function in the Connector interface */

int Connector_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Connector_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Connector_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Connector_func[i].name;
  *f = Connector_func[i].fonc;
}

#line 425 "codegen/connector.override"

/* methods for the graphic class 
 *
 *
 */

#define CIRCLE 0

static void nsp_draw_connector(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  NspConnector *B = (NspConnector *) Obj;
  double loc[6];
  int cpat, cwidth,locked,lockid=0;
  /* only draw block which are in a frame */
  if ( Obj->obj->show == FALSE ) return ;

  /* check if the block is inside drawing rectangle
   */

  if ( ! nsp_graphic_intersect_rectangle((NspGraphic *) B, rect))
    {
      return ;
    }

  /* Xgc=B->obj->frame->Xgc; */
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);

  locked =  connector_is_lock_connected(B,lockid);

  if ( Obj->obj->hilited == TRUE || locked == FALSE ) 
    {
      /* draw frame rectangle if hilited or non connected connector */
      Xgc->graphic_engine->xset_pattern(Xgc,B->obj->color);
      Xgc->graphic_engine->scale->drawrectangle(Xgc,B->obj->r);
    }
  if ( Obj->obj->hilited == TRUE ) 
    {
      /* draw control points when hilited */ 
      loc[0]=B->obj->r[0]; loc[1]=B->obj->r[1];loc[2]=loc[3]= lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
      loc[0]+= B->obj->r[2] -lock_size; loc[1] -= B->obj->r[3] -lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
    }
  if ( CIRCLE )
    {
      /* test file */
      int i;
      for ( i = 0; i < 4 ;i++) loc[i]=B->obj->r[i];
      loc[4]=0;loc[5]=360*64;
      Xgc->graphic_engine->scale->fillarc(Xgc,loc);
      Xgc->graphic_engine->scale->drawarc(Xgc,loc);
    }
  /* draw the lock point when connected 
   */
  if ( locked == TRUE)
    {
      Xgc->graphic_engine->xset_pattern(Xgc,lock_color); 
    }
  else 
    {
      Xgc->graphic_engine->xset_pattern(Xgc,1); 
    }
  connector_get_lock_pos(B,lockid,loc);
  loc[0] -= lock_size/2; loc[1] += lock_size/2;loc[2]=loc[3]= lock_size;
  if ( CIRCLE ) 
    {
      if ( locked ) 
	Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
      Xgc->graphic_engine->scale->drawrectangle(Xgc,loc);
    }
  else
    {
      loc[4]=0;loc[5]=360*64;
      if ( locked ) 
	Xgc->graphic_engine->scale->fillarc(Xgc,loc);
      Xgc->graphic_engine->scale->drawarc(Xgc,loc);
    }
  /* back to default */
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}


/**
 * connector_tranlate:
 * @B: a connector 
 * @pt: 
 * 
 * Tranlates the connector origin (upper left point) using the 
 * value of vector @pt. 
 *
 **/

static void nsp_translate_connector(NspGraphic *Obj,const double *tr)
{
  NspConnector *B = (NspConnector *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  B->obj->r[0] += tr[0] ;
  B->obj->r[1] += tr[1] ;
  connector_update_locks(B);
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_connector(NspGraphic *Obj,double *R)
{
  
}

static void nsp_scale_connector(NspGraphic *Obj,double *alpha)
{
  /*   NspBlock *P = (NspBlock *) Obj; */
}

/* compute in bounds the enclosing rectangle of connector 
 *
 */

static int nsp_getbounds_connector (NspGraphic *Obj,double *bounds)
{
  NspConnector *B = (NspConnector *) Obj;
  bounds[0]=B->obj->r[0];/* xmin */
  bounds[1]=B->obj->r[1]-B->obj->r[3];/* ymin */
  bounds[2]=B->obj->r[0]+B->obj->r[2];/* xmax */
  bounds[3]=B->obj->r[1];/* ymax */
  return TRUE;
}

/*
 * implementation of the GRint interface 
 * for a connector 
 */ 

/**
 * connector_get_hilited:
 * @B: a connector 
 *
 * Returns: the value of the hilited attribute of object @B.
 *
 **/

int connector_get_hilited(NspConnector *B) {  return ((NspGraphic *)B)->obj->hilited; } 

/**
 * connector_set_hilited:
 * @B: a connector 
 * @val: %True or %False. 
 * 
 * Sets the hilited status of the connector @B.
 *
 **/

void connector_set_hilited(NspConnector *B,int val) 
{
  NspGraphic *G = (NspGraphic *)B;
  if ( G->obj->hilited == val) return;
  G->obj->hilited = val; 
  nsp_graphic_invalidate(G);
}


/**
 * connector_get_show:
 * @B: a connector 
 * @val:  %True or %False. 
 * 
 * Returns: the value of the show attribute of object @B.
 *
 **/

int connector_get_show(NspConnector *B) {  return ((NspGraphic *)B)->obj->show; } 

/**
 * connector_set_show:
 * @B: a connector 
 *
 * Sets the show status of the given Connector.
 *
 **/

void connector_set_show(NspConnector *B,int val) {  ((NspGraphic *)B)->obj->show = val; } 


int connector_set_pos(NspConnector *B,const double pt[2])
{
  B->obj->r[0] = pt[0] ;
  B->obj->r[1] = pt[1] ;
  connector_update_locks(B);
  return OK;
}

void connector_get_pos(NspConnector *B, double pt[2])
{
  pt[0]= B->obj->r[0]; 
  pt[1]= B->obj->r[1];
}


/**
 * connector_resize: 
 * @B: a connector 
 * @size: new width and height of the connector given in an array of double.
 * 
 * Resize the connector using the value of vector @size. 
 *
 **/

void connector_resize(NspConnector *B,const double size[2])
{
  nsp_graphic_invalidate((NspGraphic *) B);
  B->obj->r[2] = Max(size[0],3*lock_size) ;
  B->obj->r[3] = Max(size[1],3*lock_size) ;
  connector_update_locks(B);
  nsp_graphic_invalidate((NspGraphic *) B);
}


/**
 * connector_update_locks:
 * @B: a connector 
 * 
 * Recomputes the positions of the lock points position. 
 *
 **/

void connector_update_locks(NspConnector *B)
{
  B->obj->lock.pt[0]=B->obj->r[0]+B->obj->r[2]/2; 
  B->obj->lock.pt[1]=B->obj->r[1]-B->obj->r[3]/2;
}

/**
 * connector_contains_pt
 * @B: a connector 
 * @pt: a point position 
 * 
 * Checks if the given point in inside the connector rectangle.
 * If the point is inside the lock point of connector and 
 * the connector is connected then %FALSE is returned
 * (Because we want to be able to select a link lock point connected 
 *  to the connector lock point) 
 *
 * Return value: %True or %False.
 **/

int connector_contains_pt(const NspConnector *B,const double pt[2])
{
  int rep = B->obj->r[0] <= pt[0] && B->obj->r[1] >= pt[1] 
    && B->obj->r[0]+B->obj->r[2] >= pt[0] && B->obj->r[1]- B->obj->r[3] <= pt[1];
  if (rep == TRUE &&  connector_is_lock_connected(B,0))
    {
      double d= Max(Abs( B->obj->lock.pt[0] -pt[0]),Abs( B->obj->lock.pt[1] -pt[1])) ;
      if ( d < lock_size/2 ) 
	return FALSE;
    }
  return rep;
}



/**
 * connector_control_near_pt:
 * @B: a connector 
 * @pt: a point position 
 * @cp: the control point id in case of success.
 * 
 * Checks if the given point is near a connector control point. 
 * 
 * Return value: %True or %False.
 **/

int connector_control_near_pt(const NspConnector *B,const double pt[2], int *cp)
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
 * connector_lock_near_pt:
 * @B: a connector 
 * @pt: a point position 
 * @cp: the control point id in case of success.
 * 
 * Checks if the given point is near a connector lock point. 
 * If %True the given point is changed so as to contains 
 * the lock point coordinates and @cp is filled with the control point id. 
 * 
 * Return value: %True or %False.
 **/

int connector_lock_near_pt(const NspConnector *B, double pt[2], int *cp)
{
  int i=0;
  double d = Max(Abs( B->obj->lock.pt[0] -pt[0]),Abs( B->obj->lock.pt[1] -pt[1])) ;
  if ( d < lock_size ) 
    { 
      *cp = i;
      pt[0]=B->obj->lock.pt[0];
      pt[1]=B->obj->lock.pt[1];
      return TRUE;
    }
  return FALSE;
}

/**
 * connector_move_control_init:
 * @B: a connector 
 * @ct: a control point id
 * @pts: point coordinates 
 * 
 * Used to init a control point interactive move. 
 * This function is empty for Connectors. 
 **/

void connector_move_control_init( NspConnector *B,int cp,double ptc[2])
{
}

/**
 * connector_move_control:
 * @F: a graphic frame 
 * @B: a connector 
 * pt: point coordinates 
 * @ct: a control point id
 * @ptc: point coordinates 
 * 
 * Updates the connector structure when a control point (there's just one control point 
 * for connectors) is moved. 
 **/

void connector_move_control(void *F, NspConnector *B,const double mpt[2], int cp,double ptc[2])
{
  ptc[0]  =  Max(  mpt[0] - B->obj->r[0] ,0);
  ptc[1]  =  Max(  B->obj->r[1] -mpt[1] ,0);
  connector_resize(B,ptc);
  /* return a copy of mpt */
  ptc[0]=mpt[0];
  ptc[1]=mpt[1];
}

/**
 * connector_get_number_of_locks: 
 * @B: a connector 
 * 
 * Returns the number of lock points of the connector 
 * 
 * Return value: the number of lock points
 **/

int connector_get_number_of_locks(const NspConnector *B) 
{
  return 1;
}

/**
 * connector_get_number_of_ports: 
 * @B: a connector 
 * @lp: a lock point 
 * 
 * Returns the number of ports of lock points lp;
 * 
 * Return value: the number of ports
 **/

int connector_get_number_of_ports(const NspConnector *B,int lp) 
{
  return  B->obj->lock.n_ports; 
}

/**
 * connector_get_lock_connection: 
 * @B: a connector 
 * @i: a lock point id. 
 * @port: a port of the lock point @i;
 * 
 * Returns in a gr_port structure information about the object 
 * connected to the port @port of lock point @i. 
 * 
 * Return value: #OK if lock point and port number exists or #FAIL.
 **/

int connector_get_lock_connection(const NspConnector *B,int i,int port, gr_port *p )
{
  int np = B->obj->lock.n_ports; 
  if ( i == 0 && port >= 0 && port < np ) 
    {
      *p = B->obj->lock.ports[port];
      return OK;
    }
  return FAIL;
}

/**
 * connector_get_lock_pos:
 * @B: a connector 
 * @i: a lock point id. 
 * @pt: point coordinates.
 *
 * @pt is filled with the position of lock point @i. 
 **/

void connector_get_lock_pos(const NspConnector *B,int i,double pt[])
{
  if ( i ==  0 ) 
    {
      pt[0]= B->obj->lock.pt[0];
      pt[1]= B->obj->lock.pt[1];
    }
}

/**
 * connector_get_lock_dir:
 * @B: 
 * @i: 
 * 
 * Return value: 
 **/

lock_dir connector_get_lock_dir(const NspConnector *L, int i)
{
  return LD_ANY;
}

/**
 * connector_set_lock_connection: 
 * @B: a connector 
 * @i: a lock point id. 
 * @p: information to be connected to one port of lock point i;
 * 
 * the port described by @p is connected to a port @prt of lock point @i 
 * (a connector just has one lock point). If @prt is equal to -1 a free 
 * port is checked else the port @prt is used.
 * return value: -1 or the port number used for connection.
 **/

int connector_set_lock_connection(NspConnector *B,int i,int prt,const gr_port *p)
{
  int port_number=0,j;
  gr_port *port= NULL;
  if ( i !=  0 ) return -1;
  if ( prt != -1 ) 
    {
      /* using an existing port */
      if ( prt >= 0 && prt < B->obj->lock.n_ports )
	{
	  port= &B->obj->lock.ports[prt]; 
	  port_number = prt;
	}
      else 
	{
	  return -1;
	}
    }
  else 
    {
      for ( j= 0 ; j < B->obj->lock.n_ports ; j++) 
	{
	  if ( B->obj->lock.ports[j].object_id == NULL) 
	    {
	      port= &B->obj->lock.ports[j]; 
	      port_number = j;
	      break;
	    }
	}
      if ( port == NULL ) 
	{
	  if ( B->obj->lock.fixed == FALSE ) 
	    {
	      gr_port *loc= NULL;
	      if ( B->obj->lock.ports == NULL ) 
		{
		  if ((loc = malloc((B->obj->lock.n_ports+1)*sizeof(gr_port)))==NULL)
		    return -1;
		}
	      else 
		{
		  if ((loc = realloc(B->obj->lock.ports,(B->obj->lock.n_ports+1)*sizeof(gr_port)))==NULL) 
		    return -1; 
		}
	      B->obj->lock.n_ports++; 
	      B->obj->lock.ports = loc;
	      port = &B->obj->lock.ports[B->obj->lock.n_ports-1];
	      port_number = B->obj->lock.n_ports-1;
	    }
	  else 
	    return -1;
	}
    }
  /* copy p values in port */
  *port = *p;
  /* Scierror("XXXget connection OK on port %d\n",port_number); */
  return port_number;
}

/**
 * connector_unset_lock_connection: 
 * @B: a connector 
 * @i: a lock point id. 
 * @prt: a lock point port. 
 * 
 * unconect the object locked to port @port of lock point @i.
 * 
 **/

void connector_unset_lock_connection(NspConnector *B,int i,int port)
{
  int n_port= B->obj->lock.n_ports;
  if ( i ==  0 && port >= 0 && port < n_port  ) 
    {
      B->obj->lock.ports[port].object_id = NULL;
    }
  /* invalidate is only requested if the connected 
   * links is equal tozéro 
   */
  nsp_graphic_invalidate((NspGraphic *) B);
}

/**
 * connector_is_lock_connectable
 * @B: a connector 
 * @i: a lock point id. 
 * 
 * Checks if there's a free port in lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int connector_is_lock_connectable(NspConnector *B,int i)
{
  if ( i ==  0 ) 
    {
      int j;
      for ( j= 0 ; j < B->obj->lock.n_ports ; j++) 
	{
	  if ( B->obj->lock.ports[j].object_id == NULL) return TRUE; 
	}
      if ( B->obj->lock.fixed == FALSE ) 
	return TRUE; 
      else 
	return FALSE;
    }
  return FALSE;
}

/**
 * connector_is_lock_connected 
 * @B: a connector 
 * @i: a lock point id. 
 * 
 * Checks if there's a locked port for lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int connector_is_lock_connected(const NspConnector *B,int i)
{
  if ( i ==  0 ) 
    {
      int j;
      for ( j= 0 ; j < B->obj->lock.n_ports ; j++) 
	{
	  if ( B->obj->lock.ports[j].object_id != NULL) return TRUE; 
	}
    }
  return FALSE;
}

/**
 * connector_set_lock_pos: 
 * @B: a connector 
 * @i: a lock point id. 
 * @pt: a point coordinates 
 * 
 * Sets the lock point @i poistion to @pt. 
 **/

void connector_set_lock_pos(NspConnector *B, int i,const double pt[],int keep_angle,lock_dir dir)
{
  if ( i == 0 )
    {
      B->obj->lock.pt[0] = pt[0];
      B->obj->lock.pt[1] = pt[1];
    }
}


/**
 * connector_unlock:
 * @L: 
 * @lp: 
 * 
 * unlock the associated lock point of the connector 
 **/

static void connector_unlock( NspConnector *B,int lp) 
{
  NspObject *O1;
  gr_port p; 
  int i;
  /* just test if unlock is necessary */
  if ( connector_is_lock_connected(B,lp)==FALSE ) return; 
  for ( i= 0 ; i < B->obj->lock.n_ports ; i++) 
    {
      if ( connector_get_lock_connection(B,0,i,&p)==FAIL) continue;
      /* we are locked to to an object unlock it */
      O1 = p.object_id;
      if ( O1 != NULLOBJ ) 
	{
	  /* propagate unlock to the locked object */
	  GR_INT(O1->basetype->interface)->unset_lock_connection(O1,p.lock,p.port);
	}
      /* unset the lock on the connector */
      connector_unset_lock_connection(B,lp,i);
    }
}







/* requested for gr_lock
 *
 */

static void  nsp_destroy_gr_lock(gr_lock *locks,NspConnector *H)
{
  connector_unlock(H,0);
  FREE(H->obj->lock.ports);
}

static int nsp_save_gr_lock(XDR *xdrs,gr_lock *lock,NspConnector *M)
{
  int i;
  /* the ports of the unique lock point */
  for ( i = 0 ; i < M->obj->lock.n_ports  ; i++) 
    {
      gr_port *port = M->obj->lock.ports+i;
      /* the port */
      if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(port->object_id)) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,port->lock) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,port->port) == FAIL) return FAIL;
    }
  return OK;
}

static int nsp_eq_gr_lock(gr_lock *lock1,gr_lock *lock2)
{
  /* ZZZZ */
  return OK;
}

  
static int nsp_load_gr_lock(XDR *xdrs,gr_lock *locks,NspConnector *M)
{  
  int i,id;
  if ( M->obj->lock.n_ports == 0) 
    {
      M->obj->lock.ports=NULL;
    }
  else 
    {
      if (( M->obj->lock.ports = malloc(M->obj->lock.n_ports*sizeof(gr_port))) == NULL) 
	return FAIL;
    }
  for ( i = 0 ; i < M->obj->lock.n_ports  ; i++) 
    {
      gr_port *port = M->obj->lock.ports+i;
      /* the port */
      port->object_id = NULL;
      if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return FAIL;
      port->object_sid = NSP_INT_TO_POINTER(id);
      if ( nsp_xdr_load_i(xdrs,&port->lock) == FAIL) return FAIL;
      if ( nsp_xdr_load_i(xdrs,&port->port) == FAIL) return FAIL;
    }
  return OK;
}

static int nsp_print_gr_lock(int indent,gr_lock *locks,NspConnector *M)
{
  return OK;
}

static int nsp_check_gr_lock(gr_lock *locks,NspConnector *M)
{
  return OK;
}

static void nsp_init_gr_lock(gr_lock *lock)
{
  lock->n_ports  = 0;  /* initial number of ports */ 
  lock->fixed = FALSE; /* number of ports can be changed */
  lock->ports = NULL; 
}



static int nsp_gr_lock_full_copy(NspConnector *C,gr_lock *lock_c,NspConnector *M)
{
  gr_lock *lock = &M->obj->lock;
  int i;
  lock_c->n_ports = lock->n_ports;
  lock_c->fixed = lock->fixed;
  if (( lock_c->ports = malloc(lock_c->n_ports*sizeof(gr_port))) == NULL)
    return FAIL;
  for ( i = 0 ; i < lock_c->n_ports  ; i++) 
    {
      lock_c->ports[i]= lock->ports[i];
      lock_c->ports[i].object_id = NULLOBJ;
      lock_c->ports[i].object_sid = lock->ports[i].object_id ;
    }

  /* trick: we use  this function to also update the object_sid value */
  C->obj->object_sid = M;
  return OK;
}

#line 1527 "connector.c"
