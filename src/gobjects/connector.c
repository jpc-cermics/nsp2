/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#define  Connector_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/grint.h" /* interface definition */
#include "nsp/graphics/Graphics.h"

/*
 * NspConnector inherits from NspObject and implements GRint 
 * Used for multiple link connections. 
 * A connector has just one lock point where many links can be locked 
 */

int nsp_type_connector_id=0;
NspTypeConnector *nsp_type_connector=NULL;

NspTypeConnector *new_type_connector(type_mode mode)
{
  NspTypeConnector *type = NULL;
  NspTypeObject *top;
  NspTypeGRint *gri;

  if ( nsp_type_connector != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_connector;
    }
  if (( type =  malloc(sizeof(NspTypeConnector))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  connector_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  connector_get_methods; 
  type->new = (new_func *) new_connector;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for connector */ 

  top->pr = (print_func *) connector_print;                    
  top->dealloc = (dealloc_func *) connector_destroy;            
  top->copy  =  (copy_func *) connector_copy;                   
  top->size  = (size_func *) connector_size;                  
  top->s_type =  (s_type_func *) connector_type_as_string;    
  top->sh_type = (sh_type_func *) connector_type_short_string;
  top->info = (info_func *) connector_info;                    
  /* top->is_true = (is_true_func  *) ConnectorIsTrue;           */
  /* top->loop =(loop_func *) connector_loop; */
  top->path_extract =    (path_func *)  object_path_extract ;       
  top->get_from_obj = (get_from_obj_func *) connector_object  ;  
  top->eq  = (eq_func *) connector_eq;
  top->neq  = (eq_func *) connector_neq;
  top->save  = (save_func *) connector_xdr_save;
  top->load  = (load_func *) connector_xdr_load;
  top->create = (create_func*) int_connector_create;

  /* specific methods for connector */

  type->init =  (init_func *) init_connector;

  /* Connector implement grint interface */

  gri = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) gri;
  
  gri->get_hilited 	=(gr_get_hilited *) connector_get_hilited;
  gri->set_hilited 	=(gr_set_hilited *) connector_set_hilited;
  gri->get_show    	=(gr_get_show *) connector_get_show;
  gri->set_show		=(gr_set_show *) connector_set_show;
  gri->draw    		=(gr_draw *) connector_draw;
  gri->translate 	=(gr_translate *) connector_translate;
  gri->resize 		=(gr_resize *) connector_resize;
  gri->update_locks 	=(gr_update_locks *) connector_update_locks;
  gri->contains_pt 	=(gr_contains_pt *) connector_contains_pt;
  gri->control_near_pt 	=(gr_control_near_pt *) connector_control_near_pt;
  gri->lock_near_pt 	=(gr_lock_near_pt *) connector_lock_near_pt;
  gri->move_control_init=(gr_move_control_init *) connector_move_control_init;
  gri->move_control 	=(gr_move_control *) connector_move_control;

  gri->get_number_of_locks =(gr_get_number_of_locks *) connector_get_number_of_locks;
  gri->get_number_of_ports =(gr_get_number_of_ports *) connector_get_number_of_ports;
  gri->get_lock_connection =(gr_get_lock_connection *) connector_get_lock_connection;
  gri->get_lock_pos =(gr_get_lock_pos *) connector_get_lock_pos;
  gri->set_lock_connection =(gr_set_lock_connection *) connector_set_lock_connection;
  gri->unset_lock_connection =(gr_unset_lock_connection *) connector_unset_lock_connection;
  gri->is_lock_connectable =(gr_is_lock_connectable *) connector_is_lock_connectable;
  gri->is_lock_connected =(gr_is_lock_connected *) connector_is_lock_connected;
  gri->set_lock_pos =(gr_set_lock_pos *) connector_set_lock_pos;
  
  if ( nsp_type_connector_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_connector
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
 * initialize Connector instances 
 * locally and by calling initializer on parent class 
 */

static int init_connector(NspConnector *o,NspTypeConnector *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Connector 
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
 * Object method redefined for Connector 
 *-----------------------------------------------*/

 /*
  * size 
  */

static int connector_size(NspConnector *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char connector_type_name[]="Connector";
static char connector_short_type_name[]="gc";

static char *connector_type_as_string(void)
{
  return(connector_type_name);
}

static char *connector_type_short_string(void)
{
  return(connector_short_type_name);
}

/** used in for x=y where y is a Connector **/

int ConnectorFullComp(NspConnector * A,NspConnector * B,char *op,int *err)
{
  Scierror("ConnectorFullComp: to be implemented \n");
  return FALSE;
}

static int connector_eq(NspConnector *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_connector_id) == FALSE) return FALSE ;
  rep = ConnectorFullComp(A,(NspConnector *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int connector_neq(NspConnector *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_connector_id) == FALSE) return TRUE;
  rep = ConnectorFullComp(A,(NspConnector *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int connector_xdr_save(XDR  *xdrs, NspConnector *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("connector_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspConnector  *connector_xdr_load(XDR  *xdrs)
{
  NspConnector *M=NULLCONNECTOR;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCONNECTOR;
  Scierror("connector_xdr_load: to be implemented \n");
  return M;
}

static void connector_destroy(NspConnector *H)
{
  FREE(NSP_OBJECT(H)->name);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     connector_unlock(H,0);
     FREE(H->obj->lock.ports);
     FREE(H->obj);
   }
  FREE(H);
}

static void connector_info(NspConnector *H, int indent)
{
  int i;
  if ( H == NULLCONNECTOR) 
    {
      Sciprintf("Null Pointer Connector \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[Connector %s, r=[%5.2f,%5.2f,%5.2f,%5.2f] co=%d, th=%d bg=%d]\n",
	    NSP_OBJECT(H)->name,H->obj->r[0],H->obj->r[1],H->obj->r[2],H->obj->r[3],
	    H->obj->color,H->obj->thickness,H->obj->background);
}

static void connector_print(NspConnector *H, int indent)
{
  connector_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspConnector *connector_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_connector_id) == TRUE) return ((NspConnector *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_connector));
  return(NULL);
}

int IsConnectorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_connector_id);
}

int IsConnector(NspObject *O)
{
  return nsp_object_type(O , nsp_type_connector_id);
}

NspConnector  *GetConnectorCopy(Stack stack, int i)
{
  if (  GetConnector(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspConnector  *GetConnector(Stack stack, int i)
{
  NspConnector *M;
  if (( M = connector_object(NthObj(i))) == NULLCONNECTOR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

static NspConnector *connector_create_void(char *name,NspTypeBase *type)
{
 NspConnector *H  = (type == NULL) ? new_connector() : type->new();
 if ( H ==  NULLCONNECTOR)
  {
   Sciprintf("No more memory\n");
   return NULLCONNECTOR;
  }
 if ( ( NSP_OBJECT(H)->name =new_nsp_string(name)) == NULLSTRING) return NULLCONNECTOR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspConnector *connector_create(char *name,double rect[],int color,int thickness,int background,
			       NspTypeBase *type )
{
  int i;
  NspConnector *H  = connector_create_void(name,type);
  if ( H ==  NULLCONNECTOR) return NULLCONNECTOR;
  if ((H->obj = malloc(sizeof(nsp_connector))) == NULL) return NULL;
  H->obj->ref_count=1;
  H->obj->frame = NULL; 
  /* fields */
  for ( i=0; i < 4 ; i++) H->obj->r[i]=rect[i];
  H->obj->color = color;
  H->obj->thickness = thickness;
  H->obj->background = background;
  H->obj->hilited = FALSE ; 

  H->obj->show = TRUE   ; 
  H->obj->lock.n_ports  = 1;  /* initial number of ports */ 
  H->obj->lock.fixed = FALSE; /* number of ports can be changed */
  if (( H->obj->lock.ports = malloc(sizeof(gr_port))) == NULL) return NULLCONNECTOR;
  H->obj->lock.ports[0].object_id = NULL; /* port is unused */
  connector_update_locks(H);
  return H;
}

/*
 * copy for gobject derived class  
 */

NspConnector *connector_copy(NspConnector *self)
{
  NspConnector *H  =connector_create_void(NVOID,(NspTypeBase *) nsp_type_connector);
  if ( H ==  NULLCONNECTOR) return NULLCONNECTOR;
  H->obj = self->obj;
  self->obj->ref_count++;
  return H;
}

/*-------------------------------------------------------------------
 * constructor at nsp level 
 * %types.Connector(...) or %types.Connector.new[]
 *-------------------------------------------------------------------*/

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);

static int int_connector_create(Stack stack, int rhs, int opt, int lhs)
{
  NspConnector *H;
  double *val;
  int back=-1,color=-1,thickness=-1;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = connector_create(NVOID,val,color,thickness,back,NULL)) == NULLCONNECTOR) return RET_BUG;
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
      CheckLength(stack.fname,1,M1,4);
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
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 4\r\n",stack.fname,rhs-opt);
      return FAIL;
    }
  return OK;
}


/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_gconnector_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspConnector *) Hv)->obj->color);
}

static int int_gconnector_set_color(void *Hv, char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspConnector *)Hv)->obj->color = color;
  return OK ;
}

static NspObject * int_gconnector_get_thickness(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspConnector *) Hv)->obj->thickness);
}
                                                                                                      
static int int_gconnector_set_thickness(void *Hv, char *attr, NspObject *O)
{
  int thickness;
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspConnector *)Hv)->obj->thickness = thickness;
  return OK ;
}

static NspObject * int_gconnector_get_background(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspConnector *) Hv)->obj->background);
}
                                                                                                      
static int int_gconnector_set_background(void *Hv, char *attr, NspObject *O)
{
  int background;
  if (  IntScalar(O,&background) == FAIL) return FAIL;
  ((NspConnector *)Hv)->obj->background = background;
  return OK ;
}

static NspObject * int_gconnector_get_hilited(void *Hv,char *attr)
{
  return nsp_new_boolean_obj(((NspConnector *) Hv)->obj->hilited);
}
                                                                                                      
static int int_gconnector_set_hilited(void *Hv, char *attr, NspObject *O)
{
  int hilited;
  if (  BoolScalar(O,&hilited) == FAIL) return FAIL;
  ((NspConnector *)Hv)->obj->hilited = hilited;
  return OK ;
}

static NspObject * int_gconnector_get_show(void *Hv,char *attr)
{
  return nsp_new_boolean_obj(((NspConnector *) Hv)->obj->show);
}
                                                                                                      
static int int_gconnector_set_show(void *Hv, char *attr, NspObject *O)
{
  int show;
  if (  BoolScalar(O,&show) == FAIL) return FAIL;
  ((NspConnector *)Hv)->obj->show = show;
  return OK ;
}

static AttrTab connector_attrs[] = {
  { "color",        int_gconnector_get_color ,     int_gconnector_set_color ,     NULL },
  { "background",    int_gconnector_get_background,  int_gconnector_set_background,  NULL },
  { "thickness",    int_gconnector_get_thickness,  int_gconnector_set_thickness,  NULL },
  { "hilited",   int_gconnector_get_hilited, int_gconnector_set_hilited, NULL },
  { "show",   int_gconnector_get_show, int_gconnector_set_show, NULL },
  { (char *) 0, NULL}
};

/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/

/* draw */

static int int_gcdraw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  connector_draw(self);
  return 0;
}

/* translate */

static int int_gctranslate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,M,2);
  connector_translate(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/* resize */ 

static int int_gcresize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,M,2);
  connector_resize(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

static NspMethods connector_methods[] = {
  { "translate", int_gctranslate},
  { "resize",   int_gcresize},
  { "draw",   int_gcdraw},
  { (char *) 0, NULL}
};

static NspMethods *connector_get_methods(void) { return connector_methods;};

/*------------------------------------------------------
 *  Interface 
 *---------------------------------------------------*/

static OpTab Connector_func[]={
  {"setrowscols_gc",int_set_attributes}, 
  {(char *) 0, NULL}
};

/** call ith function in the Connector interface **/

int Connector_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Connector_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Connector_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Connector_func[i].name;
  *f = Connector_func[i].fonc;
}


#include <string.h>
#include <stdio.h>
#include <math.h>

#include "nsp/object.h"
#include "nsp/graphics/Graphics.h"

/*
 * implementation of the GRint interface 
 * for a connector 
 */ 

/**
 * connector_get_hilited:
 * @B: a connector 
 *
 * Returns the value of the hilited attribute of object @B.
 *
 **/

int connector_get_hilited(NspConnector *B) {  return B->obj->hilited; } 

/**
 * connector_set_hilited:
 * @B: a connector 
 * @val: %True or %False. 
 * 
 * Sets the hilited status of the connector @B.
 *
 **/

void connector_set_hilited(NspConnector *B,int val) {  B->obj->hilited = val; } 

/**
 * connector_get_show:
 * @B: a connector 
 * @val:  %True or %False. 
 * 
 * Returns the value of the show attribute of object @B.
 *
 **/

int connector_get_show(NspConnector *B) {  return B->obj->show; } 

/**
 * connector_set_show:
 * @B: a connector 
 *
 * Sets the show status of the given Connector.
 *
 **/

void connector_set_show(NspConnector *B,int val) {  B->obj->show = val; } 

/**
 * connector_draw:
 * @B: a connector 
 *
 * Draws the connector given as argument using the current graphic driver 
 *
 **/

static int lock_size=2;
static int lock_color=10;

void connector_draw(NspConnector *B)
{
  BCG *Xgc;
  double loc[4];
  int cpat, cwidth,i;
  /* only draw block which are in a frame */
  if ( B->obj->frame == NULL) return;
  if ( B->obj->show == FALSE ) return ;
  Xgc=B->obj->frame->Xgc;
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
  /* draw frame rectangle only if hilited */
  Xgc->graphic_engine->xset_pattern(Xgc,B->obj->color);
  Xgc->graphic_engine->scale->drawrectangle(Xgc,B->obj->r);
  /* add hilited */ 
  Xgc->graphic_engine->xset_pattern(Xgc,lock_color);
  if ( B->obj->hilited == TRUE ) 
    {
      loc[0]=B->obj->r[0]; loc[1]=B->obj->r[1];loc[2]=loc[3]= lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
      loc[0]+= B->obj->r[2] -2; loc[1] -= B->obj->r[3] -2;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
    }
  i=0; 
  if ( connector_is_lock_connected(B,i)== TRUE)
    Xgc->graphic_engine->xset_pattern(Xgc,lock_color); 
  else 
    Xgc->graphic_engine->xset_pattern(Xgc,1); 
  connector_get_lock_pos(B,i,loc);
  loc[0] += -1; loc[1] += 1;loc[2]=loc[3]= lock_size;
  Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
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

void connector_translate(NspConnector *B,const double pt[2])
{
  B->obj->r[0] += pt[0] ;
  B->obj->r[1] += pt[1] ;
  connector_update_locks(B);
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
  B->obj->r[2] = size[0] ;
  B->obj->r[3] = size[1] ;
  connector_update_locks(B);
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
 * Checks if the given point in inside the connector rectangle
 * 
 * Return value: %True or %False.
 **/

int connector_contains_pt(const NspConnector *B,const double pt[2])
{
  return B->obj->r[0] <= pt[0] && B->obj->r[1] >= pt[1] 
    && B->obj->r[0]+B->obj->r[2] >= pt[0] && B->obj->r[1]- B->obj->r[3] <= pt[1];
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

int connector_lock_near_pt(const NspConnector *B,double pt[2], int *cp)
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

void connector_move_control(NspGFrame *F, NspConnector *B,const double mpt[2], int cp,double ptc[2])
{
  B->obj->r[2] =  Max(  mpt[0] - B->obj->r[0] ,0);
  B->obj->r[3] =  Max(  B->obj->r[1] -mpt[1] ,0);
  ptc[0]=mpt[0];
  ptc[1]=mpt[1];
  connector_update_locks(B);
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
 * Return value: #TRUE if lock point and port number exists or #FALSE. 
 **/

int connector_get_lock_connection(const NspConnector *B,int i,int port, gr_port *p )
{
  int np = B->obj->lock.n_ports; 
  if ( i == 0 && port >= 0 && port < np ) 
    {
      gr_port *gport= &B->obj->lock.ports[port]; 
      if ( gport->object_id == NULL) return FAIL;
      p->object_id = gport->object_id;
      p->lock = gport->lock;
      p->port = gport->port;
      Scierror("XXXget lock connection OK %d %d\n",p->lock,p->port);
      return OK;
    }
  Scierror("XXXget lock connection failed\n");
  return FAIL;
}

/**
 * connector_get_lock_pos:
 * @B: a connector 
 * @i: a lock point id. 
 * @pt: point coordinates.
 *
 * Returns in @pt the position of lock point @i. 
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
 * connector_set_lock_connection: 
 * @B: a connector 
 * @i: a lock point id. 
 * @p: information to be connected to one port of lock point i;
 * 
 * the port described by @p is connected to a port of lock point i; 
 * return value: -1 or the port number used for connection.
 **/

int connector_set_lock_connection(NspConnector *B,int i,const gr_port *p)
{
  int port_number=0;
  gr_port *port= NULL;
  if ( i ==  0 ) 
    {
      int j;
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
	      B->obj->lock.n_ports++; 
	      if ((loc = realloc(B->obj->lock.ports, B->obj->lock.n_ports*sizeof(gr_port)))==NULL) 
		return -1; 
	      B->obj->lock.ports = loc;
	      port = &B->obj->lock.ports[B->obj->lock.n_ports-1];
	      port_number = B->obj->lock.n_ports-1;
	    }
	  else 
	    return -1;
	}
      port->object_id = p->object_id;
      port->lock = p->lock;
      port->port= p->port; 
      Scierror("XXXget connection OK on port %d\n",port_number);

      return port_number;
    }
  return -1;
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
      /* XXXX : faut-il aussi propager l'info sur l'object locké ? */
      B->obj->lock.ports[port].object_id = NULL;
    }
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

int connector_is_lock_connected(NspConnector *B,int i)
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

void connector_set_lock_pos(NspConnector *B, int i,const double pt[])
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
      connector_unset_lock_connection(B,0,i);
    }
}




