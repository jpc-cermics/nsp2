/* -*- Mode: C -*- */

#include "nsp/object.h"
#include "nsp/interf.h"
#define  Port_Private 
#include "gport.h"

/* 
 * NspPort inherits from NspObject 
 */

int nsp_type_port_id=0;
NspTypePort *nsp_type_port=NULL;

/*
 * Type object for Port 
 * all the instance of NspTypePort share the same id. 
 * nsp_type_port: is an instance of NspTypePort 
 *    used for objects of NspPort type (i.e built with new_port) 
 * other instances are used for derived classes 
 */
NspTypePort *new_type_port(type_mode mode)
{
  NspTypePort *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_port != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_port;
    }
  if ((type =  malloc(sizeof(NspTypePort))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = port_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = port_get_methods; 
  type->new = (new_func *) new_port;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for port */ 

  top->pr = (print_func *) nsp_port_print;                  
  top->dealloc = (dealloc_func *) nsp_port_destroy;
  top->copy  =  (copy_func *) nsp_port_copy;                 
  top->size  = (size_func *) nsp_port_size;                
  top->s_type =  (s_type_func *) nsp_port_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_port_type_short_string;
  top->info = (info_func *) nsp_port_info ;                  
  /* top->is_true = (is_true_func  *) nsp_port_is_true; */
  /* top->loop =(loop_func *) nsp_port_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_port_object;
  top->eq  = (eq_func *) nsp_port_eq;
  top->neq  = (eq_func *) nsp_port_neq;
  top->save  = (save_func *) nsp_port_xdr_save;
  top->load  = (load_func *) nsp_port_xdr_load;
  top->create = (create_func*) int_port_create;
  
  /* specific methods for port */
      
  type->init = (init_func *) init_port;

/* 
 * Port interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_port_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePort called nsp_type_port
       */
      type->id =  nsp_type_port_id = nsp_new_type_id();
      nsp_type_port = type;
      if ( nsp_register_type(nsp_type_port) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_port(mode);
    }
  else 
    {
       type->id = nsp_type_port_id;
       return type;
    }
}

/*
 * initialize Port instances 
 * locally and by calling initializer on parent class 
 */

static int init_port(NspPort *o,NspTypePort *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Port 
 */

NspPort *new_port() 
{
  NspPort *loc; 
  /* type must exists */
  nsp_type_port = new_type_port(T_BASE);
  if ( (loc = malloc(sizeof(NspPort)))== NULLPORT) return loc;
  /* initialize object */
  if ( init_port(loc,nsp_type_port) == FAIL) return NULLPORT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Port 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_port_size(NspPort *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char port_type_name[]="Port";
static char port_short_type_name[]="port";

static char *nsp_port_type_as_string(void)
{
  return(port_type_name);
}

static char *nsp_port_type_short_string(void)
{
  return(port_short_type_name);
}

/*
 * A == B 
 */

static int nsp_port_eq(NspPort *A, NspObject *B)
{
  NspPort *loc = (NspPort *) B;
  if ( check_cast(B,nsp_type_port_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->lock != loc->obj->lock) return FALSE;
  if ( A->obj->port != loc->obj->port) return FALSE;
  if ( A->obj->object_id != loc->obj->object_id) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_port_neq(NspPort *A, NspObject *B)
{
  return ( nsp_port_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_port_xdr_save(XDR *xdrs, NspPort *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->lock) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->port) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->object_id)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspPort  *nsp_port_xdr_load(XDR *xdrs)
{
  NspPort *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPORT;
  if ((M  = port_create_void(name,(NspTypeBase *) nsp_type_port))== NULLPORT) return M;
  if ((M->obj = malloc(sizeof(nsp_port))) == NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->lock) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->port) == FAIL) return NULL;
  if ((M->obj->object_id = nsp_object_xdr_load(xdrs))== NULLOBJ) return NULL;
  return M;
}

/*
 * delete 
 */

void nsp_port_destroy(NspPort *H)
{
  FREE(NSP_OBJECT(H)->name);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
    {
      FREE(H->obj);
    }
  FREE(H);
}

/*
 * info 
 */

void nsp_port_info(NspPort *M, int indent,char *name,int rec_level)
{
  int i;
  if ( M == NULLPORT) 
    {
      Sciprintf("Null Pointer Port \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("Port %s {\n", NSP_OBJECT(M)->name);
  for ( i=0 ; i < indent+2 ; i++) Sciprintf(" "); Sciprintf("lock=%d\n",M->obj->lock);
  for ( i=0 ; i < indent+2 ; i++) Sciprintf(" "); Sciprintf("port=%d\n",M->obj->port);
  for ( i=0 ; i < indent+2 ; i++) Sciprintf(" "); Sciprintf("Object=0x%x\n",M->obj->object_id);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");Sciprintf("}\n");
}

/*
 * print 
 */

void nsp_port_print(NspPort *M, int indent,char *name, int rec_level)
{
  int i;
  if ( M == NULLPORT) 
    {
      Sciprintf("Null Pointer Port \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("Port %s {\n", NSP_OBJECT(M)->name);
  for ( i=0 ; i < indent+2 ; i++) Sciprintf(" "); Sciprintf("lock=%d\n",M->obj->lock);
  for ( i=0 ; i < indent+2 ; i++) Sciprintf(" "); Sciprintf("port=%d\n",M->obj->port);
  for ( i=0 ; i < indent+2 ; i++) Sciprintf(" "); Sciprintf("Object=0x%x\n",M->obj->object_id);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");Sciprintf("}\n");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Port objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPort   *nsp_port_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_port_id) == TRUE ) return ((NspPort *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_port));
  return NULL;
}

int IsPortObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_port_id);
}

int IsPort(NspObject *O)
{
  return nsp_object_type(O,nsp_type_port_id);
}

NspPort  *GetPortCopy(Stack stack, int i)
{
  if (  GetPort(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPort  *GetPort(Stack stack, int i)
{
  NspPort *M;
  if (( M = nsp_port_object(NthObj(i))) == NULLPORT)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspPort *port_create_void(char *name,NspTypeBase *type)
{
 NspPort *H  = (type == NULL) ? new_port() : type->new();
 if ( H ==  NULLPORT)
  {
   Sciprintf("No more memory\n");
   return NULLPORT;
  }
 if ( ( NSP_OBJECT(H)->name =new_nsp_string(name)) == NULLSTRING) return NULLPORT;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspPort *port_create(char *name,int lock,int port,NspObject*object_id,NspTypeBase *type)
{
  NspPort *H  = port_create_void(name,type);
  if ( H ==  NULLPORT) return NULLPORT;
  if ((H->obj = malloc(sizeof(nsp_port))) == NULL) return NULL;
  H->obj->ref_count=1;
  H->obj->lock=lock;
  H->obj->port=port;
  H->obj->object_id = object_id;
  return H;
}

/*
 * copy for gobject derived class  
 */

NspPort *nsp_port_copy(NspPort *self)
{
  NspPort *H  =port_create_void(NVOID,(NspTypeBase *) nsp_type_port);
  if ( H ==  NULLPORT) return NULLPORT;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Port
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_port_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPort *H;
  CheckStdRhs(0,0);
  /* want to be sure that type port is initialized */
  nsp_type_port = new_type_port(T_BASE);
  if(( H = port_create_void(NVOID,(NspTypeBase *) nsp_type_port)) == NULLPORT) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if((H->obj = calloc(1,sizeof(nsp_port)))== NULL ) return RET_BUG;
  H->obj->ref_count = 1;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  H->obj->object_id = NULL;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 


static NspMethods port_methods[] = {
  { NULL, NULL}
};

static NspMethods *port_get_methods(void) { return port_methods;};

/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_port_get_lock(void *self,char *attr)
{
  int ret = ((NspPort *) self)->obj->lock;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_port_set_lock(void *self, char *attr, NspObject *O)
{
  int lock;
  if ( IntScalar(O,&lock) == FAIL) return FAIL;
  ((NspPort *) self)->obj->lock = lock;
  return OK;
}

static NspObject *_wrap_port_get_port(void *self,char *attr)
{
  int ret = ((NspPort *) self)->obj->port;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_port_set_port(void *self, char *attr, NspObject *O)
{
  int port;

  if ( IntScalar(O,&port) == FAIL) return FAIL;
  ((NspPort *) self)->obj->port = port;
  return OK;
}

static NspObject *_wrap_port_get_object_id(void *self,char *attr)
{
  NspObject *ret;
  ret = ((NspObject*) ((NspPort *) self)->obj->object_id);
 return ret;
}

static int _wrap_port_set_object_id(void *self, char *attr, NspObject *O)
{
  NspObject *object_id;
  if (( object_id =  nsp_object_copy_and_name(attr,O)) == NULL) return FAIL;
  if (((NspPort *) self)->obj->object_id != NULL ) 
    nsp_object_destroy((NspObject **) &((NspPort *) self)->obj->object_id);
  ((NspPort *) self)->obj->object_id = object_id;
  return OK;
}

static AttrTab port_attrs[] = {
  { "lock", (attr_get_function *)_wrap_port_get_lock, (attr_set_function *)_wrap_port_set_lock,(attr_get_object_function *)int_get_object_failed },
  { "port", (attr_get_function *)_wrap_port_get_port, (attr_set_function *)_wrap_port_set_port,(attr_get_object_function *)int_get_object_failed },
  { "object_id", (attr_get_function *)_wrap_port_get_object_id, (attr_set_function *)_wrap_port_set_object_id,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gport_func[]={
  { "gport_create", int_port_create},
  { NULL, NULL}
};

/* call ith function in the gport interface */

int gport_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(gport_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gport_Interf_Info(int i, char **fname, function (**f))
{
  *fname = gport_func[i].name;
  *f = gport_func[i].fonc;
}
