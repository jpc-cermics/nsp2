/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2004 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  Serial_Private 
#include "nsp/serial.h"
#include "nsp/interf.h"

/* 
 * NspSerial inherits from NspObject
 */

int nsp_type_serial_id=0;
NspTypeSerial *nsp_type_serial=NULL;

/*
 * Type object for Serial 
 * all the instance of NspTypeSerial share the same id. 
 * nsp_type_serial: is an instance of NspTypeSerial 
 *    used for objects of NspSerial type (i.e built with new_serial) 
 * other instances are used for derived classes 
 */

NspTypeSerial *new_type_serial(type_mode mode)
{
  NspTypeSerial *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_serial != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_serial;
    }
  
  if ((type =  malloc(sizeof(NspTypeSerial))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = serial_get_methods; 
  type->new = (new_func *) new_serial;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for serial */ 
  
  top->pr = (print_func *) nsp_serial_print;                    
  top->dealloc = (dealloc_func *) nsp_serial_destroy;
  top->copy  =  (copy_func *) nsp_serial_copy;                   
  top->size  = (size_func *) nsp_serial_size;                  
  top->s_type =  (s_type_func *) nsp_serial_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_serial_type_short_string;
  top->info = (info_func *) nsp_serial_info ;                    
  /* top->is_true = (is_true_func  *) SerialIsTrue; */
  /* top->loop =(loop_func *) serial_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) nsp_serial_object;
  top->eq  = (eq_func *) nsp_serial_eq;
  top->neq  = (eq_func *) nsp_serial_neq;
  top->save  = (save_func *) nsp_serial_xdr_save;
  top->load  = (load_func *) nsp_serial_xdr_load;

  /* specific methods for serial */
      
  type->init = (init_func *) init_serial;
      
  /* 
   * Serial interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_serial_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSerial called nsp_type_serial
       */
      type->id =  nsp_type_serial_id = nsp_new_type_id();
      nsp_type_serial = type;
      if ( nsp_register_type(nsp_type_serial) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_serial(mode);
    }
  else 
    {
      type->id = nsp_type_serial_id;
      return type;
    }
}

/*
 * initialize Serial instances 
 * locally and by calling initializer on parent class 
 */

static int init_serial(NspSerial *o,NspTypeSerial *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  o->val = NULL;
  o->nbytes = 0;
  return OK;
}

/*
 * new instance of Serial 
 */

NspSerial *new_serial() 
{
  NspSerial *loc; 
  /* type must exists */
  nsp_type_serial = new_type_serial(T_BASE);
  if ( (loc = malloc(sizeof(NspSerial)))== NULLSERIAL) return loc;
  /* initialize object */
  if ( init_serial(loc,nsp_type_serial) == FAIL) return NULLSERIAL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Serial 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_serial_size(NspSerial *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char serial_type_name[]="Serial";
static char serial_short_type_name[]="serial";

static char *nsp_serial_type_as_string(void)
{
  return(serial_type_name);
}

static char *nsp_serial_type_short_string(void)
{
  return(serial_short_type_name);
}

static int nsp_serial_full_comp(NspSerial * A,NspSerial * B,char *op,int *err)
{
  Scierror("serial_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int nsp_serial_eq(NspSerial *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_serial_id) == FALSE) return FALSE ;
  rep = nsp_serial_full_comp(A,(NspSerial *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int nsp_serial_neq(NspSerial *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_serial_id) == FALSE) return TRUE;
  rep = nsp_serial_full_comp(A,(NspSerial *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int nsp_serial_xdr_save(XDR  *xdrs, NspSerial *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->nbytes) == FAIL) return FAIL;
  if (nsp_xdr_save_array_c(xdrs,M->val,M->nbytes) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspSerial  *nsp_serial_xdr_load(XDR *xdrs)
{
  int nbytes;
  NspSerial *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSERIAL;
  if (nsp_xdr_load_i(xdrs,&nbytes) == FAIL) return NULLSERIAL;
  /* nbytes here is all the bytes i.e + the serial header */
  if ((M = nsp_serial_create(name,NULL,nbytes-strlen(nsp_serial_header)))== NULLSERIAL) return NULLSERIAL;
  if (nsp_xdr_load_array_c(xdrs,M->val,M->nbytes) == FAIL) return  NULLSERIAL;
  return M;
}

/*
 * delete 
 */

void nsp_serial_destroy(NspSerial *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  FREE(H->val);
  FREE(H);
}

/*
 * info 
 */

void nsp_serial_info(NspSerial *H, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= <%d-bytes>\t\t serial\n",pname,H->nbytes);
}

/*
 * print 
 */

void nsp_serial_print(NspSerial *H, int indent,const char *name, int rec_level)
{
  nsp_serial_info(H,indent,name,rec_level);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Serial objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspSerial   *nsp_serial_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast(O,nsp_type_serial_id) == TRUE) return ((NspSerial *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_serial));
  return(NULL);
}

int IsSerialObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_serial_id);
}

int IsSerial(NspObject *O)
{
  return nsp_object_type(O,nsp_type_serial_id);
}

NspSerial  *GetSerialCopy(Stack stack, int i)
{
  if (  GetSerial(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSerial  *GetSerial(Stack stack, int i)
{
  NspSerial *M;
  if (( M = nsp_serial_object(NthObj(i))) == NULLSERIAL)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspSerial instance 
 *
 * creates a serial buffer which can store nbytes 
 * If buf is present buf is copied into the serial buffer 
 * When filled we start val whith a string to get the 
 * opportunity of checking an incorrect serial.
 *-----------------------------------------------------*/

const char nsp_serial_header[]="@nsp01";

static NspSerial *_nsp_serial_create(const char *name,const char *buf,int nbytes,NspTypeBase *type)
{
  NspSerial *H  = (type == NULL) ? new_serial() : type->new();
  if ( H ==  NULLSERIAL)
    {
      Sciprintf("No more memory\n");
      return NULLSERIAL;
    }
  /* should be unsigned */
  nbytes=Max(0,nbytes);
  H->nbytes = nbytes + strlen(nsp_serial_header);
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return(NULLSERIAL);
  NSP_OBJECT(H)->ret_pos = -1 ;
  if ((H->val = malloc((H->nbytes)*sizeof(char)))== NULL) 
    {
      Sciprintf("No more memory\n");
      return NULLSERIAL;
    }
  if ( buf != NULL) 
    {
      memcpy(H->val,nsp_serial_header,strlen(nsp_serial_header));
      memcpy(H->val+strlen(nsp_serial_header),buf,nbytes);
    }
  else 
    {
      memset(H->val,0,H->nbytes);
    }
  return H;
}

NspSerial *nsp_serial_create(const char *name,const char *buf,int nbytes)
{
  return _nsp_serial_create(name,buf,nbytes,NULL);
}

/*
 * copy 
 */

NspSerial *nsp_serial_copy(NspSerial *H)
{
  return _nsp_serial_create(NVOID,H->val,H->nbytes,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the Serial
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static int int_serial_meth_unserialize(void *a,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  return 1;
}

static NspMethods serial_methods[] = {
  { "unserialize", int_serial_meth_unserialize},
  { (char *) 0, NULL}
};

static NspMethods *serial_get_methods(void) { return serial_methods;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_serial_unserialize(Stack stack, int rhs, int opt, int lhs)
{
  NspSerial *a;
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (( a= GetSerial(stack,1))== NULLSERIAL ) return RET_BUG;
  if ((Obj=nsp_object_unserialize(a))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Obj); 
  return 1; 
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Serial_func[]={
  {"unserialize",int_serial_unserialize},
  {(char *) 0, NULL}
};

/* call ith function in the Serial interface */

int Serial_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Serial_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Serial_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Serial_func[i].name;
  *f = Serial_func[i].fonc;
}

