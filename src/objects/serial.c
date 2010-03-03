/*
 * Copyright (C) 2006-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * Serial Object. 
 */

#include "nsp/object.h"
#define  Serial_Private 
#include "nsp/serial.h"
#include "nsp/interf.h"
#include "nsp/smio.h" /* zlib.h */
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
  top->full_copy  =  (copy_func *) nsp_serial_copy;                   

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
  switch (flag) 
    {
    case 0: return Mat->nbytes;
    case 1: return 1;
    case 2: return Mat->nbytes;
    }
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

static char *nsp_serial_type_short_string(NspObject *v)
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
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_serial)) == FAIL) return FAIL;
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
  /* nbytes here is all the bytes i.e + the serial header 
   * Note that the following code will work even if the 
   * serial object was compressed. 
   */
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

int nsp_serial_info(NspSerial *H, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= <%d-bytes>\t\t serial\n",pname,H->nbytes);
  return TRUE;
}

/*
 * print 
 */

int nsp_serial_print(NspSerial *H, int indent,const char *name, int rec_level)
{
  nsp_serial_info(H,indent,name,rec_level);
  return TRUE;
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
const char nsp_zserial_header[]="@nspz01";

static NspSerial *_nsp_serial_create(const char *name,const char *buf,int nbytes,NspTypeBase *type,int zflag)
{
  int len;
  const char *header=NULL;
  NspSerial *H  = (type == NULL) ? new_serial() : type->new();
  if ( H ==  NULLSERIAL)
    {
      Sciprintf("No more memory\n");
      return NULLSERIAL;
    }
  if ( zflag == TRUE)
    {
      header =  nsp_zserial_header; 
      len = strlen(header)+8;
    }
  else
    {
      header =  nsp_serial_header; 
      len = strlen(header);
    }
  /* should be unsigned */
  nbytes=Max(0,nbytes);
  H->nbytes = nbytes + len ;
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
      memcpy(H->val,header,strlen(header));
      memcpy(H->val+len,buf,nbytes);
    }
  else 
    {
      memset(H->val,0,H->nbytes);
    }
  return H;
}

NspSerial *nsp_serial_create(const char *name,const char *buf,int nbytes)
{
  return _nsp_serial_create(name,buf,nbytes,NULL,FALSE);
}

/*
 * copy 
 */

NspSerial *nsp_serial_copy(const NspSerial *H)
{
  if (strncmp(H->val,nsp_zserial_header,strlen(nsp_zserial_header))==0)
    {
      int len = strlen(nsp_zserial_header)+8;
      NspSerial *S=_nsp_serial_create(NVOID,H->val+len,H->nbytes-len,NULL,TRUE);
      if ( S == NULL) return S;
      memcpy(S->val+strlen(nsp_zserial_header),H->val+strlen(nsp_zserial_header),sizeof(int));
      return S;
    }
  else
    {
      int len = strlen(nsp_serial_header);
      return _nsp_serial_create(NVOID,H->val+len,H->nbytes-len,NULL,FALSE);
    }
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
  NspObject *Obj;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((Obj=nsp_object_unserialize(a))== NULLOBJ) return RET_BUG;
  /* take care that nsp_object_unserialize returns a new object 
   * but with a name, we have to delete the name here otherwise 
   * Obj will be copied at return and we will have unfreed memory.
   */
  if (nsp_object_set_name(Obj,NVOID) == FAIL) return RET_BUG;
  MoveObj(stack,1,Obj); 
  return 1;
}


NspSerial *nsp_serial_compress(const NspSerial *S)
{
#ifdef HAVE_ZLIB
  unsigned long nc;
  char *loc;
  NspSerial *Sz;
  int nb,rep;
  int len = strlen(nsp_serial_header);
  if ( strncmp(S->val,nsp_zserial_header,strlen(nsp_zserial_header))==0 )
    {
      /*  S is already a compressed serial object 
       */
      return nsp_serial_copy(S);
    }
  /* number of bytes in serial object */
  nb = S->nbytes-len;
  /* initial size for compression buffer */
  nc = nb*(1+0.1) + 12;
  if ((Sz = _nsp_serial_create(NVOID,NULL,nc,NULL,TRUE))== NULL) 
    return NULL;
  rep = compress ((Bytef *) Sz->val + strlen(nsp_zserial_header)+8,&nc,
		  (Bytef *) S->val + len, nb);
  if ( rep != Z_OK )
    {
      Scierror("Error: compression failed\n");
      nsp_serial_destroy(Sz);
      return NULL;
    }
  loc = realloc(Sz->val, nc + strlen(nsp_zserial_header)+8);
  if ( loc == NULL) 
    {
      Scierror("Error: running out of memory\n");
      nsp_serial_destroy(Sz);
      return NULL;
    }
  Sz->val = loc;
  Sz->nbytes = nc +strlen(nsp_zserial_header)+8 ; 
  memcpy(Sz->val,nsp_zserial_header, strlen(nsp_zserial_header));
  memcpy(Sz->val+strlen(nsp_zserial_header),&nb,sizeof(int));
  return Sz;
#else
  Scierror("Error: cannot compress with this nsp version\n");
  return NULL;
#endif 
}

static int int_serial_meth_compress(void *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSerial *Sz; 
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((Sz = nsp_serial_compress(self))== NULL)
    return RET_BUG; 
  MoveObj(stack,1,NSP_OBJECT(Sz)); 
  return 1;
}


NspSerial *nsp_serial_uncompress(const NspSerial *Sz)
{
#ifdef HAVE_ZLIB
  NspSerial *S;
  unsigned long nb;
  int rep, zlen = strlen(nsp_zserial_header);
  int len = strlen(nsp_serial_header);
  if ( strncmp(Sz->val,nsp_serial_header,len) ==0) 
    {
      return nsp_serial_copy(Sz);
    }
  /* get size after uncompression */
  memcpy(&nb,Sz->val+ zlen ,sizeof(int));
  if ((S = _nsp_serial_create(NVOID,NULL,nb,NULL,FALSE))== NULL) 
    return NULL;
  rep = uncompress ((Bytef *) S->val + len ,&nb,
		    (Bytef *) Sz->val + zlen+8 ,Sz->nbytes - (zlen+8));
  if ( rep != Z_OK )
    {
      Scierror("Error: uncompression failed\n");
      nsp_serial_destroy(S);
      return NULL;
    }
  memcpy(S->val,nsp_serial_header,len);
  return S;
#else
  Scierror("Error: cannot compress with this nsp version\n");
  return NULL;
#endif 
}

static int int_serial_meth_uncompress(void *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSerial *S; 
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((S = nsp_serial_uncompress(self))== NULL)
    return RET_BUG; 
  MoveObj(stack,1,NSP_OBJECT(S)); 
  return 1;
}

static NspMethods serial_methods[] = {
  { "unserialize", int_serial_meth_unserialize},
  { "compress", int_serial_meth_compress },
  { "uncompress", int_serial_meth_uncompress },
  { (char *) 0, NULL}
};

static NspMethods *serial_get_methods(void) { return serial_methods;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/* Store a serial object in a matrix. This is potentially 
 * dangerous since changing the matrix will damage the serial 
 * object, but this is used in scicos in order to store data 
 * in a block state.
 */

NspMatrix *nsp_serial_to_matrix(const NspSerial *S)
{
  NspMatrix *A;
  int hs= strlen(nsp_serial_header);
  int n;
  if ( S->nbytes < hs || strncmp(S->val,nsp_serial_header,hs) != 0)
    {
      Scierror("Error: serial object does not contain a serialized Nsp object\n");
      return NULLMAT;
    }
  n = S->nbytes / sizeof(double);
  n += 2; 
  if ((A=nsp_matrix_create(NVOID,'r',n,1))==NULLMAT) return NULLMAT;
  A->R[0]= S->nbytes;
  memcpy(A->R +1,S->val,S->nbytes);
  return A;
}

NspSerial *nsp_matrix_to_serial(const NspMatrix *A)
{
  NspSerial *S;
  int nbytes;
  int hs= strlen(nsp_serial_header);
  nbytes = A->R[0];
  if ( (nbytes/sizeof(double))+2 != A->mn) 
    {
      Scierror("Error: Matrix argument do not contains a serialized object\n");
      return NULLSERIAL;
    }
  S = nsp_serial_create(NVOID,((const char *) (A->R+1))+hs,nbytes-hs);
  return S;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

int int_serial_unserialize(Stack stack, int rhs, int opt, int lhs)
{
  NspSerial *a;
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (( a= GetSerial(stack,1))== NULLSERIAL ) return RET_BUG;
  if ((Obj=nsp_object_unserialize(a))== NULLOBJ) return RET_BUG;
  /* take care that nsp_object_unserialize returns a new object 
   * but with a name, we have to delete the name here otherwise 
   * Obj will be copied at return and we will have unfreed memory.
   */
  if (nsp_object_set_name(Obj,NVOID) == FAIL) return RET_BUG;
  MoveObj(stack,1,Obj); 
  return 1; 
}

/* useful to creatr a buffer which can receive a serialized 
 * object as in pvm or mpi.
 */

int int_serial_create(Stack stack, int rhs, int opt, int lhs)
{
  int count;
  NspSerial *S;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ( GetScalarInt(stack,1,&count) == FAIL ) return RET_BUG;
  if (( S = nsp_serial_create(NVOID,NULL,count))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S)); 
  return 1; 
}


static OpTab Serial_func[]={
  /* {"unserialize_serial",int_serial_unserialize}, moved in object.c */
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

