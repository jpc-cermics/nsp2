/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  ModuleElt_Private 
#include "nsp/modulelt.h"
#include "nsp/interf.h"

/* 
 * NspModuleElt inherits from NspObject
 */

int nsp_type_modulelt_id=0;
NspTypeModuleElt *nsp_type_modulelt=NULL;

/*
 * Type object for ModuleElt 
 * all the instance of NspTypeModuleElt share the same id. 
 * nsp_type_modulelt: is an instance of NspTypeModuleElt 
 *    used for objects of NspModuleElt type (i.e built with new_modulelt) 
 * other instances are used for derived classes 
 */

NspTypeModuleElt *new_type_modulelt(type_mode mode)
{
  NspTypeModuleElt *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_modulelt != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_modulelt;
    }
  
  if ((type =  malloc(sizeof(NspTypeModuleElt))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = modulelt_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = modulelt_get_methods; 
  type->new = (new_func *) new_modulelt;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for modulelt */ 
  
  top->pr = (print_func *) modulelt_print;                    
  top->dealloc = (dealloc_func *) modulelt_destroy;
  top->copy  =  (copy_func *) modulelt_copy;                   
  top->size  = (size_func *) modulelt_size;                  
  top->s_type =  (s_type_func *) modulelt_type_as_string;    
  top->sh_type = (sh_type_func *) modulelt_type_short_string;
  top->info = (info_func *) modulelt_info ;                    
  /* top->is_true = (is_true_func  *) ModuleEltIsTrue; */
  /* top->loop =(loop_func *) modulelt_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) modulelt_object;
  top->eq  = (eq_func *) modulelt_eq;
  top->neq  = (eq_func *) modulelt_neq;
  top->save  = (save_func *) modulelt_xdr_save;
  top->load  = (load_func *) modulelt_xdr_load;
  top->create = (create_func*) int_me_create;

  /* specific methods for modulelt */
      
  type->init = (init_func *) init_modulelt;
      
  /* 
   * ModuleElt interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_modulelt_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeModuleElt called nsp_type_modulelt
       */
      type->id =  nsp_type_modulelt_id = nsp_new_type_id();
      nsp_type_modulelt = type;
      if ( nsp_register_type(nsp_type_modulelt) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_modulelt(mode);
    }
  else 
    {
      type->id = nsp_type_modulelt_id;
      return type;
    }
}

/*
 * initialize ModuleElt instances 
 * locally and by calling initializer on parent class 
 */

static int init_modulelt(NspModuleElt *o,NspTypeModuleElt *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of ModuleElt 
 */

NspModuleElt *new_modulelt() 
{
  NspModuleElt *loc; 
  /* type must exists */
  nsp_type_modulelt = new_type_modulelt(T_BASE);
  if ( (loc = malloc(sizeof(NspModuleElt)))== NULLME) return loc;
  /* initialize object */
  if ( init_modulelt(loc,nsp_type_modulelt) == FAIL) return NULLME;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ModuleElt 
 *-----------------------------------------------*/

/*
 * size 
 */

static int modulelt_size(NspModuleElt *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char modulelt_type_name[]="ModuleElt";
static char modulelt_short_type_name[]="me";

static char *modulelt_type_as_string(void)
{
  return(modulelt_type_name);
}

static char *modulelt_type_short_string(void)
{
  return(modulelt_short_type_name);
}

static int modulelt_full_comp(NspModuleElt * A,NspModuleElt * B,char *op,int *err)
{
  Scierror("modulelt_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int modulelt_eq(NspModuleElt *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_modulelt_id) == FALSE) return FALSE ;
  rep = modulelt_full_comp(A,(NspModuleElt *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int modulelt_neq(NspModuleElt *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_modulelt_id) == FALSE) return TRUE;
  rep = modulelt_full_comp(A,(NspModuleElt *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int modulelt_xdr_save(XDR  *xdrs, NspModuleElt *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("modulelt_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspModuleElt  *modulelt_xdr_load(XDR  *xdrs)
{
  NspModuleElt *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLME;
  Scierror("modulelt_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void modulelt_destroy(NspModuleElt *H)
{
  FREE(NSP_OBJECT(H)->name);
  FREE(H);
}

/*
 * info 
 */

void modulelt_info(NspModuleElt *H, int indent)
{
  int i;
  if ( H == NULLME) 
    {
      Sciprintf("Null Pointer ModuleElt \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[ModuleElt %s]\n", NSP_OBJECT(H)->name);
}

/*
 * print 
 */

void modulelt_print(NspModuleElt *H, int indent)
{
  modulelt_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ModuleElt objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspModuleElt   *modulelt_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast(O,nsp_type_modulelt_id) == TRUE) return ((NspModuleElt *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_modulelt));
  return(NULL);
}

int IsModuleEltObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_modulelt_id);
}

int IsModuleElt(NspObject *O)
{
  return nsp_object_type(O,nsp_type_modulelt_id);
}

NspModuleElt  *GetModuleEltCopy(Stack stack, int i)
{
  if (  GetModuleElt(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspModuleElt  *GetModuleElt(Stack stack, int i)
{
  NspModuleElt *M;
  if (( M = modulelt_object(NthObj(i))) == NULLME)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspModuleElt instance 
 *-----------------------------------------------------*/

NspModuleElt *modulelt_create(char *name,NspTypeBase *type)
{
  NspModuleElt *H  = (type == NULL) ? new_modulelt() : type->new();
  if ( H ==  NULLME)
    {
      Sciprintf("No more memory\n");
      return NULLME;
    }
  if ( ( NSP_OBJECT(H)->name =new_nsp_string(name)) == NULLSTRING) return(NULLME);
  NSP_OBJECT(H)->ret_pos = -1 ;
  return H;
}

/*
 * copy 
 */

NspModuleElt *modulelt_copy(NspModuleElt *H)
{
  return modulelt_create(NVOID,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the ModuleElt
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_me_create(Stack stack, int rhs, int opt, int lhs)
{
  NspModuleElt *H;
  /* first argument is a unused its a NspType */
  CheckRhs(1,100);
  /* we first create a default object */
  if(( H = modulelt_create(NVOID,NULL)) == NULLME) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static AttrTab modulelt_attrs[] = {
  { (char *) 0, NULL}
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/


static NspMethods modulelt_methods[] = {
  { (char *) 0, NULL}
};

static NspMethods *modulelt_get_methods(void) { return modulelt_methods;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_me_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspModuleElt *a;
  if (( a= GetModuleElt(stack,1))== NULLME) return RET_BUG;
  nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ModuleElt_func[]={
  {"setrowscols_me",int_set_attribute},/* a(xxx)= b */
  {"test_me",int_me_test},
  {(char *) 0, NULL}
};

/* call ith function in the ModuleElt interface */

int ModuleElt_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ModuleElt_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ModuleElt_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ModuleElt_func[i].name;
  *f = ModuleElt_func[i].fonc;
}

