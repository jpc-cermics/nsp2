/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  Module_Private 
#include "module.h"
#include "nsp/interf.h"

/* 
 * NspModule inherits from NspObject
 */

int nsp_type_module_id=0;
NspTypeModule *nsp_type_module=NULL;

/*
 * Type object for Module 
 * all the instance of NspTypeModule share the same id. 
 * nsp_type_module: is an instance of NspTypeModule 
 *    used for objects of NspModule type (i.e built with new_module) 
 * other instances are used for derived classes 
 */

NspTypeModule *new_type_module(type_mode mode)
{
  NspTypeModule *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_module != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_module;
    }
  
  if ((type =  malloc(sizeof(NspTypeModule))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = module_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = module_get_methods; 
  type->new = (new_func *) new_module;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for module */ 
  
  top->pr = (print_func *) module_print;                    
  top->dealloc = (dealloc_func *) module_destroy;
  top->copy  =  (copy_func *) module_copy;                   
  top->size  = (size_func *) module_size;                  
  top->s_type =  (s_type_func *) module_type_as_string;    
  top->sh_type = (sh_type_func *) module_type_short_string;
  top->info = (info_func *) module_info ;                    
  /* top->is_true = (is_true_func  *) ModuleIsTrue; */
  /* top->loop =(loop_func *) module_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) module_object;
  top->eq  = (eq_func *) module_eq;
  top->neq  = (eq_func *) module_neq;
  top->save  = (save_func *) module_xdr_save;
  top->load  = (load_func *) module_xdr_load;
  top->create = (create_func*) int_mo_create;

  /* specific methods for module */
      
  type->init = (init_func *) init_module;
      
  /* 
   * Module interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_module_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeModule called nsp_type_module
       */
      type->id =  nsp_type_module_id = nsp_new_type_id();
      nsp_type_module = type;
      if ( nsp_register_type(nsp_type_module) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_module(mode);
    }
  else 
    {
      type->id = nsp_type_module_id;
      return type;
    }
}

/*
 * initialize Module instances 
 * locally and by calling initializer on parent class 
 */

static int init_module(NspModule *o,NspTypeModule *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  o->path = NULL;
  o->mname= NULL;
  o->T = NULL;
  o->L = NULL;
  o->flag = 0;
  return OK;
}

/*
 * new instance of Module 
 */

NspModule *new_module() 
{
  NspModule *loc; 
  /* type must exists */
  nsp_type_module = new_type_module(T_BASE);
  if ( (loc = malloc(sizeof(NspModule)))== NULLMODULE) return loc;
  /* initialize object */
  if ( init_module(loc,nsp_type_module) == FAIL) return NULLMODULE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Module 
 *-----------------------------------------------*/

/*
 * size 
 */

static int module_size(NspModule *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char module_type_name[]="Module";
static char module_short_type_name[]="mo";

static char *module_type_as_string(void)
{
  return(module_type_name);
}

static char *module_type_short_string(void)
{
  return(module_short_type_name);
}

static int module_full_comp(NspModule * A,NspModule * B,char *op,int *err)
{
  Scierror("module_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int module_eq(NspModule *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_module_id) == FALSE) return FALSE ;
  rep = module_full_comp(A,(NspModule *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int module_neq(NspModule *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_module_id) == FALSE) return TRUE;
  rep = module_full_comp(A,(NspModule *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int module_xdr_save(NspFile  *F, NspModule *M)
{
  if ( XdrSaveI(F,M->type->id) == FAIL) return FAIL;
  if ( XdrSaveString(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("module_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspModule  *module_xdr_load(NspFile  *F)
{
  NspModule *M = NULL;
  static char name[NAME_MAXL];
  if ( XdrLoadString(F,name,NAME_MAXL) == FAIL) return NULLMODULE;
  Scierror("module_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void module_destroy(NspModule *H)
{
  FREE(NSP_OBJECT(H)->name);
  FREE(H->path);
  FREE(H->mname);
  if ( H->flag  == 0) 
    {
      nsp_hash_destroy(H->T);
      if ( H->L != NULL) nsp_list_destroy(H->L);
    }
  FREE(H);
}

/*
 * info 
 */

void module_info(NspModule *H, int indent)
{
  int i;
  if ( H == NULLMODULE) 
    {
      Sciprintf("Null Pointer Module \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[Module %s, path=%s mname=%s\n", NSP_OBJECT(H)->name,
	    H->path,H->mname);
  hash_info(H->T,indent+2);
  nsp_list_info(H->L,indent+2);
  for ( i=0 ; i < indent ; i++) Sciprintf("]\n");
}

/*
 * print 
 */

void module_print(NspModule *H, int indent)
{
  int i;
  if ( H == NULLMODULE) 
    {
      Sciprintf("Null Pointer Module \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s =\tmodule\n",NSP_OBJECT(H)->name); 
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("<Hdule name=%s path=%s\n",H->mname,H->path);
  hash_print(H->T,indent+2);
  nsp_list_print(H->L,indent+2);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf(">\n");

}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Module objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspModule   *module_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast(O,nsp_type_module_id) == TRUE) return ((NspModule *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_module));
  return(NULL);
}

int IsModuleObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_module_id);
}

int IsModule(NspObject *O)
{
  return nsp_object_type(O,nsp_type_module_id);
}

NspModule  *GetModuleCopy(Stack stack, int i)
{
  if (  GetModule(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspModule  *GetModule(Stack stack, int i)
{
  NspModule *M;
  if (( M = module_object(NthObj(i))) == NULLMODULE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspModule instance 
 *    mname is the module name 
 *    path the path leading to the module (it should be an absolute path)
 *    The table is filled with the contents of names 
 *    The list of sub modules is empty 
 *--------------------------------------------------------*/

/*********************************************************************
 * fill a module hash table 
 * using file: names 
 * or scanning the directory for .bin files  XXXXX 
 *********************************************************************/

/** XXXXX remplacer 1048 par une macro **/

#define F_SIZE 1048

static int module_fill(NspModule *M)
{
  char F[F_SIZE];
  FILE *f;
  strcpy(F,M->path); 
  strcat(F,"/names");
  if (( f= fopen(F,"r") ) == (FILE *)0 )
    {
      Scierror("Error:\t:Can't open file %s\n",F);
      return FAIL;
    }
  while (1) 
    {
      NspMe *elt;
      int rep;
      char name[NAME_MAXL];
      rep = fscanf(f,"%s",name);
      if ( rep == 0 || rep == EOF ) break;
      if ((elt = MeCreate(name))== NULLME ) 
	{
	  fclose(f); return FAIL;
	}
      if (nsp_hash_enter(M->T,(NspObject *) elt) == FAIL) 
	{
	  fclose(f); return FAIL;
	}
    }
  fclose(f);
  return OK;
}

NspModule *module_create(char *name,const char *path,const char *mname,NspTypeBase *type)
{
  NspModule *M  = (type == NULL) ? new_module() : type->new();
  if ( M ==  NULLMODULE)
    {
      Sciprintf("No more memory\n");
      return NULLMODULE;
    }
  if ( ( NSP_OBJECT(M)->name = NewString(name)) == NULLSTRING) return(NULLMODULE);
  NSP_OBJECT(M)->ret_pos = -1 ;
  /* specific for Module */
  if ((M->path = NewString(path))== NULLSTRING)
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  if ((M->mname = NewString(mname))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  if ((M->T= nsp_hash_create(NVOID,10))== NULLHASH) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  /* fill Mod->T with names contained in file 
   * dir/names 
   * FIXME : could also scan dir in the future 
   */
  if ( module_fill(M) == FAIL) return NULLMODULE;
  M->L = NULL;
  M->flag=0;
  return(M);
} 

/*
 * real copy 
 */

NspModule *module_copy(NspModule *H)
{
  return module_create(NVOID,H->path,H->mname,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the Module
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_mo_create(Stack stack, int rhs, int opt, int lhs)
{
  NspModule *H;
  /* first argument is a unused its a NspType */
  CheckRhs(1,100);
  /* we first create a default object */
  if(( H = module_create(NVOID,"","",NULL)) == NULLMODULE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static AttrTab module_attrs[] = {
  { (char *) 0, NULL}
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods module_methods[] = {
  { (char *) 0, NULL}
};

static NspMethods *module_get_methods(void) { return module_methods;};

/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_mo_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspModule *a;
  if (( a= GetModule(stack,1))== NULLMODULE) return RET_BUG;
  nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Module_func[]={
  {"setrowscols_mo",int_set_attribute},/* a(xxx)= b */
  /* {"test_mo",int_mo_test},*/
  {"mocreate",int_mo_create},
  {(char *) 0, NULL}
};

/* call ith function in the Module interface */

int Module_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Module_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Module_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Module_func[i].name;
  *f = Module_func[i].fonc;
}

