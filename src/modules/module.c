/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  Module_Private 
#include "nsp/module.h"
#include "nsp/modulelt.h"
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

static char *module_type_short_string(NspObject *v)
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
  int err=0,rep;
  if ( check_cast(B,nsp_type_module_id) == FALSE) return TRUE;
  rep = module_full_comp(A,(NspModule *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int module_xdr_save(XDR  *xdrs, NspModule *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("module_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspModule  *module_xdr_load(XDR  *xdrs)
{
  NspModule *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLMODULE;
  Scierror("module_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void module_destroy(NspModule *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  FREE(H->path);
  FREE(H->mname);
  if ( H->flag  == 0) 
    {
      if ( H->T != NULL) nsp_hash_destroy(H->T);
      if ( H->L != NULL) nsp_list_destroy(H->L);
    }
  FREE(H);
}

/*
 * info 
 */

int module_info(NspModule *H, int indent,char *name,int rec_level)
{
  int i;
  if ( H == NULLMODULE) 
    {
      Sciprintf("Null Pointer Module \n");
      return TRUE;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[Module %s, path=%s mname=%s\n", NSP_OBJECT(H)->name,
	    H->path,H->mname);
  if ( H->T != NULL) nsp_hash_info(H->T,indent+2,NULL,0);
  if ( H->L != NULL)nsp_list_info(H->L,indent+2,NULL,0);
  for ( i=0 ; i < indent ; i++) Sciprintf("]\n");
  return TRUE;
}

/*
 * print 
 */

int module_print(NspModule *H, int indent,char *name, int rec_level)
{
  int i;
  if ( H == NULLMODULE) 
    {
      Sciprintf("Null Pointer Module \n");
      return TRUE;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s =\tmodule\n",NSP_OBJECT(H)->name); 
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("<Module name=%s path=%s\n",H->mname,H->path);
  if ( H->T != NULL)  nsp_hash_print(H->T,indent+2,NULL,0);
  if ( H->L != NULL)  nsp_list_print(H->L,indent+2,NULL,0);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf(">\n");
  return TRUE;

}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Module objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspModule   *module_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
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
      NspModuleElt *elt;
      int rep;
      char name[NAME_MAXL];
      rep = fscanf(f,"%s",name);
      if ( rep == 0 || rep == EOF ) break;
      if ((elt = modulelt_create(name,NULL))== NULLME ) 
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
  if ( nsp_object_set_initial_name(NSP_OBJECT(M),name) == NULL)
    return(NULLMODULE);
  NSP_OBJECT(M)->ret_pos = -1 ;
  /* specific for Module */
  if ((M->path =new_nsp_string(path))== NULLSTRING)
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  if ((M->mname =new_nsp_string(mname))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  if (M->path[0] != '\0') 
    {
      if ((M->T= nsp_hash_create(NVOID,10))== NULLHASH)
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(NULLMODULE);
	}
      /* fill Mod->T with names contained in file 
       * dir/names 
       * FIXME : could also scan dir in the future 
       */
      if (module_fill(M) == FAIL) return NULLMODULE;
    }
  if ((M->L =  nsp_list_create(NVOID))==NULLLIST) return NULLMODULE;
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

/*
 * new module which shares hash table and list 
 * with an other module 
 */

NspModule *module_copy_ref(NspModule *Mod)
{
  NspModule *M  =  new_module() ;
  if ( M ==  NULLMODULE)
    {
      Sciprintf("No more memory\n");
      return NULLMODULE;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(M),NSP_OBJECT(Mod)->name) == NULL)
    return(NULLMODULE);
  NSP_OBJECT(M)->ret_pos = -1 ;
  /* specific for Module */
  if ((M->path =new_nsp_string(Mod->path))== NULLSTRING)
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  if ((M->mname =new_nsp_string(Mod->mname))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMODULE);
    }
  M->T = Mod->T;
  M->L = Mod->L;
  M->flag=1; /* T and L are not local */
  return(M);
} 


/*-------------------------------------------------------------------
 * wrappers for the Module
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_mo_create(Stack stack, int rhs, int opt, int lhs)
{
  char *mname,*path;
  NspModule *H;
  /* first argument is a unused its a NspType */
  CheckRhs(2,2);
  if ((path = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((mname = GetString(stack,2)) == (char*)0) return RET_BUG;
  /* we first create a default object */
  if(( H = module_create(NVOID,path,mname,NULL)) == NULLMODULE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static AttrTab module_attrs[] = {
  { (char *) 0, NULL, NULL , NULL , NULL }
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
  nsp_object_print((NspObject *) a,0,NULL,0);
  return 0;
}

/**************************************************
 * insert 
 **************************************************/

int int_mo_insertlast(Stack stack, int rhs, int opt, int lhs)
{

  NspList *H;
  NspSMatrix *Mname; 
  char *dir;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((dir=GetString(stack,2))== (char *) 0 ) return RET_BUG;     
  if ((Mname = GetSMat(stack,3))  == NULLSMAT) return RET_BUG;
  if ( nsp_insert_module_last(H,dir,Mname->S) == FAIL) 
    {
      Scierror("Error:List insertion failed\n");
      return RET_BUG;
    }
  return 0;
}

/**************************************************
 * search 
 **************************************************/

int int_mo_search(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  NspList *H;
  NspSMatrix *Mname; 
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((H = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((Mname = GetSMat(stack,2))  == NULLSMAT) return RET_BUG;
  if ((O= nsp_module_search_name(H,Mname->S)) == NULLOBJ) 
    {
      Scierror("Error:List search failed\n");
      return RET_BUG;
    }
  MoveObj(stack,1,O);
  return 1;
}

/**************************************************
 * import 
 **************************************************/

int int_mo_import(Stack stack, int rhs, int opt, int lhs)
{
  NspList *H;
  NspSMatrix *Mname; 
  char *dir;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((dir=GetString(stack,2))== (char *) 0 ) return RET_BUG;     
  if ((Mname = GetSMat(stack,3))  == NULLSMAT) return RET_BUG;
  if ( nsp_module_import(H,dir,Mname->S) == FAIL ) 
    {
      Scierror("Error:List import failed\n");
      return RET_BUG;
    }
  return 0;
}



/**************************************************
 * search for a full name using a path variable 
 * and dynamically add modules in the lmo variable 
 * during the search 
 * ==> If object is found it is loaded in the 
 *     current env 
 **************************************************/

int int_mo_pathsearch(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O1,*O;
  NspList *H;
  NspSMatrix *Mname,*path;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((path = GetSMat(stack,2))  == NULLSMAT) return RET_BUG;
  if ((Mname = GetSMat(stack,3))  == NULLSMAT) return RET_BUG;
  /*if XXX ((O= lmo_path_search_name(H,path,Mname->S)) == NULLOBJ) */
  if ((O =nsp_create_true_object(NVOID))== NULLOBJ) return RET_BUG;
  if ((O1= module_path_search_object(H,path,Mname->S)) == NULLOBJ) 
    {
      ((NspBMatrix *)O)->B[0] = FALSE;
    }
  MoveObj(stack,1,O);
  return 1;
}


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Module_func[]={
  {"setrowscols_mo",int_set_attribute},/* a(xxx)= b */
  /* {"test_mo",int_mo_test},*/
  {"mocreate",int_mo_create},
  {"insert",int_mo_insertlast},  		
  {"search",int_mo_search},  		
  {"pathsearch",int_mo_pathsearch},  		
  {"import",int_mo_import},  		
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

