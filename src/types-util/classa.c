/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2004 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  ClassA_Private 
#include "classa.h"
#include "nsp/interf.h"

/* 
 * NspClassA inherits from NspObject
 */

int nsp_type_classa_id=0;
NspTypeClassA *nsp_type_classa=NULL;

/*
 * Type object for ClassA 
 * all the instance of NspTypeClassA share the same id. 
 * nsp_type_classa: is an instance of NspTypeClassA 
 *    used for objects of NspClassA type (i.e built with new_classa) 
 * other instances are used for derived classes 
 */

NspTypeClassA *new_type_classa(type_mode mode)
{
  NspTypeClassA *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classa != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classa;
    }
  
  if ((type =  malloc(sizeof(NspTypeClassA))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classa_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = classa_get_methods; 
  type->new = (new_func *) new_classa;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classa */ 
  
  top->pr = (print_func *) classa_print;                    
  top->dealloc = (dealloc_func *) classa_destroy;
  top->copy  =  (copy_func *) classa_copy;                   
  top->size  = (size_func *) classa_size;                  
  top->s_type =  (s_type_func *) classa_type_as_string;    
  top->sh_type = (sh_type_func *) classa_type_short_string;
  top->info = (info_func *) classa_info ;                    
  /* top->is_true = (is_true_func  *) ClassAIsTrue; */
  /* top->loop =(loop_func *) classa_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) classa_object;
  top->eq  = (eq_func *) classa_eq;
  top->neq  = (eq_func *) classa_neq;
  top->save  = (save_func *) classa_xdr_save;
  top->load  = (load_func *) classa_xdr_load;
  top->create = (create_func*) int_cla_create;

  /* specific methods for classa */
      
  type->init = (init_func *) init_classa;
      
  /* 
   * ClassA interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_classa_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassA called nsp_type_classa
       */
      type->id =  nsp_type_classa_id = nsp_new_type_id();
      nsp_type_classa = type;
      if ( nsp_register_type(nsp_type_classa) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classa(mode);
    }
  else 
    {
      type->id = nsp_type_classa_id;
      return type;
    }
}

/*
 * initialize ClassA instances 
 * locally and by calling initializer on parent class 
 */

static int init_classa(NspClassA *o,NspTypeClassA *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* FIXME : specific */
  o->classa_val = nsp_matrix_create("val",'r',0,0);
  if ( o->classa_val == NULLMAT) return FAIL;
  return OK;
}

/*
 * new instance of ClassA 
 */

NspClassA *new_classa() 
{
  NspClassA *loc; 
  /* type must exists */
  nsp_type_classa = new_type_classa(T_BASE);
  if ( (loc = malloc(sizeof(NspClassA)))== NULLCLA) return loc;
  /* initialize object */
  if ( init_classa(loc,nsp_type_classa) == FAIL) return NULLCLA;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassA 
 *-----------------------------------------------*/

/*
 * size 
 */

static int classa_size(NspClassA *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char classa_type_name[]="ClassA";
static char classa_short_type_name[]="cla";

static char *classa_type_as_string(void)
{
  return(classa_type_name);
}

static char *classa_type_short_string(void)
{
  return(classa_short_type_name);
}

static int classa_full_comp(NspClassA * A,NspClassA * B,char *op,int *err)
{
  Scierror("classa_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int classa_eq(NspClassA *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_classa_id) == FALSE) return FALSE ;
  rep = classa_full_comp(A,(NspClassA *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int classa_neq(NspClassA *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_classa_id) == FALSE) return TRUE;
  rep = classa_full_comp(A,(NspClassA *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int classa_xdr_save(NspFile  *F, NspClassA *M)
{
  if (nsp_xdr_save_i(F,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("classa_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspClassA  *classa_xdr_load(NspFile  *F)
{
  NspClassA *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F,name,NAME_MAXL) == FAIL) return NULLCLA;
  Scierror("classa_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void classa_destroy(NspClassA *H)
{
  FREE(NSP_OBJECT(H)->name);
  FREE(H);
}

/*
 * info 
 */

void classa_info(NspClassA *H, int indent)
{
  int i;
  if ( H == NULLCLA) 
    {
      Sciprintf("Null Pointer ClassA \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[ClassA %s, col=%d th=%d]\n", NSP_OBJECT(H)->name,
	    H->classa_color,H->classa_thickness);
}

/*
 * print 
 */

void classa_print(NspClassA *H, int indent)
{
  classa_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspClassA   *classa_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast(O,nsp_type_classa_id) == TRUE) return ((NspClassA *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_classa));
  return(NULL);
}

int IsClassAObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classa_id);
}

int IsClassA(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classa_id);
}

NspClassA  *GetClassACopy(Stack stack, int i)
{
  if (  GetClassA(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassA  *GetClassA(Stack stack, int i)
{
  NspClassA *M;
  if (( M = classa_object(NthObj(i))) == NULLCLA)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

NspClassA *classa_create(char *name,int color,int thickness,NspTypeBase *type)
{
  NspClassA *H  = (type == NULL) ? new_classa() : type->new();
  if ( H ==  NULLCLA)
    {
      Sciprintf("No more memory\n");
      return NULLCLA;
    }
  if ( ( NSP_OBJECT(H)->name = NewString(name)) == NULLSTRING) return(NULLCLA);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->classa_color = color;
  H->classa_thickness = thickness;
  return H;
}

/*
 * copy 
 */

NspClassA *classa_copy(NspClassA *H)
{
  return classa_create(NVOID,H->classa_color,H->classa_thickness,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the ClassA
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_cla_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassA *H;
  int color=-1,thickness=-1;
  /* first argument is a unused its a NspType */
  CheckRhs(1,100);
  /* we first create a default object */
  if(( H = classa_create(NVOID,color,thickness,NULL)) == NULLCLA) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_cla_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspClassA *) Hv)->classa_color);
}

static int int_cla_set_color(void *Hv, char *attr, NspObject *O)
{
  int color; 
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspClassA *)Hv)->classa_color = color;
  return OK ;
}

static NspObject * int_cla_get_thickness(void *Hv, char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspClassA *) Hv)->classa_thickness);
}

static int int_cla_set_thickness(void *Hv, char *attr, NspObject *O)
{
  int thickness; 
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspClassA *) Hv)->classa_thickness = thickness;
  return OK ;
}

static NspObject * int_cla_get_val(void *Hv,char *attr)
{
  return (NspObject *) ((NspClassA *)Hv)->classa_val;
}

static NspObject *int_cla_get_object_val(void *Hv,char *str)
{
  return (NspObject *)  ((NspClassA *)Hv)->classa_val;
}

static int int_cla_set_val(void *Hv, char *attr, NspObject *O)
{
  NspMatrix *m;
  if ((m = (NspMatrix *) nsp_object_copy(O)) == NULLMAT) return RET_BUG;
  ((NspClassA *)Hv)->classa_val = m;
  return OK ;
}

static AttrTab classa_attrs[] = {
  { "cla_color", 	int_cla_get_color , 	int_cla_set_color , 	NULL },
  { "cla_thickness",int_cla_get_thickness, 	int_cla_set_thickness,	NULL },
  { "cla_val", 	int_cla_get_val, 	int_cla_set_val, 	int_cla_get_object_val },
  { (char *) 0, NULL}
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static int int_cla_classa_color_change(void *a,Stack stack,int rhs,int opt,int lhs)
{
  int color; 
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,2,&color) == FAIL) return RET_BUG;
  ((NspClassA *) a)->classa_color = color;
  NSP_OBJECT(a)->ret_pos = 1;
  return 1;
}

static int int_cla_classa_color_show(void *a,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  Sciprintf("color of %s is %d\n",NSP_OBJECT(a)->name,((NspClassA *) a)->classa_color);
  return 0;
}

static int int_cla_set(void *a,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(1,1000);
  CheckLhs(1,1);
  return int_set_attributes(stack,rhs,opt,lhs);
}


static NspMethods classa_methods[] = {
  { "classa_color_change", int_cla_classa_color_change},
  { "classa_color_show",   int_cla_classa_color_show},
  { "set",  int_cla_set},
  { (char *) 0, NULL}
};

static NspMethods *classa_get_methods(void) { return classa_methods;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_cla_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspClassA *a;
  if (( a= GetClassA(stack,1))== NULLCLA) return RET_BUG;
  nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassA_func[]={
  {"setrowscols_cla",int_set_attribute},/* a(xxx)= b */
  {"test_cla",int_cla_test},
  {(char *) 0, NULL}
};

/* call ith function in the ClassA interface */

int ClassA_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassA_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassA_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassA_func[i].name;
  *f = ClassA_func[i].fonc;
}

