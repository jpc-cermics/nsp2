/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2004 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#define  ClassB_Private 
#include "nsp/object.h"
#include "nsp/interf.h"

/* 
 * NspClassB inherits from NspClassA
 */

int nsp_type_classb_id=0;
NspTypeClassB *nsp_type_classb=NULL;

/*
 * Type object for ClassB 
 * all the instance of NspTypeClassB share the same id. 
 * nsp_type_classb: is an instance of NspTypeClassB 
 *    used for objects of NspClassB type (i.e built with new_classb) 
 * other instances are used for derived classes 
 */

NspTypeClassB *new_type_classb(type_mode mode)
{
  NspTypeClassB *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classb != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classb;
    }
  
  if ((type =  malloc(sizeof(NspTypeClassB))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_classa(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classb_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = classb_get_methods; 
  type->new = (new_func *) new_classb;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classb */ 
  
  top->pr = (print_func *) classb_print;                    
  top->dealloc = (dealloc_func *) classb_destroy;
  top->copy  =  (copy_func *) classb_copy;                   
  top->size  = (size_func *) classb_size;                  
  top->s_type =  (s_type_func *) classb_type_as_string;    
  top->sh_type = (sh_type_func *) classb_type_short_string;
  top->info = (info_func *) classb_info ;                    
  /* top->is_true = (is_true_func  *) ClassBIsTrue; */
  /* top->loop =(loop_func *) classb_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) classb_object;
  top->eq  = (eq_func *) classb_eq;
  top->neq  = (eq_func *) classb_neq;
  top->save  = (save_func *) classb_xdr_save;
  top->load  = (load_func *) classb_xdr_load;
  top->create = (create_func*) int_clb_create;

  /* specific methods for classb */
      
  type->init = (init_func *) init_classb;
      
  /* 
   * ClassB interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_classb_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassB called nsp_type_classb
       */
      type->id =  nsp_type_classb_id = nsp_new_type_id();
      nsp_type_classb = type;
      if ( nsp_register_type(nsp_type_classb) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classb(mode);
    }
  else 
    {
      type->id = nsp_type_classb_id;
      return type;
    }
}

/*
 * initialize ClassB instances 
 * locally and by calling initializer on parent class 
 */

static int init_classb(NspClassB *o,NspTypeClassB *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* FIXME : specific */
  o->classb_val = nsp_matrix_create("val",'r',0,0);
  if ( o->classb_val == NULLMAT) return FAIL;
  return OK;
}

/*
 * new instance of ClassB 
 */

NspClassB *new_classb() 
{
  NspClassB *loc; 
  /* type must exists */
  nsp_type_classb = new_type_classb(T_BASE);
  if ( (loc = malloc(sizeof(NspClassB)))== NULLCLB) return loc;
  /* initialize object */
  if ( init_classb(loc,nsp_type_classb) == FAIL) return NULLCLB;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassB 
 *-----------------------------------------------*/

/*
 * size 
 */

static int classb_size(NspClassB *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char classb_type_name[]="ClassB";
static char classb_short_type_name[]="clb";

static char *classb_type_as_string(void)
{
  return(classb_type_name);
}

static char *classb_type_short_string(void)
{
  return(classb_short_type_name);
}

static int classb_full_comp(NspClassB * A,NspClassB * B,char *op,int *err)
{
  Scierror("classb_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int classb_eq(NspClassB *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_classb_id) == FALSE) return FALSE ;
  rep = classb_full_comp(A,(NspClassB *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int classb_neq(NspClassB *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_classb_id) == FALSE) return TRUE;
  rep = classb_full_comp(A,(NspClassB *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int classb_xdr_save(NspFile  *F, NspClassB *M)
{
  if (nsp_xdr_save_i(F,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("classb_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspClassB  *classb_xdr_load(NspFile  *F)
{
  NspClassB *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F,name,NAME_MAXL) == FAIL) return NULLCLB;
  Scierror("classb_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void classb_destroy(NspClassB *H)
{
  FREE(NSP_OBJECT(H)->name);
  FREE(H);
}

/*
 * info 
 */

void classb_info(NspClassB *H, int indent)
{
  int i;
  if ( H == NULLCLB) 
    {
      Sciprintf("Null Pointer ClassB \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[ClassB %s, col=%d th=%d]\n", NSP_OBJECT(H)->name,
	    H->classb_color,H->classb_thickness);
}

/*
 * print 
 */

void classb_print(NspClassB *H, int indent)
{
  classb_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassB objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspClassB   *classb_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast(O,nsp_type_classb_id) == TRUE) return ((NspClassB *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_classb));
  return(NULL);
}

int IsClassBObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classb_id);
}

int IsClassB(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classb_id);
}

NspClassB  *GetClassBCopy(Stack stack, int i)
{
  if (  GetClassB(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassB  *GetClassB(Stack stack, int i)
{
  NspClassB *M;
  if (( M = classb_object(NthObj(i))) == NULLCLB)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

NspClassB *classb_create(char *name,int color,int thickness,NspTypeBase *type)
{
  NspClassB *H  = (type == NULL) ? new_classb() : type->new();
  if ( H ==  NULLCLB)
    {
      Sciprintf("No more memory\n");
      return NULLCLB;
    }
  if ( ( NSP_OBJECT(H)->name = NewString(name)) == NULLSTRING) return(NULLCLB);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->classb_color = color;
  H->classb_thickness = thickness;
  return H;
}

/*
 * copy 
 */

NspClassB *classb_copy(NspClassB *H)
{
  return classb_create(NVOID,H->classb_color,H->classb_thickness,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the ClassB
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_clb_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassB *H;
  int color=-1,thickness=-1;
  /* first argument is a unused its a NspType */
  CheckRhs(1,100);
  /* we first create a default object */
  if(( H = classb_create(NVOID,color,thickness,NULL)) == NULLCLB) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_clb_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspClassB *) Hv)->classb_color);
}

static int int_clb_set_color(void *Hv, char *attr, NspObject *O)
{
  int color; 
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspClassB *)Hv)->classb_color = color;
  return OK ;
}

static NspObject * int_clb_get_thickness(void *Hv, char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspClassB *) Hv)->classb_thickness);
}

static int int_clb_set_thickness(void *Hv, char *attr, NspObject *O)
{
  int thickness; 
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspClassB *) Hv)->classb_thickness = thickness;
  return OK ;
}

static NspObject * int_clb_get_val(void *Hv,char *attr)
{
  return (NspObject *) ((NspClassB *)Hv)->classb_val;
}

static NspObject *int_clb_get_object_val(void *Hv,char *str)
{
  return (NspObject *)  ((NspClassB *)Hv)->classb_val;
}

static int int_clb_set_val(void *Hv, char *attr, NspObject *O)
{
  NspMatrix *m;
  if ((m = (NspMatrix *) nsp_object_copy(O)) == NULLMAT) return RET_BUG;
  ((NspClassB *)Hv)->classb_val = m;
  return OK ;
}

static AttrTab classb_attrs[] = {
  { "clb_color", 	int_clb_get_color , 	int_clb_set_color , 	NULL },
  { "clb_thickness",int_clb_get_thickness, 	int_clb_set_thickness,	NULL },
  { "clb_val", 	int_clb_get_val, 	int_clb_set_val, 	int_clb_get_object_val },
  { (char *) 0, NULL}
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static int int_clb_classb_color_change(void *a,Stack stack,int rhs,int opt,int lhs)
{
  int color; 
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&color) == FAIL) return RET_BUG;
  ((NspClassB *) a)->classb_color = color;
  return 0;
}

static int int_clb_classb_color_show(void *a,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  CheckLhs(1,1);
  Sciprintf("color of %s is %d\n",NSP_OBJECT(a)->name,((NspClassB *) a)->classb_color);
  return 0;
}

static NspMethods classb_methods[] = {
  { "classb_color_change", int_clb_classb_color_change},
  { "classb_color_show",   int_clb_classb_color_show},
  { (char *) 0, NULL}
};

static NspMethods *classb_get_methods(void) { return classb_methods;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_clb_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspClassB *a;
  if (( a= GetClassB(stack,1))== NULLCLB) return RET_BUG;
  nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassB_func[]={
  {"setrowscols_clb",int_set_attribute},/* a(xxx)= b */
  {"test_clb",int_clb_test},
  {(char *) 0, NULL}
};

/* call ith function in the ClassB interface */

int ClassB_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassB_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassB_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassB_func[i].name;
  *f = ClassB_func[i].fonc;
}

