/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  ClassC_Private 
#include "classc.h"
#include "nsp/interf.h"

/* 
 * NspClassC inherits from NspClassA
 * ClassC only adds methods to ClassA 
 * we try here to find a very short code 
 * sequence to implement ClassC 
 * the only pb is that when you cast a NspClassC to a NspClassA 
 * the type is wrong it is a subtype of NspClassA 
 */

int nsp_type_classc_id=0;
NspTypeClassC *nsp_type_classc=NULL;

NspTypeClassC *new_type_classc(type_mode mode)
{
  NspTypeClassC *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classc != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classc;
    }
  if ((type =  malloc(sizeof(NspTypeClassC))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL;/* classc_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = classc_get_methods; 
  type->new = (new_func *) new_classc;

  /* specific methods for classc */
      
  type->init = (init_func *) init_classc;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classc */ 

  top->s_type =  (s_type_func *) classc_type_as_string;    
  top->sh_type = (sh_type_func *) classc_type_short_string;
  
  /* specific methods for classc */
      
  type->init = (init_func *) init_classc;

  if ( nsp_type_classc_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassC called nsp_type_classc
       */
      type->id =  nsp_type_classc_id = nsp_new_type_id();
      nsp_type_classc = type;
      if ( nsp_register_type(nsp_type_classc) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classc(mode);
    }
  else 
    {
      type->id = nsp_type_classc_id;
      return type;
    }
}

/*
 * initialize ClassC instances 
 * locally and by calling initializer on parent class 
 */

static int init_classc(NspClassC *o,NspTypeClassC *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->surtype->init(&o->father,type->surtype->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of ClassC 
 */

NspClassC *new_classc() 
{
  NspClassC *loc; 
  /* type must exists */
  nsp_type_classc = new_type_classc(T_BASE);
  if ( (loc = malloc(sizeof(NspClassC)))== NULLCLC) return loc;
  /* initialize object */
  if ( init_classc(loc,nsp_type_classc) == FAIL) return NULLCLC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassC 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char classc_type_name[]="ClassC";
static char classc_short_type_name[]="clc";

static char *classc_type_as_string(void)
{
  return(classc_type_name);
}

static char *classc_type_short_string(void)
{
  return(classc_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassC objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspClassC   *classc_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_classc_id) == TRUE) return ((NspClassC *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_classc));
  return(NULL);
}

int IsClassCObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classc_id);
}

int IsClassC(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classc_id);
}

NspClassC  *GetClassCCopy(Stack stack, int i)
{
  if (  GetClassC(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassC  *GetClassC(Stack stack, int i)
{
  NspClassC *M;
  if (( M = classc_object(NthObj(i))) == NULLCLC)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy 
 */

NspClassC *classc_copy(NspClassC *H)
{
  return classa_create(NVOID,H->classa_color,H->classa_thickness,(NspTypeBase *) nsp_type_classc);
}

/*-------------------------------------------------------------------
 * wrappers for the ClassC
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassC *H;
  int color=-1,thickness=-1;

  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(0,2);
  if ( get_optional_args(stack,rhs,opt,opts,&color,&thickness) == FAIL) return RET_BUG;
  /* want to be sure that type classc is initialized */
  nsp_type_classc = new_type_classc(T_BASE);
  if(( H = classa_create(NVOID,color,thickness,(NspTypeBase *) nsp_type_classc)) == NULLCLC) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods classc_methods[] = {
  { (char *) 0, NULL}
};

static NspMethods *classc_get_methods(void) { return classc_methods;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassC_func[]={
  /* #include "classc-in.nam" */ 
  {"clc_create",int_clc_create}, 
  /* 
  {"setrowscols_clc",int_set_attribute},
  {"$dot_clc",int_get_attribute},
  {"$set_clc",int_set_attributes},
  */
  {(char *) 0, NULL}
};

/** call ith function in the ClassC interface **/

int ClassC_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassC_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void ClassC_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassC_func[i].name;
  *f = ClassC_func[i].fonc;
}

