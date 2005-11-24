/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoAttribute_Private 
#include <pango/pango.h>
#include "nsp/gtk/pangoattribute.h"
#include "nsp/interf.h"

/* 
 * NspPangoAttribute inherits from NspObject
 */

int nsp_type_pangoattribute_id=0;
NspTypePangoAttribute *nsp_type_pangoattribute=NULL;

/*
 * Type object for PangoAttribute 
 * all the instance of NspTypePangoAttribute share the same id. 
 * nsp_type_pangoattribute: is a an instance of NspTypePangoAttribute 
 *    used for objects of NspPangoAttribute type (i.e built with new_pangoattribute) 
 * other instances are used for derived classes 
 */

NspTypePangoAttribute *new_type_pangoattribute(type_mode mode)
{
  NspTypePangoAttribute *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangoattribute != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangoattribute;
    }
  
  if ((type =  malloc(sizeof(NspTypePangoAttribute))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangoattribute_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = pangoattribute_get_methods; 
  type->new = (new_func *) new_pangoattribute;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangoattribute */ 
  
  top->pr = (print_func *) pangoattribute_print;                    
  top->dealloc = (dealloc_func *) pangoattribute_destroy;
  top->copy  =  (copy_func *) pangoattribute_copy;                   
  top->size  = (size_func *) pangoattribute_size;                  
  top->s_type =  (s_type_func *) pangoattribute_type_as_string;    
  top->sh_type = (sh_type_func *) pangoattribute_type_short_string;
  top->info = (info_func *) pangoattribute_info ;                    
  /* top->is_true = (is_true_func  *) PangoAttributeIsTrue; */
  /* top->loop =(loop_func *) pangoattribute_loop;*/
  top->path_extract = (path_func *) pangoattribute_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) pangoattribute_object;
  top->eq  = (eq_func *) pangoattribute_eq;
  top->neq  = (eq_func *) pangoattribute_neq;
  top->save  = (save_func *) pangoattribute_xdr_save;
  top->load  = (load_func *) pangoattribute_xdr_load;

  /* specific methods for pangoattribute */
      
  type->init = (init_func *) init_pangoattribute;
      
  /* 
   * PangoAttribute interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_pangoattribute_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoAttribute called nsp_type_pangoattribute
       */
      type->id =  nsp_type_pangoattribute_id = nsp_new_type_id();
      nsp_type_pangoattribute = type;
      if ( nsp_register_type(nsp_type_pangoattribute) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_pangoattribute(mode);
    }
  else 
    return type;
}

/*
 * initialize PangoAttribute instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangoattribute(NspPangoAttribute *o,NspTypePangoAttribute *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoAttribute 
 */

NspPangoAttribute *new_pangoattribute() 
{
  NspPangoAttribute *loc; 
  /* type must exists */
  nsp_type_pangoattribute = new_type_pangoattribute(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoAttribute)))== NULLPATTR) return loc;
  /* initialize object */
  if ( init_pangoattribute(loc,nsp_type_pangoattribute) == FAIL) return NULLPATTR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoAttribute 
 *-----------------------------------------------*/

/*
 * size 
 */

static int pangoattribute_size(NspPangoAttribute *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char pangoattribute_type_name[]="PangoAttribute";
static char pangoattribute_short_type_name[]="pangoattribute";

static char *pangoattribute_type_as_string(void)
{
  return(pangoattribute_type_name);
}

static char *pangoattribute_type_short_string(void)
{
  return(pangoattribute_short_type_name);
}

static int pangoattribute_full_comp(NspPangoAttribute * A,NspPangoAttribute * B,char *op,int *err)
{
  if (pango_attribute_equal(A->attr, B->attr)) return TRUE; 
  return FALSE;
}

/*
 * A == B 
 */

static int pangoattribute_eq(NspPangoAttribute *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_pangoattribute_id) == FALSE) return FALSE ;
  rep = pangoattribute_full_comp(A,(NspPangoAttribute *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int pangoattribute_neq(NspPangoAttribute *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_pangoattribute_id) == FALSE) return TRUE;
  rep = pangoattribute_full_comp(A,(NspPangoAttribute *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *pangoattribute_path_extract(NspPangoAttribute *a, NspObject *ob)
{
  char *str;
  if ((str=nsp_string_object(ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str) ;
}

/*
 * save 
 */

static int pangoattribute_xdr_save(XDR  *xdrs, NspPangoAttribute *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("pangoattribute_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspPangoAttribute  *pangoattribute_xdr_load(XDR  *xdrs)
{
  NspPangoAttribute *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPATTR;
  Scierror("pangoattribute_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void pangoattribute_destroy(NspPangoAttribute *self)
{
  pango_attribute_destroy(self->attr);
  FREE(NSP_OBJECT(self)->name);
  FREE(self);
}

/*
 * info 
 */

void pangoattribute_info(NspPangoAttribute *H, int indent)
{
  int i;
  if ( H == NULLPATTR) 
    {
      Sciprintf("Null Pointer PangoAttribute \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[PangoAttribute %s]\n", NSP_OBJECT(H)->name);
}

/*
 * print 
 */

void pangoattribute_print(NspPangoAttribute *H, int indent)
{
  pangoattribute_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoAttribute objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoAttribute   *pangoattribute_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast(O,nsp_type_pangoattribute_id) == TRUE) return ((NspPangoAttribute *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_pangoattribute));
  return(NULL);
}

int IsPangoAttributeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangoattribute_id);
}

int IsPangoAttribute(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangoattribute_id);
}

NspPangoAttribute  *GetPangoAttributeCopy(Stack stack, int i)
{
  if (  GetPangoAttribute(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoAttribute  *GetPangoAttribute(Stack stack, int i)
{
  NspPangoAttribute *M;
  if (( M = pangoattribute_object(NthObj(i))) == NULLPATTR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

NspPangoAttribute *pangoattribute_create(char *name,  PangoAttribute *attr, NspTypeBase *type)
{
  NspPangoAttribute *H  = (type == NULL) ? new_pangoattribute() : type->new();
  if ( H ==  NULLPATTR)
    {
      Sciprintf("No more memory\n");
      return NULLPATTR;
    }
  if ( ( NSP_OBJECT(H)->name =new_nsp_string(name)) == NULLSTRING) return(NULLPATTR);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->attr = attr;
  return H;
}

/*
 * copy 
 */

NspPangoAttribute *pangoattribute_copy(NspPangoAttribute *self)
{
  PangoAttribute *pa = pango_attribute_copy(self->attr); 
  return pangoattribute_create(NVOID,pa,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoAttribute
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_pangoattribute_create(Stack stack, int rhs, int opt, int lhs)
{
  return 0; /* XXXXXXXXXXXX*/ 
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject *
_wrap_pango_attr__get_start_index(NspPangoAttribute *self, char *attr)
{
  return  nsp_new_double_obj((double) self->attr->start_index);
}

static int 
_wrap_pango_attr__set_start_index(NspPangoAttribute *self, char *attr,NspObject *value)
{
  gint val;
  if ( IntScalar(value,&val) == FAIL) return FAIL;
  self->attr->start_index = val;
  return 0;
}

static NspObject *
_wrap_pango_attr__get_end_index(NspPangoAttribute *self, char *attr)
{
  return  nsp_new_double_obj((double) self->attr->end_index);
}

static int 
_wrap_pango_attr__set_end_index(NspPangoAttribute *self, char *attr,NspObject *value,int pos) 
{
  gint val;
  if ( IntScalar(value,&val) == FAIL) return FAIL;
  self->attr->end_index = val;
  return 0;
}

static AttrTab pangoattribute_attrs[] = {
  { "start_index", (attr_get_function *) _wrap_pango_attr__get_start_index, 
    (attr_set_function *) _wrap_pango_attr__set_start_index,NULL },
  { "end_index", (attr_get_function *) _wrap_pango_attr__get_end_index, 
    (attr_set_function *) _wrap_pango_attr__set_end_index,NULL },
  { NULL,NULL,NULL,NULL },
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *pangoattribute_get_methods(void) { return NULL;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_pangoattribute_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspPangoAttribute *a;
  if (( a= GetPangoAttribute(stack,1))== NULLPATTR) return RET_BUG;
 nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab PangoAttribute_func[]={
  /* #include "pangoattribute-in.nam" */ 
  {"pangoattribute_create",int_pangoattribute_create}, 
  {"setrowscols_pangoattribute",int_set_attribute},
  /* 
   * {"$dot_pangoattribute",int_get_attribute},
   * {"$set_pangoattribute",int_set_attributes},
   */
  {"test_pangoattribute",int_pangoattribute_test},
  {(char *) 0, NULL}
};

/** call ith function in the PangoAttribute interface **/

int PangoAttribute_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(PangoAttribute_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void PangoAttribute_Interf_Info(int i, char **fname, function (**f))
{
  *fname = PangoAttribute_func[i].name;
  *f = PangoAttribute_func[i].fonc;
}

