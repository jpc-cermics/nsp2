/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cergrene                            
 *-------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#define Lmo_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

/*
 * NspLmo inherits from NspObject 
 */

int nsp_type_lmo_id=0;
NspTypeLmo *nsp_type_lmo=NULL;

NspTypeLmo *new_type_lmo(type_mode mode)
{
  NspTypeLmo *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_lmo != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_lmo;
    }
  if ((type =  malloc(sizeof(NspTypeLmo))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL;/* lmo_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL;/* lmo_get_methods; */
  type->new = (new_func *) new_lmo;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for lmo */ 

  top->pr = (print_func *) LmoPrint;                    /* printing*/   
  top->dealloc = (dealloc_func *) LmoDestroy;              /* dealloc */  
  top->copy  =  (copy_func *) LmoCopy;                   /* copy object */  
  top->size  = (size_func *) LmoSize;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *) LmoType;                /* type as a String */  
  top->sh_type = (sh_type_func *) LmoShType ;              /* type as a short string */  
  top->info = (info_func *) LmoInfo;                    /* info */  
  /*top->is_true = (is_true_func  *) LmoIsTrue;    */         /* check if object can be considered as true */  
  /* top->loop =(loop_func *) LmoLoopExtract ; */               /* for loops */  
  /* top->path_extract =(path_extract *) LmoPathExtract;*/     /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)  LmoObj ;    /* get object stored in SciObj */  
  top->eq  = (eq_func *) LmoObjEq;                       /* equality check */  
  top->neq  = (eq_func *) LmoObjNeq;                      /* non-equality check */
  /* specific methods for lmo*/

  type->init = (init_func *) init_lmo;
      
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_lmo_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_lmo
       */
      type->id =  nsp_type_lmo_id = nsp_new_type_id();
      nsp_type_lmo = type;
      if ( nsp_register_type(nsp_type_lmo) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_lmo(mode);
    }
  else 
    {
      type->id = nsp_type_lmo_id;
      return type;
    }
}
/*
 * initialize Lmo instances 
 * locally and by calling initializer on parent class 
 */

static int init_lmo(NspLmo *o,NspTypeLmo *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Lmo 
 */

NspLmo *new_lmo() 
{
  NspLmo *loc; 
  /* type must exists */
  nsp_type_lmo = new_type_lmo(T_BASE);
  if ( (loc = malloc(sizeof(NspLmo)))== NULLLMO) return loc;
  /* initialize object */
  if ( init_lmo(loc,nsp_type_lmo) == FAIL) return NULLLMO;
  return loc;
}


/**************************************************
 * LmoSize : returns filled,hsize,or hsize 
 **************************************************/

int LmoSize(NspLmo *L, int flag)
{
  return nsp_list_length((NspList *) L);
}

/**************************************************
 * LmoType 
 **************************************************/

static char list_type_name[]="Lmo";
static char list_short_type_name[]="lmo";

char *LmoType(void)
{
  return(list_type_name);
}

char *LmoShType(NspLmo *L)
{
  return(list_short_type_name);
}


int LmoObjEq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_lmo_id) == FALSE) return FALSE ;
  rep = LmoFullComp((NspLmo *) A,(NspLmo *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int LmoObjNeq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_lmo_id) == FALSE) return TRUE;
  rep = LmoFullComp((NspLmo *) A,(NspLmo *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}


int LmoFullComp(NspLmo * A,NspLmo * B,char *op,int *err)
{
  Scierror("LmoFullComp: to be implemented \n");
  return FALSE;
}



/****************************************************
 * A = LmoObj(O);
 * checks that O is an object of NspLmo type. 
 * or a Hobj which points to an object of type Lmo
 * if so, returns a pointer to that NspLmo and else returns NULL
 ****************************************************/

NspLmo   *LmoObj(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_lmo_id) == TRUE) return ((NspLmo *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",NSP_TYPE_OBJECT(nsp_type_lmo->surtype)->s_type());
  return(NULL);
}


/****************************************************
 * IsLmoObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  Lmo 
 * or a Hobj which points to an object of type Lmo
 ****************************************************/

int IsLmoObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i), nsp_type_lmo_id);
}

/****************************************************
 * IsLmo(O)
 * only checks that object is an object of type  Lmo 
 * or a Hobj which points to an object of type Lmo
 ****************************************************/

int IsLmo(NspObject *O)
{
  return nsp_object_type(O,nsp_type_lmo_id);
}

/*
 * XXXX 
 */


NspObject *LmoPathExtract( NspLmo *L, NspObject *O)
{
  NspSMatrix *M;
  int ival;
  if ( IsMat(O)  ) 
    {
      if ( IntScalar(O,&ival) == FAIL ) return NULLOBJ ;
      return nsp_list_get_element((NspList *) L,ival);
    }
  else if ( IsSMat(O) ) 
    {
      if (( M = SMatObj(O)) == NULLSMAT || M->mn != 1) return NULLOBJ ;
      return nsp_list_search((NspList *) L,M->S[0]);
    }
  return  NULLOBJ;
}


/*************************************
 * Checks that first+i object on the stack 
 * is a LMO and returns that LMO  
 * or a copy of that LMO if its name 
 * is != NVOID 
 *************************************/

NspLmo *GetLmoCopy(Stack stack, int i)
{
  if (  GetLmo(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*************************************
 * Checks that i-th object on the stack 
 * is a Lmo and returns that Lmo or NULLLMO 
 *************************************/

NspLmo *GetLmo(Stack stack, int i)
{
  NspLmo *M;
  if (( M = LmoObj(NthObj(i))) == NULLLMO)
    ArgMessage(stack,i);
  return M;
}
