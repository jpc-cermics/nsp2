/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cergrene                            
 *-------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#define Me_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "../zcalelm/convert.h"

/*
 * NspMe inherits from NspObject 
 */

int nsp_type_me_id=0;
NspTypeMe *nsp_type_me=NULL;

NspTypeMe *new_type_me(type_mode mode)
{
  NspTypeMe *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_me != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_me;
    }
  if ((type =  malloc(sizeof(NspTypeMe))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL;/*me_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =NULL;/* me_get_methods;  */
  type->new = (new_func *) new_me;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for me */ 

  top->pr = (print_func *) MePrint;                    /* printing*/   
  top->dealloc = (dealloc_func *) MeDestroy;              /* dealloc */  
  top->copy  =  (copy_func *) MeCopy;                   /* copy object */  
  top->size  = (size_func *) MeSize;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *) MeType;                /* type as a String */  
  top->sh_type = (sh_type_func *) MeShType ;              /* type as a short string */  
  top->info = (info_func *) MeInfo;                    /* info */  
  /*top->is_true = (is_true_func  *) MeIsTrue;  */           /* check if object can be considered as true */  
  /*top->loop =(loop_func *) MeLoopExtract ;  */              /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)  MeObj ;    /* get object stored in SciObj */  
  top->eq  = (eq_func *) MeObjEq;                       /* equality check */  
  top->neq  = (eq_func *) MeObjNeq;                      /* non-equality check */
  /* specific methods for me */
  type->init = (init_func *) init_me;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_me_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_me
       */
      type->id =  nsp_type_me_id = nsp_new_type_id();
      nsp_type_me = type;
      if ( nsp_register_type(nsp_type_me) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_me(mode);
    }
  else 
    {
      type->id = nsp_type_me_id;
      return type;
    }


}
/*
 * initialize Me instances 
 * locally and by calling initializer on parent class 
 */

static int init_me(NspMe *o,NspTypeMe *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Me 
 */

NspMe *new_me() 
{
  NspMe *loc; 
  /* type must exists */
  nsp_type_me = new_type_me(T_BASE);
  if ( (loc = malloc(sizeof(NspMe)))== NULLME) return loc;
  /* initialize object */
  if ( init_me(loc,nsp_type_me) == FAIL) return NULLME;
  return loc;
}


int MeSize(NspMe *me, int flag)
{
  return 0;
}

static char Me_type_name[]="Me";
static char Me_short_type_name[]="me";

char *MeType(void)
{
  return(Me_type_name);
}

char *MeShType(NspMe *F)
{
  return(Me_short_type_name);
}

int MeObjEq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_me_id) == FALSE) return FALSE ;
  rep = MeFullComp((NspMe *) A,(NspMe *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int MeObjNeq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_me_id) == FALSE) return TRUE;
  rep = MeFullComp((NspMe *) A,(NspMe *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}


int MeFullComp(NspMe * A,NspMe * B,char *op,int *err)
{
  Scierror("LmoFullComp: to be implemented \n");
  return FALSE;
}

/****************************************************
 * A = MeObj(O);
 * checks that O is an object of NspMe type. 
 * or a Hobj which points to an object of type Me
 * if so, returns a pointer to that NspMe and else returns NULL
 ****************************************************/

NspMe   *MeObj(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_me_id) == TRUE) return ((NspMe *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",NSP_TYPE_OBJECT(nsp_type_me->surtype)->s_type());
  return(NULL);
}


/****************************************************
 * IsMeObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  Me 
 * or a Hobj which points to an object of type Me
 ****************************************************/

int IsMeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i), nsp_type_me_id);
}

/****************************************************
 * IsMe(O)
 * only checks that object is an object of type  Me 
 * or a Hobj which points to an object of type Me
 ****************************************************/

int IsMe(NspObject *O)
{
  return nsp_object_type(O,nsp_type_me_id);
}


/*************************************************
 * GetMeCopy 						  
 *************************************************/

NspMe *GetMeCopy(Stack stack, int i)
{
  if (  GetMe(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*************************************
 * GetMe 
 *************************************/

NspMe *GetMe(Stack stack, int i)
{
  NspMe *H;
  if (( H = MeObj(NthObj(i))) == NULLME)
    ArgMessage(stack,i);
  return H;
}

/*********************************************************************
 * Elts which are stored in module has tables 
 * mainly names of function or data belonging to a specific 
 * module. 
 *********************************************************************/

/*********************************************************************
 * Create a Me
 *********************************************************************/

NspMe *MeCreate(char *name)
{
  NspMe *H = new_me();
  if ( H == NULLME)
    {
      Sciprintf("No more memory\n");
      return NULLME;
    }
  if ((NSP_OBJECT(H)->name = NewString(name))== NULLSTRING) return NULLME;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  H->path= H->module=NULL;
  return H;
}

/*************************************************
 * Copy of a Me : the objet it points to is not copied 
 * The copy has  name NVOID
 * returns NULLME on failure 
 * XXXXX
 ***************************************************/

NspMe *MeCopy(NspMe *H)
{
  NspMe *Loc;
  Loc = MeCreate(NVOID);
  if ( Loc == NULLME ) return NULLME;
  return(Loc);
}

/*****************************************
 * Delete Me but not the obj it points to 
 *****************************************/

void MeDestroy(NspMe *H)
{
  FREE(NSP_OBJECT(H)->name);
  FREE(H) ;
}

/*********************************************
 * MeInfo 
 *********************************************/

void MeInfo(NspMe *H, int indent)
{
  int i;
  if ( H == NULLME) 
    {
      Sciprintf("Null Pointer Me \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[me %s]\n",NSP_OBJECT(H)->name);
}

/**************************************************
 * MePrint 
 **************************************************/

void MePrint(NspMe *H, int indent)
{
  int i;
  if ( H == NULLME) 
    {
      Sciprintf("Null Pointer Me \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[Me %s ]\n",NSP_OBJECT(H)->name);
}


