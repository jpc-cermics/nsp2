/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cergrene                            
 *-------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#define Mod_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "../zcalelm/convert.h"

/*
 * NspMod inherits from NspObject 
 */

int nsp_type_mod_id=0;
NspTypeMod *nsp_type_mod=NULL;

NspTypeMod *new_type_mod(type_mode mode)
{
  NspTypeMod *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_mod != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_mod;
    }
  if ((type =  malloc(sizeof(NspTypeMod))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  NULL;/* mod_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =NULL; /* mod_get_methods;   */
  type->new = (new_func *) new_mod;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for mod */ 

  top->pr = (print_func *) ModPrint;                    /* printing*/   
  top->dealloc = (dealloc_func *) ModDestroy;              /* dealloc */  
  top->copy  =  (copy_func *) ModCopy;                   /* copy object */  
  top->size  = (size_func *) ModSize;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *) ModType;                /* type as a String */  
  top->sh_type = (sh_type_func *) ModShType ;              /* type as a short string */  
  top->info = (info_func *) ModInfo;                    /* info */  
  /*top->is_true = (is_true_func  *) ModIsTrue;   */          /* check if object can be considered as true */  
  /*top->loop =(loop_func *) ModLoopExtract ;  */              /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)  ModObj ;    /* get object stored in SciObj */  
  top->eq  = (eq_func *) ModObjEq;                       /* equality check */  
  top->neq  = (eq_func *) ModObjNeq;                      /* non-equality check */
  /* specific methods for mod */
  type->init = (init_func *) init_mod;

  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_mod_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_mod
       */
      type->id =  nsp_type_mod_id = nsp_new_type_id();
      nsp_type_mod = type;
      if ( nsp_register_type(nsp_type_mod) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_mod(mode);
    }
  else 
    {
      type->id = nsp_type_mod_id;
      return type;
    }
}
/*
 * initialize Mod instances 
 * locally and by calling initializer on parent class 
 */

static int init_mod(NspMod *o,NspTypeMod *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Mod 
 */

NspMod *new_mod() 
{
  NspMod *loc; 
  /* type must exists */
  nsp_type_mod = new_type_mod(T_BASE);
  if ( (loc = malloc(sizeof(NspMod)))== NULLMOD) return loc;
  /* initialize object */
  if ( init_mod(loc,nsp_type_mod) == FAIL) return NULLMOD;
  return loc;
}


/**************************************************
 * ModSize : returns 0 
 **************************************************/

int ModSize(NspMod *H, int flag)
{
  return 0;
}

/**************************************************
 * ModType infos 
 **************************************************/

static char hash_type_name[]="Mod";
static char hash_short_type_name[]="mo";

char *ModType(void)
{
  return(hash_type_name);
}

char *ModShType(NspMod *H)
{
  return(hash_short_type_name);
}

int ModObjEq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_mod_id) == FALSE) return FALSE ;
  rep = ModFullComp((NspMod *) A,(NspMod *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int ModObjNeq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_mod_id) == FALSE) return TRUE;
  rep = ModFullComp((NspMod *) A,(NspMod *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}


int ModFullComp(NspMod * A,NspMod * B,char *op,int *err)
{
  Scierror("LmoFullComp: to be implemented \n");
  return FALSE;
}



/****************************************************
 * A = ModObj(O);
 * checks that O is an object of NspMod type. 
 * or a Hobj which points to an object of type Mod
 * if so, returns a pointer to that NspMod and else returns NULL
 ****************************************************/

NspMod   *ModObj(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_mod_id) == TRUE) return ((NspMod *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",NSP_TYPE_OBJECT(nsp_type_mod->surtype)->s_type());
  return(NULL);
}


/****************************************************
 * IsModObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  Mod 
 * or a Hobj which points to an object of type Mod
 ****************************************************/

int IsModObj(Stack stack, int i)
{
  return ObjType(NthObj(i), nsp_type_mod_id);
}

/****************************************************
 * IsMod(O)
 * only checks that object is an object of type  Mod 
 * or a Hobj which points to an object of type Mod
 ****************************************************/

int IsMod(NspObject *O)
{
  return ObjType(O,nsp_type_mod_id);
}


/*************************************************
 * GetModCopy 						  
 *************************************************/

NspMod *GetModCopy(Stack stack, int i)
{
  if (  GetMod(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*************************************
 * GetMod 
 *************************************/

NspMod *GetMod(Stack stack, int i)
{
  NspMod *H;
  if (( H = ModObj(NthObj(i))) == NULLMOD)
    ArgMessage(stack,i);
  return H;
}

