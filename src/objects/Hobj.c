/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/datas.h"

/* include Private Part */
#define Hobj_Private 
#include "nsp/hobj.h" 

static NspMethods *hobj_get_methods(void);

/**
 * SECTION:hobj
 * @title: #NspHobj are used to store a reference to another nsp object.
 * @short_description: stores a reference to another nsp object.
 * @see_also: 
 *
 * <para>
 * A #NspHobj object is used to store a reference to another nsp object.
 * They are used to implement the by value argument passing semantic in a 
 * lazy way and used to access global variables. 
 * </para>
 **/

/*
 * NspHobj inherits from NspObject 
 */

int nsp_type_hobj_id=0;
NspTypeHobj *nsp_type_hobj=NULL;

NspTypeHobj *new_type_hobj(type_mode mode)
{
  NspTypeHobj *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_hobj != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_hobj;
    }
  if ((type =  malloc(sizeof(NspTypeHobj))) ==  NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* hobj_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  hobj_get_methods; 
  type->new = (new_func *) new_hobj;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for hobj */ 

  top->pr = (print_func *)nsp_hobj_print;                    /* printing*/   
  top->dealloc = (dealloc_func *)nsp_hobj_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_hobj_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_hobj_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_hobj_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_hobj_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_hobj_info;                    /* info */  
  top->is_true = (is_true_func  *)nsp_hobj_is_true;            /* check if object can be considered as true */  
  /* top->loop =(loop_func *)nsp_hobj_loop;*/                /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_hobj_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_hobj_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_hobj_neq;                      /* non-equality check */
  top->save  = (save_func *)nsp_hobj_xdr_save;
  top->load  = NULL;

  /* specific methods for hobj */
      
  type->init = (init_func *) init_hobj;
      
  /* 
   * Hobj interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_hobj_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeHobj called nsp_type_hobj
       */
      type->id =  nsp_type_hobj_id = nsp_new_type_id();
      nsp_type_hobj = type;
      if ( nsp_register_type(nsp_type_hobj) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_hobj(mode);
    }
  else 
    {
      type->id = nsp_type_hobj_id;
      return type;
    }
}

/*
 * initialize Hobj instances 
 * locally and by calling initializer on parent class 
 */

static int init_hobj(NspHobj *o,NspTypeHobj *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Hobj 
 */

NspHobj *new_hobj() 
{
  NspHobj *loc; 
  /* type must exists */
  nsp_type_hobj = new_type_hobj(T_BASE);
  if ( (loc = malloc(sizeof(NspHobj)))== NULLHOBJ) return loc;
  /* initialize object */
  if ( init_hobj(loc,nsp_type_hobj) == FAIL) return NULLHOBJ;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Hobj
 *-----------------------------------------------*/
 

/*
 * size: returns the sizes of the object it points to
 */

static int nsp_hobj_size(NspHobj *H, int flag)
{
  NspObject *O;
  if ( H->htype != 'g' ) return nsp_object_get_size(H->O,flag);
  if ((O= nsp_global_frame_search_object(NSP_OBJECT(H)->name)) == NULLOBJ) return 0;
  return nsp_object_get_size(O,flag);
}

/*
 * type as string 
 */

static char hobj_type_name[]="HObj";
/* static char hobj_short_type_name[]="hobj"; */

static char *nsp_hobj_type_as_string(void)
{
  return(hobj_type_name);
}

static char *nsp_hobj_type_short_string(NspHobj *H)
{
  NspObject *O;
  if ( H->htype != 'g' )  return(nsp_object_type_short(H->O));
  if ((O= nsp_global_frame_search_object(NSP_OBJECT(H)->name)) == NULLOBJ) return "xx";
  return nsp_object_type_short(O);
}

/* used in for x=y where y is a Hobj
 * FIXME: to be done 
 */


static int HobjFullComp(NspHobj * A,NspHobj * B,char *op,int *err)
{
  Scierror("HobjFullComp: to be implemented \n");
  return FALSE;
}

static int nsp_hobj_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_hobj_id) == FALSE) return FALSE ;
  rep = HobjFullComp((NspHobj *) A,(NspHobj *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int nsp_hobj_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_hobj_id) == FALSE) return TRUE;
  rep = HobjFullComp((NspHobj *) A,(NspHobj *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

static int nsp_hobj_is_true(NspHobj *H)
{
  NspObject *O;
  if ( H->htype != 'g' )  return(nsp_object_is_true(H->O));
  if ((O= nsp_global_frame_search_object(NSP_OBJECT(H)->name)) == NULLOBJ) return FALSE;
  return nsp_object_is_true(O);
}

/*
 * saving a NspHobj object 
 */

static int nsp_hobj_xdr_save(XDR *xdrs, NspHobj *O)
{
  int rep;
  NspObject *O2,*O1=NULLOBJ;
  if ( O  == NULLHOBJ) return OK;
  if ( Ocheckname(O,NVOID) )  
    {
      Sciprintf("Warning:\t trying to save an object without name\n");
      return OK;
    }
  /* FIXME: is it worth saving an hobj ? */
  if ( O->htype != 'g' )  
    {
      if (( O1 =nsp_object_copy( ((NspHobj *) O)->O)) == NULLOBJ ) return FAIL;
    }
  else 
    {
      if ((O2 = nsp_global_frame_search_object(NSP_OBJECT(O)->name)) == NULLOBJ) return FAIL;
      if (( O1 =nsp_object_copy(O2)) == NULLOBJ ) return FAIL;
    }
  if (nsp_object_set_name(O1,NSP_OBJECT(O)->name) == FAIL) return FAIL;
  rep = nsp_object_xdr_save(xdrs,O1);
  nsp_object_destroy(&O1);
  return rep;
}

/*
 * load not implemented for Hobj. 
 */

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Hobj objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspHobj   *nsp_hobj_object(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)
    return ((NspHobj *) O);
  else 
    Scierror("Error:\tExpecting a pointer\n");
  return(NULLHOBJ);
}

/*
 * IsHobj(O) and IsHopt(O) 
 */

int IsHobj(NspObject *O)
{
  return check_cast(O,nsp_type_hobj_id);
}

int IsHopt(NspObject *O)
{
  return ( check_cast(O,nsp_type_hobj_id) && ((NspHobj *) O)->htype == 'o' );
}

int IsGlobal(NspObject *O)
{
  return ( check_cast(O,nsp_type_hobj_id) && ((NspHobj *) O)->htype == 'g' );
}

/*-------------------------------------------------------------------
 * wrappers for the BMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/


static NspMethods hash_methods[] = {
  { (char *) 0, NULL}
};

static NspMethods *hobj_get_methods(void) { return hash_methods;};


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/


/*----------------------------------------------------
 * A set of functions 
 *----------------------------------------------------*/

static NspHobj  *hobj_create_gen(const char *name, NspObject *Obj,char htype)
{
  NspHobj *H = new_hobj();
  if ( H == NULLHOBJ)
    {
      Sciprintf("No more memory\n");
      return NULLHOBJ;
    }
  NSP_OBJECT(H)->name = NULL;
  if ( NSP_OBJECT(H)->type->set_name(NSP_OBJECT(H),name) == NULL)
    return(NULLHOBJ);
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  H->htype = htype; 

  /* take care here of the fact that O can also be a Hobj 
   * in that case we must get the object it points to 
   */
  if ( IsHobj(Obj) )  Obj = ((NspHobj *) Obj)->O;
  
  H->O=Obj;
  return H;
}

/*
 * Create a Hobj (a handler to an other object) 
 */


NspHobj  *HobjCreate(const char *name, NspObject *O)
{
  return hobj_create_gen(name, O, 'h');
}

/*
 * Create a Hopt ( used for optional arguments transmited as x = val )
 */

NspHobj  *HoptCreate(const char *name, NspObject *O)
{
  return hobj_create_gen(name, O, 'o');
}


/*
 * Create a Gobj ( used for global variables ) 
 */

NspHobj  *GobjCreate(const char *name, NspObject *O)
{
  return hobj_create_gen(name, O, 'g');
}

/*
 * Copy of a Hobj : the object it points to is NOT copied 
 * The copy has  name NVOID
 * returns NULLHOBJ on failure 
 */

NspHobj  *nsp_hobj_copy(NspHobj *H)
{
  NspHobj *Loc;
  Loc = HobjCreate(NVOID,H->O);
  if ( Loc == NULLHOBJ ) return NULLHOBJ;
  return(Loc);
}


/*
 * Delete Hobj but not the obj it points to 
 */

void nsp_hobj_destroy(NspHobj *H)
{
  NSP_OBJECT(H)->type->set_name(NSP_OBJECT(H),NVOID);
  FREE(H) ;
}

/*
 * HobjInfo 
 */

int nsp_hobj_info(NspHobj *H, int indent,char *name,int rec_level)
{
  return nsp_hobj_print(H,indent,name,rec_level);
}

/*
 * HobjPrint 
 */

int nsp_hobj_print(NspHobj *H, int indent,char *name, int rec_level)
{
  const char *pname;
  NspObject *O;
  if ( H == NULLHOBJ) 
    {
      Sciprintf1(indent,"Null Pointer Hobj \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  if ( H->htype == 'g' ) 
    {
      if ((O= nsp_global_frame_search_object(NSP_OBJECT(H)->name)) == NULLOBJ) 
	{
	  Sciprintf1(indent,"Pointer to a global non existant variable\n");
	  return TRUE;
	}
    }
  else 
    {
      O = H->O;
    }
  Sciprintf1(indent,"%s\t-> %s\t\thobj\n",
	     (pname==NULL) ? "" : pname,
	     nsp_object_get_name(O));
  return nsp_object_print(O,indent+2,NULL,rec_level);
}




/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * Interface for HashTables 
 */

/*
 * Create a pointer to a given object 
 * XXXX: le pb est que StoreResult 
 *       si l'objet est un pointeur 
 *       cré une copie de l'objet pointé ce qui est 
 *       pas forcément utile et rend inutile cette fonction 
 *       A revoir donc 
 */

static int int_hobjcreate(Stack stack, int rhs, int opt, int lhs)
{
  NspHobj *H;
  CheckRhs(1,1);
  CheckLhs(1,1); 
  /* l'objet envoyé doit avoir un nom ? XXXX */ 
  if ((H = HobjCreate(NVOID,NthObj(1)))==NULLHOBJ) return RET_BUG;
  NthObj(1) = (NspObject *) H; 
  NSP_OBJECT(H)->ret_pos = 1;
  return 1;
} 

/* get the name of the object the hobj points to 
 *
 */

static int int_hobj_target_name(Stack stack, int rhs, int opt, int lhs)
{
  NspHobj *H;
  NspObject *Obj;
  CheckStdRhs(1,1);
  CheckLhs(1,1);

  if ( IsHobj(NthObj(1))==FALSE ) return RET_BUG;

  H=(NspHobj *) NthObj(1);
  if ( H->htype == 'g' ) 
    {
      if ((Obj= nsp_global_frame_search_object(NSP_OBJECT(H)->name)) == NULLOBJ) 
	{
	  Scierror("Error: Pointer to a global non existant variable\n");
	  return RET_BUG;
	}
    }
  else 
    {
      Obj = H->O;
    }
  if ( nsp_move_string(stack,1,nsp_object_get_name(Obj),-1) == FAIL) return RET_BUG;
  return 1;
}

/* is argument an hobj 
 *
 */

static int int_hobj_ishobj(Stack stack, int rhs, int opt, int lhs)
{
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ( nsp_move_boolean(stack,1,IsHobj(NthObj(1))) == FAIL) return RET_BUG;
  return 1;
}

/*
 *  Interface 
 */

static OpTab Hobj_func[]={
  {"handler",int_hobjcreate},
  {"hobj_name",int_hobj_target_name},
  {"ishobj",int_hobj_ishobj},
  {(char *) 0, NULL}
};

/* call ith function in the NspHobj interface **/

int Hobj_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Hobj_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void Hobj_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Hobj_func[i].name;
  *f = Hobj_func[i].fonc;
}


