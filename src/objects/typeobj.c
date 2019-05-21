/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

/* 
 * Object which describe a type. 
 */

#define  Type_Private 
#include <nsp/nsp.h> 
#include <nsp/object.h> 
#include <nsp/type.h> 
#include <nsp/hobj.h> 
#include <nsp/list.h> 
#include <nsp/none.h> 
#include <nsp/hash.h> 
#include <nsp/cells.h> 
#include <nsp/plist.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/sprowmatrix.h> 
#include <nsp/spcolmatrix.h> 
#include <nsp/spmaxpcolmatrix.h> 
#include <nsp/matint.h> 
#include <nsp/file.h> 
#include <nsp/hash.h> 
#include <nsp/function.h> 
#include <nsp/ivect.h> 
#include <nsp/module.h> 
#include <nsp/modulelt.h> 
#include <nsp/pmatrix.h> 
#include <nsp/classa.h> 
#include <nsp/classb.h> 
#include <nsp/classaref.h> 
#include <nsp/classbref.h> 
#include <nsp/serial.h> 
#include <nsp/bhash.h> 
#include <nsp/mpmatrix.h> 
#include <nsp/classaref.h> 
#include <nsp/classbref.h> 
#include <nsp/astnode.h> 
#include <nsp/bvar.h> 
#include <nsp/linearsys.h> 
#include <nsp/interf.h>

#ifdef WITH_MONGODB_C 
#include <nsp/bson.h> 
#include <nsp/mclient.h> 
#include <nsp/mcursor.h> 
#include <nsp/mcollection.h> 
#endif 

/* FIXME */
extern void nsp_init_gtk_types(void);
extern void nsp_init_gtk_types_added(void);
extern void *new_type_arrows(type_mode mode);
extern void *new_type_axes(type_mode mode);
extern void *new_type_block(type_mode mode);
extern void *new_type_box3d(type_mode mode);
extern void *new_type_compound(type_mode mode);
extern void *new_type_connector(type_mode mode);
extern void *new_type_contour(type_mode mode);
extern void *new_type_contour3d(type_mode mode);
extern void *new_type_curve(type_mode mode);
extern void *new_type_diagram(type_mode mode);
extern void *new_type_fec(type_mode mode);
extern void *new_type_figure(type_mode mode);
extern void *new_type_figuredata(type_mode mode);
extern void *new_type_gmatrix(type_mode mode);
extern void *new_type_gmatrix1(type_mode mode);
extern void *new_type_rect(type_mode mode); 
extern void *new_type_grarc(type_mode mode);
extern void *new_type_grimage(type_mode mode);
extern void *new_type_groot(type_mode mode);
extern void *new_type_grrect(type_mode mode);
extern void *new_type_grstring(type_mode mode);
extern void *new_type_link(type_mode mode);
extern void *new_type_objs3d(type_mode mode);
extern void *new_type_points3d(type_mode mode);
extern void *new_type_polyhedron(type_mode mode);
extern void *new_type_polyline(type_mode mode);
extern void *new_type_polyline3d(type_mode mode);
extern void *new_type_qcurve(type_mode mode);
extern void *new_type_segments(type_mode mode);
extern void *new_type_spolyhedron(type_mode mode);
extern void *new_type_string3d(type_mode mode);
extern void *new_type_surf(type_mode mode);
extern void *new_type_vfield(type_mode mode);
extern void *new_type_graphic(type_mode mode);
/* extern void *new_type_astnode(type_mode mode); */
extern void *new_type_ast(type_mode mode);
extern void *new_type_astv(type_mode mode);
extern void *new_type_frame(type_mode mode);
/* extern void *new_type_linearsys(type_mode mode); */
/* 
 * NspType inherits from NspObject
 */

int nsp_type_type_id=0;
NspTypeType *nsp_type_type=NULL;

/*
 * Type object for Type 
 * all the instance of NspTypeType share the same id. 
 * nsp_type_type: is a an instance of NspTypeType 
 *    used for objects of NspType type (i.e built with new_type) 
 * other instances are used for derived classes 
 */

NspTypeType *new_type_type(type_mode mode)
{
  NspTypeType *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_type != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_type;
    }
  
  if ((type =  malloc(sizeof(NspTypeType))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  NULL; /*type_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = type_get_methods; 
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_type;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for type */ 
  
  top->pr = (print_func *) type_print;                    
  top->dealloc = (dealloc_func *) type_destroy;
  top->copy  =  (copy_func *) type_copy;                   
  top->size  = (size_func *) type_size;                  
  top->s_type =  (s_type_func *) type_type_as_string;    
  top->sh_type = (sh_type_func *) type_type_short_string;
  top->info = (info_func *) type_info ;                    
  /* top->is_true = (is_true_func  *) TypeIsTrue; */
  /* top->loop =(loop_func *) type_loop;*/
  top->path_extract = (path_func *) type_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) type_object;
  top->eq  = (eq_func *) type_eq;
  top->neq  = (eq_func *) type_neq;
  top->save  = (save_func *) type_xdr_save;
  top->load  = (load_func *) type_xdr_load;
  top->full_copy  =  (copy_func *) type_copy;                   

  /* specific methods for type */
      
  type->init = (init_func *) init_type;
      
  /* 
   * Type interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_type_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeType called nsp_type_type
       */
      type->id =  nsp_type_type_id = nsp_new_type_id();
      nsp_type_type = type;
      if ( nsp_register_type(nsp_type_type) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_type(mode);
    }
  else 
    return type;
}

/*
 * initialize Type instances 
 * locally and by calling initializer on parent class 
 */

static int init_type(NspType *o,NspTypeType *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  o->nsp_type = NULL; 
  return OK;
}

/*
 * new instance of Type 
 */

NspType *new_type() 
{
  NspType *loc; 
  /* type must exists */
  nsp_type_type = new_type_type(T_BASE);
  if ( (loc = malloc(sizeof(NspType)))== NULLTYPE) return loc;
  /* initialize object */
  if ( init_type(loc,nsp_type_type) == FAIL) return NULLTYPE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Type 
 *-----------------------------------------------*/

/*
 * size 
 */

static int type_size(NspType *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char type_type_name[]="Type";
static char type_short_type_name[]="type";

static char *type_type_as_string(void)
{
  return(type_type_name);
}

static char *type_type_short_string(NspObject *v)
{
  return(type_short_type_name);
}

/*
 * A == B 
 */

static int type_eq(NspType *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_type_id) == FALSE) return FALSE ;
  return ( A->nsp_type == ((NspType *) B)->nsp_type) ?  TRUE: FALSE;
}

/*
 * A != B 
 */

static int type_neq(NspType *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_type_id) == FALSE) return TRUE;
  return ( A->nsp_type != ((NspType *) B)->nsp_type) ?  TRUE: FALSE;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *type_path_extract(NspType *a,int n, NspObject **ob,int *copy)
{
  char *str;
  *copy = FALSE;
  if ( n != 1 ) return NULLOBJ;
  if ((str=nsp_string_object(*ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str,copy) ;
}

/*
 * save 
 */

static int type_xdr_save(XDR *xdrs, NspType *M)
{
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_type)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("type_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspType  *type_xdr_load(XDR *xdrs)
{
  NspType *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLTYPE;
  Scierror("type_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void type_destroy(NspType *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  FREE(H);
}

/*
 * info 
 */

int type_info(NspType *H, int indent,char *name,int rec_level)
{
  NspTypeObject *top;
  if ( H == NULLTYPE) 
    {
      Sciprintf("Null Pointer Type \n");
      return TRUE;
    }
  top = NSP_TYPE_OBJECT(H->nsp_type);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  Sciprintf1(indent,"%s\t = %s\t\ttype\n",
	    NSP_OBJECT(H)->name,
	    top->s_type());
  return TRUE;
}

/*
 * print 
 */

int type_print(NspType *H, int indent,char *name, int rec_level)
{
  type_info(H,indent,NULL,0);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Type objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspType   *type_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_type_id) == TRUE) return ((NspType *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",   type_get_name(nsp_type_type));
  return(NULL);
}

int IsTypeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_type_id);
}

int IsType(NspObject *O)
{
  return nsp_object_type(O,nsp_type_type_id);
}

NspType  *GetTypeCopy(Stack stack, int i)
{
  if (  GetType(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspType  *GetType(Stack stack, int i)
{
  NspType *M;
  if (( M = type_object(NthObj(i))) == NULLTYPE)
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

NspType *type_create(char *name,NspTypeBase *nsp_type ,NspTypeBase *type)
{
  NspType *H  = (type == NULL) ? new_type() : type->new();
  if ( H ==  NULLTYPE)
    {
      Sciprintf("No more memory\n");
      return NULLTYPE;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return(NULLTYPE);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->nsp_type = nsp_type;
  return H;
}

/*
 * copy 
 */

NspType *type_copy(NspType *H)
{
  return type_create(NVOID,H->nsp_type,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the Type
 *-------------------------------------------------------------------*/

/* 
 * this interface is used to create an object which type is given 
 * by the first argument on the stack which is of type NspType 
 */ 
#if 0 
static int int_type_create(Stack stack,int rhs,int opt,int lhs)
{
  NspType *type;
  NspTypeObject *top;
  CheckRhs(1,1000);
  CheckLhs(0,1);
  if (( type = GetType(stack,1)) == NULLTYPE)  return RET_BUG; 
  top = NSP_TYPE_OBJECT(type->nsp_type);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  return ((function *) top->create)(stack,rhs,opt,lhs); 
}
#endif 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

char *type_get_name(void *type)
{
  NspTypeObject *top = NSP_TYPE_OBJECT(type);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  return top->s_type();
}

static int int_type_name(void *self,Stack stack,int rhs,int opt,int lhs)
{
  char *name = type_get_name(((NspType *) self)->nsp_type);
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( nsp_move_string(stack,1,name,-1) == FAIL) return RET_BUG; 
  return 1; 
}

/* creates an object whose type is type using the 
 */

static int int_type_new(void *self,Stack stack,int rhs,int opt,int lhs)
{
  NspTypeObject *top = NSP_TYPE_OBJECT(((NspType *) self)->nsp_type);
  CheckRhs(0,100);
  CheckLhs(0,1);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  return ((function *) top->create)(stack,rhs,opt,lhs); 
}

static int int_type_surtype(void *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *ret;
  NspTypeBase *type  = ((NspType *) self)->nsp_type; 
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( type->surtype == NULL) 
    {
      if (( ret =(NspObject *) nsp_none_create(NVOID,NULL)) ==  NULLOBJ ) return RET_BUG; 
    }
  else 
    {
      /* we need the BASE representant for type_surtype */
      type = nsp_get_type_from_id(type->surtype->id);
      if (( ret = (NspObject *) type_create(NVOID,type,NULL)) == NULLOBJ ) return RET_BUG; 
    }
  MoveObj(stack,1,ret);
  return 1; 
}

static NspMethods type_methods[] = {
  { "name", int_type_name},
  { "surtype",int_type_surtype},  
  { "new", int_type_new},  
  { (char *) 0, NULL}
};

static NspMethods *type_get_methods(void) { return type_methods;};

/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_type_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspType *a;
  if (( a= GetType(stack,1))== NULLTYPE) return RET_BUG;
  nsp_object_print((NspObject *) a,0,NULL,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Type_func[]={
  /* #include "type-in.nam" */ 
  {"setrowscols_type",int_set_attribute},
  {"test_type",int_type_test},
  {(char *) 0, NULL}
};

/* call ith function in the Type interface **/

int Type_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Type_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void Type_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Type_func[i].name;
  *f = Type_func[i].fonc;
}

/*---------------------------------------------------
 * set of function for dealing with types 
 *--------------------------------------------------*/

NspHash *nsp_types_hash_table = NULL; 
const int nsp_no_type_id = 0;  /* an id never used by a type */ 
const int nsp_dynamic_id = SHRT_MAX ;   /* id given by a name */
static int nsp_last_static_id_ = 0;    /* static id's are <=  nsp_last_static_id  */

/**
 * primitive_types_register:
 * @void: 
 * 
 * register basic types in the type system by assigning an id to each 
 * predefined classes. It is important not to break this order since 
 * the load of saved objects is based on the id given here.
 *
 **/

static void nsp_graphic_types_register(void);

void primitive_types_register(void) 
{
  new_type_matrix(T_BASE);
  new_type_bmatrix(T_BASE);
  new_type_function(T_BASE);
  new_type_hash(T_BASE);
  new_type_hobj(T_BASE);
  new_type_ivect(T_BASE);
  new_type_list(T_BASE);
  new_type_module(T_BASE);
  new_type_modulelt(T_BASE);
  new_type_object(T_BASE);
  new_type_plist(T_BASE);
  new_type_pmatrix(T_BASE);
  new_type_file(T_BASE);
  new_type_smatrix(T_BASE);
  new_type_sprowmatrix(T_BASE);
  new_type_none(T_BASE);
  new_type_type(T_BASE);
  new_type_classa(T_BASE);
  new_type_classb(T_BASE);
  new_type_rect(T_BASE);
  new_type_block(T_BASE);
  new_type_link(T_BASE);
  new_type_diagram(T_BASE);
  /* initialize nsp gtk objects types */
  nsp_init_gtk_types(); 
  /* cells */
  new_type_cells(T_BASE); 
  new_type_connector(T_BASE);
  new_type_matint(T_BASE);
  new_type_serial(T_BASE);
  new_type_spcolmatrix(T_BASE);
  new_type_bhash(T_BASE);
  new_type_imatrix(T_BASE);
  /* graphics objects */
  nsp_graphic_types_register();
  /* more gtk types */
  nsp_init_gtk_types_added();
  new_type_mpmatrix(T_BASE);
  new_type_classaref(T_BASE);
  new_type_classbref(T_BASE);
  new_type_astnode(T_BASE);
  new_type_spmaxpcolmatrix(T_BASE);
  new_type_bvar(T_BASE);
  new_type_linearsys(T_BASE);
#ifdef WITH_MONGODB_C 
  /* mongodb types */
  new_type_bson(T_BASE);
  new_type_mclient(T_BASE);
  new_type_mcollection(T_BASE);
  new_type_mcursor(T_BASE);
  nsp_last_static_id_ = nsp_type_mcursor_id;
#else
  /* take care here that the last declared class id 
   * must be copied in nsp_last_static_id_
   */
  // nsp_last_static_id_ = nsp_type_bvar_id;
  nsp_last_static_id_ = nsp_type_linearsys_id;
#endif 

}

static void nsp_graphic_types_register(void) 
{
  new_type_arrows(T_BASE);
  new_type_axes(T_BASE);
  new_type_box3d(T_BASE);
  new_type_compound(T_BASE);
  new_type_contour(T_BASE);
  new_type_contour3d(T_BASE);
  new_type_curve(T_BASE);
  new_type_fec(T_BASE);
  new_type_figure(T_BASE);
  new_type_figuredata(T_BASE);
  new_type_gmatrix(T_BASE);
  new_type_gmatrix1(T_BASE);
  new_type_graphic(T_BASE);
  new_type_grarc(T_BASE);
  new_type_grimage(T_BASE);
  new_type_groot(T_BASE);
  new_type_grrect(T_BASE);
  new_type_grstring(T_BASE);
  new_type_objs3d(T_BASE);
  new_type_points3d(T_BASE);
  new_type_polyhedron(T_BASE);
  new_type_polyline(T_BASE);
  new_type_polyline3d(T_BASE);
  new_type_qcurve(T_BASE);
  new_type_segments(T_BASE);
  new_type_spolyhedron(T_BASE);
  new_type_string3d(T_BASE);
  new_type_surf(T_BASE);
  new_type_vfield(T_BASE);
}



/*
 * registering types in an Nsp Hash Table 
 */

int nsp_last_static_id(void) {return nsp_last_static_id_;}

/**
 * nsp_new_type_id:
 * 
 * increments and return a new type number which can be assigned
 * to a new class.
 * 
 * Return value: a new #NspTypeId.
 **/

NspTypeId  nsp_new_type_id(void) { static int i=1; return i++; }


/**
 * nsp_register_type:
 * @type: id of a type. 
 * 
 * This function is called to register new types (objects of type #NspType) in the global 
 * variable nsp_types_hash_table. 
 * 
 * Return value: %TRUE or %FALSE.
 **/

int nsp_register_type(void *type) 
{
  NspTypeObject *top = type; 
  NspObject *nsp_type; 
  /* since nsp_types_hash_table is a hash table there's. Registering 
   * hash tables is a bit special
   */
  if ( type == nsp_type_hash && nsp_types_hash_table == NULLHASH ) return TRUE;
  if ( nsp_types_hash_table == NULLHASH ) 
    {
      /* create and store in the protected frame XXXX  */  
      if (( nsp_types_hash_table = nsp_hash_create("%types",250))== NULLHASH) return FALSE;  
      /* register nsp_type_hash here since the call to hash_create 
       * cause a recursive call to nsp_register_type for nsp_type_hash which is aborted */
      nsp_register_type(nsp_type_hash); 
    }
  if ( type == NULL ) return TRUE ; /* ignore */
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  if (( nsp_type = (NspObject *) type_create(top->s_type(), type ,NULL)) == NULLOBJ ) return FALSE; 
  if (nsp_hash_enter(nsp_types_hash_table,nsp_type) == FAIL) return FALSE; 
  return TRUE;
}

/**
 * nsp_get_type_from_id:
 * @id: a #NspTypeId which gives the id (integer) of a type
 * 
 * Return value: a pointer to a #NspType or NULL
 **/

void *nsp_get_type_from_id(NspTypeId id)
{
  NspObject *O;
  int i=0; 
  if ( id <= nsp_no_type_id ) return NULL ;     /* id are > 0 */ 
  if ( nsp_types_hash_table == NULLHASH || nsp_types_hash_table->filled == 0) return NULL;
  while (1) 
    {
      if (nsp_hash_get_next_object(nsp_types_hash_table,&i,&O) == FAIL ) return NULL;
      if ( O != NULLOBJ )
	{ 
	  if ( ((NspType *) O)->nsp_type->id == id) return ((NspType *) O)->nsp_type; 
	}
    }
  return NULL;
}

/**
 * nsp_get_type_from_name:
 * @name: the name of a class 
 *
 * returns a type given its name. 
 *
 * Return value: a pointer to a #NspType or NULL
 **/

void *nsp_get_type_from_name(const char *name)
{
  NspObject *O;
  if ( nsp_types_hash_table == NULLHASH)  return NULL;
  if (nsp_hash_find(nsp_types_hash_table,name,&O) == FAIL) return NULL; 
  return ((NspType *) O)->nsp_type;
}

