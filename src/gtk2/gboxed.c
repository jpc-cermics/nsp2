/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GBoxed_Private 
#include "nsp/gtk/gboxed.h"
#include "nsp/interf.h"

/* XXXXX : temporaire ici */ 

#include <gtk/gtk.h>

/* 
 * NspGBoxed inherits from NspObject
 */

int nsp_type_gboxed_id=0;
NspTypeGBoxed *nsp_type_gboxed=NULL;

/*
 * Type object for GBoxed 
 * all the instance of NspTypeGBoxed share the same id. 
 * nsp_type_gboxed: is a an instance of NspTypeGBoxed 
 *    used for objects of NspGBoxed type (i.e built with new_gboxed) 
 * other instances are used for derived classes 
 */

NspTypeGBoxed *new_type_gboxed(type_mode mode)
{
  NspTypeGBoxed *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gboxed != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gboxed;
    }
  
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = gboxed_get_methods; 
  type->new = (new_func *) new_gboxed;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gboxed */ 
  
  top->pr = (print_func *) gboxed_print;                    
  top->dealloc = (dealloc_func *) gboxed_destroy;
  /* top->size  = (size_func *) gboxed_size; */
  top->copy  =  (copy_func *) gboxed_copy;                   
  top->s_type =  (s_type_func *) gboxed_type_as_string;    
  top->sh_type = (sh_type_func *) gboxed_type_short_string;
  top->info = (info_func *) gboxed_info ;                    
  /* top->is_true = (is_true_func  *) GBoxedIsTrue; */
  /* top->loop =(loop_func *) gboxed_loop;*/
  top->path_extract = (path_func *) object_path_extract ;
  top->get_from_obj = (get_from_obj_func *) gboxed_object;
  top->eq  = (eq_func *) gboxed_eq;
  top->neq  = (eq_func *) gboxed_neq;
  /* top->save  = (save_func *) gboxed_xdr_save;*/
  /* top->load  = (load_func *) gboxed_xdr_load;*/

  /* specific methods for gboxed */
      
  type->init = (init_func *) init_gboxed;
      
  /* 
   * GBoxed interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_gboxed_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGBoxed called nsp_type_gboxed
       */
      type->id =  nsp_type_gboxed_id = nsp_new_type_id();
      nsp_type_gboxed = type;
      if ( nsp_register_type(nsp_type_gboxed) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gboxed(mode);
    }
  else 
    {
      type->id = nsp_type_gboxed_id;
      return type;
    }
}

/*
 * initialize GBoxed instances 
 * locally and by calling initializer on parent class 
 */

static int init_gboxed(NspGBoxed *o,NspTypeGBoxed *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  o->boxed = NULL;
  o->gtype = 0;
  o->free_on_dealloc= TRUE;
  return OK;
}

/*
 * new instance of GBoxed 
 */

NspGBoxed *new_gboxed() 
{
  NspGBoxed *loc; 
  /* type must exists */
  nsp_type_gboxed = new_type_gboxed(T_BASE);
  if ( (loc = malloc(sizeof(NspGBoxed)))== NULLGBOXED) return loc;
  /* initialize object */
  if ( init_gboxed(loc,nsp_type_gboxed) == FAIL) return NULLGBOXED;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GBoxed 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gboxed_type_name[]="GBoxed";
static char gboxed_short_type_name[]="gb";

static char *gboxed_type_as_string(void)
{
  return(gboxed_type_name);
}

static char *gboxed_type_short_string(void)
{
  return(gboxed_short_type_name);
}

static int gboxed_full_comp(NspGBoxed * A,NspGBoxed * B,char *op,int *err)
{
  Scierror("gboxed_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int gboxed_eq(NspGBoxed *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_gboxed_id) == FALSE) return FALSE ;
  rep = gboxed_full_comp(A,(NspGBoxed *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int gboxed_neq(NspGBoxed *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_gboxed_id) == FALSE) return TRUE;
  rep = gboxed_full_comp(A,(NspGBoxed *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * delete 
 */

void gboxed_destroy(NspGBoxed *self)
{
  
  Scierror("==>destroy boxed %s free=%d at Ox%lx\n",
	   g_type_name(self->gtype),
	   self->free_on_dealloc,self);
  if (self->free_on_dealloc && self->boxed) {
    nspg_unblock_threads();
    g_boxed_free(self->gtype, self->boxed);
    self->boxed = NULL;
    nspg_block_threads();
  }
  FREE(NSP_OBJECT(self)->name);
  FREE(self);
}

/*
 * info 
 */

void gboxed_info(NspGBoxed *self, int indent)
{
  int i;
  if ( self == NULLGBOXED) 
    {
      Sciprintf("Null Pointer GBoxed \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= %s GBoxed at 0x%lx\n", NSP_OBJECT(self)->name,
	    g_type_name(self->gtype),
	    (long)self->boxed);
}

/*
 * print 
 */

void gboxed_print(NspGBoxed *self, int indent)
{
  gboxed_info(self,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GBoxed objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGBoxed   *gboxed_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_gboxed_id) == TRUE) return ((NspGBoxed *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gboxed));
  return(NULL);
}

int IsGBoxedObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gboxed_id);
}

int IsGBoxed(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gboxed_id);
}

NspGBoxed  *GetGBoxedCopy(Stack stack, int i)
{
  if (  GetGBoxed(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGBoxed  *GetGBoxed(Stack stack, int i)
{
  NspGBoxed *M;
  if (( M = gboxed_object(NthObj(i))) == NULLGBOXED)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * @boxed_type: the GType of the boxed value.
 * @boxed: the boxed value.
 * @copy_boxed: whether the new boxed wrapper should hold a copy of the value.
 * @own_ref: whether the boxed wrapper should own the boxed value.
 *
 * Creates a wrapper for a boxed value.  If @copy_boxed is set to
 * True, the wrapper will hold a copy of the value, instead of the
 * value itself.  If @own_ref is True, then the value held by the
 * wrapper will be freed when the wrapper is deallocated.  If
 * @copy_boxed is True, then @own_ref must also be True.
 *
 * Returns: the boxed wrapper.
 *-----------------------------------------------------*/

NspGBoxed *gboxed_create(char *name,GType boxed_type, gpointer boxed, gboolean copy_boxed,
			 gboolean own_ref,void *tp)
{
  NspTypeBase *type = (NspTypeBase *) tp;
  NspGBoxed *self; 

  if ( boxed == NULL) return (NspGBoxed *) none_create(NVOID,NULL);
    
  self =  (type == NULL) ? new_gboxed() : type->new();
  
  if ( self ==  NULLGBOXED)
    {
      Sciprintf("No more memory\n");
      return NULLGBOXED;
    }
  
  if ( ( NSP_OBJECT(self)->name = NewString(name)) == NULLSTRING) return(NULLGBOXED);
  NSP_OBJECT(self)->ret_pos = -1 ;
  
  g_return_val_if_fail(boxed_type != 0, NULL);
  g_return_val_if_fail(!copy_boxed || (copy_boxed && own_ref), NULL);
  g_return_val_if_fail(boxed , NULL);
  
  if (copy_boxed) 
    {
      boxed = g_boxed_copy(boxed_type, boxed);
      own_ref = TRUE;
    }

  Scierror("==>Create a boxed %s copy=%d own ref=%d at Ox%lx\n",g_type_name(boxed_type), copy_boxed,own_ref,self);

  self->boxed = boxed;
  self->gtype = boxed_type;
  self->free_on_dealloc = own_ref;
  return self;
}

/*
 * copy 
 */

NspGBoxed *gboxed_copy(NspGBoxed *self)
{
  NspTypeBase *type = nsp_type_from_gtype(self->gtype);
  return gboxed_create(NVOID,self->gtype, self->boxed, TRUE, TRUE,type);
}

/*-------------------------------------------------------------------
 * wrappers for the GBoxed
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *gboxed_get_methods(void) { return NULL;};


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GBoxed_func[]={
  /* #include "gboxed-in.nam" */ 
  {"setrowscols_gb",int_set_attribute},
  {(char *) 0, NULL}
};

/** call ith function in the GBoxed interface **/

int GBoxed_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GBoxed_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void GBoxed_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GBoxed_func[i].name;
  *f = GBoxed_func[i].fonc;
}

/*------------------------------------------
 * Utilities 
 *------------------------------------------*/

int nspg_boxed_check(NspObject *self,GType boxed_type) 
{
  return ((NspGBoxed *) self)->gtype ==  boxed_type;
}

  
