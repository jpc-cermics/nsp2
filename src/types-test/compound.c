/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 25 "codegen/compound.override"
#include <nsp/figuredata.h> 
#include <nsp/figure.h>
#include <nsp/curve.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 21 "compound.c"

/* ----------- NspCompound ----------- */


#define  NspCompound_Private 
#include <nsp/object.h>
#include <nsp/compound.h>
#include <nsp/interf.h>

/* 
 * NspCompound inherits from Graphic 
 */

int nsp_type_compound_id=0;
NspTypeNspCompound *nsp_type_compound=NULL;

/*
 * Type object for NspCompound 
 * all the instance of NspTypeNspCompound share the same id. 
 * nsp_type_compound: is an instance of NspTypeNspCompound 
 *    used for objects of NspCompound type (i.e built with new_compound) 
 * other instances are used for derived classes 
 */
NspTypeNspCompound *new_type_compound(type_mode mode)
{
  NspTypeNspCompound *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_compound != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_compound;
    }
  if ((type =  malloc(sizeof(NspTypeNspCompound))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = compound_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = compound_get_methods; 
  type->new = (new_func *) new_compound;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for compound */ 

  top->pr = (print_func *) nsp_compound_print;                  
  top->dealloc = (dealloc_func *) nsp_compound_destroy;
  top->copy  =  (copy_func *) nsp_compound_copy;                 
  top->size  = (size_func *) nsp_compound_size;                
  top->s_type =  (s_type_func *) nsp_compound_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_compound_type_short_string;
  top->info = (info_func *) nsp_compound_info ;                  
  /* top->is_true = (is_true_func  *) nsp_compound_is_true; */
  /* top->loop =(loop_func *) nsp_compound_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_compound_object;
  top->eq  = (eq_func *) nsp_compound_eq;
  top->neq  = (eq_func *) nsp_compound_neq;
  top->save  = (save_func *) nsp_compound_xdr_save;
  top->load  = (load_func *) nsp_compound_xdr_load;
  top->create = (create_func*) int_compound_create;
  top->latex = (print_func *) nsp_compound_latex;
  
  /* specific methods for compound */
      
  type->init = (init_func *) init_compound;

#line 40 "codegen/compound.override"
  /* inserted verbatim in the type definition */
  ((NspTypeNspGraphic *) type->surtype)->draw = nsp_draw_compound;
  ((NspTypeNspGraphic *) type->surtype)->translate =nsp_translate_compound ;
  ((NspTypeNspGraphic *) type->surtype)->rotate =nsp_rotate_compound  ;
  ((NspTypeNspGraphic *) type->surtype)->scale =nsp_scale_compound  ;
  ((NspTypeNspGraphic *) type->surtype)->bounds =nsp_getbounds_compound  ;
  ((NspTypeNspGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_compound_full_copy ;
  ((NspTypeNspGraphic *) type->surtype)->link_figure = nsp_compound_link_figure; 
  ((NspTypeNspGraphic *) type->surtype)->unlink_figure = nsp_compound_unlink_figure; 
  ((NspTypeNspGraphic *) type->surtype)->children = (children_func *) nsp_compound_children ;
#line 103 "compound.c"
  /* 
   * NspCompound interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_compound_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeNspCompound called nsp_type_compound
       */
      type->id =  nsp_type_compound_id = nsp_new_type_id();
      nsp_type_compound = type;
      if ( nsp_register_type(nsp_type_compound) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_compound(mode);
    }
  else 
    {
       type->id = nsp_type_compound_id;
       return type;
    }
}

/*
 * initialize NspCompound instances 
 * locally and by calling initializer on parent class 
 */

static int init_compound(NspCompound *Obj,NspTypeNspCompound *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspCompound 
 */

NspCompound *new_compound() 
{
  NspCompound *loc; 
  /* type must exists */
  nsp_type_compound = new_type_compound(T_BASE);
  if ( (loc = malloc(sizeof(NspCompound)))== NULLCOMPOUND) return loc;
  /* initialize object */
  if ( init_compound(loc,nsp_type_compound) == FAIL) return NULLCOMPOUND;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspCompound 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_compound_size(NspCompound *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char compound_type_name[]="Compound";
static char compound_short_type_name[]="compound";

static char *nsp_compound_type_as_string(void)
{
  return(compound_type_name);
}

static char *nsp_compound_type_short_string(NspObject *v)
{
  return(compound_short_type_name);
}

/*
 * A == B 
 */

static int nsp_compound_eq(NspCompound *A, NspObject *B)
{
  NspCompound *loc = (NspCompound *) B;
  if ( check_cast(B,nsp_type_compound_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->bounds)->type->eq(A->obj->bounds,loc->obj->bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_compound_neq(NspCompound *A, NspObject *B)
{
  return ( nsp_compound_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_compound_xdr_save(XDR *xdrs, NspCompound *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_compound)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCompound  *nsp_compound_xdr_load_partial(XDR *xdrs, NspCompound *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_compound))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspCompound  *nsp_compound_xdr_load(XDR *xdrs)
{
  NspCompound *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCOMPOUND;
  if ((H  = nsp_compound_create_void(name,(NspTypeBase *) nsp_type_compound))== NULLCOMPOUND) return H;
  if ((H  = nsp_compound_xdr_load_partial(xdrs,H))== NULLCOMPOUND) return H;
  if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
#line 256 "compound.c"
  return H;
}

/*
 * delete 
 */

void nsp_compound_destroy_partial(NspCompound *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 270 "compound.c"
    nsp_matrix_destroy(H->obj->bounds);
    nsp_list_destroy(H->obj->children);
    FREE(H->obj);
   }
}

void nsp_compound_destroy(NspCompound *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_compound_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_compound_info(NspCompound *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer NspCompound \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_compound_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_compound_print(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer NspCompound \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_compound_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_compound_latex(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspCompound objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspCompound   *nsp_compound_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_compound_id) == TRUE ) return ((NspCompound *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_compound));
  return NULL;
}

int IsCompoundObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_compound_id);
}

int IsCompound(NspObject *O)
{
  return nsp_object_type(O,nsp_type_compound_id);
}

NspCompound  *GetCompoundCopy(Stack stack, int i)
{
  if (  GetCompound(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCompound  *GetCompound(Stack stack, int i)
{
  NspCompound *M;
  if (( M = nsp_compound_object(NthObj(i))) == NULLCOMPOUND)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspCompound *nsp_compound_create_void(char *name,NspTypeBase *type)
{
 NspCompound *H  = (type == NULL) ? new_compound() : type->new();
 if ( H ==  NULLCOMPOUND)
  {
   Sciprintf("No more memory\n");
   return NULLCOMPOUND;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCOMPOUND;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_compound_create_partial(NspCompound *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_compound)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->bounds = NULLMAT;
  H->obj->children = NULLLIST;
  return OK;
}

int nsp_compound_check_values(NspCompound *H)
{
  if ( H->obj->bounds == NULLMAT) 
    {
     double x_def[4]={0};
     if (( H->obj->bounds = nsp_matrix_create("bounds",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->bounds->R,x_def,4*sizeof(double));
  }
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspCompound *nsp_compound_create(char *name,NspMatrix* bounds,NspList* children,NspTypeBase *type)
{
 NspCompound *H  = nsp_compound_create_void(name,type);
 if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
  H->obj->bounds= bounds;
  H->obj->children= children;
 if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
 return H;
}


NspCompound *nsp_compound_create_default(char *name)
{
 NspCompound *H  = nsp_compound_create_void(name,NULL);
 if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
 if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCompound *nsp_compound_copy_partial(NspCompound *H,NspCompound *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspCompound *nsp_compound_copy(NspCompound *self)
{
  NspCompound *H  =nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCOMPOUND;
  if ( nsp_compound_copy_partial(H,self)== NULL) return NULLCOMPOUND;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspCompound *nsp_compound_full_copy_partial(NspCompound *H,NspCompound *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_compound))) == NULL) return NULLCOMPOUND;
  H->obj->ref_count=1;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_copy_and_name("bounds",NSP_OBJECT(self->obj->bounds))) == NULLMAT) return NULL;
    }
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_copy_and_name("children",NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  return H;
}

NspCompound *nsp_compound_full_copy(NspCompound *self)
{
  NspCompound *H  =nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCOMPOUND;
  if ( nsp_compound_full_copy_partial(H,self)== NULL) return NULLCOMPOUND;
#line 517 "compound.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspCompound
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_compound_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCompound *H;
  CheckStdRhs(0,0);
  /* want to be sure that type compound is initialized */
  nsp_type_compound = new_type_compound(T_BASE);
  if(( H = nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound)) == NULLCOMPOUND) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_compound_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_compound_check_values(H) == FAIL) return RET_BUG;
#line 537 "compound.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *compound_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 102 "codegen/compound.override"

/* here we override get_obj  and set_obj 
 * we want get to be followed by a set to check that 
 * inserted value is correct thus we use copy = TRUE.
 */

static NspObject *_wrap_compound_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE; 
  ret = ((NspList*) ((NspCompound *) self)->obj->children);
  return (NspObject *) ret;
}

/* in this function we can check that val is correct before 
 * setting the field with val. return FAIL if val is incorrect.
 */

static int _wrap_compound_set_obj_children(void *self,NspObject *val)
{
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (((NspCompound *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspCompound *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspCompound *) self)->obj->children);
    }
  ((NspCompound *) self)->obj->children =  (NspList *) val;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig);
  nsp_compound_compute_inside_bounds(NULL,self);
  return OK;
}

static int _wrap_compound_set_children(void *self, char *attr, NspObject *O)
{
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspCompound *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspCompound *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspCompound *) self)->obj->children);
    }
  ((NspCompound *) self)->obj->children= children;
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig);
  nsp_compound_compute_inside_bounds(NULL,self);
  return OK;
}


#line 602 "compound.c"
static NspObject *_wrap_compound_get_children(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspCompound *) self)->obj->children;
  return (NspObject *) ret;
}

static AttrTab compound_attrs[] = {
  { "children", (attr_get_function *)_wrap_compound_get_children, (attr_set_function *)_wrap_compound_set_children,(attr_get_object_function *)_wrap_compound_get_obj_children, (attr_set_object_function *)_wrap_compound_set_obj_children },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 60 "codegen/compound.override"
int _wrap_compound_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  tape_store_graphic_object(Xgc, pl);
  return 0;
}

#line 632 "compound.c"


#line 158 "codegen/compound.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_compound(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 644 "compound.c"


#line 168 "codegen/compound.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_compound(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 657 "compound.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Compound_func[]={
  {"compound_attach", _wrap_compound_attach},
  {"extractelts_compound", _wrap_nsp_extractelts_compound},
  {"setrowscols_compound", _wrap_nsp_setrowscols_compound},
  { "compound_create", int_compound_create},
  { NULL, NULL}
};

/* call ith function in the Compound interface */

int Compound_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Compound_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Compound_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Compound_func[i].name;
  *f = Compound_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Compound_register_classes(NspObject *d)
{

#line 35 "codegen/compound.override"

Init portion 


#line 698 "compound.c"
  nspgobject_register_class(d, "NspCompound", Compound, &NspNspCompound_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 179 "codegen/compound.override"

/* inserted verbatim at the end */

static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspCompound *P = (NspCompound *) Obj;
  NspList *L = P->obj->children;
  Cell *cloc = L->first;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  /* draw elements */
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G,NULL);
	}
      cloc = cloc->next;
    }
}

/* compute the bounds of the set of objects countained in the 
 * compound. This function is to be called when contained 
 * objects are changed.
 */

static void nsp_compound_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj)
{
  double l_bounds[4],bounds[4];
  Cell *cloc;
  NspList *L;
  NspCompound *P = (NspCompound *) Obj;
  L = P->obj->children;
  cloc = L->first ;
  if ( cloc == NULLCELL) 
    {
      bounds[0]=bounds[1]=0;
      bounds[2]=bounds[3]=0;
      return;
    }
  
  bounds[0]=bounds[1]=LARGEST_REAL;
  bounds[2]=bounds[3]=-LARGEST_REAL;

  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->bounds(Xgc,G,l_bounds);
	  if ( l_bounds[0] < bounds[0] ) 
	    bounds[0]= l_bounds[0];
	  if (  l_bounds[2] > bounds[2])
	    bounds[2]= l_bounds[2];
	  if ( l_bounds[1] < bounds[1] ) 
	    bounds[1]= l_bounds[1];
	  if (  l_bounds[3] > bounds[3])
	    bounds[3]= l_bounds[3];
	}
      cloc = cloc->next;
    }
  memcpy(P->obj->bounds->R,bounds,4*sizeof(double));
}

/* Note that the bounds should be changed here
 */

static void nsp_translate_compound(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  int draw_now;
  NspCompound *P = (NspCompound *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* just in case we inihibit the draw during the 
   * while 
   */
  draw_now = ((nsp_figure *) Obj->obj->Fig)->draw_now;
  ((nsp_figure *) Obj->obj->Fig)->draw_now =  FALSE;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->translate(Xgc,G,tr);
	}
      cloc = cloc->next;
    }
  ((nsp_figure *) Obj->obj->Fig)->draw_now = draw_now;
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_compound(BCG *Xgc,NspGraphic *Obj,double *R)
{
  int draw_now;
  NspCompound *P = (NspCompound *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* just in case we inihibit the draw during the 
   * while 
   */
  draw_now = ((nsp_figure *) Obj->obj->Fig)->draw_now;
  ((nsp_figure *) Obj->obj->Fig)->draw_now =  FALSE;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->rotate(Xgc,G,R);
	}
      cloc = cloc->next;
    }
  ((nsp_figure *) Obj->obj->Fig)->draw_now = draw_now;
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_compound(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int draw_now;
  NspCompound *P = (NspCompound *) Obj;
  NspList *L=  P->obj->children;
  Cell *cloc =  L->first ;
  /* just in case we inihibit the draw during the 
   * while 
   */
  draw_now = ((nsp_figure *) Obj->obj->Fig)->draw_now;
  ((nsp_figure *) Obj->obj->Fig)->draw_now =  FALSE;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->scale(Xgc,G,alpha);
	}
      cloc = cloc->next;
    }
  ((nsp_figure *) Obj->obj->Fig)->draw_now = draw_now;
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of compound 
 *
 */

static int nsp_getbounds_compound(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspCompound *P = (NspCompound *) Obj;
  /* XXX should not be always computed */
  nsp_compound_compute_inside_bounds(Xgc,Obj);
  memcpy(bounds,P->obj->bounds->R,4*sizeof(double));
  return TRUE;
}

static void nsp_compound_link_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_link_figure(G, ((NspFigure *) F)->obj);
  /* link children */
  nsp_list_link_figure(((NspCompound *) G)->obj->children,F);
}


static void nsp_compound_unlink_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_unlink_figure(G,   ((NspFigure *) F)->obj);
  /* link children */
  nsp_list_unlink_figure(((NspCompound *) G)->obj->children,F);
}

static NspList *nsp_compound_children(NspGraphic *Obj)
{
  return  ((NspCompound *) Obj)->obj->children;
}



#line 880 "compound.c"
