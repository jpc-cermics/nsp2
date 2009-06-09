/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/graphic.override"

#include <nsp/figure.h>
#include "../interp/Eval.h"
extern void nsp_graphic_link_figure(NspGraphic *G, void *F);
extern void nsp_graphic_unlink_figure(NspGraphic *G, void *F);

#line 19 "graphic.c"

/* ----------- Graphic ----------- */


#define  Graphic_Private 
#include "nsp/object.h"
#include "nsp/graphic.h"
#include "nsp/interf.h"

/* 
 * NspGraphic inherits from NspObject 
 */

int nsp_type_graphic_id=0;
NspTypeGraphic *nsp_type_graphic=NULL;

/*
 * Type object for Graphic 
 * all the instance of NspTypeGraphic share the same id. 
 * nsp_type_graphic: is an instance of NspTypeGraphic 
 *    used for objects of NspGraphic type (i.e built with new_graphic) 
 * other instances are used for derived classes 
 */
NspTypeGraphic *new_type_graphic(type_mode mode)
{
  NspTypeGraphic *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_graphic != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_graphic;
    }
  if ((type =  malloc(sizeof(NspTypeGraphic))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = graphic_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = graphic_get_methods; 
  type->new = (new_func *) new_graphic;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for graphic */ 

  top->pr = (print_func *) nsp_graphic_print;                  
  top->dealloc = (dealloc_func *) nsp_graphic_destroy;
  top->copy  =  (copy_func *) nsp_graphic_copy;                 
  top->size  = (size_func *) nsp_graphic_size;                
  top->s_type =  (s_type_func *) nsp_graphic_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_graphic_type_short_string;
  top->info = (info_func *) nsp_graphic_info ;                  
  /* top->is_true = (is_true_func  *) nsp_graphic_is_true; */
  /* top->loop =(loop_func *) nsp_graphic_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_graphic_object;
  top->eq  = (eq_func *) nsp_graphic_eq;
  top->neq  = (eq_func *) nsp_graphic_neq;
  top->save  = (save_func *) nsp_graphic_xdr_save;
  top->load  = (load_func *) nsp_graphic_xdr_load;
  top->create = (create_func*) int_graphic_create;
  top->latex = (print_func *) nsp_graphic_latex;
  
  /* specific methods for graphic */
      
  type->init = (init_func *) init_graphic;

#line 55 "codegen/graphic.override"

  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   * this method of class Graphic are to be defined by subclasses.
   */
  type->draw = NULL;
  type->translate = NULL;
  type->rotate = NULL;
  type->scale = NULL;
  type->bounds = NULL;
  type->full_copy = NULL; 
  type->link_figure = nsp_graphic_link_figure;
  type->unlink_figure = nsp_graphic_unlink_figure;
  type->children = NULL;
  type->zmean = NULL; 
  type->n_faces = NULL;

#line 108 "graphic.c"
  /* 
   * Graphic interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_graphic_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGraphic called nsp_type_graphic
       */
      type->id =  nsp_type_graphic_id = nsp_new_type_id();
      nsp_type_graphic = type;
      if ( nsp_register_type(nsp_type_graphic) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_graphic(mode);
    }
  else 
    {
       type->id = nsp_type_graphic_id;
       return type;
    }
}

/*
 * initialize Graphic instances 
 * locally and by calling initializer on parent class 
 */

static int init_graphic(NspGraphic *Obj,NspTypeGraphic *type)
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
 * new instance of Graphic 
 */

NspGraphic *new_graphic() 
{
  NspGraphic *loc; 
  /* type must exists */
  nsp_type_graphic = new_type_graphic(T_BASE);
  if ( (loc = malloc(sizeof(NspGraphic)))== NULLGRAPHIC) return loc;
  /* initialize object */
  if ( init_graphic(loc,nsp_type_graphic) == FAIL) return NULLGRAPHIC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Graphic 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_graphic_size(NspGraphic *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char graphic_type_name[]="Graphic";
static char graphic_short_type_name[]="graphic";

static char *nsp_graphic_type_as_string(void)
{
  return(graphic_type_name);
}

static char *nsp_graphic_type_short_string(NspObject *v)
{
  return(graphic_short_type_name);
}

/*
 * A == B 
 */

static int nsp_graphic_eq(NspGraphic *A, NspObject *B)
{
  NspGraphic *loc = (NspGraphic *) B;
  if ( check_cast(B,nsp_type_graphic_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->hidden != loc->obj->hidden) return FALSE;
  if ( A->obj->Fig != loc->obj->Fig) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_graphic_neq(NspGraphic *A, NspObject *B)
{
  return ( nsp_graphic_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_graphic_xdr_save(XDR *xdrs, NspGraphic *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_graphic)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->hidden) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGraphic  *nsp_graphic_xdr_load_partial(XDR *xdrs, NspGraphic *M)
{
  if ((M->obj = calloc(1,sizeof(nsp_graphic))) == NULL) return NULL;
  M->obj->ref_count=1;
  if (nsp_xdr_load_i(xdrs, &M->obj->hidden) == FAIL) return NULL;
 return M;
}

static NspGraphic  *nsp_graphic_xdr_load(XDR *xdrs)
{
  NspGraphic *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRAPHIC;
  if ((H  = nsp_graphic_create_void(name,(NspTypeBase *) nsp_type_graphic))== NULLGRAPHIC) return H;
  if ((H  = nsp_graphic_xdr_load_partial(xdrs,H))== NULLGRAPHIC) return H;
  if ( nsp_graphic_check_values(H) == FAIL) return NULLGRAPHIC;
#line 251 "graphic.c"
  return H;
}

/*
 * delete 
 */

void nsp_graphic_destroy_partial(NspGraphic *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 264 "graphic.c"
    FREE(H->obj);
   }
}

void nsp_graphic_destroy(NspGraphic *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_graphic_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_graphic_info(NspGraphic *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRAPHIC) 
    {
      Sciprintf("Null Pointer Graphic \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_graphic_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_graphic_print(NspGraphic *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRAPHIC) 
    {
      Sciprintf("Null Pointer Graphic \n");
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
          nsp_graphic_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_graphic_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"hidden	= %s\n", ( M->obj->hidden == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"Fig=%xl\n",M->obj->Fig);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_graphic_latex(NspGraphic *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_graphic_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"hidden	= %s\n", ( M->obj->hidden == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"Fig=%xl\n",M->obj->Fig);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Graphic objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspGraphic   *nsp_graphic_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_graphic_id) == TRUE ) return ((NspGraphic *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_graphic));
  return NULL;
}

int IsGraphicObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_graphic_id);
}

int IsGraphic(NspObject *O)
{
  return nsp_object_type(O,nsp_type_graphic_id);
}

NspGraphic  *GetGraphicCopy(Stack stack, int i)
{
  if (  GetGraphic(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGraphic  *GetGraphic(Stack stack, int i)
{
  NspGraphic *M;
  if (( M = nsp_graphic_object(NthObj(i))) == NULLGRAPHIC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspGraphic *nsp_graphic_create_void(char *name,NspTypeBase *type)
{
 NspGraphic *H  = (type == NULL) ? new_graphic() : type->new();
 if ( H ==  NULLGRAPHIC)
  {
   Sciprintf("No more memory\n");
   return NULLGRAPHIC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRAPHIC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_graphic_create_partial(NspGraphic *H)
{
  if((H->obj = calloc(1,sizeof(nsp_graphic)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->hidden = FALSE;
  H->obj->Fig = NULL;
  return OK;
}

int nsp_graphic_check_values(NspGraphic *H)
{
  return OK;
}

NspGraphic *nsp_graphic_create(char *name,Boolean hidden,void* Fig,NspTypeBase *type)
{
 NspGraphic *H  = nsp_graphic_create_void(name,type);
 if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_create_partial(H) == FAIL) return NULLGRAPHIC;
  H->obj->hidden=hidden;
  H->obj->Fig = Fig;
 if ( nsp_graphic_check_values(H) == FAIL) return NULLGRAPHIC;
 return H;
}


NspGraphic *nsp_graphic_create_default(char *name)
{
 NspGraphic *H  = nsp_graphic_create_void(name,NULL);
 if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_create_partial(H) == FAIL) return NULLGRAPHIC;
 if ( nsp_graphic_check_values(H) == FAIL) return NULLGRAPHIC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGraphic *nsp_graphic_copy_partial(NspGraphic *H,NspGraphic *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGraphic *nsp_graphic_copy(NspGraphic *self)
{
  NspGraphic *H  =nsp_graphic_create_void(NVOID,(NspTypeBase *) nsp_type_graphic);
  if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_copy_partial(H,self)== NULL) return NULLGRAPHIC;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspGraphic *nsp_graphic_full_copy_partial(NspGraphic *H,NspGraphic *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_graphic))) == NULL) return NULLGRAPHIC;
  H->obj->ref_count=1;
  H->obj->hidden=self->obj->hidden;
  H->obj->Fig = self->obj->Fig;
  return H;
}

NspGraphic *nsp_graphic_full_copy(NspGraphic *self)
{
  NspGraphic *H  =nsp_graphic_create_void(NVOID,(NspTypeBase *) nsp_type_graphic);
  if ( H ==  NULLGRAPHIC) return NULLGRAPHIC;
  if ( nsp_graphic_full_copy_partial(H,self)== NULL) return NULLGRAPHIC;
#line 473 "graphic.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Graphic
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_graphic_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGraphic *H;
  CheckStdRhs(0,0);
  /* want to be sure that type graphic is initialized */
  nsp_type_graphic = new_type_graphic(T_BASE);
  if(( H = nsp_graphic_create_void(NVOID,(NspTypeBase *) nsp_type_graphic)) == NULLGRAPHIC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_graphic_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_graphic_check_values(H) == FAIL) return RET_BUG;
#line 493 "graphic.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

#line 74 "codegen/graphic.override"
/* take care that the name to give for override is the c-name of 
 * the method 
 */
static int _wrap_graphic_translate(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,t_end};
  NspMatrix *tr;
  if ( GetArgs(stack,rhs,opt,T,&tr) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,tr,2);
  self->type->translate(NULL,self, tr->R);
  return 0;
}

#line 512 "graphic.c"


#line 89 "codegen/graphic.override"
static int _wrap_graphic_scale(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,t_end};
  NspMatrix *sc;

  if ( GetArgs(stack,rhs,opt,T,&sc) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,sc,2);
  self->type->scale(NULL,self, sc->R);
  return 0;
}

#line 527 "graphic.c"


#line 102 "codegen/graphic.override"
static int _wrap_graphic_rotate(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,t_end};
  NspMatrix *R;
  if ( GetArgs(stack,rhs,opt,T,&R) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,R,2);
  self->type->rotate(NULL,self, R->R);
  return 0;
}

#line 541 "graphic.c"


#line 114 "codegen/graphic.override"
static int _wrap_graphic_full_copy(NspGraphic *self,Stack stack,int rhs,int opt,int lhs)
{
  NspGraphic *ret;
  CheckRhs(0,0);
  ret = self->type->full_copy(self);
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 554 "graphic.c"


static NspMethods graphic_methods[] = {
  {"translate",(nsp_method *) _wrap_graphic_translate},
  {"scale",(nsp_method *) _wrap_graphic_scale},
  {"rotate",(nsp_method *) _wrap_graphic_rotate},
  {"full_copy",(nsp_method *) _wrap_graphic_full_copy},
  { NULL, NULL}
};

static NspMethods *graphic_get_methods(void) { return graphic_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_graphic_get_hidden(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGraphic *) self)->obj->hidden;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_graphic_set_hidden(void *self, char *attr, NspObject *O)
{
  int hidden;

  if ( BoolScalar(O,&hidden) == FAIL) return FAIL;
  ((NspGraphic *) self)->obj->hidden= hidden;
  return OK;
}

static AttrTab graphic_attrs[] = {
  { "hidden", (attr_get_function *)_wrap_graphic_get_hidden, (attr_set_function *)_wrap_graphic_set_hidden,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Graphic_func[]={
  { "graphic_create", int_graphic_create},
  { NULL, NULL}
};

/* call ith function in the Graphic interface */

int Graphic_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Graphic_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Graphic_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Graphic_func[i].name;
  *f = Graphic_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Graphic_register_classes(NspObject *d)
{

#line 12 "codegen/graphic.override"

GLURP 


#line 633 "graphic.c"
  nspgobject_register_class(d, "Graphic", Graphic, &NspGraphic_Type, Nsp_BuildValue("(O)", &NspObject_Type));
}
*/

#line 125 "codegen/graphic.override"

/* verbatim at the end */
/* default methods in graphic */

void nsp_graphic_link_figure(NspGraphic *G,void *F)
{
  nsp_figure *Fi = F;
  if ( G->obj->Fig == NULL ) 
    {
      /* 
      Fi->obj->ref_count++;
      ((NspGraphic *) Fi)->obj->ref_count++;
      */
      G->obj->Fig = Fi;
    }
}

void nsp_graphic_unlink_figure(NspGraphic *G, void *F)
{
  /* NspFigure *Fi = F;*/
  if ( G->obj->Fig == F ) 
    {
      /* 
      Fi->obj->ref_count--;
      ((NspGraphic *) Fi)->obj->ref_count--;
      */
      G->obj->Fig = NULL ;
    }
}

/* interface shared by all graphic objects */

/*
 * Extract requested child of a graphicobject 
 * the returned object is not copied.
 */

int int_nspgraphic_extract_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Elts;
  int i, nret, L_name;
  NspObject *O;
  NspList *L;
  NspGraphic *Gr;
  
  CheckRhs (2,2);
  if ((Gr=GetGraphic(stack,1)) == NULLGRAPHIC) return RET_BUG;
  if ((Elts = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( Gr->type->children == NULL) return RET_BUG;
  if ((L = Gr->type->children(Gr)) == NULLLIST) return RET_BUG;
  
  nret = Elts->mn;
  L_name = Ocheckname(L,NVOID);

  for ( i = 0 ; i < nret ; i++ )
    {
      if ( (O=nsp_list_get_element(L,((int) Elts->R[i]))) ==  NULLOBJ )
	return RET_BUG;  /* error message done in nsp_list_get_element */
      /* If NspList has no name or list element has no name we must copy */
      if ( L_name || Ocheckname(O,NVOID) ) 
	if ( (O=nsp_object_copy(O)) == NULLOBJ )  return RET_BUG;
      NthObj(rhs+i+1) = O;
    }

  nsp_void_object_destroy(&NthObj(1));
  nsp_void_object_destroy(&NthObj(2));
  for ( i = 0 ; i < nret ; i++) 
    {
      NthObj(i+1)= NthObj(i+rhs+1);
      NSP_OBJECT(NthObj(i+1))->ret_pos = i+1;
      NthObj(i+rhs+1)= NULLOBJ;
    }
  return nret;
}

/* extract a field  grobject('field') <=> grobject.field
 * or grobject.get['field']
 *
 */

int int_nspgraphic_extract_s(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Gr;
  NspObject *Ret;
  NspSMatrix *S;
  int i,j,count=0;
  CheckRhs(2,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  if ((Gr= (NspObject *) GetGraphic(stack,1)) == NULLOBJ) return RET_BUG;
  for ( j = 2 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  Ret = nsp_get_attribute_util(Gr,Gr->basetype,S->S[i]);
	  if ( Ret == NULL) return RET_BUG;
	  NthObj(rhs+ ++count) = Ret ;
	  NSP_OBJECT(Ret)->ret_pos = count;
	  if (count == lhs) break;
	}
      if (count == lhs) break;
    }
  return count;
}

/* extraction part when argument is a list  */ 

int int_nspgraphic_extract_l(Stack stack, int rhs, int opt, int lhs)
{
  char name[NAME_MAXL];
  int rep,n ;
  if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
  if ( rep == 3 ) 
    {
      /* last extraction : here O can be anything */ 
      nsp_build_funcname("extractelts",&stack,stack.first+1,1,name);
      if ((n=nsp_eval_func(NULLOBJ,name,2,stack,stack.first+1,2,0,1)) < 0) 
	{
	  return RET_BUG;
	}
    }
  nsp_void_object_destroy(&NthObj(1));
  NSP_OBJECT(NthObj(2))->ret_pos = 1;
  return 1;
}

/* extraction part */

int int_nspgraphic_extract(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(2,2);
  if (IsMatObj(stack,2)) 
    {
      return int_nspgraphic_extract_m(stack,rhs,opt,lhs);
    }
  else if (IsSMatObj(stack,2)) 
    {
      return int_nspgraphic_extract_s(stack,rhs,opt,lhs);
    }
  else if ( IsListObj(stack,2) ) 
    {
      return int_nspgraphic_extract_l(stack,rhs,opt,lhs);
    }
  else 
    {
      Scierror("Error: Wrong type for argument in list extraction int or list required\n");
      return RET_BUG;
    }
  return 1;
}

/*
 * set an attribute value. 
 */

int int_graphic_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Gr;
  NspObject *Obj;
  const char *name;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((Gr= (NspObject *) GetGraphic(stack,1)) == NULLOBJ) return RET_BUG;
  if ( IsSMatObj(stack,2) )
    {
      if ((name = GetString(stack,2)) == (char*)0) return RET_BUG;  
    }
  else 
    {
      Scierror("%s: indice for extraction should be a string\n",NspFname(stack));
      return RET_BUG;
    }
  if ((  Obj = nsp_get_object(stack,3)) == NULLOBJ ) return RET_BUG;
  if ( nsp_set_attribute_util(Gr,Gr->basetype,name,Obj) == FAIL) return RET_BUG;
  NSP_OBJECT(Gr)->ret_pos = 1;
  return 1;
}


#line 819 "graphic.c"
