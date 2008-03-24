/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "figure.override"

#include "nsp/figure.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_figure(BCG *Xgc,NspGraphic *Obj);
static int nsp_figure_connect(NspFigure *);
static int nsp_figure_unconnect(NspFigure *);

#line 21 "figure.c"

/* ----------- Figure ----------- */


#define  Figure_Private 
#include "nsp/object.h"
#include "nsp/figure.h"
#include "nsp/interf.h"

/* 
 * NspFigure inherits from NspGraphic 
 */

int nsp_type_figure_id=0;
NspTypeFigure *nsp_type_figure=NULL;

/*
 * Type object for Figure 
 * all the instance of NspTypeFigure share the same id. 
 * nsp_type_figure: is an instance of NspTypeFigure 
 *    used for objects of NspFigure type (i.e built with new_figure) 
 * other instances are used for derived classes 
 */
NspTypeFigure *new_type_figure(type_mode mode)
{
  NspTypeFigure *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_figure != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_figure;
    }
  if ((type =  malloc(sizeof(NspTypeFigure))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = figure_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = figure_get_methods; 
  type->new = (new_func *) new_figure;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for figure */ 

  top->pr = (print_func *) nsp_figure_print;                  
  top->dealloc = (dealloc_func *) nsp_figure_destroy;
  top->copy  =  (copy_func *) nsp_figure_copy;                 
  top->size  = (size_func *) nsp_figure_size;                
  top->s_type =  (s_type_func *) nsp_figure_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_figure_type_short_string;
  top->info = (info_func *) nsp_figure_info ;                  
  /* top->is_true = (is_true_func  *) nsp_figure_is_true; */
  /* top->loop =(loop_func *) nsp_figure_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_figure_object;
  top->eq  = (eq_func *) nsp_figure_eq;
  top->neq  = (eq_func *) nsp_figure_neq;
  top->save  = (save_func *) nsp_figure_xdr_save;
  top->load  = (load_func *) nsp_figure_xdr_load;
  top->create = (create_func*) int_figure_create;
  top->latex = (print_func *) nsp_figure_latex;
  
  /* specific methods for figure */
      
  type->init = (init_func *) init_figure;

#line 19 "figure.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_figure;

#line 96 "figure.c"
  /* 
   * Figure interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_figure_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeFigure called nsp_type_figure
       */
      type->id =  nsp_type_figure_id = nsp_new_type_id();
      nsp_type_figure = type;
      if ( nsp_register_type(nsp_type_figure) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_figure(mode);
    }
  else 
    {
       type->id = nsp_type_figure_id;
       return type;
    }
}

/*
 * initialize Figure instances 
 * locally and by calling initializer on parent class 
 */

static int init_figure(NspFigure *Obj,NspTypeFigure *type)
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
 * new instance of Figure 
 */

NspFigure *new_figure() 
{
  NspFigure *loc; 
  /* type must exists */
  nsp_type_figure = new_type_figure(T_BASE);
  if ( (loc = malloc(sizeof(NspFigure)))== NULLFIGURE) return loc;
  /* initialize object */
  if ( init_figure(loc,nsp_type_figure) == FAIL) return NULLFIGURE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Figure 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_figure_size(NspFigure *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char figure_type_name[]="Figure";
static char figure_short_type_name[]="figure";

static char *nsp_figure_type_as_string(void)
{
  return(figure_type_name);
}

static char *nsp_figure_type_short_string(NspObject *v)
{
  return(figure_short_type_name);
}

/*
 * A == B 
 */

static int nsp_figure_eq(NspFigure *A, NspObject *B)
{
  NspFigure *loc = (NspFigure *) B;
  if ( check_cast(B,nsp_type_figure_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( strcmp(A->obj->fname,loc->obj->fname) != 0) return FALSE;
  if ( strcmp(A->obj->driver,loc->obj->driver) != 0) return FALSE;
  if ( A->obj->id != loc->obj->id) return FALSE;
  if ( NSP_OBJECT(A->obj->dims)->type->eq(A->obj->dims,loc->obj->dims) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->viewport_dims)->type->eq(A->obj->viewport_dims,loc->obj->viewport_dims) == FALSE ) return FALSE;
  if ( A->obj->wresize != loc->obj->wresize) return FALSE;
  if ( NSP_OBJECT(A->obj->position)->type->eq(A->obj->position,loc->obj->position) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_figure_neq(NspFigure *A, NspObject *B)
{
  return ( nsp_figure_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_figure_xdr_save(XDR *xdrs, NspFigure *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->fname) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->driver) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->id) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->dims)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->viewport_dims)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->wresize) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->position)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspFigure  *nsp_figure_xdr_load_partial(XDR *xdrs, NspFigure *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_figure))) == NULL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->fname)) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->driver)) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->id) == FAIL) return NULL;
  if ((M->obj->dims =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->viewport_dims =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->wresize) == FAIL) return NULL;
  if ((M->obj->position =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspFigure  *nsp_figure_xdr_load(XDR *xdrs)
{
  NspFigure *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLFIGURE;
  if ((M  = nsp_figure_create_void(name,(NspTypeBase *) nsp_type_figure))== NULLFIGURE) return M;
  return nsp_figure_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_figure_destroy_partial(NspFigure *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_string_destroy(&(H->obj->fname));
  nsp_string_destroy(&(H->obj->driver));
    nsp_matrix_destroy(H->obj->dims);
    nsp_matrix_destroy(H->obj->viewport_dims);
    nsp_matrix_destroy(H->obj->position);
    nsp_list_destroy(H->obj->children);
    FREE(H->obj);
   }
}

void nsp_figure_destroy(NspFigure *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_figure_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_figure_info(NspFigure *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLFIGURE) 
    {
      Sciprintf("Null Pointer Figure \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_figure_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_figure_print(NspFigure *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLFIGURE) 
    {
      Sciprintf("Null Pointer Figure \n");
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
          nsp_figure_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_figure_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"driver=%s\n",M->obj->driver);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  if ( M->obj->dims != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->dims),indent+2,"dims",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->viewport_dims != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->viewport_dims),indent+2,"viewport_dims",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"wresize	= %s\n", ( M->obj->wresize == TRUE) ? "T" : "F" );
  if ( M->obj->position != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->position),indent+2,"position",rec_level+1)== FALSE ) return FALSE ;
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

int nsp_figure_latex(NspFigure *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_figure_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"driver=%s\n",M->obj->driver);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  if ( M->obj->dims != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->dims),indent+2,"dims",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->viewport_dims != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->viewport_dims),indent+2,"viewport_dims",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"wresize	= %s\n", ( M->obj->wresize == TRUE) ? "T" : "F" );
  if ( M->obj->position != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->position),indent+2,"position",rec_level+1)== FALSE ) return FALSE ;
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
 * for Figure objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspFigure   *nsp_figure_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_figure_id) == TRUE ) return ((NspFigure *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_figure));
  return NULL;
}

int IsFigureObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_figure_id);
}

int IsFigure(NspObject *O)
{
  return nsp_object_type(O,nsp_type_figure_id);
}

NspFigure  *GetFigureCopy(Stack stack, int i)
{
  if (  GetFigure(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFigure  *GetFigure(Stack stack, int i)
{
  NspFigure *M;
  if (( M = nsp_figure_object(NthObj(i))) == NULLFIGURE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspFigure *nsp_figure_create_void(char *name,NspTypeBase *type)
{
 NspFigure *H  = (type == NULL) ? new_figure() : type->new();
 if ( H ==  NULLFIGURE)
  {
   Sciprintf("No more memory\n");
   return NULLFIGURE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLFIGURE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_figure_create_partial(NspFigure *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_figure)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_figure_check_values(NspFigure *H)
{
  if ( H->obj->fname == NULL) 
    {
     if (( H->obj->fname = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->driver == NULL) 
    {
     if (( H->obj->driver = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->dims == NULLMAT) 
    {
     if (( H->obj->dims = nsp_matrix_create("dims",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->viewport_dims == NULLMAT) 
    {
     if (( H->obj->viewport_dims = nsp_matrix_create("viewport_dims",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->position == NULLMAT) 
    {
     if (( H->obj->position = nsp_matrix_create("position",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspFigure *nsp_figure_create(char *name,char* fname,char* driver,int id,NspMatrix* dims,NspMatrix* viewport_dims,gboolean wresize,NspMatrix* position,NspList* children,NspTypeBase *type)
{
 NspFigure *H  = nsp_figure_create_void(name,type);
 if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_figure_create_partial(H) == FAIL) return NULLFIGURE;
  if ((H->obj->fname = nsp_string_copy(fname)) == NULL) return NULL;
  if ((H->obj->driver = nsp_string_copy(driver)) == NULL) return NULL;
  H->obj->id=id;
  if ( dims == NULL )
    { H->obj->dims = NULL;}
  else
    {
      if ((H->obj->dims = (NspMatrix *)  nsp_object_copy_and_name("dims",NSP_OBJECT(dims))) == NULLMAT) return NULL;
    }
  if ( viewport_dims == NULL )
    { H->obj->viewport_dims = NULL;}
  else
    {
      if ((H->obj->viewport_dims = (NspMatrix *)  nsp_object_copy_and_name("viewport_dims",NSP_OBJECT(viewport_dims))) == NULLMAT) return NULL;
    }
  H->obj->wresize=wresize;
  if ( position == NULL )
    { H->obj->position = NULL;}
  else
    {
      if ((H->obj->position = (NspMatrix *)  nsp_object_copy_and_name("position",NSP_OBJECT(position))) == NULLMAT) return NULL;
    }
  if ( children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *)  nsp_object_copy_and_name("children",NSP_OBJECT(children))) == NULLLIST) return NULL;
    }
 if ( nsp_figure_check_values(H) == FAIL) return NULLFIGURE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspFigure *nsp_figure_copy_partial(NspFigure *H,NspFigure *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspFigure *nsp_figure_copy(NspFigure *self)
{
  NspFigure *H  =nsp_figure_create_void(NVOID,(NspTypeBase *) nsp_type_figure);
  if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLFIGURE;
  if ( nsp_figure_copy_partial(H,self)== NULL) return NULLFIGURE;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Figure
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_figure_create(Stack stack, int rhs, int opt, int lhs)
{
  NspFigure *H;
  CheckStdRhs(0,0);
  /* want to be sure that type figure is initialized */
  nsp_type_figure = new_type_figure(T_BASE);
  if(( H = nsp_figure_create_void(NVOID,(NspTypeBase *) nsp_type_figure)) == NULLFIGURE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_figure_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_figure_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_figure_connect(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_connect(self);
  return 0;
}

static int _wrap_nsp_figure_unconnect(NspFigure *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_figure_unconnect(self);
  return 0;
}

static NspMethods figure_methods[] = {
  {"connect",(nsp_method *) _wrap_nsp_figure_connect},
  {"unconnect",(nsp_method *) _wrap_nsp_figure_unconnect},
  { NULL, NULL}
};

static NspMethods *figure_get_methods(void) { return figure_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_figure_get_fname(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspFigure *) self)->obj->fname;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_figure_set_fname(void *self, char *attr, NspObject *O)
{
  char *fname;

  if ((fname = nsp_string_object(O))==NULL) return FAIL;
  if ((fname = nsp_string_copy(fname)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspFigure *) self)->obj->fname);
  return OK;
}

static NspObject *_wrap_figure_get_driver(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspFigure *) self)->obj->driver;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_figure_set_driver(void *self, char *attr, NspObject *O)
{
  char *driver;

  if ((driver = nsp_string_object(O))==NULL) return FAIL;
  if ((driver = nsp_string_copy(driver)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspFigure *) self)->obj->driver);
  return OK;
}

static NspObject *_wrap_figure_get_id(void *self,char *attr)
{
  int ret;

  ret = ((NspFigure *) self)->obj->id;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_id(void *self, char *attr, NspObject *O)
{
  int id;

  if ( IntScalar(O,&id) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_figure_get_dims(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFigure *) self)->obj->dims;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_dims(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFigure *) self)->obj->dims);
  return (NspObject *) ret;
}

static int _wrap_figure_set_dims(void *self, char *attr, NspObject *O)
{
  NspMatrix *dims;

  if ( ! IsMat(O) ) return FAIL;
  if ((dims = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigure *) self)->obj->dims != NULL ) 
    nsp_matrix_destroy(((NspFigure *) self)->obj->dims);
  return OK;
}

static NspObject *_wrap_figure_get_viewport_dims(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFigure *) self)->obj->viewport_dims;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_viewport_dims(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFigure *) self)->obj->viewport_dims);
  return (NspObject *) ret;
}

static int _wrap_figure_set_viewport_dims(void *self, char *attr, NspObject *O)
{
  NspMatrix *viewport_dims;

  if ( ! IsMat(O) ) return FAIL;
  if ((viewport_dims = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigure *) self)->obj->viewport_dims != NULL ) 
    nsp_matrix_destroy(((NspFigure *) self)->obj->viewport_dims);
  return OK;
}

static NspObject *_wrap_figure_get_wresize(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspFigure *) self)->obj->wresize;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_figure_set_wresize(void *self, char *attr, NspObject *O)
{
  int wresize;

  if ( BoolScalar(O,&wresize) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_figure_get_position(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFigure *) self)->obj->position;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_position(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFigure *) self)->obj->position);
  return (NspObject *) ret;
}

static int _wrap_figure_set_position(void *self, char *attr, NspObject *O)
{
  NspMatrix *position;

  if ( ! IsMat(O) ) return FAIL;
  if ((position = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFigure *) self)->obj->position != NULL ) 
    nsp_matrix_destroy(((NspFigure *) self)->obj->position);
  return OK;
}

static NspObject *_wrap_figure_get_children(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspFigure *) self)->obj->children;
  return (NspObject *) ret;
}

static NspObject *_wrap_figure_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;

  *copy = FALSE;
  ret = ((NspList*) ((NspFigure *) self)->obj->children);
  return (NspObject *) ret;
}

static int _wrap_figure_set_children(void *self, char *attr, NspObject *O)
{
  NspList *children;

  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspFigure *) self)->obj->children != NULL ) 
    nsp_list_destroy(((NspFigure *) self)->obj->children);
  return OK;
}

static AttrTab figure_attrs[] = {
  { "fname", (attr_get_function *)_wrap_figure_get_fname, (attr_set_function *)_wrap_figure_set_fname,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "driver", (attr_get_function *)_wrap_figure_get_driver, (attr_set_function *)_wrap_figure_set_driver,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "id", (attr_get_function *)_wrap_figure_get_id, (attr_set_function *)_wrap_figure_set_id,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "dims", (attr_get_function *)_wrap_figure_get_dims, (attr_set_function *)_wrap_figure_set_dims,(attr_get_object_function *)_wrap_figure_get_obj_dims, (attr_set_object_function *)int_set_object_failed },
  { "viewport_dims", (attr_get_function *)_wrap_figure_get_viewport_dims, (attr_set_function *)_wrap_figure_set_viewport_dims,(attr_get_object_function *)_wrap_figure_get_obj_viewport_dims, (attr_set_object_function *)int_set_object_failed },
  { "wresize", (attr_get_function *)_wrap_figure_get_wresize, (attr_set_function *)_wrap_figure_set_wresize,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "position", (attr_get_function *)_wrap_figure_get_position, (attr_set_function *)_wrap_figure_set_position,(attr_get_object_function *)_wrap_figure_get_obj_position, (attr_set_object_function *)int_set_object_failed },
  { "children", (attr_get_function *)_wrap_figure_get_children, (attr_set_function *)_wrap_figure_set_children,(attr_get_object_function *)_wrap_figure_get_obj_children, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Figure_func[]={
  { "figure_create", int_figure_create},
  { NULL, NULL}
};

/* call ith function in the Figure interface */

int Figure_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Figure_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Figure_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Figure_func[i].name;
  *f = Figure_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Figure_register_classes(NspObject *d)
{

#line 14 "figure.override"

Init portion 


#line 826 "figure.c"
  nspgobject_register_class(d, "Figure", Figure, &NspFigure_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 42 "figure.override"


/* draw the axes contained in the Figure 
 *
 */

static void nsp_draw_figure(BCG *Xgc,NspGraphic *Obj)
{
  Cell *cloc;
  NspList *L;
  NspFigure *F = (NspFigure *) Obj;
  /* draw elements */
  L = F->obj->children;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G);
	}
      cloc = cloc->next;
    }
}

/* create a graphic window (in fact a Gtk one)
 * an set this window as the current one 
 * then add figure to the Xgc associated to the 
 * window 
 */

#ifdef WITH_GTKGLEXT 
extern Gengine GL_gengine; 
#endif 

#ifdef WITH_CAIRO
extern Gengine Cairo_gengine; 
#endif 

extern Gengine XFig_gengine, Pos_gengine, Gtk_gengine; 


static int nsp_figure_connect(NspFigure *F)
{
  driver_initgraphic *initg = Gtk_gengine.initgraphic;
  int v1=-1, wdim[2], wpdim[2],  wpos[2];
  BCG *Xgc;

  Xgc = window_list_search(F->obj->id);
  if ( Xgc != NULL) 
    {
      Sciprintf("Error: Figure is already connected\n");
      return FAIL;
    }
  
  if ( F->obj->dims != NULL && F->obj->dims->mn == 2 )
    { 
      wdim[0] = F->obj->dims->R[0];
      wdim[1] = F->obj->dims->R[1];
    }

  if ( F->obj->viewport_dims != NULL &&  F->obj->viewport_dims->mn == 2 ) 
    { 
      wpdim[0] = F->obj->viewport_dims->R[0];
      wpdim[1] = F->obj->viewport_dims->R[1];
    }

  /* A FAIRE c'est les offset du viewport */
  /* 
     if (viewport != NULL && viewport->mn != 2 ) 
     {
     viewport[0]= F->obj->viewport_pos->R[0];
     viewport[1]= F->obj->viewport_pos->R[1];
     }
  */
  if ( F->obj->position != NULL && F->obj->position->mn == 2 )
    { 
      wpos[0] = F->obj->position->R[0];
      wpos[1] = F->obj->position->R[1];
    }
  
  if ( strcmp(F->obj->driver,"Gtk") == 0) initg = Gtk_gengine.initgraphic;
  else if ( strcmp(F->obj->driver,"OpenGl") == 0) 
    {
#ifdef WITH_GTKGLEXT 
      initg = GL_gengine.initgraphic;
#else 
      Sciprintf("No opengl support in this version\n");
#endif 
    }
  else if ( strcmp(F->obj->driver,"Cairo") == 0) 
    {
#ifdef WITH_CAIRO 
      initg = Cairo_gengine.initgraphic;
#else 
      Sciprintf("No cairo support in this version\n");
#endif
    }
  else 
    initg = Gtk_gengine.initgraphic;
  v1 = F->obj->id;
  initg("",&v1, 
	(F->obj->dims != NULL  && F->obj->dims->mn == 2 ) ? wdim :NULL, 
	(F->obj->viewport_dims  != NULL &&  F->obj->viewport_dims->mn == 2) ? wpdim : NULL , 
	NULL, 
	( F->obj->position != NULL && F->obj->position->mn == 2 ) ? wpos: NULL , 
	'e');
  /* check ! */
  Xgc = window_list_search(F->obj->id);
  if ( Xgc == NULL) 
    {
      Sciprintf("failed to connect figure\n");
      return FAIL;
    }
  if ( wpdim != NULL )  
    {
      Xgc->graphic_engine->scale->xset_wresize(Xgc,0);
    }
  if ( F->obj->fname != NULL && strcmp(F->obj->fname,"") != 0 )
    Xgc->graphic_engine->setpopupname(Xgc,F->obj->fname);
  store_graphic_object(Xgc,NSP_OBJECT(F));
  return OK;
}



/* delete window associated to F 
 */

static int nsp_figure_unconnect(NspFigure *F)
{
  scig_delete(F->obj->id); 
  return OK ;
}

#line 967 "figure.c"
