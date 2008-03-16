/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "figure.override"

#include "nsp/compound.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_figure(BCG *Xgc,NspGraphic *Obj);

#line 19 "figure.c"

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

#line 17 "figure.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_figure;

#line 94 "figure.c"
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
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  if ( strcmp(A->obj->fname,loc->obj->fname) != 0) return FALSE;
  if ( A->obj->wresize != loc->obj->wresize) return FALSE;
  if ( A->obj->id != loc->obj->id) return FALSE;
  if ( A->obj->width != loc->obj->width) return FALSE;
  if ( A->obj->height != loc->obj->height) return FALSE;
  if ( A->obj->gr_width != loc->obj->gr_width) return FALSE;
  if ( A->obj->gr_height != loc->obj->gr_height) return FALSE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
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
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->fname) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->wresize) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->id) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->width) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->height) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->gr_width) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->gr_height) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->y) == FAIL) return FAIL;
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
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->fname)) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->wresize) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->id) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->width) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->height) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->gr_width) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->gr_height) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->y) == FAIL) return NULL;
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
  nsp_list_destroy(H->obj->children);
  nsp_string_destroy(&(H->obj->fname));
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

void nsp_figure_info(NspFigure *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLFIGURE) 
    {
      Sciprintf("Null Pointer Figure \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_figure_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_figure_print(NspFigure *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLFIGURE) 
    {
      Sciprintf("Null Pointer Figure \n");
      return;
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
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_figure_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        if ( M->obj->children != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1);
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"wresize=%d\n",M->obj->wresize);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  Sciprintf1(indent+2,"width=%d\n",M->obj->width);
  Sciprintf1(indent+2,"height=%d\n",M->obj->height);
  Sciprintf1(indent+2,"gr_width=%d\n",M->obj->gr_width);
  Sciprintf1(indent+2,"gr_height=%d\n",M->obj->gr_height);
  Sciprintf1(indent+2,"x=%d\n",M->obj->x);
  Sciprintf1(indent+2,"y=%d\n",M->obj->y);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_figure_latex(NspFigure *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_figure_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    if ( M->obj->children != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1);
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"wresize=%d\n",M->obj->wresize);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  Sciprintf1(indent+2,"width=%d\n",M->obj->width);
  Sciprintf1(indent+2,"height=%d\n",M->obj->height);
  Sciprintf1(indent+2,"gr_width=%d\n",M->obj->gr_width);
  Sciprintf1(indent+2,"gr_height=%d\n",M->obj->gr_height);
  Sciprintf1(indent+2,"x=%d\n",M->obj->x);
  Sciprintf1(indent+2,"y=%d\n",M->obj->y);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
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
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  if ( H->obj->fname == NULL) 
    {
     if (( H->obj->fname = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspFigure *nsp_figure_create(char *name,NspList* children,char* fname,gboolean wresize,int id,int width,int height,int gr_width,int gr_height,int x,int y,NspTypeBase *type)
{
 NspFigure *H  = nsp_figure_create_void(name,type);
 if ( H ==  NULLFIGURE) return NULLFIGURE;
  if ( nsp_figure_create_partial(H) == FAIL) return NULLFIGURE;
  if ( children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *)  nsp_object_copy_and_name("children",NSP_OBJECT(children))) == NULLLIST) return NULL;
    }
  if ((H->obj->fname = nsp_string_copy(fname)) == NULL) return NULL;
  H->obj->wresize=wresize;
  H->obj->id=id;
  H->obj->width=width;
  H->obj->height=height;
  H->obj->gr_width=gr_width;
  H->obj->gr_height=gr_height;
  H->obj->x=x;
  H->obj->y=y;
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

static NspMethods *figure_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_figure_get_children(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspFigure *) self)->obj->children);
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
  ((NspFigure *) self)->obj->children = children;
  return OK;
}

static NspObject *_wrap_figure_get_fname(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((char*) ((NspFigure *) self)->obj->fname);
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_figure_set_fname(void *self, char *attr, NspObject *O)
{
  char *fname;

  if ((fname = nsp_string_object(O))==NULL) return FAIL;
  if ((fname = nsp_string_copy(fname)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspFigure *) self)->obj->fname);
  ((NspFigure *) self)->obj->fname = fname;
  return OK;
}

static NspObject *_wrap_figure_get_wresize(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((gboolean) ((NspFigure *) self)->obj->wresize);
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_figure_set_wresize(void *self, char *attr, NspObject *O)
{
  int wresize;

  if ( BoolScalar(O,&wresize) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->wresize = wresize;
  return OK;
}

static NspObject *_wrap_figure_get_id(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->id);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_id(void *self, char *attr, NspObject *O)
{
  int id;

  if ( IntScalar(O,&id) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->id = id;
  return OK;
}

static NspObject *_wrap_figure_get_width(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->width);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_width(void *self, char *attr, NspObject *O)
{
  int width;

  if ( IntScalar(O,&width) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->width = width;
  return OK;
}

static NspObject *_wrap_figure_get_height(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->height);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_height(void *self, char *attr, NspObject *O)
{
  int height;

  if ( IntScalar(O,&height) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->height = height;
  return OK;
}

static NspObject *_wrap_figure_get_gr_width(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->gr_width);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_gr_width(void *self, char *attr, NspObject *O)
{
  int gr_width;

  if ( IntScalar(O,&gr_width) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->gr_width = gr_width;
  return OK;
}

static NspObject *_wrap_figure_get_gr_height(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->gr_height);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_gr_height(void *self, char *attr, NspObject *O)
{
  int gr_height;

  if ( IntScalar(O,&gr_height) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->gr_height = gr_height;
  return OK;
}

static NspObject *_wrap_figure_get_x(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->x);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_x(void *self, char *attr, NspObject *O)
{
  int x;

  if ( IntScalar(O,&x) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->x = x;
  return OK;
}

static NspObject *_wrap_figure_get_y(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspFigure *) self)->obj->y);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_figure_set_y(void *self, char *attr, NspObject *O)
{
  int y;

  if ( IntScalar(O,&y) == FAIL) return FAIL;
  ((NspFigure *) self)->obj->y = y;
  return OK;
}

static AttrTab figure_attrs[] = {
  { "children", (attr_get_function *)_wrap_figure_get_children, (attr_set_function *)_wrap_figure_set_children,(attr_get_object_function *)_wrap_figure_get_obj_children, (attr_set_object_function *)int_set_object_failed },
  { "fname", (attr_get_function *)_wrap_figure_get_fname, (attr_set_function *)_wrap_figure_set_fname,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "wresize", (attr_get_function *)_wrap_figure_get_wresize, (attr_set_function *)_wrap_figure_set_wresize,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "id", (attr_get_function *)_wrap_figure_get_id, (attr_set_function *)_wrap_figure_set_id,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "width", (attr_get_function *)_wrap_figure_get_width, (attr_set_function *)_wrap_figure_set_width,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "height", (attr_get_function *)_wrap_figure_get_height, (attr_set_function *)_wrap_figure_set_height,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "gr_width", (attr_get_function *)_wrap_figure_get_gr_width, (attr_set_function *)_wrap_figure_set_gr_width,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "gr_height", (attr_get_function *)_wrap_figure_get_gr_height, (attr_set_function *)_wrap_figure_set_gr_height,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "x", (attr_get_function *)_wrap_figure_get_x, (attr_set_function *)_wrap_figure_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_figure_get_y, (attr_set_function *)_wrap_figure_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 26 "figure.override"
int _wrap_figure_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  /* here we should get the Xgc with the same id as figure XXXX */
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 744 "figure.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab figure_func[]={
  {"figure_attach", _wrap_figure_attach},
  { "figure_create", int_figure_create},
  { NULL, NULL}
};

/* call ith function in the figure interface */

int figure_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(figure_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void figure_Interf_Info(int i, char **fname, function (**f))
{
  *fname = figure_func[i].name;
  *f = figure_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
figure_register_classes(NspObject *d)
{

#line 12 "figure.override"

Init portion 


#line 783 "figure.c"
  nspgobject_register_class(d, "Figure", Figure, &NspFigure_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 40 "figure.override"


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

#line 814 "figure.c"
