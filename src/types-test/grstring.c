/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/grstring.override"
#include "nsp/grstring.h"
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_grstring(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_grstring(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_grstring(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_grstring(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_grstring(BCG *Xgc,NspGraphic *o,double *bounds);

extern void nsp_figure_force_redraw( NspFigure *F);

#line 25 "grstring.c"

/* ----------- Grstring ----------- */


#define  Grstring_Private 
#include "nsp/object.h"
#include "nsp/grstring.h"
#include "nsp/interf.h"

/* 
 * NspGrstring inherits from NspGraphic 
 */

int nsp_type_grstring_id=0;
NspTypeGrstring *nsp_type_grstring=NULL;

/*
 * Type object for Grstring 
 * all the instance of NspTypeGrstring share the same id. 
 * nsp_type_grstring: is an instance of NspTypeGrstring 
 *    used for objects of NspGrstring type (i.e built with new_grstring) 
 * other instances are used for derived classes 
 */
NspTypeGrstring *new_type_grstring(type_mode mode)
{
  NspTypeGrstring *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_grstring != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grstring;
    }
  if ((type =  malloc(sizeof(NspTypeGrstring))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = grstring_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = grstring_get_methods; 
  type->new = (new_func *) new_grstring;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for grstring */ 

  top->pr = (print_func *) nsp_grstring_print;                  
  top->dealloc = (dealloc_func *) nsp_grstring_destroy;
  top->copy  =  (copy_func *) nsp_grstring_copy;                 
  top->size  = (size_func *) nsp_grstring_size;                
  top->s_type =  (s_type_func *) nsp_grstring_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_grstring_type_short_string;
  top->info = (info_func *) nsp_grstring_info ;                  
  /* top->is_true = (is_true_func  *) nsp_grstring_is_true; */
  /* top->loop =(loop_func *) nsp_grstring_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_grstring_object;
  top->eq  = (eq_func *) nsp_grstring_eq;
  top->neq  = (eq_func *) nsp_grstring_neq;
  top->save  = (save_func *) nsp_grstring_xdr_save;
  top->load  = (load_func *) nsp_grstring_xdr_load;
  top->create = (create_func*) int_grstring_create;
  top->latex = (print_func *) nsp_grstring_latex;
  
  /* specific methods for grstring */
      
  type->init = (init_func *) init_grstring;

#line 23 "codegen/grstring.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grstring;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grstring ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grstring  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grstring  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grstring  ;
  ((NspTypeGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_grstring_full_copy ;
  /* next method are defined in NspGraphic and need not be chnaged here for Grstring */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 110 "grstring.c"
  /* 
   * Grstring interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_grstring_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGrstring called nsp_type_grstring
       */
      type->id =  nsp_type_grstring_id = nsp_new_type_id();
      nsp_type_grstring = type;
      if ( nsp_register_type(nsp_type_grstring) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grstring(mode);
    }
  else 
    {
       type->id = nsp_type_grstring_id;
       return type;
    }
}

/*
 * initialize Grstring instances 
 * locally and by calling initializer on parent class 
 */

static int init_grstring(NspGrstring *Obj,NspTypeGrstring *type)
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
 * new instance of Grstring 
 */

NspGrstring *new_grstring() 
{
  NspGrstring *loc; 
  /* type must exists */
  nsp_type_grstring = new_type_grstring(T_BASE);
  if ( (loc = malloc(sizeof(NspGrstring)))== NULLGRSTRING) return loc;
  /* initialize object */
  if ( init_grstring(loc,nsp_type_grstring) == FAIL) return NULLGRSTRING;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Grstring 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_grstring_size(NspGrstring *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char grstring_type_name[]="Grstring";
static char grstring_short_type_name[]="grstring";

static char *nsp_grstring_type_as_string(void)
{
  return(grstring_type_name);
}

static char *nsp_grstring_type_short_string(NspObject *v)
{
  return(grstring_short_type_name);
}

/*
 * A == B 
 */

static int nsp_grstring_eq(NspGrstring *A, NspObject *B)
{
  NspGrstring *loc = (NspGrstring *) B;
  if ( check_cast(B,nsp_type_grstring_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
  if ( strcmp(A->obj->font,loc->obj->font) != 0) return FALSE;
  if ( strcmp(A->obj->text,loc->obj->text) != 0) return FALSE;
  if ( A->obj->position != loc->obj->position) return FALSE;
  if ( A->obj->angle != loc->obj->angle) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_grstring_neq(NspGrstring *A, NspObject *B)
{
  return ( nsp_grstring_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_grstring_xdr_save(XDR *xdrs, NspGrstring *M)
{
  if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */
   if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->y) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->font) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->text) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->position) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->angle) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrstring  *nsp_grstring_xdr_load_partial(XDR *xdrs, NspGrstring *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_grstring))) == NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->font)) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->text)) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->position) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->angle) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspGrstring  *nsp_grstring_xdr_load(XDR *xdrs)
{
  NspGrstring *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRSTRING;
  if ((H  = nsp_grstring_create_void(name,(NspTypeBase *) nsp_type_grstring))== NULLGRSTRING) return H;
  if ((H  = nsp_grstring_xdr_load_partial(xdrs,H))== NULLGRSTRING) return H;
#line 269 "grstring.c"
  return H;
}

/*
 * delete 
 */

void nsp_grstring_destroy_partial(NspGrstring *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 283 "grstring.c"
  nsp_string_destroy(&(H->obj->font));
  nsp_string_destroy(&(H->obj->text));
    FREE(H->obj);
   }
}

void nsp_grstring_destroy(NspGrstring *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_grstring_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_grstring_info(NspGrstring *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRSTRING) 
    {
      Sciprintf("Null Pointer Grstring \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_grstring_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_grstring_print(NspGrstring *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRSTRING) 
    {
      Sciprintf("Null Pointer Grstring \n");
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
          nsp_grstring_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grstring_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%d\n",M->obj->x);
  Sciprintf1(indent+2,"y=%d\n",M->obj->y);
  Sciprintf1(indent+2,"font=%s\n",M->obj->font);
  Sciprintf1(indent+2,"text=%s\n",M->obj->text);
  Sciprintf1(indent+2,"position=%d\n",M->obj->position);
  Sciprintf1(indent+2,"angle=%f\n",M->obj->angle);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_grstring_latex(NspGrstring *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grstring_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%d\n",M->obj->x);
  Sciprintf1(indent+2,"y=%d\n",M->obj->y);
  Sciprintf1(indent+2,"font=%s\n",M->obj->font);
  Sciprintf1(indent+2,"text=%s\n",M->obj->text);
  Sciprintf1(indent+2,"position=%d\n",M->obj->position);
  Sciprintf1(indent+2,"angle=%f\n",M->obj->angle);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Grstring objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspGrstring   *nsp_grstring_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grstring_id) == TRUE ) return ((NspGrstring *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_grstring));
  return NULL;
}

int IsGrstringObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_grstring_id);
}

int IsGrstring(NspObject *O)
{
  return nsp_object_type(O,nsp_type_grstring_id);
}

NspGrstring  *GetGrstringCopy(Stack stack, int i)
{
  if (  GetGrstring(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGrstring  *GetGrstring(Stack stack, int i)
{
  NspGrstring *M;
  if (( M = nsp_grstring_object(NthObj(i))) == NULLGRSTRING)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspGrstring *nsp_grstring_create_void(char *name,NspTypeBase *type)
{
 NspGrstring *H  = (type == NULL) ? new_grstring() : type->new();
 if ( H ==  NULLGRSTRING)
  {
   Sciprintf("No more memory\n");
   return NULLGRSTRING;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRSTRING;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_grstring_create_partial(NspGrstring *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grstring)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0;
  H->obj->y = 0;
  H->obj->font = NULL;
  H->obj->text = NULL;
  H->obj->position = 0;
  H->obj->angle = 0.0;
  return OK;
}

int nsp_grstring_check_values(NspGrstring *H)
{
  if ( H->obj->font == NULL) 
    {
     if (( H->obj->font = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->text == NULL) 
    {
     if (( H->obj->text = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGrstring *nsp_grstring_create(char *name,int x,int y,char* font,char* text,int position,double angle,NspTypeBase *type)
{
 NspGrstring *H  = nsp_grstring_create_void(name,type);
 if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_grstring_create_partial(H) == FAIL) return NULLGRSTRING;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->font = font;
  H->obj->text = text;
  H->obj->position=position;
  H->obj->angle=angle;
 if ( nsp_grstring_check_values(H) == FAIL) return NULLGRSTRING;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGrstring *nsp_grstring_copy_partial(NspGrstring *H,NspGrstring *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrstring *nsp_grstring_copy(NspGrstring *self)
{
  NspGrstring *H  =nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRSTRING;
  if ( nsp_grstring_copy_partial(H,self)== NULL) return NULLGRSTRING;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspGrstring *nsp_grstring_full_copy_partial(NspGrstring *H,NspGrstring *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_grstring))) == NULL) return NULLGRSTRING;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  if ((H->obj->font = nsp_string_copy(self->obj->font)) == NULL) return NULL;
  if ((H->obj->text = nsp_string_copy(self->obj->text)) == NULL) return NULL;
  H->obj->position=self->obj->position;
  H->obj->angle=self->obj->angle;
  return H;
}

NspGrstring *nsp_grstring_full_copy(NspGrstring *self)
{
  NspGrstring *H  =nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring);
  if ( H ==  NULLGRSTRING) return NULLGRSTRING;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRSTRING;
  if ( nsp_grstring_full_copy_partial(H,self)== NULL) return NULLGRSTRING;
#line 520 "grstring.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Grstring
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_grstring_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGrstring *H;
  CheckStdRhs(0,0);
  /* want to be sure that type grstring is initialized */
  nsp_type_grstring = new_type_grstring(T_BASE);
  if(( H = nsp_grstring_create_void(NVOID,(NspTypeBase *) nsp_type_grstring)) == NULLGRSTRING) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_grstring_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_grstring_check_values(H) == FAIL) return RET_BUG;
#line 540 "grstring.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_grstring_full_copy(NspGrstring *self,Stack stack,int rhs,int opt,int lhs)
{
  NspGrstring *ret;

  ret = nsp_grstring_full_copy(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods grstring_methods[] = {
  {"full_copy",(nsp_method *) _wrap_nsp_grstring_full_copy},
  { NULL, NULL}
};

static NspMethods *grstring_get_methods(void) { return grstring_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_grstring_get_x(void *self,char *attr)
{
  int ret;

  ret = ((NspGrstring *) self)->obj->x;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_x(void *self, char *attr, NspObject *O)
{
  int x;

  if ( IntScalar(O,&x) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_grstring_get_y(void *self,char *attr)
{
  int ret;

  ret = ((NspGrstring *) self)->obj->y;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_y(void *self, char *attr, NspObject *O)
{
  int y;

  if ( IntScalar(O,&y) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_grstring_get_font(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->font;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_grstring_set_font(void *self, char *attr, NspObject *O)
{
  char *font;

  if ((font = nsp_string_object(O))==NULL) return FAIL;
  if ((font = nsp_string_copy(font)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspGrstring *) self)->obj->font);
  ((NspGrstring *) self)->obj->font= font;
  return OK;
}

static NspObject *_wrap_grstring_get_text(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->text;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_grstring_set_text(void *self, char *attr, NspObject *O)
{
  char *text;

  if ((text = nsp_string_object(O))==NULL) return FAIL;
  if ((text = nsp_string_copy(text)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspGrstring *) self)->obj->text);
  ((NspGrstring *) self)->obj->text= text;
  return OK;
}

static NspObject *_wrap_grstring_get_position(void *self,char *attr)
{
  int ret;

  ret = ((NspGrstring *) self)->obj->position;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grstring_set_position(void *self, char *attr, NspObject *O)
{
  int position;

  if ( IntScalar(O,&position) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->position= position;
  return OK;
}

static NspObject *_wrap_grstring_get_angle(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrstring *) self)->obj->angle;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grstring_set_angle(void *self, char *attr, NspObject *O)
{
  double angle;

  if ( DoubleScalar(O,&angle) == FAIL) return FAIL;
  ((NspGrstring *) self)->obj->angle= angle;
  return OK;
}

static AttrTab grstring_attrs[] = {
  { "x", (attr_get_function *)_wrap_grstring_get_x, (attr_set_function *)_wrap_grstring_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_grstring_get_y, (attr_set_function *)_wrap_grstring_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "font", (attr_get_function *)_wrap_grstring_get_font, (attr_set_function *)_wrap_grstring_set_font,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "text", (attr_get_function *)_wrap_grstring_get_text, (attr_set_function *)_wrap_grstring_set_text,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "position", (attr_get_function *)_wrap_grstring_get_position, (attr_set_function *)_wrap_grstring_set_position,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "angle", (attr_get_function *)_wrap_grstring_get_angle, (attr_set_function *)_wrap_grstring_set_angle,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 46 "codegen/grstring.override"
int _wrap_grstring_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 703 "grstring.c"


#line 89 "codegen/grstring.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grstring(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 715 "grstring.c"


#line 99 "codegen/grstring.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grstring(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 728 "grstring.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Grstring_func[]={
  {"grstring_attach", _wrap_grstring_attach},
  {"extractelts_grstring", _wrap_nsp_extractelts_grstring},
  {"setrowscols_grstring", _wrap_nsp_setrowscols_grstring},
  { "grstring_create", int_grstring_create},
  { NULL, NULL}
};

/* call ith function in the Grstring interface */

int Grstring_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Grstring_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Grstring_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Grstring_func[i].name;
  *f = Grstring_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Grstring_register_classes(NspObject *d)
{

#line 18 "codegen/grstring.override"

Init portion 


#line 769 "grstring.c"
  nspgobject_register_class(d, "Grstring", Grstring, &NspGrstring_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 110 "codegen/grstring.override"

/* inserted verbatim at the end */

static void nsp_draw_grstring(BCG *Xgc,NspGraphic *Obj, void *data)
{
  NspGrstring *P = (NspGrstring *) Obj;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  Xgc->graphic_engine->scale->displaystring(Xgc,P->obj->text,P->obj->x,P->obj->y,0,P->obj->angle);
}

static void nsp_translate_grstring(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspGrstring *P = (NspGrstring *) Obj;
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_grstring(BCG *Xgc,NspGraphic *Obj,double *R)
{
  NspGrstring *P = (NspGrstring *) Obj;
  double x1;
  x1 = R[0]*(P->obj->x) -R[1]*(P->obj->y);
  P->obj->y = R[1]*(P->obj->x) +R[0]*(P->obj->y);
  P->obj->x = x1;
  /* Il faut aussi changer l'angle */
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_grstring(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  NspGrstring *P = (NspGrstring *) Obj;
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of grstring 
 *
 */

static void nsp_getbounds_grstring(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  /* NspGrstring *P = (NspGrstring *) Obj; */
  bounds[0]= bounds[1] = bounds[2]= bounds[3]=0;
  /*   bounds[0]=*x;/\* xmin *\/ */
  /*   bounds[1]=*y;/\* ymin *\/ */
  /*   bounds[2]=*x;/\* xmax *\/ */
  /*   bounds[3]=*y;/\* ymax *\/ */
}


#line 828 "grstring.c"
