/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/string3d.override"
#include "nsp/string3d.h"
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_string3d(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_string3d(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_string3d(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_string3d(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_string3d(BCG *Xgc,NspGraphic *o,double *bounds);

extern void nsp_figure_force_redraw( NspFigure *F);

#line 25 "string3d.c"

/* ----------- String3d ----------- */


#define  String3d_Private 
#include "nsp/object.h"
#include "nsp/string3d.h"
#include "nsp/interf.h"

/* 
 * NspString3d inherits from NspGraphic 
 */

int nsp_type_string3d_id=0;
NspTypeString3d *nsp_type_string3d=NULL;

/*
 * Type object for String3d 
 * all the instance of NspTypeString3d share the same id. 
 * nsp_type_string3d: is an instance of NspTypeString3d 
 *    used for objects of NspString3d type (i.e built with new_string3d) 
 * other instances are used for derived classes 
 */
NspTypeString3d *new_type_string3d(type_mode mode)
{
  NspTypeString3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_string3d != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_string3d;
    }
  if ((type =  malloc(sizeof(NspTypeString3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = string3d_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = string3d_get_methods; 
  type->new = (new_func *) new_string3d;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for string3d */ 

  top->pr = (print_func *) nsp_string3d_print;                  
  top->dealloc = (dealloc_func *) nsp_string3d_destroy;
  top->copy  =  (copy_func *) nsp_string3d_copy;                 
  top->size  = (size_func *) nsp_string3d_size;                
  top->s_type =  (s_type_func *) nsp_string3d_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_string3d_type_short_string;
  top->info = (info_func *) nsp_string3d_info ;                  
  /* top->is_true = (is_true_func  *) nsp_string3d_is_true; */
  /* top->loop =(loop_func *) nsp_string3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_string3d_object;
  top->eq  = (eq_func *) nsp_string3d_eq;
  top->neq  = (eq_func *) nsp_string3d_neq;
  top->save  = (save_func *) nsp_string3d_xdr_save;
  top->load  = (load_func *) nsp_string3d_xdr_load;
  top->create = (create_func*) int_string3d_create;
  top->latex = (print_func *) nsp_string3d_latex;
  
  /* specific methods for string3d */
      
  type->init = (init_func *) init_string3d;

#line 23 "codegen/string3d.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_string3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_string3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_string3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_string3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_string3d  ;
  ((NspTypeGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_string3d_full_copy ;
  /* next method are defined in NspGraphic and need not be chnaged here for String3d */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 110 "string3d.c"
  /* 
   * String3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_string3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeString3d called nsp_type_string3d
       */
      type->id =  nsp_type_string3d_id = nsp_new_type_id();
      nsp_type_string3d = type;
      if ( nsp_register_type(nsp_type_string3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_string3d(mode);
    }
  else 
    {
       type->id = nsp_type_string3d_id;
       return type;
    }
}

/*
 * initialize String3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_string3d(NspString3d *Obj,NspTypeString3d *type)
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
 * new instance of String3d 
 */

NspString3d *new_string3d() 
{
  NspString3d *loc; 
  /* type must exists */
  nsp_type_string3d = new_type_string3d(T_BASE);
  if ( (loc = malloc(sizeof(NspString3d)))== NULLSTRING3D) return loc;
  /* initialize object */
  if ( init_string3d(loc,nsp_type_string3d) == FAIL) return NULLSTRING3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for String3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_string3d_size(NspString3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char string3d_type_name[]="String3d";
static char string3d_short_type_name[]="string3d";

static char *nsp_string3d_type_as_string(void)
{
  return(string3d_type_name);
}

static char *nsp_string3d_type_short_string(NspObject *v)
{
  return(string3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_string3d_eq(NspString3d *A, NspObject *B)
{
  NspString3d *loc = (NspString3d *) B;
  if ( check_cast(B,nsp_type_string3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->z)->type->eq(A->obj->z,loc->obj->z) == FALSE ) return FALSE;
  if ( A->obj->mesh != loc->obj->mesh) return FALSE;
  if ( A->obj->mesh_color != loc->obj->mesh_color) return FALSE;
  if ( A->obj->face_color != loc->obj->face_color) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_string3d_neq(NspString3d *A, NspObject *B)
{
  return ( nsp_string3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_string3d_xdr_save(XDR *xdrs, NspString3d *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->z)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mesh) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mesh_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->face_color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspString3d  *nsp_string3d_xdr_load_partial(XDR *xdrs, NspString3d *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_string3d))) == NULL) return NULL;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->z =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mesh) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mesh_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->face_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspString3d  *nsp_string3d_xdr_load(XDR *xdrs)
{
  NspString3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSTRING3D;
  if ((H  = nsp_string3d_create_void(name,(NspTypeBase *) nsp_type_string3d))== NULLSTRING3D) return H;
  if ((H  = nsp_string3d_xdr_load_partial(xdrs,H))== NULLSTRING3D) return H;
#line 268 "string3d.c"
  return H;
}

/*
 * delete 
 */

void nsp_string3d_destroy_partial(NspString3d *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 282 "string3d.c"
    nsp_matrix_destroy(H->obj->x);
    nsp_matrix_destroy(H->obj->y);
    nsp_matrix_destroy(H->obj->z);
    FREE(H->obj);
   }
}

void nsp_string3d_destroy(NspString3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_string3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_string3d_info(NspString3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSTRING3D) 
    {
      Sciprintf("Null Pointer String3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_string3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_string3d_print(NspString3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSTRING3D) 
    {
      Sciprintf("Null Pointer String3d \n");
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
          nsp_string3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_string3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->z != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"mesh_color=%d\n",M->obj->mesh_color);
  Sciprintf1(indent+2,"face_color=%d\n",M->obj->face_color);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_string3d_latex(NspString3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_string3d_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->z != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"mesh_color=%d\n",M->obj->mesh_color);
  Sciprintf1(indent+2,"face_color=%d\n",M->obj->face_color);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for String3d objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspString3d   *nsp_string3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_string3d_id) == TRUE ) return ((NspString3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_string3d));
  return NULL;
}

int IsString3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_string3d_id);
}

int IsString3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_string3d_id);
}

NspString3d  *GetString3dCopy(Stack stack, int i)
{
  if (  GetString3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspString3d  *GetString3d(Stack stack, int i)
{
  NspString3d *M;
  if (( M = nsp_string3d_object(NthObj(i))) == NULLSTRING3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspString3d *nsp_string3d_create_void(char *name,NspTypeBase *type)
{
 NspString3d *H  = (type == NULL) ? new_string3d() : type->new();
 if ( H ==  NULLSTRING3D)
  {
   Sciprintf("No more memory\n");
   return NULLSTRING3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSTRING3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_string3d_create_partial(NspString3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_string3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->z = NULLMAT;
  H->obj->mesh = TRUE;
  H->obj->mesh_color = -1;
  H->obj->face_color = -1;
  return OK;
}

int nsp_string3d_check_values(NspString3d *H)
{
  if ( H->obj->x == NULLMAT) 
    {
       if (( H->obj->x = nsp_matrix_create("x",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->y == NULLMAT) 
    {
       if (( H->obj->y = nsp_matrix_create("y",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->z == NULLMAT) 
    {
       if (( H->obj->z = nsp_matrix_create("z",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspString3d *nsp_string3d_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,gboolean mesh,int mesh_color,int face_color,NspTypeBase *type)
{
 NspString3d *H  = nsp_string3d_create_void(name,type);
 if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_string3d_create_partial(H) == FAIL) return NULLSTRING3D;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->z= z;
  H->obj->mesh=mesh;
  H->obj->mesh_color=mesh_color;
  H->obj->face_color=face_color;
 if ( nsp_string3d_check_values(H) == FAIL) return NULLSTRING3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspString3d *nsp_string3d_copy_partial(NspString3d *H,NspString3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspString3d *nsp_string3d_copy(NspString3d *self)
{
  NspString3d *H  =nsp_string3d_create_void(NVOID,(NspTypeBase *) nsp_type_string3d);
  if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSTRING3D;
  if ( nsp_string3d_copy_partial(H,self)== NULL) return NULLSTRING3D;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspString3d *nsp_string3d_full_copy_partial(NspString3d *H,NspString3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_string3d))) == NULL) return NULLSTRING3D;
  H->obj->ref_count=1;
  if ( self->obj->x == NULL )
    { H->obj->x = NULL;}
  else
    {
      if ((H->obj->x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(self->obj->x))) == NULLMAT) return NULL;
    }
  if ( self->obj->y == NULL )
    { H->obj->y = NULL;}
  else
    {
      if ((H->obj->y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(self->obj->y))) == NULLMAT) return NULL;
    }
  if ( self->obj->z == NULL )
    { H->obj->z = NULL;}
  else
    {
      if ((H->obj->z = (NspMatrix *) nsp_object_copy_and_name("z",NSP_OBJECT(self->obj->z))) == NULLMAT) return NULL;
    }
  H->obj->mesh=self->obj->mesh;
  H->obj->mesh_color=self->obj->mesh_color;
  H->obj->face_color=self->obj->face_color;
  return H;
}

NspString3d *nsp_string3d_full_copy(NspString3d *self)
{
  NspString3d *H  =nsp_string3d_create_void(NVOID,(NspTypeBase *) nsp_type_string3d);
  if ( H ==  NULLSTRING3D) return NULLSTRING3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSTRING3D;
  if ( nsp_string3d_full_copy_partial(H,self)== NULL) return NULLSTRING3D;
#line 555 "string3d.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the String3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_string3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspString3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type string3d is initialized */
  nsp_type_string3d = new_type_string3d(T_BASE);
  if(( H = nsp_string3d_create_void(NVOID,(NspTypeBase *) nsp_type_string3d)) == NULLSTRING3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_string3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_string3d_check_values(H) == FAIL) return RET_BUG;
#line 575 "string3d.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_string3d_full_copy(NspString3d *self,Stack stack,int rhs,int opt,int lhs)
{
  NspString3d *ret;

  ret = nsp_string3d_full_copy(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods string3d_methods[] = {
  {"full_copy",(nsp_method *) _wrap_nsp_string3d_full_copy},
  { NULL, NULL}
};

static NspMethods *string3d_get_methods(void) { return string3d_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_string3d_get_x(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspString3d *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_string3d_get_obj_x(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspString3d *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_string3d_set_x(void *self, char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspString3d *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspString3d *) self)->obj->x);
  ((NspString3d *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_string3d_get_y(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspString3d *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_string3d_get_obj_y(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspString3d *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_string3d_set_y(void *self, char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspString3d *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspString3d *) self)->obj->y);
  ((NspString3d *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_string3d_get_z(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspString3d *) self)->obj->z;
  return (NspObject *) ret;
}

static NspObject *_wrap_string3d_get_obj_z(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspString3d *) self)->obj->z);
  return (NspObject *) ret;
}

static int _wrap_string3d_set_z(void *self, char *attr, NspObject *O)
{
  NspMatrix *z;

  if ( ! IsMat(O) ) return FAIL;
  if ((z = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspString3d *) self)->obj->z != NULL ) 
    nsp_matrix_destroy(((NspString3d *) self)->obj->z);
  ((NspString3d *) self)->obj->z= z;
  return OK;
}

static NspObject *_wrap_string3d_get_mesh(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspString3d *) self)->obj->mesh;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_string3d_set_mesh(void *self, char *attr, NspObject *O)
{
  int mesh;

  if ( BoolScalar(O,&mesh) == FAIL) return FAIL;
  ((NspString3d *) self)->obj->mesh= mesh;
  return OK;
}

static NspObject *_wrap_string3d_get_mesh_color(void *self,char *attr)
{
  int ret;

  ret = ((NspString3d *) self)->obj->mesh_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_string3d_set_mesh_color(void *self, char *attr, NspObject *O)
{
  int mesh_color;

  if ( IntScalar(O,&mesh_color) == FAIL) return FAIL;
  ((NspString3d *) self)->obj->mesh_color= mesh_color;
  return OK;
}

static NspObject *_wrap_string3d_get_face_color(void *self,char *attr)
{
  int ret;

  ret = ((NspString3d *) self)->obj->face_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_string3d_set_face_color(void *self, char *attr, NspObject *O)
{
  int face_color;

  if ( IntScalar(O,&face_color) == FAIL) return FAIL;
  ((NspString3d *) self)->obj->face_color= face_color;
  return OK;
}

static AttrTab string3d_attrs[] = {
  { "x", (attr_get_function *)_wrap_string3d_get_x, (attr_set_function *)_wrap_string3d_set_x,(attr_get_object_function *)_wrap_string3d_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_string3d_get_y, (attr_set_function *)_wrap_string3d_set_y,(attr_get_object_function *)_wrap_string3d_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "z", (attr_get_function *)_wrap_string3d_get_z, (attr_set_function *)_wrap_string3d_set_z,(attr_get_object_function *)_wrap_string3d_get_obj_z, (attr_set_object_function *)int_set_object_failed },
  { "mesh", (attr_get_function *)_wrap_string3d_get_mesh, (attr_set_function *)_wrap_string3d_set_mesh,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mesh_color", (attr_get_function *)_wrap_string3d_get_mesh_color, (attr_set_function *)_wrap_string3d_set_mesh_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "face_color", (attr_get_function *)_wrap_string3d_get_face_color, (attr_set_function *)_wrap_string3d_set_face_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 46 "codegen/string3d.override"
int _wrap_string3d_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 766 "string3d.c"


#line 89 "codegen/string3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_string3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 778 "string3d.c"


#line 99 "codegen/string3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_string3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 791 "string3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab String3d_func[]={
  {"string3d_attach", _wrap_string3d_attach},
  {"extractelts_string3d", _wrap_nsp_extractelts_string3d},
  {"setrowscols_string3d", _wrap_nsp_setrowscols_string3d},
  { "string3d_create", int_string3d_create},
  { NULL, NULL}
};

/* call ith function in the String3d interface */

int String3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(String3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void String3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = String3d_func[i].name;
  *f = String3d_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
String3d_register_classes(NspObject *d)
{

#line 18 "codegen/string3d.override"

Init portion 


#line 832 "string3d.c"
  nspgobject_register_class(d, "String3d", String3d, &NspString3d_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 110 "codegen/string3d.override"

/* inserted verbatim at the end */

static void nsp_draw_string3d(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int flag[]={1,2,4};
  double bbox[]={0,1,0,1,0,1};
  double teta = 35, alpha=45;
  NspString3d *P =(NspString3d*) Obj ;
  if ( Obj->obj->hidden == TRUE ) return ;
  /* be sure that object are in canonical form */
  Mat2double(P->obj->x);
  Mat2double(P->obj->y);
  Mat2double(P->obj->z);
  nsp_plot3d_1(Xgc,P->obj->x->R,P->obj->y->R,P->obj->z->R,&P->obj->z->m,&P->obj->z->n,
	       &teta,&alpha,"X@Y@Z",flag,bbox);
}

static void nsp_translate_string3d(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_string3d(BCG *Xgc,NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_string3d(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of string3d 
 *
 */

static void nsp_getbounds_string3d(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  bounds[0]= bounds[1] = bounds[2]= bounds[3]=0;
  return;
}


#line 883 "string3d.c"
