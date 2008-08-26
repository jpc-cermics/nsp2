/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/contour.override"
#include "nsp/axes.h"
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_contour(BCG *Xgc,NspGraphic *Obj);
static void nsp_translate_contour(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_contour(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_contour(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_contour(BCG *Xgc,NspGraphic *o,double *bounds);

extern void nsp_figure_force_redraw( NspFigure *F);

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 29 "contour.c"

/* ----------- Contour ----------- */


#define  Contour_Private 
#include "nsp/object.h"
#include "nsp/contour.h"
#include "nsp/interf.h"

/* 
 * NspContour inherits from NspGraphic 
 */

int nsp_type_contour_id=0;
NspTypeContour *nsp_type_contour=NULL;

/*
 * Type object for Contour 
 * all the instance of NspTypeContour share the same id. 
 * nsp_type_contour: is an instance of NspTypeContour 
 *    used for objects of NspContour type (i.e built with new_contour) 
 * other instances are used for derived classes 
 */
NspTypeContour *new_type_contour(type_mode mode)
{
  NspTypeContour *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_contour != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_contour;
    }
  if ((type =  malloc(sizeof(NspTypeContour))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = contour_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = contour_get_methods; 
  type->new = (new_func *) new_contour;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for contour */ 

  top->pr = (print_func *) nsp_contour_print;                  
  top->dealloc = (dealloc_func *) nsp_contour_destroy;
  top->copy  =  (copy_func *) nsp_contour_copy;                 
  top->size  = (size_func *) nsp_contour_size;                
  top->s_type =  (s_type_func *) nsp_contour_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_contour_type_short_string;
  top->info = (info_func *) nsp_contour_info ;                  
  /* top->is_true = (is_true_func  *) nsp_contour_is_true; */
  /* top->loop =(loop_func *) nsp_contour_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_contour_object;
  top->eq  = (eq_func *) nsp_contour_eq;
  top->neq  = (eq_func *) nsp_contour_neq;
  top->save  = (save_func *) nsp_contour_xdr_save;
  top->load  = (load_func *) nsp_contour_xdr_load;
  top->create = (create_func*) int_contour_create;
  top->latex = (print_func *) nsp_contour_latex;
  
  /* specific methods for contour */
      
  type->init = (init_func *) init_contour;

#line 27 "codegen/contour.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_contour;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_contour ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_contour  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_contour  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_contour  ;
  /* next method are defined in NspGraphic and need not be changed here for Contour */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 111 "contour.c"
  /* 
   * Contour interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_contour_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeContour called nsp_type_contour
       */
      type->id =  nsp_type_contour_id = nsp_new_type_id();
      nsp_type_contour = type;
      if ( nsp_register_type(nsp_type_contour) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_contour(mode);
    }
  else 
    {
       type->id = nsp_type_contour_id;
       return type;
    }
}

/*
 * initialize Contour instances 
 * locally and by calling initializer on parent class 
 */

static int init_contour(NspContour *Obj,NspTypeContour *type)
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
 * new instance of Contour 
 */

NspContour *new_contour() 
{
  NspContour *loc; 
  /* type must exists */
  nsp_type_contour = new_type_contour(T_BASE);
  if ( (loc = malloc(sizeof(NspContour)))== NULLCONTOUR) return loc;
  /* initialize object */
  if ( init_contour(loc,nsp_type_contour) == FAIL) return NULLCONTOUR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Contour 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_contour_size(NspContour *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char contour_type_name[]="Contour";
static char contour_short_type_name[]="contour";

static char *nsp_contour_type_as_string(void)
{
  return(contour_type_name);
}

static char *nsp_contour_type_short_string(NspObject *v)
{
  return(contour_short_type_name);
}

/*
 * A == B 
 */

static int nsp_contour_eq(NspContour *A, NspObject *B)
{
  NspContour *loc = (NspContour *) B;
  if ( check_cast(B,nsp_type_contour_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->z)->type->eq(A->obj->z,loc->obj->z) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->levels)->type->eq(A->obj->levels,loc->obj->levels) == FALSE ) return FALSE;
  if ( A->obj->nlevels != loc->obj->nlevels) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_contour_neq(NspContour *A, NspObject *B)
{
  return ( nsp_contour_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_contour_xdr_save(XDR *xdrs, NspContour *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->z)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->levels)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->nlevels) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspContour  *nsp_contour_xdr_load_partial(XDR *xdrs, NspContour *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_contour))) == NULL) return NULL;
  if ((M->obj->z =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->levels =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->nlevels) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspContour  *nsp_contour_xdr_load(XDR *xdrs)
{
  NspContour *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCONTOUR;
  if ((M  = nsp_contour_create_void(name,(NspTypeBase *) nsp_type_contour))== NULLCONTOUR) return M;
  return nsp_contour_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_contour_destroy_partial(NspContour *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    nsp_matrix_destroy(H->obj->z);
    nsp_matrix_destroy(H->obj->x);
    nsp_matrix_destroy(H->obj->y);
    nsp_matrix_destroy(H->obj->levels);
    FREE(H->obj);
   }
}

void nsp_contour_destroy(NspContour *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
#line 289 "contour.c"
  nsp_contour_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_contour_info(NspContour *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCONTOUR) 
    {
      Sciprintf("Null Pointer Contour \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_contour_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_contour_print(NspContour *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCONTOUR) 
    {
      Sciprintf("Null Pointer Contour \n");
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
          nsp_contour_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_contour_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->z != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->levels != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->levels),indent+2,"levels",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"nlevels=%d\n",M->obj->nlevels);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_contour_latex(NspContour *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_contour_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->z != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->levels != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->levels),indent+2,"levels",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"nlevels=%d\n",M->obj->nlevels);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Contour objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspContour   *nsp_contour_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_contour_id) == TRUE ) return ((NspContour *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_contour));
  return NULL;
}

int IsContourObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_contour_id);
}

int IsContour(NspObject *O)
{
  return nsp_object_type(O,nsp_type_contour_id);
}

NspContour  *GetContourCopy(Stack stack, int i)
{
  if (  GetContour(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspContour  *GetContour(Stack stack, int i)
{
  NspContour *M;
  if (( M = nsp_contour_object(NthObj(i))) == NULLCONTOUR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspContour *nsp_contour_create_void(char *name,NspTypeBase *type)
{
 NspContour *H  = (type == NULL) ? new_contour() : type->new();
 if ( H ==  NULLCONTOUR)
  {
   Sciprintf("No more memory\n");
   return NULLCONTOUR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCONTOUR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_contour_create_partial(NspContour *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_contour)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->z = NULLMAT;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->levels = NULLMAT;
  H->obj->nlevels = 0;
  return OK;
}

int nsp_contour_check_values(NspContour *H)
{
  if ( H->obj->z == NULLMAT) 
    {
       if (( H->obj->z = nsp_matrix_create("z",'r',0,0)) == NULLMAT)
       return FAIL;

    }
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
  if ( H->obj->levels == NULLMAT) 
    {
       if (( H->obj->levels = nsp_matrix_create("levels",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspContour *nsp_contour_create(char *name,NspMatrix* z,NspMatrix* x,NspMatrix* y,NspMatrix* levels,int nlevels,NspTypeBase *type)
{
 NspContour *H  = nsp_contour_create_void(name,type);
 if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_contour_create_partial(H) == FAIL) return NULLCONTOUR;
  if ( z == NULL )
    { H->obj->z = NULL;}
  else
    {
      if ((H->obj->z = (NspMatrix *)  nsp_object_copy_and_name("z",NSP_OBJECT(z))) == NULLMAT) return NULL;
    }
  if ( x == NULL )
    { H->obj->x = NULL;}
  else
    {
      if ((H->obj->x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT) return NULL;
    }
  if ( y == NULL )
    { H->obj->y = NULL;}
  else
    {
      if ((H->obj->y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT) return NULL;
    }
  if ( levels == NULL )
    { H->obj->levels = NULL;}
  else
    {
      if ((H->obj->levels = (NspMatrix *)  nsp_object_copy_and_name("levels",NSP_OBJECT(levels))) == NULLMAT) return NULL;
    }
  H->obj->nlevels=nlevels;
 if ( nsp_contour_check_values(H) == FAIL) return NULLCONTOUR;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspContour *nsp_contour_copy_partial(NspContour *H,NspContour *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspContour *nsp_contour_copy(NspContour *self)
{
  NspContour *H  =nsp_contour_create_void(NVOID,(NspTypeBase *) nsp_type_contour);
  if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONTOUR;
  if ( nsp_contour_copy_partial(H,self)== NULL) return NULLCONTOUR;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspContour *nsp_contour_full_copy_partial(NspContour *H,NspContour *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_contour))) == NULL) return NULLCONTOUR;
  H->obj->ref_count=1;
  if ( self->obj->z == NULL )
    { H->obj->z = NULL;}
  else
    {
      if ((H->obj->z = (NspMatrix *) nsp_object_copy_and_name("z",NSP_OBJECT(self->obj->z))) == NULLMAT) return NULL;
    }
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
  if ( self->obj->levels == NULL )
    { H->obj->levels = NULL;}
  else
    {
      if ((H->obj->levels = (NspMatrix *) nsp_object_copy_and_name("levels",NSP_OBJECT(self->obj->levels))) == NULLMAT) return NULL;
    }
  H->obj->nlevels=self->obj->nlevels;
  return H;
}

NspContour *nsp_contour_full_copy(NspContour *self)
{
  NspContour *H  =nsp_contour_create_void(NVOID,(NspTypeBase *) nsp_type_contour);
  if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONTOUR;
  if ( nsp_contour_full_copy_partial(H,self)== NULL) return NULLCONTOUR;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Contour
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_contour_create(Stack stack, int rhs, int opt, int lhs)
{
  NspContour *H;
  CheckStdRhs(0,0);
  /* want to be sure that type contour is initialized */
  nsp_type_contour = new_type_contour(T_BASE);
  if(( H = nsp_contour_create_void(NVOID,(NspTypeBase *) nsp_type_contour)) == NULLCONTOUR) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_contour_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_contour_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *contour_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_contour_get_z(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->z;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_z(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->z);
  return (NspObject *) ret;
}

static int _wrap_contour_set_z(void *self, char *attr, NspObject *O)
{
  NspMatrix *z;

  if ( ! IsMat(O) ) return FAIL;
  if ((z = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->z != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->z);
  ((NspContour *) self)->obj->z= z;
  return OK;
}

static NspObject *_wrap_contour_get_x(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_x(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_contour_set_x(void *self, char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->x);
  ((NspContour *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_contour_get_y(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_y(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_contour_set_y(void *self, char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->y);
  ((NspContour *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_contour_get_levels(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->levels;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_levels(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->levels);
  return (NspObject *) ret;
}

static int _wrap_contour_set_levels(void *self, char *attr, NspObject *O)
{
  NspMatrix *levels;

  if ( ! IsMat(O) ) return FAIL;
  if ((levels = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->levels != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->levels);
  ((NspContour *) self)->obj->levels= levels;
  return OK;
}

static NspObject *_wrap_contour_get_nlevels(void *self,char *attr)
{
  int ret;

  ret = ((NspContour *) self)->obj->nlevels;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_contour_set_nlevels(void *self, char *attr, NspObject *O)
{
  int nlevels;

  if ( IntScalar(O,&nlevels) == FAIL) return FAIL;
  ((NspContour *) self)->obj->nlevels= nlevels;
  return OK;
}

static AttrTab contour_attrs[] = {
  { "z", (attr_get_function *)_wrap_contour_get_z, (attr_set_function *)_wrap_contour_set_z,(attr_get_object_function *)_wrap_contour_get_obj_z, (attr_set_object_function *)int_set_object_failed },
  { "x", (attr_get_function *)_wrap_contour_get_x, (attr_set_function *)_wrap_contour_set_x,(attr_get_object_function *)_wrap_contour_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_contour_get_y, (attr_set_function *)_wrap_contour_set_y,(attr_get_object_function *)_wrap_contour_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "levels", (attr_get_function *)_wrap_contour_get_levels, (attr_set_function *)_wrap_contour_set_levels,(attr_get_object_function *)_wrap_contour_get_obj_levels, (attr_set_object_function *)int_set_object_failed },
  { "nlevels", (attr_get_function *)_wrap_contour_get_nlevels, (attr_set_function *)_wrap_contour_set_nlevels,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 48 "codegen/contour.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_contour(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 765 "contour.c"


#line 58 "codegen/contour.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_contour(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 777 "contour.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Contour_func[]={
  {"extractelts_contour", _wrap_nsp_extractelts_contour},
  {"setrowscols_contour", _wrap_nsp_setrowscols_contour},
  { "contour_create", int_contour_create},
  { NULL, NULL}
};

/* call ith function in the Contour interface */

int Contour_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Contour_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Contour_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Contour_func[i].name;
  *f = Contour_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Contour_register_classes(NspObject *d)
{

#line 22 "codegen/contour.override"

Init portion 


#line 817 "contour.c"
  nspgobject_register_class(d, "Contour", Contour, &NspContour_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 68 "codegen/contour.override"

/* inserted verbatim at the end */

/* 
static void nsp_draw_contour(BCG *Xgc,NspGraphic *Obj)
{
  NspVField *P = (NspVField *) Obj;
  int *xm,*ym,  j;
  int colminmax[2];
  double *zminmax = NULL;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;

  if ( P->obj->colminmax->mn == 2 ) 
    {
      colminmax[0] = P->obj->colminmax->R[0];
      colminmax[1] = P->obj->colminmax->R[1];
    }
  if ( P->obj->zminmax->mn == 2 ) 
    zminmax = P->obj->colminmax->R;
  else
    remap = FALSE;

  if  (  Xgc->scales->cosa==1.0 ) 
    {
      scale_f2i(Xgc,xx,yy,xx1,yy1,2);
      xm = graphic_alloc(0,P->obj->data->n+1,sizeof(int));
      ym = graphic_alloc(1,P->obj->data->m+1,sizeof(int));
      if ( xm == 0 || ym == 0 )
	{
	  Scistring("Xgray: running out of memory\n");
	  return ; 
	}
      for ( j =0 ; j < (P->obj->data->n+1) ; j++)	 
	xm[j]= (int) (( xx1[1]*j + xx1[0]*(P->obj->data->n-j) )/((double) P->obj->data->n));
      for ( j =0 ; j < (P->obj->data->m+1) ; j++)	 
	ym[j]= (int) (( yy1[0]*j + yy1[1]*(P->obj->data->m-j) )/((double) P->obj->data->m));
      Xgc->graphic_engine->fill_grid_rectangles1(Xgc,xm,ym,P->obj->data->R,
						 P->obj->data->m, 
						 P->obj->data->n,
						 remap,
						 colminmax,
						 zminmax);
    }
  else
    {
      double xp[4],yp[4];
      const double *z =P->obj->data->R;
      int nr =P->obj->data->m , nc=P->obj->data->n;
      int colmin,colmax;
      double zmin,zmax,coeff;
      int i,j,fill[1],cpat,xz[2];
      cpat = Xgc->graphic_engine->xget_pattern(Xgc);
      Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
      nsp_remap_colors(Xgc,remap,&colmin,&colmax,&zmin,&zmax,&coeff,colminmax,zminmax,z,nr*nc);
      
      for (i = 0 ; i < nc-1 ; i++)
	for (j = 0 ; j < nr-1 ; j++)
	  {
	    int w,h;
	    fill[0]= (remap == FALSE) ? rint(z[i+nr*j]) : rint((colmax-colmin)*(z[i+nr*j] - zmin)*coeff + colmin);
	    if ( fill[0] < colmin || fill[0] > colmax ) continue ;
	    Xgc->graphic_engine->xset_pattern(Xgc,fill[0]);
	    xp[0]= (( xx[1]*i + xx[0]*(P->obj->data->n-i) )/((double) P->obj->data->n));
	    yp[0]= (( yy[0]*j + yy[1]*(P->obj->data->m-j) )/((double) P->obj->data->m));
	    xp[1]= xp[0];
	    yp[1]= (( yy[0]*(j+1) + yy[1]*(P->obj->data->m-(j+1)) )/((double) P->obj->data->m));
	    xp[2]= (( xx[1]*(i+1) + xx[0]*(P->obj->data->n-(i+1)) )/((double) P->obj->data->n));
	    yp[2]= yp[1];
	    xp[3]= xp[2];
	    yp[3]= yp[0];
	    Xgc->graphic_engine->scale->fillpolyline(Xgc,xp,yp,4,1);
	  }
      Xgc->graphic_engine->xset_pattern(Xgc,cpat);
    }
  
}
*/

static double min_of_doubles(const double *x, int n);

static void nsp_draw_contour(BCG *Xgc,NspGraphic *Obj)
{
  NspContour *P = (NspContour *) Obj;
  double *x= P->obj->x->R; 
  double *y= P->obj->y->R; 
  double *z= P->obj->z->R; 
  int n1 = P->obj->x->mn;
  int n2 = P->obj->y->mn;
  
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  if ( P->obj->x->mn  == 0 || P->obj->y->mn  == 0 ) return;
  nsp_contour2d_draw(Xgc,x,y,z,n1,n2,P->obj->nlevels,P->obj->levels->R);
}

static double min_of_doubles(const double *x, int n)
{
  int i;
  double dx=1,mindx=1;
  if ( n < 2 ) return(mindx);
  mindx= Abs(x[1]-x[0]);
  mindx = ( mindx != 0 ) ? mindx : 1;
  for ( i = 2 ; i < n ; i++) 
    {
      dx = Abs(x[i]-x[i-1]);
      if ( dx < mindx && dx != 0 ) mindx=dx;
    }
  return(mindx);
}



static void nsp_translate_contour(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspContour *P = (NspContour *) Obj;
  int i;
  for ( i = 0 ; i < P->obj->x->mn ; i++) 
    P->obj->x->R[i] += tr[0];
  for ( i = 0 ; i < P->obj->y->mn ; i++) 
    P->obj->x->R[i] += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_contour(BCG *Xgc,NspGraphic *Obj,double *R)
{
  Sciprintf("we should get a double here for alpha\n");
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_contour(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int i;
  NspContour *P = (NspContour *) Obj;
  for ( i = 0 ; i < P->obj->x->mn ; i++) 
    P->obj->x->R[i] *= alpha[0];
  for ( i = 0 ; i < P->obj->y->mn ; i++) 
    P->obj->x->R[i] *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of contour 
 *
 */

static void nsp_getbounds_contour (BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspContour *P = (NspContour *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  if ( P->obj->x->mn != 0 ) 
    {
      bounds[0]=P->obj->x->R[0]; /* xmin */
      bounds[2]=P->obj->x->R[P->obj->x->mn-1];/* xmax */
    }
  if ( P->obj->y->mn != 0 ) 
    {
      bounds[1]=P->obj->y->R[0] ; /* ymin */
      bounds[3]=P->obj->y->R[P->obj->y->mn-1];/* ymax */
    }
}


#line 983 "contour.c"
