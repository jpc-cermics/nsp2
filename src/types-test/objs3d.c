/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/objs3d.override"
#include <nsp/figure.h>
#include <nsp/curve.h>
#include <nsp/polyhedron.h>
#include "../graphics/Plo3dObj.h"


extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_objs3d(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_objs3d(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_objs3d(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_objs3d(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_objs3d(BCG *Xgc,NspGraphic *o,double *bounds);
static void nsp_objs3d_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds);
static void nsp_objs3d_link_figure(NspGraphic *G, void *F);
static void nsp_objs3d_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_objs3d_children(NspGraphic *Obj);

/* should be inserted in figure.h */

extern void nsp_list_link_figure(NspList *L, NspFigure *F);
extern void nsp_list_unlink_figure(NspList *L, NspFigure *F);
extern int nsp_list_check_figure(NspList *L, NspFigure *F);
extern void nsp_graphic_link_figure(NspGraphic *G, void *F);
extern void nsp_graphic_unlink_figure(NspGraphic *G, void *F);
extern void nsp_figure_force_redraw( NspFigure *F);

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 44 "objs3d.c"

/* ----------- Objs3d ----------- */


#define  Objs3d_Private 
#include "nsp/object.h"
#include "nsp/objs3d.h"
#include "nsp/interf.h"

/* 
 * NspObjs3d inherits from NspGraphic 
 */

int nsp_type_objs3d_id=0;
NspTypeObjs3d *nsp_type_objs3d=NULL;

/*
 * Type object for Objs3d 
 * all the instance of NspTypeObjs3d share the same id. 
 * nsp_type_objs3d: is an instance of NspTypeObjs3d 
 *    used for objects of NspObjs3d type (i.e built with new_objs3d) 
 * other instances are used for derived classes 
 */
NspTypeObjs3d *new_type_objs3d(type_mode mode)
{
  NspTypeObjs3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_objs3d != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_objs3d;
    }
  if ((type =  malloc(sizeof(NspTypeObjs3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = objs3d_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = objs3d_get_methods; 
  type->new = (new_func *) new_objs3d;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for objs3d */ 

  top->pr = (print_func *) nsp_objs3d_print;                  
  top->dealloc = (dealloc_func *) nsp_objs3d_destroy;
  top->copy  =  (copy_func *) nsp_objs3d_copy;                 
  top->size  = (size_func *) nsp_objs3d_size;                
  top->s_type =  (s_type_func *) nsp_objs3d_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_objs3d_type_short_string;
  top->info = (info_func *) nsp_objs3d_info ;                  
  /* top->is_true = (is_true_func  *) nsp_objs3d_is_true; */
  /* top->loop =(loop_func *) nsp_objs3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_objs3d_object;
  top->eq  = (eq_func *) nsp_objs3d_eq;
  top->neq  = (eq_func *) nsp_objs3d_neq;
  top->save  = (save_func *) nsp_objs3d_xdr_save;
  top->load  = (load_func *) nsp_objs3d_xdr_load;
  top->create = (create_func*) int_objs3d_create;
  top->latex = (print_func *) nsp_objs3d_latex;
  
  /* specific methods for objs3d */
      
  type->init = (init_func *) init_objs3d;

#line 42 "codegen/objs3d.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_objs3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_objs3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_objs3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_objs3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_objs3d  ;
  ((NspTypeGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_objs3d_full_copy ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_objs3d_link_figure; 
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_objs3d_unlink_figure; 
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_objs3d_children ;
#line 126 "objs3d.c"
  /* 
   * Objs3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_objs3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeObjs3d called nsp_type_objs3d
       */
      type->id =  nsp_type_objs3d_id = nsp_new_type_id();
      nsp_type_objs3d = type;
      if ( nsp_register_type(nsp_type_objs3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_objs3d(mode);
    }
  else 
    {
       type->id = nsp_type_objs3d_id;
       return type;
    }
}

/*
 * initialize Objs3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_objs3d(NspObjs3d *Obj,NspTypeObjs3d *type)
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
 * new instance of Objs3d 
 */

NspObjs3d *new_objs3d() 
{
  NspObjs3d *loc; 
  /* type must exists */
  nsp_type_objs3d = new_type_objs3d(T_BASE);
  if ( (loc = malloc(sizeof(NspObjs3d)))== NULLOBJS3D) return loc;
  /* initialize object */
  if ( init_objs3d(loc,nsp_type_objs3d) == FAIL) return NULLOBJS3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Objs3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_objs3d_size(NspObjs3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char objs3d_type_name[]="Objs3d";
static char objs3d_short_type_name[]="objs3d";

static char *nsp_objs3d_type_as_string(void)
{
  return(objs3d_type_name);
}

static char *nsp_objs3d_type_short_string(NspObject *v)
{
  return(objs3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_objs3d_eq(NspObjs3d *A, NspObject *B)
{
  NspObjs3d *loc = (NspObjs3d *) B;
  if ( check_cast(B,nsp_type_objs3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->wrect)->type->eq(A->obj->wrect,loc->obj->wrect) == FALSE ) return FALSE;
  if ( A->obj->rho != loc->obj->rho) return FALSE;
  if ( A->obj->top != loc->obj->top) return FALSE;
  if ( NSP_OBJECT(A->obj->bounds)->type->eq(A->obj->bounds,loc->obj->bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->arect)->type->eq(A->obj->arect,loc->obj->arect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->frect)->type->eq(A->obj->frect,loc->obj->frect) == FALSE ) return FALSE;
  if ( strcmp(A->obj->title,loc->obj->title) != 0) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->colormap)->type->eq(A->obj->colormap,loc->obj->colormap) == FALSE ) return FALSE;
  if ( A->obj->alpha != loc->obj->alpha) return FALSE;
  if ( A->obj->theta != loc->obj->theta) return FALSE;
  if ( A->obj->with_box != loc->obj->with_box) return FALSE;
  if ( A->obj->box_color != loc->obj->box_color) return FALSE;
  if ( A->obj->box_style != loc->obj->box_style) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_objs3d_neq(NspObjs3d *A, NspObject *B)
{
  return ( nsp_objs3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_objs3d_xdr_save(XDR *xdrs, NspObjs3d *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->wrect)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->rho) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->top) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->arect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->frect)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->title) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colormap)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->alpha) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->theta) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->with_box) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->box_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->box_style) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspObjs3d  *nsp_objs3d_xdr_load_partial(XDR *xdrs, NspObjs3d *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_objs3d))) == NULL) return NULL;
  if ((M->obj->wrect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->rho) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->top) == FAIL) return NULL;
  if ((M->obj->arect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->frect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->title)) == FAIL) return NULL;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if ((M->obj->colormap =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->alpha) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->theta) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->with_box) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->box_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->box_style) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspObjs3d  *nsp_objs3d_xdr_load(XDR *xdrs)
{
  NspObjs3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLOBJS3D;
  if ((H  = nsp_objs3d_create_void(name,(NspTypeBase *) nsp_type_objs3d))== NULLOBJS3D) return H;
  if ((H  = nsp_objs3d_xdr_load_partial(xdrs,H))== NULLOBJS3D) return H;
#line 306 "objs3d.c"
  return H;
}

/*
 * delete 
 */

void nsp_objs3d_destroy_partial(NspObjs3d *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 320 "objs3d.c"
    nsp_matrix_destroy(H->obj->wrect);
    nsp_matrix_destroy(H->obj->bounds);
    nsp_matrix_destroy(H->obj->arect);
    nsp_matrix_destroy(H->obj->frect);
  nsp_string_destroy(&(H->obj->title));
    nsp_list_destroy(H->obj->children);
    nsp_matrix_destroy(H->obj->colormap);
    FREE(H->obj);
   }
}

void nsp_objs3d_destroy(NspObjs3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_objs3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_objs3d_info(NspObjs3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLOBJS3D) 
    {
      Sciprintf("Null Pointer Objs3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_objs3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_objs3d_print(NspObjs3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLOBJS3D) 
    {
      Sciprintf("Null Pointer Objs3d \n");
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
          nsp_objs3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_objs3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->wrect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"rho=%f\n",M->obj->rho);
  Sciprintf1(indent+2,"top	= %s\n", ( M->obj->top == TRUE) ? "T" : "F" );
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->arect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->arect),indent+2,"arect",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->frect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"title=%s\n",M->obj->title);
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colormap != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colormap),indent+2,"colormap",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"alpha=%f\n",M->obj->alpha);
  Sciprintf1(indent+2,"theta=%f\n",M->obj->theta);
  Sciprintf1(indent+2,"with_box	= %s\n", ( M->obj->with_box == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"box_color=%d\n",M->obj->box_color);
  Sciprintf1(indent+2,"box_style=%d\n",M->obj->box_style);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_objs3d_latex(NspObjs3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_objs3d_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->wrect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"rho=%f\n",M->obj->rho);
  Sciprintf1(indent+2,"top	= %s\n", ( M->obj->top == TRUE) ? "T" : "F" );
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->arect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->arect),indent+2,"arect",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->frect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"title=%s\n",M->obj->title);
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colormap != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colormap),indent+2,"colormap",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"alpha=%f\n",M->obj->alpha);
  Sciprintf1(indent+2,"theta=%f\n",M->obj->theta);
  Sciprintf1(indent+2,"with_box	= %s\n", ( M->obj->with_box == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"box_color=%d\n",M->obj->box_color);
  Sciprintf1(indent+2,"box_style=%d\n",M->obj->box_style);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Objs3d objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspObjs3d   *nsp_objs3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_objs3d_id) == TRUE ) return ((NspObjs3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_objs3d));
  return NULL;
}

int IsObjs3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_objs3d_id);
}

int IsObjs3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_objs3d_id);
}

NspObjs3d  *GetObjs3dCopy(Stack stack, int i)
{
  if (  GetObjs3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspObjs3d  *GetObjs3d(Stack stack, int i)
{
  NspObjs3d *M;
  if (( M = nsp_objs3d_object(NthObj(i))) == NULLOBJS3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspObjs3d *nsp_objs3d_create_void(char *name,NspTypeBase *type)
{
 NspObjs3d *H  = (type == NULL) ? new_objs3d() : type->new();
 if ( H ==  NULLOBJS3D)
  {
   Sciprintf("No more memory\n");
   return NULLOBJS3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLOBJS3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_objs3d_create_partial(NspObjs3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_objs3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->wrect = NULLMAT;
  H->obj->rho = 0.0;
  H->obj->top = TRUE;
  H->obj->bounds = NULLMAT;
  H->obj->arect = NULLMAT;
  H->obj->frect = NULLMAT;
  H->obj->title = NULL;
  H->obj->children = NULLLIST;
  H->obj->colormap = NULLMAT;
  H->obj->alpha = 35;
  H->obj->theta = 45;
  H->obj->with_box = TRUE;
  H->obj->box_color = 0;
  H->obj->box_style = 1;
  return OK;
}

int nsp_objs3d_check_values(NspObjs3d *H)
{
  if ( H->obj->wrect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->wrect = nsp_matrix_create("wrect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->wrect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->bounds == NULLMAT) 
    {
       if (( H->obj->bounds = nsp_matrix_create("bounds",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->arect == NULLMAT) 
    {
     double x_def[4]={0.08,0.08,0.08,0.08};
     if (( H->obj->arect = nsp_matrix_create("arect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->arect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->frect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->frect = nsp_matrix_create("frect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->frect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->title == NULL) 
    {
     if (( H->obj->title = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  if ( H->obj->colormap == NULLMAT) 
    {
       if (( H->obj->colormap = nsp_matrix_create("colormap",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspObjs3d *nsp_objs3d_create(char *name,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,NspList* children,NspMatrix* colormap,double alpha,double theta,gboolean with_box,int box_color,int box_style,NspTypeBase *type)
{
 NspObjs3d *H  = nsp_objs3d_create_void(name,type);
 if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_objs3d_create_partial(H) == FAIL) return NULLOBJS3D;
  H->obj->wrect= wrect;
  H->obj->rho=rho;
  H->obj->top=top;
  H->obj->bounds= bounds;
  H->obj->arect= arect;
  H->obj->frect= frect;
  H->obj->title = title;
  H->obj->children= children;
  H->obj->colormap= colormap;
  H->obj->alpha=alpha;
  H->obj->theta=theta;
  H->obj->with_box=with_box;
  H->obj->box_color=box_color;
  H->obj->box_style=box_style;
 if ( nsp_objs3d_check_values(H) == FAIL) return NULLOBJS3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspObjs3d *nsp_objs3d_copy_partial(NspObjs3d *H,NspObjs3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspObjs3d *nsp_objs3d_copy(NspObjs3d *self)
{
  NspObjs3d *H  =nsp_objs3d_create_void(NVOID,(NspTypeBase *) nsp_type_objs3d);
  if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLOBJS3D;
  if ( nsp_objs3d_copy_partial(H,self)== NULL) return NULLOBJS3D;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspObjs3d *nsp_objs3d_full_copy_partial(NspObjs3d *H,NspObjs3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_objs3d))) == NULL) return NULLOBJS3D;
  H->obj->ref_count=1;
  if ( self->obj->wrect == NULL )
    { H->obj->wrect = NULL;}
  else
    {
      if ((H->obj->wrect = (NspMatrix *) nsp_object_copy_and_name("wrect",NSP_OBJECT(self->obj->wrect))) == NULLMAT) return NULL;
    }
  H->obj->rho=self->obj->rho;
  H->obj->top=self->obj->top;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_copy_and_name("bounds",NSP_OBJECT(self->obj->bounds))) == NULLMAT) return NULL;
    }
  if ( self->obj->arect == NULL )
    { H->obj->arect = NULL;}
  else
    {
      if ((H->obj->arect = (NspMatrix *) nsp_object_copy_and_name("arect",NSP_OBJECT(self->obj->arect))) == NULLMAT) return NULL;
    }
  if ( self->obj->frect == NULL )
    { H->obj->frect = NULL;}
  else
    {
      if ((H->obj->frect = (NspMatrix *) nsp_object_copy_and_name("frect",NSP_OBJECT(self->obj->frect))) == NULLMAT) return NULL;
    }
  if ((H->obj->title = nsp_string_copy(self->obj->title)) == NULL) return NULL;
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_copy_and_name("children",NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  if ( self->obj->colormap == NULL )
    { H->obj->colormap = NULL;}
  else
    {
      if ((H->obj->colormap = (NspMatrix *) nsp_object_copy_and_name("colormap",NSP_OBJECT(self->obj->colormap))) == NULLMAT) return NULL;
    }
  H->obj->alpha=self->obj->alpha;
  H->obj->theta=self->obj->theta;
  H->obj->with_box=self->obj->with_box;
  H->obj->box_color=self->obj->box_color;
  H->obj->box_style=self->obj->box_style;
  return H;
}

NspObjs3d *nsp_objs3d_full_copy(NspObjs3d *self)
{
  NspObjs3d *H  =nsp_objs3d_create_void(NVOID,(NspTypeBase *) nsp_type_objs3d);
  if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLOBJS3D;
  if ( nsp_objs3d_full_copy_partial(H,self)== NULL) return NULLOBJS3D;
#line 689 "objs3d.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Objs3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_objs3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspObjs3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type objs3d is initialized */
  nsp_type_objs3d = new_type_objs3d(T_BASE);
  if(( H = nsp_objs3d_create_void(NVOID,(NspTypeBase *) nsp_type_objs3d)) == NULLOBJS3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_objs3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_objs3d_check_values(H) == FAIL) return RET_BUG;
#line 709 "objs3d.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *objs3d_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_objs3d_get_wrect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->wrect;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_wrect(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->wrect);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_wrect(void *self, char *attr, NspObject *O)
{
  NspMatrix *wrect;

  if ( ! IsMat(O) ) return FAIL;
  if ((wrect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->wrect != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->wrect);
  ((NspObjs3d *) self)->obj->wrect= wrect;
  return OK;
}

#line 88 "codegen/objs3d.override"
/* override set rho */
static int _wrap_objs3d_set_rho(void *self, char *attr, NspObject *O)
{
  double rho;
  if ( DoubleScalar(O,&rho) == FAIL) return FAIL;

  if ( ((NspObjs3d *) self)->obj->rho != rho) 
    {
      ((NspObjs3d *) self)->obj->rho = rho;
      nsp_figure_force_redraw(((NspGraphic *) self)->obj->Fig);
    }
  return OK;
}

#line 763 "objs3d.c"
static NspObject *_wrap_objs3d_get_rho(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->rho;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_objs3d_get_top(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->top;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_objs3d_set_top(void *self, char *attr, NspObject *O)
{
  int top;

  if ( BoolScalar(O,&top) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->top= top;
  return OK;
}

static NspObject *_wrap_objs3d_get_arect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->arect;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_arect(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->arect);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_arect(void *self, char *attr, NspObject *O)
{
  NspMatrix *arect;

  if ( ! IsMat(O) ) return FAIL;
  if ((arect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->arect != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->arect);
  ((NspObjs3d *) self)->obj->arect= arect;
  return OK;
}

static NspObject *_wrap_objs3d_get_frect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->frect;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_frect(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->frect);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_frect(void *self, char *attr, NspObject *O)
{
  NspMatrix *frect;

  if ( ! IsMat(O) ) return FAIL;
  if ((frect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->frect != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->frect);
  ((NspObjs3d *) self)->obj->frect= frect;
  return OK;
}

static NspObject *_wrap_objs3d_get_title(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->title;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_objs3d_set_title(void *self, char *attr, NspObject *O)
{
  char *title;

  if ((title = nsp_string_object(O))==NULL) return FAIL;
  if ((title = nsp_string_copy(title)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspObjs3d *) self)->obj->title);
  ((NspObjs3d *) self)->obj->title= title;
  return OK;
}

#line 104 "codegen/objs3d.override"

/* here we override get_obj  and set_obj 
 * we want get to be followed by a set to check that 
 * inserted value is correct thus we use copy = TRUE.
 */

static NspObject *_wrap_objs3d_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE; 
  ret = ((NspList*) ((NspObjs3d *) self)->obj->children);
  return (NspObject *) ret;
}

/* in this function we can check that val is correct before 
 * setting the field with val. return FAIL if val is incorrect.
 */

static int _wrap_objs3d_set_obj_children(void *self,NspObject *val)
{
  double inside_bounds[6];
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (((NspObjs3d *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspObjs3d *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspObjs3d *) self)->obj->children);
    }
  ((NspObjs3d *) self)->obj->children =  (NspList *) val;
  nsp_objs3d_compute_inside_bounds(NULL,self,inside_bounds);
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig);
  return OK;
}

static int _wrap_objs3d_set_children(void *self, char *attr, NspObject *O)
{
  double inside_bounds[6];
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspObjs3d *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspObjs3d *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspObjs3d *) self)->obj->children);
    }
  ((NspObjs3d *) self)->obj->children= children;
  nsp_objs3d_compute_inside_bounds(NULL,self,inside_bounds);
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig);
  return OK;
}


#line 929 "objs3d.c"
static NspObject *_wrap_objs3d_get_children(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspObjs3d *) self)->obj->children;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_colormap(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->colormap;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_colormap(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->colormap);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_colormap(void *self, char *attr, NspObject *O)
{
  NspMatrix *colormap;

  if ( ! IsMat(O) ) return FAIL;
  if ((colormap = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->colormap != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->colormap);
  ((NspObjs3d *) self)->obj->colormap= colormap;
  return OK;
}

static NspObject *_wrap_objs3d_get_alpha(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->alpha;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_objs3d_set_alpha(void *self, char *attr, NspObject *O)
{
  double alpha;

  if ( DoubleScalar(O,&alpha) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->alpha= alpha;
  return OK;
}

static NspObject *_wrap_objs3d_get_theta(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->theta;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_objs3d_set_theta(void *self, char *attr, NspObject *O)
{
  double theta;

  if ( DoubleScalar(O,&theta) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->theta= theta;
  return OK;
}

static NspObject *_wrap_objs3d_get_with_box(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->with_box;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_objs3d_set_with_box(void *self, char *attr, NspObject *O)
{
  int with_box;

  if ( BoolScalar(O,&with_box) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->with_box= with_box;
  return OK;
}

static NspObject *_wrap_objs3d_get_box_color(void *self,char *attr)
{
  int ret;

  ret = ((NspObjs3d *) self)->obj->box_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_objs3d_set_box_color(void *self, char *attr, NspObject *O)
{
  int box_color;

  if ( IntScalar(O,&box_color) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->box_color= box_color;
  return OK;
}

static NspObject *_wrap_objs3d_get_box_style(void *self,char *attr)
{
  int ret;

  ret = ((NspObjs3d *) self)->obj->box_style;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_objs3d_set_box_style(void *self, char *attr, NspObject *O)
{
  int box_style;

  if ( IntScalar(O,&box_style) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->box_style= box_style;
  return OK;
}

static AttrTab objs3d_attrs[] = {
  { "wrect", (attr_get_function *)_wrap_objs3d_get_wrect, (attr_set_function *)_wrap_objs3d_set_wrect,(attr_get_object_function *)_wrap_objs3d_get_obj_wrect, (attr_set_object_function *)int_set_object_failed },
  { "rho", (attr_get_function *)_wrap_objs3d_get_rho, (attr_set_function *)_wrap_objs3d_set_rho,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "top", (attr_get_function *)_wrap_objs3d_get_top, (attr_set_function *)_wrap_objs3d_set_top,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "arect", (attr_get_function *)_wrap_objs3d_get_arect, (attr_set_function *)_wrap_objs3d_set_arect,(attr_get_object_function *)_wrap_objs3d_get_obj_arect, (attr_set_object_function *)int_set_object_failed },
  { "frect", (attr_get_function *)_wrap_objs3d_get_frect, (attr_set_function *)_wrap_objs3d_set_frect,(attr_get_object_function *)_wrap_objs3d_get_obj_frect, (attr_set_object_function *)int_set_object_failed },
  { "title", (attr_get_function *)_wrap_objs3d_get_title, (attr_set_function *)_wrap_objs3d_set_title,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "children", (attr_get_function *)_wrap_objs3d_get_children, (attr_set_function *)_wrap_objs3d_set_children,(attr_get_object_function *)_wrap_objs3d_get_obj_children, (attr_set_object_function *)_wrap_objs3d_set_obj_children },
  { "colormap", (attr_get_function *)_wrap_objs3d_get_colormap, (attr_set_function *)_wrap_objs3d_set_colormap,(attr_get_object_function *)_wrap_objs3d_get_obj_colormap, (attr_set_object_function *)int_set_object_failed },
  { "alpha", (attr_get_function *)_wrap_objs3d_get_alpha, (attr_set_function *)_wrap_objs3d_set_alpha,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "theta", (attr_get_function *)_wrap_objs3d_get_theta, (attr_set_function *)_wrap_objs3d_set_theta,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "with_box", (attr_get_function *)_wrap_objs3d_get_with_box, (attr_set_function *)_wrap_objs3d_set_with_box,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "box_color", (attr_get_function *)_wrap_objs3d_get_box_color, (attr_set_function *)_wrap_objs3d_set_box_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "box_style", (attr_get_function *)_wrap_objs3d_get_box_style, (attr_set_function *)_wrap_objs3d_set_box_style,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 62 "codegen/objs3d.override"
int _wrap_objs3d_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 1091 "objs3d.c"


#line 162 "codegen/objs3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_objs3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1103 "objs3d.c"


#line 172 "codegen/objs3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_objs3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 1115 "objs3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Objs3d_func[]={
  {"objs3d_attach", _wrap_objs3d_attach},
  {"extractelts_objs3d", _wrap_nsp_extractelts_objs3d},
  {"setrowscols_objs3d", _wrap_nsp_setrowscols_objs3d},
  { "objs3d_create", int_objs3d_create},
  { NULL, NULL}
};

/* call ith function in the Objs3d interface */

int Objs3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Objs3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Objs3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Objs3d_func[i].name;
  *f = Objs3d_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Objs3d_register_classes(NspObject *d)
{

#line 37 "codegen/objs3d.override"

Init portion 


#line 1156 "objs3d.c"
  nspgobject_register_class(d, "Objs3d", Objs3d, &NspObjs3d_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 182 "codegen/objs3d.override"

/* inserted verbatim at the end */
extern void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,double *arect,
					 int *aaint,int isomode, int auto_axes, char *xf);
extern Plot3dBox* make_box(BCG *Xgc,double Box[], GBoolean with_ticks, BoxStyle box_style,int box_color, double lim[]);
extern void apply_transforms(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);
extern void nsp_obj3d_dsortc(double x[], int *n, int p[]);
extern void nsp_obj3d_draw_box(BCG *Xgc,Plot3dBox *B);
extern void nsp_obj3d_draw_near_box_segments(BCG *Xgc,Plot3dBox *B);
extern void nsp_obj3d_free_box(Plot3dBox *B);
static void nsp_draw_objs3d_s2( BCG *Xgc,NspObjs3d *Obj,double theta,double alpha,const char *legend,
				int *flag,double *ebox,int with_box,int box_color,int box_style);

static void nsp_draw_objs3d(BCG *Xgc,NspGraphic *Obj, void *data)
{
  char xf[]="onn";
  char strflag[]="151";
  double WRect[4],*wrect1,WRect1[4], FRect[4], ARect[4], frect[4]={0,0,10,10}, inside_bounds[6];
  char logscale[2];
  int aaint[4]={10,2,10,2};
  Cell *cloc;
  NspList *L;
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  /* draw elements */
  L = P->obj->children;
  cloc = L->first ;
  /* we change the scale according to the objs3d */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  if ( P->obj->top == TRUE ) 
    {
      /* This is a top level objs3d, wrect gives the objs3d position in the 
       * enclosing graphic window. 
       */
      set_scale(Xgc,"fTffft",P->obj->wrect->R,NULL,NULL,NULL,P->obj->arect->R);
      wrect1= P->obj->wrect->R;
    }
  else 
    {
      /* This is not a top level objs3d, we draw its enclosing rectangle 
       * if alpha is non nul we should draw a rotated rectangle
       */
      Xgc->graphic_engine->scale->drawrectangle(Xgc,P->obj->wrect->R);
      /* wrect->R is [left,up,w,h] 
       * we need to compute wrect->R in term on window/proportions 
       */
      WRect1[0]= ARect[0]+(1-ARect[0]-ARect[2])*(P->obj->wrect->R[0]-FRect[0])/(FRect[2]-FRect[0]);
      WRect1[1]= ARect[1]+(1-ARect[1]-ARect[3])*(1- (P->obj->wrect->R[1]-FRect[1])/(FRect[3]-FRect[1]));
      WRect1[2]= (1-ARect[0]-ARect[2])*(P->obj->wrect->R[2])/(FRect[2]-FRect[0]);
      WRect1[3]= (1-ARect[1]-ARect[3])*(P->obj->wrect->R[3])/(FRect[3]-FRect[1]);
      wrect1 = WRect1;
      Xgc->scales->cosa= cos( P->obj->rho);
      Xgc->scales->sina= sin( P->obj->rho);
    }
  /* we directly change the default scale because we do not want 
   * to register all the scales that will be generated by set_scale 
   * thus we use T in flag[1].
   */
  /* set_scale(Xgc,"fTtfff",WRect1,P->obj->frect->R,NULL,NULL,NULL); */
  nsp_axes_update_frame_bounds(Xgc,wrect1,
			       TRUE ? frect : P->obj->frect->R,
			       P->obj->arect->R,
			       aaint,
			       TRUE,
			       TRUE,
			       xf);
  nsp_objs3d_compute_inside_bounds(Xgc,Obj,inside_bounds);
  axis_draw(Xgc,strflag);
  frame_clip_on(Xgc);
  {
    int flag[]={1,2,4};
    char legend[]="X@Y@Z";
    if ( P->obj->colormap->n == 3 )
      Xgc->graphic_engine->scale->xset_colormap(Xgc,P->obj->colormap->m,
						P->obj->colormap->R);
    nsp_draw_objs3d_s2(Xgc,P,P->obj->theta,P->obj->alpha,legend,flag,inside_bounds,
		       P->obj->with_box,P->obj->box_color,P->obj->box_style);
  }
  /* Note that clipping is wrong when an axe is rotated 
   * since clipping only works with rectangles 
   */
  frame_clip_off(Xgc);
  /* title if present */
  if ( P->obj->title[0] != '\0') 
    Xgc->graphic_engine->scale->displaystringa(Xgc,P->obj->title,1);
  /* scale back */
  set_scale(Xgc,"fTtfft",WRect,FRect,NULL,NULL,ARect);
  if (  P->obj->top != TRUE )
    {
      Xgc->scales->cosa=1.0;
      Xgc->scales->sina=0.0;
    }
}



/* compute the bounds of the set of objects countained in the 
 * objs3d 
 */

static void nsp_objs3d_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  int i;
  double l_bounds[6];
  Cell *cloc;
  NspList *L;
  NspObjs3d *P = (NspObjs3d *) Obj;
  L = P->obj->children;
  cloc = L->first ;
  
  if ( cloc == NULLCELL) 
    {
      bounds[0]=bounds[1]=bounds[2]=bounds[3]=bounds[4]=bounds[5]=0;
      return;
    }
  
  bounds[0]=bounds[2]=bounds[4]=LARGEST_REAL;
  bounds[1]=bounds[3]=bounds[5]=-LARGEST_REAL;

  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->bounds(Xgc,G,l_bounds);
	  for ( i = 0 ; i < 3 ; i++) 
	    {
	      if ( l_bounds[2*i] < bounds[2*i] )   bounds[2*i]= l_bounds[2*i];
	      if ( l_bounds[2*i+1] > bounds[2*i+1])   bounds[2*i+1]= l_bounds[2*i+1];
	    }
	}
      cloc = cloc->next;
    }
}

static void nsp_translate_objs3d(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[0] += tr[0];
  P->obj->wrect->R[1] += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_objs3d(BCG *Xgc,NspGraphic *Obj,double *R)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  Sciprintf("we should get a double here for rho\n");
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_objs3d(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[2] *= alpha[0];
  P->obj->wrect->R[3] *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of objs3d 
 *
 */

static void nsp_getbounds_objs3d(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  bounds[0]=P->obj->wrect->R[0]; /* xmin */
  bounds[1]=P->obj->wrect->R[1]-P->obj->wrect->R[3];/* ymin */
  bounds[2]=P->obj->wrect->R[0]+P->obj->wrect->R[2];/* xmax */
  bounds[3]=P->obj->wrect->R[1];/* ymax */
}

static void nsp_objs3d_link_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_link_figure(G,F);
  /* link children */
  nsp_list_link_figure(((NspObjs3d *) G)->obj->children,F);
}


static void nsp_objs3d_unlink_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_unlink_figure(G,F);
  /* link children */
  nsp_list_unlink_figure(((NspObjs3d *) G)->obj->children,F);
}

static NspList *nsp_objs3d_children(NspGraphic *Obj)
{
  return  ((NspObjs3d *) Obj)->obj->children;
}


static void nsp_draw_3d_obj_ogl( BCG *Xgc,NspObjs3d *,double theta,double alpha,const char *legend,
				 int *flag,double *ebox,int with_box,int box_color,int box_style);




/* Author: Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> */

static void nsp_draw_objs3d_s2( BCG *Xgc,NspObjs3d *Obj,double theta,double alpha,const char *legend,
				int *flag,double *ebox,int with_box,int box_color,int box_style)
{
  NspObject **objs_array= NULL;
  Cell *cloc;
  NspList *Children;
  nsp_box_3d box;
  double x[2],y[2],zz[2],zzmin,zzmax;
  int two=2;
  double Box[6]={ebox[0],ebox[2],ebox[4],ebox[1],ebox[3],ebox[5]};
  int nf=0,nbObj;
  int i, j, k, n, *p;
  HFstruct *HF;
  double lim[3], *z;
  Plot3dBox *B=NULL;
  int flagx;
  /* should be shared */
  int foreground_color;
  int background_color;

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_draw_3d_obj_ogl(Xgc,Obj,theta,alpha,legend,flag,ebox,with_box,box_color,box_style);
      nsp_ogl_set_2dview(Xgc);
      return; 
    }
#endif
    
  flagx = Xgc->graphic_engine->xget_last(Xgc);
  /* XXX */
  foreground_color = flagx+1;
  background_color = flagx+2;

  /* allocate a structure for drawing purpose 
   * The unchanged values are kept in Lobj
   */

  /* Obj = (Obj3d *)obj3d_from_list(SciStack,Lobj,TRUE,&err,&nf,&nbObj) ; */
  Children = nsp_objs3d_children((NspGraphic *) Obj);
  nbObj = nsp_list_length(Children);
  /* we have to loop here to collect the number of faces */
  cloc = Children->first ;
  nf = 0;

  while ( cloc != NULLCELL ) 
    {
      NspGraphic *G = (NspGraphic *) cloc->O;
      nf += G->type->n_faces(Xgc,G);
      cloc = cloc->next;
    }

  x[0]= Box[0];y[0]= Box[1];zz[0]= Box[2];
  x[1]= Box[3];y[1]= Box[4];zz[1]= Box[5];
  
  nsp_plot3d_update_bounds(Xgc,"box",x,y,zz,&two,&two,&theta,&alpha,legend,&flag[1],
			   ebox,&zzmin,&zzmax,param3d_t);
  SetEch3d1(Xgc,&box,ebox,theta,alpha,(long)(flag[1]+1)/2);

#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl pretending we are doing 2d !!! */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
    }
#endif
  
  lim[0] = 1.e+10;
  lim[1] = 1.e+10;
  lim[2] = - 1.e+10;

  /* fabrication de la boite et calcul de ses coordonnees ds le repere local */
  if ( with_box == TRUE ) B = make_box(Xgc,Box, BTRUE, box_style,box_color, lim);
  
  HF= malloc( nf * sizeof(HFstruct) );
  z = malloc( nf * sizeof(double) );
  p = malloc( nf * sizeof(int) );

  /* just to accelerate next step */
  objs_array = malloc( nbObj*sizeof(NspObject *));

  cloc = Children->first ;
  n=0;
  while ( cloc != NULLCELL ) 
    {
      objs_array[n++]= cloc->O; 
      cloc = cloc->next;
    }
  
  /* step 1 : for each object :
   *            a/ get the coordinates in the local repair
   *               and determines the pos of each point within the pyramidal
   *               visible region (IN, OUT_XY, OUT_Z)
   *            b/ then add the visible parts (faces, segments, points) in the z 
   *               and HF arrays for the hidden face algorithm (only partial visible 
   *               parts without any OUT_Z point are included)
   */
  n = 0;k=0;
  cloc = Children->first ;
  nf = 0;
  while ( cloc != NULLCELL ) 
    {
      NspGraphic *G = (NspGraphic *) cloc->O;
      G->type->zmean(Xgc,G,z,HF,&n,k,lim);
      k++;
      cloc = cloc->next;
    }
  /*  step 3 : sort of all the a priori visible "faces" (faces, segments, points) */
  nsp_obj3d_dsortc(z, &n, p);

  /* step 4 : drawing of each faces */
  if ( with_box == TRUE  ) nsp_obj3d_draw_box(Xgc,B);

  for (i = n -1 ; i >= 0 ; i--)
    {
      k = HF[p[i]].num_obj;  /* numero de l'objet correspondant a cette "face" */
      j = HF[p[i]].num_in_obj; /* son numéro de face dans l'objet en question */
      /* dessin partiel de l'objet en utilisant la face j */
      /* XXX OBJ3D(Obj[k].obj)->draw_partial(Xgc,Obj[k].obj,j); */
      ((NspGraphic *) objs_array[k])->type->draw(Xgc,(NspGraphic *) objs_array[k],&j);
    }
  if ( with_box == TRUE  &&  B->box_style == SCILAB )  nsp_obj3d_draw_near_box_segments(Xgc,B);
  if ( with_box == TRUE ) nsp_obj3d_free_box(B);
  free(HF);
  free(z);
  free(p);
}

#ifdef  WITH_GTKGLEXT 

static void nsp_draw_3d_obj_ogl( BCG *Xgc,NspObjs3d *Obj,double theta,double alpha,const char *legend,
				 int *flag,double *ebox,int with_box,int box_color,int box_style)
{
  Cell *cloc;
  NspList *Children;
  /* Stack stack;*/ /* just used for messages i.e NspFname(stack) */
  nsp_box_3d box;
  double x[2],y[2],zz[2],zzmin,zzmax;
  double Box[6]={ebox[0],ebox[2],ebox[4],ebox[1],ebox[3],ebox[5]};
  int flagx, nf=0, two=2;
  double lim[3];
  Plot3dBox *B;
  /* should be shared */
  int foreground_color;
  int background_color;

  /* NspFname(stack) ="drawobj"; */
  flagx = Xgc->graphic_engine->xget_last(Xgc);
  foreground_color = flagx+1;
  background_color = flagx+2;

  /* allocate a structure for drawing purpose 
   * The unchanged values are kept in Lobj
   */
  
  x[0]= Box[0];y[0]= Box[1];zz[0]= Box[2];
  x[1]= Box[3];y[1]= Box[4];zz[1]= Box[5];

  nsp_plot3d_update_bounds(Xgc,"box",x,y,zz,&two,&two,&theta,&alpha,legend,&flag[1],
			   ebox,&zzmin,&zzmax,param3d_t);
  SetEch3d1(Xgc,&box,ebox,theta,alpha,(long)(flag[1]+1)/2);
  
  lim[0] = 1.e+10;
  lim[1] = 1.e+10;
  lim[2] = - 1.e+10;

  /* fabrication de la boite et calcul de ses coordonnees ds le repere local */
  if ( with_box == TRUE  )
    {
      B = make_box(Xgc,Box, BTRUE, box_style,box_color,lim);
      nsp_obj3d_draw_box(Xgc,B);
      if (B->box_style == SCILAB ) nsp_obj3d_draw_near_box_segments(Xgc,B);
      nsp_obj3d_free_box(B);
    }

  Children = nsp_objs3d_children((NspGraphic *) Obj);
  cloc = Children->first ;
  nf = 0;
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
#endif 

/* Obj is a Figure 
 *
 */

void nsp_figure_change3d_orientation(NspGraphic *Obj,double theta, double alpha)
{
  NspFigure *F = (NspFigure *) Obj;
  NspList *L = F->obj->children;
  Cell *cloc = (L != NULL) ? L->first : NULLCELL;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ &&  IsObjs3d(cloc->O)) 
	{
	  NspObjs3d *Obj = (NspObjs3d *) cloc->O;
	  Obj->obj->alpha = alpha;
	  Obj->obj->theta = theta;
	}
      cloc = cloc->next;
    }
}



#line 1583 "objs3d.c"
