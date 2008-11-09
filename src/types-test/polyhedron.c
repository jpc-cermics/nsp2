/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/polyhedron.override"
#include "nsp/polyhedron.h"
#include <nsp/figure.h> 
#include <nsp/gsort-p.h>

#include "../graphics/Plo3dObj.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
extern void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p); 
extern  int nsp_obj3d_orientation(int x[], int y[], int n);
extern void nsp_figure_force_redraw( NspFigure *F);
extern void apply_transforms(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);
#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

static void nsp_draw_polyhedron(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_polyhedron(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_polyhedron(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_polyhedron(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_polyhedron(BCG *Xgc,NspGraphic *o,double *bounds);
static void nsp_polyhedron_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
static int nsp_polyhedron_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_polyhedron(NspPolyhedron *P);

static void draw_polyhedron_ogl(BCG *Xgc,void *Ob);
static void draw_polyhedron_face(BCG *Xgc,NspGraphic *Ob, int j);

static NspMatrix *nsp_surf_to_faces(const char *name,NspMatrix *x,NspMatrix *y)  ;
static NspMatrix *nsp_surf_to_coords(const char *name,NspMatrix *x,NspMatrix *y, NspMatrix *z);

static int nsp_facets_to_faces(NspMatrix *x,NspMatrix *y,NspMatrix *z,NspMatrix *colors,
			       NspMatrix **Cr,NspMatrix **Fr);

#line 46 "polyhedron.c"

/* ----------- Polyhedron ----------- */


#define  Polyhedron_Private 
#include "nsp/object.h"
#include "nsp/polyhedron.h"
#include "nsp/interf.h"

/* 
 * NspPolyhedron inherits from NspGraphic 
 */

int nsp_type_polyhedron_id=0;
NspTypePolyhedron *nsp_type_polyhedron=NULL;

/*
 * Type object for Polyhedron 
 * all the instance of NspTypePolyhedron share the same id. 
 * nsp_type_polyhedron: is an instance of NspTypePolyhedron 
 *    used for objects of NspPolyhedron type (i.e built with new_polyhedron) 
 * other instances are used for derived classes 
 */
NspTypePolyhedron *new_type_polyhedron(type_mode mode)
{
  NspTypePolyhedron *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_polyhedron != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_polyhedron;
    }
  if ((type =  malloc(sizeof(NspTypePolyhedron))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = polyhedron_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = polyhedron_get_methods; 
  type->new = (new_func *) new_polyhedron;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for polyhedron */ 

  top->pr = (print_func *) nsp_polyhedron_print;                  
  top->dealloc = (dealloc_func *) nsp_polyhedron_destroy;
  top->copy  =  (copy_func *) nsp_polyhedron_copy;                 
  top->size  = (size_func *) nsp_polyhedron_size;                
  top->s_type =  (s_type_func *) nsp_polyhedron_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_polyhedron_type_short_string;
  top->info = (info_func *) nsp_polyhedron_info ;                  
  /* top->is_true = (is_true_func  *) nsp_polyhedron_is_true; */
  /* top->loop =(loop_func *) nsp_polyhedron_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_polyhedron_object;
  top->eq  = (eq_func *) nsp_polyhedron_eq;
  top->neq  = (eq_func *) nsp_polyhedron_neq;
  top->save  = (save_func *) nsp_polyhedron_xdr_save;
  top->load  = (load_func *) nsp_polyhedron_xdr_load;
  top->create = (create_func*) int_polyhedron_create;
  top->latex = (print_func *) nsp_polyhedron_latex;
  
  /* specific methods for polyhedron */
      
  type->init = (init_func *) init_polyhedron;

#line 44 "codegen/polyhedron.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_polyhedron;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_polyhedron ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_polyhedron  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_polyhedron  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_polyhedron  ;
  ((NspTypeGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_polyhedron_full_copy ;
  /* next method are defined in NspGraphic and need not be chnaged here for Polyhedron */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 
  ((NspTypeGraphic *) type->surtype)->zmean = nsp_polyhedron_zmean;
  ((NspTypeGraphic *) type->surtype)->n_faces = nsp_polyhedron_n_faces;

#line 133 "polyhedron.c"
  /* 
   * Polyhedron interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_polyhedron_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePolyhedron called nsp_type_polyhedron
       */
      type->id =  nsp_type_polyhedron_id = nsp_new_type_id();
      nsp_type_polyhedron = type;
      if ( nsp_register_type(nsp_type_polyhedron) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_polyhedron(mode);
    }
  else 
    {
       type->id = nsp_type_polyhedron_id;
       return type;
    }
}

/*
 * initialize Polyhedron instances 
 * locally and by calling initializer on parent class 
 */

static int init_polyhedron(NspPolyhedron *Obj,NspTypePolyhedron *type)
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
 * new instance of Polyhedron 
 */

NspPolyhedron *new_polyhedron() 
{
  NspPolyhedron *loc; 
  /* type must exists */
  nsp_type_polyhedron = new_type_polyhedron(T_BASE);
  if ( (loc = malloc(sizeof(NspPolyhedron)))== NULLPOLYHEDRON) return loc;
  /* initialize object */
  if ( init_polyhedron(loc,nsp_type_polyhedron) == FAIL) return NULLPOLYHEDRON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Polyhedron 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_polyhedron_size(NspPolyhedron *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char polyhedron_type_name[]="Polyhedron";
static char polyhedron_short_type_name[]="polyhedron";

static char *nsp_polyhedron_type_as_string(void)
{
  return(polyhedron_type_name);
}

static char *nsp_polyhedron_type_short_string(NspObject *v)
{
  return(polyhedron_short_type_name);
}

/*
 * A == B 
 */

static int nsp_polyhedron_eq(NspPolyhedron *A, NspObject *B)
{
  NspPolyhedron *loc = (NspPolyhedron *) B;
  if ( check_cast(B,nsp_type_polyhedron_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  if ( A->obj->Mcoord_l != loc->obj->Mcoord_l) return FALSE;
  if ( NSP_OBJECT(A->obj->Mface)->type->eq(A->obj->Mface,loc->obj->Mface) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->Mcolor)->type->eq(A->obj->Mcolor,loc->obj->Mcolor) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->Mback_color)->type->eq(A->obj->Mback_color,loc->obj->Mback_color) == FALSE ) return FALSE;
  if ( A->obj->mesh != loc->obj->mesh) return FALSE;
  {int i;
    for ( i = 0 ; i < A->obj->pos_length ; i++)
      if ( A->obj->pos[i] != loc->obj->pos[i]) return FALSE;
  }
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_polyhedron_neq(NspPolyhedron *A, NspObject *B)
{
  return ( nsp_polyhedron_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_polyhedron_xdr_save(XDR *xdrs, NspPolyhedron *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_polyhedron)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mface)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcolor)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mback_color)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mesh) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspPolyhedron  *nsp_polyhedron_xdr_load_partial(XDR *xdrs, NspPolyhedron *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_polyhedron))) == NULL) return NULL;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->Mface =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->Mcolor =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->Mback_color =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mesh) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspPolyhedron  *nsp_polyhedron_xdr_load(XDR *xdrs)
{
  NspPolyhedron *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPOLYHEDRON;
  if ((H  = nsp_polyhedron_create_void(name,(NspTypeBase *) nsp_type_polyhedron))== NULLPOLYHEDRON) return H;
  if ((H  = nsp_polyhedron_xdr_load_partial(xdrs,H))== NULLPOLYHEDRON) return H;

#line 69 "codegen/polyhedron.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyhedron(H)== FAIL) return NULL; 

#line 305 "polyhedron.c"
  return H;
}

/*
 * delete 
 */

void nsp_polyhedron_destroy_partial(NspPolyhedron *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {

#line 74 "codegen/polyhedron.override"
  /* verbatim in destroy */
  nsp_matrix_destroy(H->obj->Mcoord_l);

#line 324 "polyhedron.c"
    nsp_matrix_destroy(H->obj->Mcoord);
    nsp_matrix_destroy(H->obj->Mface);
    nsp_matrix_destroy(H->obj->Mcolor);
    nsp_matrix_destroy(H->obj->Mback_color);
    FREE(H->obj->pos);
    FREE(H->obj);
   }
}

void nsp_polyhedron_destroy(NspPolyhedron *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_polyhedron_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_polyhedron_info(NspPolyhedron *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLPOLYHEDRON) 
    {
      Sciprintf("Null Pointer Polyhedron \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_polyhedron_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_polyhedron_print(NspPolyhedron *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLPOLYHEDRON) 
    {
      Sciprintf("Null Pointer Polyhedron \n");
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
          nsp_polyhedron_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_polyhedron_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  if ( M->obj->Mface != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mface),indent+2,"Mface",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mcolor != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcolor),indent+2,"Mcolor",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mback_color != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mback_color),indent+2,"Mback_color",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_polyhedron_latex(NspPolyhedron *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_polyhedron_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  if ( M->obj->Mface != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mface),indent+2,"Mface",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mcolor != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcolor),indent+2,"Mcolor",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mback_color != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mback_color),indent+2,"Mback_color",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Polyhedron objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPolyhedron   *nsp_polyhedron_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_polyhedron_id) == TRUE ) return ((NspPolyhedron *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_polyhedron));
  return NULL;
}

int IsPolyhedronObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_polyhedron_id);
}

int IsPolyhedron(NspObject *O)
{
  return nsp_object_type(O,nsp_type_polyhedron_id);
}

NspPolyhedron  *GetPolyhedronCopy(Stack stack, int i)
{
  if (  GetPolyhedron(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPolyhedron  *GetPolyhedron(Stack stack, int i)
{
  NspPolyhedron *M;
  if (( M = nsp_polyhedron_object(NthObj(i))) == NULLPOLYHEDRON)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspPolyhedron *nsp_polyhedron_create_void(char *name,NspTypeBase *type)
{
 NspPolyhedron *H  = (type == NULL) ? new_polyhedron() : type->new();
 if ( H ==  NULLPOLYHEDRON)
  {
   Sciprintf("No more memory\n");
   return NULLPOLYHEDRON;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLPOLYHEDRON;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_polyhedron_create_partial(NspPolyhedron *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_polyhedron)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  H->obj->Mcoord_l = NULL;
  H->obj->Mface = NULLMAT;
  H->obj->Mcolor = NULLMAT;
  H->obj->Mback_color = NULLMAT;
  H->obj->mesh = TRUE;
  H->obj->pos = NULL; H->obj->pos_length = 0; 
  return OK;
}

int nsp_polyhedron_check_values(NspPolyhedron *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->Mface == NULLMAT) 
    {
       if (( H->obj->Mface = nsp_matrix_create("Mface",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->Mcolor == NULLMAT) 
    {
       if (( H->obj->Mcolor = nsp_matrix_create("Mcolor",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->Mback_color == NULLMAT) 
    {
       if (( H->obj->Mback_color = nsp_matrix_create("Mback_color",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspPolyhedron *nsp_polyhedron_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,NspMatrix* Mface,NspMatrix* Mcolor,NspMatrix* Mback_color,gboolean mesh,int* pos, int pos_length,NspTypeBase *type)
{
 NspPolyhedron *H  = nsp_polyhedron_create_void(name,type);
 if ( H ==  NULLPOLYHEDRON) return NULLPOLYHEDRON;
  if ( nsp_polyhedron_create_partial(H) == FAIL) return NULLPOLYHEDRON;
  H->obj->Mcoord= Mcoord;
  H->obj->Mcoord_l = Mcoord_l;
  H->obj->Mface= Mface;
  H->obj->Mcolor= Mcolor;
  H->obj->Mback_color= Mback_color;
  H->obj->mesh=mesh;
  H->obj->pos = pos;
  H->obj->pos_length = pos_length;
 if ( nsp_polyhedron_check_values(H) == FAIL) return NULLPOLYHEDRON;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspPolyhedron *nsp_polyhedron_copy_partial(NspPolyhedron *H,NspPolyhedron *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspPolyhedron *nsp_polyhedron_copy(NspPolyhedron *self)
{
  NspPolyhedron *H  =nsp_polyhedron_create_void(NVOID,(NspTypeBase *) nsp_type_polyhedron);
  if ( H ==  NULLPOLYHEDRON) return NULLPOLYHEDRON;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYHEDRON;
  if ( nsp_polyhedron_copy_partial(H,self)== NULL) return NULLPOLYHEDRON;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspPolyhedron *nsp_polyhedron_full_copy_partial(NspPolyhedron *H,NspPolyhedron *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_polyhedron))) == NULL) return NULLPOLYHEDRON;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  H->obj->Mcoord_l = self->obj->Mcoord_l;
  if ( self->obj->Mface == NULL )
    { H->obj->Mface = NULL;}
  else
    {
      if ((H->obj->Mface = (NspMatrix *) nsp_object_copy_and_name("Mface",NSP_OBJECT(self->obj->Mface))) == NULLMAT) return NULL;
    }
  if ( self->obj->Mcolor == NULL )
    { H->obj->Mcolor = NULL;}
  else
    {
      if ((H->obj->Mcolor = (NspMatrix *) nsp_object_copy_and_name("Mcolor",NSP_OBJECT(self->obj->Mcolor))) == NULLMAT) return NULL;
    }
  if ( self->obj->Mback_color == NULL )
    { H->obj->Mback_color = NULL;}
  else
    {
      if ((H->obj->Mback_color = (NspMatrix *) nsp_object_copy_and_name("Mback_color",NSP_OBJECT(self->obj->Mback_color))) == NULLMAT) return NULL;
    }
  H->obj->mesh=self->obj->mesh;
  if ((H->obj->pos = malloc(self->obj->pos_length*sizeof(int)))== NULL) return NULL;
  H->obj->pos_length = self->obj->pos_length;
  memcpy(H->obj->pos,self->obj->pos,self->obj->pos_length*sizeof(int));
  return H;
}

NspPolyhedron *nsp_polyhedron_full_copy(NspPolyhedron *self)
{
  NspPolyhedron *H  =nsp_polyhedron_create_void(NVOID,(NspTypeBase *) nsp_type_polyhedron);
  if ( H ==  NULLPOLYHEDRON) return NULLPOLYHEDRON;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYHEDRON;
  if ( nsp_polyhedron_full_copy_partial(H,self)== NULL) return NULLPOLYHEDRON;

#line 69 "codegen/polyhedron.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyhedron(H)== FAIL) return NULL; 

#line 625 "polyhedron.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Polyhedron
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_polyhedron_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyhedron *H;
  CheckStdRhs(0,0);
  /* want to be sure that type polyhedron is initialized */
  nsp_type_polyhedron = new_type_polyhedron(T_BASE);
  if(( H = nsp_polyhedron_create_void(NVOID,(NspTypeBase *) nsp_type_polyhedron)) == NULLPOLYHEDRON) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_polyhedron_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_polyhedron_check_values(H) == FAIL) return RET_BUG;

#line 69 "codegen/polyhedron.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyhedron(H)== FAIL) return RET_BUG; 

#line 650 "polyhedron.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_polyhedron_full_copy(NspPolyhedron *self,Stack stack,int rhs,int opt,int lhs)
{
  NspPolyhedron *ret;

  ret = nsp_polyhedron_full_copy(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods polyhedron_methods[] = {
  {"full_copy",(nsp_method *) _wrap_nsp_polyhedron_full_copy},
  { NULL, NULL}
};

static NspMethods *polyhedron_get_methods(void) { return polyhedron_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_polyhedron_get_Mcoord(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyhedron *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyhedron_get_obj_Mcoord(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyhedron *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_polyhedron_set_Mcoord(void *self, char *attr, NspObject *O)
{
  NspMatrix *Mcoord;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyhedron *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspPolyhedron *) self)->obj->Mcoord);
  ((NspPolyhedron *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static NspObject *_wrap_polyhedron_get_Mface(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyhedron *) self)->obj->Mface;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyhedron_get_obj_Mface(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyhedron *) self)->obj->Mface);
  return (NspObject *) ret;
}

static int _wrap_polyhedron_set_Mface(void *self, char *attr, NspObject *O)
{
  NspMatrix *Mface;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mface = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyhedron *) self)->obj->Mface != NULL ) 
    nsp_matrix_destroy(((NspPolyhedron *) self)->obj->Mface);
  ((NspPolyhedron *) self)->obj->Mface= Mface;
  return OK;
}

static NspObject *_wrap_polyhedron_get_Mcolor(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyhedron *) self)->obj->Mcolor;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyhedron_get_obj_Mcolor(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyhedron *) self)->obj->Mcolor);
  return (NspObject *) ret;
}

static int _wrap_polyhedron_set_Mcolor(void *self, char *attr, NspObject *O)
{
  NspMatrix *Mcolor;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcolor = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyhedron *) self)->obj->Mcolor != NULL ) 
    nsp_matrix_destroy(((NspPolyhedron *) self)->obj->Mcolor);
  ((NspPolyhedron *) self)->obj->Mcolor= Mcolor;
  return OK;
}

static NspObject *_wrap_polyhedron_get_Mback_color(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyhedron *) self)->obj->Mback_color;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyhedron_get_obj_Mback_color(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyhedron *) self)->obj->Mback_color);
  return (NspObject *) ret;
}

static int _wrap_polyhedron_set_Mback_color(void *self, char *attr, NspObject *O)
{
  NspMatrix *Mback_color;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mback_color = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyhedron *) self)->obj->Mback_color != NULL ) 
    nsp_matrix_destroy(((NspPolyhedron *) self)->obj->Mback_color);
  ((NspPolyhedron *) self)->obj->Mback_color= Mback_color;
  return OK;
}

static NspObject *_wrap_polyhedron_get_mesh(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspPolyhedron *) self)->obj->mesh;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_polyhedron_set_mesh(void *self, char *attr, NspObject *O)
{
  int mesh;

  if ( BoolScalar(O,&mesh) == FAIL) return FAIL;
  ((NspPolyhedron *) self)->obj->mesh= mesh;
  return OK;
}

static AttrTab polyhedron_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_polyhedron_get_Mcoord, (attr_set_function *)_wrap_polyhedron_set_Mcoord,(attr_get_object_function *)_wrap_polyhedron_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { "Mface", (attr_get_function *)_wrap_polyhedron_get_Mface, (attr_set_function *)_wrap_polyhedron_set_Mface,(attr_get_object_function *)_wrap_polyhedron_get_obj_Mface, (attr_set_object_function *)int_set_object_failed },
  { "Mcolor", (attr_get_function *)_wrap_polyhedron_get_Mcolor, (attr_set_function *)_wrap_polyhedron_set_Mcolor,(attr_get_object_function *)_wrap_polyhedron_get_obj_Mcolor, (attr_set_object_function *)int_set_object_failed },
  { "Mback_color", (attr_get_function *)_wrap_polyhedron_get_Mback_color, (attr_set_function *)_wrap_polyhedron_set_Mback_color,(attr_get_object_function *)_wrap_polyhedron_get_obj_Mback_color, (attr_set_object_function *)int_set_object_failed },
  { "mesh", (attr_get_function *)_wrap_polyhedron_get_mesh, (attr_set_function *)_wrap_polyhedron_set_mesh,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 79 "codegen/polyhedron.override"
int _wrap_polyhedron_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 835 "polyhedron.c"


#line 122 "codegen/polyhedron.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_polyhedron(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 847 "polyhedron.c"


#line 132 "codegen/polyhedron.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_polyhedron(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 859 "polyhedron.c"


#line 142 "codegen/polyhedron.override"

int _wrap_nsp_surf_to_coords(Stack stack, int rhs, int opt, int lhs) /* surf_to_coord */
{
  int_types T[] = {realmat,realmat,realmat,t_end};
  NspMatrix *x, *y, *z, *ret;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &z) == FAIL) return RET_BUG;
  CheckDimProp(NspFname(stack),1,3, x->mn != z->m);
  CheckDimProp(NspFname(stack),2,3, y->mn != z->n);
  ret = nsp_surf_to_coords(NVOID,x, y, z);
  if ( ret == NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 877 "polyhedron.c"


#line 158 "codegen/polyhedron.override"

int _wrap_nsp_surf_to_faces(Stack stack, int rhs, int opt, int lhs) /* surf_to_face */
{
  int_types T[] = {realmat,realmat,t_end};
  NspMatrix *x, *y, *ret;
  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  ret = nsp_surf_to_faces(NVOID,x, y);
  if ( ret == NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 893 "polyhedron.c"


#line 172 "codegen/polyhedron.override"

int _wrap_nsp_facets_to_faces(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {mat, mat, mat, mat,t_end};
  NspMatrix *x, *y, *z, *colors, *retc,*retf;
  CheckLhs(0,2);
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &z, &colors) == FAIL) return RET_BUG;
  if ( nsp_facets_to_faces(x, y, z, colors,&retc,&retf)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(retc));
  if ( lhs >= 2 ) 
    {
      MoveObj(stack,2,NSP_OBJECT(retf));
    }
  else
    {
      nsp_matrix_destroy(retf);
    }
  return Max(lhs,0);
}

#line 917 "polyhedron.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Polyhedron_func[]={
  {"polyhedron_attach", _wrap_polyhedron_attach},
  {"extractelts_polyhedron", _wrap_nsp_extractelts_polyhedron},
  {"setrowscols_polyhedron", _wrap_nsp_setrowscols_polyhedron},
  {"surf_to_coords", _wrap_nsp_surf_to_coords},
  {"surf_to_faces", _wrap_nsp_surf_to_faces},
  {"facets_to_faces", _wrap_nsp_facets_to_faces},
  { "polyhedron_create", int_polyhedron_create},
  { NULL, NULL}
};

/* call ith function in the Polyhedron interface */

int Polyhedron_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Polyhedron_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Polyhedron_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Polyhedron_func[i].name;
  *f = Polyhedron_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Polyhedron_register_classes(NspObject *d)
{

#line 39 "codegen/polyhedron.override"

Init portion 


#line 961 "polyhedron.c"
  nspgobject_register_class(d, "Polyhedron", Polyhedron, &NspPolyhedron_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 194 "codegen/polyhedron.override"

/* function called when draw is needed 
 * data can be NULL and when non null 
 * it gives the face to draw for the polyhedron 
 */

static void nsp_draw_polyhedron(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int face; 
  if ( Obj->obj->hidden == TRUE ) return ;
  nsp_check_polyhedron((NspPolyhedron *) Obj);
#ifdef  WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* if we are using OpenGl we make a full draw of 
       * object and return 
       */
      draw_polyhedron_ogl(Xgc,Obj);
      nsp_ogl_set_2dview(Xgc); 
      return; 
    }
#endif 
  if ( data != NULL) 
    {
      face = *((int *) data);
      draw_polyhedron_face(Xgc,Obj,face);
    }
  else 
    {
      int i;
      /* draw all the faces: this is not really used  
       * since the face order is computed and sequenced in upper object.
       */
      for ( i= 0 ; i < ((NspPolyhedron*) Obj)->obj->Mface->n ; i++) 
	draw_polyhedron_face(Xgc,Obj,i);
    }
}


static void nsp_translate_polyhedron(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_polyhedron(BCG *Xgc,NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_polyhedron(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute the bounds of the polyhedron: 
 * note that for 3dobjets this is of length 6 
 */

extern void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) ;

static void nsp_getbounds_polyhedron(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  int i;
  /* this should be stored in a cache and recomputed when necessary 
   *
   */
  nsp_polyhedron *Q= ((NspPolyhedron *) Obj)->obj;
  nsp_check_polyhedron((NspPolyhedron *) Obj);
  if ( Q->Mcoord->mn == 0) 
    {
      bounds[0]= bounds[1] = bounds[2]= bounds[3]= bounds[4]=bounds[5]= 0;
      return;
    }
  for ( i = 0 ; i < Q->Mcoord->m ; i++) 
    nsp_gr_bounds_min_max(Q->Mcoord->n,Q->Mcoord->R+i,3,&bounds[2*i],&bounds[2*i+1]);
  return;
}

void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) 
{
  int i,i1;
  double amin= A[0], amax=A[0];
  /* look for the first non Nan component */
  i = 0; i1 = 1;
  while ( i1 <= n && ISNAN(A[i]) ) { i += incr; i1++; }
  if ( i1 <= n )
    {
      /* init with the first non Nan component then do the usual loop */
      amin = amax = A[i];
      i1++; i+=incr;
      for (  ; i1 <= n ; i += incr, i1++ )
	{
	  if ( A[i] < amin )
	    {
	      amin = A[i];
	    }
	  else if ( A[i] > amax )
	    {
	      amax = A[i];
	    }
	}
    }
  *Amax = amax; *Amin = amin;
  return;
}

/* checks that polyhedron is OK:
 * converts matrices data to int 
 * allocated extra matrices 
 */

int nsp_check_polyhedron( NspPolyhedron *P)
{
  nsp_polyhedron *Q = P->obj;
  int Q_nb_faces = Q->Mface->n;
  int Q_nb_coords = Q->Mcoord->n;

  if ( Q->Mcoord->m != 3 ) 
    {
      Scierror("Error: bad coord for polyhedron, first dimension should be 3\n");
      return FAIL;
    }
  if ( Q->Mface->m < 3 ) 
    {
      Scierror("Error: bad face for polyhedron, first dimension should be >= 3\n");
      return FAIL;
    }
  if ( Q->Mcolor->mn !=  Q_nb_faces   && Q->Mcolor->mn != 1 ) 
    {
      Scierror("Error: bad color size for polyhedron, expecting 1 or %d\n", Q_nb_faces);
      return FAIL;
    }
  
  if ( Q->Mback_color->mn  !=  Q_nb_faces  && Q->Mback_color->mn != 1 ) 
    {
      Scierror("Error: bad back_color size for polyhedron, expecting 1 or %d\n", Q_nb_faces);
      return FAIL;
    }

  Q->Mface = Mat2int(Q->Mface);
  Q->Mback_color = Mat2int(Q->Mback_color);
  Q->Mcolor = Mat2int(Q->Mcolor);

  /* create extra data for qpos declared int* 
   * Q->pos id only usefull for non opengl driver 
   */
  if ( Q->pos == NULL) Q->pos = malloc( Q_nb_coords * sizeof(int));
  Q->pos_length = Q_nb_coords;

  /* create extra data for Mcoord_l declared void* */
  if ( Q->Mcoord_l == NULL) 
    {
      Q->Mcoord_l = nsp_matrix_create("local",'r',Q->Mcoord->m, Q->Mcoord->n);
    }
  return OK;
}


/* draw one face of a polyhedron 
 * Author: Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>
 */

static void draw_polyhedron_face(BCG *Xgc,NspGraphic *Ob, int j)
{
  nsp_polyhedron *Q = ((NspPolyhedron *) Ob)->obj;
  int i, np=1, m;
  int x[6], y[6];   /* a changer */
  int numpt, *current_vertex, color;

  /* int Q_nb_coords = Q->Mcoord->n; */
  double * Q_coord = ((NspMatrix *) Q->Mcoord_l)->R;
  int Q_nb_vertices_per_face = Q->Mface->m;
  /* int Q_nb_faces = Q->Mface->n; */
  int * Q_face = Q->Mface->I;
  int Q_nb_colors = Q->Mcolor->mn;
  int * Q_color =  Q->Mcolor->I;
  int Q_nb_back_colors = Q->Mback_color->mn ;
  int * Q_back_color =  Q->Mback_color->I;
  
  int foreground_color = 1; /* XX should be shared */

  m = Q_nb_vertices_per_face;
  current_vertex = &(Q_face[m*j]);
  for (i = 0 ; i < m ; i++)
    {
      numpt = current_vertex[i]-1;
      x[i] = XScale(Q_coord[3*numpt]);
      y[i] = YScale(Q_coord[3*numpt+1]);
    }
  
  if ( nsp_obj3d_orientation(x, y, m) == -1 )  /* le rep�re de la cam�ra est indirect ! */
    if ( Q_nb_colors == 1 )
      color = Q_color[0];
    else
      color = Q_color[j];
  else       /* orientation < 0 =>  back color is used */
    if ( Q_nb_back_colors == 1 )
      color = Q_back_color[0];
    else
      color = Q_back_color[j];
	    
  if ( ! Q->mesh )  /* le contour du polygone ne doit pas apparaitre */
    color = -color; 

  /* color = 0;  permet de voir uniquement le maillage */
  /* 
   *  x, y : polygone(s) coordinates, nr : number of sides
   *  np : number of polygone(s) =1 here
   */
  Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
  Xgc->graphic_engine->fillpolylines(Xgc, x, y, &color, np, m);
}

static void draw_polyhedron_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  nsp_polyhedron *Q = ((NspPolyhedron *) Ob)->obj;
  int i,j, m;
  double x[6], y[6], z[6];   /* a changer */
  int numpt, *current_vertex, color;

  /* int Q_nb_coords = Q->Mcoord->n;  */
  double * Q_coord = Q->Mcoord->R;
  int Q_nb_vertices_per_face = Q->Mface->m;
  int Q_nb_faces = Q->Mface->n;
  int * Q_face = Q->Mface->I;
  int Q_nb_colors = Q->Mcolor->mn;
  int * Q_color =  Q->Mcolor->I;
  /*   int Q_nb_back_colors = Q->Mback_color->mn ; */
  /* int * Q_back_color =  Q->Mback_color->I;*/
  
  int foreground_color = 1; /* should be shared */
  
  m = Q_nb_vertices_per_face;

  for ( j = 0 ; j < Q_nb_faces ; j++ )
    {
      current_vertex = &(Q_face[m*j]);
      for (i = 0 ; i < m ; i++)
	{
	  numpt = current_vertex[i]-1;
	  x[i] = Q_coord[3*numpt];
	  y[i] = Q_coord[3*numpt+1];
	  z[i] = Q_coord[3*numpt+2];
	}
      
      color = ( Q_nb_colors == 1 ) ? Q_color[0]: Q_color[j];
      /* le contour du polygone ne doit pas apparaitre */
      if ( ! Q->mesh ) 	color = -color; 
      /* color = 0;  permet de voir uniquement le maillage */
      /* draw one face */
      Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
      fillpolylines3D(Xgc, x, y, z, &color, 1 , m);
    }
#endif
}

/* 
 * this function is to be called after a transformation which have filled 
 * Q->Mcoord_l->R
 * Author: Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>
 */

static void zmean_faces_for_Polyhedron(void *Obj, double z[], HFstruct HF[], int *n, int k)
{
  nsp_polyhedron *Q = ((NspPolyhedron *) Obj)->obj;
  int m, i, j, *current_vertex;
  VisionPos pos_face, pos_vertex;
  double coef, zmean;

  /* int Q_nb_coords = Q->Mcoord->n;  */
  double * Q_coord = ((NspMatrix *) Q->Mcoord_l)->R;
  int Q_nb_vertices_per_face = Q->Mface->m;
  int Q_nb_faces = Q->Mface->n;
  int * Q_face = Q->Mface->I;

  nsp_check_polyhedron((NspPolyhedron *) Obj);
  
  m = Q_nb_vertices_per_face; 
  coef = 1.0/m;
  current_vertex = Q_face;
  for ( j = 0 ; j < Q_nb_faces ; j++ )
    {
      zmean = 0.0; pos_face = OUT_XY;
      /* Une face rentre dans le calcul des faces cach�es si :
       *     1/ aucun point n'est en position OUT_Z
       *     2/ au moins un point est IN (les autres �tant alors soit
       *        IN soit OUT_XY)
       * On pourra par la suite d�tailler un peu plus car si tous les
       * sommets de la face sont IN aucun clippling n'est � effectuer.
       * Faire ce clipping moi-m�me ?
       */
      for ( i = 0 ; i < m ; i++ )
	{
	  zmean += Q_coord[3*(*current_vertex-1)+2];
	  pos_vertex = Q->pos[*current_vertex-1];
	  if (pos_vertex == OUT_Z)
	    pos_face = OUT_Z;
	  else if (pos_vertex == VIN && pos_face != OUT_Z)
	    pos_face = VIN;
	  current_vertex++;
	}
      if (pos_face == VIN) 
	{
	  z[*n] = coef*zmean;
	  HF[*n].num_obj = k;
	  HF[*n].num_in_obj = j;
	  (*n)++; 
	}
    }
}


/*
 * requested method for 3d objects.
 */

static void nsp_polyhedron_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim)
{
  nsp_polyhedron *Q= ((NspPolyhedron *) Obj)->obj;
  apply_transforms(Xgc,((NspMatrix *) Q->Mcoord_l)->R,Q->Mcoord->R,Q->pos, lim, Q->Mcoord->n);
  zmean_faces_for_Polyhedron(Obj, z,  HF, n, k);
}

/* requested method for 3d objects.
 *
 */

static int nsp_polyhedron_n_faces(BCG *Xgc,NspGraphic *Obj)
{
  return ((NspPolyhedron *) Obj)->obj->Mface->n;
}


/* Utilities 
 * 
 */

static NspMatrix *nsp_surf_to_coords(const char *name,NspMatrix *x,NspMatrix *y, NspMatrix *z)
{
  int i,j;
  NspMatrix *coord;
  if ((coord = nsp_matrix_create(name,'r',3,z->mn))==NULL) return NULL;
  for (i= 0 ; i < z->mn;  i++)
    {
      coord->R[3*i+2]= z->R[i];
    }
  for (j = 0 ; j < y->mn ; j++) 
    for (i= 0 ; i < x->mn;  i++)
      coord->R[3*(i+j*(x->mn))]= x->R[i];
  for (j = 0 ; j < y->mn ; j++) 
    for (i= 0 ; i < x->mn;  i++)
      coord->R[3*(i+j*(x->mn))+1]= y->R[j];
  return coord;
}

static NspMatrix *nsp_surf_to_faces(const char *name,NspMatrix *x,NspMatrix *y)  
{
  NspMatrix *face;
  int j,i,nface;
  if ((face = nsp_matrix_create(NVOID,'r',4,(x->mn-1)*(y->mn-1)))==NULL) return NULL;
  nface = 0;
  for (j = 0 ; j < y->mn -1 ; j++) 
    {
      for ( i = 0 ; i < x->mn-1 ; i++)
	{
	  face->R[ nface*4]   = i+j*(x->mn)+1 ; 
	  face->R[ nface*4+1] = i+1+j*(x->mn)+1 ; 
	  face->R[ nface*4+2] = (i+1)+(j+1)*(x->mn)+1 ; 
	  face->R[ nface*4+3] = (i)+(j+1)*(x->mn)+1 ; 
	  nface++;
	}
    }
  return face;
}

/* 
 *
 *
 */

static int nsp_facets_to_faces(NspMatrix *x,NspMatrix *y,NspMatrix *z,NspMatrix *colors,
			       NspMatrix **Cr,NspMatrix **Fr)
{
  index_vector index={0};
  int i,k;
  NspMatrix *C,*Fs,*Fc,*Fsc,*Index;
  NspBMatrix *B;
  if ((C = nsp_matrix_create(NVOID,'r',x->mn,3))==NULL) return FAIL;
  for ( i=0 ; i < C->m; i++)
    {
      C->R[i]= x->R[i];
      C->R[i+C->m]= y->R[i];
      C->R[i+2*C->m]= z->R[i];
    }
  /* sort the matrix 
   * [Cs,ks]= sort(C,'ldr','i');
   */
  nsp_matrix_lexical_row_sort(C,&Index,TRUE,'i','d');
  /* create the facets 
   * kp(ks)=1:m*n;
   * Fs = matrix(kp,m,n); // les faces au sens polyhedron 
   */
  if ((Fs = nsp_matrix_create(NVOID,'r',x->m,x->n))==NULL) return FAIL;
  Fs->convert = 'i';
  for ( i=0 ; i < Fs->mn; i++)
    {
      Fs->I[Index->I[i]-1]= i+1;
    }
  
  // on enleve les redondances 
  // mise a jour de Fs 
  if ((Fc = nsp_matrix_create(NVOID,'r',x->mn,1))==NULL) return FAIL;
  Fc->convert = 'i';
  for ( i=0 ; i < Fc->mn; i++)
    {
      Fc->I[i]= 1;
    }
  k=1;
  for ( i = 1 ; i < x->mn ; i++)
    {
      if ( C->R[i] == C->R[i-1] && C->R[i+C->m] == C->R[i-1+C->m] && C->R[i+2*C->m] == C->R[i-1+2*C->m]) 
	{
	  Fc->I[i]=k;
	}
      else
	{
	  k++; Fc->I[i]=k;
	}
    }
  if ((Fsc = nsp_matrix_create(NVOID,'r',x->m,x->n))==NULL) return FAIL;
  Fsc->convert = 'i';
  for ( i=0 ; i < Fsc->mn; i++)
    Fsc->I[i]= Fc->I[Fs->I[i]-1];
  /* remove duplicates in C */
  if (( B = nsp_bmatrix_create(NVOID,Fc->mn,1)) ==NULL) return FAIL;
  B->B[0]= FALSE;
  for ( i=1 ; i < B->mn; i++)
    B->B[i] =  Fc->I[i] == Fc->I[i-1]; 
  
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(NSP_OBJECT(B),&index) == FAIL) return FAIL;
  if ( nsp_matint_delete_rows(NSP_OBJECT(C),&index)==FAIL) return FAIL;

  nsp_matrix_destroy(Fc);
  nsp_matrix_destroy(Fs); 
  nsp_matrix_destroy(Index); 
  nsp_bmatrix_destroy(B); 
  *Cr=C;
  *Fr=Fsc;
  return OK;

}








#line 1429 "polyhedron.c"
