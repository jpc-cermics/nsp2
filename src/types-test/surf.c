/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/surf.override"
#include <nsp/surf.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_surf(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_surf(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_surf(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_surf(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_surf(BCG *Xgc,NspGraphic *o,double *bounds);

extern void nsp_figure_force_redraw(nsp_figure *F);

#line 26 "surf.c"

/* ----------- NspSurf ----------- */


#define  NspSurf_Private 
#include <nsp/object.h>
#include <nsp/surf.h>
#include <nsp/interf.h>

/* 
 * NspSurf inherits from Graphic 
 */

int nsp_type_surf_id=0;
NspTypeNspSurf *nsp_type_surf=NULL;

/*
 * Type object for NspSurf 
 * all the instance of NspTypeNspSurf share the same id. 
 * nsp_type_surf: is an instance of NspTypeNspSurf 
 *    used for objects of NspSurf type (i.e built with new_surf) 
 * other instances are used for derived classes 
 */
NspTypeNspSurf *new_type_surf(type_mode mode)
{
  NspTypeNspSurf *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_surf != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_surf;
    }
  if ((type =  malloc(sizeof(NspTypeNspSurf))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = surf_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = surf_get_methods; 
  type->new = (new_func *) new_surf;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for surf */ 

  top->pr = (print_func *) nsp_surf_print;                  
  top->dealloc = (dealloc_func *) nsp_surf_destroy;
  top->copy  =  (copy_func *) nsp_surf_copy;                 
  top->size  = (size_func *) nsp_surf_size;                
  top->s_type =  (s_type_func *) nsp_surf_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_surf_type_short_string;
  top->info = (info_func *) nsp_surf_info ;                  
  /* top->is_true = (is_true_func  *) nsp_surf_is_true; */
  /* top->loop =(loop_func *) nsp_surf_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_surf_object;
  top->eq  = (eq_func *) nsp_surf_eq;
  top->neq  = (eq_func *) nsp_surf_neq;
  top->save  = (save_func *) nsp_surf_xdr_save;
  top->load  = (load_func *) nsp_surf_xdr_load;
  top->create = (create_func*) int_surf_create;
  top->latex = (print_func *) nsp_surf_latex;
  
  /* specific methods for surf */
      
  type->init = (init_func *) init_surf;

#line 24 "codegen/surf.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeNspGraphic *) type->surtype)->draw = nsp_draw_surf;
  ((NspTypeNspGraphic *) type->surtype)->translate =nsp_translate_surf ;
  ((NspTypeNspGraphic *) type->surtype)->rotate =nsp_rotate_surf  ;
  ((NspTypeNspGraphic *) type->surtype)->scale =nsp_scale_surf  ;
  ((NspTypeNspGraphic *) type->surtype)->bounds =nsp_getbounds_surf  ;
  ((NspTypeNspGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_surf_full_copy ;
  /* next method are defined in NspGraphic and need not be chnaged here for Surf */
  /* ((NspTypeNspGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeNspGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 111 "surf.c"
  /* 
   * NspSurf interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_surf_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeNspSurf called nsp_type_surf
       */
      type->id =  nsp_type_surf_id = nsp_new_type_id();
      nsp_type_surf = type;
      if ( nsp_register_type(nsp_type_surf) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_surf(mode);
    }
  else 
    {
       type->id = nsp_type_surf_id;
       return type;
    }
}

/*
 * initialize NspSurf instances 
 * locally and by calling initializer on parent class 
 */

static int init_surf(NspSurf *Obj,NspTypeNspSurf *type)
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
 * new instance of NspSurf 
 */

NspSurf *new_surf() 
{
  NspSurf *loc; 
  /* type must exists */
  nsp_type_surf = new_type_surf(T_BASE);
  if ( (loc = malloc(sizeof(NspSurf)))== NULLSURF) return loc;
  /* initialize object */
  if ( init_surf(loc,nsp_type_surf) == FAIL) return NULLSURF;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspSurf 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_surf_size(NspSurf *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char surf_type_name[]="NspSurf";
static char surf_short_type_name[]="surf";

static char *nsp_surf_type_as_string(void)
{
  return(surf_type_name);
}

static char *nsp_surf_type_short_string(NspObject *v)
{
  return(surf_short_type_name);
}

/*
 * A == B 
 */

static int nsp_surf_eq(NspSurf *A, NspObject *B)
{
  NspSurf *loc = (NspSurf *) B;
  if ( check_cast(B,nsp_type_surf_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->z)->type->eq(A->obj->z,loc->obj->z) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->colors)->type->eq(A->obj->colors,loc->obj->colors) == FALSE ) return FALSE;
  if ( A->obj->mesh != loc->obj->mesh) return FALSE;
  if ( A->obj->zcolor != loc->obj->zcolor) return FALSE;
  if ( A->obj->mesh_color != loc->obj->mesh_color) return FALSE;
  if ( A->obj->face_color != loc->obj->face_color) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_surf_neq(NspSurf *A, NspObject *B)
{
  return ( nsp_surf_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_surf_xdr_save(XDR *xdrs, NspSurf *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_surf)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->z)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colors)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mesh) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->zcolor) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mesh_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->face_color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspSurf  *nsp_surf_xdr_load_partial(XDR *xdrs, NspSurf *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_surf))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->z =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->colors =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mesh) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->zcolor) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mesh_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->face_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspSurf  *nsp_surf_xdr_load(XDR *xdrs)
{
  NspSurf *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSURF;
  if ((H  = nsp_surf_create_void(name,(NspTypeBase *) nsp_type_surf))== NULLSURF) return H;
  if ((H  = nsp_surf_xdr_load_partial(xdrs,H))== NULLSURF) return H;
  if ( nsp_surf_check_values(H) == FAIL) return NULLSURF;
#line 284 "surf.c"
  return H;
}

/*
 * delete 
 */

void nsp_surf_destroy_partial(NspSurf *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 298 "surf.c"
    nsp_matrix_destroy(H->obj->x);
    nsp_matrix_destroy(H->obj->y);
    nsp_matrix_destroy(H->obj->z);
    nsp_matrix_destroy(H->obj->colors);
    FREE(H->obj);
   }
}

void nsp_surf_destroy(NspSurf *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_surf_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_surf_info(NspSurf *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSURF) 
    {
      Sciprintf("Null Pointer NspSurf \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_surf_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_surf_print(NspSurf *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSURF) 
    {
      Sciprintf("Null Pointer NspSurf \n");
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
          nsp_surf_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_surf_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
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
  if ( M->obj->colors != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colors),indent+2,"colors",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"zcolor	= %s\n", ( M->obj->zcolor == TRUE) ? "T" : "F" );
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

int nsp_surf_latex(NspSurf *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_surf_type_short_string(NSP_OBJECT(M)));
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
  if ( M->obj->colors != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colors),indent+2,"colors",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"zcolor	= %s\n", ( M->obj->zcolor == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"mesh_color=%d\n",M->obj->mesh_color);
  Sciprintf1(indent+2,"face_color=%d\n",M->obj->face_color);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspSurf objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspSurf   *nsp_surf_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_surf_id) == TRUE ) return ((NspSurf *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_surf));
  return NULL;
}

int IsSurfObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_surf_id);
}

int IsSurf(NspObject *O)
{
  return nsp_object_type(O,nsp_type_surf_id);
}

NspSurf  *GetSurfCopy(Stack stack, int i)
{
  if (  GetSurf(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSurf  *GetSurf(Stack stack, int i)
{
  NspSurf *M;
  if (( M = nsp_surf_object(NthObj(i))) == NULLSURF)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspSurf *nsp_surf_create_void(char *name,NspTypeBase *type)
{
 NspSurf *H  = (type == NULL) ? new_surf() : type->new();
 if ( H ==  NULLSURF)
  {
   Sciprintf("No more memory\n");
   return NULLSURF;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSURF;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_surf_create_partial(NspSurf *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_surf)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->z = NULLMAT;
  H->obj->colors = NULLMAT;
  H->obj->mesh = TRUE;
  H->obj->zcolor = TRUE;
  H->obj->mesh_color = -1;
  H->obj->face_color = -1;
  return OK;
}

int nsp_surf_check_values(NspSurf *H)
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
  if ( H->obj->colors == NULLMAT) 
    {
       if (( H->obj->colors = nsp_matrix_create("colors",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspSurf *nsp_surf_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,NspMatrix* colors,gboolean mesh,gboolean zcolor,int mesh_color,int face_color,NspTypeBase *type)
{
 NspSurf *H  = nsp_surf_create_void(name,type);
 if ( H ==  NULLSURF) return NULLSURF;
  if ( nsp_surf_create_partial(H) == FAIL) return NULLSURF;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->z= z;
  H->obj->colors= colors;
  H->obj->mesh=mesh;
  H->obj->zcolor=zcolor;
  H->obj->mesh_color=mesh_color;
  H->obj->face_color=face_color;
 if ( nsp_surf_check_values(H) == FAIL) return NULLSURF;
 return H;
}


NspSurf *nsp_surf_create_default(char *name)
{
 NspSurf *H  = nsp_surf_create_void(name,NULL);
 if ( H ==  NULLSURF) return NULLSURF;
  if ( nsp_surf_create_partial(H) == FAIL) return NULLSURF;
 if ( nsp_surf_check_values(H) == FAIL) return NULLSURF;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspSurf *nsp_surf_copy_partial(NspSurf *H,NspSurf *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspSurf *nsp_surf_copy(NspSurf *self)
{
  NspSurf *H  =nsp_surf_create_void(NVOID,(NspTypeBase *) nsp_type_surf);
  if ( H ==  NULLSURF) return NULLSURF;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSURF;
  if ( nsp_surf_copy_partial(H,self)== NULL) return NULLSURF;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspSurf *nsp_surf_full_copy_partial(NspSurf *H,NspSurf *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_surf))) == NULL) return NULLSURF;
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
  if ( self->obj->colors == NULL )
    { H->obj->colors = NULL;}
  else
    {
      if ((H->obj->colors = (NspMatrix *) nsp_object_copy_and_name("colors",NSP_OBJECT(self->obj->colors))) == NULLMAT) return NULL;
    }
  H->obj->mesh=self->obj->mesh;
  H->obj->zcolor=self->obj->zcolor;
  H->obj->mesh_color=self->obj->mesh_color;
  H->obj->face_color=self->obj->face_color;
  return H;
}

NspSurf *nsp_surf_full_copy(NspSurf *self)
{
  NspSurf *H  =nsp_surf_create_void(NVOID,(NspTypeBase *) nsp_type_surf);
  if ( H ==  NULLSURF) return NULLSURF;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSURF;
  if ( nsp_surf_full_copy_partial(H,self)== NULL) return NULLSURF;
#line 607 "surf.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspSurf
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_surf_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSurf *H;
  CheckStdRhs(0,0);
  /* want to be sure that type surf is initialized */
  nsp_type_surf = new_type_surf(T_BASE);
  if(( H = nsp_surf_create_void(NVOID,(NspTypeBase *) nsp_type_surf)) == NULLSURF) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_surf_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_surf_check_values(H) == FAIL) return RET_BUG;
#line 627 "surf.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_surf_full_copy(NspSurf *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSurf *ret;

  ret = nsp_surf_full_copy(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods surf_methods[] = {
  {"full_copy",(nsp_method *) _wrap_nsp_surf_full_copy},
  { NULL, NULL}
};

static NspMethods *surf_get_methods(void) { return surf_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_surf_get_x(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspSurf *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_surf_get_obj_x(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSurf *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_surf_set_x(void *self, char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSurf *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspSurf *) self)->obj->x);
  ((NspSurf *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_surf_get_y(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspSurf *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_surf_get_obj_y(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSurf *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_surf_set_y(void *self, char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSurf *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspSurf *) self)->obj->y);
  ((NspSurf *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_surf_get_z(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspSurf *) self)->obj->z;
  return (NspObject *) ret;
}

static NspObject *_wrap_surf_get_obj_z(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSurf *) self)->obj->z);
  return (NspObject *) ret;
}

static int _wrap_surf_set_z(void *self, char *attr, NspObject *O)
{
  NspMatrix *z;

  if ( ! IsMat(O) ) return FAIL;
  if ((z = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSurf *) self)->obj->z != NULL ) 
    nsp_matrix_destroy(((NspSurf *) self)->obj->z);
  ((NspSurf *) self)->obj->z= z;
  return OK;
}

static NspObject *_wrap_surf_get_colors(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspSurf *) self)->obj->colors;
  return (NspObject *) ret;
}

static NspObject *_wrap_surf_get_obj_colors(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSurf *) self)->obj->colors);
  return (NspObject *) ret;
}

static int _wrap_surf_set_colors(void *self, char *attr, NspObject *O)
{
  NspMatrix *colors;

  if ( ! IsMat(O) ) return FAIL;
  if ((colors = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSurf *) self)->obj->colors != NULL ) 
    nsp_matrix_destroy(((NspSurf *) self)->obj->colors);
  ((NspSurf *) self)->obj->colors= colors;
  return OK;
}

static NspObject *_wrap_surf_get_mesh(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspSurf *) self)->obj->mesh;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_surf_set_mesh(void *self, char *attr, NspObject *O)
{
  int mesh;

  if ( BoolScalar(O,&mesh) == FAIL) return FAIL;
  ((NspSurf *) self)->obj->mesh= mesh;
  return OK;
}

static NspObject *_wrap_surf_get_zcolor(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspSurf *) self)->obj->zcolor;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_surf_set_zcolor(void *self, char *attr, NspObject *O)
{
  int zcolor;

  if ( BoolScalar(O,&zcolor) == FAIL) return FAIL;
  ((NspSurf *) self)->obj->zcolor= zcolor;
  return OK;
}

static NspObject *_wrap_surf_get_mesh_color(void *self,char *attr)
{
  int ret;

  ret = ((NspSurf *) self)->obj->mesh_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_surf_set_mesh_color(void *self, char *attr, NspObject *O)
{
  int mesh_color;

  if ( IntScalar(O,&mesh_color) == FAIL) return FAIL;
  ((NspSurf *) self)->obj->mesh_color= mesh_color;
  return OK;
}

static NspObject *_wrap_surf_get_face_color(void *self,char *attr)
{
  int ret;

  ret = ((NspSurf *) self)->obj->face_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_surf_set_face_color(void *self, char *attr, NspObject *O)
{
  int face_color;

  if ( IntScalar(O,&face_color) == FAIL) return FAIL;
  ((NspSurf *) self)->obj->face_color= face_color;
  return OK;
}

static AttrTab surf_attrs[] = {
  { "x", (attr_get_function *)_wrap_surf_get_x, (attr_set_function *)_wrap_surf_set_x,(attr_get_object_function *)_wrap_surf_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_surf_get_y, (attr_set_function *)_wrap_surf_set_y,(attr_get_object_function *)_wrap_surf_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "z", (attr_get_function *)_wrap_surf_get_z, (attr_set_function *)_wrap_surf_set_z,(attr_get_object_function *)_wrap_surf_get_obj_z, (attr_set_object_function *)int_set_object_failed },
  { "colors", (attr_get_function *)_wrap_surf_get_colors, (attr_set_function *)_wrap_surf_set_colors,(attr_get_object_function *)_wrap_surf_get_obj_colors, (attr_set_object_function *)int_set_object_failed },
  { "mesh", (attr_get_function *)_wrap_surf_get_mesh, (attr_set_function *)_wrap_surf_set_mesh,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "zcolor", (attr_get_function *)_wrap_surf_get_zcolor, (attr_set_function *)_wrap_surf_set_zcolor,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mesh_color", (attr_get_function *)_wrap_surf_get_mesh_color, (attr_set_function *)_wrap_surf_set_mesh_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "face_color", (attr_get_function *)_wrap_surf_get_face_color, (attr_set_function *)_wrap_surf_set_face_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 47 "codegen/surf.override"
int _wrap_surf_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}


#line 869 "surf.c"


#line 61 "codegen/surf.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_surf(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 881 "surf.c"


#line 71 "codegen/surf.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_surf(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 894 "surf.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Surf_func[]={
  {"surf_attach", _wrap_surf_attach},
  {"extractelts_surf", _wrap_nsp_extractelts_surf},
  {"setrowscols_surf", _wrap_nsp_setrowscols_surf},
  { "surf_create", int_surf_create},
  { NULL, NULL}
};

/* call ith function in the Surf interface */

int Surf_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Surf_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Surf_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Surf_func[i].name;
  *f = Surf_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Surf_register_classes(NspObject *d)
{

#line 19 "codegen/surf.override"

Init portion 


#line 935 "surf.c"
  nspgobject_register_class(d, "NspSurf", Surf, &NspNspSurf_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 82 "codegen/surf.override"

/* inserted verbatim at the end */

static void nsp_draw_surf(BCG *Xgc,NspGraphic *Obj, void *data)
{
  /* 
  char leg[]="X@Y@Z";
  int flag[]={1,2,4};
  double ebox[]={0,1,0,1,0,1};
  double theta = 35, alpha=45;
  NspSurf *P =(NspSurf*) Obj ;
  NspMatrix *x = P->obj->x;
  NspMatrix *y = P->obj->y;
  NspMatrix *z = P->obj->z;
  if ( Obj->obj->hidden == TRUE ) return ;
  / * be sure that object are in canonical form * /
  Mat2double(x);
  Mat2double(y);
  Mat2double(z);
  Mat2int(P->obj->colors);
  
  if ( x->mn == y->mn && x->mn == z->mn && x->mn != 1) 
    { 
      if ( P->obj->colors->mn == P->obj->z->n ) 
	{
	  nsp_plot_fac3d_2(Xgc,x->R,y->R,z->R,P->obj->colors->I,&z->m,&z->n,&theta,&alpha,leg,flag,ebox);
	}
      else if ( P->obj->colors->m == P->obj->z->m &&  P->obj->colors->n == P->obj->z->n ) 
	{
	  nsp_plot_fac3d_3(Xgc,x->R,y->R,z->R,P->obj->colors->I,&z->m,&z->n,&theta,&alpha,leg,flag,ebox);
	}
      else 
	{
	  / * here colors is unused * /
	  if ( P->obj->zcolor == TRUE ) 
	    nsp_plot_fac3d_1(Xgc,x->R,y->R,z->R,P->obj->colors->I,&z->m,&z->n,&theta,&alpha,leg,flag,ebox);
	  else 
	    nsp_plot_fac3d(Xgc,x->R,y->R,z->R,P->obj->colors->I,&z->m,&z->n,&theta,&alpha,leg,flag,ebox);
	}
    } 
  else 
    {
      if ( P->obj->zcolor == TRUE ) 
	nsp_plot3d_1(Xgc,P->obj->x->R,P->obj->y->R,P->obj->z->R,&P->obj->z->m,&P->obj->z->n,
		     &theta,&alpha,leg,flag,ebox);
      else
	nsp_plot3d  (Xgc,P->obj->x->R,P->obj->y->R,P->obj->z->R,&P->obj->z->m,&P->obj->z->n,
		     &theta,&alpha,leg,flag,ebox);
    }
  */
}

static void nsp_translate_surf(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_surf(BCG *Xgc,NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_surf(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of surf 
 *
 */

static void nsp_getbounds_surf(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  bounds[0]= bounds[1] = bounds[2]= bounds[3]=0;
  return;
}


#line 1020 "surf.c"
