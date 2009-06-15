/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/fec.override"
#include <nsp/axes.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
extern void nsp_figure_force_redraw(nsp_figure *F);
extern void PermutOfSort (const int *tab, int *perm);
extern void PaintTriangle (BCG *Xgc,const double *sx,const double *sy,const  double *fxy, 
			   const int *zxy, const double *zlevel,const int *fill);
extern void FindIntersection(const double *sx,const double *sy,const double *fxy,
			     double z,int inda, int indb,  int *xint, int *yint);

static void nsp_draw_fec(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_fec(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_fec(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_fec(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_fec(BCG *Xgc,NspGraphic *o,double *bounds);


static void draw_triangle(BCG *Xgc,const double *sx,const double *sy);

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 38 "fec.c"

/* ----------- NspFec ----------- */


#define  NspFec_Private 
#include <nsp/object.h>
#include <nsp/fec.h>
#include <nsp/interf.h>

/* 
 * NspFec inherits from Graphic 
 */

int nsp_type_fec_id=0;
NspTypeNspFec *nsp_type_fec=NULL;

/*
 * Type object for NspFec 
 * all the instance of NspTypeNspFec share the same id. 
 * nsp_type_fec: is an instance of NspTypeNspFec 
 *    used for objects of NspFec type (i.e built with new_fec) 
 * other instances are used for derived classes 
 */
NspTypeNspFec *new_type_fec(type_mode mode)
{
  NspTypeNspFec *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_fec != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_fec;
    }
  if ((type =  malloc(sizeof(NspTypeNspFec))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = fec_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = fec_get_methods; 
  type->new = (new_func *) new_fec;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for fec */ 

  top->pr = (print_func *) nsp_fec_print;                  
  top->dealloc = (dealloc_func *) nsp_fec_destroy;
  top->copy  =  (copy_func *) nsp_fec_copy;                 
  top->size  = (size_func *) nsp_fec_size;                
  top->s_type =  (s_type_func *) nsp_fec_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_fec_type_short_string;
  top->info = (info_func *) nsp_fec_info ;                  
  /* top->is_true = (is_true_func  *) nsp_fec_is_true; */
  /* top->loop =(loop_func *) nsp_fec_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_fec_object;
  top->eq  = (eq_func *) nsp_fec_eq;
  top->neq  = (eq_func *) nsp_fec_neq;
  top->save  = (save_func *) nsp_fec_xdr_save;
  top->load  = (load_func *) nsp_fec_xdr_load;
  top->create = (create_func*) int_fec_create;
  top->latex = (print_func *) nsp_fec_latex;
  
  /* specific methods for fec */
      
  type->init = (init_func *) init_fec;

#line 36 "codegen/fec.override"
  /* inserted verbatim in the type definition */
  ((NspTypeNspGraphic *) type->surtype)->draw = nsp_draw_fec;
  ((NspTypeNspGraphic *) type->surtype)->translate =nsp_translate_fec ;
  ((NspTypeNspGraphic *) type->surtype)->rotate =nsp_rotate_fec  ;
  ((NspTypeNspGraphic *) type->surtype)->scale =nsp_scale_fec  ;
  ((NspTypeNspGraphic *) type->surtype)->bounds =nsp_getbounds_fec  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Fec */
  /* ((NspTypeNspGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeNspGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 120 "fec.c"
  /* 
   * NspFec interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_fec_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeNspFec called nsp_type_fec
       */
      type->id =  nsp_type_fec_id = nsp_new_type_id();
      nsp_type_fec = type;
      if ( nsp_register_type(nsp_type_fec) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_fec(mode);
    }
  else 
    {
       type->id = nsp_type_fec_id;
       return type;
    }
}

/*
 * initialize NspFec instances 
 * locally and by calling initializer on parent class 
 */

static int init_fec(NspFec *Obj,NspTypeNspFec *type)
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
 * new instance of NspFec 
 */

NspFec *new_fec() 
{
  NspFec *loc; 
  /* type must exists */
  nsp_type_fec = new_type_fec(T_BASE);
  if ( (loc = malloc(sizeof(NspFec)))== NULLFEC) return loc;
  /* initialize object */
  if ( init_fec(loc,nsp_type_fec) == FAIL) return NULLFEC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspFec 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_fec_size(NspFec *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char fec_type_name[]="NspFec";
static char fec_short_type_name[]="fec";

static char *nsp_fec_type_as_string(void)
{
  return(fec_type_name);
}

static char *nsp_fec_type_short_string(NspObject *v)
{
  return(fec_short_type_name);
}

/*
 * A == B 
 */

static int nsp_fec_eq(NspFec *A, NspObject *B)
{
  NspFec *loc = (NspFec *) B;
  if ( check_cast(B,nsp_type_fec_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->triangles)->type->eq(A->obj->triangles,loc->obj->triangles) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->func)->type->eq(A->obj->func,loc->obj->func) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->colminmax)->type->eq(A->obj->colminmax,loc->obj->colminmax) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->zminmax)->type->eq(A->obj->zminmax,loc->obj->zminmax) == FALSE ) return FALSE;
  if ( A->obj->draw != loc->obj->draw) return FALSE;
  if ( NSP_OBJECT(A->obj->colout)->type->eq(A->obj->colout,loc->obj->colout) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_fec_neq(NspFec *A, NspObject *B)
{
  return ( nsp_fec_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_fec_xdr_save(XDR *xdrs, NspFec *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_fec)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->triangles)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->func)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colminmax)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->zminmax)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->draw) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colout)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspFec  *nsp_fec_xdr_load_partial(XDR *xdrs, NspFec *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_fec))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->triangles =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->func =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->colminmax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->zminmax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->draw) == FAIL) return NULL;
  if ((M->obj->colout =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspFec  *nsp_fec_xdr_load(XDR *xdrs)
{
  NspFec *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLFEC;
  if ((H  = nsp_fec_create_void(name,(NspTypeBase *) nsp_type_fec))== NULLFEC) return H;
  if ((H  = nsp_fec_xdr_load_partial(xdrs,H))== NULLFEC) return H;
  if ( nsp_fec_check_values(H) == FAIL) return NULLFEC;
#line 293 "fec.c"
  return H;
}

/*
 * delete 
 */

void nsp_fec_destroy_partial(NspFec *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 307 "fec.c"
    nsp_matrix_destroy(H->obj->x);
    nsp_matrix_destroy(H->obj->y);
    nsp_matrix_destroy(H->obj->triangles);
    nsp_matrix_destroy(H->obj->func);
    nsp_matrix_destroy(H->obj->colminmax);
    nsp_matrix_destroy(H->obj->zminmax);
    nsp_matrix_destroy(H->obj->colout);
    FREE(H->obj);
   }
}

void nsp_fec_destroy(NspFec *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_fec_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_fec_info(NspFec *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLFEC) 
    {
      Sciprintf("Null Pointer NspFec \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_fec_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_fec_print(NspFec *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLFEC) 
    {
      Sciprintf("Null Pointer NspFec \n");
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
          nsp_fec_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_fec_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->triangles != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->triangles),indent+2,"triangles",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->func != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->func),indent+2,"func",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colminmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colminmax),indent+2,"colminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zminmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->zminmax),indent+2,"zminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"draw	= %s\n", ( M->obj->draw == TRUE) ? "T" : "F" );
  if ( M->obj->colout != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colout),indent+2,"colout",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_fec_latex(NspFec *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_fec_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->triangles != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->triangles),indent+2,"triangles",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->func != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->func),indent+2,"func",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colminmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colminmax),indent+2,"colminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zminmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->zminmax),indent+2,"zminmax",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"draw	= %s\n", ( M->obj->draw == TRUE) ? "T" : "F" );
  if ( M->obj->colout != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colout),indent+2,"colout",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspFec objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspFec   *nsp_fec_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_fec_id) == TRUE ) return ((NspFec *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_fec));
  return NULL;
}

int IsFecObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_fec_id);
}

int IsFec(NspObject *O)
{
  return nsp_object_type(O,nsp_type_fec_id);
}

NspFec  *GetFecCopy(Stack stack, int i)
{
  if (  GetFec(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFec  *GetFec(Stack stack, int i)
{
  NspFec *M;
  if (( M = nsp_fec_object(NthObj(i))) == NULLFEC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspFec *nsp_fec_create_void(char *name,NspTypeBase *type)
{
 NspFec *H  = (type == NULL) ? new_fec() : type->new();
 if ( H ==  NULLFEC)
  {
   Sciprintf("No more memory\n");
   return NULLFEC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLFEC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_fec_create_partial(NspFec *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_fec)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->triangles = NULLMAT;
  H->obj->func = NULLMAT;
  H->obj->colminmax = NULLMAT;
  H->obj->zminmax = NULLMAT;
  H->obj->draw = TRUE;
  H->obj->colout = NULLMAT;
  return OK;
}

int nsp_fec_check_values(NspFec *H)
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
  if ( H->obj->triangles == NULLMAT) 
    {
       if (( H->obj->triangles = nsp_matrix_create("triangles",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->func == NULLMAT) 
    {
       if (( H->obj->func = nsp_matrix_create("func",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->colminmax == NULLMAT) 
    {
       if (( H->obj->colminmax = nsp_matrix_create("colminmax",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->zminmax == NULLMAT) 
    {
       if (( H->obj->zminmax = nsp_matrix_create("zminmax",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->colout == NULLMAT) 
    {
       if (( H->obj->colout = nsp_matrix_create("colout",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspFec *nsp_fec_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* triangles,NspMatrix* func,NspMatrix* colminmax,NspMatrix* zminmax,gboolean draw,NspMatrix* colout,NspTypeBase *type)
{
 NspFec *H  = nsp_fec_create_void(name,type);
 if ( H ==  NULLFEC) return NULLFEC;
  if ( nsp_fec_create_partial(H) == FAIL) return NULLFEC;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->triangles= triangles;
  H->obj->func= func;
  H->obj->colminmax= colminmax;
  H->obj->zminmax= zminmax;
  H->obj->draw=draw;
  H->obj->colout= colout;
 if ( nsp_fec_check_values(H) == FAIL) return NULLFEC;
 return H;
}


NspFec *nsp_fec_create_default(char *name)
{
 NspFec *H  = nsp_fec_create_void(name,NULL);
 if ( H ==  NULLFEC) return NULLFEC;
  if ( nsp_fec_create_partial(H) == FAIL) return NULLFEC;
 if ( nsp_fec_check_values(H) == FAIL) return NULLFEC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspFec *nsp_fec_copy_partial(NspFec *H,NspFec *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspFec *nsp_fec_copy(NspFec *self)
{
  NspFec *H  =nsp_fec_create_void(NVOID,(NspTypeBase *) nsp_type_fec);
  if ( H ==  NULLFEC) return NULLFEC;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLFEC;
  if ( nsp_fec_copy_partial(H,self)== NULL) return NULLFEC;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspFec *nsp_fec_full_copy_partial(NspFec *H,NspFec *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_fec))) == NULL) return NULLFEC;
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
  if ( self->obj->triangles == NULL )
    { H->obj->triangles = NULL;}
  else
    {
      if ((H->obj->triangles = (NspMatrix *) nsp_object_copy_and_name("triangles",NSP_OBJECT(self->obj->triangles))) == NULLMAT) return NULL;
    }
  if ( self->obj->func == NULL )
    { H->obj->func = NULL;}
  else
    {
      if ((H->obj->func = (NspMatrix *) nsp_object_copy_and_name("func",NSP_OBJECT(self->obj->func))) == NULLMAT) return NULL;
    }
  if ( self->obj->colminmax == NULL )
    { H->obj->colminmax = NULL;}
  else
    {
      if ((H->obj->colminmax = (NspMatrix *) nsp_object_copy_and_name("colminmax",NSP_OBJECT(self->obj->colminmax))) == NULLMAT) return NULL;
    }
  if ( self->obj->zminmax == NULL )
    { H->obj->zminmax = NULL;}
  else
    {
      if ((H->obj->zminmax = (NspMatrix *) nsp_object_copy_and_name("zminmax",NSP_OBJECT(self->obj->zminmax))) == NULLMAT) return NULL;
    }
  H->obj->draw=self->obj->draw;
  if ( self->obj->colout == NULL )
    { H->obj->colout = NULL;}
  else
    {
      if ((H->obj->colout = (NspMatrix *) nsp_object_copy_and_name("colout",NSP_OBJECT(self->obj->colout))) == NULLMAT) return NULL;
    }
  return H;
}

NspFec *nsp_fec_full_copy(NspFec *self)
{
  NspFec *H  =nsp_fec_create_void(NVOID,(NspTypeBase *) nsp_type_fec);
  if ( H ==  NULLFEC) return NULLFEC;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLFEC;
  if ( nsp_fec_full_copy_partial(H,self)== NULL) return NULLFEC;
#line 664 "fec.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspFec
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_fec_create(Stack stack, int rhs, int opt, int lhs)
{
  NspFec *H;
  CheckStdRhs(0,0);
  /* want to be sure that type fec is initialized */
  nsp_type_fec = new_type_fec(T_BASE);
  if(( H = nsp_fec_create_void(NVOID,(NspTypeBase *) nsp_type_fec)) == NULLFEC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_fec_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_fec_check_values(H) == FAIL) return RET_BUG;
#line 684 "fec.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *fec_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_fec_get_x(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_x(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_fec_set_x(void *self, char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->x);
  ((NspFec *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_fec_get_y(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_y(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_fec_set_y(void *self, char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->y);
  ((NspFec *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_fec_get_triangles(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->triangles;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_triangles(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->triangles);
  return (NspObject *) ret;
}

static int _wrap_fec_set_triangles(void *self, char *attr, NspObject *O)
{
  NspMatrix *triangles;

  if ( ! IsMat(O) ) return FAIL;
  if ((triangles = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->triangles != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->triangles);
  ((NspFec *) self)->obj->triangles= triangles;
  return OK;
}

static NspObject *_wrap_fec_get_func(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->func;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_func(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->func);
  return (NspObject *) ret;
}

static int _wrap_fec_set_func(void *self, char *attr, NspObject *O)
{
  NspMatrix *func;

  if ( ! IsMat(O) ) return FAIL;
  if ((func = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->func != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->func);
  ((NspFec *) self)->obj->func= func;
  return OK;
}

static NspObject *_wrap_fec_get_colminmax(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->colminmax;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_colminmax(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->colminmax);
  return (NspObject *) ret;
}

static int _wrap_fec_set_colminmax(void *self, char *attr, NspObject *O)
{
  NspMatrix *colminmax;

  if ( ! IsMat(O) ) return FAIL;
  if ((colminmax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->colminmax != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->colminmax);
  ((NspFec *) self)->obj->colminmax= colminmax;
  return OK;
}

static NspObject *_wrap_fec_get_zminmax(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->zminmax;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_zminmax(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->zminmax);
  return (NspObject *) ret;
}

static int _wrap_fec_set_zminmax(void *self, char *attr, NspObject *O)
{
  NspMatrix *zminmax;

  if ( ! IsMat(O) ) return FAIL;
  if ((zminmax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->zminmax != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->zminmax);
  ((NspFec *) self)->obj->zminmax= zminmax;
  return OK;
}

static NspObject *_wrap_fec_get_draw(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspFec *) self)->obj->draw;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_fec_set_draw(void *self, char *attr, NspObject *O)
{
  int draw;

  if ( BoolScalar(O,&draw) == FAIL) return FAIL;
  ((NspFec *) self)->obj->draw= draw;
  return OK;
}

static NspObject *_wrap_fec_get_colout(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspFec *) self)->obj->colout;
  return (NspObject *) ret;
}

static NspObject *_wrap_fec_get_obj_colout(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspFec *) self)->obj->colout);
  return (NspObject *) ret;
}

static int _wrap_fec_set_colout(void *self, char *attr, NspObject *O)
{
  NspMatrix *colout;

  if ( ! IsMat(O) ) return FAIL;
  if ((colout = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspFec *) self)->obj->colout != NULL ) 
    nsp_matrix_destroy(((NspFec *) self)->obj->colout);
  ((NspFec *) self)->obj->colout= colout;
  return OK;
}

static AttrTab fec_attrs[] = {
  { "x", (attr_get_function *)_wrap_fec_get_x, (attr_set_function *)_wrap_fec_set_x,(attr_get_object_function *)_wrap_fec_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_fec_get_y, (attr_set_function *)_wrap_fec_set_y,(attr_get_object_function *)_wrap_fec_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "triangles", (attr_get_function *)_wrap_fec_get_triangles, (attr_set_function *)_wrap_fec_set_triangles,(attr_get_object_function *)_wrap_fec_get_obj_triangles, (attr_set_object_function *)int_set_object_failed },
  { "func", (attr_get_function *)_wrap_fec_get_func, (attr_set_function *)_wrap_fec_set_func,(attr_get_object_function *)_wrap_fec_get_obj_func, (attr_set_object_function *)int_set_object_failed },
  { "colminmax", (attr_get_function *)_wrap_fec_get_colminmax, (attr_set_function *)_wrap_fec_set_colminmax,(attr_get_object_function *)_wrap_fec_get_obj_colminmax, (attr_set_object_function *)int_set_object_failed },
  { "zminmax", (attr_get_function *)_wrap_fec_get_zminmax, (attr_set_function *)_wrap_fec_set_zminmax,(attr_get_object_function *)_wrap_fec_get_obj_zminmax, (attr_set_object_function *)int_set_object_failed },
  { "draw", (attr_get_function *)_wrap_fec_get_draw, (attr_set_function *)_wrap_fec_set_draw,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "colout", (attr_get_function *)_wrap_fec_get_colout, (attr_set_function *)_wrap_fec_set_colout,(attr_get_object_function *)_wrap_fec_get_obj_colout, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 57 "codegen/fec.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_fec(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 941 "fec.c"


#line 67 "codegen/fec.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_fec(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 953 "fec.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Fec_func[]={
  {"extractelts_fec", _wrap_nsp_extractelts_fec},
  {"setrowscols_fec", _wrap_nsp_setrowscols_fec},
  { "fec_create", int_fec_create},
  { NULL, NULL}
};

/* call ith function in the Fec interface */

int Fec_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Fec_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Fec_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Fec_func[i].name;
  *f = Fec_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Fec_register_classes(NspObject *d)
{

#line 31 "codegen/fec.override"

Init portion 


#line 993 "fec.c"
  nspgobject_register_class(d, "NspFec", Fec, &NspNspFec_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 77 "codegen/fec.override"

/* inserted verbatim at the end */

static void nsp_translate_fec(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  int i;
  NspFec *P = (NspFec *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) += tr[0];
    }
  for ( i=0; i < P->obj->y->mn ; i++) 
    {
      *(y++) += tr[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_fec(BCG *Xgc,NspGraphic *Obj,double *R)
{
  /* nsp_figure_force_redraw(Obj->obj->Fig); */
}

static void nsp_scale_fec(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int i;
  NspFec *P = (NspFec *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) *= alpha[0];
    }
  for ( i=0; i < P->obj->y->mn ; i++) 
    {
      *(y++) *= alpha[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of fec 
 *
 */

static void nsp_getbounds_fec (BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspFec *P = (NspFec *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  bounds[0]=Mini(P->obj->x->R,P->obj->x->mn);/* xmin */
  bounds[2]=Maxi(P->obj->x->R,P->obj->x->mn);/* xmax */
  bounds[1]=Mini(P->obj->y->R,P->obj->y->mn);/* ymin */
  bounds[3]=Maxi(P->obj->y->R,P->obj->y->mn);/* ymin */
  
}



static void nsp_draw_fec(BCG *Xgc,NspGraphic *Obj, void *data)
{
  double *colout = NULL ; /* XXX à rajouter */
  NspFec *P = (NspFec *) Obj;
  double *zminmax = NULL;
  double *colminmax = NULL;
  double *func= P->obj->func->R;
  double *x =  P->obj->x->R;
  double *y =  P->obj->y->R;
  double *triangles = P->obj->triangles->R;
  int Nnode = P->obj->x->mn;
  int Ntr = P->obj->triangles->m;
  int draw = P->obj->draw;
  int *xm,*ym,i,  j, k;

  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  if ( P->obj->colminmax->mn == 2 ) 
    colminmax = P->obj->colminmax->R;

  if ( P->obj->zminmax->mn == 2 ) 
    zminmax = P->obj->zminmax->R;

  if ( P->obj->colout->mn == 2) 
    colout = P->obj->colout->R;

  /* Allocation */
  xm = graphic_alloc(0,Nnode,sizeof(int));
  ym = graphic_alloc(1,Nnode,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return;
    }      
  
  scale_f2i(Xgc,x,y,xm,ym,Nnode);

  /* Fec code */
  {
    /*
     *	 beginning of the code modified by Bruno 01/02/2001  
     */
    
    int nz, whiteid; 
    double *zlevel, dz, zmin, zmax, sx[3], sy[3];
    int *zone, *fill, zxy[3], color_min, color_max;

    /* choice between zmin and zmax given by the user or computed
     * with the min and max z values. 
     */

    if ( zminmax == NULL  ) { 
      zmin=(double) Mini(func,Nnode); 
      zmax=(double) Maxi(func,Nnode);
    } 
    else {
      zmin = Min( zminmax[0] , zminmax[1] );
      zmax = Max( zminmax[0] , zminmax[1] );
    };
      
    whiteid= Xgc->graphic_engine->xget_last(Xgc);
    nz=whiteid;
    
    /* choice for the colormap (in case of a user 's choice 
     * verify the parameter). 
     */

    if ( colminmax == NULL ) 
      {
	color_min=1; 
      }
    else 
      {
	/* we project on accepted values */
	color_min = Max(1,Min(Abs(colminmax[0]),Abs(colminmax[1])));
	color_max = Min(nz,Max(Abs(colminmax[0]),Abs(colminmax[1])));
	nz = color_max-color_min  + 1;
      }
      
    /* 
     *  1/ the purpose of the first part is to to compute the "zone" of each point :
     *    
     *    - the array zlevel are the boundaries between the differents zones :
     *
     *        zlevel[0] = zmin, zlevel[nz] = zmax 
     *     and zlevel[i] = zmin + i*(zmax-zmin)/nz
     *  
     *     - if  zlevel[j-1] <= func[i] < zlevel[j]  then zone[i] = j
     *       if func[i] > zmax  then zone[i] = nz+1
     *       if func[i] < zmin  then zone[i] = 0
     *     - the zone j is filled with color fill[j] with
     *       fill[j] = -(j-1 + color_min) if 1 <= j <= nz
     *     - if colout == NULL
     *        fill[0] = color attributed for fill[1]     ---> this behavior may be changed ...
     *        fill[nz+1] = color attributed for fill[nz] --/
     *       else 
     *        fill[0]=- colout[0];
     *        fill[1]=- colout[1];
     */
 
    /* allocations for some arrays ... */

    zone = graphic_alloc(2,(Nnode),sizeof(int));
    zlevel = graphic_alloc(3,nz+1,sizeof(double));
    fill  = graphic_alloc(4,nz+2,sizeof(int));
    if ( (zone == NULL) || (zlevel == NULL) || (fill  == NULL)) 
      {
	Scistring("fec: malloc No more Place\n");
	return;
      }
    /* compute the fill array (fill = - num color) */
    fill[1] = - color_min;
    for ( i = 2 ; i <= nz ; i++ ) fill[i] = fill[i-1] - 1;
    if ( colout == NULL) 
      {
	fill[0] =  fill[1] ; fill[nz+1] = fill[nz];
      }
    else 
      {
	fill[0] = - colout[0] ; fill[nz+1] = - colout[1];
      }

    /* compute the zlevels */
    dz = (zmax - zmin)/nz;
    for (i = 0 ; i < nz ; i++) zlevel[i] = zmin + i*dz;
    zlevel[nz] = zmax;

    /* finaly compute the zone of each point */
    for ( i = 0 ; i < (Nnode) ; i++ ) {
      if ( func[i] > zmax )
	zone[i] = nz+1;
      else if ( func[i] < zmin )
	zone[i] = 0;
      else
	zone[i] = floor( (func[i] - zmin)/dz ) + 1;
    };

    /* 
     *  2/ loop on the triangles : each triangle is finally decomposed 
     *     into its differents zones (polygons) by the function PaintTriangle   
     */
#if 0 
    if (  Xgc->graphic_engine == &GL_gengine ) 
      {
	for ( j = 0 ; j < Ntr ; j++) 
	  {
	    int ii[3],isx[3],isy[3]; 
	    /* retrieve node numbers and functions values */
	    for ( k = 0 ; k < 3 ; k++ ) {
	      ii[k] = (int) triangles[j+(Ntr)*(k+1)] - 1;
	      zxy[k] = zone[ii[k]];
	      isx[k]  = xm[ii[k]];   
	      isy[k]  = ym[ii[k]];
	      /* using ii for colors */
	      ii[k]= - fill[zxy[k]]; 
	    };
	    if (ii[0] != 0 && ii[1] != 0 && ii[2] != 0 ) 
	      {
		fillpolyline2D_shade(Xgc,isx,isy,ii,3,1); 
	      }     
	    /* call the "painting" function */
	    if ( draw == TRUE ) draw_triangle(Xgc,sx,sy);
	  };
      }
    else
#else 
      {
	for ( j = 0 ; j < Ntr ; j++) 
	  {
	    int ii[3], perm[3],kp;
	    double fxy[3];
	    
	    /* retrieve node numbers and functions values */
	    for ( k = 0 ; k < 3 ; k++ ) {
	      ii[k] = (int) triangles[j+(Ntr)*(k+1)] - 1;
	      zxy[k] = zone[ii[k]];
	    }

	    /* get the permutation perm so as zxy[perm] is sorted */
	    PermutOfSort(zxy, perm); 

	    /* apply the permutation to get the triangle 's vertices
	       in increasing zone (zxy[0] <= zxy[1] <= zxy[2]) */
	    for ( k = 0 ; k < 3 ; k++ ) {
	      kp = perm[k];
	      sx[k]  = xm[ii[kp]];   sy[k]  = ym[ii[kp]];
	      fxy[k] = func[ii[kp]]; zxy[k] = zone[ii[kp]];
	    };
	    
	    /* call the "painting" function */
	    PaintTriangle(Xgc,sx, sy, fxy, zxy, zlevel, fill);
	    
	    if ( draw == TRUE ) draw_triangle(Xgc,sx,sy);
	  }
      }
#endif 
  }

  /*
   *                     end of the modified code
   */
  
}



static void draw_triangle(BCG *Xgc,const double *sx,const double *sy)
{
  int nr, resx[3],resy[3];
  /* 
   * case of only one color for the triangle : 
   */
  resx[0]=inint(sx[0]); resx[1]=inint(sx[1]);  resx[2]=inint(sx[2]);
  resy[0]=inint(sy[0]); resy[1]=inint(sy[1]);  resy[2]=inint(sy[2]);
  nr = 3;
  Xgc->graphic_engine->drawpolyline(Xgc,resx,resy,nr,1);
}

#line 1273 "fec.c"
