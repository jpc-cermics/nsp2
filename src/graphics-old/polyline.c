/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "polyline.override"
#include "nsp/polyline.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_polyline(BCG *Xgc,NspGraphic *Obj);

#line 18 "polyline.c"

/* ----------- Polyline ----------- */


#define  Polyline_Private 
#include "nsp/object.h"
#include "nsp/polyline.h"
#include "nsp/interf.h"

/* 
 * NspPolyline inherits from NspGraphic 
 */

int nsp_type_polyline_id=0;
NspTypePolyline *nsp_type_polyline=NULL;

/*
 * Type object for Polyline 
 * all the instance of NspTypePolyline share the same id. 
 * nsp_type_polyline: is an instance of NspTypePolyline 
 *    used for objects of NspPolyline type (i.e built with new_polyline) 
 * other instances are used for derived classes 
 */
NspTypePolyline *new_type_polyline(type_mode mode)
{
  NspTypePolyline *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_polyline != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_polyline;
    }
  if ((type =  malloc(sizeof(NspTypePolyline))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = polyline_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = polyline_get_methods; 
  type->new = (new_func *) new_polyline;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for polyline */ 

  top->pr = (print_func *) nsp_polyline_print;                  
  top->dealloc = (dealloc_func *) nsp_polyline_destroy;
  top->copy  =  (copy_func *) nsp_polyline_copy;                 
  top->size  = (size_func *) nsp_polyline_size;                
  top->s_type =  (s_type_func *) nsp_polyline_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_polyline_type_short_string;
  top->info = (info_func *) nsp_polyline_info ;                  
  /* top->is_true = (is_true_func  *) nsp_polyline_is_true; */
  /* top->loop =(loop_func *) nsp_polyline_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_polyline_object;
  top->eq  = (eq_func *) nsp_polyline_eq;
  top->neq  = (eq_func *) nsp_polyline_neq;
  top->save  = (save_func *) nsp_polyline_xdr_save;
  top->load  = (load_func *) nsp_polyline_xdr_load;
  top->create = (create_func*) int_polyline_create;
  top->latex = (print_func *) nsp_polyline_latex;
  
  /* specific methods for polyline */
      
  type->init = (init_func *) init_polyline;

#line 16 "polyline.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_polyline;

#line 93 "polyline.c"
  /* 
   * Polyline interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_polyline_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePolyline called nsp_type_polyline
       */
      type->id =  nsp_type_polyline_id = nsp_new_type_id();
      nsp_type_polyline = type;
      if ( nsp_register_type(nsp_type_polyline) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_polyline(mode);
    }
  else 
    {
       type->id = nsp_type_polyline_id;
       return type;
    }
}

/*
 * initialize Polyline instances 
 * locally and by calling initializer on parent class 
 */

static int init_polyline(NspPolyline *Obj,NspTypePolyline *type)
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
 * new instance of Polyline 
 */

NspPolyline *new_polyline() 
{
  NspPolyline *loc; 
  /* type must exists */
  nsp_type_polyline = new_type_polyline(T_BASE);
  if ( (loc = malloc(sizeof(NspPolyline)))== NULLPOLYLINE) return loc;
  /* initialize object */
  if ( init_polyline(loc,nsp_type_polyline) == FAIL) return NULLPOLYLINE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Polyline 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_polyline_size(NspPolyline *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char polyline_type_name[]="Polyline";
static char polyline_short_type_name[]="polyline";

static char *nsp_polyline_type_as_string(void)
{
  return(polyline_type_name);
}

static char *nsp_polyline_type_short_string(NspObject *v)
{
  return(polyline_short_type_name);
}

/*
 * A == B 
 */

static int nsp_polyline_eq(NspPolyline *A, NspObject *B)
{
  NspPolyline *loc = (NspPolyline *) B;
  if ( check_cast(B,nsp_type_polyline_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( NSP_OBJECT(A->obj->Pts)->type->eq(A->obj->Pts,loc->obj->Pts) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_polyline_neq(NspPolyline *A, NspObject *B)
{
  return ( nsp_polyline_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_polyline_xdr_save(XDR *xdrs, NspPolyline *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Pts)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspPolyline  *nsp_polyline_xdr_load_partial(XDR *xdrs, NspPolyline *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_polyline))) == NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if ((M->obj->Pts =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspPolyline  *nsp_polyline_xdr_load(XDR *xdrs)
{
  NspPolyline *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPOLYLINE;
  if ((M  = nsp_polyline_create_void(name,(NspTypeBase *) nsp_type_polyline))== NULLPOLYLINE) return M;
  return nsp_polyline_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_polyline_destroy_partial(NspPolyline *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_matrix_destroy(H->obj->Pts);
    FREE(H->obj);
   }
}

void nsp_polyline_destroy(NspPolyline *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_polyline_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_polyline_info(NspPolyline *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLPOLYLINE) 
    {
      Sciprintf("Null Pointer Polyline \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_polyline_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_polyline_print(NspPolyline *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLPOLYLINE) 
    {
      Sciprintf("Null Pointer Polyline \n");
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
          nsp_polyline_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_polyline_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  if ( M->obj->Pts != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->Pts),indent+2,"Pts",rec_level+1);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_polyline_latex(NspPolyline *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_polyline_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  if ( M->obj->Pts != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->Pts),indent+2,"Pts",rec_level+1);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Polyline objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPolyline   *nsp_polyline_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_polyline_id) == TRUE ) return ((NspPolyline *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_polyline));
  return NULL;
}

int IsPolylineObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_polyline_id);
}

int IsPolyline(NspObject *O)
{
  return nsp_object_type(O,nsp_type_polyline_id);
}

NspPolyline  *GetPolylineCopy(Stack stack, int i)
{
  if (  GetPolyline(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPolyline  *GetPolyline(Stack stack, int i)
{
  NspPolyline *M;
  if (( M = nsp_polyline_object(NthObj(i))) == NULLPOLYLINE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspPolyline *nsp_polyline_create_void(char *name,NspTypeBase *type)
{
 NspPolyline *H  = (type == NULL) ? new_polyline() : type->new();
 if ( H ==  NULLPOLYLINE)
  {
   Sciprintf("No more memory\n");
   return NULLPOLYLINE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLPOLYLINE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_polyline_create_partial(NspPolyline *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_polyline)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_polyline_check_values(NspPolyline *H)
{
  if ( H->obj->Pts == NULLMAT) 
    {
     if (( H->obj->Pts = nsp_matrix_create("Pts",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspPolyline *nsp_polyline_create(char *name,int color,NspMatrix* Pts,NspTypeBase *type)
{
 NspPolyline *H  = nsp_polyline_create_void(name,type);
 if ( H ==  NULLPOLYLINE) return NULLPOLYLINE;
  if ( nsp_polyline_create_partial(H) == FAIL) return NULLPOLYLINE;
  H->obj->color=color;
  if ( Pts == NULL )
    { H->obj->Pts = NULL;}
  else
    {
      if ((H->obj->Pts = (NspMatrix *)  nsp_object_copy_and_name("Pts",NSP_OBJECT(Pts))) == NULLMAT) return NULL;
    }
 if ( nsp_polyline_check_values(H) == FAIL) return NULLPOLYLINE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspPolyline *nsp_polyline_copy_partial(NspPolyline *H,NspPolyline *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspPolyline *nsp_polyline_copy(NspPolyline *self)
{
  NspPolyline *H  =nsp_polyline_create_void(NVOID,(NspTypeBase *) nsp_type_polyline);
  if ( H ==  NULLPOLYLINE) return NULLPOLYLINE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYLINE;
  if ( nsp_polyline_copy_partial(H,self)== NULL) return NULLPOLYLINE;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Polyline
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_polyline_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyline *H;
  CheckStdRhs(0,0);
  /* want to be sure that type polyline is initialized */
  nsp_type_polyline = new_type_polyline(T_BASE);
  if(( H = nsp_polyline_create_void(NVOID,(NspTypeBase *) nsp_type_polyline)) == NULLPOLYLINE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_polyline_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_polyline_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *polyline_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_polyline_get_color(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspPolyline *) self)->obj->color);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_polyline_set_color(void *self, char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->color = color;
  return OK;
}

static NspObject *_wrap_polyline_get_Pts(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspPolyline *) self)->obj->Pts);
  return (NspObject *) ret;
}

static NspObject *_wrap_polyline_get_Pts_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspPolyline *) self)->obj->Pts);
  return (NspObject *) ret;
}

static int _wrap_polyline_set_Pts(void *self, char *attr, NspObject *O)
{
  NspMatrix *Pts;

  if ( ! IsMat(O) ) return FAIL;
  if ((Pts = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyline *) self)->obj->Pts != NULL ) 
    nsp_object_destroy((NspObject **) &((NspPolyline *) self)->obj->Pts);
  ((NspPolyline *) self)->obj->Pts = Pts;
  return OK;
}

static AttrTab polyline_attrs[] = {
  { "color", (attr_get_function *)_wrap_polyline_get_color, (attr_set_function *)_wrap_polyline_set_color,(attr_get_object_function *)int_get_object_failed },
  { "Pts", (attr_get_function *)_wrap_polyline_get_Pts, (attr_set_function *)_wrap_polyline_set_Pts,(attr_get_object_function *)_wrap_polyline_get_Pts_obj },
  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 29 "polyline.override"
int _wrap_polyline_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 537 "polyline.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Polyline_func[]={
  {"polyline_attach", _wrap_polyline_attach},
  { "polyline_create", int_polyline_create},
  { NULL, NULL}
};

/* call ith function in the Polyline interface */

int Polyline_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Polyline_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Polyline_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Polyline_func[i].name;
  *f = Polyline_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Polyline_register_classes(NspObject *d)
{

#line 11 "polyline.override"

Init portion 


#line 576 "polyline.c"
  nspgobject_register_class(d, "Polyline", Polyline, &NspPolyline_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 55 "polyline.override"

/* inserted verbatim at the end */

static void nsp_draw_polyline(BCG *Xgc,NspGraphic *Obj)
{
  NspPolyline *P = (NspPolyline *) Obj;
  NspMatrix *M = P->obj->Pts;
  Xgc->graphic_engine->scale->fillpolyline(Xgc,M->R,M->R+M->m,M->m,1);
}


#line 593 "polyline.c"
