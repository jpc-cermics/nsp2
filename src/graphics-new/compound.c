/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "compound.override"
#include "nsp/compound.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj);

#line 18 "compound.c"

/* ----------- Compound ----------- */


#define  Compound_Private 
#include "nsp/object.h"
#include "nsp/compound.h"
#include "nsp/interf.h"

/* 
 * NspCompound inherits from NspGraphic 
 */

int nsp_type_compound_id=0;
NspTypeCompound *nsp_type_compound=NULL;

/*
 * Type object for Compound 
 * all the instance of NspTypeCompound share the same id. 
 * nsp_type_compound: is an instance of NspTypeCompound 
 *    used for objects of NspCompound type (i.e built with new_compound) 
 * other instances are used for derived classes 
 */
NspTypeCompound *new_type_compound(type_mode mode)
{
  NspTypeCompound *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_compound != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_compound;
    }
  if ((type =  malloc(sizeof(NspTypeCompound))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = compound_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = compound_get_methods; 
  type->new = (new_func *) new_compound;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for compound */ 

  top->pr = (print_func *) nsp_compound_print;                  
  top->dealloc = (dealloc_func *) nsp_compound_destroy;
  top->copy  =  (copy_func *) nsp_compound_copy;                 
  top->size  = (size_func *) nsp_compound_size;                
  top->s_type =  (s_type_func *) nsp_compound_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_compound_type_short_string;
  top->info = (info_func *) nsp_compound_info ;                  
  /* top->is_true = (is_true_func  *) nsp_compound_is_true; */
  /* top->loop =(loop_func *) nsp_compound_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_compound_object;
  top->eq  = (eq_func *) nsp_compound_eq;
  top->neq  = (eq_func *) nsp_compound_neq;
  top->save  = (save_func *) nsp_compound_xdr_save;
  top->load  = (load_func *) nsp_compound_xdr_load;
  top->create = (create_func*) int_compound_create;
  top->latex = (print_func *) nsp_compound_latex;
  
  /* specific methods for compound */
      
  type->init = (init_func *) init_compound;

#line 16 "compound.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_compound;

#line 93 "compound.c"
  /* 
   * Compound interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_compound_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCompound called nsp_type_compound
       */
      type->id =  nsp_type_compound_id = nsp_new_type_id();
      nsp_type_compound = type;
      if ( nsp_register_type(nsp_type_compound) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_compound(mode);
    }
  else 
    {
       type->id = nsp_type_compound_id;
       return type;
    }
}

/*
 * initialize Compound instances 
 * locally and by calling initializer on parent class 
 */

static int init_compound(NspCompound *Obj,NspTypeCompound *type)
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
 * new instance of Compound 
 */

NspCompound *new_compound() 
{
  NspCompound *loc; 
  /* type must exists */
  nsp_type_compound = new_type_compound(T_BASE);
  if ( (loc = malloc(sizeof(NspCompound)))== NULLCOMPOUND) return loc;
  /* initialize object */
  if ( init_compound(loc,nsp_type_compound) == FAIL) return NULLCOMPOUND;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Compound 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_compound_size(NspCompound *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char compound_type_name[]="Compound";
static char compound_short_type_name[]="compound";

static char *nsp_compound_type_as_string(void)
{
  return(compound_type_name);
}

static char *nsp_compound_type_short_string(NspObject *v)
{
  return(compound_short_type_name);
}

/*
 * A == B 
 */

static int nsp_compound_eq(NspCompound *A, NspObject *B)
{
  NspCompound *loc = (NspCompound *) B;
  if ( check_cast(B,nsp_type_compound_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->frect)->type->eq(A->obj->frect,loc->obj->frect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->wrect)->type->eq(A->obj->wrect,loc->obj->wrect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->elts)->type->eq(A->obj->elts,loc->obj->elts) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_compound_neq(NspCompound *A, NspObject *B)
{
  return ( nsp_compound_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_compound_xdr_save(XDR *xdrs, NspCompound *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->frect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->wrect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->elts)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCompound  *nsp_compound_xdr_load_partial(XDR *xdrs, NspCompound *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_compound))) == NULL) return NULL;
  if ((M->obj->frect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->wrect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->elts =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspCompound  *nsp_compound_xdr_load(XDR *xdrs)
{
  NspCompound *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCOMPOUND;
  if ((M  = nsp_compound_create_void(name,(NspTypeBase *) nsp_type_compound))== NULLCOMPOUND) return M;
  return nsp_compound_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_compound_destroy_partial(NspCompound *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_matrix_destroy(H->obj->frect);
  nsp_matrix_destroy(H->obj->wrect);
  nsp_list_destroy(H->obj->elts);
    FREE(H->obj);
   }
}

void nsp_compound_destroy(NspCompound *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_compound_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_compound_info(NspCompound *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer Compound \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_compound_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_compound_print(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer Compound \n");
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
          nsp_compound_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        if ( M->obj->frect != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1);
  if ( M->obj->wrect != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1);
  if ( M->obj->elts != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->elts),indent+2,"elts",rec_level+1);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_compound_latex(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    if ( M->obj->frect != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1);
  if ( M->obj->wrect != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1);
  if ( M->obj->elts != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->elts),indent+2,"elts",rec_level+1);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Compound objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspCompound   *nsp_compound_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_compound_id) == TRUE ) return ((NspCompound *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_compound));
  return NULL;
}

int IsCompoundObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_compound_id);
}

int IsCompound(NspObject *O)
{
  return nsp_object_type(O,nsp_type_compound_id);
}

NspCompound  *GetCompoundCopy(Stack stack, int i)
{
  if (  GetCompound(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCompound  *GetCompound(Stack stack, int i)
{
  NspCompound *M;
  if (( M = nsp_compound_object(NthObj(i))) == NULLCOMPOUND)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspCompound *nsp_compound_create_void(char *name,NspTypeBase *type)
{
 NspCompound *H  = (type == NULL) ? new_compound() : type->new();
 if ( H ==  NULLCOMPOUND)
  {
   Sciprintf("No more memory\n");
   return NULLCOMPOUND;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCOMPOUND;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_compound_create_partial(NspCompound *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_compound)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_compound_check_values(NspCompound *H)
{
  if ( H->obj->frect == NULLMAT) 
    {
     if (( H->obj->frect = nsp_matrix_create("frect",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->wrect == NULLMAT) 
    {
     if (( H->obj->wrect = nsp_matrix_create("wrect",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->elts == NULLLIST) 
    {
     if (( H->obj->elts = nsp_list_create("elts")) == NULLLIST)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspCompound *nsp_compound_create(char *name,NspMatrix* frect,NspMatrix* wrect,NspList* elts,NspTypeBase *type)
{
 NspCompound *H  = nsp_compound_create_void(name,type);
 if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
  if ( frect == NULL )
    { H->obj->frect = NULL;}
  else
    {
      if ((H->obj->frect = (NspMatrix *)  nsp_object_copy_and_name("frect",NSP_OBJECT(frect))) == NULLMAT) return NULL;
    }
  if ( wrect == NULL )
    { H->obj->wrect = NULL;}
  else
    {
      if ((H->obj->wrect = (NspMatrix *)  nsp_object_copy_and_name("wrect",NSP_OBJECT(wrect))) == NULLMAT) return NULL;
    }
  if ( elts == NULL )
    { H->obj->elts = NULL;}
  else
    {
      if ((H->obj->elts = (NspList *)  nsp_object_copy_and_name("elts",NSP_OBJECT(elts))) == NULLLIST) return NULL;
    }
 if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCompound *nsp_compound_copy_partial(NspCompound *H,NspCompound *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspCompound *nsp_compound_copy(NspCompound *self)
{
  NspCompound *H  =nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCOMPOUND;
  if ( nsp_compound_copy_partial(H,self)== NULL) return NULLCOMPOUND;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Compound
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_compound_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCompound *H;
  CheckStdRhs(0,0);
  /* want to be sure that type compound is initialized */
  nsp_type_compound = new_type_compound(T_BASE);
  if(( H = nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound)) == NULLCOMPOUND) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_compound_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_compound_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *compound_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_compound_get_frect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->frect);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_frect_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->frect);
  return (NspObject *) ret;
}

static int _wrap_compound_set_frect(void *self, char *attr, NspObject *O)
{
  NspMatrix *frect;

  if ( ! IsMat(O) ) return FAIL;
  if ((frect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCompound *) self)->obj->frect != NULL ) 
    nsp_object_destroy((NspObject **) &((NspCompound *) self)->obj->frect);
  ((NspCompound *) self)->obj->frect = frect;
  return OK;
}

static NspObject *_wrap_compound_get_wrect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->wrect);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_wrect_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->wrect);
  return (NspObject *) ret;
}

static int _wrap_compound_set_wrect(void *self, char *attr, NspObject *O)
{
  NspMatrix *wrect;

  if ( ! IsMat(O) ) return FAIL;
  if ((wrect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCompound *) self)->obj->wrect != NULL ) 
    nsp_object_destroy((NspObject **) &((NspCompound *) self)->obj->wrect);
  ((NspCompound *) self)->obj->wrect = wrect;
  return OK;
}

static NspObject *_wrap_compound_get_elts(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspCompound *) self)->obj->elts);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_elts_obj(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspCompound *) self)->obj->elts);
  return (NspObject *) ret;
}

static int _wrap_compound_set_elts(void *self, char *attr, NspObject *O)
{
  NspList *elts;

  if ( ! IsList(O) ) return FAIL;
  if ((elts = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspCompound *) self)->obj->elts != NULL ) 
    nsp_object_destroy((NspObject **) &((NspCompound *) self)->obj->elts);
  ((NspCompound *) self)->obj->elts = elts;
  return OK;
}

static AttrTab compound_attrs[] = {
  { "frect", (attr_get_function *)_wrap_compound_get_frect, (attr_set_function *)_wrap_compound_set_frect,(attr_get_object_function *)_wrap_compound_get_frect_obj },
  { "wrect", (attr_get_function *)_wrap_compound_get_wrect, (attr_set_function *)_wrap_compound_set_wrect,(attr_get_object_function *)_wrap_compound_get_wrect_obj },
  { "elts", (attr_get_function *)_wrap_compound_get_elts, (attr_set_function *)_wrap_compound_set_elts,(attr_get_object_function *)_wrap_compound_get_elts_obj },
  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 29 "compound.override"
int _wrap_compound_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 609 "compound.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Compound_func[]={
  {"compound_attach", _wrap_compound_attach},
  { "compound_create", int_compound_create},
  { NULL, NULL}
};

/* call ith function in the Compound interface */

int Compound_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Compound_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Compound_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Compound_func[i].name;
  *f = Compound_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Compound_register_classes(NspObject *d)
{

#line 11 "compound.override"

Init portion 


#line 648 "compound.c"
  nspgobject_register_class(d, "Compound", Compound, &NspCompound_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 55 "compound.override"

/* inserted verbatim at the end */

static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj)
{
  double WRect[4],WRect1[4], FRect[4], ARect[4];
  char logscale[2];
  Cell *cloc;
  NspList *L;
  NspCompound *P = (NspCompound *) Obj;
  Xgc->graphic_engine->scale->drawrectangle(Xgc,P->obj->wrect->R);
  /* draw elements */
  L = P->obj->elts;
  cloc = L->first ;
  /* we change the scale according to the compound */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  /* wrect is [left,up,w,h] */
  WRect1[0]= (P->obj->wrect->R[0]-FRect[0])/(FRect[2]-FRect[0]);
  WRect1[1]= 1- (P->obj->wrect->R[1]-FRect[1])/(FRect[3]-FRect[1]);
  WRect1[2]= (P->obj->wrect->R[2])/(FRect[2]-FRect[0]);
  WRect1[3]= (P->obj->wrect->R[3])/(FRect[3]-FRect[1]);
  /* we directly change the default scale because we do not want 
   * to register all the scales that will be generated by set_scale 
   * thus we use T in flag[1].
   */
  set_scale(Xgc,"fTtfff",WRect1,P->obj->frect->R,NULL,NULL,NULL);
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G);
	}
      cloc = cloc->next;
    }
  /* scale back */
  set_scale(Xgc,"fTtfff",WRect,FRect,NULL,NULL,NULL);
}


/*
  //xsetech(arect=[0.0,0.0,0.0,0.0],wrect=[0,0,1,1],frect=[-2,-3,5,5]);
  plot2d()
  C=compound_create();
  C.wrect=[1,3,3,2]; // the position of the compound in its parent 
  C.frect=[-2,0,2,3]; // the scales that the compound establish for its 
  // inside 
  compound_attach(C);
  P=polyline_create();
  P.Pts=[0,0;1,2;2,0];
  // polyline_attach(P);
  C.elts(1) = P;
  P=polyline_create();
  P.Pts=[0,0;1,2;2,0;0,0];
  polyline_attach(P);

  
  */

#line 713 "compound.c"
