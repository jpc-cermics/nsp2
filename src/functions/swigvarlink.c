/*------------------------------------------------------------------------
 * Copyright (C) 2007-2007 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library emulates Matlab' API functions.
 * It is a fully rewriten version of Scilab mexlib.c file 
 * since Scilab and nsp object are totally different 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * objects for swig 
 * NspSwigVarLink objects contains a NspHashTable filled with 
 * NspSwigGlobalVars i.e objects for which the get and set functional 
 * attributes are to be used instead of standard set and get 
 *--------------------------------------------------------------------------*/

#include <nsp/object.h>
#include <gtk/gtk.h>

#define SwigVarLink_Private
#include "swiglib.h"
#include "swigvarlink.h"
#include "swigglobalvar.h"
#include "nsp/interf.h"

/* 
 * NspSwigVarLink inherits from NspObject 
 */

int nsp_type_swigvarlink_id=0;
NspTypeSwigVarLink *nsp_type_swigvarlink=NULL;

/*
 * Type object for SwigVarLink 
 * all the instance of NspTypeSwigVarLink share the same id. 
 * nsp_type_swigvarlink: is an instance of NspTypeSwigVarLink 
 *    used for objects of NspSwigVarLink type (i.e built with new_swigvarlink) 
 * other instances are used for derived classes 
 */
NspTypeSwigVarLink *new_type_swigvarlink(type_mode mode)
{
  NspTypeSwigVarLink *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_swigvarlink != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_swigvarlink;
    }
  if ((type =  malloc(sizeof(NspTypeSwigVarLink))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = swigvarlink_attrs ; 
  type->get_attrs = (attrs_func *) int_swigvarlink_get_attribute; 
  type->set_attrs = (attrs_func *) int_swigvarlink_set_attribute;
  type->methods = swigvarlink_get_methods; 
  type->new = (new_func *) new_swigvarlink;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for swigvarlink */ 

  top->pr = (print_func *) nsp_swigvarlink_print;                  
  top->dealloc = (dealloc_func *) nsp_swigvarlink_destroy;
  top->copy  =  (copy_func *) nsp_swigvarlink_copy;                 
  top->size  = (size_func *) nsp_swigvarlink_size;                
  top->s_type =  (s_type_func *) nsp_swigvarlink_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_swigvarlink_type_short_string;
  top->info = (info_func *) nsp_swigvarlink_info ;                  
  /* top->is_true = (is_true_func  *) nsp_swigvarlink_is_true; */
  /* top->loop =(loop_func *) nsp_swigvarlink_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_swigvarlink_object;
  top->eq  = (eq_func *) nsp_swigvarlink_eq;
  top->neq  = (eq_func *) nsp_swigvarlink_neq;
  top->save  = (save_func *) nsp_swigvarlink_xdr_save;
  top->load  = (load_func *) nsp_swigvarlink_xdr_load;
  top->create = (create_func*) int_swigvarlink_create;
  top->latex = (print_func *) nsp_swigvarlink_latex_print;
  
  /* specific methods for swigvarlink */
      
  type->init = (init_func *) init_swigvarlink;

/* 
 * SwigVarLink interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_swigvarlink_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSwigVarLink called nsp_type_swigvarlink
       */
      type->id =  nsp_type_swigvarlink_id = nsp_new_type_id();
      nsp_type_swigvarlink = type;
      if ( nsp_register_type(nsp_type_swigvarlink) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_swigvarlink(mode);
    }
  else 
    {
       type->id = nsp_type_swigvarlink_id;
       return type;
    }
}

/*
 * initialize SwigVarLink instances 
 * locally and by calling initializer on parent class 
 */

static int init_swigvarlink(NspSwigVarLink *o,NspTypeSwigVarLink *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of SwigVarLink 
 */

NspSwigVarLink *new_swigvarlink() 
{
  NspSwigVarLink *loc; 
  /* type must exists */
  nsp_type_swigvarlink = new_type_swigvarlink(T_BASE);
  if ( (loc = malloc(sizeof(NspSwigVarLink)))== NULLSWIGVARLINK) return loc;
  /* initialize object */
  if ( init_swigvarlink(loc,nsp_type_swigvarlink) == FAIL) return NULLSWIGVARLINK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for SwigVarLink 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_swigvarlink_size(NspSwigVarLink *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char swigvarlink_type_name[]="SwigVarLink";
static char swigvarlink_short_type_name[]="swigvarlink";

static char *nsp_swigvarlink_type_as_string(void)
{
  return(swigvarlink_type_name);
}

static char *nsp_swigvarlink_type_short_string(NspObject *v)
{
  return(swigvarlink_short_type_name);
}

/*
 * A == B 
 */

static int nsp_swigvarlink_eq(NspSwigVarLink *A, NspObject *B)
{
  NspSwigVarLink *loc = (NspSwigVarLink *) B;
  if ( check_cast(B,nsp_type_swigvarlink_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->table != loc->obj->table) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_swigvarlink_neq(NspSwigVarLink *A, NspObject *B)
{
  return ( nsp_swigvarlink_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_swigvarlink_xdr_save(XDR *xdrs, NspSwigVarLink *M)
{
  Scierror("Error: swig_vl objects cannot be saved\n");
  return FAIL;
}

/*
 * load 
 */

static NspSwigVarLink  *nsp_swigvarlink_xdr_load(XDR *xdrs)
{
  return NULL;
}

/*
 * delete 
 */

void nsp_swigvarlink_destroy(NspSwigVarLink *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
  FREE(H);
}

/*
 * info 
 */

void nsp_swigvarlink_info(NspSwigVarLink *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSWIGVARLINK) 
    {
      Sciprintf("Null Pointer SwigVarLink \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_swigvarlink_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_swigvarlink_print(NspSwigVarLink *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSWIGVARLINK) 
    {
      Sciprintf("Null Pointer SwigVarLink \n");
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
          nsp_swigvarlink_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_swigvarlink_type_short_string(NSP_OBJECT(M)));
    }
}

/*
 * latex print 
 */

void nsp_swigvarlink_latex_print(NspSwigVarLink *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_swigvarlink_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    Sciprintf1(indent+2,"table=%d\n",M->obj->table);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for SwigVarLink objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspSwigVarLink   *nsp_swigvarlink_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_swigvarlink_id) == TRUE ) return ((NspSwigVarLink *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_swigvarlink));
  return NULL;
}

int IsSwigVarLinkObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_swigvarlink_id);
}

int IsSwigVarLink(NspObject *O)
{
  return nsp_object_type(O,nsp_type_swigvarlink_id);
}

NspSwigVarLink  *GetSwigVarLinkCopy(Stack stack, int i)
{
  if (  GetSwigVarLink(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSwigVarLink  *GetSwigVarLink(Stack stack, int i)
{
  NspSwigVarLink *M;
  if (( M = nsp_swigvarlink_object(NthObj(i))) == NULLSWIGVARLINK)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspSwigVarLink *swigvarlink_create_void(char *name,NspTypeBase *type)
{
 NspSwigVarLink *H  = (type == NULL) ? new_swigvarlink() : type->new();
 if ( H ==  NULLSWIGVARLINK)
  {
   Sciprintf("No more memory\n");
   return NULLSWIGVARLINK;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSWIGVARLINK;
 NSP_OBJECT(H)->ret_pos = -1 ;
  H->obj = NULL;
 return H;
}

NspSwigVarLink *swigvarlink_create(char *name)
{
  NspSwigVarLink *H  = swigvarlink_create_void(name,NULL);
  if ( H ==  NULLSWIGVARLINK) return NULLSWIGVARLINK;
  if ((H->obj = malloc(sizeof(nsp_swigvarlink))) == NULL) return NULL;
  if ((H->obj->table = nsp_hash_create("table",10)) == NULL) return NULL;
  H->obj->ref_count=1;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspSwigVarLink *nsp_swigvarlink_copy(NspSwigVarLink *self)
{
  NspSwigVarLink *H  =swigvarlink_create_void(NVOID,(NspTypeBase *) nsp_type_swigvarlink);
  if ( H ==  NULLSWIGVARLINK) return NULLSWIGVARLINK;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the SwigVarLink
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static void  fill_swigvarlink(NspSwigVarLink *H);

int int_swigvarlink_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSwigVarLink *H;
  CheckStdRhs(0,0);
  /* want to be sure that type swigvarlink is initialized */
  nsp_type_swigvarlink = new_type_swigvarlink(T_BASE);
  if(( H = swigvarlink_create(NVOID)) == NULLSWIGVARLINK) return RET_BUG;
  fill_swigvarlink(H);
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *swigvarlink_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

/* return all the keys H.keys entered in the swigvarlink table as a string matrice  */

static NspObject * int_swigvarlink_get_keys(void *Hv, char *attr)
{
  return (NspObject *) nsp_hash_get_keys(((NspSwigVarLink *)Hv)->obj->table);
}

static int int_swigvarlink_set_keys(void *Hv,const char *attr, NspObject *O)
{
  Scierror("attribute __keys of swigvarlink instances cannot be set !\n");
  return FAIL;
}

static AttrTab swigvarlink_attrs[] = {
  { "__keys", 	int_swigvarlink_get_keys , 	int_swigvarlink_set_keys , 	NULL },
  { (char *) 0, NULL}
};

/* 
 * get and set attributes are redefined 
 * to access data stored in the swigvarlink table 
 */

static int int_swigvarlink_get_attribute(Stack stack, int rhs, int opt, int lhs)
{
  char *key;
  NspSwigVarLink *H;
  NspObject *Obj;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((H = GetSwigVarLink(stack,1)) == NULL) return RET_BUG;
  if ((key = GetString(stack,2)) == (char*)0) return RET_BUG;  
  if (nsp_hash_find(H->obj->table,key,&Obj) == OK )
    {
      NspObject *Loc;
      if ((Loc = ((NspSwigGlobalVar *) Obj)->get_attr())== NULLOBJ )
	{
	  Scierror("Error: cannot get internal value of C global var %s\n",key);
	  return RET_BUG;
	}
      MoveObj(stack,1,Loc);
      return 1;      
    }
  /* Check now if key is an attribute of object Hash **/
  return int_get_attribute(stack,rhs,opt,lhs);
}

static int int_swigvarlink_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Val,*Obj;
  char *name;
  NspSwigVarLink *H;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H    = GetSwigVarLink(stack,1)) == NULL) return RET_BUG;
  if ((name = GetString(stack,2)) == (char*)0) return RET_BUG;  
  if ( strcmp(name,"__attrs") == 0 || strcmp(name,"__keys") == 0 ) 
    {
      Scierror("%s should not be used as a swigvarlink table entry\n",name);
      return RET_BUG;
    }
  /* check that object is already prsent on the hash table */
  if (nsp_hash_find(H->obj->table,name,&Obj) == OK )
    {
      /* no need to copy val */
      if (( Val = nsp_get_object(stack,3)) == NULLOBJ ) return RET_BUG;
      if ( ((NspSwigGlobalVar *) Obj)->set_attr(Val) != 0 )
	{
	  Scierror("Error: failed to give value to %s\n",name);
	  return RET_BUG;
	}
    }
  else 
    {
      Scierror("Error: cannot set the value of %s\n",name);
      return RET_BUG;
    }
  NSP_OBJECT(H)->ret_pos = 1;
  return 1;
}


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab swigvarlink_func[]={
  { "swigvarlink_create", int_swigvarlink_create},
  { NULL, NULL}
};

/* call ith function in the swigvarlink interface */

int swigvarlink_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(swigvarlink_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void swigvarlink_Interf_Info(int i, char **fname, function (**f))
{
  *fname = swigvarlink_func[i].name;
  *f = swigvarlink_func[i].fonc;
}

/* test 
 *
 */

static int  status;
static char path[256];

static int              ivar;
static short            svar;
static long             lvar;
static unsigned int     uivar;
static unsigned short   usvar;
static unsigned long    ulvar;
static signed char      scvar;
static unsigned char    ucvar;
static char             cvar;
static float            fvar;
static double           dvar;
static char            *strvar;
static const char       cstrvar[256];
/* static int             *iptrvar; */

static char             name[256];

/*
static Point           *ptptr;
static Point            pt;
*/

#define SWIGINTERN static 

SWIGINTERN int ivar_set(NspObject *_val) {
  {
    int val;
    int res = SWIG_AsVal_int(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""ivar""' of type '""int""'");
    }
    ivar = (int)(val);
  }
  return 0;
fail:
  return 1;
}

SWIGINTERN NspObject *ivar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_int((int)(ivar));
  return pyobj;
}


SWIGINTERN int svar_set(NspObject *_val) {
  {
    short val;
    int res = SWIG_AsVal_short(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""svar""' of type '""short""'");
    }
    svar = (short)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *svar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_short((short)(svar));
  return pyobj;
}


SWIGINTERN int lvar_set(NspObject *_val) {
  {
    long val;
    int res = SWIG_AsVal_long(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""lvar""' of type '""long""'");
    }
    lvar = (long)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *lvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_long((long)(lvar));
  return pyobj;
}


SWIGINTERN int uivar_set(NspObject *_val) {
  {
    unsigned int val;
    int res = SWIG_AsVal_unsigned_SS_int(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""uivar""' of type '""unsigned int""'");
    }
    uivar = (unsigned int)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *uivar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_unsigned_SS_int((unsigned int)(uivar));
  return pyobj;
}


SWIGINTERN int usvar_set(NspObject *_val) {
  {
    unsigned short val;
    int res = SWIG_AsVal_unsigned_SS_short(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""usvar""' of type '""unsigned short""'");
    }
    usvar = (unsigned short)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *usvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_unsigned_SS_short((unsigned short)(usvar));
  return pyobj;
}


SWIGINTERN int ulvar_set(NspObject *_val) {
  {
    unsigned long val;
    int res = SWIG_AsVal_unsigned_SS_long(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""ulvar""' of type '""unsigned long""'");
    }
    ulvar = (unsigned long)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *ulvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_unsigned_SS_long((unsigned long)(ulvar));
  return pyobj;
}


SWIGINTERN int scvar_set(NspObject *_val) {
  {
    signed char val;
    int res = SWIG_AsVal_signed_SS_char(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""scvar""' of type '""signed char""'");
    }
    scvar = (signed char)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *scvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_signed_SS_char((signed char)(scvar));
  return pyobj;
}


SWIGINTERN int ucvar_set(NspObject *_val) {
  {
    unsigned char val;
    int res = SWIG_AsVal_unsigned_SS_char(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""ucvar""' of type '""unsigned char""'");
    }
    ucvar = (unsigned char)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *ucvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_unsigned_SS_char((unsigned char)(ucvar));
  return pyobj;
}


SWIGINTERN int cvar_set(NspObject *_val) {
  {
    char val;
    int res = SWIG_AsVal_char(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""cvar""' of type '""char""'");
    }
    cvar = (char)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *cvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_char((char)(cvar));
  return pyobj;
}


SWIGINTERN int fvar_set(NspObject *_val) {
  {
    float val;
    int res = SWIG_AsVal_float(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""fvar""' of type '""float""'");
    }
    fvar = (float)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *fvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_float((float)(fvar));
  return pyobj;
}


SWIGINTERN int dvar_set(NspObject *_val) {
  {
    double val;
    int res = SWIG_AsVal_double(_val, &val);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""dvar""' of type '""double""'");
    }
    dvar = (double)(val);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *dvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_double((double)(dvar));
  return pyobj;
}


SWIGINTERN int strvar_set(NspObject *_val) {
  {
    char *cptr = 0; size_t csize = 0; int alloc = SWIG_NEWOBJ;
    int res = SWIG_AsCharPtrAndSize(_val, &cptr, &csize, &alloc);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""strvar""' of type '""char *""'");
    }
    if (strvar) free((char*)strvar);
    if (alloc == SWIG_NEWOBJ) {
      strvar = cptr;
    } else {
      strvar = csize ? (char *)(char *)memcpy((char *)malloc((csize)*sizeof(char)), cptr, sizeof(char)*(csize)) : 0;
    }
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *strvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_FromCharPtr(strvar);
  return pyobj;
}


SWIGINTERN int cstrvar_set(NspObject *_val ) {
  SWIG_Error(SWIG_AttributeError,"Variable cstrvar is read-only.");
  return 1;
}


SWIGINTERN NspObject *cstrvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_FromCharPtr(cstrvar);
  return pyobj;
}

/*
SWIGINTERN int iptrvar_set(NspObject *_val) {
  {
    void *argp = 0;
    int res = SWIG_ConvertPtr(_val, &argp, SWIGTYPE_p_int,  0 );  
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""iptrvar""' of type '""int *""'");
    }
    iptrvar = (int *)(argp);
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *iptrvar_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_NewPointerObj(SWIG_as_voidptr(iptrvar), SWIGTYPE_p_int,  0 );
  return pyobj;
}
*/


SWIGINTERN int name_set(NspObject *_val) {
  {
    int res = SWIG_AsCharArray(_val, name, 256);
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""name""' of type '""char [256]""'");
    }
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *name_get(void) {
  NspObject *pyobj = 0;
  
  size_t size = 256;
  
  while (size && (name[size - 1] == '\0')) --size;
  
  pyobj = SWIG_FromCharPtrAndSize(name, size);
  return pyobj;
}

/*
SWIGINTERN int ptptr_set(NspObject *_val) {
  {
    void *argp = 0;
    int res = SWIG_ConvertPtr(_val, &argp, SWIGTYPE_p_Point,  0 );  
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""ptptr""' of type '""Point *""'");
    }
    ptptr = (Point *)(argp);
  }
  return 0;
fail:
  return 1;
}

SWIGINTERN NspObject *ptptr_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_NewPointerObj(SWIG_as_voidptr(ptptr), SWIGTYPE_p_Point,  0 );
  return pyobj;
}
*/

/*
SWIGINTERN int pt_set(NspObject *_val) {
  {
    void *argp = 0;
    int res = SWIG_ConvertPtr(_val, &argp, SWIGTYPE_p_Point,  0 );
    if (!SWIG_IsOK(res)) {
      SWIG_exception_fail(SWIG_ArgError(res), "in variable '""pt""' of type '""Point""'");
    }
    if (!argp) {
      SWIG_exception_fail(SWIG_ValueError, "invalid null reference " "in variable '""pt""' of type '""Point""'");
    } else {
      pt = *((Point *)(argp));
    }
  }
  return 0;
fail:
  return 1;
}


SWIGINTERN NspObject *pt_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_NewPointerObj(SWIG_as_voidptr(&pt), SWIGTYPE_p_Point,  0 );
  return pyobj;
}
*/

SWIGINTERN int status_set(NspObject *_val ) {
  SWIG_Error(SWIG_AttributeError,"Variable status is read-only.");
  return 1;
}


SWIGINTERN NspObject *status_get(void) {
  NspObject *pyobj = 0;
  
  pyobj = SWIG_From_int((int)(status));
  return pyobj;
}


SWIGINTERN int path_set(NspObject *_val ) {
  SWIG_Error(SWIG_AttributeError,"Variable path is read-only.");
  return 1;
}


SWIGINTERN NspObject *path_get(void) {
  NspObject *pyobj = 0;
  
  size_t size = 256;
  
  while (size && (path[size - 1] == '\0')) --size;
  
  pyobj = SWIG_FromCharPtrAndSize(path, size);
  return pyobj;
}


static int SWIG_addvarlink(NspSwigVarLink *H,const char *name,swig_gv_get_attr *get_attr,swig_gv_set_attr *set_attr)
{
  NspSwigGlobalVar *Ivar; 
  Ivar = swigglobalvar_create(name,get_attr,set_attr,NULL);
  if (Ivar == NULL) return FAIL;
  if (nsp_hash_enter(H->obj->table,NSP_OBJECT(Ivar)) == FAIL) return FAIL;
  return OK;
}

static void  fill_swigvarlink(NspSwigVarLink *H)
{
  SWIG_addvarlink(H,(char*)"ivar",ivar_get, ivar_set);
  SWIG_addvarlink(H,(char*)"svar",svar_get, svar_set);
  SWIG_addvarlink(H,(char*)"lvar",lvar_get, lvar_set);
  SWIG_addvarlink(H,(char*)"uivar",uivar_get, uivar_set);
  SWIG_addvarlink(H,(char*)"usvar",usvar_get, usvar_set);
  SWIG_addvarlink(H,(char*)"ulvar",ulvar_get, ulvar_set);
  SWIG_addvarlink(H,(char*)"scvar",scvar_get, scvar_set);
  SWIG_addvarlink(H,(char*)"ucvar",ucvar_get, ucvar_set);
  SWIG_addvarlink(H,(char*)"cvar",cvar_get, cvar_set);
  SWIG_addvarlink(H,(char*)"fvar",fvar_get, fvar_set);
  SWIG_addvarlink(H,(char*)"dvar",dvar_get, dvar_set);
  SWIG_addvarlink(H,(char*)"strvar",strvar_get, strvar_set);
  SWIG_addvarlink(H,(char*)"cstrvar",cstrvar_get, cstrvar_set);
  /* SWIG_addvarlink(H,(char*)"iptrvar",iptrvar_get, iptrvar_set); */
  SWIG_addvarlink(H,(char*)"name",name_get, name_set);
  /* SWIG_addvarlink(H,(char*)"ptptr",ptptr_get, ptptr_set); */
  /* SWIG_addvarlink(H,(char*)"pt",pt_get, pt_set); */
  SWIG_addvarlink(H,(char*)"status",status_get, status_set);
  SWIG_addvarlink(H,(char*)"path",path_get, path_set);
}

