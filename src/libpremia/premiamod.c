/* Nsp
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface with the sndfile library. 
 *
 */

#include <gtk/gtk.h>
#define  PremiaModel_Private 
#include "nsp/object.h"
#include "premiamod.h"
#include "nsp/interf.h"


static void nsp_premia_free_vars(VAR *vars,int flag,int n);
static int nsp_premia_clone_vars(VAR **res,int flag,const VAR *vars,int n);
static NspList* nsp_premia_get_var_names(const VAR *vars,int n);
static int nsp_premia_get_nvar(const VAR *vars);
static int nsp_premia_set_var_names(VAR *vars,int n,NspList *L);

/* 
 * NspPremiaModel inherits from NspObject 
 */

int nsp_type_premiamodel_id=0;
NspTypePremiaModel *nsp_type_premiamodel=NULL;

/*
 * Type object for PremiaModel 
 * all the instance of NspTypePremiaModel share the same id. 
 * nsp_type_premiamodel: is an instance of NspTypePremiaModel 
 *    used for objects of NspPremiaModel type (i.e built with new_premiamodel) 
 * other instances are used for derived classes 
 */
NspTypePremiaModel *new_type_premiamodel(type_mode mode)
{
  NspTypePremiaModel *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_premiamodel != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_premiamodel;
    }
  if ((type =  malloc(sizeof(NspTypePremiaModel))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = premiamodel_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = premiamodel_get_methods; 
  type->new = (new_func *) new_premiamodel;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for premiamodel */ 

  top->pr = (print_func *) nsp_premiamodel_print;                  
  top->dealloc = (dealloc_func *) nsp_premiamodel_destroy;
  top->copy  =  (copy_func *) nsp_premiamodel_copy;                 
  top->size  = (size_func *) nsp_premiamodel_size;                
  top->s_type =  (s_type_func *) nsp_premiamodel_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_premiamodel_type_short_string;
  top->info = (info_func *) nsp_premiamodel_info ;                  
  /* top->is_true = (is_true_func  *) nsp_premiamodel_is_true; */
  /* top->loop =(loop_func *) nsp_premiamodel_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_premiamodel_object;
  top->eq  = (eq_func *) nsp_premiamodel_eq;
  top->neq  = (eq_func *) nsp_premiamodel_neq;
  /* not implemented  
  top->save  = (save_func *) nsp_premiamodel_xdr_save;
  top->load  = (load_func *) nsp_premiamodel_xdr_load;
  */
  top->create = (create_func*) int_premiamodel_create;
  
  /* specific methods for premiamodel */
      
  type->init = (init_func *) init_premiamodel;

/* 
 * PremiaModel interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_premiamodel_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePremiaModel called nsp_type_premiamodel
       */
      type->id =  nsp_type_premiamodel_id = nsp_new_type_id();
      nsp_type_premiamodel = type;
      if ( nsp_register_type(nsp_type_premiamodel) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_premiamodel(mode);
    }
  else 
    {
       type->id = nsp_type_premiamodel_id;
       return type;
    }
}

/*
 * initialize PremiaModel instances 
 * locally and by calling initializer on parent class 
 */

static int init_premiamodel(NspPremiaModel *o,NspTypePremiaModel *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PremiaModel 
 */

NspPremiaModel *new_premiamodel() 
{
  NspPremiaModel *loc; 
  /* type must exists */
  nsp_type_premiamodel = new_type_premiamodel(T_BASE);
  if ( (loc = malloc(sizeof(NspPremiaModel)))== NULLPREMIAMODEL) return loc;
  /* initialize object */
  if ( init_premiamodel(loc,nsp_type_premiamodel) == FAIL) return NULLPREMIAMODEL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PremiaModel 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_premiamodel_size(NspPremiaModel *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char premiamodel_type_name[]="PremiaModel";
static char premiamodel_short_type_name[]="premiamodel";

static char *nsp_premiamodel_type_as_string(void)
{
  return(premiamodel_type_name);
}

static char *nsp_premiamodel_type_short_string(void)
{
  return(premiamodel_short_type_name);
}

/*
 * A == B 
 */

static int nsp_premiamodel_eq(NspPremiaModel *A, NspObject *B)
{
  NspPremiaModel *loc = (NspPremiaModel *) B;
  if ( check_cast(B,nsp_type_premiamodel_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int nsp_premiamodel_neq(NspPremiaModel *A, NspObject *B)
{
  return ( nsp_premiamodel_eq(A,B) == TRUE ) ? FALSE : TRUE;
}


/*
 * delete 
 */

void nsp_premiamodel_destroy(NspPremiaModel *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     /* FREE(H->obj->mod.TypeModel); */
     if (H->obj->mod.TypeModel != NULL) 
       nsp_premia_free_vars(H->obj->mod.TypeModel,TRUE,H->obj->mod.nvar);
     if (H->obj->opt.TypeOpt != NULL) 
       nsp_premia_free_vars(H->obj->opt.TypeOpt,TRUE,H->obj->opt.nvar);
     FREE(H->obj);
   }
  FREE(H);
}

/*
 * info 
 */

void nsp_premiamodel_info(NspPremiaModel *M, int indent)
{
  int i;
  if ( M == NULLPREMIAMODEL) 
    {
      Sciprintf("Null Pointer PremiaModel \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("PremiaModel %s {\n", NSP_OBJECT(M)->name);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");Sciprintf("}\n");
}

/*
 * print 
 */

void nsp_premiamodel_print(NspPremiaModel *M, int indent)
{
  int i;
  if ( M == NULLPREMIAMODEL) 
    {
      Sciprintf("Null Pointer PremiaModel \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("PremiaModel %s {\n", NSP_OBJECT(M)->name);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");Sciprintf("}\n");
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PremiaModel objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPremiaModel   *nsp_premiamodel_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_premiamodel_id) == TRUE ) return ((NspPremiaModel *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_premiamodel));
  return NULL;
}

int IsPremiaModelObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_premiamodel_id);
}

int IsPremiaModel(NspObject *O)
{
  return nsp_object_type(O,nsp_type_premiamodel_id);
}

NspPremiaModel  *GetPremiaModelCopy(Stack stack, int i)
{
  if (  GetPremiaModel(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPremiaModel  *GetPremiaModel(Stack stack, int i)
{
  NspPremiaModel *M;
  if (( M = nsp_premiamodel_object(NthObj(i))) == NULLPREMIAMODEL)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspPremiaModel *premiamodel_create_void(char *name,NspTypeBase *type)
{
 NspPremiaModel *H  = (type == NULL) ? new_premiamodel() : type->new();
 if ( H ==  NULLPREMIAMODEL)
  {
   Sciprintf("No more memory\n");
   return NULLPREMIAMODEL;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLPREMIAMODEL;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspPremiaModel *premiamodel_create(char *name,NspTypeBase *type)
{
  NspPremiaModel *H  = premiamodel_create_void(name,type);
  if ( H ==  NULLPREMIAMODEL) return NULLPREMIAMODEL;
  if ((H->obj = malloc(sizeof(nsp_premiamodel))) == NULL) return NULL;
  H->obj->ref_count=1;
  /* can be used to check that objects are non-initialized */
  H->obj->mod.TypeModel = NULL;
  H->obj->mod.Name[0]= '\0';
  H->obj->opt.TypeOpt = NULL;
  H->obj->opt.Name[0]= '\0';
  H->obj->meth.Name[0]= '\0';
  H->obj->compat = FALSE;
  return H;
}

/*
 * copy for gobject derived class  
 */

NspPremiaModel *nsp_premiamodel_copy(NspPremiaModel *self)
{
  NspPremiaModel *H  =premiamodel_create_void(NVOID,(NspTypeBase *) nsp_type_premiamodel);
  if ( H ==  NULLPREMIAMODEL) return NULLPREMIAMODEL;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the PremiaModel
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

extern Model *models[];
extern Family *families[];

/* Creat a first object 
 *
 *
 */
 
int int_premiapb_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPremiaModel *H;
  CheckStdRhs(0,0);
  /* want to be sure that type premiamodel is initialized */
  nsp_type_premiamodel = new_type_premiamodel(T_BASE);
  if(( H = premiamodel_create(NVOID,(NspTypeBase *) nsp_type_premiamodel)) == NULLPREMIAMODEL) 
    return RET_BUG;
  InitErrorMsg();
  InitVar();
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

int int_premiamodel_create(Stack stack, int rhs, int opt, int lhs)
{
  VAR *var;
  Model *poo;
  NspPremiaModel *H;
  int m,nmodels=0;
  CheckStdRhs(1,1);
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  /* number of models */
  while (models[nmodels] != NULL) nmodels++;
  /* use first model */
  poo=models[Min(Max(m,0),nmodels-1)];
  /* be sure that model is initialized to have correct nvar */
  poo->Init(poo);
  /* want to be sure that type premiamodel is initialized */
  nsp_type_premiamodel = new_type_premiamodel(T_BASE);
  if(( H = premiamodel_create(NVOID,(NspTypeBase *) nsp_type_premiamodel)) == NULLPREMIAMODEL) 
    return RET_BUG;
  H->obj->mod = *poo;
  H->obj->mod.init=0;
  /* clone vars recursively, even if it's not usefull for models */
  if ( nsp_premia_clone_vars(&var,TRUE,(VAR *) poo->TypeModel,poo->nvar)==FAIL) 
    return RET_BUG;
  H->obj->mod.init=1;
  H->obj->mod.TypeModel=var;
  /* H->obj->mod.TypeModel = malloc(poo->nvar*sizeof(VAR));  */
  /* poo->Init(&H->obj->mod); */
  /* check Ok */
  InitErrorMsg();
  InitVar();
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

int int_premia_get_models(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspSMatrix *S;
  int nmodels=0;
  CheckStdRhs(0,0);
  while (models[nmodels] != NULL) nmodels++;
  if ((S=nsp_smatrix_create_with_length(NVOID,nmodels,1,-1))== NULLSMAT) 
    return RET_BUG;
  for ( i=0 ; i < nmodels ; i++) 
    if ((S->S[ i] =nsp_string_copy(models[i]->Name)) == (nsp_string) 0) 
      return RET_BUG;
  MoveObj(stack,1,(NspObject  *) S);
  return 1;
}

extern Pricing *pricings[];
 
/* get the options in family m 
 * if a second optional argument is given the 
 * subset of the family compatible with model n 
 * is returned. 
 */

int int_premia_get_family(Stack stack, int rhs, int opt, int lhs)
{
  Model *poo;
  NspSMatrix *S;
  Option **loc;
  int nf=0,fsize=0,i,m,n=-1,nmodels=0;
  CheckStdRhs(1,2);
  /* the family */
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  /* a model */
  if ( rhs == 2 )
    {
      if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
      while (models[nmodels] != NULL) nmodels++;
      if ( n < 0 || n > nmodels -1 )
	{
	  Scierror("Error: model %d does not exists\n",n);
	  return RET_BUG;
	}
      poo=models[n];
    }
  while ( families[nf] != NULL) nf++;
  if ( m < 0 || m > nf -1 ) 
    {
      if ((S=nsp_smatrix_create_with_length(NVOID,0,0,-1))== NULLSMAT) 
	return RET_BUG;
      MoveObj(stack,1,(NspObject  *) S);
      return 1;
    }
  loc = (*families[m]);
  while ( loc[fsize] != NULL) fsize++;
  if ( n != -1 ) 
    {
      int count=0;
      /* if n is given we check if the family is compatible with 
       * the model. This is done by checking the first 
       */
      for ( i = 0; i < fsize ; i++) 
	{
	  if ( MatchingPricing(0,poo,loc[i],pricings) == 0) count++;
	}
      if ( count == fsize ) 
	{
	  Sciprintf("All the option matches with model %d\n",n);
	}
      fsize = count;
    }
  if ((S=nsp_smatrix_create_with_length(NVOID,fsize,1,-1))== NULLSMAT) 
    return RET_BUG;
  for ( i=0 ; i < fsize ; i++) 
    {
      if ((S->S[ i] =nsp_string_copy(loc[i]->Name)) == (nsp_string) 0) 
	return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) S);
  return 1;
}

/* get possible methods 
 *
 *
 */

int int_premia_get_methods(Stack stack, int rhs, int opt, int lhs)
{
  Pricing *res;
  Model *poo;
  Option **loc;
  NspSMatrix *S;
  int nf=0,fsize=0,i,m,n=-1,nmodels=0,npm=0,npm_ok=0;
  CheckStdRhs(3,3);
  /* the family */
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  /* the option */
  if (GetScalarInt(stack,1,&opt) == FAIL) return RET_BUG;
  /* the model */
  if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  while (models[nmodels] != NULL) nmodels++;
  if ( n < 0 || n > nmodels -1 ) goto empty;
  poo=models[n];
  poo->Init(poo);
  while ( families[nf] != NULL) nf++;
  if ( m < 0 || m > nf -1 ) goto empty;
  loc = (*families[m]);
  while ( loc[fsize] != NULL) fsize++;
  if ( opt < 0 || opt > fsize -1 ) goto empty;
  /* select the first matching pricing */
  InitErrorMsg();
  InitVar();
  loc[opt]->Init(loc[opt]);
  if ( SelectPricing(0,poo,loc[opt],pricings,&res) == WRONG) goto empty;
  /* walk on the selected pricing and check correct methods */
  while ( res->Methods[npm] != NULL) npm++;
  npm_ok=0;
  for ( i=0 ; i < npm ; i++) 
    {
      if ( res->Methods[i]->CheckOpt(loc[opt],poo)== OK) npm_ok++;
    }
  if ((S=nsp_smatrix_create_with_length(NVOID,npm_ok,1,-1))== NULLSMAT) 
    return RET_BUG;
  npm_ok=0;
  for ( i=0 ; i < npm ; i++) 
    {
      if ( res->Methods[i]->CheckOpt(loc[opt],poo)== OK) 
	{
	  if ((S->S[npm_ok] =nsp_string_copy(res->Methods[i]->Name)) == (nsp_string) 0) 
	    return RET_BUG;
	  npm_ok++;
	}
    }
  MoveObj(stack,1,(NspObject  *) S);
  return 1;
 empty:
  if ((S=nsp_smatrix_create_with_length(NVOID,0,0,-1))== NULLSMAT) 
    return RET_BUG;
  MoveObj(stack,1,(NspObject  *) S);
  return 1;
}


int int_premiaopt_create(Stack stack, int rhs, int opt, int lhs)
{
  VAR *var;
  Option *poo;
  Option **loc;
  NspPremiaModel *H;
  int m,n;
  int nf=0,fsize=0;
  CheckStdRhs(2,2);
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  while ( families[nf] != NULL) nf++;
  if ( m < 0 || m > nf -1 ) 
    {
      Scierror("Error: family %d does not exists\n",m);
      return RET_BUG;
    }
  loc = (*families[m]);
  while ( loc[fsize] != NULL) fsize++;
  if ( n < 0 || n > fsize -1 ) 
    {
      Scierror("Error: option %d does not exists in family %d\n",n,m);
      return RET_BUG;
    }
  poo=(*families[m])[n];
  /* be sure that model is initialized to have correct nvar */
  poo->Init(poo);
  /* want to be sure that type premiamodel is initialized */
  nsp_type_premiamodel = new_type_premiamodel(T_BASE);
  if(( H = premiamodel_create(NVOID,(NspTypeBase *) nsp_type_premiamodel)) == NULLPREMIAMODEL) 
    return RET_BUG;
  H->obj->opt = *poo;
  H->obj->opt.init=0;
  /* clone vars recursively, even if it's not usefull for models */
  if ( nsp_premia_clone_vars(&var,TRUE,(VAR *) poo->TypeOpt,poo->nvar)==FAIL) 
    return RET_BUG;
  H->obj->mod.init=1;
  H->obj->opt.TypeOpt=var;
  /* poo->Init(&H->obj->opt); */
  /* check Ok */
  InitErrorMsg();
  InitVar();
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 



int int_premiamethod_create(Stack stack, int rhs, int opt, int lhs)
{
  VAR *var;
  PricingMethod *poo=NULL;
  NspPremiaModel *H;
  int m,npar,nres;
  CheckStdRhs(1,1);
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  poo=pricings[3]->Methods[m];
  /* be sure that model is initialized to have correct nvar */
  poo->Init(poo);
  /* want to be sure that type premiamodel is initialized */
  nsp_type_premiamodel = new_type_premiamodel(T_BASE);
  if(( H = premiamodel_create(NVOID,(NspTypeBase *) nsp_type_premiamodel)) == NULLPREMIAMODEL) 
    return RET_BUG;
  H->obj->meth = *poo;
  H->obj->meth.init=0;
  /* clone vars recursively, even if it's not usefull for models */
  var=H->obj->meth.Par;
  npar = nsp_premia_get_nvar(var);
  if ( nsp_premia_clone_vars(&var,FALSE,(VAR *) poo->Par,npar)==FAIL) 
    return RET_BUG;
  var = H->obj->meth.Res;
  nres = nsp_premia_get_nvar(var);
  if ( nsp_premia_clone_vars(&var,FALSE,(VAR *) poo->Res,nres)==FAIL) 
    return RET_BUG;
  /* poo->Init(&H->obj->opt); */
  /* check Ok */
  H->obj->mod.init=1;
  InitErrorMsg();
  InitVar();
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 


extern int         *true_typeV;
typedef enum { p_model, p_option, p_method_in, p_method_out } p_objs;

static int _wrap_premiamodel_get_vars(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  int nvar;
  NspSMatrix *Res;
  VAR *vars;
  int i;
  CheckLhs(0,2);
  CheckRhs(0,0);
  switch (type ) 
    {
    case p_model : 
      vars = self->obj->mod.TypeModel;
      nvar = self->obj->mod.nvar;
      break;
    case p_option :
      vars = self->obj->opt.TypeOpt;
      nvar = self->obj->opt.nvar_setable;
      break;
    case p_method_in: 
      vars=self->obj->meth.Par;
      nvar = nsp_premia_get_nvar(vars);
      break;
    case p_method_out: 
      vars=self->obj->meth.Res;
      nvar = nsp_premia_get_nvar(vars);
      break;
    default:
      Scierror("Warning: to be done\n");
      return RET_BUG;
    }
  if ( ( Res =nsp_smatrix_create_with_length(NVOID,nvar,1,-1) ) == NULLSMAT)
    return RET_BUG;
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Res->mn ; i++ )
    {
      if ((Res->S[i] =nsp_string_copy(vars[i].Vname)) == (nsp_string) 0) 
	return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  if ( lhs == 2 ) 
    {
      NspList *L=nsp_premia_get_var_names(vars,Res->mn);
      if ( L == NULL) return RET_BUG;
      /* XXXX just a test */
      if ( nsp_premia_set_var_names(vars,nvar,L) == FAIL) 
	{
	  Scierror("nsp_premia_set_var_names failed \n");
	  return RET_BUG;
	}
      MoveObj(stack,2,NSP_OBJECT(L));
    }
  return Max(1,lhs);
}

static int _wrap_premiamodel_get_model_vars(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  return _wrap_premiamodel_get_vars(self,stack,rhs,opt,lhs,p_model);
}

static int _wrap_premiamodel_get_option_vars(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  return _wrap_premiamodel_get_vars(self,stack,rhs,opt,lhs,p_option);
}

static int _wrap_premiamodel_get_method_vars(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  return _wrap_premiamodel_get_vars(self,stack,rhs,opt,lhs,p_method_in);
}

static int _wrap_premiamodel_get_method_res_vars(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  return _wrap_premiamodel_get_vars(self,stack,rhs,opt,lhs,p_method_out);
}



static int get_value(const VAR *x,double *val)
{
  int vt;
  if ( x->Vtype >= FIRSTLEVEL)
    {
      *val =0;
      return OK;
    }
  vt=true_typeV[x->Vtype];
  switch(vt)
    {
    case DOUBLE:  *val = x->Val.V_DOUBLE; break;
    case INT: *val = (double) x->Val.V_INT; break;
    case LONG: *val = (double) x->Val.V_LONG;break;
    default:
      Scierror("Warning: UNKNOWN TRUETYPE IN THE VAR SYSTEM\n");
      return FAIL;
      break;
    }
  return OK;
}

static int set_value(VAR *x,double val)
{
  int vt;
  if ( x->Vtype >= FIRSTLEVEL) return OK;
  vt=true_typeV[x->Vtype];
  switch(vt)
    {
    case DOUBLE: x->Val.V_DOUBLE = val ; break;
    case INT:    x->Val.V_INT = val ; break;
    case LONG:   x->Val.V_LONG = val ;break;
    default:
      Scierror("Warning: UNKNOWN TRUETYPE IN THE VAR SYSTEM\n");
      return FAIL;
      break;
    }
  return OK;
}



static int _wrap_premiamodel_get_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  NspMatrix *Res;
  VAR *vars;
  int i,nvar;
  CheckLhs(0,1);
  CheckRhs(0,0);
  switch (type ) 
    {
    case p_model : 
      vars = self->obj->mod.TypeModel;
      nvar = self->obj->mod.nvar;
      break;
    case p_option :
      vars = self->obj->opt.TypeOpt;
      nvar = self->obj->opt.nvar_setable;
      break;
    case p_method_in: 
      vars=self->obj->meth.Par;
      nvar = nsp_premia_get_nvar(vars);
      break;
    case p_method_out: 
      vars=self->obj->meth.Res;
      nvar = nsp_premia_get_nvar(vars);
      break;
    default:
      Scierror("Warning: to be done\n");
      return RET_BUG;
    }
  if ( ( Res =nsp_matrix_create(NVOID,'r',nvar,1) ) == NULLMAT)
    return RET_BUG;
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Res->mn ; i++ )
    {
      if ( get_value(&vars[i],&Res->R[i]) == FAIL) return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int _wrap_premiamodel_get_option_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_get_values(self,stack,rhs,opt,lhs,p_option);
}

static int _wrap_premiamodel_get_model_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_get_values(self,stack,rhs,opt,lhs,p_model);
}

static int _wrap_premiamodel_get_method_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_get_values(self,stack,rhs,opt,lhs,p_method_in);
}

static int _wrap_premiamodel_get_method_res_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_get_values(self,stack,rhs,opt,lhs,p_method_out);
}


static int _wrap_premiamodel_set_values_old(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  NspMatrix *A;
  VAR *vars;
  int i,nvar;
  CheckLhs(0,0);
  CheckRhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  switch (type ) 
    {
    case p_model : 
      vars = self->obj->mod.TypeModel;
      nvar = self->obj->mod.nvar;
      break;
    case p_option :
      vars = self->obj->opt.TypeOpt;
      nvar = self->obj->opt.nvar_setable;
      break;
    case p_method_in: 
      vars=self->obj->meth.Par;
      nvar = nsp_premia_get_nvar(vars);
      break;
    case p_method_out: 
      vars=self->obj->meth.Res;
      nvar = nsp_premia_get_nvar(vars);
      break;
    default:
      Scierror("Warning: to be done\n");
      return RET_BUG;
    }
  CheckLength(NspFname(stack),1,A,nvar);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < A->mn ; i++ )
    {
      if ( set_value(&vars[i],A->R[i]) == FAIL) return RET_BUG;
    }
  return 0;
}


static int _wrap_premiamodel_set_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs,p_objs type)
{
  NspList *A;
  VAR *vars;
  int nvar;
  CheckLhs(0,0);
  CheckRhs(1,1);
  if (( A=GetList(stack,1)) == NULLLIST) return RET_BUG;
  switch (type ) 
    {
    case p_model : 
      vars = self->obj->mod.TypeModel;
      nvar = self->obj->mod.nvar;
      break;
    case p_option :
      vars = self->obj->opt.TypeOpt;
      nvar = self->obj->opt.nvar_setable;
      break;
    case p_method_in: 
      vars=self->obj->meth.Par;
      nvar = nsp_premia_get_nvar(vars);
      break;
    case p_method_out: 
      vars=self->obj->meth.Res;
      nvar = nsp_premia_get_nvar(vars);
      break;
    default:
      Scierror("Warning: to be done\n");
      return RET_BUG;
    }
  if(  nsp_premia_set_var_names(vars,nvar,A) == FAIL) return RET_BUG;
  return 0;
}

static int _wrap_premiamodel_set_option_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_set_values(self,stack,rhs,opt,lhs,p_option);
}

static int _wrap_premiamodel_set_model_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_set_values(self,stack,rhs,opt,lhs,p_model);
}

static int _wrap_premiamodel_set_method_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs) 
{
  return _wrap_premiamodel_set_values(self,stack,rhs,opt,lhs,p_method_in);
}





static int _wrap_premiamodel_check_values(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int i,nvar;
  VAR *vars;
  NspBMatrix *Res;
  CheckLhs(0,1);
  CheckRhs(0,0);
  if ( self->obj->mod.TypeModel != NULL) 
    {
      vars = self->obj->mod.TypeModel;
      nvar = self->obj->mod.nvar;
    }
  else 
    {
      vars = self->obj->opt.TypeOpt;
      nvar = self->obj->opt.nvar_setable;
    }
  if ( ( Res =nsp_bmatrix_create(NVOID,nvar,1) ) == NULLBMAT)
    return RET_BUG;
  /* allocate elements and store copies of A elements **/
  /* self->obj->mod.Check(NO_PAR,NULL,&self->obj->mod); */
  for ( i = 0 ; i < Res->mn ; i++ )
    {
      Res->B[i]= (ChkVar(NULL,&vars[i]) == 0);
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}




static int _wrap_premiamodel_check_values1(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  char *error,*format;
  int i,type,nvar;
  VAR *vars;
  NspBMatrix *Res;
  CheckLhs(0,1);
  CheckRhs(0,0);
  if ( self->obj->mod.TypeModel != NULL) 
    {
      vars = self->obj->mod.TypeModel;
      nvar = self->obj->mod.nvar;
    }
  else 
    {
      vars = self->obj->opt.TypeOpt;
      nvar = self->obj->opt.nvar_setable;
    }
  if ( ( Res =nsp_bmatrix_create(NVOID,1,1) ) == NULLBMAT)
    return RET_BUG;
  Res->B[0] = TRUE;
  for ( i = 0 ; i < nvar ; i++ )
    {
      int status =  (ChkVar(NULL,&vars[i]) == 0);
      Res->B[0] = Res->B[0] && status;
      if ( status == FALSE ) 
	{
	  premia_Vtype_info(&vars[i],&format,&error,&type);
	  Scierror("Error: %s is wrong, %s\n",vars[i].Vname,error);
	}
    }
  if ( Res->B[0]== FALSE) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}


static int _wrap_premiamodel_set_model(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  VAR *var;
  Model *premia_model;
  int model,n_models=0;
  CheckStdRhs(1,1);
  if (GetScalarInt(stack,1,&model) == FAIL) return RET_BUG;
  /* number of models */
  while (models[n_models] != NULL) n_models++;
  if ( model < 0 || model > n_models -1 )
    {
      Scierror("Error: model %d does not exists\n",model);
      return RET_BUG;
    }
  if ( self->obj->mod.TypeModel != NULL ) 
    {
      Scierror("Error: model is already set\n",model);
      return RET_BUG;
    }
  /* use first model */
  premia_model= models[model];
  /* be sure that model is initialized to have correct nvar */
  premia_model->Init(premia_model);
  /* want to be sure that type premiamodel is initialized */
  self->obj->mod = *premia_model;
  self->obj->mod.init=0;
  /* clone vars recursively, even if it's not usefull for models */
  if ( nsp_premia_clone_vars(&var,TRUE,(VAR *) premia_model->TypeModel,premia_model->nvar)==FAIL) 
    return RET_BUG;
  self->obj->mod.init=1;
  self->obj->mod.TypeModel=var;
  return 0;
} 

static int _wrap_premiamodel_get_model(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *S;
  CheckStdRhs(0,0);
  if ( self->obj->mod.TypeModel == NULL ) 
    {
      if ((S=nsp_smatrix_create_with_length(NVOID,0,0,-1))== NULLSMAT) 
	return RET_BUG;
      MoveObj(stack,1,(NspObject  *) S);
      return 1;
    }
  if ( nsp_move_string(stack,1,self->obj->mod.Name,-1) == FAIL) return RET_BUG;
  return 1;
} 


static int _wrap_premiamodel_set_option(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  Option *premia_option;
  Option **loc;
  VAR *var;
  int option,n_option=0,family,n_family=0;
  CheckStdRhs(2,2);
  if (GetScalarInt(stack,1,&family) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&option) == FAIL) return RET_BUG;
  if ( self->obj->mod.TypeModel == NULL) 
    {
      Scierror("Error: you must first set a model\n");
      return RET_BUG;
    }
  if ( self->obj->opt.TypeOpt != NULL) 
    {
      Scierror("Error: option is already set\n");
      return RET_BUG;
    }

  while ( families[n_family] != NULL) n_family++;
  if ( family  < 0 || family > n_family -1 ) 
    {
      Scierror("Error: family %d does not exists\n",family);
      return RET_BUG;
    }
  loc = (*families[family]);
  while ( loc[n_option] != NULL) n_option++;
  if ( option < 0 || option > n_option -1 ) 
    {
      Scierror("Error: option %d does not exists in family %d\n",option,family);
      return RET_BUG;
    }
  premia_option=(*families[family])[option];
  /* be sure that model is initialized to have correct nvar */
  premia_option->Init(premia_option);
  /* check that option is compatible */
  if ( MatchingPricing(0,&self->obj->mod,premia_option,pricings) != 0) 
    {
      Scierror("Error: option %d is not compatible with model\n",option);
      return RET_BUG;
    }
  self->obj->opt = *premia_option;
  self->obj->opt.init=0;
  /* clone vars recursively, even if it's not usefull for models */
  if ( nsp_premia_clone_vars(&var,TRUE,(VAR *) premia_option->TypeOpt,premia_option->nvar)==FAIL) 
    return RET_BUG;
  self->obj->mod.init=1;
  self->obj->opt.TypeOpt=var;
  return 0;
} 


static int _wrap_premiamodel_get_option(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *S;
  CheckStdRhs(0,0);
  if ( self->obj->opt.TypeOpt == NULL ) 
    {
      if ((S=nsp_smatrix_create_with_length(NVOID,0,0,-1))== NULLSMAT) 
	return RET_BUG;
      MoveObj(stack,1,(NspObject  *) S);
      return 1;
    }
  if ( nsp_move_string(stack,1,self->obj->opt.Name,-1) == FAIL) return RET_BUG;
  return 1;
} 



static int _wrap_premiamodel_set_method(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  Pricing *res;
  VAR *var;
  int method,n_method=0, npar,nres;
  CheckStdRhs(1,1);
  if (GetScalarInt(stack,1,&method) == FAIL) return RET_BUG;
  if ( self->obj->mod.TypeModel == NULL) 
    {
      Scierror("Error: you must first set a model\n");
      return RET_BUG;
    }
  if ( self->obj->opt.TypeOpt == NULL) 
    {
      Scierror("Error: you must first set an option\n");
      return RET_BUG;
    }
  /* we know that MatcingPricing is ok */
  if ( SelectPricing(0,&self->obj->mod,&self->obj->opt,pricings,&res) == WRONG)
    {
      Scierror("Error: We have possible pricings but model and option have incompatibilities\n");
      return RET_BUG;
    }
  /* now we have a pricing */
  while ( res->Methods[n_method] != NULL) n_method++;
  if ( method < 0 || method > n_method -1 )
    {
      Scierror("Error: method %d does not exists in possible pricing methods [%d,%d]\n",method,0,n_method);
      return RET_BUG;
    }
  
  self->obj->meth = *(res->Methods[method]);
  self->obj->meth.init=0;
  /* clone vars recursively */
  var=self->obj->meth.Par;
  npar = nsp_premia_get_nvar(var);
  if ( nsp_premia_clone_vars(&var,FALSE,(VAR *) res->Methods[method]->Par,npar)==FAIL) 
    return RET_BUG;
  var = self->obj->meth.Res;
  nres = nsp_premia_get_nvar(var);
  if ( nsp_premia_clone_vars(&var,FALSE,(VAR *) res->Methods[method]->Res,nres)==FAIL) 
    return RET_BUG;
  self->obj->mod.init=1;
  return 0;
} 


static int _wrap_premiamodel_get_method(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *S;
  CheckStdRhs(0,0);
  if ( self->obj->meth.Name[0] == '\0' ) 
    {
      if ((S=nsp_smatrix_create_with_length(NVOID,0,0,-1))== NULLSMAT) 
	return RET_BUG;
      MoveObj(stack,1,(NspObject  *) S);
      return 1;
      Scierror("Error: model is already set\n");
      return RET_BUG;
    }
  if ( nsp_move_string(stack,1,self->obj->meth.Name,-1) == FAIL) return RET_BUG;
  return 1;
} 


static int _wrap_premiamodel_compute(NspPremiaModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int user=NO_PAR,error;
  Planning pt_plan;
  CheckStdRhs(0,0);
  if ( self->obj->mod.TypeModel == NULL) 
    {
      Scierror("Error: you must first set a model\n");
      return RET_BUG;
    }
  if ( self->obj->opt.TypeOpt == NULL) 
    {
      Scierror("Error: you must first set an option\n");
      return RET_BUG;
    }
  if ( self->obj->meth.Name[0] == '\0') 
    {
      Scierror("Error: you must first set a method\n");
      return RET_BUG;
    }
  if ((self->obj->mod.Check)(user,&pt_plan,&self->obj->mod) != OK ) 
    {
      Scierror("Error: model is not correct\n");
      return RET_BUG;
    }
  if ((self->obj->opt.Check)(user,&pt_plan,&self->obj->opt) !=OK)
    {
      Scierror("Error: option is not correct\n");
      return RET_BUG;
    }
  /* Il faut ici un pricing 
  if ((self->obj->meth.CheckMixing)(&self->obj->opt,&self->obj->mod) != OK) 
    {
      Scierror("Error: option and model do not mix\n");
      return RET_BUG;
    }
  */
  if ((self->obj->meth.Check)(user,&pt_plan,&self->obj->meth)!=OK)
    {
      Scierror("Error: method is not correct\n");
      return RET_BUG;
    }
  error=(self->obj->meth.Compute)(self->obj->opt.TypeOpt,self->obj->mod.TypeModel,&self->obj->meth);  		
  return 0;
}



static NspMethods premiamodel_methods[] = {
  {"set_model",(nsp_method *) _wrap_premiamodel_set_model},
  {"get_model",(nsp_method *) _wrap_premiamodel_get_model},
  {"set_option",(nsp_method *) _wrap_premiamodel_set_option},
  {"get_option",(nsp_method *) _wrap_premiamodel_get_option},
  {"set_method",(nsp_method *) _wrap_premiamodel_set_method},
  {"get_method",(nsp_method *) _wrap_premiamodel_get_method},

  {"get_model_vars",(nsp_method *) _wrap_premiamodel_get_model_vars},
  {"get_model_values",(nsp_method *) _wrap_premiamodel_get_model_values},
  {"set_model_values",(nsp_method *) _wrap_premiamodel_set_model_values},

  {"get_option_vars",(nsp_method *) _wrap_premiamodel_get_option_vars},
  {"get_option_values",(nsp_method *) _wrap_premiamodel_get_option_values},
  {"set_option_values",(nsp_method *) _wrap_premiamodel_set_option_values},

  {"get_method_vars",(nsp_method *) _wrap_premiamodel_get_method_vars},
  {"get_method_values",(nsp_method *) _wrap_premiamodel_get_method_values},
  {"set_method_values",(nsp_method *) _wrap_premiamodel_set_method_values},

  {"get_method_res_vars",(nsp_method *) _wrap_premiamodel_get_method_res_vars},
  {"get_method_res_values",(nsp_method *) _wrap_premiamodel_get_method_res_values},

  {"check",(nsp_method *) _wrap_premiamodel_check_values},
  {"check1",(nsp_method *) _wrap_premiamodel_check_values1},
  {"compute",(nsp_method *) _wrap_premiamodel_compute},
  { NULL, NULL}
};

static NspMethods *premiamodel_get_methods(void) { return premiamodel_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab premiamodel_attrs[] = {
  { NULL,NULL,NULL,NULL },
};

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab premiamodel_func[]={
  { "premiapb_create", int_premiapb_create},
  { "premiamodel_create", int_premiamodel_create},
  { "premiaoption_create", int_premiaopt_create},
  { "premiamethod_create", int_premiamethod_create},
  { "premia_get_models",int_premia_get_models},
  { "premia_get_family",int_premia_get_family},
  { "premia_get_methods",int_premia_get_methods},
  { NULL, NULL}
};

/* call ith function in the premiamodel interface */

int premiamodel_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(premiamodel_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void premiamodel_Interf_Info(int i, char **fname, function (**f))
{
  *fname = premiamodel_func[i].name;
  *f = premiamodel_func[i].fonc;
}


/* libraries 
 *
 *
 */

/* This function is used to clone an array of vars of size n.
 * 
 */

static int nsp_premia_clone_vars(VAR **res,int flag,const VAR *vars,int n)
{
  int i;
  VAR *loc,*loc2; 
  if ( flag == TRUE ) 
    {
      if ((loc = malloc(n*sizeof(VAR))) == NULL) 
	return FAIL;
      *res = loc;
    }
  else 
    {
      loc = *res;
    }
  /* now we have to check if recursive allocation is needed */
  for (i=0 ; i < n ; i++)
    {
      int count =0;
      loc[i]=vars[i];
      switch( vars[i].Vtype)
	{
	case NUMFUNC_1:
	  loc2 = (vars[i].Val.V_NUMFUNC_1)->Par; 
	  /* count how many vars are present in vars[i] */
	  while (loc2->Vtype!=END) { count++; loc2++;} 
	  /* allocate */
	  if ((loc[i].Val.V_NUMFUNC_1 = malloc(sizeof(NumFunc_1)))==NULL) return FAIL;
	  *(loc[i].Val.V_NUMFUNC_1) = *(vars[i].Val.V_NUMFUNC_1);
	  /* recursive allocation */
	  loc2 =loc[i].Val.V_NUMFUNC_1->Par;
	  nsp_premia_clone_vars(&loc2,FALSE,vars[i].Val.V_NUMFUNC_1->Par,count);
	  break;
	case NUMFUNC_2:
	  loc2 = (vars[i].Val.V_NUMFUNC_2)->Par;
	  while (loc2->Vtype!=END) { count++; loc2++;} 
	  /* allocate */
	  if ((loc[i].Val.V_NUMFUNC_2 = malloc(sizeof(NumFunc_2)))==NULL) return FAIL;
	  *(loc[i].Val.V_NUMFUNC_2) = *(vars[i].Val.V_NUMFUNC_2);
	  /* recursive allocation */
	  loc2 =loc[i].Val.V_NUMFUNC_2->Par;
	  nsp_premia_clone_vars(&loc2,FALSE, vars[i].Val.V_NUMFUNC_2->Par,count);
	  break;
	case PTVAR:
	  loc2 = (vars[i].Val.V_PTVAR)->Par;
	  while (loc2->Vtype!=END) { count++; loc2++;} 
	  if ((loc[i].Val.V_PTVAR = malloc(sizeof(PTVAR)))==NULL) return FAIL;
	  *(loc[i].Val.V_PTVAR) = *(vars[i].Val.V_PTVAR);
	  loc2 =loc[i].Val.V_PTVAR->Par;
	  nsp_premia_clone_vars(&loc2,FALSE,vars[i].Val.V_PTVAR->Par,count);
	  break;
	case DOUBLEARRAY:
	  if ((loc[i].Val.V_DOUBLEARRAY= malloc(sizeof(DOUBLEARRAY))) ==NULL) return FAIL;
	  loc[i].Val.V_DOUBLEARRAY->array = malloc(sizeof(double)*vars[i].Val.V_DOUBLEARRAY->size);
	  if ( loc[i].Val.V_DOUBLEARRAY->array == NULL) return FAIL;
	  loc[i].Val.V_DOUBLEARRAY->size = vars[i].Val.V_DOUBLEARRAY->size;
	  memcpy(loc[i].Val.V_DOUBLEARRAY->array,vars[i].Val.V_DOUBLEARRAY->array,
		 vars[i].Val.V_DOUBLEARRAY->size*sizeof(double));
	  break;
	default: 
	  break;
	}
    }
  return OK;
}

/* deallocate.
 *
 */

static void nsp_premia_free_vars(VAR *vars,int flag,int n)
{
  int i;
  VAR *loc2; 
  for (i=0 ; i < n ; i++)
    {
      int count =0;
      switch( vars[i].Vtype)
	{
	case NUMFUNC_1:
	  loc2 = (vars[i].Val.V_NUMFUNC_1)->Par; 
	  /* count how many vars are present in vars[i] */
	  while (loc2->Vtype!=END) { count++; loc2++;} 
	  nsp_premia_free_vars(loc2,FALSE,count);
	  break;
	case NUMFUNC_2:
	  loc2 = (vars[i].Val.V_NUMFUNC_2)->Par;
	  while (loc2->Vtype!=END) { count++; loc2++;} 
	  nsp_premia_free_vars(loc2,FALSE,count);
	  break;
	case PTVAR:
	  loc2 = (vars[i].Val.V_PTVAR)->Par;
	  while (loc2->Vtype!=END) { count++; loc2++;} 
	  nsp_premia_free_vars(loc2,FALSE,count);
	  break;
	case DOUBLEARRAY:
	  FREE( vars[i].Val.V_DOUBLEARRAY->array);
	  FREE( vars[i].Val.V_DOUBLEARRAY);
	  break;
	default: 
	  break;
	}
    }
  if ( flag == TRUE ) 
    FREE(vars);
}


static NspList *nsp_premia_get_var_names_util(const VAR *var,const VAR *sub_vars, int n);


/* get names and values in a list 
 *
 */

static NspList* nsp_premia_get_var_names(const VAR *vars,int n)
{
  int i;
  NspList *Res=NULL,*loc1=NULL;
  NspObject *Ob;
  NspMatrix *M;
  VAR *vars1,*vars2;
  if ((Res= nsp_list_create("X")) == NULLLIST ) return NULLLIST;
  for (i=0 ; i < n ; i++)
    {
      double val;
      int_types Ret_default[]={string,s_double, t_end};
      int_types Ret_array[]={string,realmat, t_end};
      int count =0;
      switch( vars[i].Vtype)
	{
	case NUMFUNC_1:
	  vars1=vars2 = (vars[i].Val.V_NUMFUNC_1)->Par;
	  while (vars2->Vtype!=END) { count++; vars2++;} 
	  if ((loc1=nsp_premia_get_var_names_util(&vars[i],vars1,count))== NULL) goto err;
	  if ( nsp_list_end_insert(Res,NSP_OBJECT(loc1)) == FAIL) goto err;
	  break;
	case NUMFUNC_2:
	  vars1=vars2 = (vars[i].Val.V_NUMFUNC_2)->Par;
	  while (vars2->Vtype!=END) { count++; vars2++;} 
	  if ((loc1=nsp_premia_get_var_names_util(&vars[i],vars1,count))== NULL) goto err;
	  if ( nsp_list_end_insert(Res,NSP_OBJECT(loc1)) == FAIL) goto err;
	  break;
	case PTVAR:
	  /* list(name,sublist_of-args) */
	  vars1=vars2 = (vars[i].Val.V_PTVAR)->Par;
	  while (vars2->Vtype!=END) { count++; vars2++;} 
	  if ((loc1=nsp_premia_get_var_names_util(&vars[i],vars1,count))== NULL) goto err;
	  if ( nsp_list_end_insert(Res,NSP_OBJECT(loc1)) == FAIL) goto err;
	  break;
	case DOUBLEARRAY:
	  if (( Ob = nsp_create_object_from_str("X",vars[i].Vname)) == NULLOBJ ) goto err;
	  if (( M = nsp_matrix_create_from_array("M",1,vars[i].Val.V_DOUBLEARRAY->size,
						 vars[i].Val.V_DOUBLEARRAY->array,NULL)) 
	      == NULLMAT) goto err;
	  if (( Ob = (NspObject *)BuildListFromArgs("lel",Ret_array,vars[i].Vname,M))== NULLOBJ )
	    goto err;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) goto err;
	  break;
	default: 
	  if ( get_value(&vars[i],&val) == FAIL)  goto err;
	  if (( Ob = (NspObject *)BuildListFromArgs("lel",Ret_default,vars[i].Vname,val))== NULLOBJ )
	    goto err;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) goto err;
	  break;
	}
    }
  return Res;
 err:
  if ( Res != NULL ) nsp_list_destroy(Res);
  return NULL;
}

static NspList *nsp_premia_get_var_names_util(const VAR *var,const VAR *sub_vars, int n)
{
  NspObject *Ob=NULL;
  NspList *loc1=NULL,*loc2=NULL;
  if ((loc1= nsp_list_create("X")) == NULLLIST ) goto err;
  if (( Ob = nsp_create_object_from_str("X",var->Vname)) == NULLOBJ ) goto err;
  if ( nsp_list_end_insert(loc1,Ob) == FAIL) goto err;
  if ((loc2 = nsp_premia_get_var_names(sub_vars,n))== NULLLIST ) goto err;
  if ( nsp_list_end_insert(loc1,NSP_OBJECT(loc2)) == FAIL) goto err;
  return loc1;
 err:
  if ( loc1 != NULL ) nsp_list_destroy(loc1);
  return NULL;
}
  

/* get number of vars: end is detected by END key word 
 *
 */

static int nsp_premia_get_nvar(const VAR *vars)
{
  int count=0;
  while (1) 
    {
      if( vars[count].Vtype == END ) break;
      count++;
    }
  return count;
}



/* get names and values in a list 
 *
 */

static int nsp_premia_set_var_names(VAR *vars,int n,NspList *L)
{
  NspObject *Obj,*Name,*Obj1;
  int len = nsp_list_length(L);
  int i;
  VAR *vars1,*vars2;
  if ( n != len) return FAIL;
  for (i=0 ; i < n ; i++)
    {
      double val;
      int count =0;
      Obj = nsp_list_get_element(L,i+1);
      if ( ! IsList(Obj) ) return FAIL;
      if ( nsp_list_length((NspList *)Obj) != 2 ) return FAIL;
      Name = nsp_list_get_element((NspList *)Obj,1);
      if ( ! IsString(Name) ) return FAIL;
      Obj1 = nsp_list_get_element((NspList *)Obj,2);
      if (strcmp(((NspSMatrix *) Name)->S[0],vars[i].Vname) != 0) return FAIL;
      switch( vars[i].Vtype)
	{
	case NUMFUNC_1:
	  vars1=vars2 = (vars[i].Val.V_NUMFUNC_1)->Par;
	  while (vars2->Vtype!=END) { count++; vars2++;} 
	  if ( ! IsList(Obj1) ) return FAIL;
	  if ( count != nsp_list_length((NspList *)Obj1)) return FAIL;
	  if (nsp_premia_set_var_names(vars1,count,(NspList *) Obj1) == FAIL) return FAIL;
	  break;
	case NUMFUNC_2:
	  vars1=vars2 = (vars[i].Val.V_NUMFUNC_2)->Par;
	  while (vars2->Vtype!=END) { count++; vars2++;} 
	  if ( ! IsList(Obj1) ) return FAIL;
	  if ( count != nsp_list_length((NspList *)Obj1)) return FAIL;
	  if (nsp_premia_set_var_names(vars1,count,(NspList *) Obj1) == FAIL) return FAIL;
	  break;
	case PTVAR:
	  /* list(name,sublist_of-args) */
	  vars1=vars2 = (vars[i].Val.V_PTVAR)->Par;
	  while (vars2->Vtype!=END) { count++; vars2++;} 
	  if ( ! IsList(Obj1) ) return FAIL;
	  if ( count != nsp_list_length((NspList *)Obj1)) return FAIL;
	  if (nsp_premia_set_var_names(vars1,count,(NspList *) Obj1) == FAIL) return FAIL;
	  break;
	case DOUBLEARRAY:
	  if ( ! IsMat(Obj1) ) return FAIL;
	  if ( ((NspMatrix *) Obj1)->mn != vars[i].Val.V_DOUBLEARRAY->size) return FAIL;
	  memcpy(vars[i].Val.V_DOUBLEARRAY->array,((NspMatrix *) Obj1)->R, 
		 vars[i].Val.V_DOUBLEARRAY->size*sizeof(double));
	  break;
	default: 
	  if ( ! IsMat(Obj1) ) return FAIL;
	  if ( ((NspMatrix *) Obj1)->mn != 1  ) return FAIL;
	  val = ((NspMatrix *) Obj1)->R[0];
	  set_value(&vars[i],val);
	  break;
	}
    }
  return OK;
}
