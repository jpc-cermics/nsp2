/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PList_Private 
#include "nsp/object.h"
#include "nsp/plistc.h"
#include "../system/files.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

static NspMethods *nsp_macro_get_methods(void);
/*
 * NspPList inherits from NspObject 
 */

int nsp_type_plist_id=0;
NspTypePList *nsp_type_plist=NULL;

NspTypePList *new_type_plist(type_mode mode)
{
  NspTypePList *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_plist != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_plist;
    }
  if ((type =  malloc(sizeof(NspTypePList))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* plist_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = nsp_macro_get_methods;
  type->new = (new_func *) new_plist;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for plist */ 

  top->pr = (print_func *) NspPListPrint;                    /* printing*/   
  top->dealloc = (dealloc_func *) NspPListDestroy;              /* dealloc */  
  top->copy  =  (copy_func *) NspPListCopy;                   /* copy object */  
  top->size  = (size_func *) NspPListSize;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *) NspPListType;                /* type as a String */  
  top->sh_type = (sh_type_func *) NspPListShType ;              /* type as a short string */  
  top->info = (info_func *) NspPListInfo;                    /* info */  
  /*top->is_true = (is_true_func  *) PListIsTrue;  */           /* check if object can be considered as true */  
  /*top->loop =(loop_func *) NspPListLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)  NspPListObj ;    /* get object stored in SciObj */  
  top->eq  = (eq_func *) NspPListObjEq;                       /* equality check */  
  top->neq  = (eq_func *) NspPListObjNeq;                      /* non-equality check */

  top->save  = (save_func *) NspPListXdrSave;
  top->load  = (load_func *) NspPListXdrLoad;

  top->full_copy  =  (copy_func *) NspPListCopy;                   /* copy object */  

  /* specific methods for plist */
  type->init = (init_func *) init_plist;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_plist_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_plist
       */
      type->id =  nsp_type_plist_id = nsp_new_type_id();
      nsp_type_plist = type;
      if ( nsp_register_type(nsp_type_plist) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_plist(mode);
    }
  else 
    {
      type->id = nsp_type_plist_id;
      return type;
    }

}

/*
 * initialize Plist instances 
 * locally and by calling initializer on parent class 
 */

static int init_plist(NspPList *o,NspTypePList *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PList 
 */

NspPList *new_plist() 
{
  NspPList *loc; 
  /* type must exists */
  nsp_type_plist = new_type_plist(T_BASE);
  if ( (loc = malloc(sizeof(NspPList)))== NULLP_PLIST) return loc;
  /* initialize object */
  if ( init_plist(loc,nsp_type_plist) == FAIL) return NULLP_PLIST;
  return loc;
}

/*
 * NspPListSize
 */

int NspPListSize(NspPList *Mat, int flag)
{
  return 0;
}

/*
 * MatType 
 */

static char plist_type_name[]="PList";
static char plist_short_type_name[]="pl";

char *NspPListType(void)
{
  return(plist_type_name);
}

char *NspPListShType(void)	
{
  return(plist_short_type_name);
}

int  NspPListFullComp(NspPList * A,NspPList * B,char *op,int *err)
{
  Scierror("PListFullComp: to be implemented \n");
  return FALSE;
}

int NspPListObjEq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return FALSE ;
  rep =  NspPListFullComp((NspPList *) A,(NspPList *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int NspPListObjNeq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return TRUE;
  rep =  NspPListFullComp((NspPList *) A,(NspPList *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}


/*
 * Save a NspPList
 */

static int PListXdrSave(XDR *xdrs, PList L);

int NspPListXdrSave(XDR *xdrs, NspPList *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->file_name == NULL ? "" : M->file_name) == FAIL)
    return FAIL;
  return ( PListXdrSave(xdrs,M->D) );
}

/*
 * Load a NspPList
 */

static int PListXdrLoad(XDR *xdrs, PList *plist);

NspPList *NspPListXdrLoad(XDR *xdrs)
{
  PList L=NULLPLIST,L1;
  char file_name[FSIZE]; 
  char name[NAME_MAXL]; 
  if (nsp_xdr_load_string(xdrs, name,NAME_MAXL) == FAIL) return NULLP_PLIST;
  if (nsp_xdr_load_string(xdrs, file_name,FSIZE) == FAIL) return NULLP_PLIST;
  if ( PListXdrLoad(xdrs,&L) == FAIL) return NULLP_PLIST;
  if ( L->type != PLIST ) return NULLP_PLIST;
  L1= L->O;
  L->O = NULLPLIST;
  nsp_plist_destroy(&L);
  return(  NspPListCreate(name,L1,file_name[0]== '\0' ? NULL : file_name));
}


/*
 * Save a PList to a File 
 */

int PListXdrSave_I(XDR *xdrs, PList L)
{
  nsp_xdr_save_c(xdrs,'L');
  while ( L != NULLPLIST ) 
    {
      switch ( L->type ) 
	{
	case STRING:
	  nsp_xdr_save_c(xdrs,'S');
	  nsp_xdr_save_string(xdrs,(char *) L->O);
	  break;
	case COMMENT:
	  nsp_xdr_save_c(xdrs,'C');
	  nsp_xdr_save_string(xdrs,(char *) L->O);
	  break;
	case NUMBER:
	  nsp_xdr_save_c(xdrs,'D');
	  nsp_xdr_save_string(xdrs,((parse_double *) L->O)->str);
	  break;
	case NAME :
	  nsp_xdr_save_c(xdrs,'N');
	  nsp_xdr_save_string(xdrs,(char *) L->O);
#ifdef WITH_SYMB_TABLE 
	  nsp_xdr_save_i(xdrs,L->arity);
#endif 
	  break;
	case OPNAME: 
	  nsp_xdr_save_c(xdrs,'P');
	  nsp_xdr_save_string(xdrs,(char *) L->O);
	case OBJECT : 
	  nsp_xdr_save_c(xdrs,'B');
	  if (nsp_object_xdr_save(xdrs,L->O)== FAIL)
	    return FAIL;
	  break;
	case PLIST:
	  PListXdrSave_I(xdrs, L->O);
	  break;
	case EMPTYMAT:
	  nsp_xdr_save_c(xdrs,'M');
	  nsp_xdr_save_i(xdrs, NSP_POINTER_TO_INT(L->O));
	  break; /* XXXX */
	default:
	  nsp_xdr_save_c(xdrs,'O');
	  nsp_xdr_save_i(xdrs,L->arity);
	  nsp_xdr_save_i(xdrs,L->type);
	  nsp_xdr_save_i(xdrs, NSP_POINTER_TO_INT(L->O));
	}
      L = L->next ;
    }
  nsp_xdr_save_c(xdrs,'E');  
  return OK ;
}

static int PListXdrSave(XDR *xdrs, PList L)
{
  if ( PListXdrSave_I(xdrs,L) == FAIL) return FAIL;
  return nsp_xdr_save_c(xdrs,'Z');
}

/*
 * Read a PList from a file 
 */

static int PListXdrLoad(XDR *xdrs, PList *plist)
{
  NspObject *Obj;
  int opar,op,oline,arity;
  PList loc=NULLPLIST;
  PList loc1=NULLPLIST;
  char buf[TBUF];
  char c ;
  while ( 1) 
    {
      c= EOF;
      nsp_xdr_load_c(xdrs,&c);
      switch (c) 
	{
	case 'S' : 
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_string(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'C' : 
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_comment(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'D':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_doublei(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'N':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
#ifdef WITH_SYMB_TABLE 
	  nsp_xdr_load_i(xdrs,&arity);
#else 
	  arity=-1;
#endif 
	  if (nsp_parse_add_name1(plist,buf,arity) == FAIL) return (FAIL);
	  break;
	case 'P':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_opname(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'B':
	  if ((Obj=nsp_object_xdr_load(xdrs))== NULLOBJ )  return FAIL;
	  if ( nsp_parse_add_object(plist,Obj ) == FAIL) return (FAIL);
	  break;
	case 'L':
	  loc1 = loc = NULLPLIST;
	  if (PListXdrLoad(xdrs,&loc) == FAIL) return (FAIL);
	  if (nsp_parse_add_list1(&loc1,&loc) == FAIL) return (FAIL);
	  if (nsp_parse_add_list(plist,&loc1)== FAIL)  return (FAIL);
	  break;
	case 'M': 
	  nsp_xdr_load_i(xdrs,&oline);
	  if (nsp_parse_add_last(plist,EMPTYMAT,0,oline) == FAIL) return(FAIL);
	  break;
	case 'O':
	  nsp_xdr_load_i(xdrs,&opar);
	  nsp_xdr_load_i(xdrs,&op);
	  nsp_xdr_load_i(xdrs,&oline);
	  if (nsp_parse_add_last(plist,op,opar,oline) == FAIL) return(FAIL);
	  break;
	case 'E':
	  return(OK);
	  break;
	case 'Z' :
	  return OK;
	  break;
	default: 
	  Scierror("Error:\tSomething wrong in saved plist\n");
	  return FAIL;
	}
    }
  return OK ;
}



/*
 * A = PListObj(O);
 * checks that O is an object of NspPList type. 
 * or a Hobj which points to an object of type PList
 * if so, returns a pointer to that NspPList and else returns NULL
 */

NspPList *NspPListObj(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_plist_id) == TRUE) return ((NspPList *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_plist));
  return(NULL);
}


/*
 * IsPListObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  PList 
 * or a Hobj which points to an object of type PList
 */

int IsNspPListObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_plist_id);
}

/*
 * IsPList(O)
 * only checks that object is an object of type  PList 
 * or a Hobj which points to an object of type PList
 */

int IsNspPList(NspObject *O)
{
  return nsp_object_type(O , nsp_type_plist_id);
}

/*
 * Checks that i-th object on the stack 
 * is a NspList and returns that NspList or NULLLIST 
 */

NspPList *GetNspPList(Stack stack, int i)
{
  NspPList *M;
  if (( M = NspPListObj(NthObj(i))) == NULLP_PLIST)
    ArgMessage(stack,i);
  return M;
}


/*
 * Checks that first+i object on the stack 
 * is a LIST and returns that LIST  
 * or a copy of that LIST if its name 
 * is != NVOID 
 */

NspPList *GetNspPListCopy(Stack stack, int i)
{
  if (  GetNspPList(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}



/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static int int_nsp_macro_get_name(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  /* direct access to the function name is the AST */
  char *name = ((PList) ((PList) self->D->next->O)->next->next->O)->next->O;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( name == NULL) return RET_BUG;
  if ( nsp_move_string(stack,1,name,-1)== FAIL) return RET_BUG;
  return 1;
}

static int int_nsp_macro_get_args(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  int pl_lhs,pl_rhsp1;
  CheckRhs(0,0);
  CheckLhs(0,2);
  plist_get_nargs(self->D,&pl_lhs,&pl_rhsp1);
  if ( nsp_move_double(stack,1,(double) pl_lhs )== FAIL) return RET_BUG;
  if ( lhs == 2 ) 
    {
      if ( nsp_move_double(stack,2,(double)pl_rhsp1-1 )== FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}

/* XXXXX: to be moved in .h */

extern NspList *nsp_plist_to_list(PList L);


static int int_nsp_macro_to_list(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((L= nsp_plist_to_list(self->D))== NULLLIST ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  return 1;
}

static int int_nsp_macro_to_string(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((S= NspPList2SMatrix(self,0))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}



static NspMethods nsp_macro_methods[] = {
  {"get_name",(nsp_method *) int_nsp_macro_get_name },
  {"get_args",(nsp_method *) int_nsp_macro_get_args },
  {"to_list",(nsp_method *) int_nsp_macro_to_list },
  {"to_string",(nsp_method *) int_nsp_macro_to_string },
  { NULL, NULL}
};

static NspMethods *nsp_macro_get_methods(void) { return nsp_macro_methods;}

/*----------------------------------------------------------
 * Now the interfaced function for macros (pl)
 *--------------------------------------------------------*/

/*
 * NspPList2SMatrix 
 */

int int_pl2s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  if ((S= NspPList2SMatrix(PL,0))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}

/*
 * get rhs and lhs 
 * should be changed to also return opt 
 * FIXME: should be a method of NspPList 
 */

static int int_inout(Stack stack, int rhs, int opt, int lhs)
{
  int pl_lhs,pl_rhsp1;
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  plist_get_nargs(PL->D,&pl_lhs,&pl_rhsp1);
  if ( nsp_move_double(stack,1,(double)pl_lhs )== FAIL) return RET_BUG;
  if ( lhs == 2 ) 
    {
      if ( nsp_move_double(stack,2,(double)pl_rhsp1-1 )== FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}


static int int_print_internal(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  CheckRhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  nsp_plist_print_internal(PL->D);
  return 0;
}

static int int_plist_to_list(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  NspPList *PL;
  CheckRhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  if ((L= nsp_plist_to_list(PL->D))== NULLLIST ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  return 1;
}



/*
 * The Interface for parsed lists
 */

static OpTab NspPList_func[]={
  {"pl2s", int_pl2s},
  {"pl2l", int_plist_to_list},
  {"inout", int_inout},
  {"print_internal", int_print_internal},
  {(char *) 0, NULL}
};

int NspPList_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(NspPList_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void NspPList_Interf_Info(int i, char **fname, function (**f))
{
  *fname = NspPList_func[i].name;
  *f = NspPList_func[i].fonc;
}

