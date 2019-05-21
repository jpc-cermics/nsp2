/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h>
#include <nsp/objects.h>

#define PList_Private
#include <nsp/type.h>
#include <nsp/plist.h>
#include <nsp/plistc.h>
#include <nsp/ivect.h>
#include <nsp/file.h>
#include <nsp/list.h>
#include <nsp/ast.h>
#include <nsp/cells.h>
#include <nsp/frame.h>
#include <nsp/system.h>
#include "nsp/pr-output.h"
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/libstab.h"

extern const char *nsp_get_libdir(int num);

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
  type->gtk_methods = FALSE;
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
  top->latex = (print_func *) NspPListPrint_latex;
  
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
  if ( check_cast(B,nsp_type_plist_id) == FALSE) return FALSE ;
  if ( nsp_plist_equal(((NspPList *) A)->D,((NspPList *) B)->D) == TRUE)
    return TRUE;
  return FALSE;
}

int NspPListObjNeq(NspObject *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_plist_id) == FALSE) return TRUE;
  if ( nsp_plist_equal(((NspPList *) A)->D,((NspPList *) B)->D) == TRUE)
    return FALSE;
  return TRUE;
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

#define TBUF TOK_BUF

static int PListXdrLoad(XDR *xdrs, PList *plist,char *buf);

NspPList *NspPListXdrLoad(XDR *xdrs)
{
  char buf[TBUF];
  PList L=NULLPLIST,L1;
  char file_name[FSIZE];
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs, name,NAME_MAXL) == FAIL) return NULLP_PLIST;
  if (nsp_xdr_load_string(xdrs, file_name,FSIZE) == FAIL) return NULLP_PLIST;
  if ( PListXdrLoad(xdrs,&L,buf) == FAIL) return NULLP_PLIST;
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
	case INUMBER32:
	  nsp_xdr_save_c(xdrs,'I');
	  nsp_xdr_save_string(xdrs,((parse_int *) L->O)->str);
	  break;
	case INUMBER64:
	  nsp_xdr_save_c(xdrs,'J');
	  nsp_xdr_save_string(xdrs,((parse_int *) L->O)->str);
	  break;
	case UNUMBER32:
	  nsp_xdr_save_c(xdrs,'U');
	  nsp_xdr_save_string(xdrs,((parse_int *) L->O)->str);
	  break;
	case UNUMBER64:
	  nsp_xdr_save_c(xdrs,'V');
	  nsp_xdr_save_string(xdrs,((parse_int *) L->O)->str);
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
	  break;
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

static int PListXdrLoad(XDR *xdrs, PList *plist,char *buf)
{
  NspObject *Obj;
  int opar,op,oline,arity;
  PList loc=NULLPLIST;
  PList loc1=NULLPLIST;
  char c ;
  /* static int count = 0; count++;
   * Sciprintf("Enter PListXdrLoad %d\n",count);
   */
  while ( 1)
    {
      c= EOF;
      nsp_xdr_load_c(xdrs,&c);
      switch (c)
	{
	case 'S' :
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  /* type of string should be saved/loaded */
	  if (nsp_parse_add_string(plist,buf,0) == FAIL) goto fail;
	  break;
	case 'C' :
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_comment(plist,buf) == FAIL) goto fail;
	  break;
	case 'D':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_doublei(plist,buf) == FAIL) goto fail;
	  break;
	case 'I':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_inti(plist,buf, INUMBER32) == FAIL) goto fail;
	  break;
	case 'J':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_inti(plist,buf, INUMBER64) == FAIL) goto fail;
	  break;
	case 'U':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_inti(plist,buf, UNUMBER32) == FAIL) goto fail;
	  break;
	case 'V':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_inti(plist,buf, UNUMBER64) == FAIL) goto fail;
	  break;
	case 'N':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
#ifdef WITH_SYMB_TABLE
	  nsp_xdr_load_i(xdrs,&arity);
#else
	  arity=-1;
#endif
	  if (nsp_parse_add_name1(plist,buf,arity) == FAIL) goto fail;
	  break;
	case 'P':
	  nsp_xdr_load_string(xdrs,buf,TBUF);
	  if (nsp_parse_add_opname(plist,buf) == FAIL) goto fail;
	  break;
	case 'B':
	  if ((Obj=nsp_object_xdr_load(xdrs))== NULLOBJ ) goto fail;
	  if ( nsp_parse_add_object(plist,Obj ) == FAIL) goto fail;
	  break;
	case 'L':
	  loc1 = loc = NULLPLIST;
	  if (PListXdrLoad(xdrs,&loc,buf) == FAIL) goto fail;
	  if (nsp_parse_add_list1(&loc1,&loc) == FAIL) goto fail;
	  if (nsp_parse_add_list(plist,&loc1)== FAIL)  goto fail;
	  break;
	case 'M':
	  nsp_xdr_load_i(xdrs,&oline);
	  if (nsp_parse_add_last(plist,EMPTYMAT,0,oline) == FAIL) goto fail;
	  break;
	case 'O':
	  nsp_xdr_load_i(xdrs,&opar);
	  nsp_xdr_load_i(xdrs,&op);
	  nsp_xdr_load_i(xdrs,&oline);
	  if (nsp_parse_add_last(plist,op,opar,oline) == FAIL) goto fail;
	  break;
	case 'E': goto ok; break;
	case 'Z' :goto ok; break;
	  break;
	default:
	  Scierror("Error:\tSomething wrong in saved plist\n");
	  goto fail;
	}
    }
 ok:   /*count--; Sciprintf("Quit PListXdrLoad\n"); */
  return OK ;
 fail : /* count--;  Sciprintf("Quit PListXdrLoad\n");*/
  return FAIL;
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

static int int_nsp_macro_get_nargs(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  int pl_lhs,pl_rhsp1;
  CheckRhs(0,0);
  CheckLhs(0,2);
  nsp_plist_get_nargs(self->D,&pl_lhs,&pl_rhsp1,NULL,NULL);
  if ( nsp_move_double(stack,1,(double) pl_lhs )== FAIL) return RET_BUG;
  if ( lhs == 2 )
    {
      if ( nsp_move_double(stack,2,(double)pl_rhsp1-1 )== FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}

static int int_nsp_macro_get_args(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *in=NULL,*out=NULL;
  int pl_lhs,pl_rhsp1;
  CheckRhs(0,0);
  CheckLhs(0,2);
  if ( lhs >= 1)
    {
      in = nsp_smatrix_create(NVOID,0,0,"v",0);
    }
  if ( lhs >= 2)
    {
      out = nsp_smatrix_create(NVOID,0,0,"v",0);
    }
  nsp_plist_get_nargs(self->D,&pl_lhs,&pl_rhsp1,in,out);
  if ( lhs >= 1)
    MoveObj(stack,1,NSP_OBJECT(in));
  if ( lhs >= 2)
    MoveObj(stack,2,NSP_OBJECT(out));
  return Max(lhs,0);
}

/* XXXXX: to be moved in .h */

extern NspList *nsp_plist_to_list(const char *name,PList L);

static int int_nsp_macro_to_list(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((L= nsp_plist_to_list(NVOID,self->D))== NULLLIST ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  return 1;
}

static int int_nsp_macro_reset_persistents(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{

  NspCells *T = (NspCells *) self->D->next->next->next->O;
  NspBHash *H = (NspBHash *) T->objs[0];
  int i=0,val;
  CheckStdRhs(0,1);
  CheckLhs(0,1);
  if ( rhs - opt == 0)
    {
      while (1)
	{
	  char *str=NULL;
	  int rep = nsp_bhash_get_next_object(H,&i,&str,&val);
	  if ( str != NULL && VAR_IS_PERSISTENT(val))
	    {
	      val = VAR_ID(val);
	      if ( ((NspCells *) T->objs[2])->objs[val] != NULLOBJ )
		{
		  nsp_object_destroy(&((NspCells *) T->objs[2])->objs[val]);
		  ((NspCells *) T->objs[2])->objs[val]=NULL;
		}
	    }
	  if (rep == FAIL) break;
	}
    }
  else
    {
      NspSMatrix *S;
      if  ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
      for ( i= 0 ; i < S->mn ; i++)
	{
	  if ( nsp_bhash_find(H, S->S[i],&val) == OK && VAR_IS_PERSISTENT(val) )
	    {
	      val = VAR_ID(val);
	      if ( ((NspCells *) T->objs[2])->objs[val] != NULLOBJ )
		{
		  nsp_object_destroy(&((NspCells *) T->objs[2])->objs[val]);
		  ((NspCells *) T->objs[2])->objs[val]=NULL;
		}
	    }
	}
    }
  return 0;
}

/* returns cpu and nb of calls */

static int int_nsp_macro_get_cpu(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  if (( M = nsp_matrix_create(NVOID,'r',1,2) ) == NULLMAT ) return RET_BUG;
  M->R[0] = self->cpu;
  M->R[1] = self->counter;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

/* reset cpu and nb of calls */

static int int_nsp_macro_reset_cpu(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckStdRhs(0,0);
  CheckLhs(0,0);
  self->cpu=0;
  self->counter=0;
  return 0;
}

static int int_nsp_macro_clear_cache(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  const char *dir= nsp_get_libdir(self->dir);
  if ( dir != NULL )
    {
      /* if dir is non-null then the macros is from library */
      if ( self->D != NULL) nsp_plist_destroy(&self->D);
      self->D = NULL;
    }
  return 0;
}

static int int_nsp_macro_to_string(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( self == NULL ) return RET_BUG;
  if ((S= nsp_plist2smatrix(self->D,0))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}

static int int_nsp_macro_get_path(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  char fname[FSIZE+1];
  CheckRhs(0,0);
  CheckLhs(0,1);
  nsp_plist_get_path(fname,self);
  if (nsp_move_string(stack,1,fname,-1)==FAIL) return RET_BUG;
  return 1;
}

/* very similar to the generic function for printing objects and 
 * redirection of output to string, file or stdout
 */

typedef enum { string_out, stdout_out, file_out } print_mode;

static int int_plist_print_gen(NspPList *self,Stack stack, int rhs, int opt, int lhs, print_mode mode);

static int int_plist_print(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  return int_plist_print_gen(self,stack,rhs,opt,lhs,stdout_out);
}

static int int_plist_sprint(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  return int_plist_print_gen(self,stack,rhs,opt,lhs,string_out);
}

static int int_plist_fprint(NspPList *self,Stack stack, int rhs, int opt, int lhs)
{
  return int_plist_print_gen(self,stack,rhs,opt,lhs,file_out);
}

/* This method shares code with the same generic method for ast and could be 
 * shared 
 */

static int int_plist_print_gen(NspPList *self,Stack stack, int rhs, int opt, int lhs, print_mode mode)
{
  int target = 4, columns = 90;
  char *s_target = NULL;
  const char *targets[]={"html", "gtk", "latex", "term", NULL };
  NspFile *F=NULL;
  FILE *f=NULL;
  IOVFun def=NULL ;
  MoreFun mf=NULL; 
  
  NspObject *res;
  int dp=user_pref.pr_depth;
  int cr=user_pref.color;
  int as_read=FALSE,depth=INT_MAX,indent=0,color=TRUE,html=FALSE,gtk=FALSE;
  nsp_option print_opts[] ={{ "as_read",s_bool,NULLOBJ,-1},
			    { "color",s_bool,NULLOBJ,-1},
			    { "depth", s_int,NULLOBJ,-1},
			    { "indent",s_int,NULLOBJ,-1},
			    { "target",string,NULLOBJ,-1},
			    { "columns",s_int, NULLOBJ,-1},
			    { NULL,t_end,NULLOBJ,-1}};
			    
  if ( mode == file_out ) 
    {
      CheckStdRhs(1,1);
      if ((F= GetSciFile(stack,1))== NULL) return RET_BUG; 
    }
  else 
    {
      CheckStdRhs(0,0);
    }
  CheckLhs(0,1);

  if ( mode == string_out) color=FALSE;
  
  if ( get_optional_args(stack, rhs, opt, print_opts,
			    &as_read,&color,&depth,&indent,&s_target,&columns) == FAIL) 
    return RET_BUG;

  if ( s_target != NULL ) 
    {
      int rep;
      if ( (rep= is_string_in_array(s_target,targets,0)) == -1 )
	{
	  string_not_in_array(stack,s_target,targets , "optional argument targets");
	  return RET_BUG;
	}
      target = rep+1;
    }
  
  /* initialize according to mode */
  switch ( mode ) 
    {
    case string_out: 
      def = SetScilabIO(Sciprint2string);
      mf =nsp_set_nsp_more(scimore_void);
      break;
    case stdout_out:
      break;
    case file_out : 
      /* changes io in order to write to file F */
      if ( !IS_OPENED(F->obj->flag))
	{
	  Scierror("Warning:\tfile %s is already closed\n",F->obj->fname);
	  return RET_BUG;
	}
      f=Sciprint_file(F->obj->file); 
      def = SetScilabIO(Sciprint2file);
      mf =nsp_set_nsp_more(scimore_void);
      break;
    }
  /* print object */
  user_pref.pr_depth= depth;
  user_pref.color=color;
  
  if ( gtk == TRUE )
    {
      user_pref.color= color = TRUE;
    }
  
  if ( html == TRUE )
    {
      /*
	.nsp_code  { background: #EEEEEE; color: Black;}
	.nsp_code .code { color : Black; }
	.nsp_code .keyword { color : blue; }
	.nsp_code .comment { color : red;}
	.nsp_code .string { color : brown;}
	.nsp_code .number { color : YellowGreen; }
	.nsp_code .function { color : SkyBlue ;  font-weight: bold;}
      */
      
      user_pref.color= color = TRUE;
      Sciprintf2(indent," ","<!-- style directives ");
      Sciprintf2(indent," ","<head>\n");
      Sciprintf2(indent+2," ","<style>\n");
      Sciprintf2(indent+4," ",".nsp_code  { background:  WhiteSmoke; color: Black; font-size: 120%;}\n");
      Sciprintf2(indent+4," ",".nsp_code .code { color : Black; }\n");
      Sciprintf2(indent+4," ",".nsp_code .keyword { color : MediumPurple; }\n");
      Sciprintf2(indent+4," ",".nsp_code .comment { color :  OrangeRed; }\n");
      Sciprintf2(indent+4," ",".nsp_code .string { color : LightSalmon; }\n");
      Sciprintf2(indent+4," ",".nsp_code .number { color : YellowGreen; }\n");
      Sciprintf2(indent+4," ",".nsp_code .function { color : SkyBlue ;  font-weight: bold;}\n");
      Sciprintf2(indent+2," ","</style>\n");
      Sciprintf2(indent," ","</head>\n");
      Sciprintf2(indent," ","-->\n");
      Sciprintf2(indent," ","%s\n","<div class=\"nsp_code\">");
			    Sciprintf2(indent+2," ","%s\n","<pre class=\"code\">");
			    nsp_plist_pretty_print(self->D, indent + 2, user_pref.color, target, TRUE,columns);
      Sciprintf2(indent+2," ","\n");
      Sciprintf2(indent+2," ","%s\n","</pre>");
      Sciprintf2(indent," ","%s\n","</div>");
    }
  else
    {
      nsp_plist_pretty_print(self->D, indent, user_pref.color, target, FALSE,columns);
    }
  user_pref.color=cr;
  user_pref.pr_depth= dp;
  /* restore to default values */
  switch ( mode ) 
    {
    case string_out: 
      res = Sciprint2string_reset(); 
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      if ( res == NULL) return RET_BUG; 
      MoveObj(stack,1, res);
      return 1;
    case stdout_out: 
      return 0;
    case file_out:
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      Sciprint_file(f); 
      return 0;
    }
  return 0;
}

static NspMethods nsp_macro_methods[] = {
  {"get_fname",(nsp_method *) int_nsp_macro_get_name },
  {"get_nargs",(nsp_method *) int_nsp_macro_get_nargs },
  {"get_args",(nsp_method *) int_nsp_macro_get_args },
  {"get_path",(nsp_method *) int_nsp_macro_get_path },
  {"to_list",(nsp_method *) int_nsp_macro_to_list },
  {"to_string",(nsp_method *) int_nsp_macro_to_string },
  {"reset_persistents",(nsp_method *) int_nsp_macro_reset_persistents },
  {"clear_cache", (nsp_method *) int_nsp_macro_clear_cache},
  {"get_cpu",(nsp_method *) int_nsp_macro_get_cpu },
  {"reset_cpu",(nsp_method *) int_nsp_macro_reset_cpu },
  {"sprint",(nsp_method *) int_plist_sprint},
  {"fprint",(nsp_method *) int_plist_fprint},
  {"print",(nsp_method *)  int_plist_print},
  { NULL, NULL}
};

static NspMethods *nsp_macro_get_methods(void) { return nsp_macro_methods;}

/*----------------------------------------------------------
 * Now the interfaced function for macros (pl)
 *--------------------------------------------------------*/

int int_pl2s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  if ((S= nsp_plist2smatrix(PL->D,0))== NULL) return RET_BUG;
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
  nsp_plist_get_nargs(PL->D,&pl_lhs,&pl_rhsp1,NULL,NULL);
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
  if ((L= nsp_plist_to_list(NVOID,PL->D))== NULLLIST ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  return 1;
}

extern NspAst *nsp_plist_to_ast(const char *name,PList L);

static int int_plist_to_ast(Stack stack, int rhs, int opt, int lhs)
{
  NspAst *L;
  NspPList *PL;
  CheckRhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  if ((L= nsp_plist_to_ast(NVOID,PL->D))== NULLAST ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  return 1;
}

static int int_collect_cputime(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if (( H = nsp_collect_cpu())== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

static int int_init_cputime(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,0);
  nsp_init_cpu();
  return 0;
}


/*
 * The Interface for parsed lists
 */

static OpTab NspPList_func[]={
  {"pl2s", int_pl2s},
  {"pl2l", int_plist_to_list},
  {"pl2ast", int_plist_to_ast},
  {"inout", int_inout},
  {"print_internal", int_print_internal},
  {"collect_cputime", int_collect_cputime},
  {"init_cputime", int_init_cputime},
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
