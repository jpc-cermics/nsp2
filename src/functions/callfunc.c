/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * Call a function given (Int,Num) 
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "nsp/stack.h" 
#include "nsp/math.h"
#include "nsp/plisttoken.h" /* for  name_maxl 52 */
#include "nsp/sciio.h"
#include "nsp/interf.h"
#include "addinter.h"
#include "callfunc.h"

extern  sci_interface  Matrix_Interf ;extern  interface_info  Matrix_Interf_Info ;
extern  sci_interface  SMatrix_Interf  ;extern  interface_info  SMatrix_Interf_Info  ;
extern  sci_interface  PMatrix_Interf  ;extern  interface_info  PMatrix_Interf_Info  ;
extern  sci_interface  BMatrix_Interf  ;extern  interface_info  BMatrix_Interf_Info  ;
extern  sci_interface  B2mMatrix_Interf  ;extern  interface_info  B2mMatrix_Interf_Info  ;
extern  sci_interface  Hash_Interf  ;extern  interface_info  Hash_Interf_Info  ;
extern  sci_interface  NspPList_Interf  ;extern  interface_info  NspPList_Interf_Info  ;
extern  sci_interface  System_Interf  ;extern  interface_info  System_Interf_Info  ;
extern  sci_interface  IVect_Interf  ;extern  interface_info  IVect_Interf_Info  ;
extern  sci_interface  List_Interf  ;extern  interface_info  List_Interf_Info  ;
extern  sci_interface  Obj_Interf  ;extern  interface_info  Obj_Interf_Info  ;
extern  sci_interface  File_Interf  ;extern  interface_info  File_Interf_Info  ;
extern  sci_interface  Parse_Interf ;extern  interface_info  Parse_Interf_Info ;
extern  sci_interface  SpRowMatrix_Interf  ;extern  interface_info  SpRowMatrix_Interf_Info  ;
extern  sci_interface  SpColMatrix_Interf  ;extern  interface_info  SpColMatrix_Interf_Info  ;
extern  sci_interface  Functions_Interf  ;extern  interface_info  Functions_Interf_Info  ;
extern  sci_interface  Function_Interf  ;extern  interface_info  Function_Interf_Info  ;
extern  sci_interface  mex_Interf  ;extern  interface_info  mex_Interf_Info  ;
extern  sci_interface  Interf_Interf  ;extern  interface_info  Interf_Interf_Info  ;
extern  sci_interface  Datas_Interf ;extern  interface_info  Datas_Interf_Info ;
extern  sci_interface  Graphics_Interf ;extern  interface_info  Graphics_Interf_Info ;
extern  sci_interface  None_Interf ;extern  interface_info  None_Interf_Info ;
extern  sci_interface  Menus_Interf ;extern  interface_info  Menus_Interf_Info ;
extern  sci_interface  Hobj_Interf ;extern  interface_info  Hobj_Interf_Info ;
extern  sci_interface  Rect_Interf ;extern  interface_info  Rect_Interf_Info ;
extern  sci_interface  GFrame_Interf ;extern  interface_info  GFrame_Interf_Info ;
extern  sci_interface  Block_Interf ;extern  interface_info  Block_Interf_Info ;
extern  sci_interface  Link_Interf ;extern  interface_info  Link_Interf_Info ;
extern  sci_interface  ClassA_Interf ;extern  interface_info  ClassA_Interf_Info ;
extern  sci_interface  ClassB_Interf ;extern  interface_info  ClassB_Interf_Info ;
extern  sci_interface  ClassARef_Interf ;extern  interface_info  ClassARef_Interf_Info ;
extern  sci_interface  ClassBRef_Interf ;extern  interface_info  ClassBRef_Interf_Info ;
extern  sci_interface  ClassC_Interf ;extern  interface_info  ClassC_Interf_Info ;
extern  sci_interface  GBoxed_Interf ;extern  interface_info  GBoxed_Interf_Info ;
extern  sci_interface  GObject_Interf ;extern  interface_info  GObject_Interf_Info ;
extern  sci_interface  GdkAtom_Interf ;extern  interface_info  GdkAtom_Interf_Info ;
extern  sci_interface  gtk_Interf ;extern  interface_info  gtk_Interf_Info ;
extern  sci_interface  gdk_Interf ;extern  interface_info  gdk_Interf_Info ;
extern  sci_interface  atk_Interf ;extern  interface_info  atk_Interf_Info ;
extern  sci_interface  pango_Interf ;extern  interface_info  pango_Interf_Info ;
extern  sci_interface  Type_Interf ;extern  interface_info  Type_Interf_Info ;
extern  sci_interface  Dcd_Interf ;extern  interface_info  Dcd_Interf_Info ;
extern  sci_interface  Approx_Interf ;extern  interface_info  Approx_Interf_Info ;
extern  sci_interface  Spmf_Interf ;extern  interface_info  Spmf_Interf_Info ;
extern  sci_interface  Random_Interf ;extern  interface_info  Random_Interf_Info ;
extern  sci_interface  Lapack_Interf ;extern  interface_info  Lapack_Interf_Info ;
extern  sci_interface  Fft_Interf ;extern  interface_info  Fft_Interf_Info ;
extern  sci_interface  Module_Interf ;extern  interface_info  Module_Interf_Info ;
extern  sci_interface  ModuleElt_Interf ;extern  interface_info  ModuleElt_Interf_Info ;
extern  sci_interface  gdate_Interf ;extern  interface_info  gdate_Interf_Info ;
extern  sci_interface  BHash_Interf  ;extern  interface_info  BHash_Interf_Info  ;
extern  sci_interface  Cells_Interf ;extern  interface_info  Cells_Interf_Info ;
extern  sci_interface  Sound_Interf ;extern  interface_info  Sound_Interf_Info ;
extern  sci_interface  Pvm_Interf ;extern  interface_info  Pvm_Interf_Info ;
extern  sci_interface  Scicos_Interf ;extern  interface_info  Scicos_Interf_Info ;
extern  sci_interface  kiko_Interf ;extern  interface_info  kiko_Interf_Info ;
extern  sci_interface  mpz_Interf ;extern  interface_info  mpz_Interf_Info ;
extern  sci_interface  premiamodel_Interf ;extern  interface_info  premiamodel_Interf_Info ;
extern  sci_interface  gmarkup_node_Interf ;extern  interface_info  gmarkup_node_Interf_Info ;
extern  sci_interface  scalexp_Interf ;extern  interface_info  scalexp_Interf_Info ;
extern  sci_interface  Mtlb_Interf ;extern  interface_info  Mtlb_Interf_Info ;

extern  sci_interface  MpMatrix_Interf ;extern  interface_info  MpMatrix_Interf_Info ;
#ifdef WITH_MAXPLUSLIB
extern  sci_interface  Maxplus_Interf ;extern  interface_info  Maxplus_Interf_Info ;
#endif 

#ifdef EXTEND 
extern  sci_interface  ClassD_Interf ;extern  interface_info  ClassD_Interf_Info ;
extern  sci_interface  Gtk_Interf ;extern  interface_info  Gtk_Interf_Info ;
#endif 

#ifdef WITH_SNDFILE
extern  sci_interface  SndFile_Interf ;extern  interface_info  SndFile_Interf_Info ;
#endif 

#ifdef WITH_UMFPACK 
extern  sci_interface umfpack_Interf ;extern  interface_info  umfpack_Interf_Info ;
#endif 

/* #define WITH_SQLITE3  */

#ifdef WITH_SQLITE3 
extern  sci_interface sqlc_Interf ;extern  interface_info  sqlc_Interf_Info ;
#endif 

/* #define WITH_DCLASS */
#ifdef WITH_DCLASS
extern  sci_interface DClass_Interf ;extern  interface_info  DClass_Interf_Info ;
#endif

/* #define WITH_LIBAMOS */
#ifdef WITH_LIBAMOS
extern  sci_interface AmosCephes_Interf ;extern  interface_info  AmosCephes_Info ;
#endif 

InterfTab Interfaces[]={
  {Matrix_Interf,Matrix_Interf_Info},
  {Hash_Interf,Hash_Interf_Info},
  {NspPList_Interf,NspPList_Interf_Info},
  {System_Interf,System_Interf_Info},
  {IVect_Interf,IVect_Interf_Info},
  {Obj_Interf,Obj_Interf_Info},
  {BMatrix_Interf,BMatrix_Interf_Info},
  {B2mMatrix_Interf,B2mMatrix_Interf_Info},
  {List_Interf,List_Interf_Info},
  {SMatrix_Interf,SMatrix_Interf_Info},
  {File_Interf,File_Interf_Info},
  {SpRowMatrix_Interf,SpRowMatrix_Interf_Info},
  {SpColMatrix_Interf,SpColMatrix_Interf_Info}, 
  {Parse_Interf,Parse_Interf_Info},
  {Functions_Interf,Functions_Interf_Info},
  {Function_Interf,Function_Interf_Info},
  {mex_Interf,mex_Interf_Info},
  {Interf_Interf,Interf_Interf_Info},
  {Datas_Interf,Datas_Interf_Info},
  {Graphics_Interf,Graphics_Interf_Info},
  {Menus_Interf,Menus_Interf_Info},
  {Hobj_Interf,Hobj_Interf_Info},
  {Rect_Interf,Rect_Interf_Info},
  {GFrame_Interf,GFrame_Interf_Info},
  {Block_Interf,Block_Interf_Info},
  {Link_Interf,Link_Interf_Info},
  {ClassA_Interf,ClassA_Interf_Info},
  {ClassB_Interf,ClassB_Interf_Info},
  {ClassARef_Interf,ClassARef_Interf_Info},
  {ClassBRef_Interf,ClassBRef_Interf_Info},
  {ClassC_Interf,ClassC_Interf_Info},
  {GBoxed_Interf,GBoxed_Interf_Info},
  {GObject_Interf,GObject_Interf_Info},
  {None_Interf,None_Interf_Info},
  {GdkAtom_Interf,GdkAtom_Interf_Info},
  {gtk_Interf,gtk_Interf_Info},
  {gdk_Interf,gdk_Interf_Info},
  {atk_Interf,atk_Interf_Info},
  {pango_Interf,pango_Interf_Info},
  {Type_Interf,Type_Interf_Info},
  {Dcd_Interf , Dcd_Interf_Info },
  {Approx_Interf , Approx_Interf_Info },
  {Spmf_Interf , Spmf_Interf_Info },
  {Random_Interf , Random_Interf_Info },
  {Lapack_Interf , Lapack_Interf_Info },
  {Fft_Interf , Fft_Interf_Info },
  {Module_Interf , Module_Interf_Info },
  {ModuleElt_Interf , ModuleElt_Interf_Info },
#ifdef EXTEND 
  {ClassD_Interf,ClassD_Interf_Info},
  {Gtk_Interf,Gtk_Interf_Info},
#endif
  {MpMatrix_Interf, MpMatrix_Interf_Info},
#ifdef WITH_MAXPLUSLIB
  {Maxplus_Interf, Maxplus_Interf_Info},
#endif 
  {Cells_Interf, Cells_Interf_Info},
  {PMatrix_Interf,PMatrix_Interf_Info},
  {Scicos_Interf,Scicos_Interf_Info},
  /* 
  {Sound_Interf , Sound_Interf_Info},
  {kiko_Interf , kiko_Interf_Info},
  {mpz_Interf , mpz_Interf_Info},
  */
#ifdef WITH_PVM
  {Pvm_Interf , Pvm_Interf_Info},
#endif 
#ifdef WITH_SNDFILE
  {SndFile_Interf , SndFile_Interf_Info},
#endif
  {BHash_Interf , BHash_Interf_Info},
#ifdef WITH_UMFPACK
  {umfpack_Interf , umfpack_Interf_Info},
#endif
#ifdef WITH_PREMIA
  {premiamodel_Interf , premiamodel_Interf_Info},
#endif
  {gmarkup_node_Interf , gmarkup_node_Interf_Info},
  {scalexp_Interf , scalexp_Interf_Info},
  {gdate_Interf,  gdate_Interf_Info},
#ifdef WITH_SQLITE3 
  {sqlc_Interf , sqlc_Interf_Info},
#endif
#ifdef WITH_DCLASS
  {DClass_Interf , DClass_Interf_Info},
#endif
  {Mtlb_Interf, Mtlb_Interf_Info},
#ifdef WITH_LIBAMOS
  {AmosCephes_Interf, AmosCephes_Info},
#endif 
  {NULL,NULL}
}; 

/**********************************************
 * Call function number num in interface i 
 **********************************************/


static int show_returned_positions(Stack stack,int pos);
/* static int  reorder_follow_cycle(Stack stack,int pos); */
int reorder_stack(Stack stack, int ret) ;

/* XXXXX Only in DEBUG Mode */

void nsp_check_stack( Stack stack, int rhs, int opt, int lhs,char *message,char *name)
{
  int count=0;
  NspObject**O;
  if ( stack.first > 0 ) 
    {
      int i;
      /* check that objects at the begining of the stack are non nul */
      for ( i=0 ; i < stack.first ; i++ ) 
	{
	  if ( stack.val->S[i] == NULL) 
	    {
	      fprintf(stderr,"Null object (%d) before stack.first=%d \n",i,stack.first);
	      break;
	    }
	}
    }
  if ( stack.val->S[stack.first + rhs] != NULL )
    {
      /* check that stack is null terminated */ 
      fprintf(stderr,"%s %s \n",message, NspFname(stack));
      fprintf(stderr,"Non null objects found after rhs(=%d) !\n",rhs);
      if ( name != 0) 
	fprintf(stderr,"previous call %s\n",name);
      fprintf(stderr,"stack.val->S=<%lx>, first=%d\n",(long) stack.val->S,stack.first);
      O = stack.val->S + stack.first+rhs; 
      while ( *O != NULL) 
	{ 
	  nsp_object_info(*O,1,NULL,0); 
	  *O = NULL;
	  O++;
	}
      fprintf(stderr,"I clean the stack and continue \n");
    }
  
  /* check that object on the stack have correct ret_pos initialization */ 

  O = stack.val->S + stack.first; 
  while ( *O != NULL ) 
    {
      count++;
      if ( (*O)->ret_pos != -1 ) 
	{
	  fprintf(stderr,"%s %s \n",message,  NspFname(stack));
	  fprintf(stderr,"Stack is corrupted ret_pos(=%d) !=-1 for object at position %d ! but I go on ",
		  (*O)->ret_pos,count);
	  if ( name != 0) 
	    fprintf(stderr,"previous call %s\n",name);
	  fprintf(stderr,"stack.val->S=<%lx>, first=%d\n",(long) stack.val->S,stack.first);
	  nsp_object_info(*O,1,NULL,0);
	  fprintf(stderr,"I change ret_pos and continue \n");
	  (*O)->ret_pos = -1 ;
	}
      O++;
    }  
}


int nsp_interfaces(int i, int num, Stack stack, int rhs, int opt, int lhs)
{
  int ret;

  /* debug */ 
  static int first = 0;
  static char buf[128];
/*   nsp_check_stack(stack,rhs,opt,lhs,"Something wrong before entering interface for",(first == 0) ? NULL: buf); */
  first=1; 
  strcpy(buf,NspFname(stack));
  
  if ( i >= DYN_INTERF_START ) 
    {
      /** interface is a dynamically linked one **/
      /* we check nothing here if k=(i - DYN_INTERF_START) 
       * is in the range [0,MAXINTERF[ DynInterf is bound to something 
       */
      ret = (*(DynInterf[i - DYN_INTERF_START].func))(num,stack,rhs,opt,lhs);
    }
  else 
    {
      /** Standard interfaces **/
      ret = (*(Interfaces[i].fonc))(num,stack,rhs,opt,lhs);
    }
  
  if ( ret == RET_BUG || ret == RET_ERROR_RAISED ) 
    {
      /* clean the stack before returning */
      NspObject**O = stack.val->S + stack.first; 
      while ( *O != NULL) 
	{ 
	  (*O)->ret_pos= -1;
	  O++;
	}
      reorder_stack(stack,0);
      return ret;
    }

  ret = reorder_stack(stack,ret);
  
  if ( ret == RET_BUG ) 
    {
      /* XXXX */
      return RET_BUG;
    }

  return ret;
}

/* 
 * direct call to an interface 
 */

int call_interf(function *f, Stack stack, int rhs, int opt, int lhs)
{
  int ret;

  /* debug */ 
  static int first = 0;
  static char buf[128];
/*   nsp_check_stack(stack,rhs,opt,lhs,"Something wrong before entering interface for",(first == 0) ? NULL: buf); */
  first=1; 
  strcpy(buf,NspFname(stack));
  
  /** Standard interfaces **/
  ret = (*f)(stack,rhs,opt,lhs);
  
  if ( ret == RET_BUG ) 
    {
      /* clean the stack before returning */
      NspObject**O = stack.val->S + stack.first; 
      while ( *O != NULL) 
	{ 
	  (*O)->ret_pos= -1;
	  O++;
	}
      reorder_stack(stack,0);
      return RET_BUG;
    }

  ret = reorder_stack(stack,ret);
  
  if ( ret == RET_BUG ) 
    {
      /* XXXX */
      return RET_BUG;
    }

  return ret;
}

/*------------------------------------------------------
 * reorder the stack after a function call 
 *------------------------------------------------------*/


int  reorder_stack(Stack stack, int ret)
{
  NspObject **O1=stack.val->S+stack.first, **obj=stack.val->S+stack.first-1;
  NspObject*O,*O2;
  int count=0, k, kn, ret_pos, must_be_reordered=0;
 
  /* DEBUG XXXX */
  if ( NspFname(stack) == NULL)
    {
      NspFname(stack) = "";
    }
 
  /* reordering and cleaning the stack */
 
  /* first pass to deal with pointers */
  while ( *O1 != NULL)
    {
      O=*O1;
      /* XXX : we keep here special cases for handler and resize2vect_h
       * which are authorized to return a Hobj. Maybe not a good idea to
       * keep special cases here.
       */
      if ( IsHobj(O) && strcmp(NspFname(stack),"handler") != 0 && strcmp(NspFname(stack),"resize2vect_h") != 0)
	{
	  /* O is of type pointer */
	  O2= ((NspHobj *) O)->O;
	  if ( IsHopt(O) )
	    {
	      if ( O->ret_pos != -1 )
		{
		  /* XXXX should not get there */
		  fprintf(stderr,"Something wrong in reorder_stack for %s: a pointer is returned \n", NspFname(stack));
		  exit(1);
		}
	      /* O is an optional argument,O2 is the value */
	      nsp_object_destroy(&O);
	      /* we go on with O2 */
	      O = *O1=O2;       /* a priori inutile *01 = 02 suffit ! */
	    }
	  else
	    {
	      /* O is a pointer */
	      if ( O->ret_pos != -1 )
		{
		  /* XXXX should not get there */
		  fprintf(stderr,"Something wrong in reorder_stack for %s: a pointer is returned \n", NspFname(stack));
		  exit(1);
		}
	      /* O points to O2 */
	      if ((k= O2->ret_pos) != -1)
		{
		  /* reset O2 */
		  O2->ret_pos=-1;
		  /* O2 is on the return list we must copy O2 */
		  if ( (O2 =nsp_object_copy(O2)) == NULLOBJ)  return RET_BUG;
		  O2->ret_pos=k;
		  /* O2 replace O, O must not be freed, we go on with O2 */
		  O= *O1= O2;     /* a priori inutile *01 = 02 suffit ! */
		}
	    }
	}
      O1++; count++;
    }

  /* second pass to destroy non returned objects  */
  kn = 0;
  for ( k = 1 ; k <= count ; k++ )
    {
      ret_pos = obj[k]->ret_pos;
      if ( ret_pos == -1 )
	{
	  nsp_void_object_destroy(&(obj[k]));
	  obj[k] = NULLOBJ;
	}
      else
	{
	  kn++;
	  obj[kn] = obj[k];
	  if ( kn < k) obj[k] = NULLOBJ;
	  if ( ret_pos != kn ) must_be_reordered = 1; else obj[kn]->ret_pos = -1;
	}
    }
  count = kn;

  /* third pass to reorder if needed */
  if ( must_be_reordered )
    {
      NspObject *otemp; int knn;
      for ( k = 1 ; k <= count ; k++ )
	{
	  if ( (kn=obj[k]->ret_pos) != -1 )
	    {
	      do
		{
		  if ( kn <= k || kn > count || (knn = obj[kn]->ret_pos)==-1 )
		    {
		      fprintf(stderr,"Something wrong at end of %s \n",  NspFname(stack));
		      fprintf(stderr,"duplication or hole in returned arguments numbering\n");
		      show_returned_positions(stack,1);
		      exit(1);
		    }
		  otemp = obj[kn]; obj[kn] = obj[k]; obj[k] = otemp;
		  obj[kn]->ret_pos = -1;
		  kn = knn;
		}
	      while (kn != k);
	      obj[k]->ret_pos = -1;
	    }
	}
    }

  /* clean extra returned arguments */
  if ( ret < count )
    for ( k = ret+1 ; k <= count ; k++ )
      {
	nsp_void_object_destroy(&(obj[k]));
	obj[k] = NULLOBJ;
      }

  return ret;
}


static int show_returned_positions(Stack stack,int pos)
{
  NspObject **obj = stack.val->S+stack.first+pos-1;
  fprintf(stderr,"from pos=%d ->[",pos);
  while (*obj != NULL) 
    {
      fprintf(stderr,"%d ",(*obj)->ret_pos);
      obj++;
    }
  fprintf(stderr,"]\n");
  return OK;
}
