/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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
#include <ctype.h> /* isxxxx */

#include "nsp/smatrix-in.h"
#include "nsp/datas.h"
#include "nsp/gsort-p.h"

/*
 * Now the interfaced function for basic matrices operations
 */

/*
 * Creation of a NspSMatrix all the strings are created with "." value 
 */

int int_smxcreate(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  int m1,n1;
  char *str=0;
  int flag = 0;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( rhs == 3) 
    {
      if ((str = GetString(stack,3)) == (char*)0) return RET_BUG;
      flag =1;
    }
  if ( (HMat =nsp_smatrix_create(NVOID,m1,n1,str,flag)) == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat);
  return 1;
}

/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 * provided through matint 
 */


/*
 * Right Concatenation 
 * Res = [A,B]  when A is a scalar matrix 
 * A is converted to SMatrix.
 */

int int_smxconcatr_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_smatrix(HMat1,NULL,0)) == NULLSMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if (nsp_smatrix_concat_right(Res,HMat2)!= OK) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Right Concatenation 
 * Res = [A,B]  when B is a scalar matrix 
 */

static int int_smxconcatr_s_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat2;
  NspSMatrix * HMat1, *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ((HMat2 = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
      return 1;
    }
  if (( Res=nsp_matrix_to_smatrix(HMat2,NULL,0)) == NULLSMAT) return RET_BUG;
  if ( HMat1->mn != 0)
    {
      if (nsp_smatrix_concat_right(HMat1,Res)!= OK) return RET_BUG;
      nsp_smatrix_destroy(Res);
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
    }
  else 
    {
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 * provided by matint
 */

/*
 * Down Concatenation 
 * Res = [A;B]  when A is a scalar matrix 
 * usefull when A=[]
 */

int int_smxconcatd_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_smatrix(HMat1,NULL,0)) == NULLSMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if ((Res=nsp_smatrix_concat_down(Res,HMat2))== NULLSMAT ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Down Concatenation 
 * Res = [A;B]  when B is a scalar matrix 
 */

static int int_smxconcatd_s_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat2;
  NspSMatrix * HMat1, *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ((HMat2 = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
      return 1;
    }
  if (( Res=nsp_matrix_to_smatrix(HMat2,NULL,0)) == NULLSMAT) return RET_BUG;
  if ( HMat1->mn != 0)
    {
      /* Res is used and destroyed */
      if (nsp_smatrix_concat_down1(HMat1,Res,TRUE)!= OK) return RET_BUG;
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
    }
  else 
    {
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}

/*
 *nsp_smatrix_add_columns: add n cols of zero to NspSMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int int_smxaddcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspSMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * AddRows : Add m rows of zero to a NspSMatrix A 
 * A = [A;ones(m,n)]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int int_smxaddrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspSMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * columns extraction for do loop
 * Cols A --> (Cols,A,Cols(A))     
 * FIXME ne sert plus ?
 */

int int_smxextractcolforloop_obsolete(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*Res;
  NspMatrix *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(3,3);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Cols = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_smatrix_extract_columns_obsolete( A,Cols,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSMAT) return RET_BUG;
  NthObj(3) = (NspObject *) Res;
  return 3;
}

/*
 *nsp_smatrix_resize: Changes NspSMatrix dimensions
 * Warning : this routine only enlarges the array 
 * of the NspSMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspSMatrix is changed 
 * return 0 on failure 
 */

int int_smxresize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspSMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * int = SMatConcatTT
 * Term to term concatenation 
 * A(i;j) = "A(i;j)cB(i;j)" : A is changed  B unchanged 
 * C unchanged : c is a 1x1 string 
 * C is used if flag == 1
 * here we need a column or row option XXXX
 * 
 */

int int_smxconcattt(Stack stack, int rhs, int opt, int lhs)
{
  char *str=NULL;
  int flag = 0;
  NspSMatrix *A,*B;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if ( rhs == 3) 
    {
      if ((str = GetString(stack,3)) == (char*)0) return RET_BUG;
      flag =1;
    }
  if ( B->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( A->mn == 0) 
    {
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
      
  if ( A->mn == 1 && B->mn != 1) 
    {
      /* must copy B */
      if ((B = GetSMatCopy(stack,2)) == NULLSMAT) return RET_BUG;
      if (nsp_smatrix_concat_string_left(B,A,str,flag)== FAIL) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  if ( B->mn == 1 && A->mn != 1) 
    {
      if (nsp_smatrix_concat_string_right(A,B,str,flag)== FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if (nsp_smatrix_concat_strings(A,B,str,flag) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res =nsp_smatrix_strcmp(A,B) A and B are not changed 
 *  Res[i;j] = strcmp(A[i;j],B[i;j]) 
 * XXXX strcmp 
 */

int int_smxcomp(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Res;
  NspSMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_smatrix_strcmp(A,B) )== NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * concat(A,[row='sep',col='sep',sep='sep'])
 */

int int_smxconcat(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *rep ;
  NspSMatrix *A;
  char *col=NULL,*row=NULL,*sep=NULL;
  int_types T[] = {smat,new_opts, t_end} ;

  nsp_option opts[] ={{ "col",string,NULLOBJ,-1},
		      { "row",string,NULLOBJ,-1},
		      { "sep",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&col,&row,&sep) == FAIL) return RET_BUG;
  if ( col  != NULL ) 
    {
      if ( row != NULL) 
	{
	  nsp_string str;
	  if ((str=nsp_smatrix_elts_concat(A,row,1,col,1)) == NULL) return RET_BUG;
	  rep =nsp_create_object_from_str(NVOID,str);
	  nsp_string_destroy(&str);
	}
      else 
	rep= (NspObject *)nsp_smatrix_column_concat(A,col,1);
    }
  else if ( row != NULL)  
    {
      rep=(NspObject *)nsp_smatrix_row_concat(A,row,1);
    }
  else if ( sep != NULL)
    {
      nsp_string str;
      if ((str=nsp_smatrix_elts_concat(A,sep,1,sep,1)) == NULL) return RET_BUG;
      rep =nsp_create_object_from_str(NVOID,str);
      nsp_string_destroy(&str);
    }
  else 
    { 
      nsp_string str;
      if ((str=nsp_smatrix_elts_concat(A,sep,0,sep,0)) == NULL) return RET_BUG;
      rep =nsp_create_object_from_str(NVOID,str);
      nsp_string_destroy(&str);
    }
    
  if ( rep == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,rep);
  return 1;
}  

/*
 * Res= Part(A,Ind) 
 * part function of Scilab A is  unchanged 
 * Ind unchanged 
 */

int int_smxpart(Stack stack, int rhs, int opt, int lhs)
{
  int alloc = FALSE;
  NspMatrix *Ind;
  NspBMatrix *BElts=NULLBMAT;
  NspSMatrix *A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A= GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean: use find(Elts) **/
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) 
	return RET_BUG;
      if ((Ind =nsp_bmatrix_find(BElts)) == NULLMAT) 
	return RET_BUG;
      alloc = TRUE;
    }
  else
    {
      if ((Ind = GetRealMat(stack,2)) == NULLMAT) 
	return RET_BUG;
    }
  if (( A =nsp_smatrix_part(A,Ind)) == NULLSMAT)  
    {
      if ( alloc ) nsp_matrix_destroy(Ind) ;
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) A);
  if ( alloc ) nsp_matrix_destroy(Ind) ;
  return 1;
}

/*
 * Res= length(A) 
 * return a matrix which contains the length of the strings 
 * contained in A 
 * A unchanged 
 */

int int_smxlength(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Res;
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_smatrix_elts_length(A)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res=nsp_matrix_to_smatrix(A) 
 * A is not changed 
 * pour l'instant on utilise %f xxxxx
 */

int int_smxm2sm(Stack stack, int rhs, int opt, int lhs)
{
  char *Format=NULL;
  NspMatrix *A;
  NspSMatrix *Res;
  int flag = 0;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( rhs == 2)
    {
      if ((Format = GetString(stack,2)) == (char*)0) return RET_BUG;
      flag =1;
    }
  if (( Res=nsp_matrix_to_smatrix(A,Format,flag)) == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res= Mattoupper(A) 
 */

int int_smxtoupper(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  nsp_smatrix_toupper(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * Res= Mattolower(A) 
 */

int int_smxtolower(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  nsp_smatrix_tolower(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res= capitalize(A) 
 */

int int_smxcapitalize(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  nsp_smatrix_capitalize(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res= isxxxx(A)
 */

typedef int (*IsF) (int c);

int int_smx_isxxx(Stack stack, int rhs, int opt, int lhs, IsF F)
{
  int i;
  char *Str;
  NspBMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((A=nsp_bmatrix_create(NVOID,1,strlen(Str))) == NULLBMAT ) return RET_BUG;
  for ( i = 0 ; i < (int) strlen(Str) ; i++ ) 
    A->B[i] = (*F)( Str[i]) != 0 ? TRUE : FALSE ;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


int int_smxisalnum(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isalnum) ;
}

int int_smxisalpha(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isalpha) ;
}

int int_smxisascii(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isascii) ;
}

int int_smxisdigit(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isdigit) ;
}

int int_smxisgraph(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isgraph) ;
}

int int_smxislower(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,islower) ;
}

int int_smxisprint(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isprint) ;
}

int int_smxispunct(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,ispunct) ;
}

int int_smxisspace(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isspace) ;
}

int int_smxisupper(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isupper) ;
}

int int_smxisxdigit(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isxdigit) ;
}

/*
 * Res = strstr(A,str)
 * strstr(A,str)
 */

int int_smxstrstr(Stack stack, int rhs, int opt, int lhs)
{
  char *Str;
  NspSMatrix *A;
  NspMatrix *B;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if (( A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;  
  if ((Str = GetString(stack,2)) == (char*)0) return RET_BUG;
  if (( B =nsp_smatrix_strstr(A,Str)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;
}


/*
 * [index,pos) = strindex(str1,str2)
 * strindex(A,str)
 * str2 can be a string matrix.
 * same as in Scilab
 * 
 */

int int_smxstrindex(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *Str1;
  NspMatrix *ind=NULLMAT,*pos = NULLMAT;
  NspSMatrix *S;
  CheckRhs(2,2);
  CheckLhs(1,2);
  if ((Str1 = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((S=GetSMat(stack,2))== NULLSMAT) return RET_BUG;
  for ( i = 0 ; i < S->mn ; i++) 
    {
      NspMatrix *ind1;
      if (( ind1 = nsp_smatrix_strindex(Str1,S->S[i])) == NULLMAT ) goto bug;
      if ( ind == NULLMAT )
	{
	  /* first call */
	  ind = ind1 ;
	  if ( lhs == 2 )
	    {
	      if ((pos = nsp_matrix_create(NVOID,'r',1,ind1->mn)) == NULLMAT) goto bug;
	      nsp_mat_set_rval(pos,(double) 1.00);
	    }
	}
      else 
	{
	  /* add ind1 */
	  int n = ind1->mn,xof=ind->mn,j;
	  if ( n !=0 ) 
	    {
	      if ( nsp_matrix_concat_right(ind,ind1) == FAIL) 
		{
		  nsp_matrix_destroy(ind1);
		  goto bug;
		}
	      if ( lhs == 2) 
		{
		  if ( nsp_matrix_resize(pos,1,xof+n) == FAIL) goto bug;
		  for ( j=0; j < n ; j++) pos->R[j+xof]=(double)i+1;
		}
	    }
	  nsp_matrix_destroy(ind1);
	}
    }
  MoveObj(stack,1,(NspObject *) ind);
  if ( lhs == 2 )  MoveObj(stack,2,(NspObject *) pos);
  return Max(lhs,1);
 bug: 
  if ( ind != NULLMAT) nsp_matrix_destroy(ind);
  if ( pos != NULLMAT) nsp_matrix_destroy(pos);
  return RET_BUG;
}


/*
 *nsp_smatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 *  The result is stored in A 
 * WARNING : no copy 
 */

int int_smxenlarge(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  int m1,n1;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_enlarge(A,m1,n1)== FAIL)  return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}



/*
 * Ascii   txt <-> ascii
 */

static int int_smxascii2smat(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspSMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( A->rc_type == 'c' ) 
    {
      Scierror("\t%s", ArgPosition(1));
      ArgName(stack,1);
      Scierror(" of function %s should not be complex\n",NspFname(stack));
      return RET_BUG;
    }
  if (( B =nsp_ascii_to_smatrix(A)) == NULLSMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;
}

static int int_smxsmat2ascii(Stack stack, int rhs, int opt, int lhs)
{
  char *Str;
  NspMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((B =nsp_string_to_ascii(Str)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;
}

int int_smxascii(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ( IsMatObj(stack,1 ))
    return int_smxascii2smat(stack,rhs,opt,lhs);
  else
    return int_smxsmat2ascii(stack,rhs,opt,lhs);
}

/* FIXME
 * SMatSort 
 * [A_sorted,Index]=sort(A, 'r'| 'c' | 'g' | 'lr'| 'lc' ,'i'|'d')
 */

int int_smatrix_sort(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *M=NULL;
  NspMatrix *Index=NULL;
  char *type_sort[]={ "g", "c", "r", "lr" , "lc" , NULL };
  char *dir_sort[]={ "i", "d",  NULL };
  int iflag = FALSE;
  char direction = 'd';
  int rep_type= sort_g,rep_dir;

  CheckRhs(1,3);
  if ((M=GetSMatCopy(stack,1)) == NULLSMAT ) return RET_BUG;

  if ( rhs >= 2) 
    {
      if ((rep_type= GetStringInArray(stack,2,type_sort,1)) == -1) return RET_BUG; 
    }

  if (rhs >= 3) 
    {
      if ((rep_dir= GetStringInArray(stack,3,dir_sort,1)) == -1) return RET_BUG; 
      direction = dir_sort[rep_dir][0];
    }

  if (lhs  == 2) 
    {
      iflag = TRUE;
    }

  switch ( rep_type  )
    {
    case 0 : 
      nsp_smatrix_sort(M,&Index,iflag,direction);break;
    case 1 : 
      nsp_smatrix_row_sort(M,&Index,iflag,direction);break;
    case 2 :
      nsp_smatrix_column_sort(M,&Index,iflag,direction);break;
    case 3:
      nsp_smatrix_lexical_row_sort(M,&Index,iflag,direction);break;
    case 4:
      nsp_smatrix_lexical_column_sort(M,&Index,iflag,direction);break;
    }
  if ( iflag == TRUE && Index == NULL) return RET_BUG;
  NSP_OBJECT(M)->ret_pos = 1;
  if ( lhs == 2 ) {
    MoveObj(stack,2, NSP_OBJECT(Index));
  }
  return Max(lhs,1);
} 


/*
 * SMatSplit
 * [A]=split(str,sep='splitchars',msep=bool)
 */

int int_smxsplit(Stack stack, int rhs, int opt, int lhs)
{
  char *defsplit = " \n\t\r";
  char *sep=NULL;
  Boolean msep=FALSE;
  NspSMatrix *A;
  NspSMatrix *Src;
  int_types T[] = {smat, new_opts, t_end};
  nsp_option opts[] ={{ "msep",s_bool,NULLOBJ,-1},
		      { "sep",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&Src,&opts,&msep,&sep) == FAIL) 
    return RET_BUG;
  if ( sep  == NULL )
    sep = defsplit;

  if ( (A=nsp_smatrix_split(Src,sep,msep)) == NULLSMAT ) 
    return RET_BUG;
  MoveObj(stack,1,(NspObject *)A);
  return 1;
}

/*
 * Operation leading to Boolean result 
 */

/* A < B */ 

int int_smxlt(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_smxle(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_smxneq(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_smxeq(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_smxgt(Stack stack, int rhs, int opt, int lhs)
{

  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


int int_smxge(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_smxf_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err;
  NspSMatrix *A,*B; NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  rep = SMatFullComp(A,B,op,&err);
  if ( err == 1) 
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n",op);
      return RET_BUG;
    }
  if ( rep == TRUE ) 
    {
      if (( Res =nsp_create_true_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  else 
    {
      if (( Res =nsp_create_false_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_smxflt(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"<");
}

int int_smxfle(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"<=");
}


int int_smxfneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"<>");
}

int int_smxfeq(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"==");
}

int int_smxfgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,">");
}

int int_smxfge(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,">=");
}



/*
 * Res =nsp_smatrix_copy(A) 
 * Creates a Copy of NspSMatrix A : A is not checked 
 */

int int_smxtranspose(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( HMat2 =nsp_smatrix_transpose(HMat1))  == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}


/*
 * 
 */

static int int_smatrix_2latexmat(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  nsp_smatrix_latex_print(HMat);
  return 0;
}

/*
 *  BMat2LaTeXTab: writes BMat Objet on fd in TeX language
 */

static int int_smatrix_2latextab(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;    
  nsp_smatrix_latex_tab_print(HMat);
  return 0;
}

/*
 * Res =  subst(A,str,rep) 
 */

int int_smxsubst(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat1,*HMat2;
  char *str1,*str2;
  CheckRhs(1,3);
  CheckLhs(1,1);
  if (( HMat1 = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ((str2 = GetString(stack,3)) == (char*)0) return RET_BUG;
  if (( HMat2 =nsp_smatrix_subst(HMat1,str1,str2))  == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}

/*
 * Res =  stripblanks(A)
 */

int int_smxstripblanks(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat1;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetSMatCopy(stack,1)) == NULLSMAT) return RET_BUG;
  NSP_OBJECT(HMat1)->ret_pos = 1;
  if (nsp_smatrix_strip_blanks(HMat1) == FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_smatrix_utf8_from_unichar(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *loc;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( loc = nsp_smatrix_utf8_from_unichar(A)) == NULLSMAT) return RET_BUG; 
  MoveObj(stack,1,NSP_OBJECT(loc));
  return 1;
}


/*
 *
 */

int int_smatrix_strtod(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *loc;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( loc = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( A = nsp_smatrix_strtod(loc))== NULLMAT) return RET_BUG; 
  MoveObj(stack,1,NSP_OBJECT(A));
  return 1;
}



/*
 * The Interface for basic matrices operation 
 */

static OpTab SMatrix_func[]={
  {"extract_s", nsp_matint_extract_xx}, 
  {"extractelts_s", nsp_matint_extractelts_xx}, 
  {"extractcols_s", nsp_matint_extractcols_xx}, 
  {"extractrows_s", nsp_matint_extractrows_xx}, 
  {"resize2vect_s", nsp_matint_resize2vect_xx},
  {"setrowscols_s", nsp_matint_setrowscols_xx},
  {"deleteelts_s", nsp_matint_deleteelts_xx},
  {"deleterows_s", nsp_matint_deleterows_xx},
  {"deletecols_s", nsp_matint_deletecols_xx},
  {"tozero_s", nsp_matint_tozero_xx},
  {"repmat_s", nsp_matint_repmat_xx},
  {"latexmat_s",int_smatrix_2latexmat},
  {"latextab_s",int_smatrix_2latextab},
  /* {"loopextract_m_s",int_smxextractcolforloop}, */
  {"smat_create_m",int_smxcreate},
  {"redim_s",int_matint_redim},
  {"matrix_s", int_matint_redim},
  {"concatr_s_s",nsp_matint_concatr_xx},
  {"concatr_m_s",int_smxconcatr_m_s},
  {"concatr_s_m",int_smxconcatr_s_m},
  {"addcols_s_m",int_smxaddcols},
  {"concatd_s_s", nsp_matint_concatd_xx}, /*  int_smxconcatd}, */
  {"concatd_s_m",  int_smxconcatd_s_m}, /*  int_smxconcatd}, */
  {"concatd_m_s", int_smxconcatd_m_s},
  {"concatdiag_s_s",int_matint_concat_diag_yy}, /* int_mxconcatdiag}, */
  {"addrows_s",int_smxaddrows},
  {"resize_s",int_smxresize},
  {"concat_s_s", int_smxconcattt },
  {"plus_s_s", int_smxconcattt },
  {"comp_s_s", int_smxcomp },
  {"catenate", int_smxconcat },
  {"part", int_smxpart },
  {"length_s", int_smxlength },
  {"m2s", int_smxm2sm },
  {"enlarge_s", int_smxenlarge },
  {"isalnum", int_smxisalnum},
  {"isalpha",int_smxisalpha},
  {"isascii",int_smxisascii},
  {"isdigit",int_smxisdigit},
  {"isgraph",int_smxisgraph},
  {"islower",int_smxislower},
  {"isprint",int_smxisprint},
  {"ispunct",int_smxispunct},
  {"isspace",int_smxisspace},
  {"isupper",int_smxisupper},
  {"isxdigit",int_smxisxdigit},
  {"tolower",int_smxtolower},
  {"toupper",int_smxtoupper},
  {"capitalize",int_smxcapitalize},
  {"strstr",int_smxstrstr},
  {"strindex",int_smxstrindex},
  {"ascii",int_smxascii},
  {"split",int_smxsplit},
  {"eq_s_s" ,  int_smxeq },
  {"feq_s_s" ,  int_smxfeq },
  {"fge_s_s" ,  int_smxfge },
  {"fgt_s_s" ,  int_smxfgt },
  {"fle_s_s" ,  int_smxfle },
  {"flt_s_s" ,  int_smxflt },
  {"fneq_s_s" ,  int_smxfneq },
  {"ge_s_s" ,  int_smxge },
  {"gt_s_s" ,  int_smxgt },
  {"le_s_s" ,  int_smxle },
  {"lt_s_s" ,  int_smxlt },
  {"ne_s_s" ,  int_smxneq },
  {"quote_s", int_smxtranspose},
  {"strsubst",int_smxsubst},
  {"stripblanks",int_smxstripblanks},
  {"sort_s", int_smatrix_sort},
  {"gsort_s", int_smatrix_sort},
  {"new_sort", int_smatrix_sort },
  {"unichar_to_utf8", int_smatrix_utf8_from_unichar},
  {"strtod",int_smatrix_strtod},
  {(char *) 0, NULL}
};

int SMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void SMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SMatrix_func[i].name;
  *f = SMatrix_func[i].fonc;
}










