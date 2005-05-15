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

/*
 * Interface demo file 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/interf.h"

static int ex1c (char *ch, int *a, int ia, float *b, int ib, double *c, int mc,
		 int nc, double *d, double *w, int *err);


/*
 * [x,y,z,t]=ex1c('mul'|'add',a,b,c);
 *
 * the function to be interfaced is ex1c (see below) 
 * the interface program is intex1c
 * the associated scilab function is ex1c (see file ex01.sce)
 *  
 * Shows how to pass 
 *     - Scilab scalar matrices as int,float or double arrays 
 *     - Scilab string as C-string 
 *       how to create and return new scalar matrices
 */

int int_ex1c(Stack stack, int rhs, int opt, int lhs)
{
  char *Str;
  int ierr=FALSE;
  NspMatrix *A,*B,*C,*D,*E;
  CheckRhs(4,4);
  CheckLhs(1,4);
  if ((Str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((A = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,3)) == NULLMAT)  return RET_BUG;
  if ((C = GetRealMatCopy(stack,4)) == NULLMAT)  return RET_BUG;
  if ((A= Mat2int(A))  == NULLMAT)  return RET_BUG;
  if ((B= Mat2float(B))  == NULLMAT)  return RET_BUG;
  /* create a matrix whith same dimensions as C */
  if ((D = nsp_matrix_create(NVOID,'r',C->m,C->n)) == NULLMAT) return RET_BUG;
  /* create a matrix whith same dimensions as C */
  if ((E = nsp_matrix_create(NVOID,'r',C->m,C->n)) == NULLMAT) return RET_BUG;

  ex1c(Str,(int *) A->R,A->mn,(float *) B->R,B->mn, C->R,C->m,C->n, D->R,E->R,&ierr);
  
  if (ierr > 0) 
    {
      Scierror("%s: Internal error \r\n",stack.fname);
      return RET_BUG;
    }
  /*   
   *  return [E,D,C,B,A] 
   */
  /* put D on the calling stack */
  NthObj(4) = NSP_OBJECT(D);
  /* put E on the calling stack */
  NthObj(5) = NSP_OBJECT(E);
  NSP_OBJECT(A)->ret_pos = 5;
  NSP_OBJECT(B)->ret_pos = 4;
  NSP_OBJECT(C)->ret_pos = 3;
  NSP_OBJECT(D)->ret_pos = 2;
  NSP_OBJECT(E)->ret_pos = 1;
  return Max(lhs,1);
}




/*
 *     inputs:  ch, a,b and c; ia,ib and mc,nc 
 *     ch=character, a=int, b=float and c=double 
 *     ia,ib and [mc,nc] are the dimensions of a,b and c resp. 
 *     outputs: a,b,c,d 
 *     if ch='mul'   a,b and c = 2 * (a,b and c) 
 *     and d of same dimensions as c with 
 *     d(i,j)=(i+j)*c(i,j) 
 *     if ch='add' a,b and c = 2 + (a,b and c) 
 *     d(i,j)=(i+j)+c(i,j) 
 *     w is a working array of size [mc,nc] 
 */

extern int ex1c (char *ch, int *a, int ia, float *b, int ib, double *c, int mc,
		 int nc, double *d, double *w, int *err)
{
  static int i, j, k;
  *err = 0;
  if (strcmp(ch, "mul") == 0) 
    {
      for (k = 0 ; k < ib; ++k) 
	a[k] <<= 1;
      for (k = 0; k < ib ; ++k) 
	b[k] *= (float)2.;
      for (i =  0 ; i < mc ; ++i) 
	for (j = 0 ;  j < nc ; ++j) 
	  c[i + j *(mc) ] *= 2.;
      for (i = 0 ; i < mc ; ++i) 
	for (j = 0 ; j < nc ; ++j) 
	  {
	    w[i + j * (mc) ] = (double) (i + j);
	    d[i + j * (mc) ] = w[i + j *(mc)] * c[i + j *(mc)];
	  }
    } 
  else if (strcmp(ch, "add") == 0) 
    {
      for (k = 0; k < ia  ; ++k) 
	a[k] += 2;
      for (k = 0 ; k < ib ; ++k) 
	b[k] += (float)2.;
      for (i =  0 ; i < mc ; ++i) 
	for (j = 0 ;  j < nc ; ++j) 
	  c[i + j *(mc) ] += 2.;
      for (i = 0 ; i < mc ; ++i) 
	for (j = 0 ; j < nc ; ++j) 
	  {
	    w[i + j * (mc) ] = (double) (i + j);
	    d[i + j * (mc) ] = w[i + j *(mc)] + c[i + j *(mc)];
	  }
    } 
  else 
    {
      *err = 1;
    }
  return(0);
}


/*
 * examples of an hand written interface 
 * Shows how to pass 
 *   - Scilab complex scalar matrices 
 */

static void f99(NspMatrix *M);

int int_ex2c(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  f99(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static void f99(NspMatrix *M)
{
  int i;
  if ( M->rc_type == 'r' ) 
    for ( i= 0 ; i < M->mn ; i++) M->R[i] *= 2.0;
  else 
    for ( i= 0 ; i < M->mn ; i++) { M->C[i].r *= 2.0;M->C[i].i *= 3.0;}
}


/*
 * examples of an hand written interface 
 * Shows how to pass 
 *       - Scilab boolean matrices as arguments 
 *       how to create and return new boolean matrices
 */

static void not(NspBMatrix *B) ;

int int_ex_3c_1(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* since not modify its argument we must use a copy */ 
  if ((A = GetBMatCopy(stack,1)) == NULLBMAT)  return RET_BUG;
  not(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1; 
}

static void not(NspBMatrix *B) 
{
  int i;
  for ( i=0 ; i < B->mn ; i++)  B->B[i] = !  B->B[i];
}



/**************************************************
 * examples of an hand written interface 
 * Shows how to create Scilab variable from a C array 
 * (of int, float or double type). This is mainly used 
 * when the arrays are allocated in the C-code and we 
 * want to return the allocated arrays as Scilab objects
 **************************************************/

/*------------------------------------------------------
 * Creating a scilab variable from a pointer
 * intex4c_1 : a pointer to a double array 
 * intex4c_2 : a pointer to an int  array 
 * intex4c_3 : a pointer to a  char array (i.e a string) 
 * intex4c_4 : 3,4,5 in one interface 
 * The interfaced functions are at the end of this file 
 *-------------------------------------------------------*/

static double *double_array(int *m,int *n) ;
static int *int_array(int *m,int *n) ;
static char *new_string(void);
/*------------------------------------------------------
 * intex4c_1 : a pointer to a double array 
 *-------------------------------------------------------*/

int int_ex_4c_1(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A,*B;
  const double l2[] ={ 1.0,2.0,3.0};
  double *l1;
  int m1,n1;
  CheckRhs(0,0);
  CheckLhs(1,2);
  l1 = double_array(&m1,&n1); 
  if ( l1 == NULL ) 
    { 
      Scierror("%s: Unable to allocate doubles\n",stack.fname);
      return RET_BUG;  
    }
  /* first example : l1 was allocated inside  double_array */ 
  if ((A = nsp_matrix_create_from_array(NVOID,m1,n1,l1,NULL)) == NULLMAT) 
    return RET_BUG;
  free(l1); /* freeing space allocated inside dblearray */ 
  NthObj(1) = NSP_OBJECT(A);
  NSP_OBJECT(A)->ret_pos = 1;
  if ( lhs == 2 ) 
    {
      /* second example : l2 is defined here */
      if ((B = nsp_matrix_create_from_array(NVOID,3,1,l2,NULL)) == NULLMAT) 
	return RET_BUG;
      NthObj(2) = NSP_OBJECT(B);
      NSP_OBJECT(B)->ret_pos = 2;
    }
  return Max(lhs,1);
}


/*------------------------------------------------------
 * intex4c_2 : a pointer to an int  array 
 *-------------------------------------------------------*/

int int_ex_4c_2(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A;
  int *l1;
  int m1,n1,i;
  CheckRhs(0,0);
  CheckLhs(1,1);
  l1 = int_array(&m1,&n1); 
  if ( l1 == NULL ) 
    { 
      Scierror("%s: Unable to allocate int\n",stack.fname);
      return RET_BUG;  
    }
  /* first example : l1 was allocated inside  double_array */ 
  if ((A = nsp_matrix_create(NVOID,'r',m1,n1)) == NULLMAT) 
    return RET_BUG;
  for (i=0; i < A->mn ; i++ ) A->R[i]=(double) l1[i];
  free(l1); /* freeing space allocated inside dblearray */ 
  NthObj(1) = NSP_OBJECT(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return Max(lhs,1);
}

/*------------------------------------------------------
 * intex4c_3 : a pointer to a  char array (i.e a string) 
 *-------------------------------------------------------*/

int int_ex_4c_3(Stack stack, int rhs, int opt, int lhs)
{ 
  NspSMatrix *A;
  char *str;
  CheckRhs(0,0);
  CheckLhs(1,1);
  str = new_string();
  if ( str == NULL ) 
    { 
      Scierror("%s: Unable to allocate\n",stack.fname);
      return RET_BUG;  
    }
  if ((A = nsp_smatrix_create(NVOID,1,1,str,-1)) == NULLSMAT) 
    return RET_BUG;
  free(str); /* freeing space allocated inside dblearray */ 
  NthObj(1) = NSP_OBJECT(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return Max(lhs,1);
}

/*------------------------------------------------------
 * intex6c :
 *-------------------------------------------------------*/

int int_ex_4c_4(Stack stack, int rhs, int opt, int lhs)
{ 
  NspSMatrix *S;
  NspMatrix *A;
  char *str;
  int m1,n1;
  CheckRhs(0,0);
  CheckLhs(1,3);
  str = new_string();
  if ( str == NULL ) 
    { 
      Scierror("%s: Unable to allocate\n",stack.fname);
      return RET_BUG;  
    }
  if ((S = nsp_smatrix_create(NVOID,1,1,str,-1)) == NULLSMAT) 
    return RET_BUG;
  free(str); /* freeing space allocated inside dblearray */ 
  NthObj(1) = NSP_OBJECT(S);
  NSP_OBJECT(S)->ret_pos = 1;
  if ( rhs >= 2 ) 
    {
      int *l1,i;
      if ((l1 = int_array(&m1,&n1))== NULL ) 
	{ 
	  Scierror("%s: Unable to allocate int\n",stack.fname);
	  return RET_BUG;  
	}
      if ((A = nsp_matrix_create(NVOID,'r',m1,n1)) == NULLMAT) 
	return RET_BUG;
      for (i=0; i < A->mn ; i++ ) A->R[i]=(double) l1[i];
      free(l1); /* freeing space allocated inside dblearray */ 
      NthObj(2) = NSP_OBJECT(A);
      NSP_OBJECT(A)->ret_pos = 2;
    }
  if (rhs >= 3) 
    {
      double *l1;
      if ((l1 = double_array(&m1,&n1))== NULL ) 
	{ 
	  Scierror("%s: Unable to allocate doubles\n",stack.fname);
	  return RET_BUG;  
	}
      /* first example : l1 was allocated inside  double_array */ 
      if ((A = nsp_matrix_create_from_array(NVOID,m1,n1,l1,NULL)) == NULLMAT) 
	return RET_BUG;
      free(l1); /* freeing space allocated inside dblearray */ 
      NthObj(3) = NSP_OBJECT(A);
      NSP_OBJECT(A)->ret_pos = 3;
    }
  return Max(lhs,1);
}

/*     double array    */

static double *double_array(int *m,int *n) 
{
  double *res; 
  int i,mn;
  *n=5 ;
  *m=3 ;
  mn = (*m)*(*n);
  res = malloc(sizeof(double)*mn);
  if ( res != NULL) 
    for ( i= 0 ; i < mn ; i++) res[i] = (double) i+1;
  return res;
}

/*  int array    */

static int *int_array(int *m,int *n) 
{
  int *res; 
  int i,mn;
  *n=5 ;
  *m=3 ;
  mn = (*m)*(*n);
  res = malloc(sizeof(int)*mn);
  if ( res != NULL) 
    for ( i= 0 ; i < mn ; i++) res[i] = i+1;
  return res;

}

/* string */

#define MYSTR "Nsp is ..."

static char *new_string(void)
{
  char *str;
  str = malloc(sizeof(char)*(strlen(MYSTR)+1));
  if ( str != NULL) strcpy(str,MYSTR);
  return str;
}



/**************************************************
 * examples of an hand written interface 
 * Shows how to pass string matrix as argument 
 *       how to return string arrays as Scilab string matrices
 **************************************************/

int int_ex_5c_1(Stack stack, int rhs, int opt, int lhs)
{ 
  NspSMatrix *A,*B,*C;
  const char *Str1[]= { "Un", "Deux","Trois", "Quatre","Cinq","Six" };
  CheckRhs(1,1) ;
  CheckLhs(1,2) ;
  /* Checks that first argument is a Scilab String matrix */
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  /* now we can use Str1 to create a new Scilab string matrix */
  if ((B = nsp_smatrix_create_from_array(6,Str1)) == NULLSMAT) return RET_BUG;
  /* now we can use Str2 to create a new Scilab string matrix */
  if ((C = nsp_smatrix_create_from_table(A->S)) == NULLSMAT) return RET_BUG;
  NthObj(2) = NSP_OBJECT(B);
  NthObj(3) = NSP_OBJECT(C);
  NSP_OBJECT(B)->ret_pos = 1;
  if ( lhs == 2 ) NSP_OBJECT(B)->ret_pos = 2;
  return Max(lhs,1);
}

/* search if first argument (a string) is 
 * an element of second argument (a string matrix) 
 */

int int_ex_5c_2(Stack stack, int rhs, int opt, int lhs)
{ 
  int i=0;
  NspSMatrix *A;
  char *str;
  CheckRhs(2,2) ;
  CheckLhs(1,1) ;
  if ((str = GetString(stack,1)) == NULL) return RET_BUG;
  if ((A = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  while ( A->S[i] != NULL) 
    {
      if (strcmp(str,A->S[i])==0) break;
      i++;
    }
  /* replace object at position 1 by a scalar 1x1 matrix */
  if ( nsp_move_double(stack,1,(double) i+1) == FAIL) return RET_BUG;
  return 1;
}


