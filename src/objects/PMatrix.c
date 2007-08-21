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

#include "nsp/object.h"
#include "nsp/pr-output.h"
#include "nsp/cnumeric.h"
#include "nsp/nsp_lapack.h"

static int nsp_pmatrix_print_internal (nsp_num_formats *fmt,NspPMatrix *M, int indent);

/*
 * Copies a polynom which is nothing but a matrix 1xn
 */

nsp_polynom nsp_polynom_copy(nsp_polynom P)
{
  return((nsp_polynom ) nsp_matrix_copy((NspMatrix *) P));
}

/*
 * doubleC --> poly 
 */

nsp_polynom nsp_basic_to_polynom(doubleC *d, char type)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(NVOID,type,(int)1,(int)1))==NULLMAT)
    return((nsp_polynom ) 0);
  if ( type == 'r') 
    {
      A->R[0] = d->r;
    }
  else 
    {
      A->C[0].r = d->r ;
      A->C[0].i = d->i ;
    }
  return((nsp_polynom ) A);
}

void nsp_polynom_destroy(nsp_polynom *P)
{
  nsp_matrix_destroy((NspMatrix *) *P);
  *P=NULL;
}

/*
 * Matrix 1xn --> Polynom of degree n-1
 * Rajouter un redim sur loc->S[1] pour avoir une 1xmn Matrix
 * xxxxxxxx
 */

NspPMatrix *nsp_matrix_to_polynom(NspMatrix *M)
{
  NspPMatrix *loc;
  if ((loc =nsp_pmatrix_create(NVOID,1,1,NULL,-1))== NULLPMAT) return(NULLPMAT);
  if (( loc->S[0] = nsp_matrix_copy(M))== NULLPOLY ) return(NULLPMAT);
  return(loc);
}



/*
 * PMatInfo : display Info on NspPMatrix PMat 
 */

int nsp_pmatrix_info(NspPMatrix *Mat, int indent,const char *name,int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  Sciprintf1(indent,"%s\t= [%s]\t\tp %c (%dx%d)\n",pname,(Mat->mn == 0) ? "": "...",
	     Mat->rc_type,Mat->m,Mat->n);
  return TRUE;
}

/*
 * PMatPrint : writes PMat Objet 
 */

int nsp_pmatrix_print(NspPMatrix *Mat, int indent,const char *name, int rec_level)
{
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",pname,(Mat->mn==0 ) ? " m2p([])\n" : "" );
	}

      else 
	{
	  Sciprintf1(indent,"%s",(Mat->mn==0 ) ? " m2p([])\n" : "" );
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_pmatrix_info(Mat,indent,pname,rec_level);
	  return rep;
	}
      Sciprintf1(indent,"%s\t=%s\t\tp (%dx%d)\n",pname,
		 (Mat->mn==0 ) ? " []" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      rep = nsp_pmatrix_print_internal (&fmt,Mat,indent);
    }
  return rep;
}




/*
 * Creation of a NspPMatrix all the elements
 *	 are created with &Czero value 
 *       when flag == -1 the array elements are NULL
 */

static doubleC Czero={0.00,0.00};

NspPMatrix *nsp_pmatrix_create(char *name, int m, int n, doubleC *cval, int flag)
{
  int i;
  NspPMatrix *Loc;
  static doubleC *init,*def=&Czero;
  Loc = new_pmatrix();
  if ( Loc == NULLPMAT) 
    { 
      Scierror("PMatCreate : Error no more space ");
      return(NULLPMAT);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLPMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    Loc->otype = PMATRIX;
    Loc->ftype = PMatrix_Type;
  */
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  Loc->rc_type = 'r' ; /* XXXXX : a preciser ? **/
  Loc->var = NULL;
  if ( Loc -> mn == 0 ) 
    {
      /* empty string Matrix */
      Loc->S = (nsp_polynom *) 0;
      return(Loc);
    }
  if ((Loc->S = (nsp_polynom *) MALLOC( Loc->mn* sizeof(nsp_polynom ))) == (nsp_polynom *) 0 )
    { 
      Scierror("PMatCreate : Error no more space ");
      return(NULLPMAT);
    }
  if ( flag >= 0) 
    {
      if ( flag == 0) 
	init = def ; 
      else 
	init = cval ;
      for ( i = 0 ; i < Loc->mn ; i++ )
	{
	  if ( (Loc->S[ i] =nsp_basic_to_polynom(init,(flag==2)? 'c':'r')) == (nsp_polynom ) 0 )  return(NULLPMAT);
	}
    }
  else
    {
      for ( i = 0 ; i < Loc->mn ; i++ ) Loc->S[i] = NULL;
    }
  return(Loc);
}

NspPMatrix *nsp_pmatrix_clone(char *name, NspPMatrix *A, int m, int n, int init)
{
  /* -1 for just allocating a matrix of pointers */
  return nsp_pmatrix_create(name, m, n, NULL,(init == TRUE) ? 0 :  -1); 
}

/*
 * Delete the NspPMatrix A
 */

void nsp_pmatrix_destroy(NspPMatrix *A)
{
  int i;
  if ( A == NULLPMAT) return;
  nsp_object_destroy_name(NSP_OBJECT(A));
  if (  A-> mn != 0 ) 
    {
      for ( i = 0 ; i < A->mn ; i++ ) 
	{
	  nsp_polynom_destroy(&A->S[i]);
	}
      FREE(A->S);
    }
  FREE(A);
}


/*
 * Res =nsp_pmatrix_copy(A) 
 * Creates a Copy of NspPMatrix A : A is not checked 
 */

NspPMatrix *nsp_pmatrix_copy(NspPMatrix *A)
{
  int i;
  NspPMatrix *Loc;
  if ( ( Loc =nsp_pmatrix_create(NVOID,A->m,A->n,&Czero,(int)0) ) == NULLPMAT) return(NULLPMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      nsp_polynom_destroy(&Loc->S[i]);
      if ((Loc->S[ i] =nsp_polynom_copy(A->S[i])) == (nsp_polynom ) 0)  return(NULLPMAT);
    }
  return(Loc);
}


/*
 * Res= length(A) 
 * return a matrix which contains the length of the strings 
 * contained in A  A unchanged 
 * for Poly length menas degre of each polynom 
 */

NspMatrix *nsp_pmatrix_length(NspPMatrix *A)
{
  int i;
  NspMatrix *Loc;
  Loc = nsp_matrix_create(NVOID,'r',A->m,A->n);
  if ( Loc == NULLMAT) { return(Loc);}
  for ( i = 0 ; i < A->mn ; i++) 
    {
      Loc->R[i] = A->S[i]->mn -1 ;
    }
  return(Loc);
}


/*
 *  Res=nsp_matrix_to_pmatrix(A) 
 *  A s not changed 
 *  pour l'instant on utilise %f ou le format pass'e 
 *  en deuxieme argument
 */


NspPMatrix *nsp_matrix_to_pmatrix(NspMatrix *A,nsp_const_string str, int flag)
{
  int i;
  NspPMatrix *Loc;
  doubleC d={0,0};
  if ((Loc =nsp_pmatrix_create(NVOID,A->m,A->n,&Czero,(int)0) ) == NULLPMAT) 
    return(NULLPMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      nsp_polynom_destroy(&Loc->S[i]);
      if ( A->rc_type == 'r') 
	d.r= A->R[i];
      else { d.r= A->C[i].r; d.i= A->C[i].i;}
      if ((Loc->S[i] =nsp_basic_to_polynom(&d,A->rc_type)) == (nsp_polynom ) 0)  return(NULLPMAT);
    }
  return(Loc);
}

/**
 * nsp_pmatrix_elt_size:
 * @M: a #NspSMatrix 
 * 
 * size of string matrix elements.
 * 
 * Return value: size of @M elements.
 **/

unsigned int  nsp_pmatrix_elt_size(NspPMatrix *M)
{
  return sizeof(nsp_polynom);
}

/*
 * PMatResize : Changes NspPMatrix dimensions
 * Warning : this routine only enlarges the array 
 * of the NspPMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspPMatrix is changed 
 * return 0 on failure 
 */


int nsp_pmatrix_resize(NspPMatrix *A, int m, int n)
{
  int i;
  if ( A->mn == m*n ) 
    {
      A->m=m;
      A->n=n;
      return(OK);
    };
  if ( m*n < 0) return FAIL;
  if ( m*n < A->mn )
    {
      /* Clear before Realloc **/
      for ( i = m*n ; i < A->mn ; i++ )
	nsp_polynom_destroy(&(A->S[i]));
    }
  if ( m*n == 0 ) 
    {
      A->m =  A->n = A->mn= 0;
      FREE(A->S);
      return OK;
    }
  
  if ( A->mn == 0 ) 
    A->S = (nsp_polynom *)  MALLOC ((m*n+1) * sizeof(nsp_polynom));
  else 
    A->S = (nsp_polynom *)  REALLOC (A->S, (m*n+1) * sizeof(nsp_polynom));
  if ( A->S == (nsp_polynom *) 0) return(FAIL);

  /* Initialize new area **/
  A->S[(m*n)] = (nsp_polynom) 0;
  for ( i = A->mn ; i < m*n ; i++ )
    {
      if ((A->S[i] =nsp_basic_to_polynom(&Czero,'r')) == ( nsp_polynom ) 0 )  return(FAIL);
    }
  A->m =m ;
  A->n =n;
  A->mn=m*n ;
  if ( A->mn == 0) A->m = A->n = 0;
  return(OK);
}

/*
 *nsp_pmatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for &Czero strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A 
 */

int nsp_pmatrix_enlarge(NspPMatrix *A, int m, int n)
{
  if ( n > A->n  )
    if ( nsp_pmatrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if ( nsp_pmatrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(PMAT1,PMAT2) ( PMAT1->m == PMAT2->m && PMAT1->n == PMAT2->n  )

/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int nsp_pmatrix_concat_right(NspPMatrix *A,const NspPMatrix *B)
{
  int Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("PMatConcat : incompatible size  \n");
      return(FAIL);
    }
  if ( nsp_pmatrix_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  if (nsp_pcopy_polynom(B->mn,B->S,A->S+Asize) == FAIL) return(FAIL);
  return(OK);
}

int nsp_pcopy_polynom(int n, nsp_polynom *s1, nsp_polynom *s2)
{
  int i;
  /* Copie ds l'ordre inverse car de temps en temps on fait
     des copies sur place **/
  for ( i = n-1 ; i >= 0 ; i--) 
    {
      nsp_polynom_destroy(&s2[i]);
      if ((s2[ i] =nsp_polynom_copy(s1[i])) == (nsp_polynom ) 0)  return(FAIL);
    }
  return(OK);
}

/*
 * PMatAddCols : add n cols of zero to NspPMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int nsp_pmatrix_add_columns(NspPMatrix *A, int n)
{
  int ns;
  int Asize;
  Asize=A->mn;
  ns= (A->m)*n;
  if ( nsp_pmatrix_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  /* normalemeny inutile car Resize le fait deja **/
  /* if (nsp_pset_polynom(ns,&Czero,A->S+Asize) == FAIL) return(FAIL);**/
  return(OK);
}

int nsp_pset_polynom(int n, doubleC *s1, nsp_polynom *s2)
{
  int i;
  for ( i = 0 ; i < n ; i++) 
    {
      nsp_polynom_destroy(&s2[i]);
      if ((s2[ i] =nsp_basic_to_polynom(s1,'r')) == (nsp_polynom ) 0)  return(FAIL);
    }
  return(OK);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLPMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

NspPMatrix*nsp_pmatrix_concat_down(const NspPMatrix *A,const NspPMatrix *B)
{
  NspPMatrix *Loc;
  int j;
  if ( A->n != B->n ) 
    {
      Scierror("PMatConcatD : incompatible size  \n");
      return(NULLPMAT);
    }
  Loc =nsp_pmatrix_create(NVOID,A->m+B->m,A->n,&Czero,(int) 0);
  if ( Loc == NULLPMAT) 
    {
      Scierror("No More Place ");
      return(NULLPMAT);
    }
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      if (nsp_pcopy_polynom(A->m,A->S+j*A->m,Loc->S+j*(Loc->m)) == FAIL) 
	return(NULLPMAT);
      if (nsp_pcopy_polynom(B->m,B->S+j*B->m,Loc->S+j*(Loc->m)+A->m) == FAIL)
	return(NULLPMAT);
    }
  return(Loc) ;
}


/*
 * Add Rows : Add m rows of zero to a NspPMatrix A 
 * A = [A;ones(m,n)]
 * return NULLPMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int nsp_pmatrix_add_rows(NspPMatrix *A, int m)
{
  int Am;
  int j;
  Am= A->m;
  if ( nsp_pmatrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  for ( j = A->n-1  ; j >= 1 ; j-- ) 
    {
      if (nsp_pcopy_polynom(Am,A->S+j*Am,A->S+j*(A->m)) == FAIL) 
	return(FAIL);
    }
  for ( j = A->n-2  ; j >= 0 ; j-- ) 
    {
      if (nsp_pset_polynom(m,&Czero,A->S+j*(A->m)+Am) == FAIL)
	return(FAIL);
    }
  return(OK);
}

/**
 * nsp_matrix_set_submatrix:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 * @B: a #NspMatrix
 * 
 * Performe  A(Rows,Cols) = B. A is changed and enlarged if necessary and 
 * size compatibility is checked i.e B must be scalar or  
 * we must have size(B)==[size(Rows),size(Cols)]. 
 * 
 * returns %OK or %FAIL.
 */

extern int nsp_pmatrix_set_submatrix(NspPMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspPMatrix *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}

/*
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 */

int nsp_pmatrix_set_rows(NspPMatrix *A, NspMatrix *Rows, NspPMatrix *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
}



/*
 * Res=nsp_pmatrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are changed (i.e converted to int) 
 * 
 */	

NspPMatrix *nsp_pmatrix_extract(NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  return (NspPMatrix*)nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}

/*
 * Res=nsp_smatrix_extract_elements(A,Elts)
 * A unchanged, Elts
 */	

NspPMatrix*nsp_pmatrix_extract_elements(NspPMatrix *A, NspMatrix *Elts, int *err)
{
  *err=0;
  return (NspPMatrix *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}

/*
 * Res=nsp_pmatrix_extract_columns(A,Cols,err)
 * A unchanged
 */

NspPMatrix*nsp_pmatrix_extract_columns(NspPMatrix *A, NspMatrix *Cols, int *err)
{
  *err=0;
  return (NspPMatrix *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/*
 * Res=nsp_pmatrix_extract_rows(A,Rows,err)
 * A unchanged
 */

NspPMatrix*nsp_pmatrix_extract_rows(NspPMatrix *A, NspMatrix *Rows, int *err)
{
  *err=0;
  return (NspPMatrix *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
}


NspBMatrix  *PMatCompOp(NspPMatrix *A, NspPMatrix *B, char *op)
{
  Scierror("PMatCompOp: to be implemented \n");
  return NULL;
}




/*
 * Res =nsp_smatrix_transpose(A) 
 * Transpose A 
 */

NspPMatrix *nsp_pmatrix_transpose(const NspPMatrix *A)
{
  int i,j;
  NspPMatrix *Loc;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_pmatrix_create(NVOID,A->n,A->m,NULL,-1)) == NULLPMAT) return NULLPMAT;
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Loc->m ; i++ )
    for ( j = 0 ; j < Loc->n ; j++ )
      {
	if ((Loc->S[i+(Loc->m)*j] =nsp_polynom_copy(A->S[j+(A->m)*i])) == NULLPOLY ) return(NULLPMAT);
      }
  return(Loc);
}

/* compute the roots using the companion matrix 
 * eigenvalues.
 */

NspMatrix *nsp_matrix_companion(NspMatrix *A)
{
  int i,j;
  NspMatrix *B=NULL;
  if ((B = nsp_matrix_create(NVOID,A->rc_type,A->mn-1, A->mn-1))== NULLMAT) return 
    NULLMAT;
  nsp_mat_set_rval (B,0.0);
  if ( A->rc_type == 'c') 
    {
      doubleC ld = A->C[A->mn-1],res;
      nsp_mat_set_ival (B,0.0);
      for ( i= 1 ; i < B->m; i++)
	B->C[i+B->m*(i-1)].r=1.0;
      for ( j= 0 ; j < B->n ; j++)
	{
	  nsp_div_cc(&A->C[B->m-j],&ld,&res);
	  B->C[B->m*j].r = - res.r ;
	  B->C[B->m*j].i = - res.i;
	}
    }
  else 
    {
      double ld = A->R[A->mn-1];
      for ( i= 1 ; i < B->m;  i++)
	B->R[i+B->m*(i-1)]=1.0;
      for ( j= 0 ; j < B->n ; j++)
	B->R[B->m*j]= -A->R[B->m-j-1]/ld;
    }
  return B;
}

NspMatrix *nsp_polynom_roots(nsp_polynom poly)
{
  NspMatrix *companion,*roots=NULL;
  if ((companion = nsp_matrix_companion((NspMatrix *) poly))== NULLMAT)
    return NULLMAT;
  /* if spec fails we will return NULL */
  nsp_spec(companion,&roots,NULL);
  nsp_matrix_destroy(companion);
  return roots; 
}


/*
 * routines for output of polynomial matrices 
 */

/* XXXX */
int nsp_matrix_any_element_is_negative (const void *M);
int nsp_matrix_any_element_is_inf_or_nan (const void *M);
int nsp_matrix_all_elements_are_int_or_inf_or_nan (const void *M);
void nsp_matrix_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax);


static char MpInit(const void *M,int *work)
{
  *work  = 0;
  return ( (NspPMatrix *) M)->rc_type;
}

/* Polynomial matrix specific code */

static int Mp_any_element_is_negative (const void *M)
{
  int sign=0,i;
  for ( i = 0 ; i < ((NspPMatrix *) M)->mn ; i++ ) 
    {
      sign = nsp_matrix_any_element_is_negative(((NspPMatrix *) M)->S[i]);
      if ( sign==1) break;
    }
  return sign;
}

/* code for polynomial matrix  **/

static int Mp_any_element_is_inf_or_nan (const void *M)
{
  int inf_or_nan=0,i;
  for ( i = 0 ; i < ((NspPMatrix *)M)->mn ; i++ ) 
    {
      inf_or_nan = nsp_matrix_any_element_is_inf_or_nan (((NspPMatrix *)M)->S[i]);
      if ( inf_or_nan ==1 ) break;
    }
  return inf_or_nan;
}

/* code for polynomial matrix **/

static int Mp_all_elements_are_int_or_inf_or_nan (const void *M)
{
  int i, int_or_inf_or_nan=0;
  for ( i = 0 ; i < ((NspPMatrix *)M)->mn ; i++ ) 
    {
      int_or_inf_or_nan = nsp_matrix_all_elements_are_int_or_inf_or_nan(((NspPMatrix *)M)->S[i]);
      if ( int_or_inf_or_nan == 0) break;
    }
  return int_or_inf_or_nan;
}

/* code for polynomial matrix **/

static void Mp_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  int i;
  nsp_matrix_pr_min_max_internal (((NspPMatrix*)M)->S[0],'r',dmin,dmax);
  for ( i = 1 ; i < ((NspPMatrix*)M)->mn ; i++ ) 
    {
      double max1,min1;
      nsp_matrix_pr_min_max_internal (((NspPMatrix*)M)->S[i],'r',&max1,&min1);
      if ( max1 > *dmax ) *dmax=max1;
      if ( min1 < *dmin ) *dmin=min1;
    }
}

/* Polynomial Matrix **/

static void Mp_set_format(nsp_num_formats *fmt,NspPMatrix *M)
{
  gen_set_format(fmt,M,Mp_any_element_is_negative,
		 Mp_any_element_is_inf_or_nan,
		 Mp_pr_min_max_internal,
		 Mp_all_elements_are_int_or_inf_or_nan,
		 MpInit);
}


/*
 * Printing Scilab Polynomial Matrices 
 */

static int  poly_size (int fw, int length);
static void pr_poly (nsp_num_formats *fmt,NspMatrix *m, int fw, int length);
static void pr_poly_exp  (NspMatrix *m, int fw, int length);

static int nsp_pmatrix_print_internal (nsp_num_formats *fmt,NspPMatrix *M, int indent)
{
  int *Iloc;
  int inc,column_width,total_width;
  int p_rows=0;
  int col;
  int max_width ,winrows ;
  int i,j;
  int nr = M->m;
  int nc = M->n;
  int fw=0;
  if (nr == 0 || nc == 0) nsp_print_empty_matrix ( nr, nc );
  sci_get_screen_size(&winrows,&max_width);
  /* get one format for all polynoms **/ 
  /* XXXXXX need to write the complex case **/
  Mp_set_format (fmt,M);
  fw= fmt->curr_real_fw;
  /* Sciprintf("prec= %d,Format [%s]\n",  user_pref.output_precision,  curr_real_fmt);*/
  /* Allocate a table to store the column width **/
  /* Iloc[j]= degree max of column j **/
  if ((Iloc =nsp_alloc_int(M->n)) == (int*) 0) return(FAIL);
  for ( j=0 ; j < M->n ; j++ )
    {
      Iloc[j]=M->S[j*M->m]->mn;
      for ( i = 1 ; i < M->m ; i++) 
	{
	  if ( Iloc[j] < M->S[i+j*M->m]->mn ) Iloc[j]= M->S[i+j*M->m]->mn;
	}
    }
  total_width=0;
  for ( j=0 ; j < M->n ; j++) 
    {
      column_width = Iloc[j]*(fw+2) + 2;
      total_width +=  column_width;
    }
  col=0;
  while ( col < nc )
    {
      int lim,num_cols,t_width;
      inc=0;
      t_width = 0;
      for ( j= col ; j < M->n ; j++) 
	{
	  t_width +=  poly_size(fw,Iloc[j]);
	  if ( t_width < max_width) inc++;
	  else break;
	}
      if (inc == 0)	inc++;
      lim = col + inc < nc ? col + inc : nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  if (col != 0)
	    Sciprintf("\n");
	  num_cols = lim - col;
	  if (num_cols == 1)
	    Sciprintf(" Column %d :\n\n",col+1);
	  else if (num_cols == 2)
	    Sciprintf(" Columns %d and %d:\n\n",col+1,lim);
	  else
	    Sciprintf(" Columns %d through %d:\n\n",col+1,lim);
	}
      for ( i = 0; i < nr; i++)
	{
	  int imore;
	  p_rows++;
	  if ( p_rows >= winrows ) 
	    {
	      scimore(&imore);
	      if ( imore == 1) return(OK);
	      p_rows=0;
	    }
	  /*
	    if (  lim - col == 1 && 	  t_width > max_width ) 
	    {
	    Sciprintf("Must try to cut thhe column \n");
	    }
	    else 
	  */
	  {
	    for ( j = col; j < lim; j++)
	      {
		Sciprintf("  ");
		pr_poly_exp ( M->S[i+(M->m)*j], fw,Iloc[j]);
	      }
	    Sciprintf("\n");
	    for ( j = col; j < lim; j++)
	      {
		Sciprintf("  ");
		pr_poly (fmt, M->S[i+(M->m)*j], fw,Iloc[j]);
	      }
	    Sciprintf("\n");
	  }
	}
      col += inc;
    }
  FREE(Iloc);
  return(OK);
}

static int  poly_size(int fw, int length)
{
  int ps=2 + length*fw,k;
  for ( k= 1 ; k <= length ; k++) 
    {
      if ( k < 10 ) ps += 3;
      else ps +=4;
    }
  return(ps);
}

static void pr_poly (nsp_num_formats *fmt,NspMatrix *m, int fw, int length)
{
  int i ; 
  for ( i=0 ; i < m->mn ; i++) 
    {
      /* xxxxxxxx : */
      if ( m->rc_type == 'r') 
	{
	  if ( i != 0 && m->R[i] >= 0.00 ) Sciprintf("+");
	  else Sciprintf(" ");
	  nsp_pr_any_float (fmt->curr_real_fmt, m->R[i], fw);
	}
      if ( i > 0 ) Sciprintf("x");
      if ( i < 10 ) nsp_pr_white(1);
      else if ( 10 <= i && i  < 99) nsp_pr_white(2); else nsp_pr_white(3);

    }
  for ( i= m->mn ; i < length ; i++) 
    {
      nsp_pr_white(fw);
      if ( i < 10 ) nsp_pr_white(3);
      else if ( 10 <= i && i  < 99)  nsp_pr_white(4); else  nsp_pr_white(5);
    }
}


static void pr_poly_exp (NspMatrix *m, int fw, int length)
{
  int i ; 
  for ( i=0 ; i < m->mn ; i++) 
    {
      /* xxxxxxxx : */
      if ( m->rc_type == 'r') 
	{
	   nsp_pr_white(fw+2);
	}
      if ( i > 0 ) Sciprintf("%d",i);
    }
  for ( i= m->mn ; i < length ; i++) 
    {
      nsp_pr_white(fw);
      if ( i < 10 )  nsp_pr_white(3);
      else if ( 10 <= i && i  < 99)  nsp_pr_white(4); else  nsp_pr_white(5);
      
    }
}





