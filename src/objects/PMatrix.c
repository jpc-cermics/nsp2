/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/pmatrix.h> 
#include <nsp/cells.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/pr-output.h"
#include "nsp/cnumeric.h"
#include "nsp/nsp_lapack.h"

#if 1 
#define nsp_polynom_mult nsp_polynom_mult_std
#else 
#define nsp_polynom_mult nsp_polynom_mult_fft 
#endif 

#define POLY_EXP /* use utf8 exponents */ 

static int nsp_pmatrix_print_internal (nsp_num_formats *fmt,NspPMatrix *M, int indent);
static int nsp_pcopy_polynom(int n, nsp_polynom *s1, nsp_polynom *s2);

/**
 * nsp_polynom_copy:
 * @P: a nsp_polynom pointer 
 * 
 * returns a polynom copy which is nothing but a matrix 1xn
 * 
 * Return value: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_copy(nsp_polynom P)
{
  return((nsp_polynom ) nsp_matrix_copy((NspMatrix *) P));
}

/**
 *nsp_polynom_copy_with_name:
 * @P: a nsp_polynom pointer 
 * 
 * returns a polynom copy of @P with the same name.
 * 
 * Return value: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_copy_with_name(nsp_polynom P)
{
  NspObject *Obj = (NspObject *) P;
  return (nsp_polynom ) nsp_object_copy_with_name(Obj);
}

/**
 *nsp_polynom_copy_and_name:
 * @name: string for the name to give to the copy 
 * @P: a nsp_polynom pointer 
 *
 * returns a copy of polynom @P with name given by @name.
 * 
 * 
 * Return value: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_copy_and_name(const char *name, nsp_polynom P)
{
  return (nsp_polynom) nsp_object_copy_and_name(name,(NspObject *) P);
}

/**
 * nsp_basic_to_polynom:
 * @d: a double or doubleC pointer 
 * @type: a characterwhich gives the type of @d
 * 
 * returns a polynom of 0 degree with coefficient set to@d.
 * 
 * Return value: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_basic_to_polynom(const doubleC *d, char type)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create("pe",type,(int)1,(int)1))==NULLMAT)
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

/**
 * nsp_polynom_destroy:
 * @P: a nsp_polynom pointer 
 * 
 * delete a polynom 
 **/

void nsp_polynom_destroy(nsp_polynom *P)
{
  nsp_matrix_destroy((NspMatrix *) *P);
  *P=NULL;
}

int nsp_polynom_pdiv(nsp_polynom a,nsp_polynom b,nsp_polynom *hq,
		    nsp_polynom *hr)
{
  int i;
  nsp_polynom ca,q;
  if ((ca = nsp_polynom_copy_and_name("pe",a))== NULL) return FAIL;
  if ((q = nsp_polynom_copy_and_name("pe",a))== NULL) return FAIL;
  for ( i = 0 ; i < ((NspMatrix *) q)->mn ; i++) 
    ((NspMatrix *) q)->R[i]=0;
  while (1) 
    {
      double c;
      if ( ca->mn < b->mn || ca->mn == 1 )
	{
	  if (( nsp_polynom_resize(q))== FAIL ) return FAIL;
	  *hq = q;
	  *hr = ca;
	  return OK;
	}
      c = ca->R[ca->mn-1]/b->R[b->mn-1];
      q->R[ca->mn-b->mn]=c;
      /* a = a - c*x^(da-db)*b */
      for ( i = 0  ; i < b->mn ; i++ ) 
	ca->R[ca->mn - (b->mn - i)] -= c*b->R[i];
      ca->R[ca->mn-1]=0.0;
      if (( nsp_polynom_resize(ca))== FAIL ) return FAIL;
    }
  /*   if (na < nb ) then r = a; break;end */
  return OK;
}



/**
 * nsp_matrix_to_polynom:
 * @M: a #NspMatrix 
 * 
 * returns a 1x1 polynomial matrix. The coefficient 
 * of the polynom being given by @M.
 * 
 * Returns:  a new #NspPMatrix or %NULLPMAT
 **/

NspPMatrix *nsp_matrix_to_polynom(NspMatrix *M)
{
  NspPMatrix *loc;
  if ((loc =nsp_pmatrix_create(NVOID,1,1,NULL,-1))== NULLPMAT) return(NULLPMAT);
  /* we need to give a name to each elts of a polynomial matrix */
  if ((loc->S[0] = nsp_polynom_copy_and_name("pe",M))== NULLPOLY ) return(NULLPMAT);
  loc->S[0]->m = 1;
  loc->S[0]->n = loc->S[0]->mn= M->mn;
  if (( nsp_polynom_resize(loc->S[0]))== FAIL ) return(NULLPMAT);
  return(loc);
}


/**
 * nsp_pmatrix_to_cells:
 * @M: a #NspPMatrix of size mxn
 * 
 * returns a mxn cell array. Each element of the 
 * cell is a row vector which contains the coefficient of 
 * the corresponding polynomial in @M.
 * 
 * Returns: a #NspCells object or %NULL
 **/

NspCells *nsp_pmatrix_to_cells(const char *name, NspPMatrix *M)
{
  int i;
  NspCells *loc;
  if ((loc = nsp_cells_create(name,M->m,M->n)) == NULL) 
    return NULL;
  for ( i = 0 ; i < M->mn ; i++)
    {
      if ((loc->objs[i] =(NspObject *) nsp_matrix_copy(M->S[i]))== NULL) 
	return NULL;
    }
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

/**
 * nsp_pmatrix_create:
 * @name: name of object
 * @m: number of rows
 * @n: number of columns 
 * @cval: pointer to double or doubleC
 * @flag: an integer 
 * 
 * returns a new mxn #NspPMatrix. if @flag 
 * is stricly negative the elements of the Matrix 
 * are initialized with %NULL. if @flag is null 
 * the elements are initilialized to zero. If flag 
 * is one or two, @cval is used for initialization 
 * (one for real values, two for complex values.
 * 
 * Returns:  a new #NspPMatrix or %NULL
 **/

static const doubleC Czero={0.00,0.00};

NspPMatrix *nsp_pmatrix_create(char *name, int m, int n,const doubleC *cval, int flag)
{
  int i;
  NspPMatrix *Loc;
  static const doubleC *init,*def=&Czero;
  Loc = new_pmatrix();
  if ( Loc == NULLPMAT) 
    { 
      Scierror("PMatCreate : Error no more space ");
      return(NULLPMAT);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLPMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; 
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  Loc->rc_type = 'r' ; /* XXXXX : a preciser ? **/
  Loc->var = NULL;
  if ( Loc -> mn == 0 ) 
    {
      /* empty pmatrix */
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
  if ( ( Loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1)) == NULLPMAT) return(NULLPMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->S[i] != NULL)
	{
	  if ((Loc->S[ i] =nsp_polynom_copy_with_name(A->S[i])) == (nsp_polynom ) 0)  return(NULLPMAT);
	}
    }
  return(Loc);
}

/**
 * nsp_pmatrix_length:
 * @A: a #NspPMatrix of size mxn
 * 
 * returns in a #NspMatrix of size mxn the degree of each polynom 
 * contained in @A.
 * 
 * Returns: a #NspMatrix or %NULL
 **/

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

/**
 * nsp_matrix_to_pmatrix:
 * @A: a #NspMatrix
 * 
 * return a new mxn polynomial matrix if @A is 
 * of size mxn. The (i,j)-th element of the result is the 
 * polynomial of degree 0 equal to @A(i,j).
 * 
 * Returns: a new #NspPMatrix or %NULL
 **/

NspPMatrix *nsp_matrix_to_pmatrix(NspMatrix *A) 
{
  int i;
  NspPMatrix *Loc;
  doubleC d={0,0};
  if ((Loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1)) == NULLPMAT) 
    return(NULLPMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->rc_type == 'r') 
	{
	  d.r= A->R[i];
	}
      else
	{ d.r= A->C[i].r; d.i= A->C[i].i;}
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

/**
 * nsp_pmatrix_resize:
 * @A: a #NspPMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspPMatrix @A dimensions are changed to be @m x @n. 
 * This routine only enlarges or shrink (using realloc()) 
 * the data array of @A to size mxn. The previous data are not moved and 
 * occupy the first array cells. Note that @A can be 
 * and empty matrix when calling this routine ( malloc() is used in that 
 * case ). The new elements of @A are filled with polynomial 0.
 *
 * returns: : %OK or %FAIL. When %OK is returned @A is changed. 
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
      /* Clear before Realloc */
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

static int nsp_pcopy_polynom(int n, nsp_polynom *s1, nsp_polynom *s2)
{
  int i;
  /* copy is performed backward since this function is used for on place copy. 
   */
  for ( i = n-1 ; i >= 0 ; i--) 
    {
      nsp_polynom_destroy(&s2[i]);
      if ((s2[ i] =nsp_polynom_copy_with_name(s1[i])) == (nsp_polynom ) 0)  return(FAIL);
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

int nsp_pset_polynom(int n,const doubleC *s1, nsp_polynom *s2)
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

NspPMatrix *nsp_pmatrix_concat_down(const NspPMatrix *A,const NspPMatrix *B)
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
      Scierror("Error: running out of memory\n");
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
 * nsp_pmatrix_set_submatrix:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 * @B: a #NspMatrix
 * 
 * Performe  A(Rows,Cols) = B. A is changed and enlarged if necessary and 
 * size compatibility is checked i.e B must be scalar or  
 * we must have size(B)==[size(Rows),size(Cols)]. 
 * 
 * returns: %OK or %FAIL.
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


/**
 * nsp_pmatrix_transpose: 
 * @A: a #NspPMatrix
 *
 * return the transpose of A
 * 
 * returns:  a new #MspPMatrix or %NULLMAT 
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
	if ((Loc->S[i+(Loc->m)*j] =nsp_polynom_copy_with_name(A->S[j+(A->m)*i])) == NULLPOLY ) return(NULLPMAT);
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


#define SameDim(PMAT1,PMAT2) ( PMAT1->m == PMAT2->m && PMAT1->n == PMAT2->n  )

NspPMatrix * nsp_pmatrix_minus_m(NspPMatrix *A,NspMatrix *B, int flag)
{
  int i;
  NspPMatrix *loc;
#define PM_MINUSM(s1,s2,i1,i2)						\
  if ((loc =nsp_pmatrix_create(NVOID,s1,s2,NULL,-1))== NULLPMAT)	\
    return(NULLPMAT);							\
  for (i=0; i < loc->mn ; i++)						\
    {									\
      if ((loc->S[i] = nsp_polynom_minus_m(A->S[i1],B->R+i2,B->rc_type)) == NULL) \
	return NULL;							\
      if ( flag == FALSE ) nsp_mat_minus(loc->S[i]);			\
    }								
  if ( SameDim(A,B) ) 
    {
      PM_MINUSM(A->m,A->n,i,i);
    }
  else if ( A->mn == 1 )
    {
      PM_MINUSM(B->m,B->n,0,i);
    }
  else if ( B->mn == 1 )
    {
      PM_MINUSM(A->m,A->n,i,0);
    }
  else
    {
      Scierror("Error:\tArguments must have the same size\n");
      return NULL;
    }
  return loc;
}


NspPMatrix * nsp_pmatrix_add_m(NspPMatrix *A,NspMatrix *B)
{
  int i;
  NspPMatrix *loc;
#define PM_ADDM(s1,s2,i1,i2)						\
  if ((loc =nsp_pmatrix_create(NVOID,s1,s2,NULL,-1))== NULLPMAT)	\
    return(NULLPMAT);							\
  for (i=0; i < loc->mn ; i++)						\
    {									\
      if ((loc->S[i] = nsp_polynom_add_m(A->S[i1],B->R+i2,B->rc_type)) == NULL) \
	return NULL;							\
    }								
  if ( SameDim(A,B) ) 
    {
      PM_ADDM(A->m,A->n,i,i);
    }
  else if ( A->mn == 1 )
    {
      PM_ADDM(B->m,B->n,0,i);
    }
  else if ( B->mn == 1 )
    {
      PM_ADDM(A->m,A->n,i,0);
    }
  else
    {
      Scierror("Error:\tArguments must have the same size\n");
      return NULL;
    }
  return loc;
}

/* returns eye(P)
 */

NspPMatrix *nsp_pmatrix_identity(NspPMatrix *P)
{
  NspPMatrix *Q=NULL;
  /* return identity */
  int i,j;
  doubleC zero={0,0},un={1,0},*val=&zero;
  if ((Q =nsp_pmatrix_create(NVOID,P->m,P->n,NULL,-1)) == NULLPMAT) 
    return(NULLPMAT);
  for ( i = 0 ; i < Q->m ; i++ )
    for (  j = 0 ; j < Q->n ; j++ )
      {
	val= ( i==j ) ? &un :&zero;
	if ((Q->S[i+Q->m*j] =nsp_basic_to_polynom(val,P->S[i+P->m*j]->rc_type)) == (nsp_polynom ) 0) 
	  return NULLPMAT;
      }
  return Q;
}

NspPMatrix *nsp_pmatrix_hat_p_m(NspPMatrix *P,int n)
{
  NspPMatrix *Q=NULL,*R=NULL,*loc=NULL;
  /* general case: power by repeated squaring */
  if ( n == 0 )
    {
      return nsp_pmatrix_identity(P);
    }
  if ((Q = nsp_pmatrix_copy(P)) == NULL  )
    return NULL;
  while  ( n > 1 )
    {
      if ( n % 2 ) 
	{
	  if ( R == NULL) 
	    {
	      if ((R = nsp_pmatrix_copy(Q))== NULL) goto err;
	    }
	  else
	    {
	      if ((loc = nsp_pmatrix_mult_p_p(R,Q)) == NULL) goto err;
	      nsp_pmatrix_destroy(R);
	      R=loc;
	    }
	}
      n /= 2;
      if ((loc = nsp_pmatrix_mult_p_p(Q,Q)) == NULL) goto err;
      nsp_pmatrix_destroy(Q);
      Q=loc;
    }
  if ( R != NULL) 
    {
      if ((loc = nsp_pmatrix_mult_p_p(Q,R)) == NULL) goto err;
      nsp_pmatrix_destroy(R);
      nsp_pmatrix_destroy(Q);
    }
  else 
    {
      loc = Q;
    }
  return loc;
 err:
  if ( Q != NULL) nsp_pmatrix_destroy(Q);
  if ( R != NULL) nsp_pmatrix_destroy(R);
  return NULL;

}



NspPMatrix *nsp_pmatrix_dh_p_m(const NspPMatrix *P,const NspMatrix *M) 
{
  int i;
  NspPMatrix *loc;
#define P_POWER(s1,s2,i1,i2)						\
  if ((loc =nsp_pmatrix_create(NVOID,s1,s2,NULL,-1))== NULLPMAT)	\
    return(NULLPMAT);							\
  for (i=0; i < loc->mn ; i++)						\
    {									\
      if ( floor(M->R[i2]) == M->R[i2] &&  M->R[i2] >= 0) {		\
	loc->S[i]= nsp_polynom_power( P->S[i1], M->R[i2]);		\
	if ( loc->S[i] == NULL) return NULL;				\
      } else {								\
	Scierror("Error: exponent should be a positive integer \n");	\
	nsp_pmatrix_destroy(loc);					\
	return NULL;							\
      }									\
    }					
  if ( P->mn == M->mn ) 
    {
      P_POWER(P->m,P->n,i,i);
    }
  else if ( P->mn == 1 )
    {
      P_POWER(M->m,M->n,0,i);
    }
  else if ( M->mn == 1 )
    {
      P_POWER(P->m,P->n,i,0);
    }
  else
    {
      Scierror("Error: arguments with incompatible dimensions\n");
      return NULL;
    }
  return loc;
}



/**
 * nsp_pmatrix_comp:
 * @A: a #NspPMatrix 
 * @B: a #NspPMatrix 
 * @op: the code for the comparison as a string
 * 
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j). A and B must be size compatible with 
 * the standard promotion of scalars i.e 1x1 matrices. 
 * A and B are unchanged : Res is created 
 * 
 * Return value: a new #NspBMatrix
 **/

static int nsp_polynom_eq(nsp_polynom p, nsp_polynom q)
{
  int err;
  int rep = nsp_mat_fullcomp (p,q,"==", &err);
  if ( err == TRUE || rep == FALSE ) return FALSE;
  return TRUE;
}

NspBMatrix  *nsp_pmatrix_comp(NspPMatrix *A, NspPMatrix *B,const char *op)
{
  int i;
  NspBMatrix *Loc ;
  if ( !( A->m == B->m && A->n == B->n ) )
    {
      /* dimensions are not the same */
      if ( B->mn == 1 ) 
	{
	  /* Special case B is a 1x1 constant, size of result is controled by A 
	   * even the 0xn and nx0 cases 
	   */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  if ( strcmp(op,"==")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if ( ! nsp_polynom_eq(A->S[i],B->S[0]) ) Loc->B[i] = FALSE;
	    }
	  else if ( strcmp(op,"<>")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if (  nsp_polynom_eq(A->S[i],B->S[0])  ) Loc->B[i] = FALSE;
	    }
	  else goto wrong;
	  return Loc;
	}
      if ( A->mn == 1 )
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  if ( strcmp(op,"==")==0 ) 
	    {
	      for ( i = 0 ; i < B->mn ; i++ )  
		if ( ! nsp_polynom_eq(A->S[0],B->S[i])  ) Loc->B[i] = FALSE;
	    }
	  else if ( strcmp(op,"<>")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if ( nsp_polynom_eq(A->S[i],B->S[0])  ) Loc->B[i] = FALSE;
	    }
	  else goto wrong;
	  return(Loc);
	}
      /* Incompatible dimensions: we return a boolean scalar as in Scilab 
       * this is not the matlab way.
       */
      if ( strcmp(op,"==") == 0) 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = FALSE;
	  return Loc;
	}
      else if ( strcmp(op,"<>") == 0) 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = TRUE ;
	  return Loc;
	}
      else goto wrong;
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  if ( strcmp(op,"==")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if ( ! nsp_polynom_eq(A->S[i],B->S[i]) ) Loc->B[i] = FALSE;
	    }
	  else if ( strcmp(op,"<>")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if (  nsp_polynom_eq(A->S[i],B->S[i]) ) Loc->B[i] = FALSE;
	    }
	  else goto wrong;
	}
    }
  return(Loc);
 wrong:
  Scierror("Error: operation %s is not implemented for polynomial matrices\n",op);
  return NULLBMAT ;
	

}


/**
 * nsp_polynom_resize:
 * @poly: a #nsp_polynom
 * 
 * Remove leading zeros from a polynom 
 * representation.
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_polynom_resize(nsp_polynom poly)
{
  int size = poly->mn ,i;
  /* remove leading zeros */
  if ( poly->rc_type == 'r') 
    {
      for ( i = poly->mn -1 ; i >= 1 ; i--) 
	{
	  if ( poly->R[i] != 0 ) break;
	  size--;
	}
    }
  else
    {
      for ( i = poly->mn -1 ; i >= 1 ; i--) 
	{
	  if ( poly->C[i].r != 0 || poly->C[i].i != 0 ) break;
	  size--;
	}
    }
  if ( size != poly->mn )
    {
      if ( nsp_matrix_resize(poly, 1, size) == FAIL) return FAIL;
    }
  return OK;
}

/**
 * nsp_polynom_roots:
 * @poly: a #nsp_polynom
 * 
 * compute the roots of a polynom by computing the 
 * eigenvalues of the associated companion matrix.
 * 
 * Returns: a new #NspMatrix or %NULL
 **/

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

/**
 * nsp_pmatrix_add:
 * @A: a #NspPMatrix 
 * @B: a #NspPMatrix 
 * 
 * return the sum of @A and @B.
 * 
 * Returns: a nex #NspPMatrix  or %NULL
 **/

NspPMatrix *nsp_pmatrix_add(NspPMatrix *A, NspPMatrix *B)
{
  NspPMatrix *loc;
  if ( SameDim(A,B) ) 
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_polynom_add(A->S[i],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( A->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	if ((loc->S[i] = nsp_polynom_add(A->S[0],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_polynom_add(A->S[i],B->S[0]))== NULL) 
	  return NULL;
      return loc;
    }
  else
    {
      Scierror("Error:\tArguments must have the same size\n");
      return NULL;
    }
}


NspPMatrix *nsp_pmatrix_minus(NspPMatrix *A, NspPMatrix *B)
{
  NspPMatrix *loc;
  if ( SameDim(A,B) ) 
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_polynom_minus(A->S[i],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( A->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	if ((loc->S[i] = nsp_polynom_minus(A->S[0],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_polynom_minus(A->S[i],B->S[0]))== NULL) 
	  return NULL;
      return loc;
    }
  else
    {
      Scierror("Error:\tArguments must have the same size\n");
      return NULL;
    }
}



/**
 * nsp_polynom_add:
 * @P: a nsp_polynom 
 * @Q: a nsp_polynom
 * 
 * return a new polynom @P + @Q.
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_add(nsp_polynom P,nsp_polynom Q)
{
  NspMatrix *A,*B,*C;
  int min,max,i;
  if ( P->mn >= Q->mn) 
    {
      min = Q->mn;max =  P->mn;
      B = Q; C = P;
    }
  else 
    {
      min = P->mn;max =  Q->mn;
      B = P; C = Q;
    }
  if ( (A =nsp_matrix_copy(B)) == NULLMAT) return NULL;
  if ( nsp_matrix_resize(A, 1,max) == FAIL) return NULL;
  if ( A->rc_type == 'r' ) 
    for (i= min ; i < max ; i++) A->R[i]=0 ;
  else
    for (i= min ; i < max ; i++) A->C[i].r = A->C[i].i =0;
  if ( nsp_mat_dadd((NspMatrix *) A, (NspMatrix *) C)== FAIL)
    {
      nsp_matrix_destroy(A);
      return NULL;
    }
  /* remove leading zeros */
  if ( nsp_polynom_resize(A) == FAIL) return NULL;
  if (nsp_object_set_name(NSP_OBJECT(A),"pe") == FAIL) 
    {
      nsp_matrix_destroy(A);
      return NULL;
    }
  return A;
}

/**
 * nsp_polynom_minus:
 * @P: a nsp_polynom 
 * @Q: a nsp_polynom
 * 
 * return a new polynom @P - @Q.
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_minus(nsp_polynom P,nsp_polynom Q)
{
  NspMatrix *A,*B,*C;
  int min,max,i;
  if ( P->mn >= Q->mn) 
    {
      min = Q->mn;max =  P->mn;
      B = Q; C = P;
    }
  else 
    {
      min = P->mn;max =  Q->mn;
      B = P; C = Q;
    }
  if ( (A =nsp_matrix_copy(B)) == NULLMAT) return NULL;
  if ( nsp_matrix_resize(A, 1,max) == FAIL) return NULL;
  if ( A->rc_type == 'r' ) 
    for (i= min ; i < max ; i++) A->R[i]=0 ;
  else
    for (i= min ; i < max ; i++) A->C[i].r = A->C[i].i =0;
  if ( nsp_mat_dsub((NspMatrix *) A, (NspMatrix *) C)== FAIL)
    {
      nsp_matrix_destroy(A);
      return NULL;
    }
  if ( P->mn >= Q->mn) nsp_mat_minus(A);
  /* remove leading zeros */
  if ( nsp_polynom_resize(A) == FAIL) return NULL;
  /* give a name to the polynom */
  if (nsp_object_set_name(NSP_OBJECT(A),"pe") == FAIL) 
    {
      nsp_matrix_destroy(A);
      return NULL;
    }
  return A;
}

/* 
 * A*B A matrix B PMatrix 
 */

NspPMatrix *nsp_pmatrix_mult_m_p(NspMatrix *A, NspPMatrix *B)
{
  NspPMatrix *loc;
  if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_polynom_copy_with_name(B->S[i]))== NULL) 
	    return NULL;
	  if ( nsp_mat_mult_scalar(loc->S[i],A) == FAIL )
	    return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      NspMatrix *C;
      int i;
      if ((C = nsp_matrix_create(NVOID,A->rc_type,1,1))== NULLMAT)
	return NULL;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_polynom_copy_with_name(B->S[0]))== NULL) 
	    return NULL;
	  if ( C->rc_type =='r' ) 
	    C->R[0]=A->R[i];
	  else 
	    C->C[0]=A->C[i];
	  if ( nsp_mat_mult_scalar(loc->S[i],C) == FAIL )
	    return NULL;
	}
      nsp_matrix_destroy(C);
      return loc;
    }
  else if ( A->n == B->m  )
    {
      NspMatrix *As;
      int i,j,k;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      if ((As = nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) 
	return NULLPMAT;
      for (i=0; i < loc->m ; i++) 
	for ( j = 0 ; j < loc->n ; j++)
	  {
	    nsp_polynom l = NULL,l1,l2;
	    k= 0;
	    if ( As->rc_type == 'r' )
	      As->R[0] = A->R[i+A->m*k];
	    else
	      As->C[0] = A->C[i+A->m*k];
	    l = nsp_polynom_mult( As, B->S[k+B->m*j]);
	    if ( l == NULL) return NULL;
	    for ( k = 1 ; k < A->n ; k++)
	      {
		if ( As->rc_type == 'r' )
		  As->R[0] = A->R[i+A->m*k];
		else
		  As->C[0] = A->C[i+A->m*k];
		l1 =  nsp_polynom_mult( As, B->S[k+B->m*j]);
		if ( l1 == NULL ) return NULL;
		if (( l2 =  nsp_polynom_add(l, l1))==NULL) return NULL;
		nsp_polynom_destroy(&l);
		nsp_polynom_destroy(&l1);
		l= l2;
	      }
	    loc->S[i+loc->m*j]=l;
	  }
      nsp_matrix_destroy(As);
      return loc;
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions for product (%d,%d)*(%d,%d)\n",A->m,A->n,B->m,B->n);
      return NULL;
    }
}

NspPMatrix *nsp_pmatrix_mult_p_m(NspPMatrix *A, NspMatrix *B)
{
  NspPMatrix *loc;
  if ( A->mn == 1 ) 
    {
      NspMatrix *C;
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return NULLPMAT;
      if ((C = nsp_matrix_create(NVOID,B->rc_type,1,1))== NULLMAT)
	return NULLPMAT;
      for (i=0; i < B->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_polynom_copy_with_name(A->S[0]))== NULL) goto err;
	  if ( C->rc_type =='r' ) 
	    C->R[0]=B->R[i];
	  else 
	    C->C[0]=B->C[i];
	  if ( nsp_mat_mult_scalar(loc->S[i],C) == FAIL ) goto err;
	}
      nsp_matrix_destroy(C);
      return loc;
    err: 
      nsp_matrix_destroy(C);
      return NULL;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_polynom_copy_with_name(A->S[i]))== NULL) 
	    return NULL;
	  if ( nsp_mat_mult_scalar(loc->S[i],B) == FAIL )
	    return NULL;
	}
      return loc;
    }
  else if ( A->n == B->m  )
    {
      NspMatrix *Bs;
      int i,j,k;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      if ((Bs = nsp_matrix_create(NVOID,B->rc_type,1,1)) == NULLMAT) 
	return NULLPMAT;
      for (i=0; i < loc->m ; i++) 
	for ( j = 0 ; j < loc->n ; j++)
	  {
	    nsp_polynom l = NULL,l1,l2;
	    k= 0;
	    if ( Bs->rc_type == 'r' )
	      Bs->R[0] =  B->R[k+B->m*j];
	    else
	      Bs->C[0] =  B->C[k+B->m*j];
	    l = nsp_polynom_mult(A->S[i+A->m*k] , Bs);
	    if ( l == NULL) return NULL;
	    for ( k = 1 ; k < A->n ; k++)
	      {
		if ( Bs->rc_type == 'r' )
		  Bs->R[0] =  B->R[k+B->m*j];
		else
		  Bs->C[0] =  B->C[k+B->m*j];
		l1 =  nsp_polynom_mult(A->S[i+A->m*k] , Bs);
		if ( l1 == NULL ) return NULL;
		if (( l2 =  nsp_polynom_add(l, l1))==NULL) return NULL;
		nsp_polynom_destroy(&l);
		nsp_polynom_destroy(&l1);
		l= l2;
	      }
	    loc->S[i+loc->m*j]=l;
	  }
      nsp_matrix_destroy(Bs);
      return loc;
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions for product (%d,%d)*(%d,%d)\n",A->m,A->n,B->m,B->n);
      return NULL;
    }
}



NspPMatrix *nsp_pmatrix_mult_p_p(NspPMatrix *A, NspPMatrix *B)
{
  NspPMatrix *loc;
  if ( A->n == B->m  )
    {
      int i,j,k;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < loc->m ; i++) 
	for ( j = 0 ; j < loc->n ; j++)
	  {
	    nsp_polynom l = NULL,l1,l2;
	    k= 0;
	    l = nsp_polynom_mult( A->S[i+A->m*k], B->S[k+B->m*j]);
	    if ( l == NULL) return NULL;
	    for ( k = 1 ; k < A->n ; k++)
	      {
		l1 =  nsp_polynom_mult( A->S[i+A->m*k], B->S[k+B->m*j]);
		if ( l1 == NULL ) return NULL;
		if (( l2 =  nsp_polynom_add(l, l1))==NULL) return NULL;
		nsp_polynom_destroy(&l);
		nsp_polynom_destroy(&l1);
		l= l2;
	      }
	    loc->S[i+loc->m*j]=l;
	  }
      return loc;
    }
  else if ( A->mn == 1 ) 
    {
      return nsp_pmatrix_mult_tt(A,B);
    }
  else if ( B->mn == 1 )
    {
      return nsp_pmatrix_mult_tt(A,B);
    }
  else
    {
      Scierror("Error:\tUncompatible dimensions\n");
      return NULL;
    }
}


NspPMatrix *nsp_pmatrix_mult_tt(NspPMatrix *A, NspPMatrix *B)
{
  NspPMatrix *loc;
  if ( SameDim(A,B) )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]=  nsp_polynom_mult( A->S[i], B->S[i]);
	  if ( loc->S[i] == NULL) return NULL;
	}
      return loc;
    }
  else if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]= nsp_polynom_mult( A->S[0], B->S[i]);
	  if ( loc->S[i] == NULL)     return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] =  nsp_polynom_mult( A->S[i], B->S[0] )) == NULL) 
	    return NULL;
	}
      return loc;
    }
  else
    {
      Scierror("Error:\targuments should have the same size\n");
      return NULL;
    }
}


NspPMatrix *nsp_pmatrix_mult_tt_p_m(NspPMatrix *A, NspMatrix *B)
{
  NspPMatrix *loc;
  if ( SameDim(A,B) )
    {
      int i;
      int flag = (B->rc_type == 'r') ? 1: 2;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]=  nsp_polynom_mult_m( A->S[i], B->R+i*flag,B->rc_type );
	  if ( loc->S[i] == NULL) return NULL;
	}
      return loc;
    }
  else if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]= nsp_polynom_mult_m( A->S[0], B->R+i,B->rc_type);
	  if ( loc->S[i] == NULL)     return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] =  nsp_polynom_mult_m( A->S[i], B->R,B->rc_type  )) == NULL) 
	    return NULL;
	}
      return loc;
    }
  else
    {
      Scierror("Error:\targuments should have the same size\n");
      return NULL;
    }
}


NspPMatrix *nsp_pmatrix_mult_tt_m_p(NspMatrix *A, NspPMatrix *B)
{
  return nsp_pmatrix_mult_tt_p_m(B,A);
}

NspPMatrix *nsp_pmatrix_div_tt_p_m(NspPMatrix *A, NspMatrix *B,int flag )
{
  NspPMatrix *loc;
  if ( SameDim(A,B) && A->mn != 1 )
    {
      if ( flag ) 
	{
	  int i;
	  if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	    return(NULLPMAT);
	  for (i=0; i < B->mn ; i++) 
	    {
	      loc->S[i]= nsp_polynom_div_m( A->S[i], B->R+i,B->rc_type);
	      if ( loc->S[i] == NULL)     return NULL;
	    }
	  return loc;
	}
      else
	{
	  Scierror("Error: unimplemented \n");
	  return NULL;
	}
    }
  else if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,B->m,B->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]= nsp_polynom_div_m( A->S[0], B->R+i,B->rc_type);
	  if ( loc->S[i] == NULL)     return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_pmatrix_create(NVOID,A->m,A->n,NULL,-1))== NULLPMAT) 
	return(NULLPMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] =  nsp_polynom_div_m( A->S[i], B->R,B->rc_type  )) == NULL) 
	    return NULL;
	}
      return loc;
    }
  else
    {
      Scierror("Error:\targuments should have the same size\n");
      return NULL;
    }
}

NspPMatrix *nsp_pmatrix_div_tt_m_p(NspMatrix *A, NspPMatrix *B)
{
  Scierror("Error: unimplemented \n");
  return NULL;
}



/* P(i) evaluated for x= V(k);  */

NspMatrix *nsp_pmatrix_horner_tt(NspPMatrix *P,NspMatrix *V)
{
  int i;
  NspMatrix *loc; 
  /* compute the rc_type for result */
  char type = (V->rc_type == 'c') ? 'c' : 'r';
  for ( i = 0 ; i < P->mn ; i++) 
    if ( P->S[i]->rc_type == 'c') 
      {
	type = 'c'; break;
      }
#define TT_HORNER(s1,s2,i1,i2)						\
  if ((loc = nsp_matrix_create(NVOID,type,s1,s2))==NULLMAT)		\
    return NULL;							\
  if ( loc->rc_type == 'r' )						\
    {									\
      for ( i = 0 ; i < loc->mn ; i++)					\
	{								\
	  loc->R[i] = nsp_hornerdd(P->S[i1]->R,P->S[i1]->mn,V->R[i2]);	\
	}								\
    }									\
  else if ( V->rc_type == 'r' )						\
    {									\
      /* polynom is complex or real */					\
      for ( i = 0 ; i < loc->mn ; i++)					\
	{								\
	  if ( P->S[i]->rc_type == 'r')					\
	    {								\
	      loc->C[i].r = nsp_hornerdd(P->S[i1]->R,P->S[i1]->mn,V->R[i2]); \
	      loc->C[i].i = 0;						\
	    }								\
	  else								\
	    loc->C[i] = nsp_hornercd(P->S[i1]->C,P->S[i1]->mn,V->R[i2]); \
	}								\
    }									\
  else									\
    {									\
      /* V is complex */						\
      for ( i = 0 ; i < loc->mn ; i++)					\
	{								\
	  if ( P->S[i]->rc_type == 'r')					\
	    loc->C[i] = nsp_hornerdc(P->S[i1]->R,P->S[i1]->mn,V->C[i2]); \
	  else								\
	    loc->C[i] = nsp_hornercc(P->S[i1]->C,P->S[i1]->mn,V->C[i2]); \
	}								\
    }									
  if ( P->mn == V->mn ) 
    {
      TT_HORNER(P->m,P->n,i,i);
    }
  else if ( P->mn == 1 )
    {
      TT_HORNER(V->m,V->n,0,i);
    }
  else if ( V->mn == 1 )
    {
      TT_HORNER(P->m,P->n,i,0);
    }
  else
    {
      Scierror("Error: arguments with incompatible dimensions\n");
      return NULL;
    }
  return loc;
}

/* P evaluated for x= V(k);  */

NspMatrix *nsp_pmatrix_horner(NspPMatrix *P,NspMatrix *V,int k)
{
  int i;
  NspMatrix *loc; 
  char type = (V->rc_type == 'c') ? 'c' : 'r';
  for ( i = 0 ; i < P->mn ; i++) 
    if ( P->S[i]->rc_type == 'c') 
      {
	type = 'c'; break;
      }
  if ((loc = nsp_matrix_create(NVOID,type,P->m,P->n))==NULLMAT)
    return NULL;
  if ( loc->rc_type == 'r' )
    {
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  loc->R[i] = nsp_hornerdd(P->S[i]->R,P->S[i]->mn,V->R[k]);
	}
    }
  else if ( V->rc_type == 'r' )
    {
      /* polynom is complex or real */
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  if ( P->S[i]->rc_type == 'r') 
	    {
	      loc->C[i].r = nsp_hornerdd(P->S[i]->R,P->S[i]->mn,V->R[k]);
	      loc->C[i].i = 0;
	    }
	  else
	    loc->C[i] = nsp_hornercd(P->S[i]->C,P->S[i]->mn,V->R[k]);
	}
    }
  else 
    {
      /* V is complex */
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  if ( P->S[i]->rc_type == 'r') 
	    loc->C[i] = nsp_hornerdc(P->S[i]->R,P->S[i]->mn,V->C[k]);
	  else 
	    loc->C[i] = nsp_hornercc(P->S[i]->C,P->S[i]->mn,V->C[k]);
	}
    }
  return loc;
}

int nsp_pmatrix_pdiv_tt(NspPMatrix *A, NspPMatrix *B, NspPMatrix **Q, NspPMatrix **R)
{
  int i, m= Max(A->m,B->m), n=Max(B->m,B->n) , mn=Max(A->mn,B->mn);
  nsp_polynom q,r;
  NspPMatrix *Ql,*Rl;
  if ( !( SameDim(A,B) || A->mn == 1 || B->mn == 1)) 
    {
      Scierror("Error:\targuments should have the same size\n");
      return FAIL;
    }
  if ((Ql =nsp_pmatrix_create(NVOID,m,n,NULL,-1))== NULLPMAT) 
    return FAIL;
  if ((Rl =nsp_pmatrix_create(NVOID,m,n,NULL,-1))== NULLPMAT) 
    return FAIL;
  for (i=0; i < mn ; i++) 
    {
      if (nsp_polynom_pdiv( A->S[Min(i,A->mn)], B->S[Min(i,B->mn)],&q,&r)== FAIL) 
	return FAIL;
      Ql->S[i]= q;
      Rl->S[i]= r;
    }
  *Q = Ql;
  *R = Rl;
  return OK;
}



/**
 * nsp_polynom_mult_fft:
 * @a: a nsp_polynom 
 * @b: a nsp_polynom
 * 
 * return a new polynom @a * @b. The product is 
 * computed with fft. 
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_mult_fft(nsp_polynom a,nsp_polynom b)
{
  int i;
  NspMatrix *A=NULL,*B=NULL,*Af=NULL,*Bf=NULL,*R=NULL;
  if ((A= nsp_matrix_create(NVOID,a->rc_type,(int)1,a->mn+b->mn-1))==NULLMAT)
    goto err;
  if ( A->rc_type == 'r') 
    {
      for ( i=0; i< Min(a->mn,A->mn); i++) A->R[i] = a->R[i];
      for ( i=a->mn; i < A->mn;i++)A->R[i] = 0;
    }
  else 
    {
      for ( i=0; i<  Min(a->mn,A->mn); i++) A->C[i] = a->C[i];
      for ( i=a->mn; i < A->mn;i++) A->C[i].r = A->C[i].i=0;
    }
  if ((B= nsp_matrix_create(NVOID,b->rc_type,(int)1,a->mn+b->mn-1))==NULLMAT)
    goto err;
  if ( B->rc_type == 'r') 
    {
      for ( i=0; i< Min(b->mn,B->mn); i++) B->R[i] = b->R[i];
      for ( i=b->mn; i < B->mn;i++)B->R[i] = 0;
    }
  else 
    {
      for ( i=0; i< Min(b->mn,B->mn); i++) B->C[i] = b->C[i];
      for ( i=b->mn; i < B->mn;i++) B->C[i].r = B->C[i].i=0;
    }
  if ((Af= nsp_fft(A))==NULL) goto err;
  if ((Bf= nsp_fft(B))==NULL) goto err;
  if ( nsp_mat_mult_el(Af,Bf) == FAIL) goto err;
  if ((R = nsp_ifft(Af))==NULL) goto err;
 err:
  if ( A != NULL) nsp_matrix_destroy(A);
  if ( B != NULL) nsp_matrix_destroy(B);
  if ( Af != NULL) nsp_matrix_destroy(Af);
  if ( Bf != NULL) nsp_matrix_destroy(Bf);
  if ( R != NULL ) 
    {
      if ( nsp_object_set_name(NSP_OBJECT(R),"pe") == FAIL) 
	{
	  nsp_matrix_destroy(R);
	  return NULL;
	}
    }
  return R;
}


/**
 * nsp_polynom_mult_std:
 * @a: a nsp_polynom 
 * @b: a nsp_polynom
 * 
 * return a new polynom @a * @b. The product is 
 * computed with standard product. 
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_mult_std(nsp_polynom a,nsp_polynom b)
{
  int i,j;
  NspMatrix *M=NULL;
  NspMatrix *A = (NspMatrix *)a;
  NspMatrix *B = (NspMatrix *)b;
  char type = (a->rc_type == 'c' || b->rc_type == 'c') ? 'c':'r';
  if ((M= nsp_matrix_create(NVOID,type,1,(a->mn+b->mn-1)))==NULLMAT)
    return NULL;
  if ( M->rc_type == 'r') 
    {
      for ( i=0; i < M->mn; i++)
	{
	  M->R[i] =0.0;
	  for (j = Max(0, i - B->mn +1) ; j <= Min(i,A->mn -1) ; j++)
	    M->R[i] += A->R[j]*B->R[i-j];
	}
    }
  else 
    {
      if ( A->rc_type == 'c' &&  B->rc_type == 'c') 
	for ( i=0; i < M->mn; i++)
	  {
	    M->C[i].r=M->C[i].i =0.0;
	    for (j = Max(0, i - B->mn +1) ; j <= Min(i,A->mn -1) ; j++)
	      {
		doubleC x= A->C[j];
		nsp_prod_c(&x,&B->C[i-j]);
		M->C[i].r += x.r;
		M->C[i].i += x.i;
	      }
	  }
      else if ( A->rc_type == 'c') 
	for ( i=0; i < M->mn; i++)
	  {
	    M->C[i].r=M->C[i].i =0.0;
	    for (j = Max(0, i - B->mn +1) ; j <= Min(i,A->mn -1) ; j++)
	      {
		M->C[i].r += A->C[j].r*B->R[i-j];
		M->C[i].i += A->C[j].i*B->R[i-j];
	      }
	  }
      else 
	for ( i=0; i < M->mn; i++)
	  {
	    M->C[i].r=M->C[i].i =0.0;
	    for (j = Max(0, i - B->mn +1) ; j <= Min(i,A->mn -1) ; j++)
	      {
		M->C[i].r += A->R[j]*B->C[i-j].r;
		M->C[i].i += A->R[j]*B->C[i-j].i;
	      }
	  }
    }
  if ( M != NULL ) 
    {
      if ( nsp_object_set_name(NSP_OBJECT(M),"pe") == FAIL) 
	{
	  nsp_matrix_destroy(M);
	  return NULL;
	}
    }
  return M;
}

/**
 * nsp_hornerdd:
 * @a: Array of double, coefficients of the polynomial.
 * @n: Length of A, also degree of polynomial - 1.
 * @x: a double, point at which the polynomial is to be evaluated. 
 * 
 * computes a(1) + a(2)*x + ... + a(n)*x^(n-1) with horner method.
 * 
 * Returns: a double 
 **/

double nsp_hornerdd (const double *a,const int n, double x)
{
  double term;
  int i;
  term = a[n -1];
  for (i = n - 2; i >= 0; --i)
    {
      term = a[i] + term * x;
    }
  return  term;
}

/**
 * nsp_hornercd:
 * @a: Array of complex, coefficients of the polynomial.
 * @n: Length of A, also degree of polynomial - 1.
 * @x: a double, point at which the polynomial is to be evaluated. 
 * 
 * computes a(1) + a(2)*x + ... + a(n)*x^(n-1) with horner method.
 * 
 * Returns: a doubleC 
 **/


doubleC nsp_hornercd(const doubleC *a,const int n, double x)
{
  doubleC term = a[n -1];
  int i;
  for (i = n - 2; i >= 0; --i)
    {
      term.r *= x;
      term.i *= x;
      term.r += a[i].r;
      term.i += a[i].i;
    }
  return  term;
}


/**
 * nsp_hornerdc:
 * @a: Array of double, coefficients of the polynomial.
 * @n: Length of A, also degree of polynomial - 1.
 * @x: a complex, point at which the polynomial is to be evaluated. 
 * 
 * computes a(1) + a(2)*x + ... + a(n)*x^(n-1) with horner method.
 * 
 * Returns: a doubleC 
 **/

doubleC nsp_hornerdc (const double *a,const int n, doubleC x)
{
  doubleC term={a[n -1],0};
  int i;
  for (i = n - 2; i >= 0; --i)
    {
      /* term*x */
      nsp_prod_c(&term,&x); 
      term.r += a[i];
    }
  return  term;
}


/**
 * nsp_hornercc:
 * @a: Array of complex, coefficients of the polynomial.
 * @n: Length of A, also degree of polynomial - 1.
 * @x: a complex, point at which the polynomial is to be evaluated. 
 * 
 * computes a(1) + a(2)*x + ... + a(n)*x^(n-1) with horner method.
 * 
 * Returns: a doubleC
 **/

doubleC nsp_hornercc (const doubleC *a,const int n, doubleC x)
{
  doubleC term = a[n -1];
  int i;
  for (i = n - 2; i >= 0; --i)
    {
      /* term*x */
      nsp_prod_c(&term,&x); 
      term.r += a[i].r;
      term.i += a[i].i;
    }
  return  term;
}

/**
 * nsp_polynom_horner:
 * @P: a #nsp_polynom 
 * @b: a #NspMatrix
 * 
 * return a #NspMatrix, with the same size as @b. 
 * element (i,j) of the result is filled with @P(@b(i,j)).
 * 
 * Returns: a new #NspMatrix or %NULL
 **/

NspMatrix *nsp_polynom_horner(nsp_polynom P,NspMatrix *b)
{
  int i;
  NspMatrix *loc; 
  char type = ( P->rc_type == 'c' || b->rc_type == 'c') ? 'c' : 'r';
  if ((loc = nsp_matrix_create(NVOID,type,b->m,b->n))==NULLMAT)
    return NULL;
  if ( loc->rc_type == 'r' )
    {
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  loc->R[i] = nsp_hornerdd(P->R,P->mn,b->R[i]);
	}
    }
  else if ( b->rc_type == 'r' )
    {
      /* polynom is complex */
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  loc->C[i] = nsp_hornercd(P->C,P->mn,b->R[i]);
	}
    }
  else if ( P->rc_type == 'r' )
    {
      /* b is complex */
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  loc->C[i] = nsp_hornerdc(P->R,P->mn,b->C[i]);
	}
    }
  else
    {
      /* both are complex */
      for ( i = 0 ; i < loc->mn ; i++)
	{
	  loc->C[i] = nsp_hornercc(P->C,P->mn,b->C[i]);
	}
    }
  return loc;
}

/**
 * nsp_polynom_hornerm:
 * @P: a #nsp_polynom 
 * @b: a #NspMatrix
 * 
 * return a #NspMatrix, with the same size as @b and equal 
 * to p(b) = p_0 *Id + p_1*b +p_2*b^2..... p_n b^n.
 *
 * Returns: a new #NspMatrix or %NULL
 **/

NspMatrix *nsp_polynom_hornerm(nsp_polynom P,NspMatrix *b)
{
  NspMatrix *res=NULL,*coef=NULL,*term=NULL;
  int i,j;
  char type = ( P->rc_type == 'c' || b->rc_type == 'c') ? 'c' : 'r';

  if ((coef= nsp_matrix_create(NVOID,P->rc_type,(int)1,(int)1))==NULLMAT)
    goto err;
  if ((res= nsp_matrix_create(NVOID,type,b->m,b->n))==NULLMAT)
    goto err;
  
  for (i = P->mn - 1; i > 0; --i)
    {
      if (  i < P->mn -1 ) term = nsp_mat_mult(res, b, 0);
      if ( coef->rc_type == 'r' )
	coef->R[0]= P->R[i];	
      else				
	coef->C[0] = P->C[i];	
      /* res <- b */
      if (res->rc_type == 'r' ) 
	{
	  memcpy(res->R, b->R, b->mn*sizeof(double));
	}
      else
	{
	  if ( b->rc_type == 'c' ) 
	    memcpy(res->C, b->C, 2*b->mn*sizeof(double));
	  else 
	    for ( j = 0 ; j < res->mn ; j++ )
	      { res->C[j].r = b->R[j]; res->C[j].i = 0; }
	}
      if ( nsp_mat_mult_scalar_bis(res,coef) == FAIL ) goto err;
      if ( i < P->mn -1 ) 
	{
	  if ( nsp_mat_add(res,term)  == FAIL ) goto err;
	  nsp_matrix_destroy(term);
	}
    }
  /* need here to add P_0 */
  if ( res->rc_type == 'r') 
    {
      for ( i = 0 ; i < res->m ; i++ )
	{ 
	  res->R[i+res->m*i] += P->R[0];
	}
    }
  else
    {
      if ( P->rc_type == 'r' ) 
	{
	  for ( i = 0 ; i < res->m ; i++ )
	    { 
	      res->C[i+res->m*i].r += P->R[0];
	    }
	}
      else
	{
	  for ( i = 0 ; i < res->m ; i++ )
	    { 
	      res->C[i+res->m*i].r += P->C[0].r;
	      res->C[i+res->m*i].i += P->C[0].i;
	    }
	}
    }
  return res;
 err:
  if ( term != NULL ) nsp_matrix_destroy(term);
  if ( res != NULL ) nsp_matrix_destroy(res);
  return NULL;
}

/**
 * nsp_polynom_power:
 * @p: a #nsp_polynom 
 * @n: an integer 
 * 
 * returns the polynomial p^n using repeated 
 * squaring. 
 * 
 * Returns: a new polynom or %NULL
 **/

nsp_polynom nsp_polynom_power(nsp_polynom p,int n)
{
  NspMatrix *P = (NspMatrix *) p;
  NspMatrix *R = NULL;
  NspMatrix *Q = NULL;
  NspMatrix *loc = NULL;
  if ( n == 0 ) 
    {
      doubleC d={1,0};
      if ((loc =nsp_basic_to_polynom(&d,p->rc_type)) == (nsp_polynom ) 0)
	return NULL;
      return loc;
    }
  if ( P->mn == 2 && P->rc_type == 'r' && P->R[0]==0 && P->R[1]== 1) 
    {
      /* detect the special case where P=x^n */
      int i;
      if ((loc= nsp_matrix_create("pe",'r', 1 , n+1))==NULLMAT)
	return NULL;
      for ( i = 0 ; i < loc->mn;i++ ) loc->R[i]=0.0;
      loc->R[n]=1;
      return loc;
    }
  /* general case: power by repeated squaring */
  if ((Q = nsp_polynom_copy(p))== NULL) 
    return NULL;
  while  ( n > 1 )
    {
      if ( n % 2 ) 
	{
	  if ( R == NULL) 
	    {
	      if ((R = nsp_polynom_copy(Q))== NULL) goto err;
	    }
	  else
	    {
	      if ((loc = nsp_polynom_mult(R,Q)) == NULL) goto err;
	      nsp_polynom_destroy(&R);
	      R=loc;
	    }
	}
      n /= 2;
      if ((loc = nsp_polynom_mult(Q,Q)) == NULL) goto err;
      nsp_polynom_destroy(&Q);
      Q=loc;
    }
  if ( R != NULL) 
    {
      if ((loc = nsp_polynom_mult(Q,R)) == NULL) goto err;
      nsp_polynom_destroy(&R);
      nsp_polynom_destroy(&Q);
    }
  else 
    {
      loc = Q;
    }
  return loc;
 err:
  if ( Q != NULL) nsp_polynom_destroy(&Q);
  if ( R != NULL) nsp_polynom_destroy(&R);
  return NULL;
}

/**
 * nsp_polynom_add_m:
 * @p: a #nsp_polynom
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new polynomial @p + @v.
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_add_m(nsp_polynom p, void *v, char type)
{
  nsp_polynom loc;
  if ((loc = nsp_polynom_copy_and_name("pe",p))== NULL) 
    return NULL;
  if ( loc->mn == 0) return loc;
  if ( type == 'c') 
    {
      if (nsp_mat_complexify(loc,0.00) == FAIL ) 
	return NULL;
    }
  if ( loc->rc_type == 'r' ) 
    {
      loc->R[0] += *((double *) v);
    }
  else
    {
      if ( type == 'r')
	{
	  loc->C[0].r += *((double *) v);
	}
      else
	{
	  doubleC x= * (doubleC *) v;
	  loc->C[0].r += x.r;
	  loc->C[0].i += x.i;
	}
    }
  return loc;
}

/**
 * nsp_polynom_minus_m:
 * @p: a #nsp_polynom
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new polynomial @p - @v.
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_minus_m(nsp_polynom p, void *v, char type)
{
  nsp_polynom loc;
  if ((loc = nsp_polynom_copy_and_name("pe",p))== NULL) 
    return NULL;
  if ( loc->mn == 0) return loc;
  if ( type == 'c') 
    {
      if (nsp_mat_complexify(loc,0.00) == FAIL ) 
	return NULL;
    }
  if ( loc->rc_type == 'r' ) 
    {
      loc->R[0] -= *((double *) v);
    }
  else
    {
      if ( type == 'r')
	{
	  loc->C[0].r -= *((double *) v);
	}
      else
	{
	  doubleC x= * (doubleC *) v;
	  loc->C[0].r -= x.r;
	  loc->C[0].i -= x.i;
	}
    }
  return loc;
}

/**
 * nsp_polynom_mult_m:
 * @p: a #nsp_polynom
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new polynomial @p * @v.
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_mult_m(nsp_polynom p, void *v, char type)
{
  int i;
  nsp_polynom loc;
  if ((loc = nsp_polynom_copy_and_name("pe",p))== NULL) 
    return NULL;
  if ( loc->mn == 0) return loc;
  if ( type == 'c') 
    {
      if (nsp_mat_complexify(loc,0.00) == FAIL ) 
	return NULL;
    }
  if ( loc->rc_type == 'r' ) 
    {
      for ( i = 0 ; i < loc->mn ; i++)
	loc->R[i] *= *((double *) v);
    }
  else
    {
      if ( type == 'r')
	{
	  for ( i = 0 ; i < loc->mn ; i++)
	    {
	      loc->C[i].r *= *((double *) v);
	      loc->C[i].i *= *((double *) v);
	    }
	}
      else
	{
	  doubleC x= * (doubleC *) v;
	  for ( i = 0 ; i < loc->mn ; i++)
	    nsp_prod_c(&loc->C[i],&x);
	}
    }
  return loc;
}

/**
 * nsp_polynom_div_m:
 * @p: a #nsp_polynom
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new polynomial @p / @v.
 * 
 * Returns: a new #nsp_polynom or %NULL
 **/

nsp_polynom nsp_polynom_div_m(nsp_polynom p, void *v, char type)
{
  int i;
  nsp_polynom loc;
  if ((loc = nsp_polynom_copy_and_name("pe",p))== NULL) 
    return NULL;
  if ( loc->mn == 0) return loc;
  if ( type == 'c') 
    {
      if (nsp_mat_complexify(loc,0.00) == FAIL ) 
	return NULL;
    }
  if ( loc->rc_type == 'r' ) 
    {
      for ( i = 0 ; i < loc->mn ; i++)
	loc->R[i] /= *((double *) v);
    }
  else
    {
      if ( type == 'r')
	{
	  for ( i = 0 ; i < loc->mn ; i++)
	    {
	      loc->C[i].r /= *((double *) v);
	      loc->C[i].i /= *((double *) v);
	    }
	}
      else
	{
	  doubleC x= * (doubleC *) v;
	  for ( i = 0 ; i < loc->mn ; i++)
	    nsp_div_cc(&loc->C[i],&x,&loc->C[i]);
	}
    }
  return loc;
}



/*
 * routines for output of polynomial matrices 
 */
/* XXX */
extern int nsp_matrix_any_element_is_negative (const void *M);
extern int nsp_matrix_any_element_is_inf_or_nan (const void *M);
extern int nsp_matrix_all_elements_are_int_or_inf_or_nan (const void *M);
extern void nsp_matrix_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax);

static char MpInit(const void *M,int *work)
{
  char type = 'r';
  int i;
  *work  = 0;
  for ( i = 0 ; i < ((NspPMatrix *) M)->mn ; i++) 
    if ( ((NspPMatrix *) M)->S[i]->rc_type == 'c') 
      {
	type = 'c'; break;
      }
  return type;
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
      nsp_matrix_pr_min_max_internal (((NspPMatrix*)M)->S[i],'r',&min1,&max1);
      if ( max1 > *dmax ) *dmax=max1;
      if ( min1 < *dmin ) *dmin=min1;
    }
}

/* Polynomial Matrix */

static void Mp_set_format(nsp_num_formats *fmt,NspPMatrix *M)
{
  gen_set_format(fmt,M,Mp_any_element_is_negative,
		 Mp_any_element_is_inf_or_nan,
		 Mp_pr_min_max_internal,
		 Mp_all_elements_are_int_or_inf_or_nan,
		 MpInit);
}


/*
 * Printing Nsp Polynomial Matrices 
 */

static int pr_poly (nsp_num_formats *fmt,NspMatrix *m, int fw, int length, int do_print);
#ifndef POLY_EXP 
static void pr_poly_exp  (NspMatrix *m, int fw, int length);
#endif 


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
  
  Sciprintf("\n");
  /* Allocate a table to store the column width 
   * Iloc[j]= degree max of column j 
   */
  if ((Iloc =nsp_alloc_int(M->n)) == (int*) 0) return FALSE;
  for ( j=0 ; j < M->n ; j++ )
    {
      Iloc[j]= pr_poly(fmt,M->S[j*M->m],fw,0,FALSE);
      for ( i = 1 ; i < M->m ; i++) 
	{
	  int size = pr_poly(fmt,M->S[i+j*M->m],fw,0,FALSE);
	  if ( Iloc[j] < size ) Iloc[j]= size;
	}
    }
  total_width=0;
  for ( j=0 ; j < M->n ; j++) 
    {
      column_width = Iloc[j] + 2;
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
	  t_width +=  Iloc[j];
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
	      if ( imore == 1) return TRUE;
	      p_rows=0;
	    }
	  {
#ifndef  POLY_EXP
	    for ( j = col; j < lim; j++)
	      {
		if (j == 0 )
		  {
		    nsp_pr_white(indent);Sciprintf(" |");
		  }
		else
		  Sciprintf(", ");
		pr_poly_exp ( M->S[i+(M->m)*j], fw,Iloc[j]);
	      }
	    if ( j == nc -1 ) 
	      Sciprintf("\n",j,nc-1);
	    else
	      Sciprintf(" |\n");
#endif 
	    for ( j = col; j < lim; j++)
	      {
		if ( j == 0 )
		  {
		    nsp_pr_white(indent);Sciprintf(" |");
		  }
		else
		  Sciprintf(", ");
		pr_poly (fmt, M->S[i+(M->m)*j], fw,Iloc[j], TRUE);
	      }
	    if ( j == nc  ) 
	      Sciprintf(" |\n",j,nc-1);
	    else
	      Sciprintf("\n",j,nc-1);
	  }
	}
      col += inc;
    }
  FREE(Iloc);
  return TRUE ;
}


static int nsp_print_exponent_utf8(int i, int do_print)
{
  int j;
  char str[8];
  static unsigned char codes[][4]={{226,   129,   176,0},
				   {194,   185,   0,  0},
				   {194,   178,   0,  0},
				   {194,   179,   0,  0},
				   {226,   129,   180,0},
				   {226,   129,   181,0},
				   {226,   129,   182,0},
				   {226,   129,   183,0},
				   {226,   129,   184,0},
				   {226,   129,   185,0}
  };
  sprintf(str,"%d",i);
  if ( str[0] == '0') return 0;
  if ( strcmp(str,"1")==0 ) return 0;
  if ( do_print )
    for ( j = 0 ; j < strlen(str); j++)
      Sciprintf("%s",codes[Min(Max(str[j]- '0',0),9)]);
  return strlen(str);
}

/**
 * pr_poly:
 * @fmt: 
 * @m: 
 * @fw: 
 * @length: length to be used. if positive it gives 
 *     the length that pr_poly should fill, completion by 
 *     white spaces is required.
 * 
 * print a polynom. 
 **/

static int pr_poly (nsp_num_formats *fmt,NspMatrix *m, int fw, int length, int do_print)
{
  int colors[]={ 34,32,31,35,36};
  int i ,count = 0, leading = TRUE;
  for ( i=0 ; i < m->mn ; i++) 
    {
      if ( m->rc_type == 'r') 
	{
	  if (  m->R[i] != 0.00 || m->mn == 1  )
	    {
	      if ( leading == FALSE && m->R[i] >= 0.00 ) 
		{
		  if (do_print) Sciprintf("+");
		  count++;
		}
	      count +=nsp_pr_any_float_vs(fmt->curr_real_fmt, m->R[i], fw,do_print);
	      leading = FALSE;
	       if (do_print) Sciprintf("\033[%dm",colors[0]);
	      if ( i > 0 ) 
		{
		  if (do_print) Sciprintf("x");count++;
		}
#ifdef POLY_EXP 
	      count += nsp_print_exponent_utf8(i,do_print);
#else 
	      count += pr_poly_exp_size(i);
	      if (do_print) nsp_pr_white(count);
#endif 
	       if (do_print) Sciprintf("\033[0m");
	    }
	}
      else
	{
	  if ( ( m->C[i].r != 0.00 || m->C[i].i != 0.00)  || m->mn == 1  )
	    {
	      if ( leading == FALSE ) 
		{
		  if (do_print) Sciprintf("+");
		  count++;
		}
	      if (do_print)Sciprintf("(");count++;
	      count +=nsp_pr_any_float_vs(fmt->curr_real_fmt, m->C[i].r, fw,do_print);
	      if (do_print)Sciprintf("+");count++;
	      count +=nsp_pr_any_float_vs(fmt->curr_imag_fmt, m->C[i].i, fw,do_print);
	      if (do_print)Sciprintf("i)");count++;
	      leading = FALSE;
	       if (do_print) Sciprintf("\033[%dm",colors[0]);
	      if ( i > 0 ) 
		{
		  if (do_print) Sciprintf("x");count++;
		}
#ifdef POLY_EXP 
	      count += nsp_print_exponent_utf8(i,do_print);
#else 
	      count += pr_poly_exp_size(i);
	      if (do_print) nsp_pr_white(count);
#endif 
	       if (do_print) Sciprintf("\033[0m");
	    }
	}
    }
  if ( length > 0 ) 
    {
      if (do_print)  nsp_pr_white(length-count);
    }
  return count;
}

/**
 * pr_poly_exp:
 * @m: 
 * @fw: 
 * @length: 
 * 
 * prints the exponent part of the polynom.
 **/
#ifndef POLY_EXP
static void pr_poly_exp (NspMatrix *m, int fw, int length)
{
  int i ,count = 0;
  for ( i=0 ; i < m->mn ; i++) 
    {
      if ( m->rc_type == 'r') 
	{
	  if (  m->R[i] != 0.00 || m->mn == 1 )
	    {
	      count += fw+2 + pr_poly_exp_size(i);
	      nsp_pr_white(fw+2);
	      if ( i > 0 ) Sciprintf("%d",i);
	    }
	}
    }
  nsp_pr_white(length-count);
}

#endif 




