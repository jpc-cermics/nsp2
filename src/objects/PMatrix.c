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
  doubleC str;
  if ((loc =nsp_pmatrix_create(NVOID,1,1,&str,0))== NULLPMAT) return(NULLPMAT);
  nsp_matrix_destroy(loc->S[0]);
  if (( loc->S[0] = nsp_matrix_copy(M))== NULLPOLY ) return(NULLPMAT);
  return(loc);
}



/*
 * PMatInfo : display Info on NspPMatrix PMat 
 */

void nsp_pmatrix_info(NspPMatrix *Mat, int indent)
{
  int i;
  if ( Mat == NULLPMAT) 
    {
      Sciprintf("Null Pointer Poly Matrix \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("PMatrix %s(%d,%d)\n",NSP_OBJECT(Mat)->name,Mat->m,Mat->n);
}

/*
 * PMatPrint : writes PMat Objet 
 */

void nsp_pmatrix_print(NspPMatrix *Mat, int indent)
{
  int i,j,k;
  Sciprintf("PMat : [%s (%dx%d)]\n",NSP_OBJECT(Mat)->name,Mat->m,Mat->n);
  for (i = 0 ; i < Mat->m; i++ ) 
    {
      for ( j= 0 ; j < Mat->n ; j++) 
	{
	  NspMatrix *loc;
	  loc = Mat->S[i+j*Mat->m];
	  Sciprintf("M(%d,%d):",i,j);
	  for ( k = 0 ; k < loc->mn ; k ++) 
	    { 
	      if ( loc->rc_type == 'r')
		Sciprintf(" %8g X^%d",loc->R[k],k);
	      else 
		Sciprintf(" (%8g+i%8g) X^%d",loc->C[k].r,loc->C[k].i,k);
	    }
	  Sciprintf("\n");
	}
    }
  Sciprintf("EndPMat\n");
  nsp_print_internalPM (Mat,indent);
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
  if ( ( NSP_OBJECT(Loc)->name =new_nsp_string(name)) == (char *) 0) return(NULLPMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    Loc->otype = PMATRIX;
    Loc->ftype = PMatrix_Type;
  */
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  Loc->rc_type = 'r' ; /* XXXXX : a preciser ? **/
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

/*
 * Delete the NspPMatrix A
 */

void nsp_pmatrix_destroy(NspPMatrix *A)
{
  int i;
  if ( A == NULLPMAT) return;
  FREE( NSP_OBJECT(A)->name);
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
  doubleC d;
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

/*
 * PMatRedim : Changes matrix dimensions
 * m*n must be unchanged 
 * The NspPMatrix is changed (m,n are changed ) 
 * return 0 on failure 
 */

int nsp_pmatrix_redim(NspPMatrix *A, int m, int n)
{
  if ( A->mn ==  m*n ) 
    {
      A->m =m ;
      A->n =n;
      return(OK);
    }
  else 
    {
      Scierror("PMatRedim : can't redim");
      return(FAIL);
    }
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
  A->S = (nsp_polynom *)  REALLOC (A->S, m*n * sizeof(nsp_polynom));
  if ( A->S == (nsp_polynom *) 0) return(FAIL);
  for ( i = A->mn ; i < m*n ; i++ )
    {
      if ((A->S[i] =nsp_basic_to_polynom(&Czero,'r')) == ( nsp_polynom ) 0 )  return(FAIL);
    }
  A->m =m ;
  A->n =n;
  A->mn=m*n ;
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


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Rows and Cols are unchanged 
 *  Size Compatibility is checked 
 */

int nsp_pmatrix_set_submatrix(NspPMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspPMatrix *B)
{
  int rmin,rmax,cmin,cmax,i,j;
  if ( B->mn != 1)
    {
      if ( Rows->mn != B->m ||  Cols->mn != B->n )
	{
	  Scierror("Set incompatible indices ");
	  return(FAIL);
	}
    }
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( rmin < 1 || cmin < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      return(FAIL);
    }
  if ( rmax > A->m ||  cmax > A->n )
    if ( nsp_pmatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  if ( B->mn != 1) 
    for ( i = 0 ; i < Rows->mn ; i++)
      for ( j = 0 ; j < Cols->mn ; j++ )
	{
	  nsp_polynom_destroy(&((A->S[((int) Rows->R[i])-1+ (((int) Cols->R[j])-1)*A->m])));
	  if (( A->S[((int) Rows->R[i])-1+ (((int)Cols->R[j])-1)*A->m] 
		=nsp_polynom_copy(B->S[i+B->m*j]))
	      == NULLPOLY)  return(FAIL);
	}
  else
    for ( i = 0 ; i < Rows->mn ; i++)
      for ( j = 0 ; j < Cols->mn ; j++ )
	{
	  nsp_polynom_destroy(&((A->S[((int) Rows->R[i])-1+ (((int) Cols->R[j])-1)*A->m])));
	  if (( A->S[((int) Rows->R[i])-1+ (((int)Cols->R[j])-1)*A->m] 
		=nsp_polynom_copy(B->S[0]))
	      == NULLPOLY )  return(FAIL);
	}
  return(OK);
}

/*
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 */

int nsp_pmatrix_set_rows(NspPMatrix *A, NspMatrix *Rows, NspPMatrix *B)
{
  int i,Bscal=0;
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Rows,B,B->m,B->n,B->mn,
		     (F_Enlarge) nsp_smatrix_enlarge,&Bscal)== FAIL) 
    return FAIL;
  if ( Bscal == 0) 
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	nsp_polynom_destroy(&((A->S[((int) Rows->R[i])-1])));
	if (( A->S[((int) Rows->R[i])-1] =nsp_polynom_copy(B->S[i]))
	    == (nsp_polynom) 0)  return(FAIL);
      }
  else
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	nsp_polynom_destroy(&((A->S[((int) Rows->R[i])-1])));
	if (( A->S[((int) Rows->R[i])-1] =nsp_polynom_copy(B->S[0]))
	    == (nsp_polynom) 0)  return(FAIL);
      }
  return(OK);
}


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 * Rows and Cols are changed (i.e) converted to int (see Matd2i) 
 *  Size Compatibility is checked 
 */

int nsp_pmatrix_setrc(NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspPMatrix *B)
{
  int rmin,rmax,cmin,cmax,i,j,*Icol,*Irow;
  if ( Rows->mn != B->m ||  Cols->mn != B->n )
    {
      Scierror("Set incompatible indices ");
      return(FAIL);
    }
  Irow = Matd2i(Rows,&rmin,&rmax);
  /* Matd2i changes Rows, thus we must check if Rows == Cols 
     before changing Cols again : **/
  if ( Cols == Rows ) 
    { cmin=rmin;cmax=rmax ; Icol = Irow ;}
  else 
    {Icol = Matd2i(Cols,&cmin,&cmax);}
  if ( rmin < 1 || cmin < 1 ) 
    {
      Scierror("negative indices");
      return(FAIL);
    }
  if ( rmax > A->m ||  cmax > A->n )
    if ( nsp_pmatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < Cols->mn ; j++ )
      {
	nsp_polynom_destroy(&(A->S[Irow[i]-1+ (Icol[j]-1)*A->m]));
	if (( A->S[Irow[i]-1+ (Icol[j]-1)*A->m] =nsp_polynom_copy(B->S[i+B->m*j]))
	    == (nsp_polynom ) 0)  return(FAIL);
      }
  return(OK);
}

/*
 * Res=nsp_pmatrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are changed (i.e converted to int) 
 * 
 */	

NspPMatrix *nsp_pmatrix_extract(NspPMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  NspPMatrix *Loc;
  int rmin,rmax,cmin,cmax,i,j,*Irow,*Icol;
  Irow = Matd2i(Rows,&rmin,&rmax);
  /* Matd2i changes Rows, thus we must check if Rows == Cols 
     before changing Cols again : **/
  if ( Cols == Rows ) 
    { cmin=rmin;cmax=rmax ; Icol = Irow ;}
  else 
    {Icol = Matd2i(Cols,&cmin,&cmax);}
  if ( rmin < 1 || cmin < 1 || rmax > A->m || cmax > A->n ) 
    {
      Scierror("Extraction wrong indices");
      return(NULLPMAT);
    }
  Loc =nsp_pmatrix_create(NVOID,Rows->mn,Cols->mn,&Czero,(int) 0);
  if ( Loc == NULLPMAT) 
    {
      return(NULLPMAT);
    }
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < Cols->mn ; j++ )
      {
	nsp_polynom_destroy(&Loc->S[i+Loc->m*j]);
	if ((Loc->S[i+Loc->m*j] =nsp_polynom_copy(A->S[Irow[i]-1+(Icol[j]-1)*A->m]))
	    == (nsp_polynom ) 0 ) return(NULLPMAT);
      }
  return(Loc);
}

/*
 * Res=nsp_smatrix_extract_elements(A,Elts)
 * A unchanged, Elts
 */	

NspPMatrix*nsp_pmatrix_extract_elements(NspPMatrix *A, NspMatrix *Elts, int *err)
{
  NspPMatrix *Loc;
  int rmin,rmax,i;
  Bounds(Elts,&rmin,&rmax);
  *err=0;
  if ( A->mn == 0) return nsp_pmatrix_create(NVOID,0,0,NULL,-1);
  if ( rmin < 1 || rmax > A->mn )
    {
      *err=1;
      return(NULLPMAT);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      if ((Loc =nsp_pmatrix_create(NVOID,1,Elts->mn,NULL,-1)) ==NULLPMAT) return(NULLPMAT);
    }
  else
    {
      if ( (Loc =nsp_pmatrix_create(NVOID,Elts->mn,1,NULL,-1)) == NULLPMAT) return(NULLPMAT);
    }
  for ( i = 0 ; i < Elts->mn ; i++)
    { 
      if ((Loc->S[i] =nsp_polynom_copy(A->S[((int) Elts->R[i])-1]))== NULLPOLY)  return(NULLPMAT);
    }
  return(Loc);
}

/*
 * Res=nsp_pmatrix_extract_columns(A,Cols,err)
 * A unchanged
 */

NspPMatrix*nsp_pmatrix_extract_columns(NspPMatrix *A, NspMatrix *Cols, int *err)
{
  NspPMatrix *Loc;
  int j,cmin,cmax;
  *err=0;
  if ( A->mn == 0) return nsp_pmatrix_create(NVOID,0,0,NULL,-1);
  Bounds(Cols,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->n )
    {
      *err=1;
      return(NULLPMAT);
    }
  if ((Loc =nsp_pmatrix_create(NVOID,A->m,Cols->mn,NULL,-1))==NULLPMAT)  return(NULLPMAT);
  for ( j = 0 ; j < Cols->mn ; j++ )
    {
      int ind=(((int) Cols->R[j])-1)*A->m, i, ind1=Loc->m*j;
      for ( i = A->m -1 ; i >= 0 ; i--) 
	{
	  if (( Loc->S[ind1+i] =nsp_polynom_copy( A->S[ind+i])) == NULLPOLY)  return NULLPMAT;
	}
    }
  return(Loc);
}

/*
 * Res=nsp_pmatrix_extract_rows(A,Rows,err)
 * A unchanged
 */

NspPMatrix*nsp_pmatrix_extract_rows(NspPMatrix *A, NspMatrix *Rows, int *err)
{
  NspPMatrix *Loc;
  int i,j,cmin,cmax;
  *err=0;
  if ( A->mn == 0) return nsp_pmatrix_create(NVOID,0,0,NULL,-1);
  Bounds(Rows,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->m )
    {
      *err=1;
      return(NULLPMAT);
    }
  if ((Loc =nsp_pmatrix_create(NVOID,Rows->mn,A->n,NULL,-1)) == NULLPMAT )   return(NULLPMAT);
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < A->n ; j++ )
      {
	if (( Loc->S[i+ j*Loc->m]=nsp_polynom_copy(A->S[(((int) Rows->R[i])-1)+ j*A->m]))
	    == NULLPOLY)  return NULLPMAT;
      }
  return(Loc);
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
