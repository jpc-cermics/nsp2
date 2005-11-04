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
#include <ctype.h> /* tolower toupper */

#include "nsp/object.h"
#include "nsp/interf.h" /* for ret_endfor */
#include "nsp/pr-output.h" 

#include "nsp/matutil.h"
#include "nsp/gsort-p.h"

static int nsp_set_cells(int n, NspObject *s1, NspObject **s2);
static int nsp_copy_cells(int n, NspObject **s1, NspObject **s2);

/*
 * Creation of a NspCells all the elements
 *   are initiated to NULL 
 * HINT : 
 * The array for storing the objects is of size (m*n)+1 
 * and the last element is set to (char *) 0 
 * This can be used to detect the last element in XXX->S
 */

NspCells *nsp_cells_create(const char *name, int m, int n)
{
  int i;
  NspCells *Loc = new_cells();
  if ( Loc == NULLCELLS) 
    { 
      Scierror("Error:\tRunning out of memory\n");
      return(NULLCELLS);
    }
  if ( ( NSP_OBJECT(Loc)->name =new_nsp_string(name)) == NULLSTRING) return(NULLCELLS);
  NSP_OBJECT(Loc)->ret_pos = -1 ;
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  if ( Loc->mn == 0 ) 
    {
      /* empty string Matrix */
      Loc->objs = NULL;
      Loc->m = Loc->n = 0;
      return(Loc);
    }
  if ((Loc->objs = (NspObject **) MALLOC((Loc->mn+1)* sizeof(NspObject *))) == NULL)
    { 
      Scierror("cell create : Error no more space\n");
      return(NULLCELLS);
    }
  for ( i = 0 ; i < Loc->mn ; i++ ) Loc->objs[i]= NULL;
  Loc->objs[Loc->mn]= NULL;
  return(Loc);
}

/*
 * Res =nsp_cells_create_from_table(T) 
 * T string table ended with (char *)0.
 */

NspCells*nsp_cells_create_from_table(NspObject **T)
{
  NspCells *Loc;
  int i=0,count=0;
  while ( T[count] != NULL) count++;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_cells_create(NVOID,count,1)) == NULLCELLS) return(NULLCELLS);
  /* allocate elements and store copies of elements **/
  for ( i = 0 ; i < count ; i++ )
    {
      if ((Loc->objs[i]=nsp_object_copy(T[i])) == NULLOBJ) return NULLCELLS;
      if (nsp_object_set_name(Loc->objs[i],"ce") == FAIL) return  NULLCELLS;
    }
  return(Loc);
}

/*
 * Res =nsp_cells_create_from_array(n,T) 
 */

NspCells* nsp_cells_create_from_array(const char *name,int n, NspObject **T)
{
  NspCells *Loc;
  int i=0;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_cells_create(name,n,1)) == NULLCELLS) return(NULLCELLS);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < n ; i++ )
    {
      if ((Loc->objs[i]=nsp_object_copy(T[i])) == NULLOBJ) return NULLCELLS;
      if (nsp_object_set_name(Loc->objs[i],"ce") == FAIL) return  NULLCELLS;
    }
  return(Loc);
}

/*
 * Res =nsp_cells_copy(A) 
 * Creates a Copy of NspCells A : A is not checked 
 */

NspCells*nsp_cells_copy(const NspCells *A)
{
  int i;
  NspCells *Loc;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_cells_create(NVOID,A->m,A->n) ) == NULLCELLS) return(NULLCELLS);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->objs[i] != NULLOBJ) 
	{
	  if ((Loc->objs[i]=nsp_object_copy_with_name(A->objs[i])) == NULLOBJ) return NULLCELLS;
	}
      else 
	{
	  Loc->objs[i]= NULLOBJ;
	}
    }
  return(Loc);
}

/*
 *nsp_cells_resize: Changes NspCells dimensions
 * Warning : this routine only enlarges the array 
 * of the NspCells storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspCells is changed 
 * return FAIL on failure 
 * should also work with A==[] XXXXX 
 */

int nsp_cells_resize(NspCells *A, int m, int n)
{
  int i;
  if ( A->mn == m*n ) 
    {
      /* easy case : nothing to allocate **/
      if ( A->mn == 0) 
	{
	  A->m = A->n = 0;
	}
      else 
	{
	  A->m=m;
	  A->n=n;
	}
      return(OK);
    };
  if ( m*n < 0) return FAIL;
  if ( m*n < A->mn )
    {
      /* Clear before Realloc 
       * note that nsp_object_destroy 
       * works for A->objs[i]==NULL
       */
      for ( i = m*n ; i < A->mn ; i++ )
	nsp_object_destroy(&(A->objs[i]));
    }
  if ( m*n == 0 ) 
    {
      A->m =  A->n = A->mn= 0;
      FREE(A->objs);
      return OK;
    }
  
  if ( A->mn == 0 ) 
    A->objs = (NspObject **)  MALLOC ((m*n+1) * sizeof(NspObject*));
  else 
    A->objs = (NspObject **)  REALLOC (A->objs, (m*n+1) * sizeof(NspObject*));
  if ( A->objs == (NspObject **) 0) return(FAIL);

  /* Initialize new area **/
  A->objs[(m*n)] = NULL;
  for ( i = A->mn ; i < m*n ; i++ )
    {
      A->objs[i] = NULL;
    }
  A->m =m ;
  A->n =n;
  A->mn=m*n ;
  if ( A->mn == 0) A->m = A->n = 0;
  return(OK);
}


/*
 * Delete the NspCells A
 */

void nsp_cells_destroy(NspCells *A)
{
  int i;
  if ( A == NULLCELLS) return;
  FREE(NSP_OBJECT(A)->name);
  if ( A-> mn != 0 ) 
    {
      for ( i = 0 ; i < A->mn ; i++ ) 
	{
	  nsp_object_destroy(&(A->objs[i]));
	}
      FREE(A->objs);
    }
  FREE(A);
}



/*
 *nsp_cells_info: display Info on Matrix Mat 
 */

void nsp_cells_info(const NspCells *Mat, int indent)
{
  int i;
  if ( Mat == NULLCELLS) 
    {
      Sciprintf("Null Pointer NspCells \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) == 0) 
    Sciprintf("Cells (%dx%d) \n",Mat->m,Mat->n);
  else
    Sciprintf("Cells %s(%dx%d) \n",NSP_OBJECT(Mat)->name,Mat->m,Mat->n);
}

/*
 *nsp_cells_print: writes Mat Objet 
 */

void nsp_cells_print(const NspCells *Mat, int indent)
{
  int i;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf("%s=%s",NSP_OBJECT(Mat)->name,(Mat->mn==0 ) ? " {}\n" : "" );
	}
    }
  else 
    {
      Sciprintf("%s\t=%s\t\tcells (%dx%d)\n",NSP_OBJECT(Mat)->name,
		(Mat->mn==0 ) ? " {}" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      for ( i=0 ; i < indent+1 ; i++) Sciprintf(" ");
      Sciprintf("{\n");
      for ( i = 0 ; i < Mat->mn; i++ ) 
	{
	  NspObject *object = Mat->objs[i];
	  if ( object != NULL ) 
	    object->type->pr(object,indent+2,TRUE);
	}
      for ( i=0 ; i < indent+1 ; i++) Sciprintf(" ");
      Sciprintf("}\n");
    }
}


/*
 * nsp_cells_redim: Changes matrix dimensions
 * m*n must be unchanged 
 * The NspCells is changed (m,n are changed ) 
 * return 0 on failure 
 */

int nsp_cells_redim(NspCells *A, int m, int n)
{
  if ( A->mn ==  m*n ) 
    {
      A->m =m ;
      A->n =n;
      return(OK);
    }
  else 
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n"
	       ,m,n,A->mn);
      Scierror("CellsRedim : can't redim");
      return(FAIL);
    }
}



/*
 *nsp_cells_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A 
 */

int nsp_cells_enlarge(NspCells *A, int m, int n)
{
  if ( A->mn == 0) return nsp_cells_resize(A,m,n);
  if ( n > A->n  )
    if ( nsp_cells_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if ( nsp_cells_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(SMAT1,SMAT2) ( SMAT1->m == SMAT2->m && SMAT1->n == SMAT2->n  )

/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int nsp_cells_concat_right(NspCells *A,const NspCells *B)
{
  int Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("Error:\tincompatible dimensions\n");
      return(FAIL);
    }
  if ( nsp_cells_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  if ( nsp_copy_cells(B->mn,B->objs,A->objs+Asize) == FAIL) return(FAIL);
  return(OK);
}

static int nsp_copy_cells(int n, NspObject **s1, NspObject **s2)
{
  int i;
  for ( i = n-1 ; i >= 0 ; i--) 
    {
      nsp_object_destroy(&(s2[i]));
      if ( s1[i] != NULL) 
	{
	  if ((s2[i]=nsp_object_copy_with_name(s1[i])) == NULLOBJ) return FAIL;
	}
      else 
	{
	  s2[ i] = NULL;
	}

    }
  return(OK);
}

/*
 *nsp_cells_add_columns: add n cols of zero to NspCells A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int nsp_cells_add_columns(NspCells *A, int n)
{
  if (n == 0) return OK;
  else if ( n < 0) 
    {      
      Scierror("Error: Negative indice (%d) in MatAddCols\n",n);
      return FAIL;
    }
  if ( nsp_cells_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  /*  normalemeny inutile car Resize le fait deja 
      int Asize;
      Asize=A->mn;
      ns= (A->m)*n;
      if ( nsp_set_cells(ns,".",A->objs+Asize) == FAIL) return(FAIL);**/
  return(OK);
}

static int nsp_set_cells(int n, NspObject *s1, NspObject **s2)
{
  int i;
  for ( i = 0 ; i < n ; i++) 
    {
      nsp_object_destroy(&(s2[i]));
      if ( s1 != NULL) 
	{
	  if ((s2[i]=nsp_object_copy_with_name(s1)) == NULLOBJ) return FAIL;
	}
      else 
	{
	  s2[ i] = NULL;
	}
    }
  return(OK);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLCELLS on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

NspCells*nsp_cells_concat_down(const NspCells *A,const NspCells *B)
{
  NspCells *Loc;
  int j;
  if ( A->n != B->n ) 
    {
      Scierror("Error: [.;.] incompatible dimensions\n");
      return(NULLCELLS);
    }
  if ((Loc =nsp_cells_create(NVOID,A->m+B->m,A->n)) == NULLCELLS) 
    return(NULLCELLS);
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      if ( nsp_copy_cells(A->m,A->objs+j*A->m,Loc->objs+j*(Loc->m)) == FAIL) 
	return(NULLCELLS);
      if ( nsp_copy_cells(B->m,B->objs+j*B->m,Loc->objs+j*(Loc->m)+A->m) == FAIL)
	return(NULLCELLS);
    }
  return(Loc) ;
}

/*
 * Diag Concatenation
 * Res = [A,0;0,B]
 * return NULLBMAT on failure ( No more space )
 * A and B are left unchanged
 * XXXXXX : A Faire ????
 */

/*
 * Add Rows : Add m rows of zero to a NspCells A 
 * A = [A;ones(m,n)]
 * return NULLCELLS on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int nsp_cells_add_rows(NspCells *A, int m)
{
  int Am;
  int j;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in CellsAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if ( nsp_cells_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  for ( j = A->n-1  ; j >= 1 ; j-- ) 
    {
      if (  nsp_copy_cells(Am,A->objs+j*Am,A->objs+j*(A->m)) == FAIL) 
	return(FAIL);
    }
  for ( j = A->n-2  ; j >= 0 ; j-- ) 
    {
      if (  nsp_set_cells (m,NULLOBJ,A->objs+j*(A->m)+Am) == FAIL)
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

int nsp_cells_set_submatrix(NspCells *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspCells *B)
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
    if ( nsp_cells_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  if ( B->mn != 1) 
    for ( i = 0 ; i < Rows->mn ; i++)
      for ( j = 0 ; j < Cols->mn ; j++ )
	{
	  nsp_object_destroy(&((A->objs[((int) Rows->R[i])-1+ (((int) Cols->R[j])-1)*A->m])));
	  if ( B->objs[i+B->m*j] != NULLOBJ ) 
	    {
	      if (( A->objs[((int) Rows->R[i])-1+ (((int)Cols->R[j])-1)*A->m] 
		    = nsp_object_copy_with_name(B->objs[i+B->m*j]))
		  == NULL)  return(FAIL);
	    }
	}
  else
    for ( i = 0 ; i < Rows->mn ; i++)
      for ( j = 0 ; j < Cols->mn ; j++ )
	{
	  nsp_object_destroy(&((A->objs[((int) Rows->R[i])-1+ (((int) Cols->R[j])-1)*A->m])));
	  if ( B->objs[0] != NULL )
	    {
	      if (( A->objs[((int) Rows->R[i])-1+ (((int)Cols->R[j])-1)*A->m] 
		    = nsp_object_copy_with_name(B->objs[0]))
		  == NULL)  return(FAIL);
	    }
	}
  return(OK);
}


/*
 */

int nsp_cells_set_element(NspCells *A,int index, NspObject *B)
{
  if ( index < 0 || index > A->mn ) 
    {
      Scierror("Error:\tindex %d is out of range\n",index);
      return FAIL;
    }
  nsp_object_destroy(&A->objs[index]);
  A->objs[index]=B;
  return(OK);
}


/*
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 */

int nsp_cells_set_rows(NspCells *A, NspMatrix *Rows, NspCells *B)
{
  int i,Bscal=0;
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Rows,B,B->m,B->n,B->mn,
		     (F_Enlarge) nsp_cells_enlarge,&Bscal)== FAIL) 
    return FAIL;
  if ( Bscal == 0) 
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	nsp_object_destroy(&((A->objs[((int) Rows->R[i])-1])));
	if ( B->objs[i] != NULL) 
	  {
	    if (( A->objs[((int) Rows->R[i])-1] = nsp_object_copy_with_name(B->objs[i]))
		== NULL )  return(FAIL);
	  }
      }
  else
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	nsp_object_destroy(&((A->objs[((int) Rows->R[i])-1])));
	if ( B->objs[0] != NULL)
	  {
	    if (( A->objs[((int) Rows->R[i])-1] = nsp_object_copy_with_name(B->objs[0]))
		== NULL)  return(FAIL);
	  }
      }
  return(OK);
}

/*
 *  A(:,Cols) = []
 */

int nsp_cells_delete_columns(NspCells *A, NspMatrix *Cols)
{
  return nsp_smatrix_delete_columns((NspSMatrix *) A,Cols);
}

/*
 *  A(Rows,:) = []
 */

int nsp_cells_delete_rows(NspCells *A, NspMatrix *Rows)
{
  return nsp_smatrix_delete_rows((NspSMatrix *) A,Rows);
}

/*
 *  A(elts) = []
 *
 */

int nsp_cells_delete_elements(NspCells *A, NspMatrix *Elts)
{
  return nsp_smatrix_delete_elements((NspSMatrix *) A,Elts);
}


/*
 * Res=nsp_cells_extract(A,Rows,Cols)
 * A, Rows and Cols are unchanged 
 */	

NspCells*nsp_cells_extract(NspCells *A, NspMatrix *Rows, NspMatrix *Cols)
{
  NspCells *Loc;
  int rmin,rmax,cmin,cmax,i,j;
  if ( A->mn == 0) return nsp_cells_create(NVOID,0,0);
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( rmin < 1 || cmin < 1 || rmax > A->m || cmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLCELLS);
    }
  if ((Loc =nsp_cells_create(NVOID,Rows->mn,Cols->mn))== NULLCELLS) 
    return(NULLCELLS);
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < Cols->mn ; j++ )
      {
	NspObject *Ob=A->objs[((int) Rows->R[i])-1+(((int) Cols->R[j])-1)*A->m];
	if ( Ob != NULLOBJ )
	  if ((Loc->objs[i+Loc->m*j] = nsp_object_copy_with_name(Ob)) == NULL ) return(NULLCELLS);
      }
  return(Loc);
}



/*
 * Res=nsp_cells_extract_elements(A,Elts)
 * A unchanged, Elts
 */	

NspCells*nsp_cells_extract_elements(NspCells *A, NspMatrix *Elts, int *err)
{
  NspCells *Loc;
  int rmin,rmax,i;
  Bounds(Elts,&rmin,&rmax);
  *err=0;
  if ( A->mn == 0) return nsp_cells_create(NVOID,0,0);
  if ( rmin < 1 || rmax > A->mn )
    {
      *err=1;
      return(NULLCELLS);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      if ((Loc =nsp_cells_create(NVOID,1,Elts->mn)) ==NULLCELLS) return(NULLCELLS);
    }
  else
    {
      if ( (Loc =nsp_cells_create(NVOID,Elts->mn,1)) == NULLCELLS) return(NULLCELLS);
    }
  for ( i = 0 ; i < Elts->mn ; i++)
    { 
      NspObject *Ob=A->objs[((int) Elts->R[i])-1];
      if ( Ob != NULL) 
	{
	  if ((Loc->objs[i] = nsp_object_copy_with_name(Ob))== NULL)  return(NULLCELLS);
	}
    }
  return(Loc);
}

/*
 * Res=nsp_cells_extract_columns(A,Cols,err)
 * A unchanged
 */

NspCells*nsp_cells_extract_columns(NspCells *A, NspMatrix *Cols, int *err)
{
  NspCells *Loc;
  int j,cmin,cmax;
  *err=0;
  if ( A->mn == 0) return nsp_cells_create(NVOID,0,0);
  Bounds(Cols,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->n )
    {
      *err=1;
      return(NULLCELLS);
    }
  if ((Loc =nsp_cells_create(NVOID,A->m,Cols->mn)) == NULLCELLS)  return(NULLCELLS);
  for ( j = 0 ; j < Cols->mn ; j++ )
    {
      int ind=(((int) Cols->R[j])-1)*A->m, i, ind1=Loc->m*j;
      for ( i = A->m -1 ; i >= 0 ; i--) 
	{
	  if (( Loc->objs[ind1+i] = nsp_object_copy_with_name( A->objs[ind+i])) == NULL)  return NULLCELLS;
	}
    }
  return(Loc);
}

/*
 * A1=CellsLoopCol(A1,M,i,rep)
 * Used in for loops
 */

NspCells*CellsLoopCol(char *str, NspCells *Col, NspCells *A, int icol, int *rep)
{
  register int iof;
  NspCells *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return(NULLCELLS);
    }
  *rep =0;
  if ( Col == NULLCELLS)
    Loc =nsp_cells_create(str,A->m,1);
  else
    Loc = Col;
  if ( Loc == NULLCELLS) return(NULLCELLS);
  iof = (icol-1)*A->m;
  if ( nsp_copy_cells(A->m,A->objs + iof ,Loc->objs) == FAIL) return NULLCELLS;
  return(Loc);
}

/*
 * Res=nsp_cells_extract_rows(A,Rows,err)
 * A unchanged
 */

NspCells*nsp_cells_extract_rows(NspCells *A, NspMatrix *Rows, int *err)
{
  NspCells *Loc;
  int i,j,cmin,cmax;
  *err=0;
  if ( A->mn == 0) return nsp_cells_create(NVOID,0,0);
  Bounds(Rows,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->m )
    {
      *err=1;
      return(NULLCELLS);
    }
  if ((Loc =nsp_cells_create(NVOID,Rows->mn,A->n)) == NULLCELLS )   return(NULLCELLS);
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < A->n ; j++ )
      {
	NspObject *Ob= A->objs[(((int) Rows->R[i])-1)+ j*A->m];
	if ( Ob != NULL) 
	  {
	    if (( Loc->objs[i+ j*Loc->m]= nsp_object_copy_with_name(Ob))  == NULL)  return NULLCELLS;
	  }
      }
  return(Loc);
}

/*
 * Comparison operators
 */

/* Operations 
 * FIXME: to be done with generic equal operator 
 */

static int Eq(NspObject * a, NspObject * b) {  return FALSE;}
static int NEq(NspObject * a, NspObject * b) {  return FALSE;}

typedef int (Cells_CompOp) (NspObject *,NspObject *);

/* typedef int (Cells_CompOp) (char *,char *); */

typedef struct cpt {
  char *name;
  Cells_CompOp *fonc,*foncop;
} CellsCompTab;

/* Warning : sorted tab **/ 

static CellsCompTab comptab[] = {
  {"<>",NEq ,Eq  },
  {"==",Eq  ,NEq },
  {(char *) NULL , 0,0}
}; 

static int CellsSearchComp(char *op, Cells_CompOp (**comp_op))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *comp_op = comptab[i].foncop;
	  return(OK);
	}
      else
	{ 
	  if ( j <= 0)
	    {
	      Sciprintf("\nUnknow comp operator <%s>\n",op);
	      return(FAIL);
	    }
	  else i++;
	}
    }
  Sciprintf("\n Unknow comp operator <%s>\n",op);
  return(FAIL);
}


/*
 * Operation on cells leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j) 
 * with the special case : 
 *      A(i;j)op B(0,0) or A(0,0) op B(i,j) if A or B are of size 1x1
 *      
 * A and B are unchanged : Res is created 
 */

NspBMatrix  *CellsCompOp(NspCells *A, NspCells *B, char *op)
{
  Cells_CompOp *comp_op;
  int i;
  NspBMatrix *Loc ;
  if ( CellsSearchComp(op,&comp_op) == FAIL) return(NULLBMAT);
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 && A->mn != 0  ) 
	{
	  /* Special case B is a constant, Loc created with true */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*comp_op)(A->objs[i],B->objs[0]) ) Loc->B[i] = FALSE;
	  return(Loc);
	}
      if ( A->mn == 1 && B->mn != 0) 
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  for ( i = 0 ; i < B->mn ; i++ )  
	    if ( (*comp_op)(A->objs[0],B->objs[i]) ) Loc->B[i] = FALSE;
	  return(Loc);
	}
      /* Incompatible dimensions **/
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
      else 
	{
	  Scierror("Error:\tIncompatible dimensions\n");
	  return( NULLBMAT);
	}
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  Loc =nsp_bmatrix_create(NVOID,1,1);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  if ( (*comp_op)(NULL,NULL)) Loc->B[0] = FALSE;
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*comp_op)(A->objs[i],B->objs[i])) Loc->B[i] = FALSE;
	}
    }
  return(Loc);
}

/* 
 * Deuxieme chose a faire :  la meme chose mais Globale
 *  Le resultat est un booleen qui vaut True si 
 *  A(i;i) <= B(i,j) pour tout les (i,j) 
 */ 

int CellsFullComp(NspCells *A, NspCells *B, char *op,int *err)
{
  Cells_CompOp *comp_op;
  int i, rep = TRUE;
  *err=0;
  if ( CellsSearchComp(op,&comp_op) == FAIL) return FALSE;
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 ) 
	{
	  for ( i = 0 ; i < A->mn ; i++ )  
	    {
	      if ( (*comp_op)(A->objs[i],B->objs[0]) ) 
		{ 
		  rep = FALSE;
		  break;
		}
	    }
	  return rep;
	}
      if ( A->mn == 1) 
	{
	  for ( i = 0 ; i < B->mn ; i++ )  
	    {
	      if ( (*comp_op)(A->objs[i],B->objs[0]) ) 
		{ 
		  rep = FALSE;
		  break;
		}
	    }
	  return rep ;
	}
      /* Scierror("Error:\tIncompatible dimensions\n");*/
      *err=1;
      return FALSE ;
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ )  
	{
	  if ( (*comp_op)(A->objs[i],B->objs[i]) ) 
	    {
	      rep =  FALSE;
	      break ;
	    }
	}
    }
  return rep;
}



/*
 * Res =nsp_cells_transpose(A) 
 * Transpose A 
 */

NspCells*nsp_cells_transpose(const NspCells *A)
{
  int i,j;
  NspCells *Loc;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_cells_create(NVOID,A->n,A->m)) == NULLCELLS) return(NULLCELLS);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Loc->m ; i++ )
    for ( j = 0 ; j < Loc->n ; j++ )
      {
	NspObject *Ob=A->objs[j+(A->m)*i];
	if ( Ob != NULL) 
	  {
	    if ((Loc->objs[i+(Loc->m)*j] = nsp_object_copy_with_name(Ob)) == NULL) return(NULLCELLS);
	  }
      }
  return(Loc);
}

