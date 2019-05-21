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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> /* tolower toupper */

#include <nsp/object.h> 
#include <nsp/bmatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/cells.h> 
#include <nsp/list.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/interf.h" /* for ret_endfor */
#include "nsp/pr-output.h" 
#include "nsp/matutil.h"
#include "nsp/gsort-p.h"

static int nsp_set_cells(int n, NspObject *s1, NspObject **s2);
static int nsp_copy_cells(int n, NspObject **s1, NspObject **s2);

/**
 * nsp_cells_create:
 * @name: name of the cell 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creation of a NspCells all the elements
 * are initiated to %NULL (This should be changed 
 * to follow matlab rules i.e store an empty matrix).
 * The array for storing the objects is of size (m*n)+1 
 * and the last element is set to %NULL
 * 
 * Return value: a new #NspCells or %NULLCELLS
 **/

NspCells *nsp_cells_create(const char *name, int m, int n)
{
  int i;
  NspCells *Loc = new_cells();
  if ( Loc == NULLCELLS) 
    { 
      Scierror("Error:\tRunning out of memory\n");
      return(NULLCELLS);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLCELLS);
  NSP_OBJECT(Loc)->ret_pos = -1 ;
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  if ( Loc->mn == 0 ) 
    {
      /* empty string Matrix */
      Loc->objs = NULL;
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


NspCells *nsp_cells_clone(const char *name, NspCells *A, int m, int n, int init)
{
  return nsp_cells_create(name, m, n);
}

/**
 * nsp_cells_create_from_table:
 * @name: name of the cell 
 * @T: a %NULL terminated  array of #NspObject
 * 
 * creates a new #NspCells object filled from the 
 * array @T.
 *
 * Return value: a new #NspCells or %NULLCELLS
 **/

NspCells*nsp_cells_create_from_table(const char *name,NspObject **T)
{
  NspCells *Loc;
  int i=0,count=0;
  while ( T[count] != NULL) count++;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_cells_create(name,count,1)) == NULLCELLS) return(NULLCELLS);
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

/**
 * nsp_cells_create_from_array:
 * @name: name of the cell 
 * @n: size of @T
 * @T: an array of #NspObject
 * 
 * creates a new #NspCells object filled from the 
 * array @T.
 *
 * Return value: a new #NspCells or %NULLCELLS
 **/

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

/**
 * nsp_cells_copy:
 * @A: a #NspCells object 
 * 
 * returns a copy of @A.
 * 
 * Return value:  a new #NspCells or %NULLCELLS
 **/

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


/**
 * nsp_cells_elt_size:
 * @A: a #NspCells object 
 * 
 * size of cells elements.
 * 
 * Return value: size of @A elements.
 **/

unsigned int  nsp_cells_elt_size(NspCells *A)
{
  return sizeof(void *);
}

/**
 * nsp_cells_resize:
 * @A: a #NspCells object 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * resize object @A to (@m,@n). This function only enlarges 
 * the storage array of the #NspCells so as to contain @m x @n
 * elements. Previous stored data occupy the first poistions of the 
 * #NspCells array. 
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_cells_resize(NspCells *A, int m, int n)
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

/**
 * nsp_cells_destroy:
 * @A: a #NspCells object 
 * 
 * deletes object @A.
 **/

void nsp_cells_destroy(NspCells *A)
{
  int i;
  if ( A == NULLCELLS) return;
  nsp_object_destroy_name(NSP_OBJECT(A));
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

/**
 * nsp_cells_info:
 * @Mat:   a #NspCells object 
 * @indent: an integer giving indentation 
 * @name: %NULL or a replacement name to be used when displaying info
 * @rec_level: the depth level of this function call
 *
 * displays infos on object @Mat. 
 * 
 **/

int nsp_cells_info(const NspCells *Mat, int indent,char *name,int rec_level)
{
  const char *pname;
  if ( Mat == NULLCELLS) 
    {
      Sciprintf("Null Pointer NspCells \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  Sciprintf1(indent,"%s\t= {%s}\t\tcells (%dx%d)\n",
	     (pname==NULL) ? "" : pname,
	     (Mat->mn==0 ) ? "" : "...",Mat->m,Mat->n);
  return TRUE;
}

/**
 * nsp_cells_print:
 * @Mat:   a #NspCells object 
 * @indent: an integer giving indentation 
 * @name: %NULL or a replacement name to be used when displaying info
 * @rec_level: the depth level of this function call
 * 
 * print object @Mat. XXXXX: the as read version is to be done 
 * 
 **/

int nsp_cells_print(const NspCells *Mat, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  int i;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",NSP_OBJECT(Mat)->name,(Mat->mn==0 ) ? " {}\n" : "" );
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  Sciprintf1(indent,"%s\t= {%s}\t\tcells (%dx%d)\n",pname,
		     (Mat->mn==0 ) ? "" : "...",Mat->m,Mat->n);
	  return TRUE;
	}
      Sciprintf1(indent,"%s\t=%s\t\tcells (%dx%d)\n",pname,
		(Mat->mn==0 ) ? " {}" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      int colors[]={ 34,32,31,35,36};
      char epname[128];
      int j;
      for ( i=0 ; i < indent+1 ; i++) Sciprintf(" ");
      Sciprintf("{\n");
      for ( j = 0 ; j < Mat->n; j++ ) 
	for ( i = 0 ; i < Mat->m; i++ ) 
	  {
	    NspObject *object = Mat->objs[i+Mat->m*j];
	    if ( object != NULL ) 
	      {
		if ( rec_level >= 0 && rec_level <= 4) 
		  {
		    int col=colors[rec_level];
		    sprintf(epname,"\033[%dm(%d,%d)\033[0m",col,i+1,j+1);
		  }
		else 
		  {
		    sprintf(epname,"(%d,%d)",i+1,j+1);
		  }
		if ( object->type->pr(object,indent+2,
				      (user_pref.pr_as_read_syntax) ? "" : epname,
				      rec_level+1)==FALSE) 
		  return FALSE;
	      }
	  }
      for ( i=0 ; i < indent+1 ; i++) Sciprintf(" ");
      Sciprintf("}\n");
    }
  return TRUE;
}

int nsp_cells_latex_print(const NspCells *Mat, int use_math,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}");
  Sciprintf("\\begin{array}{ll}\n");
  if ( Mat->mn != 0) 
    {
      int i,j;
      for ( j = 0 ; j < Mat->n; j++ ) 
	for ( i = 0 ; i < Mat->m; i++ ) 
	  {
	    NspObject *object = Mat->objs[i+Mat->m*j];
	    if ( object != NULL ) 
	      {
		if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
		if ( name != NULL || strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0)
		  Sciprintf("{\\verb|%s|}_{%d,%d} & ",pname, i+1,j+1);
		else
		  Sciprintf("{\\verb|e|}_{%d,%d} & ", i+1,j+1);
		nsp_object_latex(object,FALSE ,"",1);
		if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
		if ( ! (i== Mat->m-1 && j == Mat->n -1))
		  Sciprintf("\\\\[0.3cm]\n");
	      }
	  }
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
    }
  Sciprintf1(1,"\\end{array}\n");
  if ( use_math ) Sciprintf("\\end{equation*}\n");
  return TRUE;
}

/**
 * nsp_cells_enlarge:
 * @A: a #NspCells object 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * changes the dimensions of @A to (max(A->m,@m)xmax(A->n,@n)) 
 * i.e adds @n- @A->n columns and  @m - @A->m rows to object @A.
 * 
 * Return value: %OK or %FAIL 
 **/

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


/**
 * nsp_cells_concat_right:
 * @A: a #NspCells object 
 * @B: a #NspCells object 
 * 
 * right concatenation @A is changed to {@A,@B}.
 * 
 * Return value: %OK or %FAIL 
 **/

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

/**
 * nsp_copy_cells:
 * @n:   an integer 
 * @s1:  an array of #NspObject
 * @s2:  an array of #NspObject
 * 
 * fills array @s2 with copies of objects from array @s1. 
 * Both arrays are of size @n. Previous entries of array 
 * @s2 are freed.
 * 
 * Return value:  %OK or %FAIL 
 **/

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


/**
 * nsp_set_cells:
 * @n:   an integer 
 * @s1:  a #NspObject
 * @s2:  an array of #NspObject
 * 
 * fills array @s2 with copies of @s1 object. 
 * Array @s2 is of size @n. Previous entries of array 
 * @s2 are freed.
 * 
 * Return value:  %OK or %FAIL 
 **/

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



/**
 * nsp_cells_add_columns:
 * @A: a #NspCells object 
 * @n: number of columns to add 
 * 
 * add @n empty columns to NspCells @A 
 * 
 * Return value: %OK or %FAIL 
 **/

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

/**
 * nsp_cells_concat_down:
 * @A: a #NspCells object 
 * @B: a #NspCells object 
 * 
 * Return value: %NULLCELLS on failre 
 **/

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

/**
 * nsp_cells_add_rows:
 * @A: a #NspCells object 
 * @m: number of rows to be added 
 * 
 * Add @m rows to the #NspCells object @A 
 * 
 * Return value: %OK or %FAIL 
 **/

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


/**
 * nsp_cells_set_submatrix:
 * @A: a #NspCells object 
 * @Rows: a #NspMatrix object 
 * @Cols: a #NspMatrix object 
 * @B:  a #NspCells object 
 * 
 * performs  A(Rows,Cols) = B. @A is changed and enlarged if necessary 
 * 
 * Return value: %Ok or %FAIL.
 **/

int nsp_cells_set_submatrix(NspCells *A,const NspMatrix *Rows,const NspMatrix *Cols,
				     const NspCells *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}

/**
 * nsp_cells_set_element:
 * @A:  a #NspCells object 
 * @index: an integer 
 * @B:  a #NspObject 
 * 
 * stores object @B in cell array @A at poisition @index.
 * Note that this function is for mexlib emulation. 
 * @B is not copied and modified i.e its name is changed 
 * note also that the old value replaced by @B is not destroyed.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_cells_set_element(NspCells *A,int index, NspObject *B)
{
  if ( index < 0 || index > A->mn ) 
    {
      Scierror("Error:\tindex %d is out of range\n",index);
      return FAIL;
    }
  if (nsp_object_set_name(B,"ce") == FAIL) return  FAIL;
  /* nsp_object_destroy(&A->objs[index]);*/
  A->objs[index]=B;
  return(OK);
}


/**
 * nsp_cells_set_rows:
 * @A:  a #NspCells object 
 * @Rows:  a #NspMatrix object 
 * @B:  a #NspCells object 
 * 
 * performs A(Rows) = B
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_cells_set_rows(NspCells *A, NspMatrix *Rows, NspCells *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
}

/**
 * nsp_cells_extract:
 * @A:  a #NspCells object 
 * @Rows:  a #NspMatrix object 
 * @Cols:  a #NspMatrix object 
 * 
 * returns A(Rows,Cols)
 * 
 * Return value: a new #NspCells or %NULLCELLS.
 **/

NspCells*nsp_cells_extract(NspCells *A, NspMatrix *Rows, NspMatrix *Cols)
{
  return (NspCells*)nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}


/**
 * nsp_cells_extract_elements:
 * @A:  a #NspCells object 
 * @Elts:  a #NspMatrix object 
 * @err: an in pointer 
 * 
 * returns A(Elts). If @Elts are out of bounds @err is set to 1.
 * 
 * Return value:  a new #NspCells or %NULLCELLS.
 **/

NspCells*nsp_cells_extract_elements(NspCells *A, NspMatrix *Elts, int *err)
{
  *err=0;
  return (NspCells *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}

/**
 * nsp_cells_extract_columns:
 * @A:  a #NspCells object 
 * @Cols:  a #NspMatrix object 
 * @err: an in pointer 
 * 
 * returns A(:,Cols). If @Cols are out of bounds @err is set to 1.
 * 
 * Return value:  a new #NspCells or %NULLCELLS.
 **/

NspCells*nsp_cells_extract_columns(NspCells *A, NspMatrix *Cols, int *err)
{
  *err=0;
  return (NspCells *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/**
 * CellsLoopCol:
 * @str: 
 * @Col: 
 * @A: 
 * @icol: 
 * @rep: 
 * 
 * Used in for loops 
 * 
 * Return value: 
 **/

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

/**
 * nsp_cells_extract_diag:
 * @A: a #NspCells
 * @k: an integer 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * Returns: #NspCells or %NULLCELLS 
 */

NspCells  *nsp_cells_extract_diag(NspCells *A, int k)
{
  NspCells *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_cells_create(NVOID,0,0);
      return(Loc);
    }
  if (( Loc =nsp_cells_create(NVOID,imax-imin,1)) == NULLCELLS)
    return(NULLCELLS);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      if ( A->objs[i+(i+k)*A->m] != NULLOBJ) 
	{
	  if ((Loc->objs[j++] =nsp_object_copy_with_name(A->objs[i+(i+k)*A->m])) == NULLOBJ) return NULLCELLS;
	}
      else 
	{
	  Loc->objs[j++] = NULLOBJ;
	}
    }
  return(Loc);
}

/**
 * nsp_cells_set_diag:
 * @A: a #NspCells
 * @Diag: a #NspCells
 * @k: an integer 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns: %OK or %FAIL.
 */

int nsp_cells_set_diag(NspCells *A, NspCells *Diag, int k)
{
  int i,j;
  int imin,imax,isize;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  isize = imax-imin ;
  if ( isize > Diag->mn ) 
    {
      Scierror("Error:\tGiven vector is too small\n");
      return(FAIL);
    }
  if ( isize < Diag->mn ) 
    {
      imax = Diag->mn +imin;
      if (nsp_cells_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
    }
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      if (Diag->objs[j] != NULLOBJ) 
	{
	  if ((A->objs[i+(i+k)*A->m] =nsp_object_copy_with_name(Diag->objs[j++])) == NULLOBJ)
	    return FAIL;
	}
      else 
	{
	  A->objs[i+(i+k)*A->m] = NULLOBJ;
	}
    }
  return OK;
}

/**
 * nsp_cells_create_diag:
 * @Diag: a #NspCells
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns: a #NspCells or %NULLCELLS 
 */

NspCells  *nsp_cells_create_diag(NspCells *Diag, int k)
{
  int i,j;
  int imin,imax;
  NspCells *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  if (( Loc =nsp_cells_create(NVOID,imax,imax+k)) == NULLCELLS) 
    return(NULLCELLS);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      if (Diag->objs[j] != NULLOBJ) 
	{
	  if ((Loc->objs[i+(i+k)*Loc->m] =nsp_object_copy_with_name(Diag->objs[j++])) == NULLOBJ)
	    return NULLCELLS;
	}
      else 
	{
	  Loc->objs[i+(i+k)*Loc->m]  = NULLOBJ;
	}
    }
  return(Loc);
} 


/**
 * nsp_cells_extract_rows:
 * @A:  a #NspCells object 
 * @Rows:  a #NspMatrix object 
 * @err: an in pointer 
 * 
 * returns A(Rows,:). If @Rows are out of bounds @err is set to 1.
 * 
 * Return value:  a new #NspCells or %NULLCELLS.
 **/

NspCells*nsp_cells_extract_rows(NspCells *A, NspMatrix *Rows, int *err)
{
  *err=0;
  return (NspCells *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
}

/*
 * Comparison operators
 */

/* Operations 
 * FIXME: to be done with generic equal operator 
 */

static int Eq(NspObject * a, NspObject * b) 
{
  if ( a != NULLOBJ  &&  b != NULLOBJ )
    return ( a->type->eq != NULL) ?  a->type->eq(a,b) : FALSE ;
  else 
    return a == b;
}

static int NEq(NspObject * a, NspObject * b)
{
  if ( a != NULLOBJ  &&  b != NULLOBJ )
    return ( a->type->neq != NULL) ?  a->type->neq(a,b) : TRUE ;
  else
    return a != b;
}


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

/**
 * CellsCompOp:
 * @A:   a #NspCells object 
 * @B:  a #NspCells object 
 * @op: a string coding a comparison operator
 * 
 * returns a boolean matrix filled with @A(i,j) op @B(i;j). 
 * @A and @B must have compatible dimensions with the usual 
 * scalar (1x1 matrices) promotion. In case of incompatible 
 * 
 * Return value: a new #NspBMatrix or NULLBMAT in case of 
 * memory failure or incompatible dimensions.
 **/

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

/**
 * CellsFullComp:
 * @A:   a #NspCells object 
 * @B:  a #NspCells object 
 * @op: a string coding a comparison operator
 * @err: an integer 
 * 
 * returns %TRUE if @A(i,i) or @B(i,j) is true for all 
 * cell array elements or %FALSE if not. The #NspCells object 
 * must have compatible dimensions (scalar #NspCells are promoted to matrices).
 * @err is set to 1 if the dimensions do not fit. 
 * 
 * FIXME: the scalar promotion should be removed here since 
 *        CellsFullComp is used to test that two objects are equals
 *        as in A.equals[B] and two cells with 
 * Return value: %TRUE or %FALSE 
 **/

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


/**
 * nsp_cells_transpose:
 * @A: a #NspCells object 
 * 
 * returns a new cells filled with A'.
 * 
 * Return value: a #NspCells or %NULLCELLS
 **/

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


/**
 *nsp_cells_unique:
 * @C: a NspCells
 * @Ind:
 * @Occ:
 * 
 *  build a new #NspCells (CC) gathering unique elements from @C.
 *  If @Ind is non %NULL then a Matrix is allocated and returned in @Ind. 
 *  @Ind->R[k] gives an index i such that @C(i) is returned in the result at position k (CC(k)).
 *  if @Ind is non %NULL then @Occ may be non %NULL too (else @Occ should be %NULL) and 
 *  then a Matrix is allocated and returned in @Occ. @Occ->R[k] is filled with the number 
 *  of occurences in cell @C of the value stored in the result at position @k (CC(k)).
 *
 * Return value: a new #NspCells or %NULLCELLS.
 **/

NspCells *nsp_cells_unique(NspCells *C, NspObject **Ind, NspMatrix **Occ, char ind_type)
{
  NspCells *CC;
  int i, j, k, *index;
  Boolean found;
  NspMatrix *occ=NULLMAT;
  NspObject *Current, *O=NULLOBJ;

  if ( (CC = nsp_cells_create(NVOID, C->mn, 1)) == NULLCELLS ) return NULLCELLS;

  if (Ind != NULL )
    {
      if ( (*Ind = nsp_alloc_mat_or_imat(C->mn, 1, ind_type, &index)) == NULLOBJ ) goto err;
      if (Occ != NULL )
	if ( (occ = nsp_matrix_create(NVOID,'r', C->mn, 1)) == NULLMAT ) goto err;
    }

  k = 0;
  for ( i = 0 ; i < C->mn ; i++ )
    {
      Current =  C->objs[i];
      if ( Current != NULLOBJ )
	{
	  found = FALSE;
	  for ( j = 0 ; j < k && !found ; j++ )
	    if ( Current->type->eq(Current , CC->objs[j]) )
	      {
		found = TRUE;
		if ( Occ != NULL ) occ->R[j]++;
	      }
	  if ( !found )
	    {
	      if ( (O = nsp_object_copy_with_name(Current)) == NULLOBJ ) goto err;
	      CC->objs[k] = O;
	      if ( Ind != NULL )
		{
		  index[k] = i+1;
		  if ( Occ != NULL ) occ->R[k] = 1;
		}
	      k++;
	    }
	}
    }

  if ( k < C->mn )
    {
      nsp_cells_resize(CC, k, 1);
      if ( Ind != NULL )
	{
	  if ( ind_type == 'd' )
	    nsp_matrix_resize((NspMatrix *) *Ind, k, 1);
	  else
	    nsp_imatrix_resize((NspIMatrix *) *Ind, k, 1);
	  if ( ind_type == 'd' )
	    *Ind = (NspObject *) Mat2double((NspMatrix *) *Ind);
	  if ( Occ != NULL ) 
	      nsp_matrix_resize(occ,k,1);
	}
    }

  if ( C->m == 1 && k > 1 )  /* outputs in row form */
    {
      CC->m = 1; CC->n = k;
      if ( Ind != NULL )
	{
	  ((NspMatrix *)*Ind)->m = 1; ((NspMatrix *)*Ind)->n = k;
	  if ( Occ != NULL ) 
	    {
	      occ->m = 1; occ->n = k;
	    }
	}
    }

  if ( Occ != NULL )
    *Occ = occ;

  return CC;

 err:
  nsp_cells_destroy(CC);
  if ( Ind != NULL )
    nsp_object_destroy(Ind);
  nsp_matrix_destroy(occ);
  return NULLCELLS;
}


/**
 * nsp_cells_has:
 * @C   : a #NspCells
 * @Obj : a #NspObject
 * @ind : an integer 
 *
 *  tests if the cells array C contains the object Obj
 *
 * Return value: %TRUE or %FALSE
 **/

Boolean nsp_cells_has(NspCells *C, NspObject *Obj, int *ind)
{
  int i = 0;
  Boolean found = FALSE;
  NspObject *Current;

  while ( !found  &&  i < C->mn )
    {
      Current = C->objs[i];
      if ( Current != NULLOBJ  &&  Current->type->eq(Current, Obj) )
	found = TRUE;
      i++;
    }

  *ind = found ? i : 0;

  return found;
}


/**
 *nsp_cells_map:
 * @C: a #NspCells
 * @PL: a #NspPList
 * @args: a #NspList 
 * 
 * maps function @PL to each element of cell @L passing extra 
 * arguments to the function through @args.
 * 
 * Return value: the new #NspCells obtained after mapping or %NULLCELLS
 **/

NspCells *nsp_cells_map(NspCells *C, NspPList *PL, NspList *args)  
{
  NspCells *Res = nsp_cells_create(NVOID, C->m,C->n);
  NspObject *O[2];
  int first = -1,i;
  O[1]=NULLOBJ;
  for ( i = 0 ; i < C->mn ; i++ )
    {
      O[0] = C->objs[i];
      if ( O[0] != NULLOBJ ) 
	{
	  /* the object is copied without name,it will be freed by 
	   * nsp_eval_macro_code
	   */
	  if ((O[0] =nsp_object_copy(O[0]))== NULLOBJ) return NULLCELLS;
	}
      else 
	{
	  if ((O[0]=(NspObject *) nsp_matrix_create(NVOID,'r',0,0))== NULLOBJ) return NULLCELLS;
	}
      /* stack position to use is computed on the first call and set 
       * for next calls in first 
       */
      if ((O[0] =nsp_eval_macro_code(PL,O,args,&first))== NULLOBJ) return NULLCELLS;
      if ( nsp_object_set_name(O[0],"ce") == FAIL ) return NULLCELLS;
      Res->objs[i]= O[0];
    }
  return Res;
} 

/* partition a matrix into submatrices stored in a cell */

NspCells *nsp_cells_m2ce(NspMatrix *M, NspMatrix *Rows, NspMatrix *Cols)
{
  int i,j, first,last;
  NspMatrix *R=NULL,*C=NULL;
  NspCells *Res = nsp_cells_create(NVOID, Max(Rows->mn-1,0),Max(Cols->mn-1,0));
  if ( Res == NULL) return NULL;
  if ( (R = nsp_matrix_create_int_impl(1,1,2)) == NULL)    return NULL;
  if ( (C = nsp_matrix_create_int_impl(1,1,2)) == NULL)    return NULL;
  for ( i = 0 ; i < Res->m ; i++ )
    for ( j = 0 ; j < Res->n ; j++ )
      {
	/* we need to extract M(Rows->R[i]:Rows->R[i+1]-1,Cols->R[j]:Rows->R[j+1]-1) */
	NspObject *Elt;
	/* create implicit matrices with just first, step, last */
	last = Rows->R[i+1]-1;
	first= Rows->R[i];
	R->n=R->mn=last - first + 1;
	R->impl[0]=first;
	last = Cols->R[j+1]-1;
	first= Cols->R[j];
	C->n=C->mn=last - first + 1;
	C->impl[0]=first;
	Elt = nsp_matint_extract1(NSP_OBJECT(M),NSP_OBJECT(R),NSP_OBJECT(C));
	if ( Elt == NULL) goto end;
	if (nsp_object_set_name(Elt,"ce") == FAIL) goto end;
	Res->objs[i+Res->m*j] = Elt;
      }
  nsp_matrix_destroy(C);
  nsp_matrix_destroy(R);
  return Res;
 end:
  return NULL;
} 

/* partition a matrix into 1x1 submatrices stored in a cell */

NspCells *nsp_cells_m2ce_full(NspMatrix *M)
{
  int i,j;
  NspMatrix *R=NULL,*C=NULL;
  NspCells *Res = nsp_cells_create(NVOID, M->m, M->n);
  if ( Res == NULL) return NULL;
  for ( i = 0 ; i < Res->m ; i++ )
    for ( j = 0 ; j < Res->n ; j++ )
      {
	NspMatrix *Elt;
	if (( Elt = nsp_matrix_create("ce",M->rc_type,1,1))== NULL) goto err;
	if ( M->rc_type == 'r' )
	  {
	    Elt->R[0]= M->R[i+M->m*j];
	  }
	else
	  {
	    Elt->C[0]= M->C[i+M->m*j];
	  }
	Res->objs[i+Res->m*j] = (NspObject *) Elt;
      }
  nsp_matrix_destroy(C);
  nsp_matrix_destroy(R);
  return Res;
 err:
  if ( Res != NULL) nsp_cells_destroy(Res);
  return NULL;
} 



