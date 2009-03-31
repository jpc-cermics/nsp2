/* Nsp
 * Copyright (C) 2009 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/interf.h" /* for ret_endfor */
#include <nsp/matutil.h> /* icopy iset */
#include <nsp/imatrix.h>

static int nsp_imatrix_print_internal (nsp_num_formats *fmt,NspIMatrix *cm, int indent);

/**
 * nsp_imatrix_create:
 * @name: matrix name 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new boolean matrix filled with %TRUE values.
 * 
 * Return value: a #NspMatrix or %NULLMAT in case of allocation failure.
 */
  
NspIMatrix  *nsp_imatrix_create(const char *name, int m, int n,nsp_itype itype)
{
  int i;
  gssize s =  sizeof(gint);
  NspIMatrix *Loc= new_imatrix();

  if ( Loc == NULLIMAT) 
    { 
      Scierror("IMatCreate : Error no more space\n");
      return(NULLIMAT);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLIMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 

  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  Loc->itype = itype;
  NSP_ITYPE_SIZE(s,itype);
  Loc->eltsize=s;
  if ( (  Loc->Iv = MALLOC( Loc->mn*s)) == NULL )
    { 
      Scierror("IMatCreate : Error no more space\n");
      return(NULLIMAT);
    }
  /* initialiaze to zero */
  NSP_COPY_TO_ITYPE(Loc,itype,i,0,1,Loc->mn,0);
  return(Loc);
}

/**
 * nsp_imatrix_clone:
 * @name: matrix name 
 * @A: a #NspIMatrix
 * @m: number of rows 
 * @n: number of columns 
 * @init: unsused init flag.
 * 
 * Creates a new boolean matrix with with unspecified values. @A is not 
 * used for #NspIMatrix.
 * 
 * Return value: a #NspIMatrix or %NULLMAT in case of allocation failure.
 */

NspIMatrix *nsp_imatrix_clone(const char *name, NspIMatrix *A, int m, int n, int init)
{
  return nsp_imatrix_create(name, m, n, A->itype);
}


/**
 * nsp_imatrix_copy:
 * @A: a #NspIMatrix 
 *
 * copies #NspIMatrix @A and returns the copy or %NULLIMAT.
 * 
 * Return value: a #NspMatrix or %NULLIMAT.
 */

NspIMatrix  *nsp_imatrix_copy(NspIMatrix *A)
{
  NspIMatrix *Loc;
  if ( ( Loc =nsp_imatrix_create(NVOID,A->m,A->n,A->itype) ) == NULLIMAT) 
    return(NULLIMAT);
  memcpy(Loc->Iv, A->Iv,A->mn*A->eltsize);
  return(Loc);
}


/**
 * nsp_imatrix_change_itype:
 * @A: a #NspIMatrix 
 * @itype: the new type to be used
 *
 * changes the #NspIMatrix @A storage to use integer of type @itype.
 * 
 * Return value: %OK or %FAIL
 */

int nsp_imatrix_change_itype(NspIMatrix *A,nsp_itype itype)
{
  int i,itype_old=A->itype;
  void *old=A->Iv,*res;
  gssize s =  sizeof(gint);
  if ( A->itype == itype ) return OK;
  NSP_ITYPE_SIZE(s,itype);
  if (( res = MALLOC( A->mn*s)) == NULL )
    { 
      Scierror("Error: running out of space\n");
      return FAIL;
    }
  A->Iv = res;
  NSP_COPY_ITYPE_TO_ITYPE(A,0,itype,i,0,1,A->mn,old,itype_old);
  A->itype = itype;
  A->eltsize=s;
  return OK;
}


/**
 * nsp_imatrix_elt_size:
 * @M: a #NspMatrix 
 * 
 * size of matrix elements.
 * 
 * Return value: size of elements of matrix @M. 
 **/

unsigned int  nsp_imatrix_elt_size(NspMatrix *M)
{
  return ((NspIMatrix *)M)->eltsize;
}


/**
 * nsp_imatrix_resize:
 * @A: a #NspIMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspIMatrix @A dimensions are changed to be @m x @n. 
 * This routine only enlarges or shrink (using realloc()) 
 * the data array of @A to size mxn. The previous data are not moved and 
 * occupy the first array cells. Note that @A can be 
 * and empty matrix when calling this routine ( malloc() is used in that 
 * case ). 
 *
 * Return value: %OK or %FAIL. When %OK is returned @A is changed. 
 */


int nsp_imatrix_resize(NspIMatrix *A, int m, int n) 
{
  if ( A->mn == m*n ) 
    {
      A->m=m;
      A->n=n;
      return(OK);
    };
  if ( m*n < 0) return FAIL;
  if ( m*n == 0 ) 
    {
      A->m =  A->n = A->mn= 0;
      FREE(A->Iv);
      return OK;
    }
  A->m =m ;  A->n =n;   A->mn=m*n ;
  A->Iv =REALLOC(A->Iv,A->mn*A->eltsize);
  if ( A->Iv == NULL) return(FAIL);
  return(OK);
}

/**
 * nsp_imatrix_destroy:
 * @IMat: a #NspIMatrix 
 * 
 * frees the #NspMatrix @IMat. 
 */

void nsp_imatrix_destroy(NspIMatrix *IMat)
{
  if ( IMat != NULLIMAT)
    {
      FREE(IMat->Iv);
      nsp_object_destroy_name(NSP_OBJECT(IMat));
      FREE(IMat) ;
    };
}

/**
 * nsp_imatrix_info:
 * @IMat: a #NspIMatrix
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 *
 * Displays info on the #NspIMatrix @A using the default Sciprintf() function. 
 * @indent is the given indentation for printing. If the #NspIMatrix has a name 
 * then this name is displayed but it can be replaced by the value of the argument 
 * @name is such an argument is non null. 
 * 
 * Return value: %TRUE or %FALSE
 */

static void nsp_int_print(const void *m, int i, int j);

int nsp_imatrix_info(NspIMatrix *IMat, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(IMat)->name;
  NSP_ITYPE_NAMES(names);
  char *st=NULL;
  st = NSP_ITYPE_NAME(names,IMat->itype);
  if ( IMat->m >=1 &&  IMat->mn >= 2 ) 
    {
      Sciprintf1(indent,"%s\t= [...]\t\ti (%dx%d,%s)\n",pname,IMat->m,IMat->n,st);
    }
  else
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      if (IMat->mn != 0)
	{
	  Sciprintf1(indent,"%s\t= [ ",pname);
	  nsp_int_print(IMat,0,0);
	  Sciprintf("]\t\ti (%dx%d,%s)\n", IMat->m,IMat->n,st);
	}
      else
	Sciprintf1(indent,"%s\t= []\t\ti (%dx%d,%s)\n",pname,
		   IMat->m,IMat->n,st);
    }
  return TRUE;

}


/**
 * nsp_imatrix_print:
 * @IMat: a #NspIMatrix
 * @indent: an int 
 * @name: %NULL or name to be used.
 * @rec_level: deph level of the print.
 *
 * Print the #NspMBatrix @A using the default nsp output function. 
 * @indent is the given indentation for printing. If the #NspIMatrix has a name 
 * then this name is displayed but it can be replaced by the value of the argument 
 * @name is such an argument is non null. 
 * 
 * Return value: %TRUE or %FALSE
 */

int nsp_imatrix_print(NspIMatrix *IMat, int indent,const char *name, int rec_level)
{
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(IMat)->name;

  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",pname,(IMat->mn==0 ) ? " xxx([])\n" : "" );
	}
      else 
	{
	  Sciprintf1(indent,"%s",(IMat->mn==0 ) ? " xxx([])\n" : "" );
	}
    }
  else 
    {
      NSP_ITYPE_NAMES(names);
      char *st=NULL;
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_imatrix_info(IMat,indent,pname,rec_level);
	  return rep;
	}
      st = NSP_ITYPE_NAME(names,IMat->itype);					
      Sciprintf1(indent,"%s\t=%s\t\t i (%dx%d,%s)\n",pname,
		 (IMat->mn==0 ) ? " []" : "",IMat->m,IMat->n,st);
    }
  if ( IMat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      rep =nsp_imatrix_print_internal (&fmt,IMat,indent);
    }
  return rep;
}


/**
 * nsp_imatrix_latex_print:
 * @IMat: a #NspIMatrix
 * 
 * print the #NspIMatrix @A using the default Sciprintf() function and LaTeX 
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */

int nsp_imatrix_latex_print(NspIMatrix *IMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("{%s = \\left(\\begin{array}{",NSP_OBJECT(IMat)->name );
  for (i=0; i <  IMat->n;i++) Sciprintf("c");
  Sciprintf("}\n");
  for (i=0; i < IMat->m; i++)
    {
      for (j=0; j < IMat->n - 1; j++)
	{ 
	  nsp_int_print(IMat,i,j);
	  Sciprintf("\t& ");
	}
      nsp_int_print(IMat,i,IMat->n-1);
      Sciprintf("\t");
      if ( i != IMat->m -1 ) 
	Sciprintf("\\\\\n");
      else 
	Sciprintf("\n");
    }

  Sciprintf("\\end{array}\\right)}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}

/**
 * nsp_imatrix_latex_tab_print:
 * @IMat: a #NspIMatrix
 * 
 * print the #NspIMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */


int nsp_imatrix_latex_tab_print(NspIMatrix *IMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("\\begin{tabular}{|l|");
  for (i=0; i < IMat->n ;i++) Sciprintf("c|");
  Sciprintf("}\\hline\n %s &\t",NSP_OBJECT(IMat)->name);
  for (i=1; i < IMat->n ;i++) Sciprintf("$C_{%d}$\t&",i);
  Sciprintf("$C_{%d}$\\\\ \\hline\n",IMat->n);
  for (i=0; i < IMat->m; i++)
    {
      Sciprintf("$L_{%d}$\t&",i+1);
      for (j=0; j < IMat->n - 1; j++)
	{ 

	  Sciprintf("$");
	  nsp_int_print(IMat,i,j);
	  Sciprintf("$\t& ");
	}
      Sciprintf("$");
      nsp_int_print(IMat,i,IMat->n-1);
      Sciprintf("$\t\\\\ \\hline\n");
    }
  Sciprintf("\\end{tabular}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}


/**
 * nsp_imatrix_enlarge:
 * @A: a #NspIMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Changes the #NspIMatrix @A size to max(A->m,m) x max(A->n,n) adding 
 * rows and columns of zero to @A. 
 * A = [ A ,ones(A->m,Max(n-A->n,0); 
 *      ones(Max(m-A->m,0), Max(A->n,n)) ]
 * 
 * returns %OK or %FAIL.
 */


int nsp_imatrix_enlarge(NspIMatrix *A, int m, int n)
{
  if ( A->mn == 0)
    {
      int i;
      if (nsp_imatrix_resize(A,m,n) == FAIL) return FAIL;
      NSP_COPY_TO_ITYPE(A,A->itype,i,0,1,A->mn,0);
    }
  if ( n > A->n  )
    if (nsp_imatrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if (nsp_imatrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/**
 * nsp_imatrix_concat_right:
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix
 * 
 * Changes the #NspIMatrix @A to [@A , @B ] ; 
 * 
 * returns %OK or %FAIL.
 */


int nsp_imatrix_concat_right(NspIMatrix *A, NspIMatrix *B)
{
  return nsp_matint_concat_right_bis( NSP_OBJECT(A),NSP_OBJECT(B));
}


/**
 * nsp_imatrix_add_columns:
 * @A: a #NspIMatrix
 * @n: number of columns 
 * 
 * Adds @n columns to the #NspIMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns %OK or %FAIL.
 */

int nsp_imatrix_add_columns(NspIMatrix *A, int n)
{
  int i = 1,ns= (A->m)*n, Asize=A->mn;
  if (n == 0) return OK;
  else if ( n < 0) 
    {      
      Scierror("Error: Negative indice (%d) in IMatAddCols\n",n);
      return FAIL;
    }
  if (nsp_imatrix_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  /*
   * nsp_iset(&ns,&d,A->xx +Asize,&inc);
   */
#define IMAT_AC(name) for ( i=0 ; i < ns ; i++) A->name[i+Asize]= 0;break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC);
  return(OK);
}

/**
 * nsp_imatrix_concat_down:
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix
 * 
 * returns a new #NspIMatrix equals to  [@A ; @B ] ; 
 * 
 * returns a #NspIMatrix or %NULLMAT.
 */

NspIMatrix *nsp_imatrix_concat_down(NspIMatrix *A, NspIMatrix *B)
{
  return (NspIMatrix *) nsp_matint_concat_down(NSP_OBJECT(A),NSP_OBJECT(B));
}

/**
 * nsp_imatrix_concat_diag:
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix
 * 
 * returns a new #NspIMatrix equals to  [@A,0 ;0, @B ] ; 
 * 
 * returns a #NspIMatrix or %NULLMAT.
 */


NspIMatrix *nsp_imatrix_concat_diag(NspIMatrix *A, NspIMatrix *B)
{
  return (NspIMatrix *) nsp_matint_concat_diag(NSP_OBJECT(A),NSP_OBJECT(B));

}

/**
 * nsp_imatrix_add_rows:
 * @A: a #NspIMatrix
 * @m: number of rows
 * 
 * Adds @n rows to the #NspIMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns %OK or %FAIL.
 */

int nsp_imatrix_add_rows(NspIMatrix *A, int m)
{
  int d=FALSE; /* TRUE; */
  int inc = -1,Am;
  int j;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in IMatAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if (nsp_imatrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  for ( j = A->n-1  ; j >= 0 ; j-- ) 
    {
      if ( j != 0) nsp_icopy(&Am,A->Gint+j*Am,&inc,A->Gint+j*(A->m),&inc);
      nsp_iset(&m,&d,A->Gint+j*(A->m)+Am,&inc);
    }
  return(OK);
}

/**
 * nsp_imatrix_set_submatrix:
 * @A: a #NspIMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 * @B: a #NspIMatrix
 * 
 * Performe  A(Rows,Cols) = B. A is changed and enlarged if necessary and 
 * size compatibility is checked i.e B must be scalar or  
 * we must have size(B)==[size(Rows),size(Cols)]. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_imatrix_set_submatrix(NspIMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspIMatrix *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}


/** 
 * nsp_imatrix_set_rows:
 * @A: a #NspIMatrix
 * @Rows: a #NspMatrix
 * @B: a #NspIMatrix
 * 
 * Performs  A(Inds) = B. A is changed and enlarged if necessary 
 * Inds is unchanged 
 * Size Compatibility is checked 
 * Rules : A Matrix or A ==[]  
 *	A(Inds)=B 
 *	B must be row or column 
 *	if A==[] the size of the result depends on b 
 * 
 *      A row vector B must be row 
 *      A column vector B must be column 
 *      Inds must be in the range of A indices unless A is row or column or []
 *      Inds and B must have the same size or B must be scalar 
 * 
 * returns %OK or %FAIL.
 */

int nsp_imatrix_set_rows(NspIMatrix *A, NspMatrix *Rows, NspIMatrix *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
}

/**
 * nsp_imatrix_extract:
 * @A: a #NspIMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(Rows,Cols) and returns the new #NspIMatrix 
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_extract(NspIMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  return (NspIMatrix*)nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}

/**
 * nsp_imatrix_extract_elements:
 * @A: a #NspIMatrix
 * @Elts: a #NspMatrix
 *
 * Compute A(Elts) and returns the new #NspIMatrix 
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_extract_elements(NspIMatrix *A, NspMatrix *Elts)
{
  return (NspIMatrix *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}

/**
 * nsp_imatrix_extract_columns:
 * @A: a #NspIMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(:,Cols) and returns the new #NspIMatrix 
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_extract_columns(NspIMatrix *A, NspMatrix *Cols)
{
  return (NspIMatrix *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/**
 * nsp_imatrix_extract_rows:
 * @A: a #NspIMatrix
 * @Rows: a #NspMatrix
 *
 * Compute A(Rows,:) and returns the new #NspIMatrix 
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_extract_rows(NspIMatrix *A, NspMatrix *Rows)
{
  return (NspIMatrix *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
}

/*
 * A1=IMatLoopCol(A1,M,i,rep)
 * Used in for loops 
 */

NspIMatrix  *IMatLoopCol(char *str, NspIMatrix *Col, NspIMatrix *A, int icol, int *rep)
{
  register int iof;
  NspIMatrix *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return(NULLIMAT);
    }
  *rep =0;
  if ( Col == NULLIMAT) 
    Loc =nsp_imatrix_create(str,A->m,1,A->itype);
  else 
    Loc = Col;
  if ( Loc == NULLIMAT) return(NULLIMAT);
  iof = (icol-1)*A->m;
  /* */
  memcpy(Loc->Iv, ((char*) A->Iv)+ iof*A->eltsize,A->m);
  return(Loc);
}
/**
 * nsp_imatrix_extract_diag:
 * @A: a #NspIMatrix
 * @k: an integer 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_extract_diag(NspIMatrix *A, int k)
{
  NspIMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_imatrix_create(NVOID,(int) 0 , (int) 0,A->itype);
      return(Loc);
    }
  if (( Loc =nsp_imatrix_create(NVOID,imax-imin,(int)1,A->itype)) == NULLIMAT)
    return(NULLIMAT);
  j=0;
  /*
   * for ( i = imin ; i < imax ; i++ ) 
   *   Loc->gint[j++] = A->gint[i+(i+k)*A->m];
   */
#define IMAT_ED(name)for ( i = imin ; i < imax ; i++ ) Loc->name[j++] = A->name[i+(i+k)*A->m];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_ED);
#undef IMAT_ED
  return(Loc);
}

/**
 * nsp_imatrix_set_diag:
 * @A: a #NspIMatrix
 * @Diag: a #NspMatrix
 * @k: an integer 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_imatrix_set_diag(NspIMatrix *A, NspIMatrix *Diag, int k)
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
      if (nsp_imatrix_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
    }
  j=0;

  if ( Diag->eltsize > A->eltsize) 
    {
      if ( nsp_imatrix_change_itype(A,Diag->itype) == FAIL) 
	return FAIL;
    }
  /* 
   *   for ( i = imin ; i < imax ; i++ ) 
   *     A->gint[i+(i+k)*A->m] = Diag->gint[j++] ;
   */
  NSP_COPY_ITYPES(for ( i = imin ; i < imax ; i++ ),A,i+(i+k)*A->m,Diag->Iv,Diag->itype,j++);
  return(OK);
}

/**
 * nsp_imatrix_create_diag:
 * @Diag: a #NspIMatrix
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */


NspIMatrix  *nsp_imatrix_create_diag(NspIMatrix *Diag, int k)
{
  int i,j;
  int imin,imax;
  NspIMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  if (( Loc =nsp_imatrix_create(NVOID,imax,imax+k,Diag->itype)) == NULLIMAT) 
    return(NULLIMAT);
  j=0;
  /* for ( i = imin ; i < imax ; i++ ) 
   *   Loc->gint[i+(i+k)*Loc->m] = Diag->gint[j++] ;
   */
  NSP_COPY_ITYPES(for ( i = imin ; i < imax ; i++ ),Loc,i+(i+k)*Loc->m,Diag->Iv,Diag->itype,j++);
  return(Loc);
}

/**
 * nsp_imatrix_transpose: 
 * @A: a #NspIMatrix
 *
 * return the transpose of A
 * 
 * returns a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_transpose(NspIMatrix *A)
{
  int i,j;
  NspIMatrix *Loc;
  if (( Loc =nsp_imatrix_create(NVOID,A->n,A->m,A->itype)) == NULLIMAT) return(NULLIMAT);
  /* 
   * for ( i = 0  ; i < A->m ; i++) 
   * for ( j = 0 ; j < A->n ; j++) 
   *  Loc->gint[j+Loc->m*i ] = A->gint[i+A->m*j];
   */
  NSP_COPY_ITYPES(for ( i = 0  ; i < A->m ; i++)for ( j = 0 ; j < A->n ; j++) ,
		  Loc,j+Loc->m*i ,A->Iv,A->itype,i+A->m*j);
  return Loc;
}

/**
 * nsp_matrix_to_imatrix:
 * @M: a #NspMatrix
 * 
 * #NspMatrix to #NspIMatrix conversion 
 * 
 * Return value: a #NspIMatrix or %NULLIMAT.
 */

NspIMatrix  *nsp_matrix_to_imatrix(NspMatrix *M, nsp_itype itype)
{
  int i;
  NspIMatrix *Loc;
  if (( Loc =nsp_imatrix_create(NVOID,M->m,M->n,itype)) == NULLIMAT) 
    return(NULLIMAT);
  if ( M->rc_type == 'r') 
    {
      NSP_COPY_TO_ITYPE(Loc,itype,i,0,1,M->mn,M->R[i]);
    }
  else
    {
      NSP_COPY_TO_ITYPE(Loc,itype,i,0,1,M->mn,M->C[i].r);
    }
  return(Loc);
}


/**
 * nsp_imatrix_to_matrix:
 * @M: a #NspIMatrix
 * 
 * #NspIMatrix to #NspMatrix conversion 
 * Return value: a #NspMatrix or %NULLMAT.
 */

NspMatrix *nsp_imatrix_to_matrix(NspIMatrix *M)
{
  int i;
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,'r',M->m,M->n)) == NULLMAT) 
    return(NULLMAT);
  NSP_COPY_FROM_ITYPE(Loc->R[i],double,M,M->itype,i,0,1,M->mn);
  return(Loc);
}

/**
 * nsp_imatrix_and
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix
 *  
 * term to term logical and  A = A & B. If %OK is returned @A is changed. 
 * Return value: %OK or %FAIL. 
 */

int nsp_imatrix_and(NspIMatrix *A,const  NspIMatrix *B)
{
  int i;
  if ( A->mn != B->mn ) 
    {
      Scierror("Error: arguments must have the same size\n");
      return(FAIL);
    }
  if ( A->itype != B->itype) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return(FAIL);
    }
#define IMAT_AND(name)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] &= B->name[i];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AND);
#undef IMAT_AND
  return(OK);
}

/**
 * nsp_imatrix_scalar_and
 * @A: a #NspIMatrix. 
 * @B: a #NspIMatrix of size 1x1
 *  
 * Logical and operator between an integer matrix @A and a integer scalar @B. The result 
 * is stored in @A.
 * Return value: %OK.
 */

int nsp_imatrix_scalar_and(NspIMatrix *A,const  NspIMatrix *B)
{
  int i;
  if ( A->itype != B->itype) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return(FAIL);
    }
#define IMAT_AND(name)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] &= B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AND);
#undef IMAT_AND
  return(OK);
}


/**
 * nsp_imatrix_or
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix
 *  
 * term to term logical or A = A | B. If %OK is returned @A is changed. 
 * Return value: %OK or %FAIL. 
 */

int nsp_imatrix_or(NspIMatrix *A,const  NspIMatrix *B)
{
  int i;
  if ( A->mn != B->mn ) 
    {
      Scierror("Error: arguments must have the same size\n");
      return(FAIL);
    }

  if ( A->itype != B->itype) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return(FAIL);
    }
#define IMAT_OR(name)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] |= B->name[i];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_OR);
#undef IMAT_OR
  return(OK);
}


/**
 * nsp_imatrix_scalar_or
 * @A: a #NspIMatrix. 
 * @B: a #NspIMatrix of size 1x1
 *  
 * Logical or operator between a boolean matrix @A and a boolean scalar @B. The result 
 * is stored in @A.
 * Return value: %OK.
 */

int nsp_imatrix_scalar_or(NspIMatrix *A,const  NspIMatrix *B)
{
  int i;
  if ( A->itype != B->itype) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return(FAIL);
    }
#define IMAT_OR(name)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] |= B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_OR);
#undef IMAT_OR
  return(OK);
}



/**
 * nsp_imatrix_count_true:
 * @A: a #NspIMatrix. 
 * 
 * counts the number of %TRUE in #NspIMatrix @A.
 * returns this number in a #NspMatrix.
 **/

NspMatrix *nsp_imatrix_count_true(const NspIMatrix *A)
{
  int i;
  NspMatrix *Loc = nsp_matrix_create(NVOID,'r',(int) 1,(int) 1);
  if ( Loc == NULLMAT) return(NULLMAT);
  Loc->R[0] = 0;
#define IMAT_COUNT_TRUE(name)for ( i = 0 ; i < A->mn ; i++ ) if (A->name[i]) Loc->R[0]++;break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_COUNT_TRUE);
#undef IMAT_COUNT_TRUE
  return Loc;
}

/**
 * nsp_imatrix_find:
 * @A: a #NspIMatrix. 
 * 
 * returns in a #NspMatrix the indices for which the 
 * #NspIMatrix @A is non null considering @A as o one dimensional array.
 * 
 * Return value:  a new #NspMatrix or %NULLMAT
 */

NspMatrix *nsp_imatrix_find(const NspIMatrix *A)
{
  NspMatrix *Res;
  int i,count=0, nrow = ( A->mn == 0) ? 0: 1;
  /* first pass for counting **/
  
#define IMAT_FIND1(name) for ( i=0 ; i < A->mn ; i++) \
    {if ( A->name[i] ) count++;}break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_FIND1);
  Res = nsp_matrix_create(NVOID,'r', nrow, count);
  if ( Res == NULLMAT) return NULLMAT;
  count=0;
#define IMAT_FIND2(name) for ( i=0 ; i < A->mn ; i++) \
    if ( A->name[i] ) Res->R[count++] = i+1; break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_FIND2);
  return Res;
}


/**
 * nsp_imatrix_find_2:
 * @A: a #NspIMatrix. 
 * @lhs: a flag 
 * @Res1:  a #NspMatrix. 
 * @Res2: a #NspMatrix. 
 * 
 * returns in one or two #NspMatrix objects the indices for which the 
 * #NspIMatrix @A is true. if @lhs is equal to one, the @Res1 will 
 * contains the indices in @A considering that @A is a one dimensional array.
 * If @lhs is equal to two then @Res1 will contian the rows indices and @Res2 
 * will contain the columns indices.
 *
 * Return value: %OK or %FAIL. %FAIL is returned in case of malloc() failure.
 */

int nsp_imatrix_find_2(const NspIMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2)
{
  int j,i,count=0, k;
  double ii;
  int nrow = ( A->mn == 0) ? 0: 1;

  /* first pass for counting */
  for ( i=0 ; i < A->mn ; i++) if ( A->Gint[i] ) count++;
  /* special rule for scalars */
  if ( A-> m == 1 && count ==0) nrow =0;

  if ( lhs == 1)
    {
      *Res1 = nsp_matrix_create(NVOID,'r', nrow, count);
      if ( *Res1 == NULLMAT) return FAIL;
      count=0; ii = 0;
      for ( i = 0 ; i < A->mn ; i++ )
	{
	  ii++;
	  if ( A->Gint[i] ) (*Res1)->R[count++] = ii;
	}
      return OK;
    }
  else
    {
      *Res1 = nsp_matrix_create(NVOID,'r',nrow , count);
      if ( *Res1 == NULLMAT) return FAIL;
      *Res2 = nsp_matrix_create(NVOID,'r',nrow , count);
      if ( *Res2 == NULLMAT) { nsp_matrix_destroy(*Res1); return FAIL; }
      count=0;
      /* change loop order for speed (bruno) */
      for ( j = 0, k = 0 ; j < A->n ; j++ )
	for ( i = 0 ; i < A->m ; i++, k++ )
	  if ( A->Gint[k] )
	    {
	      (*Res1)->R[count] = i+1;
	      (*Res2)->R[count++] = j+1;
	    }
    }
  return OK;
}

/*
 * Comparison operators
 */

/* Operations **/

static int Eq(int a, int b) {  return(a==b);}
static int NEq(int a, int b) {  return(a!=b);}

typedef int (CompOp) (int,int);

typedef struct cpt {
  char *name;
  CompOp *fonc,*foncop;
} CompTab;

/* Warning : sorted tab **/ 

static CompTab comptab[] = {
  {"<>",NEq ,Eq},
  {"==",Eq  ,NEq},
  {(char *) NULL,0,0}
};

static int SearchBComp(char *op, CompOp (**realop))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *realop = comptab[i].foncop;
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
 * nsp_imatrix_compare:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * @op: can be "<>" or "==" 
 *
 * term to term comparison between the two matrices @A and @B. 
 * Performs A(i,j) == B(i,j) or A(i,j) <> B(i,j) according to @op value.
 * Note that, if @A or @B is a 1x1 matrix its size is promoted to the size of the 
 * other argument when applying @op operator.
 *
 * returns a #NspIMatrix. 
 */

NspBMatrix  *nsp_imatrix_compare(const NspIMatrix *A,const  NspIMatrix *B, char *op)
{
  CompOp *realop;
  int i;
  NspBMatrix *Loc ;
  if ( SearchBComp(op,&realop) == FAIL) return(NULLBMAT);
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 && A->mn != 0  ) 
	{
	  /* Special case B is a constant, Loc created with true */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->Gint[i],B->Gint[0]) ) Loc->B[i] = FALSE;
	  return(Loc);
	}
      if ( A->mn == 1 && B->mn != 0 ) 
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  for ( i = 0 ; i < B->mn ; i++ )  
	    if ( (*realop)(A->Gint[0],B->Gint[i]) ) Loc->B[i] = FALSE;
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
	  if ( (*realop)(1,1)) Loc->B[0] = FALSE;
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->Gint[i],B->Gint[i])) Loc->B[i] = FALSE;
	}
    }
  return(Loc);
}

/**
 * nsp_imatrix_full_compare
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * @op: can be "<>" or "==" 
 * @err: pointer to an integer
 * 
 * %TRUE is returned if @A(i,j) op @B(i,j) is %TRUE for all indices. 
 * Note that, if @A or @B is a 1x1 matrix its size is promoted to the size of the 
 * other argument when applying @op operator. In all the other cases %FALSE is returned. 
 * 
 * Return value: %TRUE or %FALSE.
 */ 

int nsp_imatrix_full_compare(const NspIMatrix *A,const NspIMatrix *B, char *op,int *err)
{
  CompOp *realop;
  int i,rep=TRUE;
  *err=0;
  if ( SearchBComp(op,&realop) == FAIL) return FALSE;
  /* Loc = true */ 
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 ) 
	{
	  for ( i = 0 ; i < A->mn ; i++ )  
	    {
	      if ( (*realop)(A->Gint[i],B->Gint[0]) ) 
		{ 
		  rep =  FALSE;
		  break;
		}
	    }
	  return rep ;
	}
      if ( A->mn == 1) 
	{
	  for ( i = 0 ; i < B->mn ; i++ )  
	    {
	      if ( (*realop)(A->Gint[i],B->Gint[0]) ) 
		{ 
		  rep  = FALSE;
		  break;
		}
	    }
	  return rep  ;
	}
      /*       Scierror("Error:\tIncompatible dimensions\n"); */
      *err=1;
      return FALSE; 
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ )  
	{
	  if ( (*realop)(A->Gint[i],B->Gint[i]) ) 
	    {
	      rep =  FALSE;
	      break ;
	    }
	}
    }
  return rep;
}


/*
 * routines for output of boolean matrices 
 */

static void nsp_int_print(const void *m, int i, int j)
{
  const NspIMatrix *M=m;
#define IMAT_PRINT(name) Sciprintf("%5d",M->name[i+(M->m)*j]);break;
  NSP_ITYPE_SWITCH(M->itype,IMAT_PRINT);
#undef IMAT_PRINT
}

static void BMij_plus_format(const void *m, int i, int j)
{
  const NspIMatrix *M=m;
#define IMAT_PRINT(name) if ( M->name[i+(M->m)*j] == 0) Sciprintf(" ");\
  else Sciprintf("+");break;
  NSP_ITYPE_SWITCH(M->itype,IMAT_PRINT);
#undef IMAT_PRINT
}

static void BMij_as_read(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  Sciprintf(" ");
  nsp_int_print(m,i,j);
}

static void BMij(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  Sciprintf(" ");
  nsp_int_print(m,i,j);
}

static int nsp_imatrix_print_internal (nsp_num_formats *fmt,NspIMatrix *cm, int indent)
{
  int rep = TRUE;
  int nr = cm->m;
  int nc = cm->n;
  if (fmt->plus_format && ! user_pref.pr_as_read_syntax)
    {
      nsp_matrix_plus_format(cm,nr,nc,BMij_plus_format,indent);
    }
  else
    {
      int column_width,total_width,inc  ;
      int max_width ,winrows ;
      column_width = 2;
      total_width = nc * column_width;
      sci_get_screen_size(&winrows,&max_width);
      if (user_pref.pr_as_read_syntax)	max_width -= 4;
      Sciprintf("\n");
      inc = nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  inc = max_width / column_width;
	  if (inc == 0)
	    inc++;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  nsp_gen_matrix_as_read_syntax(fmt,cm,nr,nc,inc,indent,BMij_as_read);
	}
      else
	{
	  rep = nsp_matrix_general(fmt,cm,nr,nc,inc,total_width,max_width,winrows,
				   indent,BMij);
	}
    }
  return rep;
}

