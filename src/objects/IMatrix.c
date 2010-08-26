/* Nsp
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
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
#include <ctype.h> 

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/matint.h> 

#include "nsp/pr-output.h" 
#include "nsp/interf.h" /* for ret_endfor */
#include <nsp/matutil.h> /* icopy iset */

static int nsp_imatrix_print_internal (nsp_num_formats *fmt,NspIMatrix *cm, int indent);

/**
 * nsp_imatrix_create:
 * @name: matrix name 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new int matrix filled with 0 values.
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
#define IMAT_SETZERO(name,type,arg) for (i= 0 ; i < Loc->mn; i++) Loc->name[i]=0;break;
  NSP_ITYPE_SWITCH(Loc->itype,IMAT_SETZERO,void);
#undef IMAT_SETZERO
  return(Loc);
}



/**
 * nsp_imatrix_create_int_impl:
 * @first: 
 * @step: 
 * @last: 
 * 
 * 
 * 
 * Returns: 
 **/

#define NSP_MAT_IMPL(name,type,arg)	{				\
    type first = First->name[0];					\
    type step  = (Step == NULL) ? 1: Step->name[0];			\
    type last = Last->name[0];						\
    int count=0;							\
    if ( (first < last  &&  step < 0) || (first > last && step > 0) || step == 0 ) \
      {									\
	Loc = nsp_imatrix_create(NVOID, 1, 0, First->itype);		\
	return Loc;							\
      }									\
    if ( step == 1 )							\
      count = last - first + 1;						\
    else if ( step == -1 )						\
      count = first - last + 1;						\
    else if ( step > 0 )						\
      count = 1 + (last-first)/step;					\
    else								\
      count = 1 + (first-last)/(-step);					\
    if ( (Loc = nsp_imatrix_create(NVOID,1,count, First->itype)) == NULLIMAT ) \
      return NULLIMAT;							\
    for ( i = 0 ; i < Loc->mn ; i++) Loc->name[i] = first+i*step;}	\
    break;

NspIMatrix *nsp_imatrix_create_int_impl(NspIMatrix *First, NspIMatrix *Step,NspIMatrix *Last)
{
  int i;
  NspIMatrix *Loc=NULL;
  NSP_ITYPE_SWITCH(First->itype,NSP_MAT_IMPL,void);
#undef NSP_MAT_IMPL
  return Loc;
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
 * nsp_imatrix_scalar_to_mn:
 * @A: a #NspIMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspIMatrix @A of dimension 1x1 is changed to a matrix of size @m x @n 
 * filled with the @A scalar value i.e A= A(1,1)*ones(m;n). 
 * Note that the size of @A is not checked on entry it sould be 1x1.
 *
 * returns: %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_imatrix_scalar_to_mn(NspIMatrix *A, int m, int n)
{
  int i;
  if ( nsp_imatrix_resize(A,m,n) == FAIL) return FAIL; 
#define IMAT_AC(name,type,arg) for ( i=1 ; i < A->mn ; i++) A->name[i]= A->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,void);
#undef IMAT_AC
  return OK;
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
  NSP_ITYPE_NAMES(names);
  char *st=NULL;
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(IMat)->name;
  st = NSP_ITYPE_NAME(names,IMat->itype);					

  if (user_pref.pr_as_read_syntax)
    {
      if (IMat->mn==0 )
	{
	  if ( strcmp(pname,NVOID) != 0) 
	    Sciprintf1(indent,"%s= imat_create(%d,%d,\"%s\");",pname,IMat->m,IMat->n,st);
	  else 
	    Sciprintf1(indent,"imat_create(%d,%d,\"%s\");",IMat->m,IMat->n, st);
	}
      else 
	{
	  if ( strcmp(pname,NVOID) != 0) 
	    Sciprintf1(indent,"%s=m2i(",pname);
	  else 
	    Sciprintf1(indent,"m2i(");
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_imatrix_info(IMat,indent,pname,rec_level);
	  return rep;
	}
      Sciprintf1(indent,"%s\t=%s\t\t i (%dx%d,%s)\n",pname,
		 (IMat->mn==0 ) ? " []" : "",IMat->m,IMat->n,st);
    }
  if ( IMat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      rep =nsp_imatrix_print_internal (&fmt,IMat,indent);
    }
  if (user_pref.pr_as_read_syntax)
    {
      Sciprintf1(indent,",\"%s\");\n",st);
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
 * returns: %OK or %FAIL.
 */


int nsp_imatrix_enlarge(NspIMatrix *A, int m, int n)
{
  if ( A->mn == 0)
    {
      int i;
      if (nsp_imatrix_resize(A,m,n) == FAIL) return FAIL;
#define IMAT_SETZERO(name,type,arg) for (i= 0 ; i < A->mn; i++) A->name[i]=0;break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_SETZERO,void);
#undef IMAT_SETZERO
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
 * returns: %OK or %FAIL.
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
 * returns: %OK or %FAIL.
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
#define IMAT_AC(name,type,arg) for ( i=0 ; i < ns ; i++) A->name[i+Asize]= 0;break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
  return(OK);
}

/**
 * nsp_imatrix_concat_down:
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix
 * 
 * returns a new #NspIMatrix equals to  [@A ; @B ] ; 
 * 
 * returns: a #NspIMatrix or %NULLMAT.
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
 * returns: a #NspIMatrix or %NULLMAT.
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
 * returns: %OK or %FAIL.
 */

int nsp_imatrix_add_rows(NspIMatrix *A, int m)
{
  int i,Am, j;
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
#define IMAT_AROWS(name,type,arg) for ( i= Am-1 ; i >=0 ; i--) A->name[i+j*(A->m)]=A->name[i+j*(Am)];break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AROWS,"");
#undef IMAT_AROWS
#define IMAT_AROWS(name,type,arg) for ( i=0 ; i < m ; i++) A->name[i+j*(A->m)+Am]=0;break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AROWS,"");
#undef IMAT_AROWS
      /* if ( j != 0) nsp_icopy(&Am,A->Gint+j*Am,&inc,A->Gint+j*(A->m),&inc); */
      /*nsp_iset(&m,&d,A->Gint+j*(A->m)+Am,&inc); */
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
 * returns: %OK or %FAIL.
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
 * returns: %OK or %FAIL.
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
 * returns: a #NspIMatrix or %NULLMAT 
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
 * returns: a #NspIMatrix or %NULLMAT 
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
 * Compute A(:,Cols) and returns: the new #NspIMatrix 
 * 
 * returns: a #NspIMatrix or %NULLMAT 
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
 * returns: a #NspIMatrix or %NULLMAT 
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
 * returns: a #NspIMatrix or %NULLMAT 
 */

NspIMatrix  *nsp_imatrix_extract_diag(NspIMatrix *A, int k)
{
  NspIMatrix *Loc;
  int j,i;
  int imin = Max(0,-k);
  int imax = Min(A->m,A->n -k );
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
#define IMAT_ED(name,type,arg)for ( i = imin ; i < imax ; i++ ) Loc->name[j++] = A->name[i+(i+k)*A->m];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_ED,"");
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
 * returns: %OK or %FAIL.
 */

int nsp_imatrix_set_diag(NspIMatrix *A, NspIMatrix *Diag, int k)
{
  int i, ind, mn = Diag->mn;
  int imin, imax, dsize, ind_start;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );  /* imax plus 1 in fact */
  dsize = imax-imin ;

  if ( dsize <= 0 || (mn != 1 && dsize != mn)  ) 
    {
      Scierror("Error:\tdiagonal number and/or vector size not compatible with given matrix\n");
      return FAIL;
    }
  ind_start = imin + (imin+k)*A->m;

  if ( Diag->eltsize > A->eltsize) 
    {
      if ( nsp_imatrix_change_itype(A,Diag->itype) == FAIL) 
	return FAIL;
    }

  if ( mn == 1 )   /* a scalar is given to fill the diagonal */
    { 
      NSP_COPY_ITYPES(for (i=0,ind=ind_start;i<dsize;i++,ind+=1+A->m),A,ind,Diag->Iv,Diag->itype,0);
    }
  else             /* a vector is given to fill the diagonal */
    {
      NSP_COPY_ITYPES(for (i=0,ind=ind_start;i<dsize;i++,ind+=1+A->m),A,ind,Diag->Iv,Diag->itype,i);
    }

  return OK;
}



/**
 * nsp_imatrix_create_diag:
 * @Diag: a #NspIMatrix
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns: a #NspIMatrix or %NULLMAT 
 */


NspIMatrix  *nsp_imatrix_create_diag(NspIMatrix *Diag, int k)
{
  int i,j;
  NspIMatrix *Loc;
  int imin = Max(0,-k);
  int imax = Diag->mn +imin;
  int nd = Diag->mn+Abs(k);
  if (( Loc =nsp_imatrix_create(NVOID,nd,nd,Diag->itype)) == NULLIMAT) 
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
 * returns: a #NspIMatrix or %NULLMAT 
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
  /* we do not use int and uint */
  if ( itype == nsp_gint ) itype = nsp_gint32;
  else if ( itype == nsp_guint ) itype = nsp_guint32;
  if (( Loc =nsp_imatrix_create(NVOID,M->m,M->n,itype)) == NULLIMAT) 
    return(NULLIMAT);
  if ( M->rc_type == 'r') 
    {
      /* to properly detect overflows in int64 we have to use long double 
       */
      if ( Loc->itype == nsp_gint64) 
	{
	  long double dmax = G_MAXINT64;
	  long double dmin = G_MININT64;

	  for (i= 0 ; i < Loc->mn; i++) 
	    {
	      Loc->Gint64[i]= ((long double) M->R[i] >= dmax) ? 
		dmax : (((long double) M->R[i] ) <= dmin  ? 
			dmin : ((gint64)  M->R[i]));
	    }
	  return Loc;
	}
      else if ( Loc->itype == nsp_guint64)
	{
	  long double dmax = G_MAXUINT64;
	  for (i= 0 ; i < Loc->mn; i++) 
	    {
	      Loc->Gint64[i]= ((long double) M->R[i] >= dmax) ? 
		dmax : ( M->R[i] <= 0  ? 0 : ((guint64)  M->R[i]));
	    }
	  return Loc;
	}
      
#define IMAT_COPY(name,type,arg)					\
      for (i= 0 ; i < Loc->mn; i++) Loc->name[i]= (type)M->R[i] ;	\
      break;
      NSP_ITYPE_SWITCH(Loc->itype,IMAT_COPY,void);
#undef IMAT_COPY
    }
  else
    {
#define IMAT_COPY(name,type,arg)					\
      for (i= 0 ; i < Loc->mn; i++) Loc->name[i]= (type)M->C[i].r;break;
      NSP_ITYPE_SWITCH(Loc->itype,IMAT_COPY,void);
#undef IMAT_COPY
    }
  return(Loc);
}

#define NSP_MAT_TO_ITYPE( name, type,tmax,tmin, tag,val )	\
  {								\
    tag dmax = tmax;						\
    tag dmin = tmin;						\
    for (i= 0 ; i < Loc->mn; i++)				\
      {								\
	Loc->name[i]= ((tag) M->val >= dmax) ?			\
	  dmax : (((tag) M->val) <= dmin  ?			\
		  dmin : ((type)  M->val));			\
      }								\
  }								\
    break;


NspIMatrix  *nsp_matrix_to_imatrix_with_bounds(NspMatrix *M, nsp_itype itype)
{
  int i;
  NspIMatrix *Loc;
  /* we do not use int and uint */
  if ( itype == nsp_gint ) itype = nsp_gint32;
  else if ( itype == nsp_guint ) itype = nsp_guint32;
  if (( Loc =nsp_imatrix_create(NVOID,M->m,M->n,itype)) == NULLIMAT) 
    return(NULLIMAT);
  if ( M->rc_type == 'r') 
    {
      switch (itype ) {				
      case nsp_gint: NSP_MAT_TO_ITYPE(Gint,gint,G_MAXINT , G_MININT,double, R[i]);		
      case nsp_guint: NSP_MAT_TO_ITYPE(Guint,guint,G_MAXUINT , 0 ,double, R[i]);	
      case nsp_gshort: NSP_MAT_TO_ITYPE(Gshort,gshort,G_MAXSHORT , G_MINSHORT,double, R[i]);	
      case nsp_gushort: NSP_MAT_TO_ITYPE(Gushort,gushort,G_MAXUSHORT , 0,double, R[i]);	
      case nsp_glong : NSP_MAT_TO_ITYPE(Glong,glong,G_MAXLONG , G_MINLONG ,double, R[i]);	
      case nsp_gulong: NSP_MAT_TO_ITYPE(Gulong,gulong,G_MAXULONG , 0,double, R[i]);	
      case nsp_gint8: NSP_MAT_TO_ITYPE(Gint8,gint8,G_MAXINT8 , G_MININT8,double, R[i]);	
      case nsp_guint8:  NSP_MAT_TO_ITYPE(Guint8,guint8,G_MAXUINT8 , 0,double, R[i]);	
      case nsp_gint16: NSP_MAT_TO_ITYPE(Gint16,gint16,G_MAXINT16 , G_MININT16,double, R[i]);	
      case nsp_guint16: NSP_MAT_TO_ITYPE(Guint16,guint16,G_MAXUINT16 , 0,double, R[i]);	
      case nsp_gint32: NSP_MAT_TO_ITYPE(Gint32,gint32,G_MAXINT32 , G_MININT32,double, R[i]);	
      case nsp_guint32: NSP_MAT_TO_ITYPE(Guint32,guint32,G_MAXUINT32 , 0,double, R[i]);	
      case nsp_gint64 : NSP_MAT_TO_ITYPE(Gint64,gint64,G_MAXINT64 , G_MININT64 ,long double, R[i]);	
      case nsp_guint64 : NSP_MAT_TO_ITYPE(Guint64,guint64,G_MAXUINT64 , 0 ,long double, R[i]);
      }
    }
  else
    {
      switch (itype ) {				
      case nsp_gint: NSP_MAT_TO_ITYPE(Gint,gint,G_MAXINT , G_MININT,double, C[i].r);		
      case nsp_guint: NSP_MAT_TO_ITYPE(Guint,guint,G_MAXUINT , 0 ,double, C[i].r);	
      case nsp_gshort: NSP_MAT_TO_ITYPE(Gshort,gshort,G_MAXSHORT , G_MINSHORT,double, C[i].r);	
      case nsp_gushort: NSP_MAT_TO_ITYPE(Gushort,gushort,G_MAXUSHORT , 0,double, C[i].r);	
      case nsp_glong : NSP_MAT_TO_ITYPE(Glong,glong,G_MAXLONG , G_MINLONG ,double, C[i].r);	
      case nsp_gulong: NSP_MAT_TO_ITYPE(Gulong,gulong,G_MAXULONG , 0,double, C[i].r);	
      case nsp_gint8: NSP_MAT_TO_ITYPE(Gint8,gint8,G_MAXINT8 , G_MININT8,double, C[i].r);	
      case nsp_guint8:  NSP_MAT_TO_ITYPE(Guint8,guint8,G_MAXUINT8 , 0,double, C[i].r);	
      case nsp_gint16: NSP_MAT_TO_ITYPE(Gint16,gint16,G_MAXINT16 , G_MININT16,double, C[i].r);	
      case nsp_guint16: NSP_MAT_TO_ITYPE(Guint16,guint16,G_MAXUINT16 , 0,double, C[i].r);	
      case nsp_gint32: NSP_MAT_TO_ITYPE(Gint32,gint32,G_MAXINT32 , G_MININT32,double, C[i].r);	
      case nsp_guint32: NSP_MAT_TO_ITYPE(Guint32,guint32,G_MAXUINT32 , 0,double, C[i].r);	
      case nsp_gint64 : NSP_MAT_TO_ITYPE(Gint64,gint64,G_MAXINT64 , G_MININT64 ,long double, C[i].r);	
      case nsp_guint64 : NSP_MAT_TO_ITYPE(Guint64,guint64,G_MAXUINT64 , 0 ,long double, C[i].r);
      }
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
#define IMAT_ITOM(name,type,arg)					\
  for (i= 0 ; i < Loc->mn; i++) Loc->R[i]=  M->name[i] ;		\
  break;
  NSP_ITYPE_SWITCH(M->itype,IMAT_ITOM,void);
#undef IMAT_ITOM
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
#define IMAT_AND(name,type,arg)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] &= B->name[i];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AND,"");
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
#define IMAT_AND(name,type,arg)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] &= B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AND,"");
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
#define IMAT_OR(name,type,arg)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] |= B->name[i];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_OR,"");
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
#define IMAT_OR(name,type,arg)for ( i = 0 ; i < A->mn ; i++ ) A->name[i] |= B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_OR,"");
#undef IMAT_OR
  return(OK);
}



/**
 * nsp_imatrix_count_true:
 * @A: a #NspIMatrix. 
 * 
 * counts the number of %TRUE in #NspIMatrix @A.
 * returns: this number in a #NspMatrix.
 **/

NspMatrix *nsp_imatrix_count_true(const NspIMatrix *A)
{
  int i;
  NspMatrix *Loc = nsp_matrix_create(NVOID,'r',(int) 1,(int) 1);
  if ( Loc == NULLMAT) return(NULLMAT);
  Loc->R[0] = 0;
#define IMAT_COUNT_TRUE(name,type,arg)for ( i = 0 ; i < A->mn ; i++ ) if (A->name[i]) Loc->R[0]++;break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_COUNT_TRUE,"");
#undef IMAT_COUNT_TRUE
  return Loc;
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
#define IMAT_FIND(name,type,arg)		\
  for ( i=0 ; i < A->mn ; i++)			\
    {if ( A->name[i] ) count++;}		\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_FIND,"");
#undef IMAT_FIND
  /* for ( i=0 ; i < A->mn ; i++) if ( A->Gint[i] ) count++; */
  /* special rule for scalars */
  if ( A-> m == 1 && count ==0) nrow =0;

  if ( lhs == 1)
    {
      *Res1 = nsp_matrix_create(NVOID,'r', nrow, count);
      if ( *Res1 == NULLMAT) return FAIL;
      count=0; ii = 0;
#define IMAT_FIND(name,type,arg)			\
      for ( i = 0 ; i < A->mn ; i++ )			\
	{						\
	  ii++;						\
	  if ( A->name[i] ) (*Res1)->R[count++] = ii;	\
	} break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_FIND,"");
#undef IMAT_FIND
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
#define IMAT_FIND(name,type,arg)			\
      for ( j = 0, k = 0 ; j < A->n ; j++ )		\
	for ( i = 0 ; i < A->m ; i++, k++ )		\
	  if ( A->Gint[k] )				\
	    {						\
	      (*Res1)->R[count] = i+1;			\
	      (*Res2)->R[count++] = j+1;		\
	    }						\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_FIND,"");
#undef IMAT_FIND
    }							
  return OK;
}



/**
 * nsp_imatrix_intmax:
 * @A: a #NspIMatrix. 
 * 
 * The max value that can be stored in @A.
 *
 * Return value: a #nsp_int_union
 **/

nsp_int_union nsp_imatrix_intmax(const NspIMatrix *A)
{
  nsp_int_union val;
  NSP_MAX_ITYPE(val,A->itype);
  return val;
}

/**
 * nsp_imatrix_intmin:
 * @A: a #NspIMatrix. 
 * 
 * The min value that can be stored in @A.
 *
 * Return value: a #nsp_int_union
 **/

nsp_int_union nsp_imatrix_intmin(const NspIMatrix *A)
{
  nsp_int_union val;
  NSP_MIN_ITYPE(val,A->itype);
  return val;
}


typedef union { 
  guint64     Guint64;
  double      Double;
} nsp_double_union ;

int nsp_hex2num(nsp_string str,double *res) 
{
  int j;
  nsp_double_union x;
  /* should only be used with less than 16 characters */
  guint64 num = 0;
  unsigned int nc = strlen(str);
  for ( j = 0; j < Min(nc,16); j++)
    {
      unsigned char ch = str[j];
      if (isxdigit (ch))
	{
	  num <<= 4;
	  num +=( ch >= 'a') ? (ch - 'a' + 10) :
	    ((ch >= 'A') ? (ch - 'A' + 10):  (ch - '0'));
	}
      else
	{
	  Scierror("hex2num: illegal character %d found in string at position %d\n",ch,j+1);
	  return FAIL;
	}
      /* always suppose that str is to 
       * right completed with 0 
       */
    }
  if (nc < 16)
    num <<= (16 - nc) * 4;
  x.Guint64 = num;
  *res = x.Double;
  return OK;
}

nsp_string nsp_num2hex(double x)
{
  int j;
  nsp_double_union xu;
  nsp_string str = new_nsp_string_n(16);
  if ( str == NULL ) return NULL;
  xu.Double = x;
  for ( j = 0; j < 16; j++)
    {
      unsigned char ch = (unsigned char) ( xu.Guint64 >> ((15 - j) * 4) & 0xF);
      if (ch >= 10)
	ch += 'a' - 10;
      else
	ch += '0';
    }
  return str;
}


/*
 * routines for output of boolean matrices 
 */

static void nsp_int_print(const void *m, int i, int j)
{
  const NspIMatrix *M=m;
  /*  nsp_gint, nsp_guint, nsp_gshort, nsp_gushort, nsp_glong , 
		 nsp_gulong, nsp_gint8, nsp_guint8, nsp_gint16,
		 nsp_guint16, nsp_gint32, nsp_guint32, nsp_gint64, 
		 nsp_guint64 } nsp_itype;
  */
  char *(fmt)[]={"%*d","%*ud", "%*d", "%*u","%*d",	
		 "%*u", "%*d", "%*u", "%*d",        
		 "%*d", "%*d", "%*u", "%*"G_GINT64_FORMAT,
		 "%*"G_GUINT64_FORMAT,NULL};
#define IMAT_PRINT(name,type,arg) Sciprintf(fmt[M->itype],5,M->name[i+(M->m)*j]);break;
  NSP_ITYPE_SWITCH(M->itype,IMAT_PRINT,"");
#undef IMAT_PRINT
}

static void BMij_plus_format(const void *m, int i, int j)
{
  const NspIMatrix *M=m;
#define IMAT_PRINT(name,type,arg) if ( M->name[i+(M->m)*j] == 0) Sciprintf(" ");\
  else Sciprintf("+");break;
  NSP_ITYPE_SWITCH(M->itype,IMAT_PRINT,"");
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

