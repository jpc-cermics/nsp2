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

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/pmatrix.h>
#include <nsp/rmatrix.h> 
#include <nsp/cells.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/pr-output.h"
#include "nsp/cnumeric.h"
#include "nsp/nsp_lapack.h"
#include "../libsignal/signal.h"

#if 1 
#define nsp_rational_mult nsp_rational_mult_std
#else 
#define nsp_rational_mult nsp_rational_mult_fft 
#endif 

#define POLY_EXP /* use utf8 exponents */ 

extern int pr_poly_latex (nsp_num_formats *fmt,const char *vname,NspMatrix *m, int fw, int length, int do_print);
static int nsp_rmatrix_print_internal (nsp_num_formats *fmt,NspRMatrix *M, int indent);
static int nsp_pcopy_rational(int n, nsp_rational *s1, nsp_rational *s2);
static void Mp_set_format(nsp_num_formats *fmt,NspRMatrix *M);

/**
 * nsp_rational_copy:
 * @P: a nsp_rational pointer 
 * 
 * returns a rational copy 
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_copy(nsp_rational P)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N=nsp_matrix_copy(P->num))== NULL) return NULL;
  if ((D=nsp_matrix_copy(P->den))== NULL) return NULL;
  rat->num = N; rat->den=D;
  return rat;
}

/**
 *nsp_rational_copy_with_name:
 * @P: a nsp_rational pointer 
 * 
 * returns a rational copy of @P with the same name.
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_copy_with_name(nsp_rational P)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N=(NspMatrix *) nsp_object_copy_with_name((NspObject*)P->num))== NULL) return NULL;
  if ((D=(NspMatrix *) nsp_object_copy_with_name((NspObject*)P->den))== NULL) return NULL;
  rat->num = N; rat->den=D;
  return rat;
}

/**
 *nsp_rational_copy_and_name:
 * @name: string for the name to give to the copy 
 * @P: a nsp_rational pointer 
 *
 * returns a copy of rational @P with name given by @name.
 * 
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_copy_and_name(const char *name, nsp_rational P)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N=(NspMatrix *) nsp_object_copy_and_name(name,(NspObject*)P->num))== NULL) return NULL;
  if ((D=(NspMatrix *) nsp_object_copy_and_name(name,(NspObject*)P->den))== NULL) return NULL;
  rat->num = N; rat->den=D;
  return rat;
}

/**
 * nsp_basic_to_rational:
 * @d: a double or doubleC pointer 
 * @type: a character which gives the type of @d
 * 
 * returns a rational of 0 degree with coefficient set to@d.
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_basic_to_rational(const doubleC *d, char type)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N= nsp_matrix_create("pe",type,(int)1,(int)1))==NULLMAT)
    return((nsp_rational ) 0);
  if ( type == 'r') 
    {
      N->R[0] = d->r;
    }
  else 
    {
      N->C[0].r = d->r ;
      N->C[0].i = d->i ;
    }
  if ((D= nsp_matrix_create("pe",type,(int)1,(int)1))==NULLMAT)
    return((nsp_rational ) 0);
  if ( type == 'r') 
    {
      D->R[0] = 1;
    }
  else 
    {
      D->C[0].r =1;
      D->C[0].i =0;
    }
  rat->num = N; rat->den=D;
  return rat;
}

/**
 * nsp_polynom_to_rational:
 * @d: a double or doubleC pointer 
 * @type: a character which gives the type of @d
 * 
 * returns a rational of 0 degree with coefficient set to@d.
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_polynom_to_rational(NspMatrix *P)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N= (NspMatrix *) nsp_object_copy_and_name("pe",(NspObject *)P)) == NULL) return NULL;
  if ((D= nsp_matrix_create("pe",N->rc_type,(int)1,(int)1))==NULLMAT)
    return((nsp_rational ) 0);
  if ( N->rc_type  == 'r') 
    {
      D->R[0] = 1;
    }
  else 
    {
      D->C[0].r =1;
      D->C[0].i =0;
    }
  rat->num = N; rat->den=D;
  return rat;
}

/**
 * nsp_polynoms_to_rational:
 * @d: a double or doubleC pointer 
 * @type: a character which gives the type of @d
 * 
 * returns a rational of 0 degree with coefficient set to@d.
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_polynoms_to_rational(NspMatrix *P,NspMatrix *Q,int simp)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N= (NspMatrix *) nsp_object_copy_and_name("pe",(NspObject *)P)) == NULL) return NULL;
  if ((D= (NspMatrix *) nsp_object_copy_and_name("pe",(NspObject *)Q)) == NULL) return NULL;
  if ( simp ) nsp_polynoms_simp(N,D);
  rat->num = N; rat->den=D;
  return rat;
}

/**
 * nsp_basic_to_rational:
 * @d: a double or doubleC pointer 
 * @type: a character which gives the type of @d
 * 
 * returns a rational of 0 degree with coefficient set to@d.
 * 
 * Return value: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_basics_to_rational(const doubleC *n,const doubleC *d, char type)
{
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N= nsp_matrix_create("pe",type,(int)1,(int)1))==NULLMAT)
    return((nsp_rational ) 0);
  if ( type == 'r') 
    {
      N->R[0] = n->r;
    }
  else 
    {
      N->C[0].r = n->r ;
      N->C[0].i = n->i ;
    }
  if ((D= nsp_matrix_create("pe",type,(int)1,(int)1))==NULLMAT)
    return((nsp_rational ) 0);
  if ( type == 'r') 
    {
      D->R[0] = d->r;
    }
  else 
    {
      D->C[0].r = d->r ;
      D->C[0].i = d->i ;
    }
  rat->num = N; rat->den=D;
  return rat;
}

/**
 * nsp_rational_destroy:
 * @P: a nsp_rational pointer 
 * 
 * delete a rational. *P null is accepted.
 * 
 **/

void nsp_rational_destroy(nsp_rational *P)
{
  if ( *P == NULL ) return ; 
  nsp_matrix_destroy((*P)->num);
  nsp_matrix_destroy((*P)->den);
  free(*P);
  *P=NULL;
}

/**
 * nsp_matrix_to_rational:
 * @M: a #NspMatrix 
 * 
 * returns a 1x1 rmatrix. The coefficient 
 * of the rational being given by @M.
 * 
 * Returns:  a new #NspPMatrix or %NULLPMAT
 **/

NspRMatrix *nsp_matrix_to_rational(NspMatrix *M)
{
  NspRMatrix *loc;
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N = nsp_polynom_copy_and_name("pe",M))== NULLPOLY ) return NULL;
  if ((D= nsp_matrix_create("pe",M->rc_type,(int)1,(int)1))==NULLMAT)
    return NULL;
  if ( D->rc_type == 'r') 
    {
      D->R[0] = 1;
    }
  else 
    {
      D->C[0].r =1;
      D->C[0].i =0;
    }
  if ((loc =nsp_rmatrix_create(NVOID,1,1,NULL,-1,NULL,'u',1))== NULLRMAT) return(NULLRMAT);
  rat->num = N; rat->den=D;
  loc->S[0] = rat;
  return loc;
}

/**
 * nsp_matrices_to_rational:
 * @M: a #NspMatrix 
 * @N: a #NspMatrix 
 * 
 * returns a 1x1 rmatrix. The coefficient 
 * of the rational being given by @M and @N.
 * 
 * Returns:  a new #NspPMatrix or %NULLPMAT
 **/

NspRMatrix *nsp_matrices_to_rational(NspMatrix *A,NspMatrix *B)
{
  NspRMatrix *loc;
  NspMatrix *N,*D;
  nsp_rational rat;
  if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) return NULL;
  if ((N = nsp_polynom_copy_and_name("pe",A))== NULLPOLY ) return NULL;
  if ((D = nsp_polynom_copy_and_name("pe",B))== NULLPOLY ) return NULL;
  if ((loc =nsp_rmatrix_create(NVOID,1,1,NULL,-1,NULL,'u',1))== NULLRMAT) return(NULLRMAT);
  rat->num = N; rat->den=D;
  loc->S[0] = rat;
  return loc;
}

int nsp_rmatrix_same_varname(const NspRMatrix *P1,const NspRMatrix *P2)
{
  const char *name1 = ( P1->var == NULL) ? "x": P1->var;
  const char *name2 = ( P2->var == NULL) ? "x": P2->var;
  return (strcmp(name1,name2) == 0) ? TRUE: FALSE;
}

/*
 * PMatInfo : display Info on NspRMatrix PMat 
 */

int nsp_rmatrix_info(NspRMatrix *Mat, int indent,const char *name,int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  Sciprintf1(indent,"%s\t= [%s]\t\tr %c (%dx%d)\n",pname,(Mat->mn == 0) ? "": "...",
	     Mat->rc_type,Mat->m,Mat->n);
  return TRUE;
}

/*
 * PMatPrint : writes PMat Objet 
 */

int nsp_rmatrix_print(NspRMatrix *Mat, int indent,const char *name, int rec_level)
{
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",pname,(Mat->mn==0 ) ? " m2r([])\n" : "" );
	}
      else 
	{
	  Sciprintf1(indent,"%s",(Mat->mn==0 ) ? " m2r([])\n" : "" );
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_rmatrix_info(Mat,indent,pname,rec_level);
	  return rep;
	}
      Sciprintf1(indent,"%s\t=%s\t\tr (%dx%d)\n",pname,
		 (Mat->mn==0 ) ? " []" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      rep = nsp_rmatrix_print_internal (&fmt,Mat,indent);
    }
  return rep;
}

/**
 * nsp_rmatrix_latex_print:
 * @Mat: a #NspRMatrix
 * 
 * print the #NspRMatrix @A using the default Sciprintf() function and LaTeX 
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */

int nsp_rmatrix_latex_print(NspRMatrix *Mat, int use_math,const char *name, int rec_level)
{
  int i,j, fw;
  nsp_num_formats fmt;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  nsp_init_pr_format (&fmt);
  Mp_set_format (&fmt,Mat);
  fw = fmt.curr_real_fw;

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}");
  if ( name != NULL || strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
    Sciprintf("\\verb|%s| = \\begin{pmatrix}", pname);
  else 
    Sciprintf("\\begin{pmatrix}");
  for (i=0; i < Mat->m; i++)
    {
      for (j=0; j < Mat->n - 1; j++)
	{
	  Sciprintf("\\dfrac{");
	  pr_poly_latex(&fmt,Mat->var,Mat->S[j*Mat->m]->num,fw,0,TRUE );
	  Sciprintf("}{");
	  pr_poly_latex(&fmt,Mat->var,Mat->S[j*Mat->m]->den,fw,0,TRUE );
	  Sciprintf("} & ");
	}
      Sciprintf("\\dfrac{");
      pr_poly_latex(&fmt,Mat->var,Mat->S[i+(Mat->n-1)*Mat->m]->num,fw,0,TRUE);
      Sciprintf("}{");
      pr_poly_latex(&fmt,Mat->var,Mat->S[i+(Mat->n-1)*Mat->m]->den,fw,0,TRUE);
      Sciprintf("}");
      if ( i != Mat->m -1 ) 
	Sciprintf("\\\\\n");
      else 
	Sciprintf("\n");
    }
  Sciprintf("\\end{pmatrix}\n");
  if ( use_math ) Sciprintf("\\end{equation*}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}

/**
 * nsp_rmatrix_create:
 * @name: name of object
 * @m: number of rows
 * @n: number of columns 
 * @cval: pointer to double or doubleC
 * @flag: an integer 
 * @dom: a character
 * @dample: a double 
 * 
 * returns a new mxn #NspRMatrix. if @flag 
 * is stricly negative the elements of the Matrix 
 * are initialized with %NULL. if @flag is null 
 * the elements are initilialized to zero. If flag 
 * is one or two, @cval is used for initialization 
 * (one for real values, two for complex values).
 * 
 * Returns:  a new #NspRMatrix or %NULL
 **/

static const doubleC Czero={0.00,0.00};

NspRMatrix *nsp_rmatrix_create(const char *name, int m, int n,const doubleC *cval, int flag, const char *varname,
			       char dom, double sample)
{
  int i;
  NspRMatrix *Loc;
  static const doubleC *init,*def=&Czero;
  
  if ( ((double) m)*((double) n) > INT_MAX )
    {
      Scierror("Error:\tMatrix dimensions too large\n");
      return NULLRMAT;
    }

  if ( (Loc= new_rmatrix()) == NULLRMAT ) 
    { 
      Scierror("Error:\tRunning out of memory\n");
      return NULLRMAT;
    }
  
  NSP_OBJECT(Loc)->ret_pos = -1 ; 
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  Loc->rc_type = 'r' ; /* XXXXX : a preciser ? **/
  Loc->var = NULL;
  Loc->S = NULL;
  Loc->dom = dom;
  Loc->dt = sample;
  
  if ( varname != NULL)
    {
      if (( Loc->var = nsp_new_string(varname,-1))==NULL )
	{
	  FREE(Loc);
	  return NULLRMAT;
	}
    }

  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    {
      FREE(Loc->var);
      FREE(Loc);
      return NULLRMAT;
    }

  if ( Loc -> mn == 0 )  /* empty rmatrix */
    {
      return Loc;
    }
  
  if ((Loc->S = malloc( Loc->mn* sizeof(nsp_rational ))) == NULL)
    { 
      Scierror("Error:\tRunning out of memory\n");
      goto fail;
    }

  for  ( i = 0 ; i < Loc->mn ; i++ ) Loc->S[i]= NULL;
  
  if ( flag >= 0) 
    {
      init = ( flag == 0) ?  def : cval;
      for ( i = 0 ; i < Loc->mn ; i++ )
	{
	  if ( (Loc->S[i] =nsp_basic_to_rational(init,(flag==2)? 'c':'r')) == NULL) goto fail;
	}
    }
  return Loc;
 fail:
  nsp_rmatrix_destroy(Loc);
  return NULLRMAT;
}

/**
 * nsp_rmatrix_create_m:
 * @name: name of object
 * @m: number of rows
 * @n: number of columns 
 * @Val: A @mx@n Matrix 
 * 
 * returns a new mxn #NspRMatrix. if @A is non null 
 * then it is used to initialize the result else 
 * rmatrix elements are  initialized to zero rational.
 * 
 * Returns:  a new #NspRMatrix or %NULL
 **/

NspRMatrix *nsp_rmatrix_create_m(char *name, int m, int n,NspMatrix *Val, const char *varname)
{
  int i;
  NspRMatrix *Loc=NULLRMAT;
  doubleC cval={0,0};

  if ( ((double) m)*((double) n) > INT_MAX )
    {
      Scierror("Error:\tMatrix dimensions too large\n");
      return NULLRMAT;
    }

  if ( (Loc= new_rmatrix()) == NULLRMAT ) 
    { 
      Scierror("Error:\tRunning out of memory\n");
      return NULLRMAT;
    }

  NSP_OBJECT(Loc)->ret_pos = -1 ; 
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  Loc->rc_type = 'r' ; /* XXXXX : a preciser ? **/
  Loc->var = NULL;
  Loc->S = NULL;
  Loc->dom = 'u'; /* undefined domain */
  Loc->dt = 1;

  if ( varname != NULL)
    {
      if (( Loc->var = nsp_new_string(varname,-1))==NULL )
	{
	  FREE(Loc);
	  return NULLRMAT;
	}
    }

  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    {
      FREE(Loc->var);
      FREE(Loc);
      return NULLRMAT;
    }

  if ( Loc-> mn == 0 ) 
    {
      /* empty rmatrix */
      return Loc;
    }
  
  if ( Val != NULL && (Val->mn != 1 && Val->mn != Loc->mn ))
    {
      Scierror("PMatCreate: initial value should be of size 1 or %dx%d\n",Loc->m,Loc->n);
      nsp_rmatrix_destroy(Loc);
      return NULLRMAT;
    }

  if ((Loc->S = malloc( Loc->mn* sizeof(nsp_rational ))) == (nsp_rational *) 0 )
    { 
      nsp_rmatrix_destroy(Loc);
      Scierror("Error:\tRunning out of memory\n");
      return NULLRMAT;
    }
  /* to enable nsp_rmatrix_destroy in case of pbs */
  for  ( i = 0 ; i < Loc->mn ; i++ ) Loc->S[i]= NULL;
  
  if ( Val != NULL) 
    {
      for ( i = 0 ; i < Loc->mn ; i++ )
	{
	  int ind = (( Val->mn == 1 ) ? 0 : i);
	  if  ( Val->rc_type == 'c') 
	    {
	      cval = Val->C[ind];
	    }
	  else
	    {
	      cval.r = Val->R[ind]; cval.i = 0.0;
	    }
	  if ( (Loc->S[i] =nsp_basic_to_rational(&cval,Val->rc_type)) == NULL) goto fail;
	}
    }
  else
    {
      for ( i = 0 ; i < Loc->mn ; i++ ) 
	{
	  if ( (Loc->S[i] =nsp_basic_to_rational(&cval,'r')) == NULL) goto fail;
	}
    }
  return Loc;
 fail:
  nsp_rmatrix_destroy(Loc);
  return NULLRMAT;
}


NspRMatrix *nsp_rmatrix_clone(char *name, NspRMatrix *A, int m, int n, int init)
{
  NspRMatrix *R;
  if ((R =  nsp_rmatrix_create(name, m, n, NULL,(init == TRUE) ? 0 :  -1, A->var,A->dom,A->dt))== NULL)
    return NULL;
  /* -1 for just allocating a matrix of pointers */
  return R;
}

/*
 * Delete the NspRMatrix A
 */

void nsp_rmatrix_destroy(NspRMatrix *A)
{
  int i;
  if ( A == NULLRMAT) return;
  nsp_object_destroy_name(NSP_OBJECT(A));
  if (  A-> mn != 0 && A->S != NULL ) 
    {
      for ( i = 0 ; i < A->mn ; i++ ) 
	{
	  nsp_rational_destroy(&A->S[i]);
	}
      FREE(A->S);
    }
  if ( A->var != NULL) nsp_string_destroy(&(A->var));
  FREE(A);
}


/*
 * Res =nsp_rmatrix_copy(A) 
 * Creates a Copy of NspRMatrix A : A is not checked 
 */

NspRMatrix *nsp_rmatrix_copy(NspRMatrix *A)
{
  int i;
  NspRMatrix *Loc;
  if ( ( Loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var,A->dom,A->dt)) == NULLRMAT) return(NULLRMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->S[i] != NULL)
	{
	  if ((Loc->S[ i] =nsp_rational_copy_with_name(A->S[i])) == (nsp_rational ) 0)  return(NULLRMAT);
	}
    }
  return(Loc);
}

/**
 * nsp_matrix_to_rmatrix:
 * @A: a #NspMatrix
 * 
 * return a new mxn rational matrix if @A is 
 * of size mxn. The (i,j)-th element of the result is the 
 * rational of degree 0 equal to @A(i,j).
 * 
 * Returns: a new #NspRMatrix or %NULL
 **/

NspRMatrix *nsp_matrix_to_rmatrix(NspMatrix *A)
{
  int i;
  NspRMatrix *Loc;
  doubleC d={0,0};
  if ((Loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, NULL,'u',1)) == NULLRMAT) 
    return(NULLRMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->rc_type == 'r') 
	{
	  d.r= A->R[i];
	}
      else
	{ d.r= A->C[i].r; d.i= A->C[i].i;}
      if ((Loc->S[i] =nsp_basic_to_rational(&d,A->rc_type)) == (nsp_rational ) 0)  return(NULLRMAT);
    }
  return(Loc);
}

/**
 * nsp_pmatrix_to_rmatrix:
 * @A: a #NspMatrix
 * 
 * return a new mxn rational matrix if @A is 
 * of size mxn. The (i,j)-th element of the result is the 
 * rational of degree 0 equal to @A(i,j).
 * 
 * Returns: a new #NspRMatrix or %NULL
 **/

NspRMatrix *nsp_pmatrix_to_rmatrix(NspPMatrix *A)
{
  int i;
  NspRMatrix *Loc;
  if ((Loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, NULL,'u',1)) == NULLRMAT) 
    return(NULLRMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ((Loc->S[i] = nsp_polynom_to_rational(A->S[i])) == (nsp_rational ) 0)  return(NULLRMAT);
    }
  return(Loc);
}

/**
 * nsp_matrices_to_rmatrix:
 * @A: a #NspMatrix
 * 
 * return a new mxn rational matrix if @A is 
 * of size mxn. The (i,j)-th element of the result is the 
 * rational of degree 0 equal to @A(i,j)/@B(i,j).
 * 
 * Returns: a new #NspRMatrix or %NULL
 **/

NspRMatrix *nsp_matrices_to_rmatrix(NspMatrix *A,NspMatrix *B)
{
  int i;
  NspRMatrix *Loc;
  doubleC d={0,0}, n={0,0};
  if ( A->m != B->m ||  A->n != B->n )
    {
      Scierror("Error: the two arguments should have same sizes\n");
      return NULL;
    }
  if ( A->rc_type != B->rc_type )
    {
      Scierror("Error: the two arguments should be both reals or complex\n");
      return NULL;
    }
  if ((Loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, NULL,'u',1)) == NULLRMAT) 
    return(NULLRMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->rc_type == 'r') 
	{
	  n.r= A->R[i];
	  d.r= B->R[i];
	}
      else
	{
	  n.r= A->C[i].r; n.i= A->C[i].i;
	  d.r= B->C[i].r; d.i= B->C[i].i;
	}
      if ((Loc->S[i] =nsp_basics_to_rational(&n,&d,A->rc_type)) == (nsp_rational ) 0)  return(NULLRMAT);
    }
  
  return(Loc);
}

/**
 * nsp_pmatrices_to_rmatrix:
 * @A: a #NspMatrix
 * 
 * return a new mxn rational matrix if @A is 
 * of size mxn (@B should have same size). 
 * The (i,j)-th element of the result is the 
 * rational of degree 0 equal to @A(i,j)/@B(i,j).
 * 
 * Returns: a new #NspRMatrix or %NULL
 **/

NspRMatrix *nsp_pmatrices_to_rmatrix(NspPMatrix *A,NspPMatrix *B,int simp)
{
  int i;
  NspRMatrix *Loc;
  if ( ! ( (A->mn == 1 && B->mn >0 ) || (A->mn >0 &&  B->mn == 1) || ( A->n == B->n && A->m == B->m )))
    {
      Scierror("Error: the two arguments should have same sizes\n");
      return NULL;
    }
  if ( A->rc_type != B->rc_type )
    {
      Scierror("Error: the two arguments should be both reals or complex\n");
      return NULL;
    }
  
  if ((Loc =nsp_rmatrix_create(NVOID,Max(A->m,B->m),Max(A->n,B->n),NULL,-1, NULL,'u',1)) == NULLRMAT) 
    return(NULLRMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ((Loc->S[i] =nsp_polynoms_to_rational(A->S[Min(i,A->mn-1)],B->S[Min(i,B->mn-1)],simp))== NULL)  return(NULLRMAT);
    }
  return(Loc);
}

/**
 * nsp_matrix_to_rmatrix_with_varname:
 * @A: a #NspMatrix
 * @varname: a char pointer 
 * 
 * return a new mxn rational matrix if @A is 
 * of size mxn. The (i,j)-th element of the result is the 
 * rational of degree 0 equal to @A(i,j).
 * @varname gives the rational variable name
 * 
 * Returns: a new #NspRMatrix or %NULL
 **/

NspRMatrix *nsp_matrix_to_rmatrix_with_varname(NspMatrix *A,const char *varname) 
{
  int i;
  NspRMatrix *Loc;
  doubleC d={0,0};
  if ((Loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, varname,'u',1)) == NULLRMAT) 
    return(NULLRMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->rc_type == 'r') 
	{
	  d.r= A->R[i];
	}
      else
	{ d.r= A->C[i].r; d.i= A->C[i].i;}
      if ((Loc->S[i] =nsp_basic_to_rational(&d,A->rc_type)) == (nsp_rational ) 0)  return(NULLRMAT);
    }
  return(Loc);
}

/**
 * nsp_rmatrix_elt_size:
 * @M: a #NspSMatrix 
 * 
 * size of string matrix elements.
 * 
 * Return value: size of @M elements.
 **/

unsigned int  nsp_rmatrix_elt_size(NspRMatrix *M)
{
  return sizeof(nsp_rational);
}

/**
 * nsp_rmatrix_resize:
 * @A: a #NspRMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspRMatrix @A dimensions are changed to be @m x @n. 
 * This routine only enlarges or shrink (using realloc()) 
 * the data array of @A to size mxn. The previous data are not moved and 
 * occupy the first array cells. Note that @A can be 
 * and empty matrix when calling this routine ( malloc() is used in that 
 * case ). The new elements of @A are filled with rational 0.
 *
 * returns: : %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_rmatrix_resize(NspRMatrix *A, int m, int n)
{
  int i;

  if ( ((double) m)*((double) n) > INT_MAX )
    {
      Scierror("Error:\tMatrix dimensions too large\n");
      return FAIL;
    }

  if ( A->mn == m*n ) 
    {
      A->m=m;
      A->n=n;
      return OK;
    };

  if ( m*n < 0) return FAIL;
  if ( m*n < A->mn )
    {
      /* Clear before Realloc */
      for ( i = m*n ; i < A->mn ; i++ )
	nsp_rational_destroy(&(A->S[i]));
    }
  if ( m*n == 0 ) 
    {
      A->m =  A->n = A->mn= 0;
      FREE(A->S);
      return OK;
    }
  
  if ( A->mn == 0 ) 
    A->S = (nsp_rational *)  MALLOC ((m*n+1) * sizeof(nsp_rational));
  else 
    A->S = (nsp_rational *)  REALLOC (A->S, (m*n+1) * sizeof(nsp_rational));
  if ( A->S == (nsp_rational *) 0) return FAIL;

  /* Initialize new area **/
  A->S[(m*n)] = (nsp_rational) 0;
  for ( i = A->mn ; i < m*n ; i++ )
    {
      if ((A->S[i] =nsp_basic_to_rational(&Czero,'r')) == ( nsp_rational ) 0 )  return(FAIL);
    }
  A->m =m ;
  A->n =n;
  A->mn=m*n ;
  if ( A->mn == 0) A->m = A->n = 0;
  return OK;
}

/*
 *nsp_rmatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for &Czero strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A 
 */

int nsp_rmatrix_enlarge(NspRMatrix *A, int m, int n)
{
  if ( ((double) m)*((double) n) > INT_MAX )
    {
      Scierror("Error:\tMatrix dimensions too large\n");
      return FAIL;
    }
  if ( n > A->n  )
    if ( nsp_rmatrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if ( nsp_rmatrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(PMAT1,PMAT2) ( PMAT1->m == PMAT2->m && PMAT1->n == PMAT2->n  )

/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int nsp_rmatrix_concat_right(NspRMatrix *A,const NspRMatrix *B)
{
  int Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("PMatConcat: incompatible size  \n");
      return(FAIL);
    }
  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("PMatConcat: incompatible rational variable name\n");
      return FAIL;
    }

  if ( nsp_rmatrix_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  if (nsp_pcopy_rational(B->mn,B->S,A->S+Asize) == FAIL) return(FAIL);
  return(OK);
}

static int nsp_pcopy_rational(int n, nsp_rational *s1, nsp_rational *s2)
{
  int i;
  /* copy is performed backward since this function is used for on place copy. 
   */
  for ( i = n-1 ; i >= 0 ; i--) 
    {
      nsp_rational_destroy(&s2[i]);
      if ((s2[ i] =nsp_rational_copy_with_name(s1[i])) == (nsp_rational ) 0)  return(FAIL);
    }
  return(OK);
}

/*
 * PMatAddCols : add n cols of zero to NspRMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int nsp_rmatrix_add_columns(NspRMatrix *A, int n)
{
  return nsp_rmatrix_resize(A,A->m,A->n+n);
}

int nsp_pset_rational(int n,const doubleC *s1, nsp_rational *s2)
{
  int i;
  for ( i = 0 ; i < n ; i++) 
    {
      nsp_rational_destroy(&s2[i]);
      if ((s2[ i] =nsp_basic_to_rational(s1,'r')) == (nsp_rational ) 0)  return(FAIL);
    }
  return(OK);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLRMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

NspRMatrix *nsp_rmatrix_concat_down(const NspRMatrix *A,const NspRMatrix *B)
{
  NspRMatrix *Loc;
  int j;
  if ( A->n != B->n ) 
    {
      Scierror("PMatConcatD: incompatible size  \n");
      return NULLRMAT;
    }

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("PMatConcatD: incompatible rational variable name\n");
      return NULLRMAT;
    }
  
  Loc =nsp_rmatrix_create(NVOID,A->m+B->m,A->n,&Czero,(int) 0, A->var,A->dom,A->dt);
  if ( Loc == NULLRMAT) 
    {
      Scierror("Error: running out of memory\n");
      return(NULLRMAT);
    }
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      if (nsp_pcopy_rational(A->m,A->S+j*A->m,Loc->S+j*(Loc->m)) == FAIL) 
	return(NULLRMAT);
      if (nsp_pcopy_rational(B->m,B->S+j*B->m,Loc->S+j*(Loc->m)+A->m) == FAIL)
	return(NULLRMAT);
    }
  return(Loc) ;
}


/*
 * Add Rows : Add m rows of zero to a NspRMatrix A 
 * A = [A;ones(m,n)]
 * return NULLRMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int nsp_rmatrix_add_rows(NspRMatrix *A, int m)
{
  int Am;
  int j;
  Am= A->m;
  if ( nsp_rmatrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  for ( j = A->n-1  ; j >= 1 ; j-- ) 
    {
      if (nsp_pcopy_rational(Am,A->S+j*Am,A->S+j*(A->m)) == FAIL) 
	return(FAIL);
    }
  for ( j = A->n-2  ; j >= 0 ; j-- ) 
    {
      if (nsp_pset_rational(m,&Czero,A->S+j*(A->m)+Am) == FAIL)
	return(FAIL);
    }
  return(OK);
}

/**
 * nsp_rmatrix_set_submatrix:
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

extern int nsp_rmatrix_set_submatrix(NspRMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspRMatrix *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}

/*
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 */

int nsp_rmatrix_set_rows(NspRMatrix *A, NspMatrix *Rows, NspRMatrix *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
}



/*
 * Res=nsp_rmatrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are changed (i.e converted to int) 
 * 
 */	

NspRMatrix *nsp_rmatrix_extract(NspRMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  return (NspRMatrix*)nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}

/*
 * Res=nsp_smatrix_extract_elements(A,Elts)
 * A unchanged, Elts
 */	

NspRMatrix*nsp_rmatrix_extract_elements(NspRMatrix *A, NspMatrix *Elts, int *err)
{
  *err=0;
  return (NspRMatrix *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}

/*
 * Res=nsp_rmatrix_extract_columns(A,Cols,err)
 * A unchanged
 */

NspRMatrix*nsp_rmatrix_extract_columns(NspRMatrix *A, NspMatrix *Cols, int *err)
{
  *err=0;
  return (NspRMatrix *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/*
 * Res=nsp_rmatrix_extract_rows(A,Rows,err)
 * A unchanged
 */

NspRMatrix*nsp_rmatrix_extract_rows(NspRMatrix *A, NspMatrix *Rows, int *err)
{
  *err=0;
  return (NspRMatrix *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
}

/**
 * nsp_rmatrix_extract_diag:
 * @A: a #NspRMatrix
 * @k: an integer 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns: a #NspRMatrix or %NULLSMAT 
 */

NspRMatrix  *nsp_rmatrix_extract_diag(NspRMatrix *A, int k) 
{
  NspRMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_rmatrix_create(NVOID,(int) 0 , (int) 0,&Czero, A->rc_type == 'c' ? 2 : 1, A->var,A->dom,A->dt );
      return(Loc);
    }
  if (( Loc =nsp_rmatrix_create(NVOID,imax-imin,1,&Czero,A->rc_type == 'c' ? 2 : 1,A->var,A->dom,A->dt )) == NULLRMAT)
    return(NULLRMAT);
  j=0; 
  for ( i = imin ; i < imax ; i++ ) 
    {
      if ((Loc->S[j++] =nsp_rational_copy_with_name(A->S[i+(i+k)*A->m])) == (nsp_rational ) 0)
	goto bug;
    }
  return(Loc);
 bug: 
  for ( i = 0 ; i < j; i++) 
    {
      nsp_rational_destroy(&Loc->S[i]);
      Loc->S[i]=NULL;
    }
  nsp_rmatrix_destroy(Loc);
  return(NULLRMAT);
}

/**
 * nsp_rmatrix_set_diag:
 * @A: a #NspRMatrix
 * @Diag: a #NspRMatrix
 * @k: an integer 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns: %OK or %FAIL.
 */

int nsp_rmatrix_set_diag(NspRMatrix *A, NspRMatrix *Diag, int k)
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

  if ( ! nsp_rmatrix_same_varname(A,Diag) )
    {
      Scierror("Error: incompatible rational variable names in set_diag\n");
      return FAIL;
    }

  if ( isize < Diag->mn ) 
    {
      imax = Diag->mn +imin;
      if (nsp_rmatrix_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
    }
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      nsp_rational_destroy(&(A->S[i+(i+k)*A->m]));
      if ((A->S[i+(i+k)*A->m] = nsp_rational_copy_with_name(Diag->S[j++])) == (nsp_rational) 0)
	return FAIL;
    }
  return OK;
}

/**
 * nsp_rmatrix_create_diag:
 * @Diag: a #NspRMatrix
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns: a #NspSMatrix or %NULLSMAT 
 */

NspRMatrix  *nsp_rmatrix_create_diag(NspRMatrix *Diag, int k)
{
  int i,j;
  int imin,imax;
  NspRMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  if (( Loc =nsp_rmatrix_create(NVOID,imax,imax+k,&Czero,Diag->rc_type == 'c' ? 2 : 1 , Diag->var
				,Diag->dom,Diag->dt)) == NULLRMAT) 
    return(NULLRMAT);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      nsp_rational_destroy(&Loc->S[i+(i+k)*Loc->m]);
      if ((Loc->S[i+(i+k)*Loc->m] =nsp_rational_copy_with_name( Diag->S[j++])) == (nsp_rational) 0)
	return(NULLRMAT);
    }
  return(Loc);
}

/**
 * nsp_rmatrix_transpose: 
 * @A: a #NspRMatrix
 *
 * return the transpose of A
 * 
 * returns:  a new #MspPMatrix or %NULLMAT 
 */

NspRMatrix *nsp_rmatrix_transpose(const NspRMatrix *A)
{
  int i,j;
  NspRMatrix *Loc;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_rmatrix_create(NVOID,A->n,A->m,NULL,-1, A->var,A->dom,A->dt)) == NULLRMAT) 
    return NULLRMAT;
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Loc->m ; i++ )
    for ( j = 0 ; j < Loc->n ; j++ )
      {
	if ((Loc->S[i+(Loc->m)*j] =nsp_rational_copy_with_name(A->S[j+(A->m)*i])) == NULL ) 
	  return(NULLRMAT);
      }
  return(Loc);
}

/* returns eye(P)
 */

#if 0

NspRMatrix *nsp_rmatrix_identity(NspRMatrix *P)
{
  NspRMatrix *Q=NULL;
  /* return identity */
  int i,j;
  doubleC zero={0,0},un={1,0},*val=&zero;
  if ((Q =nsp_rmatrix_create(NVOID,P->m,P->n,NULL,-1, P->var)) == NULLRMAT) 
    return(NULLRMAT);
  for ( i = 0 ; i < Q->m ; i++ )
    for (  j = 0 ; j < Q->n ; j++ )
      {
	val= ( i==j ) ? &un :&zero;
	if ((Q->S[i+Q->m*j] =nsp_basic_to_rational(val,P->S[i+P->m*j]->rc_type)) == (nsp_rational ) 0) 
	  return NULLRMAT;
      }
  return Q;
}
#endif

#if 0
NspRMatrix *nsp_rmatrix_hat_p_m(NspRMatrix *P,int n)
{
  NspRMatrix *Q=NULL,*R=NULL,*loc=NULL;
  /* general case: power by repeated squaring */
  if ( n == 0 )
    {
      return nsp_rmatrix_identity(P);
    }
  if ((Q = nsp_rmatrix_copy(P)) == NULL  )
    return NULL;
  while  ( n > 1 )
    {
      if ( n % 2 ) 
	{
	  if ( R == NULL) 
	    {
	      if ((R = nsp_rmatrix_copy(Q))== NULL) goto err;
	    }
	  else
	    {
	      if ((loc = nsp_rmatrix_mult_p_p(R,Q)) == NULL) goto err;
	      nsp_rmatrix_destroy(R);
	      R=loc;
	    }
	}
      n /= 2;
      if ((loc = nsp_rmatrix_mult_p_p(Q,Q)) == NULL) goto err;
      nsp_rmatrix_destroy(Q);
      Q=loc;
    }
  if ( R != NULL) 
    {
      if ((loc = nsp_rmatrix_mult_p_p(Q,R)) == NULL) goto err;
      nsp_rmatrix_destroy(R);
      nsp_rmatrix_destroy(Q);
    }
  else 
    {
      loc = Q;
    }
  return loc;
 err:
  if ( Q != NULL) nsp_rmatrix_destroy(Q);
  if ( R != NULL) nsp_rmatrix_destroy(R);
  return NULL;

}
#endif

#if 0

NspRMatrix *nsp_rmatrix_dh_p_m(const NspRMatrix *P,const NspMatrix *M) 
{
  int i;
  NspRMatrix *loc;
#define P_POWER(s1,s2,i1,i2)						\
  if ((loc =nsp_rmatrix_create(NVOID,s1,s2,NULL,-1, P->var))== NULLRMAT)\
    return(NULLRMAT);							\
  for (i=0; i < loc->mn ; i++)						\
    {									\
      if ( floor(M->R[i2]) == M->R[i2] &&  M->R[i2] >= 0) {		\
	loc->S[i]= nsp_rational_power( P->S[i1], M->R[i2]);		\
	if ( loc->S[i] == NULL) return NULL;				\
      } else {								\
	Scierror("Error: exponent should be a positive integer \n");	\
	nsp_rmatrix_destroy(loc);					\
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

#endif 

/**
 * nsp_rmatrix_comp:
 * @A: a #NspRMatrix 
 * @B: a #NspRMatrix 
 * @op: the code for the comparison as a string
 * 
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j). A and B must be size compatible with 
 * the standard promotion of scalars i.e 1x1 matrices. 
 * A and B are unchanged : Res is created 
 * 
 * Return value: a new #NspBMatrix
 **/

static int nsp_rational_eq(nsp_rational p, nsp_rational q)
{
  int err;
  int rep = nsp_mat_fullcomp (p->num,q->num,"==", &err);
  if ( err == TRUE || rep == FALSE ) return FALSE;
  rep = nsp_mat_fullcomp (p->den,q->den,"==", &err);
  if ( err == TRUE || rep == FALSE ) return FALSE;
  return TRUE;
}

NspBMatrix  *nsp_rmatrix_comp(NspRMatrix *A, NspRMatrix *B,const char *op)
{
  int i;
  NspBMatrix *Loc ;

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("Error: incompatible rational variable names in '%s' operator\n",op);
      return NULLBMAT;
    }
  
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
		if ( ! nsp_rational_eq(A->S[i],B->S[0]) ) Loc->B[i] = FALSE;
	    }
	  else if ( strcmp(op,"<>")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if (  nsp_rational_eq(A->S[i],B->S[0])  ) Loc->B[i] = FALSE;
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
		if ( ! nsp_rational_eq(A->S[0],B->S[i])  ) Loc->B[i] = FALSE;
	    }
	  else if ( strcmp(op,"<>")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if ( nsp_rational_eq(A->S[i],B->S[0])  ) Loc->B[i] = FALSE;
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
		if ( ! nsp_rational_eq(A->S[i],B->S[i]) ) Loc->B[i] = FALSE;
	    }
	  else if ( strcmp(op,"<>")==0 ) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )  
		if (  nsp_rational_eq(A->S[i],B->S[i]) ) Loc->B[i] = FALSE;
	    }
	  else goto wrong;
	}
    }
  return(Loc);
 wrong:
  Scierror("Error: operation %s is not implemented for rational matrices\n",op);
  return NULLBMAT ;
}


/**
 * nsp_rational_resize:
 * @poly: a #nsp_rational
 * 
 * Remove leading zeros from a rational 
 * representation.
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_rational_resize(nsp_rational rat)
{
  nsp_polynom_resize(rat->den);
  nsp_polynom_resize(rat->num);
  return OK;
}

/**
 * nsp_rmatrix_add:
 * @A: a #NspRMatrix 
 * @B: a #NspRMatrix 
 * 
 * return the sum of @A and @B.
 * 
 * Returns: a nex #NspRMatrix  or %NULL
 **/

#if 0
NspRMatrix *nsp_rmatrix_add(NspRMatrix *A, NspRMatrix *B)
{
  NspRMatrix *loc;

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("Error: incompatible rational variable names in rational addition\n");
      return NULL;
    }
  
  if ( SameDim(A,B) ) 
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_rational_add(A->S[i],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( A->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	if ((loc->S[i] = nsp_rational_add(A->S[0],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_rational_add(A->S[i],B->S[0]))== NULL) 
	  return NULL;
      return loc;
    }
  else
    {
      Scierror("Error:\tArguments must have the same size\n");
      return NULL;
    }
}
#endif

#if 0

NspRMatrix *nsp_rmatrix_minus(NspRMatrix *A, NspRMatrix *B)
{
  NspRMatrix *loc;

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("Error: incompatible rational variable names in rational substraction\n");
      return NULL;
    }
  
  if ( SameDim(A,B) ) 
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_rational_minus(A->S[i],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( A->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1,  A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	if ((loc->S[i] = nsp_rational_minus(A->S[0],B->S[i]))== NULL) 
	  return NULL;
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1,  A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	if ((loc->S[i] = nsp_rational_minus(A->S[i],B->S[0]))== NULL) 
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
 * nsp_rational_add:
 * @P: a nsp_rational 
 * @Q: a nsp_rational
 * 
 * return a new rational @P + @Q.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

#endif

#if 0

nsp_rational nsp_rational_add(nsp_rational P,nsp_rational Q)
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
  if ( nsp_rational_resize(A) == FAIL) return NULL;
  if (nsp_object_set_name(NSP_OBJECT(A),"pe") == FAIL) 
    {
      nsp_matrix_destroy(A);
      return NULL;
    }
  return A;
}

/**
 * nsp_rational_add_in_place 
 * @P: a nsp_rational 
 * @Q: a nsp_rational
 * 
 * returns @P + @Q in @P. Polynomial @P is 
 * assumed to have the largest degree.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

#endif

#if 0

int nsp_rational_add_in_place(nsp_rational P,nsp_rational Q)
{
  int i;
  if ( P->mn < Q->mn) return FAIL;
  if ( P->rc_type == 'r' && Q->rc_type == 'c' ) 
    {
      if ( nsp_mat_complexify (P,0.0) == FAIL ) return FAIL;
    }
  if ( P->rc_type == 'r' ) 
    {
      for ( i= 0 ; i < Q->mn ; i++) P->R[i] += Q->R[i];
    }
  else 
    {
      if ( Q->rc_type == 'r' ) 
	for ( i= 0 ; i < Q->mn ; i++) P->C[i].r += Q->R[i];
      else
	for ( i= 0 ; i < Q->mn ; i++) 
	  {
	    P->C[i].r += Q->C[i].r ;
	    P->C[i].i += Q->C[i].i ;
	  }
    }
  return OK;
}

#endif

#if 0

nsp_rational nsp_rational_zero_create(int degree, char rc_type)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create("pe",rc_type,1, Max(degree,0)+1)) ==NULLMAT)
    return((nsp_rational ) 0);
  if ( rc_type == 'c')  nsp_mat_set_ival(A,0.0);
  nsp_mat_set_rval(A,0.0);
  return A;
}

/**
 * nsp_rational_minus:
 * @P: a nsp_rational 
 * @Q: a nsp_rational
 * 
 * return a new rational @P - @Q.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/
#endif

#if 0

nsp_rational nsp_rational_minus(nsp_rational P,nsp_rational Q)
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
  if ( nsp_rational_resize(A) == FAIL) return NULL;
  /* give a name to the rational */
  if (nsp_object_set_name(NSP_OBJECT(A),"pe") == FAIL) 
    {
      nsp_matrix_destroy(A);
      return NULL;
    }
  return A;
}

#endif

#if 0

/* 
 * A*B A matrix B PMatrix 
 */

NspRMatrix *nsp_rmatrix_mult_m_p(NspMatrix *A, NspRMatrix *B)
{
  NspRMatrix *loc;
  if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, B->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_rational_copy_with_name(B->S[i]))== NULL) 
	    return NULL;
	  if ( nsp_mat_mult_scalar(loc->S[i],A) == FAIL )  return NULL;
	  if ( nsp_rational_resize(loc->S[i])== FAIL ) return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      NspMatrix *C;
      int i;
      if ((C = nsp_matrix_create(NVOID,A->rc_type,1,1))== NULLMAT)
	return NULL;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, B->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_rational_copy_with_name(B->S[0]))== NULL) 
	    return NULL;
	  if ( C->rc_type =='r' ) 
	    C->R[0]=A->R[i];
	  else 
	    C->C[0]=A->C[i];
	  if ( nsp_mat_mult_scalar(loc->S[i],C) == FAIL )
	    return NULL;
	  if ( nsp_rational_resize(loc->S[i])== FAIL ) return NULL;
	}
      nsp_matrix_destroy(C);
      return loc;
    }
  else if ( A->n == B->m  )
    {
      NspMatrix *As;
      int i,j,k;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,B->n,NULL,-1, B->var))== NULLRMAT) 
	return(NULLRMAT);
      if ((As = nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) 
	return NULLRMAT;
      for (i=0; i < loc->m ; i++) 
	for ( j = 0 ; j < loc->n ; j++)
	  {
	    nsp_rational l = NULL,l1,l2;
	    k= 0;
	    if ( As->rc_type == 'r' )
	      As->R[0] = A->R[i+A->m*k];
	    else
	      As->C[0] = A->C[i+A->m*k];
	    l = nsp_rational_mult( As, B->S[k+B->m*j]);
	    if ( l == NULL) return NULL;
	    for ( k = 1 ; k < A->n ; k++)
	      {
		if ( As->rc_type == 'r' )
		  As->R[0] = A->R[i+A->m*k];
		else
		  As->C[0] = A->C[i+A->m*k];
		l1 =  nsp_rational_mult( As, B->S[k+B->m*j]);
		if ( l1 == NULL ) return NULL;
		if (( l2 =  nsp_rational_add(l, l1))==NULL) return NULL;
		nsp_rational_destroy(&l);
		nsp_rational_destroy(&l1);
		l= l2;
	      }
	    if ( nsp_rational_resize(l)== FAIL ) return NULL;
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

#endif

#if 0

NspRMatrix *nsp_rmatrix_mult_p_m(NspRMatrix *A, NspMatrix *B)
{
  NspRMatrix *loc;
  if ( A->mn == 1 ) 
    {
      NspMatrix *C = NULL;
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var)) == NULLRMAT) 
	return NULLRMAT;
      C = nsp_matrix_create(NVOID,B->rc_type,1,1);
      if ( C == NULLMAT) { return NULLRMAT; }

      for (i=0; i < B->mn ; i++) 
	{
	  if ( C->rc_type =='r' ) 
	    {
	      C->R[0]=B->R[i];
	    }
	  else 
	    {
	      C->C[0]=B->C[i];
	    }
	  if ((loc->S[i] = nsp_rational_copy_with_name(A->S[0]))== NULL) goto err;
	  if ( nsp_mat_mult_scalar(loc->S[i],C) == FAIL ) goto err;
	  if ( nsp_rational_resize(loc->S[i])== FAIL ) goto err;
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
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] = nsp_rational_copy_with_name(A->S[i]))== NULL) return NULL;
	  if ( nsp_mat_mult_scalar(loc->S[i],B) == FAIL )  return NULL;
	  if ( nsp_rational_resize(loc->S[i])== FAIL ) return NULL;
	}
      return loc;
    }
  else if ( A->n == B->m  )
    {
      NspMatrix *Bs;
      int i,j,k;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      if ((Bs = nsp_matrix_create(NVOID,B->rc_type,1,1)) == NULLMAT) 
	return NULLRMAT;
      for (i=0; i < loc->m ; i++) 
	for ( j = 0 ; j < loc->n ; j++)
	  {
	    nsp_rational l = NULL,l1,l2;
	    k= 0;
	    if ( Bs->rc_type == 'r' )
	      Bs->R[0] =  B->R[k+B->m*j];
	    else
	      Bs->C[0] =  B->C[k+B->m*j];
	    l = nsp_rational_mult(A->S[i+A->m*k] , Bs);
	    if ( l == NULL) return NULL;
	    for ( k = 1 ; k < A->n ; k++)
	      {
		if ( Bs->rc_type == 'r' )
		  Bs->R[0] =  B->R[k+B->m*j];
		else
		  Bs->C[0] =  B->C[k+B->m*j];
		l1 =  nsp_rational_mult(A->S[i+A->m*k] , Bs);
		if ( l1 == NULL ) return NULL;
		if (( l2 =  nsp_rational_add(l, l1))==NULL) return NULL;
		nsp_rational_destroy(&l);
		nsp_rational_destroy(&l1);
		l= l2;
	      }
	    if (( nsp_rational_resize(l))== FAIL ) return NULL;
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

#endif

#if 0

NspRMatrix *nsp_rmatrix_mult_p_p(NspRMatrix *A, NspRMatrix *B)
{
  NspRMatrix *loc;

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("Error: incompatible rational variable names in rational multiplication\n");
      return NULL;
    }

  if ( A->n == B->m  )
    {
      int i,j,k;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < loc->m ; i++) 
	for ( j = 0 ; j < loc->n ; j++)
	  {
	    nsp_rational l = NULL,l1,l2;
	    k= 0;
	    l = nsp_rational_mult( A->S[i+A->m*k], B->S[k+B->m*j]);
	    if ( l == NULL) return NULL;
	    for ( k = 1 ; k < A->n ; k++)
	      {
		l1 =  nsp_rational_mult( A->S[i+A->m*k], B->S[k+B->m*j]);
		if ( l1 == NULL ) return NULL;
		if (( l2 =  nsp_rational_add(l, l1))==NULL) return NULL;
		nsp_rational_destroy(&l);
		nsp_rational_destroy(&l1);
		l= l2;
	      }
	    loc->S[i+loc->m*j]=l;
	  }
      return loc;
    }
  else if ( A->mn == 1 ) 
    {
      return nsp_rmatrix_mult_tt(A,B);
    }
  else if ( B->mn == 1 )
    {
      return nsp_rmatrix_mult_tt(A,B);
    }
  else
    {
      Scierror("Error:\tUncompatible dimensions\n");
      return NULL;
    }
}

#endif

#if 0

NspRMatrix *nsp_rmatrix_mult_tt(NspRMatrix *A, NspRMatrix *B)
{
  NspRMatrix *loc;

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("Error: incompatible rationalial variable names in rationalial multiplication\n");
      return NULL;
    }

  
  if ( SameDim(A,B) )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]=  nsp_rational_mult( A->S[i], B->S[i]);
	  if ( loc->S[i] == NULL) return NULL;
	}
      return loc;
    }
  else if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, B->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]= nsp_rational_mult( A->S[0], B->S[i]);
	  if ( loc->S[i] == NULL)     return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] =  nsp_rational_mult( A->S[i], B->S[0] )) == NULL) 
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

#endif

#if 0

NspRMatrix *nsp_rmatrix_mult_tt_p_m(NspRMatrix *A, NspMatrix *B)
{
  NspRMatrix *loc;
  if ( SameDim(A,B) )
    {
      int i;
      int flag = (B->rc_type == 'r') ? 1: 2;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]=  nsp_rational_mult_m( A->S[i], B->R+i*flag,B->rc_type );
	  if ( loc->S[i] == NULL) return NULL;
	  if (( nsp_rational_resize(loc->S[i]))== FAIL ) return NULL;
	}
      return loc;
    }
  else if ( A->mn == 1 ) 
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]= nsp_rational_mult_m( A->S[0], B->R+i,B->rc_type);
	  if ( loc->S[i] == NULL)     return NULL;
	  if (( nsp_rational_resize(loc->S[i]))== FAIL ) return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, NULL))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] =  nsp_rational_mult_m( A->S[i], B->R,B->rc_type  )) == NULL) 
	    return NULL;
	  if (( nsp_rational_resize(loc->S[i]))== FAIL ) return NULL;
	}
      return loc;
    }
  else
    {
      Scierror("Error:\targuments should have the same size\n");
      return NULL;
    }
}


NspRMatrix *nsp_rmatrix_mult_tt_m_p(NspMatrix *A, NspRMatrix *B)
{
  return nsp_rmatrix_mult_tt_p_m(B,A);
}

NspRMatrix *nsp_rmatrix_div_tt_p_m(NspRMatrix *A, NspMatrix *B,int flag )
{
  NspRMatrix *loc;
  if ( SameDim(A,B) && A->mn != 1 )
    {
      if ( flag ) 
	{
	  int i;
	  if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	    return(NULLRMAT);
	  for (i=0; i < B->mn ; i++) 
	    {
	      loc->S[i]= nsp_rational_div_m( A->S[i], B->R+i,B->rc_type);
	      if ( loc->S[i] == NULL)     return NULL;
	      if (( nsp_rational_resize(loc->S[i]))== FAIL ) return NULL;
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
      if ((loc =nsp_rmatrix_create(NVOID,B->m,B->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < B->mn ; i++) 
	{
	  loc->S[i]= nsp_rational_div_m( A->S[0], B->R+i,B->rc_type);
	  if ( loc->S[i] == NULL)     return NULL;
	  if (( nsp_rational_resize(loc->S[i]))== FAIL ) return NULL;
	}
      return loc;
    }
  else if ( B->mn == 1 )
    {
      int i;
      if ((loc =nsp_rmatrix_create(NVOID,A->m,A->n,NULL,-1, A->var))== NULLRMAT) 
	return(NULLRMAT);
      for (i=0; i < A->mn ; i++) 
	{
	  if ((loc->S[i] =  nsp_rational_div_m( A->S[i], B->R,B->rc_type  )) == NULL) 
	    return NULL;
	  if (( nsp_rational_resize(loc->S[i]))== FAIL ) return NULL;
	}
      return loc;
    }
  else
    {
      Scierror("Error:\targuments should have the same size\n");
      return NULL;
    }
}

NspRMatrix *nsp_rmatrix_div_tt_m_p(NspMatrix *A, NspRMatrix *B)
{
  Scierror("Error: unimplemented \n");
  return NULL;
}



/* P(i) evaluated for x= V(k);  */

NspMatrix *nsp_rmatrix_horner_tt(NspRMatrix *P,NspMatrix *V)
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
      /* rational is complex or real */					\
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

NspMatrix *nsp_rmatrix_horner(NspRMatrix *P,NspMatrix *V,int k)
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
      /* rational is complex or real */
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

int nsp_rmatrix_pdiv_tt(NspRMatrix *A, NspRMatrix *B, NspRMatrix **Q, NspRMatrix **R)
{
  int i, m= Max(A->m,B->m), n=Max(A->n,B->n) , mn=Max(A->mn,B->mn);
  nsp_rational q,r;
  NspRMatrix *Ql,*Rl;

  if ( ! nsp_rmatrix_same_varname(A,B) ) 
    {
      Scierror("Error: incompatible rationalial variable names in rationalial division\n");
      return FAIL;
    }

  if ( !( SameDim(A,B) || A->mn == 1 || B->mn == 1)) 
    {
      Scierror("Error:\targuments should have the same size\n");
      return FAIL;
    }
  if ((Ql =nsp_rmatrix_create(NVOID,m,n,NULL,-1, A->var ))== NULLRMAT) 
    return FAIL;
  if ((Rl =nsp_rmatrix_create(NVOID,m,n,NULL,-1, A->var))== NULLRMAT) 
    return FAIL;
  for (i=0; i < mn ; i++) 
    {
      if (nsp_rational_pdiv( A->S[Min(i,A->mn-1)], B->S[Min(i,B->mn-1)],&q,&r)== FAIL) 
	return FAIL;
      Ql->S[i]= q;
      Rl->S[i]= r;
    }
  *Q = Ql;
  *R = Rl;
  return OK;
}
#endif

/**
 * nsp_rmatrix_sum:  computes various sums of @A
 * @A: a #NspRMatrix
 * @dim: an integer 
 * 
 * for dim=0 the sum of all elements of @A is computed, a scalar is returned
 * for dim=1 the sum over the row indices is computed, a row vector is returned. 
 * for dim=2 the sum over the column indices is computed, a column vector is returned. 
 * else dim=0 is forced.
 * 
 * Return value: a  #NspRMatrix (a scalar, row or column rationalial vector)
 **/

#if 0
NspRMatrix *nsp_rmatrix_sum(NspRMatrix *A, int dim)
{
  NspRMatrix *Sum = NULL;
  int j,degree=0, i;
  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
      if ((Sum = nsp_rmatrix_create(NVOID,1,1,NULL,-1, A->var)) == NULLRMAT) 
	return(NULLRMAT);
      for ( i = 0 ; i < A->mn ; i++) 
	{
	  if ( A->S[i]->mn -1 > degree ) degree =  A->S[i]->mn -1;
	}
      if ((Sum->S[0] = nsp_rational_zero_create(degree, A->rc_type)) == NULL) 
	return(NULLRMAT);
      for ( i = 0 ; i < A->mn ; i++) 
	{
	  if (nsp_rational_add_in_place(Sum->S[0] , A->S[i]) == FAIL) 
	    return(NULLRMAT);
	}
      break;
    case 1:
      if ((Sum = nsp_rmatrix_create(NVOID,1,A->n,NULL,-1, A->var)) == NULLRMAT) 
	return NULLRMAT;
      for ( j= 0 ; j < A->n ; j++) 
	{
	  degree = 0;
	  for ( i = 0 ; i < A->m ; i++) 
	    {
	      if ( A->S[i+A->m*j]->mn -1 > degree ) 
		degree =  A->S[i+A->m*j]->mn -1;
	    }
	  if ((Sum->S[j] = nsp_rational_zero_create(degree, A->rc_type)) == NULL) 
	    return(NULLRMAT);
	  for ( i = 0 ; i < A->m ; i++) 
	    {
	      if (nsp_rational_add_in_place(Sum->S[j] , A->S[i+A->m*j]) == FAIL) 
		return(NULLRMAT);
	    }
	}
      break;
    case 2:
      if ((Sum = nsp_rmatrix_create(NVOID,A->m,1,NULL,-1, A->var)) == NULLRMAT) 
	return NULLRMAT;
      for ( i = 0 ; i < A->m ; i++) 
	{
	  degree = 0;
	  for ( j = 0 ; j < A->n ; j++) 
	    {
	      if ( A->S[i+A->m*j]->mn -1 > degree ) 
		degree =  A->S[i+A->m*j]->mn -1;
	    }
	  if ((Sum->S[i] = nsp_rational_zero_create(degree, A->rc_type)) == NULL) 
	    return(NULLRMAT);
	  for ( j = 0 ; j < A->n ; j++) 
	    {
	      if (nsp_rational_add_in_place(Sum->S[i] , A->S[i+A->m*j]) == FAIL) 
		return(NULLRMAT);
	    }
	}
      break;
    }
  return Sum;
}
#endif

/**
 * nsp_rmatrix_prod:  computes various products of elements of @A
 * @A: a #NspRMatrix
 * @dim: an integer 
 * 
 *  for dim=0 the product of all elements is computed, a scalar is returned.
 *  for dim=1 the product over the row indices is computed, a row vector is returned. 
 *  for dim=2 the product over the column indices is computed, a column vector is returned. 
 *  else dim=0 is forced.
 * 
 * Return value: a  #NspRMatrix (a scalar, row or column rationalial vector)
 **/
#if 0
NspRMatrix *nsp_rmatrix_prod(NspRMatrix *A, int dim)
{
  const doubleC one={1,0};
  nsp_rational p,q;
  NspRMatrix *Prod;
  int i,j;
  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0:
      if ((Prod = nsp_rmatrix_create(NVOID,1,1,NULL,-1,A->var)) == NULLRMAT) 
	return(NULLRMAT);
      if (( q= p = nsp_basic_to_rational(&one,A->rc_type)) == (nsp_rational ) 0) 
	return NULLRMAT;
      for ( i = 0 ; i < A->mn ; i++)
	{
	  q = nsp_rational_mult(p,A->S[i]);
	  if ( q == NULL) return NULLRMAT;
	  nsp_rational_destroy(&p);
	  p=q;
	}
      Prod->S[0]=q;
      break;
    case 1:
      if ((Prod = nsp_rmatrix_create(NVOID,1,A->n,NULL,-1,A->var)) == NULLRMAT) 
	return NULLRMAT;
      for ( j=0 ; j < A->n ; j++) 
	{
	  if (( q= p =nsp_basic_to_rational(&one,A->rc_type)) == (nsp_rational ) 0) 
	    return NULLRMAT;

	  for ( i = 0 ; i < A->m ; i++)
	    {
	      q = nsp_rational_mult(p,A->S[i+A->m*j]);
	      if ( q == NULL) return NULLRMAT;
	      nsp_rational_destroy(&p);
	      p=q;
	    }
	  Prod->S[j]=q;
	}
      break;
    case 2:
      if ((Prod = nsp_rmatrix_create(NVOID,A->m,1,NULL,-1,A->var)) == NULLRMAT) 
	return NULLRMAT;
      for ( i=0 ; i < A->m ; i++) 
	{
	  if ((q= p =nsp_basic_to_rational(&one,A->rc_type)) == (nsp_rational ) 0) 
	    return NULLRMAT;
	  for ( j = 0 ; j < A->n ; j++)
	    {
	      q = nsp_rational_mult(p,A->S[i+A->m*j]);
	      if ( q == NULL) return NULLRMAT;
	      nsp_rational_destroy(&p);
	      p=q;
	    }
	  Prod->S[i]=q;
	}
      break;
    }
  return Prod;
}

/**
 * nsp_mat_cum_prod:  cumulative products of elements of @A
 * @A: a #NspMatrix
 * @dim: and integer 
 * 
 * for dim=0 the cumulative product over all elements is computed (in column major order).
 * for dim=1 the cumulative product over the row indices is computed.
 * for dim=2 the cumulative product over the column indices is computed.
 * else dim=0 is forced.
 * 
 * Return value: a #NspMatrix of same dim than @A
 **/

 /* 
NspMatrix *nsp_mat_cum_prod(NspMatrix *A, int dim)
{
  double cuprod;
  doubleC C_cuprod;
  NspMatrix *Prod;
  int i,j, k, kp;

  if ( A->mn == 0) return nsp_matrix_create(NVOID,'r',A->m,A->n);

  if ((Prod = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) 
    return NULLMAT;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0: 
      if ( A->rc_type == 'r' ) 
	{
	  cuprod=1.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    Prod->R[i] = (cuprod *= A->R[i]);
	}
      else
	{
	  C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    { 
	      nsp_prod_c(&C_cuprod,&A->C[i]);
	      Prod->C[i] = C_cuprod;
	    }
	}
      break;

    case 1:
      if ( A->rc_type == 'r' ) 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    cuprod=1.00;
	    for ( i=0 ; i < A->m ; i++) 
	      Prod->R[i+(A->m)*j] = (cuprod *= A->R[i+(A->m)*j]);
	  }
      else
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	    for ( i=0 ; i < A->m ; i++) 
	      { 
		nsp_prod_c(&C_cuprod,&A->C[i+j*A->m]);
		Prod->C[i+j*A->m] = C_cuprod;
	      }
	  }
      break;

    case 2:
      if ( A->rc_type == 'r' ) 
	{
	  memcpy(Prod->R, A->R, A->mn*sizeof(double));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    Prod->R[k] *= Prod->R[kp];
	}
      else
	{
	  memcpy(Prod->C, A->C, A->mn*sizeof(doubleC));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    nsp_prod_c(&Prod->C[k], &Prod->C[kp]);
	}
      break;
    }

  return Prod;
}

 */

/**
 * nsp_mat_cum_sum:  cumulative sums of elements of @A
 * @A: a #NspMatrix
 * @dim: an integer 
 *
 * for dim=0 the cumulative sum over all elements is computed (in column major order).
 * for dim=1 the cumulative sum over the row indices is computed.
 * for dim=2 the cumulative sum over the column indices is computed.
 * else dim=0 is forced.
 * 
 * Return value: a #NspMatrix of same dim than @A
 **/

  /*
NspMatrix *nsp_mat_cum_sum(NspMatrix *A, int dim)
{
  double cusum;
  doubleC C_cusum;
  NspMatrix *Sum;
  int i,j, k, kp;

  if ( A->mn == 0) 
    return  nsp_matrix_create(NVOID,'r',A->m,A->n);

  if ((Sum = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) 
    return NULLMAT;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0: 
      if ( A->rc_type == 'r' ) 
	{
	  cusum=0.00;
	  for ( i=0 ; i < A->mn ; i++)
	    Sum->R[i] = (cusum += A->R[i]);
	}
      else
	{
	  C_cusum.r  = 0.00 ; C_cusum.i = 0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    { 
	      Sum->C[i].r = ( C_cusum.r += A->C[i].r);
	      Sum->C[i].i = ( C_cusum.i += A->C[i].i);
	    }
	}
      break;

    case 1:
      if ( A->rc_type == 'r' ) 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    cusum=0.00;
	    for ( i=0 ; i < A->m ; i++) 
	      Sum->R[i+(A->m)*j] = (cusum += A->R[i+(A->m)*j]);
	  }
      else
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    C_cusum.r  = 0.00 ; C_cusum.i = 0.00;
	    for ( i=0 ; i < A->m ; i++) 
	      { 
		Sum->C[i+j*A->m].r = ( C_cusum.r +=A->C[i+j*A->m].r);
		Sum->C[i+j*A->m].i = ( C_cusum.i +=A->C[i+j*A->m].i);
	      }
	  }
      break;

    case 2:
      if ( A->rc_type == 'r' ) 
	{
	  memcpy(Sum->R, A->R, A->mn*sizeof(double));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    Sum->R[k] += Sum->R[kp];
	}
      else
	{
	  memcpy(Sum->C, A->C, A->mn*sizeof(doubleC));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    {
	      Sum->C[k].r += Sum->C[kp].r;
	      Sum->C[k].i += Sum->C[kp].i;
	    }
	}
      break;
    }

  return Sum;
}

*/
#endif

/**
 * nsp_rmatrix_triu:
 * @A: a #NspRMatrix 
 * @k: an integer 
 * 
 * A = triu(A,k). 
 **/

int nsp_rmatrix_triu(NspRMatrix *A, int k)
{
  const doubleC zeroC = {0.0,0.0};
  nsp_rational *Aj,loc;
  int i,j;
  for ( j = 0, Aj = A->S ; j < Min(A->m+k-1,A->n) ; j++, Aj += A->m )
    for ( i = Max(0,j+1-k) ; i < A->m ; i++)
      {
	if ( (loc = nsp_basic_to_rational(&zeroC, A->rc_type)) == NULL) 
	  return FAIL;
	Aj[i] = loc;
      }
  return OK;
}

/**
 * nsp_rmatrix_tril:
 * @A: a #NspRMatrix 
 * @k:  an integer
 * 
 * A=Tril(A)
 **/

int nsp_rmatrix_tril(NspRMatrix *A, int k)
{
  const doubleC zeroC = {0.0,0.0};
  int i,j;
  nsp_rational *Aj, loc;
  int j0 = Max(0,k+1);
  Aj= &A->S[j0*A->m];
  for ( j = j0; j < A->n ; j++, Aj += A->m )
    for ( i = 0 ; i < Min(A->m,j-k) ; i++)
      {
	if ( (loc = nsp_basic_to_rational(&zeroC, A->rc_type)) == NULL) 
	  return FAIL;
	Aj[i] = loc;
      }
  return OK;
}

#if 0
/**
 * nsp_rational_mult_fft:
 * @a: a nsp_rational 
 * @b: a nsp_rational
 * 
 * return a new rational @a * @b. The product is 
 * computed with fft. 
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_mult_fft(nsp_rational a,nsp_rational b)
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
 * nsp_rational_mult_std:
 * @a: a nsp_rational 
 * @b: a nsp_rational
 * 
 * return a new rational @a * @b. The product is 
 * computed with standard product. 
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_mult_std(nsp_rational a,nsp_rational b)
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
 * nsp_rational_horner:
 * @R: a #nsp_rational 
 * @b: a #NspMatrix
 * 
 * return a #NspMatrix, with the same size as @b. 
 * element (i,j) of the result is filled with @R(@b(i,j)).
 * 
 * Returns: a new #NspMatrix or %NULL
 **/

NspMatrix *nsp_rational_horner(nsp_rational P,NspMatrix *b)
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
      /* rational is complex */
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
 * nsp_rational_hornerm:
 * @P: a #nsp_rational 
 * @b: a #NspMatrix
 * 
 * return a #NspMatrix, with the same size as @b and equal 
 * to p(b) = p_0 *Id + p_1*b +p_2*b^2..... p_n b^n.
 *
 * Returns: a new #NspMatrix or %NULL
 **/

NspMatrix *nsp_rational_hornerm(nsp_rational P,NspMatrix *b)
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
  if ( coef != NULL)  nsp_matrix_destroy(coef);
  return res;
 err:
  if ( coef != NULL)  nsp_matrix_destroy(coef);
  if ( term != NULL ) nsp_matrix_destroy(term);
  if ( res != NULL ) nsp_matrix_destroy(res);
  return NULL;
}

/**
 * nsp_rational_power:
 * @p: a #nsp_rational 
 * @n: an integer 
 * 
 * returns the rationalial p^n using repeated 
 * squaring. 
 * 
 * Returns: a new rational or %NULL
 **/

nsp_rational nsp_rational_power(nsp_rational p,int n)
{
  NspMatrix *P = (NspMatrix *) p;
  NspMatrix *R = NULL;
  NspMatrix *Q = NULL;
  NspMatrix *loc = NULL;
  if ( n == 0 ) 
    {
      doubleC d={1,0};
      if ((loc =nsp_basic_to_rational(&d,p->rc_type)) == (nsp_rational ) 0)
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
  if ((Q = nsp_rational_copy(p))== NULL) 
    return NULL;
  while  ( n > 1 )
    {
      if ( n % 2 ) 
	{
	  if ( R == NULL) 
	    {
	      if ((R = nsp_rational_copy(Q))== NULL) goto err;
	    }
	  else
	    {
	      if ((loc = nsp_rational_mult(R,Q)) == NULL) goto err;
	      nsp_rational_destroy(&R);
	      R=loc;
	    }
	}
      n /= 2;
      if ((loc = nsp_rational_mult(Q,Q)) == NULL) goto err;
      nsp_rational_destroy(&Q);
      Q=loc;
    }
  if ( R != NULL) 
    {
      if ((loc = nsp_rational_mult(Q,R)) == NULL) goto err;
      nsp_rational_destroy(&R);
      nsp_rational_destroy(&Q);
    }
  else 
    {
      loc = Q;
    }
  return loc;
 err:
  if ( Q != NULL) nsp_rational_destroy(&Q);
  if ( R != NULL) nsp_rational_destroy(&R);
  return NULL;
}

/**
 * nsp_rational_add_m:
 * @p: a #nsp_rational
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new rationalial @p + @v.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_add_m(nsp_rational p, void *v, char type)
{
  nsp_rational loc;
  if ((loc = nsp_rational_copy_and_name("pe",p))== NULL) 
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
 * nsp_rational_minus_m:
 * @p: a #nsp_rational
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new rationalial @p - @v.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_minus_m(nsp_rational p, void *v, char type)
{
  nsp_rational loc;
  if ((loc = nsp_rational_copy_and_name("pe",p))== NULL) 
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
 * nsp_rational_mult_m:
 * @p: a #nsp_rational
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new rationalial @p * @v.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_mult_m(nsp_rational p, void *v, char type)
{
  int i;
  nsp_rational loc;
  if ((loc = nsp_rational_copy_and_name("pe",p))== NULL) 
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
 * nsp_rational_div_m:
 * @p: a #nsp_rational
 * @v: pointer to a double or doubleC 
 * @type: type of @v coded in a character
 * 
 * returns in a new rationalial @p / @v.
 * 
 * Returns: a new #nsp_rational or %NULL
 **/

nsp_rational nsp_rational_div_m(nsp_rational p, void *v, char type)
{
  int i;
  nsp_rational loc;
  if ((loc = nsp_rational_copy_and_name("pe",p))== NULL) 
    return NULL;
  if ( loc->den->mn == 0) return loc;
  if ( type == 'c') 
    {
      if (nsp_mat_complexify(loc,0.00) == FAIL ) 
	return NULL;
    }
  if ( loc->rc_type == 'r' ) 
    {
      for ( i = 0 ; i < loc->mn ; i++)
	loc->den->R[i] *= *((double *) v);
    }
  else
    {
      if ( type == 'r')
	{
	  for ( i = 0 ; i < loc->mn ; i++)
	    {
	      loc->den->C[i].r *= *((double *) v);
	      loc->den->C[i].i *= *((double *) v);
	    }
	}
      else
	{
	  doubleC x= * (doubleC *) v;
	  for ( i = 0 ; i < loc->mn ; i++)
	    nsp_mult_cc(&loc->den->C[i],&x,&loc->C[i]);
	}
    }
  return loc;
}
#endif


/**
 * nsp_cells_to_rmatrix:
 * @C: a #NspCells of size mxn
 * 
 * returns a mxn #NspRMatrix. Each element of the 
 * cell @C1 gives the coefficient of 
 * the corresponding polynomial in the returned #NspPMatrix.
 * 
 * Returns: a #NspPMatrix object or %NULL
 **/

NspRMatrix *nsp_cells_to_rmatrix(const char *name, NspCells *C1, NspCells *C2)
{
  int i = 0 ,j = 0, k;
  NspRMatrix *loc;
  if ((loc =nsp_rmatrix_create(name,C1->m,C1->n,NULL,-1,NULL,'u',1))== NULL) return(NULL);
  for ( i= 0 ; i < C1->mn;i++)
    {
      nsp_rational rat;
      loc->S[i]= NULL;
      if ((rat = malloc(sizeof(struct _nsp_rational))) == NULL) goto bug;
      rat->num = rat->den = NULL;
      loc->S[i]= rat;
    }
  for ( i= 0 ; i < C1->mn;i++)
    {
      if ( IsMat(C1->objs[i]) )
	{
	  if ((loc->S[i]->num = nsp_polynom_copy_and_name("pe",(NspMatrix *)C1->objs[i]))== NULLPOLY )
	    goto bug;
	  /* be sure that polynom is expanded */
	  if ((loc->S[i]->num = Mat2double(loc->S[i]->num)) == NULLPOLY) 
	    goto bug;
	}
      else
	{
	  Scierror("Error: object stored at indice %d of a cell is not a Matrix \n",i);
	  goto bug;
	}
    }
  for ( j= 0 ; j < C2->mn;j++)
    {
      if ( IsMat(C2->objs[j]) )
	{
	  if ((loc->S[j]->den = nsp_polynom_copy_and_name("pe",(NspMatrix *)C2->objs[j]))== NULLPOLY )
	    goto bug;
	  /* be sure that polynom is expanded */
	  if ((loc->S[j]->den = Mat2double(loc->S[j]->den)) == NULLPOLY) 
	    goto bug;
	}
      else
	{
	  Scierror("Error: object stored at indice %d of a cell is not a Matrix \n",i);
	  goto bug;
	}
    }
  return(loc);
 bug:
  for ( k = 0 ; k < Max(i,j); k++) 
    {
      if ( loc->S[k] != NULL)
	{
	  nsp_matrix_destroy(loc->S[k]->num);
	  nsp_matrix_destroy(loc->S[k]->den);
	  free(loc->S[k]);
	  loc->S[k]=NULL;
	}
    }
  nsp_rmatrix_destroy(loc);
  return NULLRMAT;
}

int nsp_polynoms_simp(NspMatrix *P,NspMatrix *Q)
{
  NspMatrix *Work;
  int err=0;
  int nd = Max(P->mn -1,0);
  int dd = Max(Q->mn -1,0);
  int imax = Max(nd,dd) + 1;
  int Work_size = 2 * (nd + dd) + Min(nd, dd) + 10 * imax + 3 * imax * imax + 4 +1;
  int Nout=0, Dout=0;

  if ( P->mn == 0 || Q->mn == 0) return OK;
  if ( !( P->rc_type ==  'r' && Q->rc_type == 'r')) return OK;
  
  if (( Work = nsp_matrix_create(NVOID,'r', 1, Work_size)) == NULLMAT) return FAIL;
  err = Work_size;
  signal_dpsimp(P->R, &nd, Q->R, &dd, P->R, &Nout, Q->R , &Dout,Work->R, &err);
  nsp_matrix_destroy(Work);
  if (err)
    {
      Scierror("Error: work size too small in simp\n");
      return FAIL;
    }
  nsp_matrix_resize (P, 1, Nout);
  nsp_matrix_resize (Q, 1, Dout);
  return OK;
}

/*
 * routines for output of rationalial matrices 
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
  for ( i = 0 ; i < ((NspRMatrix *) M)->mn ; i++) 
    if ( ((NspRMatrix *) M)->S[i]->num->rc_type == 'c' ||
	 ((NspRMatrix *) M)->S[i]->den->rc_type == 'c'
	 ) 
      {
	type = 'c'; break;
      }
  return type;
}

/* rmatrix specific code */

static int Mp_any_element_is_negative (const void *M)
{
  int sign=0,i;
  for ( i = 0 ; i < ((NspRMatrix *) M)->mn ; i++ ) 
    {
      sign = nsp_matrix_any_element_is_negative(((NspRMatrix *) M)->S[i]->num);
      if ( sign==1) break;
      sign = nsp_matrix_any_element_is_negative(((NspRMatrix *) M)->S[i]->den);
      if ( sign==1) break;
    }
  return sign;
}

/* code for rationalial matrix  **/

static int Mp_any_element_is_inf_or_nan (const void *M)
{
  int inf_or_nan=0,i;
  for ( i = 0 ; i < ((NspRMatrix *)M)->mn ; i++ ) 
    {
      inf_or_nan = nsp_matrix_any_element_is_inf_or_nan (((NspRMatrix *)M)->S[i]->num);
      if ( inf_or_nan ==1 ) break;
      inf_or_nan = nsp_matrix_any_element_is_inf_or_nan (((NspRMatrix *)M)->S[i]->den);
      if ( inf_or_nan ==1 ) break;
    }
  return inf_or_nan;
}

/* code for rationalial matrix **/

static int Mp_all_elements_are_int_or_inf_or_nan (const void *M)
{
  int i, int_or_inf_or_nan=0;
  for ( i = 0 ; i < ((NspRMatrix *)M)->mn ; i++ ) 
    {
      int_or_inf_or_nan = nsp_matrix_all_elements_are_int_or_inf_or_nan(((NspRMatrix *)M)->S[i]->num);
      if ( int_or_inf_or_nan == 0) break;
      int_or_inf_or_nan = nsp_matrix_all_elements_are_int_or_inf_or_nan(((NspRMatrix *)M)->S[i]->den);
      if ( int_or_inf_or_nan == 0) break;
    }
  return int_or_inf_or_nan;
}

/* code for rationalial matrix **/

static void Mp_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  int i;
  nsp_matrix_pr_min_max_internal (((NspRMatrix*)M)->S[0]->num,'r',dmin,dmax);
  for ( i = 0 ; i < ((NspRMatrix*)M)->mn ; i++ ) 
    {
      double max1,min1;
      nsp_matrix_pr_min_max_internal (((NspRMatrix*)M)->S[i]->num,'r',&min1,&max1);
      if ( max1 > *dmax ) *dmax=max1;
      if ( min1 < *dmin ) *dmin=min1;
      nsp_matrix_pr_min_max_internal (((NspRMatrix*)M)->S[i]->den,'r',&min1,&max1);
      if ( max1 > *dmax ) *dmax=max1;
      if ( min1 < *dmin ) *dmin=min1;
    }
}

/* Polynomial Matrix */

static void Mp_set_format(nsp_num_formats *fmt,NspRMatrix *M)
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

extern  int pr_poly (nsp_num_formats *fmt,const char *vname,NspMatrix *m, int fw, int length, int do_print);
#ifndef POLY_EXP 
extern void pr_poly_exp  (NspMatrix *m, int fw, int length);
#endif 
static int pr_bar (int length, int do_print);

static int nsp_rmatrix_print_internal (nsp_num_formats *fmt,NspRMatrix *M, int indent)
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
  /* get one format for all rationals **/ 
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
      Iloc[j]= pr_poly(fmt,M->var,M->S[j*M->m]->num,fw,0,FALSE);
      for ( i = 1 ; i < M->m ; i++) 
	{
	  int size = pr_poly(fmt,M->var,M->S[i+j*M->m]->num,fw,0,FALSE);
	  if ( Iloc[j] < size ) Iloc[j]= size;
	}
      for ( i = 0 ; i < M->m ; i++) 
	{
	  int size = pr_poly(fmt,M->var,M->S[i+j*M->m]->den,fw,0,FALSE);
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
		  Sciprintf("  ");
		pr_poly_exp ( M->S[i+(M->m)*j]->num, fw,Iloc[j]);
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
		  Sciprintf("  ");
		pr_poly (fmt,M->var, M->S[i+(M->m)*j]->num, fw,Iloc[j], TRUE);
	      }
	    if ( j == nc  ) 
	      Sciprintf(" |\n",j,nc-1);
	    else
	      Sciprintf("\n",j,nc-1);
	    /* bar */
	    for ( j = col; j < lim; j++)
	      {
		if ( j == 0 )
		  {
		    nsp_pr_white(indent);Sciprintf(" |");
		  }
		else
		  Sciprintf(", ");
		pr_bar (Iloc[j], TRUE);
	      }
	    if ( j == nc  ) 
	      Sciprintf(" |\n",j,nc-1);
	    else
	      Sciprintf("\n",j,nc-1);
	    /* den */
#ifndef  POLY_EXP
	    for ( j = col; j < lim; j++)
	      {
		if (j == 0 )
		  {
		    nsp_pr_white(indent);Sciprintf(" |");
		  }
		else
		  Sciprintf("  ");
		pr_poly_exp ( M->S[i+(M->m)*j]->den, fw,Iloc[j]);
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
		  Sciprintf("  ");
		pr_poly (fmt,M->var, M->S[i+(M->m)*j]->den, fw,Iloc[j], TRUE);
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

/**
 * pr_bar:
 * @fmt: 
 * @m: 
 * @fw: 
 * @length: length to be used. if positive it gives 
 *     the length that pr_poly should fill, completion by 
 *     white spaces is required.
 * 
 * print a polynom. 
 **/

static int pr_bar (int length, int do_print)
{
  // unsigned char str[]={0xE2, 0x80 , 0x95 ,0};
  int i;
  if ( length > 0 && do_print)
    {
      for ( i = 0 ; i < length ; i++)
	{
	  Sciprintf("%s","-");
	}
    }
  return Max(length,0);
}

