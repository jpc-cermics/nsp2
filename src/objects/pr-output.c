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

#include "nsp/math.h"
#include "nsp/pr-output.h"
#include "nsp/user-prefs.h"
#include "nsp/cnumeric.h"
#include "nsp/sciio.h"

/*
 * routines for output of Scilab Objects 
 * All this routines use the 
 * Sciprintf function to print data on current output 
 *           this function can be changed See SciOI.c 
 */

/* Current format string for real numbers and the real part of complex */
/*numbers. */
static char curr_real_fmt[128] = "%f";
static int  curr_real_fw = 0;

/*Current format string for the imaginary part of complex numbers. */
static char curr_imag_fmt[128] = "%f";
static int  curr_imag_fw = 0;

/*Nonzero means don't do any fancy formatting. */
static int free_format = 0;

/*Nonzero means print plus sign for nonzero, blank for zero. */
static int plus_format = 0;

/*Nonzero means always print like dollars and cents. */
static int bank_format = 0;

/*Nonzero means always print in latex syntax */
static int latex_format = 0;

/* used with latex_format to add the fact that output is for texmacs */
static int latex_texmacs_format = 0;

/*Nonzero means use an e format. */
static int print_e = 0;

/*Nonzero means print E instead of e for exponent field. */
static int print_big_e = 0;

static void pr_white (int fw);
static int  poly_size (int fw, int length);
static void pr_poly  ( NspMatrix *m, int fw, int length);
static void pr_poly_exp  (NspMatrix *m, int fw, int length);

/*
 * Utility Functions used to get elements 
 * of strutures (NspMatrix SpMatrix ....)
 * in a structure independant way.
 * In the way iterator works. 
 * 
 * Init function are used for initialization 
 * Next : used to get next value until end 
 */

/* matrix case */

typedef  int (*Mnext) (const void *,double *,doubleC *);
typedef  char (*Minit) (const void *);

static int mat_ind=0;

static char MatInit(const void *M)
{
  mat_ind = 0;
  return ( (NspMatrix *) M)->rc_type;
}

static int MatNext(const void *M, double *r, doubleC *c)
{
  if ( mat_ind == ((NspMatrix *) M)->mn ) return 0;
  switch (((NspMatrix *) M)->rc_type) 
    {
    case 'r' : *r = ((NspMatrix *) M)->R[mat_ind++];break;
    case 'c' : *c = ((NspMatrix *) M)->C[mat_ind++];break;
    }
  return 1;
}

/* sparse matrix case */

static int sp_mat_ind_row =0;
static int sp_mat_ind_col =0;

static char SpInit(const void *M)
{
  sp_mat_ind_row = -1;
  sp_mat_ind_col = 0;
  return ( (NspSpMatrix *) M)->rc_type;
}

static int SpNext(const void *M, double *r, doubleC *c)
{
  const NspSpMatrix *Sp= M;
  if ( sp_mat_ind_row == -1 ) 
    {
      /* Return first a zero value **/
      switch (Sp->rc_type) 
	{
	case 'r' : *r = 0.00;break;
	case 'c' : c->r = c->i = 0.00;break;
	}
      sp_mat_ind_row++;
      return 1;
    }
  /* Now return the non nul elements **/
  if ( sp_mat_ind_row == Sp->m) return 0;
  /* we still have elements on the current line **/
  if ( sp_mat_ind_col < Sp->D[sp_mat_ind_row]->size )
    {
      switch (Sp->rc_type) 
	{
	case 'r' : *r = Sp->D[sp_mat_ind_row]->R[sp_mat_ind_col];break;
	case 'c' : *c = Sp->D[sp_mat_ind_row]->C[sp_mat_ind_col];break;
	}
      sp_mat_ind_col++;
      return 1;
    }
  else 
    {
      /* find next nonempty row **/
      while (1) 
	{
	  sp_mat_ind_row++;
	  if ( sp_mat_ind_row >= Sp->m) return (0);
	  if ( Sp->D[sp_mat_ind_row]->size != 0) break;
	}
      /* return first non nul element on the row **/
      sp_mat_ind_col =0 ;
      switch (Sp->rc_type) 
	{
	case 'r' : *r = Sp->D[sp_mat_ind_row]->R[sp_mat_ind_col];break;
	case 'c' : *c = Sp->D[sp_mat_ind_row]->C[sp_mat_ind_col];break;
	}
      sp_mat_ind_col++;
      return 1;
    }
  return 1;
}

/* Polynomial matrix */

static int mp_ind=0;

static char MpInit(const void *M)
{
  mp_ind = 0;
  return ( (NspPMatrix *) M)->rc_type;
}

/*
 * int XX_any_element_is_negative(M,Init,Next)
 * check if real(a) < 0 
 * only the real part of matrix a is considered 
 *  int M_any_element_is_negative (M) 
 *  int Sp_any_element_is_negative (M) 
 */

/* generic code */

static int gen_any_element_is_negative(const void *M, Minit Init, Mnext Next)
{
  double r;
  doubleC c;
  char type = Init(M);
  switch (type)
    {
    case 'r' : 
      while ( Next(M,&r,&c) != 0) 
	{ 
	  if ( r < 0.0 ) return 1; 
	} 
      break;
    case 'c' :
      while ( Next(M,&r,&c) != 0) 
	{ if ( c.r < 0.0 ) return 1; }
      break;
    }
  return 0;
}

/* NspMatrix specific code **/

int M_any_element_is_negative (const void *M)
{
  return gen_any_element_is_negative(M,MatInit,MatNext);
}

/* Sparse NspMatrix specific code **/

int Sp_any_element_is_negative (const void *M)
{
  return gen_any_element_is_negative(M,SpInit,SpNext);
}

/* Polynomial matrix specific code **/

int Mp_any_element_is_negative (const void *M)
{
  int sign=0,i;
  for ( i = 0 ; i < ((NspPMatrix *) M)->mn ; i++ ) 
    {
      sign = M_any_element_is_negative(((NspPMatrix *) M)->S[i]);
      if ( sign==1) break;
    }
  return sign;
}

/*
 * int XX_any_element_is_inf_or_nan (M,Init,Next)
 * Checks if NspMatrix contains Inf or Nan 
 * Real or complex matrix 
 */

static int gen_any_element_is_inf_or_nan (const void *M, Minit Init, Mnext Next)
{
  double r;
  doubleC c;
  char type = Init(M);
  switch (type)
    {
    case 'r' :
      while ( Next(M,&r,&c) != 0) 
	{ 
	  if (isinf (r) || isnan (r)) return 1; 
	} 
      break;
    case 'c':
      while ( Next(M,&r,&c) != 0) 
	{ 
	  if (nsp_isinf_c(&c) || nsp_isnan_c(&c)) return 1; 
	} 
      break;
    }
  return 0;
}

/* code for NspMatrix **/

int M_any_element_is_inf_or_nan (const void *M)
{
  return gen_any_element_is_inf_or_nan(M,MatInit,MatNext);
}

/* code for sparse **/

int Sp_any_element_is_inf_or_nan (const void *M)
{
  return gen_any_element_is_inf_or_nan(M,SpInit,SpNext);
}

/* code for polynomial matrix  **/

int Mp_any_element_is_inf_or_nan (const void *M)
{
  int inf_or_nan=0,i;
  for ( i = 0 ; i < ((NspPMatrix *)M)->mn ; i++ ) 
    {
      inf_or_nan = M_any_element_is_inf_or_nan (((NspPMatrix *)M)->S[i]);
      if ( inf_or_nan ==1 ) break;
    }
  return inf_or_nan;
}

/*
 * Checks if Matrix contains Inf or Nan or Ints
 * Real or complex matrix 
 */

static int gen_all_elements_are_int_or_inf_or_nan (const void *M, Minit Init, Mnext Next)
{
  double r;
  doubleC c;
  char type = Init(M);
  switch (type)
    {
    case 'r' :
      while ( Next(M,&r,&c) != 0) 
	{ 
	  if (isinf (r) || anint(r) == r ) 
	    continue ;
	  else 
	    return 0;
	} 
      break;
    case 'c':
      while ( Next(M,&r,&c) != 0) 
	{ 
	  if ( ( isnan (c.r) || anint(c.r) == c.r ) 
	       && ( isnan (c.i) || anint(c.i) == c.i ))
	    continue;
	  else
	    return 0;
	} 
      break;
    }
  return 1;
}

/* code for Matrix **/

int M_all_elements_are_int_or_inf_or_nan (const void *M)
{
  return gen_all_elements_are_int_or_inf_or_nan (M,MatInit,MatNext);
}

/* code for sparse **/

int Sp_all_elements_are_int_or_inf_or_nan (const void *M)
{
  return gen_all_elements_are_int_or_inf_or_nan (M,SpInit,SpNext);
}


/* code for polynomial matrix **/

int Mp_all_elements_are_int_or_inf_or_nan (const void *M)
{
  int i, int_or_inf_or_nan=0;
  for ( i = 0 ; i < ((NspPMatrix *)M)->mn ; i++ ) 
    {
      int_or_inf_or_nan = M_all_elements_are_int_or_inf_or_nan(((NspPMatrix *)M)->S[i]);
      if ( int_or_inf_or_nan == 0) break;
    }
  return int_or_inf_or_nan;
}



/*
 * max(abs(m)) for real matrix 
 * max(abs(real(m))) or max(abs(imag(m))) for complex matrix 
 *        according to the flag value ( flag = 'r' or 'c') 
 *        ignoring isinf and isnan values 
 * same for min 
 */

static void gen_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax, Minit Init, Mnext Next)
{
  double r;
  doubleC c;
  char type = Init(M);
  *dmax =  DBL_MIN;
  *dmin =  DBL_MAX;
  switch (type)
    {
    case 'r' :
      while ( Next(M,&r,&c) != 0) 
	{ 
	  if (isinf (r) || isnan(r) )  continue ;
	  if ( Abs(r) < *dmin ) *dmin = Abs(r);
	  if ( Abs(r) > *dmax ) *dmax = Abs(r);
	} 
      break;
    case 'c':
      if ( flag == 'r' ) 
	{
	  while ( Next(M,&r,&c) != 0) 
	  { 
	    if ( isinf (c.r) || isnan(c.r)) continue;
	    if ( Abs(c.r) < *dmin ) *dmin = Abs(c.r);
	    if ( Abs(c.r) > *dmax ) *dmax = Abs(c.r);
	  } 
	}
      else 
	{
	  while ( Next(M,&r,&c) != 0) 
	  { 
	    if ( isinf (c.i) || isnan(c.i)) continue;
	    if ( Abs(c.i) < *dmin ) *dmin = Abs(c.i);
	    if ( Abs(c.i) > *dmax ) *dmax = Abs(c.i);
	  } 
	}
      break;
    }
}

/* code for Matrix */

void M_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  return gen_pr_min_max_internal (M,flag,dmin,dmax,MatInit,MatNext);
}

/* code for sparse **/

void Sp_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  return gen_pr_min_max_internal (M,flag,dmin,dmax,SpInit,SpNext);
}


/* code for polynomial matrix **/

void Mp_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  int i;
  M_pr_min_max_internal (((NspPMatrix*)M)->S[0],'r',dmin,dmax);
  for ( i = 1 ; i < ((NspPMatrix*)M)->mn ; i++ ) 
    {
      double max1,min1;
      M_pr_min_max_internal (((NspPMatrix*)M)->S[i],'r',&max1,&min1);
      if ( max1 > *dmax ) *dmax=max1;
      if ( min1 < *dmin ) *dmin=min1;
    }
}


/*
 * Computes a common format for displaying all 
 * the numeric data contained in object M
 * The current format is stored in 
 * curr_real_fmt and curr_imag_fmt. 
 * curr_real_fw and curr_imag_fw are used to store their length.
 */

typedef  int (*FT) (const void *);
typedef  void (*pr_mima) (const void *,char,double *,double *);


void gen_set_format (void *M, FT is_neg, FT is_inf_or_nan, pr_mima min_max, FT all_iin, Minit Init)
{
  int sign,inf_or_nan, r_x_max,r_x_min,i_x_max,i_x_min;
  int x_max,x_min,prec,ld, rd;
  double r_max_abs, r_min_abs, i_max_abs,i_min_abs;
  char type ;
  if (free_format) return;
  type = (*Init)(M);
  sign = (*is_neg) (M);
  inf_or_nan = (*is_inf_or_nan)(M);
  if ( type == 'r' )
    {
      (*min_max)(M,'r',&r_min_abs,&r_max_abs);      
      x_max = r_max_abs == 0.0 ? 0 : (int) floor (log10 (r_max_abs) + 1.0);
      x_min = r_min_abs == 0.0 ? 0 : (int) floor (log10 (r_min_abs) + 1.0);
    }
  else 
    {
      (*min_max)(M,'r',&r_min_abs,&r_max_abs);      
      (*min_max)(M,'c',&i_min_abs,&i_max_abs);
      r_x_max = r_max_abs == 0.0 ? 0 : (int) floor (log10 (r_max_abs) + 1.0);
      r_x_min = r_min_abs == 0.0 ? 0 : (int) floor (log10 (r_min_abs) + 1.0);
      i_x_max = i_max_abs == 0.0 ? 0 : (int) floor (log10 (i_max_abs) + 1.0);
      i_x_min = i_min_abs == 0.0 ? 0 : (int) floor (log10 (i_min_abs) + 1.0);
      x_max = r_x_max > i_x_max ? r_x_max : i_x_max;
      x_min = r_x_min > i_x_min ? r_x_min : i_x_min;
    }

  prec = user_pref.output_precision;

  if (bank_format)
    {
      int digits = x_max > x_min ? x_max : x_min;
      curr_imag_fw = curr_real_fw = digits <= 0 ? 4 : digits + 3;
      if (inf_or_nan && curr_imag_fw < 3)
	curr_imag_fw = curr_real_fw = 3;
      /* Only increment the real part field size **/
      curr_real_fw += sign;
      rd = 2;
    }
  else if ( (*all_iin) (M))
    {
      int digits = x_max > x_min ? x_max : x_min;
      curr_imag_fw = curr_real_fw = digits <= 0 ? 1 : digits;
      if (inf_or_nan && curr_imag_fw < 3)
	curr_imag_fw = curr_real_fw = 3;
      curr_real_fw += sign;
      rd = 0;
    }
  else
    {
      int ld_max, rd_max;
      int ld_min, rd_min;
      if (x_max > 0)
	{
	  ld_max = x_max;
	  rd_max = prec - x_max;
	  x_max++;
	}
      else
	{
	  ld_max = 1;
	  rd_max = prec - x_max;
	  x_max = -x_max + 1;
	}

      if (x_min > 0)
	{
	  ld_min = x_min;
	  rd_min = prec - x_min;
	  x_min++;
	}
      else
	{
	  ld_min = 1;
	  rd_min = prec - x_min;
	  x_min = -x_min + 1;
	}

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;
      rd = Abs(rd);
      curr_imag_fw = curr_real_fw = ld + 1 + rd;
      if (inf_or_nan && curr_imag_fw < 3)
	curr_imag_fw = curr_real_fw = 3;
      curr_real_fw += sign;
    }

  if (! bank_format && (curr_real_fw > user_pref.output_max_field_width || print_e))
    {
      int exp_field = 4;
      if (x_max > 100 || x_min > 100) exp_field++;
      curr_imag_fw = curr_real_fw = 1 + prec + exp_field;
      if (inf_or_nan && curr_imag_fw < 3)
	curr_imag_fw = curr_real_fw = 3;
      curr_real_fw += sign;
      if (print_big_e)
	{
	  sprintf (curr_real_fmt, "%%%d.%dE", curr_real_fw, prec - 1);
	  sprintf (curr_imag_fmt, "%%%d.%dE", curr_imag_fw, prec - 1);
	}
      else
	{
	  sprintf (curr_real_fmt, "%%%d.%de", curr_real_fw, prec - 1);
	  sprintf (curr_imag_fmt, "%%%d.%de", curr_imag_fw, prec - 1);
	}
    }
  else
    {
      sprintf (curr_real_fmt, "%%%d.%df", curr_real_fw, rd);
      sprintf (curr_imag_fmt, "%%%d.%df", curr_imag_fw, rd);
    }
  if ( type == 'r' ) curr_imag_fw =0;
}

/* Matrix case **/

void M_set_format(NspMatrix *M)
{
  gen_set_format(M,M_any_element_is_negative,
		 M_any_element_is_inf_or_nan,
		 M_pr_min_max_internal,
		 M_all_elements_are_int_or_inf_or_nan,
		 MatInit);
}


/* Sparse Matrix **/

void Sp_set_format(NspSpMatrix *M)
{
  gen_set_format(M,Sp_any_element_is_negative,
		 Sp_any_element_is_inf_or_nan,
		 Sp_pr_min_max_internal,
		 Sp_all_elements_are_int_or_inf_or_nan,
		 SpInit);
}

/* Polynomial Matrix **/

void Mp_set_format(NspPMatrix *M)
{
  gen_set_format(M,Mp_any_element_is_negative,
		 Mp_any_element_is_inf_or_nan,
		 Mp_pr_min_max_internal,
		 Mp_all_elements_are_int_or_inf_or_nan,
		 MpInit);
}

/*
 * used to print any float with format 
 * fmt 
 * fw : the number of column used by format fmt 
 *      is only used for Inf or Nan to fix the 
 *      field width 
 */

static void pr_any_float (char *fmt, double d, int fw)
{
  if (d == -0.0) d = 0.0;
  if (fmt)
    {
      if (isinf (d))
	{
	  char *s = (d < 0.00) ? "-Inf" : "Inf";
	  if (user_pref.pr_as_read_syntax)
	    {
	      Sciprintf("%%inf");
	    }
	  else
	    {
	      if (fw > 0)
		Sciprintf("%*s",fw, s);
	      else
		Sciprintf(s);
	    }
	}
      else if (isnan (d))
	{
	  if (user_pref.pr_as_read_syntax)
	    {
	      Sciprintf("%%nan");
	    }
	  else
	    {
	      if (fw > 0)
		Sciprintf("%*s",fw,"NaN");
	      else
		Sciprintf("NaN");
	    }
	}
      else
	{
	  /*** XXXXX : can be improved : retirer les 000e+00 a la fin ? **/
	  Sciprintf(fmt,d);	  
	}
    }
  else
    {
      Sciprintf("%f",d);
    }
}

static void pr_float (double d)
{
  pr_any_float (curr_real_fmt,  d, curr_real_fw);
}

static  void pr_imag_float (double d)
{
  pr_any_float (curr_imag_fmt,  d,curr_imag_fw);
}

static  void pr_complex (doubleC c)
{
  double r = c.r;
  pr_float (r);
  if (! bank_format)
    {
      double i = c.i;
      if (i < 0)
	{
	  Sciprintf("-");
	  pr_imag_float (-i);
	}
      else
	{
	  Sciprintf("+");
	  pr_imag_float ( i);
	}
      if (  user_pref.pr_as_read_syntax )
	Sciprintf("*%%i");
      else 
	Sciprintf("i");
    }
  else
    {
      /** FIXME: to be done  **/
    }
}

/*
 * Print routine for empty matrix 
 */

static void print_empty_matrix (int nr, int nc)
{
  Sciprintf("[]\n");
}


/*
 * Printing Scilab Matrix 
 *    nsp_print_internalM
 *    the first function are generic functions
 *    used for other Scilab types 
 */

static void Mij_plus_format(void *m, int i, int j)
{
  NspMatrix *M=m;
  if (M->R[i+(M->m)*j] == 0.0)
    Sciprintf(" ");
  else
    Sciprintf("+");
}

typedef  void (*Mijplus) (void *,int i,int j);

static void M_plus_format(void *m, int nr, int nc, Mijplus F, int indent)
{
  int i,j;
  for ( i = 0; i < nr; i++)
    {
      for ( j = 0; j < nc; j++)
	{
	  if (j == 0) { pr_white(indent) ; Sciprintf("| ");}
	  (*F)(m,i,j);
	}
      Sciprintf(" |\n");
    }
}

static void Mij_float(const void *m, int i, int j)
{
  const NspMatrix *M=m;
  Sciprintf("  ");
  pr_float ( M->R[i+(M->m)*j]);
}

typedef void (*Mijfloat) (const void *,int i,int j);


static void M_as_read_syntax(const void *m, int nr, int nc, int inc,int indent, Mijfloat F)
{
  int i,j;
  for ( i = 0; i < nr; i++)
    {
      int col = 0;
      pr_white(indent);
      while (col < nc)
	{
	  int lim = col + inc < nc ? col + inc : nc;
	  
	  for ( j = col; j < lim; j++)
	    {
	      if (i == 0 && j == 0)
		Sciprintf("[ ");
	      else
		{
		  if (j > col && j < lim)
		    Sciprintf(", ");
		  else
		    Sciprintf("  ");
		}
	      (*F)(m,i,j);
	    }

	  col += inc;

	  if (col >= nc)
	    {
	      if (i == nr - 1)
		Sciprintf(" ]\n");
	      else
		Sciprintf(";\n");
	    }
	  else
	    Sciprintf(" ,\n");
	}
    }
}

static void M_general(void *m, int nr, int nc, int inc, int total_width, int max_width, int winrows, int indent, Mijfloat F)
{
  int i,j;
  int p_rows=0;
  int col;
  for ( col = 0; col < nc; col += inc)
    {
      int lim = col + inc < nc ? col + inc : nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  int num_cols;
	  if (col != 0)  Sciprintf("\n");
	  pr_white(indent);
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
	      if ( imore == 1) return;
	      p_rows=0;
	    }
	  pr_white(indent);Sciprintf(" |");
	  for ( j = col; j < lim; j++)
	    {
	      (*F)(m,i,j);
	    }
	  Sciprintf(" |\n");
	}
    }
}

void nsp_print_internalM (NspMatrix *m, int indent)
{
  int nr = m->m;
  int nc = m->n;
  if ( m->mn == 0) 
    {
      pr_white(indent); Sciprintf("[]\n");
    }
  else if (plus_format && ! user_pref.pr_as_read_syntax )
    {
      M_plus_format(m,nr,nc,Mij_plus_format,indent);
    }
  else
    {
      int inc,column_width,total_width;
      int max_width ,winrows, offset;
      sci_get_screen_size(&winrows,&max_width);
      M_set_format (m);
      /* Sciprintf("prec= %d,Format [%s]\n",
	 user_pref.output_precision,
	 curr_real_fmt); */
      Sciprintf("\n");
      column_width = curr_real_fw + 2;
      offset =  indent + 4; /* 4 = " |...| " */
      total_width = nc * column_width + offset;
      if (user_pref.pr_as_read_syntax) max_width -= 4;
      if (free_format)
	{
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("[\n");
	  /* XXXXXX xxxx Sciprintf(m); **/
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("]");
	  return;
	}
      inc = nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  inc = (max_width -offset) / column_width;
	  if (inc == 0) inc++;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  M_as_read_syntax(m,nr,nc,inc,indent,Mij_float);
	}
      else
	{
	  M_general(m,nr,nc,inc,total_width,max_width,winrows,
		    indent,Mij_float);
	}
    }
}

/*
 * Print any complex Matrix 
 */

void CMij_plus_format(void *m, int i, int j)
{
  NspMatrix *M=m;
  if (M->C[i+(M->m)*j].r == 0.0 && M->C[i+(M->m)*j].i == 0)
    Sciprintf(" ");
  else
    Sciprintf("+");
}


void CMij_float(const void *m, int i, int j)
{
  const NspMatrix *M=m;
  Sciprintf("  ");
  pr_complex ( M->C[i+(M->m)*j]);
}

void nsp_print_internalCM (NspMatrix *cm, int indent)
{
  int nr = cm->m;
  int nc = cm->n;

  if (cm->mn == 0) 
    {
      Sciprintf("[]\n");
    }
  else if (plus_format && ! user_pref.pr_as_read_syntax)
    {
      M_plus_format(cm,nr,nc,CMij_plus_format,indent);
    }
  else
    {
      int column_width,total_width,inc  ;
      int max_width ,winrows ;
      M_set_format(cm); 
      Sciprintf("\n");
      column_width = curr_real_fw + curr_imag_fw;
      column_width += bank_format ? 2 : 7;
      total_width = nc * column_width;

       sci_get_screen_size(&winrows,&max_width);
      if (user_pref.pr_as_read_syntax)
	max_width -= 4;

      if (free_format)
	{
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("[\n");
	  /* Sciprintf(cm); xxx **/
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("]");

	  return;
	}

      inc = nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  inc = max_width / column_width;
	  if (inc == 0)
	    inc++;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  M_as_read_syntax(cm,nr,nc,inc,indent,CMij_float);
	}
      else
	{
	  M_general(cm,nr,nc,inc,total_width,max_width,winrows,
		    indent,CMij_float);
	}
    }
}

/*
 * Printing Scilab Polynomial Matrices 
 */

int nsp_print_internalPM (NspPMatrix *M, int indent)
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
  if (nr == 0 || nc == 0) print_empty_matrix ( nr, nc );
   sci_get_screen_size(&winrows,&max_width);
  /* get one format for all polynoms **/ 
  /* XXXXXX need to write the complex case **/
  Mp_set_format (M);
  fw= curr_real_fw;
  Sciprintf("prec= %d,Format [%s]\n",
	   user_pref.output_precision,
	   curr_real_fmt);
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
		  pr_poly ( M->S[i+(M->m)*j], fw,Iloc[j]);
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

static void pr_poly (NspMatrix *m, int fw, int length)
{
  int i ; 
  for ( i=0 ; i < m->mn ; i++) 
    {
      /* xxxxxxxx : */
      if ( m->rc_type == 'r') 
	{
	  if ( i != 0 && m->R[i] >= 0.00 ) Sciprintf("+");
	  else Sciprintf(" ");
	  pr_any_float (curr_real_fmt, m->R[i], fw);
	}
      if ( i > 0 ) Sciprintf("X");
      if ( i < 10 ) pr_white(1);
      else if ( 10 <= i && i  < 99) pr_white(2); else pr_white(3);

    }
  for ( i= m->mn ; i < length ; i++) 
    {
      pr_white(fw);
      if ( i < 10 ) pr_white(3);
      else if ( 10 <= i && i  < 99) pr_white(4); else pr_white(5);
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
	  pr_white(fw+2);
	}
      if ( i > 0 ) Sciprintf("%d",i);
    }
  for ( i= m->mn ; i < length ; i++) 
    {
      pr_white(fw);
      if ( i < 10 ) pr_white(3);
      else if ( 10 <= i && i  < 99) pr_white(4); else pr_white(5);
      
    }
}

static void pr_white(int fw)
{
  int i;
  for ( i = 0 ; i < fw ; i++) Sciprintf(" ");
}

/*
 * Printing Scilab String Matrices 
 */

static void SMij_string_as_read(const void *m, int i, int j)
{
  const NspSMatrix *M=m;
  char *c= M->S[i+(M->m)*j];
  Sciprintf("\"");
  while ( *c != '\0') 
    {
      switch (*c) 
	{
	case '\'' :
	case '\"' : 
	  Sciprintf("%s","''");break;
	case '\n' :
	  Sciprintf("%s","\\n");break;
	default: 
	  Sciprintf("%c",*c);
	}
      c++;
    }
  Sciprintf("\"");
}

int nsp_print_internalSM (const NspSMatrix *m, int indent)
{
  int *Iloc;
  int inc,column_width=2,total_width;
  int p_rows=0,col;
  int max_width ,winrows,offset;
  int i,j;
  int nr = m->m;
  int nc = m->n;
  if (nr == 0 || nc == 0) print_empty_matrix ( nr, nc);
   sci_get_screen_size(&winrows,&max_width);
  /* Allocate a table to store each column width **/
  if ((Iloc =nsp_alloc_int(m->n)) == (int*) 0) return(FAIL);
  /* set Iloc[j] to the max len of column j **/
  for ( j=0 ; j < m->n ; j++ )
    {
      Iloc[j]=strlen(m->S[j*m->m]);
      for ( i = 1 ; i < m->m ; i++) 
	{
	  if ( Iloc[j] < (int) strlen(m->S[i+j*m->m]) ) Iloc[j]= strlen(m->S[i+j*m->m]);
	}
    }
  Sciprintf("\n");
  /* compute the necessary width **/
  total_width=0;
  for ( j=0 ; j < m->n ; j++) 
    {
      column_width = Iloc[j] + 2;
      total_width +=  column_width;
    }
  inc = nc;
  offset =  indent + 4; /* 4 = " |...| " */
  if (total_width > max_width && user_pref.split_long_rows)
    {
      inc = (max_width -offset) / column_width;
      if (inc == 0) inc++;
    }

  if ( user_pref.pr_as_read_syntax )
    {
      M_as_read_syntax(m,nr,nc,inc,indent,SMij_string_as_read);
      return 0;
    }
  col=0;
  while ( col < nc )
    {
      int lim,num_cols,t_width;
      inc=0;
      t_width = 0;
      for ( j= col ; j < m->n ; j++) 
	{
	  t_width +=  Iloc[j]+2;
	  if ( t_width < max_width) inc++;
	  else break;
	}
      if (inc == 0)	inc++;
      lim = col + inc < nc ? col + inc : nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  if (col != 0)  Sciprintf("\n");
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
	  if (  lim - col == 1 &&  t_width > max_width ) 
	    {
	      Sciprintf("Must try to cut the column \n");
	    }
	  else 
	  */
	    {
	      for ( j = col; j < lim; j++)
		{
		  Sciprintf("  ");
		  Sciprintf("%s",m->S[i+(m->m)*j]);
		  pr_white(Iloc[j]-strlen(m->S[i+(m->m)*j]));
		}
	      Sciprintf("\n");
	    }
	}
      col += inc;
    }
  FREE(Iloc);
  return(OK);
}

/*
 * Printing Scilab Boolean  Matrices 
 */

static void BMij_plus_format(void *m, int i, int j)
{
  NspBMatrix *M=m;
  if (M->B[i+(M->m)*j] == FALSE ) 
    Sciprintf(" ");
  else
    Sciprintf("+");
}

static void BMij_as_read(const void *m, int i, int j)
{
  const NspBMatrix *M=m;
  Sciprintf(" ");
  Sciprintf("%%%c",M->B[i+(M->m)*j]==TRUE ? 't' :'f');
}

static void BMij(const void *m, int i, int j)
{
  const NspBMatrix *M=m;
  Sciprintf(" ");
  Sciprintf("%c",M->B[i+(M->m)*j]==TRUE ? 'T' :'F');
}


void nsp_print_internalBM (NspBMatrix *cm, int indent)
{
  int nr = cm->m;
  int nc = cm->n;
  if (plus_format && ! user_pref.pr_as_read_syntax)
    {
      M_plus_format(cm,nr,nc,BMij_plus_format,indent);
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
	  M_as_read_syntax(cm,nr,nc,inc,indent,BMij_as_read);
	}
      else
	{
	  M_general(cm,nr,nc,inc,total_width,max_width,winrows,
		    indent,BMij);
	}
    }
}


/*
 * Printing Scilab Sparse Matrix 
 */

/* Sparse Matrix with + format : both real and complex cases **/

static void SpM_plus_format(NspSpMatrix *Sp, int indent)
{
  int i,j;
  for ( i = 0; i < Sp->m; i++)
    {
      int col=0;
      SpRow *Ri = Sp->D[i];
      pr_white(indent) ; Sciprintf("| ");
      for ( j = 0; j < Ri->size ; j++)
	{
	  if ( col < Ri->size && j == Ri->J[col] ) 
	    {
	      Sciprintf("+");col++;
	    }
	  else 
	    {
	      Sciprintf(" ");
	    }
	}
      Sciprintf(" |\n");
    }
}

static void SpM_general(NspSpMatrix *Sp, int indent)
{
  int i,j;
  switch ( Sp->rc_type ) 
    {
    case 'r' : 
      for ( i = 0; i < Sp->m; i++)
	{
	  SpRow *Ri = Sp->D[i];
	  for ( j = 0; j < Ri->size ; j++)
	    {
	      pr_white(indent) ;Sciprintf("(%d,%d) ",i+1,Ri->J[j]+1);
	      pr_float ( Ri->R[j]);Sciprintf("\n");
	    }
	}
      break;
    case 'c' :
      for ( i = 0; i < Sp->m; i++)
	{
	  SpRow *Ri = Sp->D[i];
	  for ( j = 0; j < Ri->size ; j++)
	    {
	      pr_white(indent) ; Sciprintf("(%d,%d) ",i+1,Ri->J[j]+1);
	      pr_complex( Ri->C[j]);
	      Sciprintf("\n");
	    }
	}
      break;
    }
}

void nsp_print_internalSpM (NspSpMatrix *m, int indent)
{
  if ( m->mn == 0) 
    {
      Sciprintf("[]\n");
    }
  else if (plus_format && ! user_pref.pr_as_read_syntax )
    {
      SpM_plus_format(m,indent);
    }
  else
    {
      Sp_set_format(m);
      Sciprintf("\n");
      if (free_format)
	{
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("free format to be done for sparse [\n");
	  /* XXXXXX xxxx Sciprintf(m); **/
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("]");
	  return;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  Sciprintf("No as read for sparse \n");
	}
      else
	{
	  SpM_general(m,indent);
	}
    }
}




/*
 * Initialize and customization of print formats 
 */

static void init_format_state (void)
{
  free_format = 0;
  plus_format = 0;
  bank_format = 0;
  latex_format = 0;
  latex_texmacs_format = 0;
  print_e = 0;
  print_big_e = 0;
}

static void set_output_prec_and_fw (int prec, int fw)
{
  user_pref.output_precision= prec;
  user_pref.output_max_field_width=fw;
}

void set_format_style (int argc, char **argv)
{
  if (--argc > 0)
    {
      argv++;
      if (*argv[0])
	{
	  if (strcmp (*argv, "short") == 0)
	    {
	      if (--argc > 0)
		{
		  argv++;
		  if (strcmp (*argv, "e") == 0)
		    {
		      init_format_state ();
		      print_e = 1;
		    }
		  else if (strcmp (*argv, "E") == 0)
		    {
		      init_format_state ();
		      print_e = 1;
		      print_big_e = 1;
		    }
		  else
		    {
		      Scierror("format: unrecognized option `short %s'", *argv);
		      return;
		    }
		}
	      else
		init_format_state ();

	      set_output_prec_and_fw (3, 8);
	    }
	  else if (strcmp (*argv, "long") == 0)
	    {
	      if (--argc > 0)
		{
		  argv++;
		  if (strcmp (*argv, "e") == 0)
		    {
		      init_format_state ();
		      print_e = 1;
		    }
		  else if (strcmp (*argv, "E") == 0)
		    {
		      init_format_state ();
		      print_e = 1;
		      print_big_e = 1;
		    }
		  else
		    {
		      Scierror ("format: unrecognized option `long %s'", *argv);
		      return;
		    }
		}
	      else
		init_format_state ();

	      set_output_prec_and_fw (15, 24);
	    }
	  else if (strcmp (*argv, "hex") == 0)
	    Scierror ("format: format state `hex' not implemented yet");
	  else if (strcmp (*argv, "+") == 0)
	    {
	      init_format_state ();
	      plus_format = 1;
	    }
	  else if (strcmp (*argv, "bank") == 0)
	    {
	      init_format_state ();
	      bank_format = 1;
	    }

	  else if (strcmp (*argv, "free") == 0)
	    {
	      init_format_state ();
	      free_format = 1;
	    }
	  else if (strcmp (*argv, "none") == 0)
	    {
	      init_format_state ();
	      free_format = 1;
	    }
	  else if (strcmp (*argv, "compact") == 0)
	    Scierror ("format: format state `compact' not implemented yet");
	  else if (strcmp (*argv, "loose") == 0)
	    Scierror ("format: format state `loose' not implemented yet");
	  else
	    Scierror ("format: unrecognized format state `%s'", *argv);
	}
      else
	Sciprintf ("format [format_state]");
    }
  else
    {
      init_format_state ();
      set_output_prec_and_fw (5, 10);
    }
}







