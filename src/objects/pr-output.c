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
 *
 * A set of generic routines for computing formats 
 * used in formating numerical matrices 
 * 
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

user_preferences user_pref= {
  10 ,/* output_max_field_width; */
  3 ,/* output_precision;	*/
  1 ,/* split_long_rows;		*/
  0 ,/* print empty dimensions   */
  0 ,/* pr_as_read_syntax;	*/
  INT_MAX ,/* stop printing at given depth for recursive objects */
  FALSE ,/* flag for list display */
  print_std  ,/* std, latex or latex tables */
  TRUE /* only print if active is true */
};


/**
 * gen_any_element_is_negative:
 * @M: a void pointer 
 * @Init: initialize iterator 
 * @Next: get next element 
 * 
 * generic function which checks if any element of given object @M is negative. 
 * @Init function is used for initialization of iteration
 * and @Next is used to get next value until end of object. Returns 1 if 
 * a negative element is found.
 * 
 * Returns: 1 or 0.
 **/

int gen_any_element_is_negative(const void *M, nsp_it_init Init, nsp_it_next Next)
{
  int work[2];
  double r;
  doubleC c;
  char type = Init(M,work);
  switch (type)
    {
    case 'r' : 
      while ( Next(M,&r,&c,work) != 0) 
	{ 
	  if ( r < 0.0 ) return 1; 
	} 
      break;
    case 'c' :
      while ( Next(M,&r,&c,work) != 0) 
	{ if ( c.r < 0.0 ) return 1; }
      break;
    }
  return 0;
}



/**
 * gen_any_element_is_inf_or_nan:
 * @M: a void pointer 
 * @Init: initialize iterator 
 * @Next: get next element 
 *
 * generic function which checks if any element of given object @M is inf or nan.
 * @Init function is used for initialization of iteration
 * and @Next is used to get next value until end of object. Returns 1 if 
 * an inf or nan is found.
 * 
 * Returns: 1 or 0.
 **/

int gen_any_element_is_inf_or_nan (const void *M, nsp_it_init Init, nsp_it_next Next)
{
  int work[2];
  double r;
  doubleC c;
  char type = Init(M,work);
  switch (type)
    {
    case 'r' :
      while ( Next(M,&r,&c,work) != 0) 
	{ 
	  if (isinf (r) || isnan (r)) return 1; 
	} 
      break;
    case 'c':
      while ( Next(M,&r,&c,work) != 0) 
	{ 
	  if (nsp_isinf_c(&c) || nsp_isnan_c(&c)) return 1; 
	} 
      break;
    }
  return 0;
}


/**
 * gen_all_elements_are_int_or_inf_or_nan:
 * @M: a void pointer 
 * @Init: initialize iterator 
 * @Next: get next element 
 *
 * generic function which checks if all element of given object @M are inf or nan or 
 * can be casted to integers. 
 * @Init function is used for initialization of iteration
 * and @Next is used to get next value until end of object. Returns 1 in case of 
 * success.
 * 
 * Returns: 1 or 0.
 **/

int gen_all_elements_are_int_or_inf_or_nan (const void *M, nsp_it_init Init, nsp_it_next Next)
{
  int work[2];
  double r;
  doubleC c;
  char type = Init(M,work);
  switch (type)
    {
    case 'r' :
      while ( Next(M,&r,&c,work) != 0) 
	{ 
	  if (isinf (r) || anint(r) == r ) 
	    continue ;
	  else 
	    return 0;
	} 
      break;
    case 'c':
      while ( Next(M,&r,&c,work) != 0) 
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

/**
 * gen_pr_min_max_internal:
 * @M: a void pointer 
 * @flag: 
 * @dmin: 
 * @dmax: 
 * @Init: initialize iterator 
 * @Next: get next element 
 *
 * generic function which returns in @dmin and @dmax 
 * max(abs(m)) for real matrix and max(abs(real(m))) or max(abs(imag(m))) for complex matrix 
 * according to the flag value ( flag = 'r' or 'c'). inf and nan values are ignored. 
 * @Init function is used for initialization of iteration
 * and @Next is used to get next value until end of object.
 * this function is used to find a common format for displaying numbers, exact zeros are ignored 
 * when computing @dmin and @dmax.
 *
 **/

void gen_pr_min_max_internal(const void *M, char flag, double *dmin, double *dmax, nsp_it_init Init, nsp_it_next Next)
{
  int work[2],zero = FALSE;
  double r;
  doubleC c;
  char type = Init(M,work);
  *dmax =  DBL_MIN;
  *dmin =  DBL_MAX;
  switch (type)
    {
    case 'r' :
      while ( Next(M,&r,&c,work) != 0) 
	{ 
	  if (isinf (r) || isnan(r) )  continue ;
	  if ( r == 0.0 ) {zero = TRUE ; continue;}
	  if ( Abs(r) < *dmin ) *dmin = Abs(r);
	  if ( Abs(r) > *dmax ) *dmax = Abs(r);
	} 
      break;
    case 'c':
      if ( flag == 'r' ) 
	{
	  while ( Next(M,&r,&c,work) != 0) 
	    { 
	      if ( isinf (c.r) || isnan(c.r)) continue;
	      if ( c.r == 0.0 ) {zero = TRUE ; continue;}
	      if ( Abs(c.r) < *dmin ) *dmin = Abs(c.r);
	      if ( Abs(c.r) > *dmax ) *dmax = Abs(c.r);
	    } 
	}
      else 
	{
	  while ( Next(M,&r,&c,work) != 0) 
	    { 
	      if ( isinf (c.i) || isnan(c.i)) continue;
	      if ( c.i == 0.0 ) {zero = TRUE ; continue;}
	      if ( Abs(c.i) < *dmin ) *dmin = Abs(c.i);
	      if ( Abs(c.i) > *dmax ) *dmax = Abs(c.i);
	    } 
	}
      break;
    }
  if ( zero == TRUE ) 
    {
      /* if a zero was found and ignored check */
      if ( *dmax ==   DBL_MIN ) *dmax=0.0;
      if ( *dmin ==   DBL_MAX ) *dmin=0.0;
    }
}

/**
 * gen_set_format:
 * @fmt: a #nsp_num_format structure 
 * @M: NspObject to be printed 
 * @is_neg: function
 * @is_inf_or_nan: function
 * @min_max: function
 * @all_iin: function
 * @Init: function
 * 
 * Computes a common format for displaying all 
 * the numeric data contained in object @M
 * The current format is stored in @fmt fields 
 * curr_real_fmt and curr_imag_fmt. 
 * curr_real_fw and curr_imag_fw are used to store the respective lengthes of the 
 * formats.
 **/

void gen_set_format (nsp_num_formats *fmt,void *M, it_gen_f is_neg, it_gen_f is_inf_or_nan,
                     pr_mima min_max, it_gen_f all_iin, nsp_it_init Init)
{
  double d_epsr=DBL_EPSILON;
  int work[2];
  int sign,inf_or_nan, r_x_max,r_x_min,i_x_max,i_x_min;
  int x_max,x_min,prec,ld, rd,output_max_width;
  double r_max_abs, r_min_abs, i_max_abs,i_min_abs;
  char type ;
  if (fmt->free_format) return;
  type = (*Init)(M,work);
  sign = (*is_neg) (M);
  inf_or_nan = (*is_inf_or_nan)(M);
  if ( type == 'r' )
    {
      (*min_max)(M,'r',&r_min_abs,&r_max_abs);      
      if (r_max_abs == 0.0) r_max_abs = d_epsr;
      if (r_min_abs == 0.0) r_min_abs = d_epsr;
      x_max = r_max_abs == 0.0 ? 0 : (int) floor (log10 (r_max_abs) + 1.0);
      x_min = r_min_abs == 0.0 ? 0 : (int) floor (log10 (r_min_abs) + 1.0);
    }
  else 
    {
      int real_is_zero = FALSE;
      int imag_is_zero = FALSE;
      (*min_max)(M,'r',&r_min_abs,&r_max_abs); 
      if (r_max_abs == 0.0 && r_min_abs == 0.0 ) real_is_zero = TRUE;
      if (r_max_abs == 0.0) r_max_abs = d_epsr;
      if (r_min_abs == 0.0) r_min_abs = d_epsr;     
      (*min_max)(M,'c',&i_min_abs,&i_max_abs);
      if ( i_max_abs == 0.0 && i_min_abs == 0.0 ) imag_is_zero = TRUE;
      if (i_max_abs == 0.0) i_max_abs = d_epsr;
      if (i_min_abs == 0.0) i_min_abs = d_epsr;
      r_x_max = r_max_abs == 0.0 ? 0 : (int) floor (log10 (r_max_abs) + 1.0);
      r_x_min = r_min_abs == 0.0 ? 0 : (int) floor (log10 (r_min_abs) + 1.0);
      i_x_max = i_max_abs == 0.0 ? 0 : (int) floor (log10 (i_max_abs) + 1.0);
      i_x_min = i_min_abs == 0.0 ? 0 : (int) floor (log10 (i_min_abs) + 1.0);
      /* we will use a common format for real and imaginary parts */
      if ( imag_is_zero  ) 
	{
	  x_max = r_x_max;
	  x_min = r_x_min;
	}
      else if ( real_is_zero )
	{
	  x_max = i_x_max;
	  x_min = i_x_min;
	}
      else 
	{
	  x_max = Max(r_x_max, i_x_max);
	  x_min = Min(r_x_min, i_x_min);
	}
    }

  prec = user_pref.output_precision;
  output_max_width = user_pref.output_max_field_width;

  if (fmt->bank_format)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fmt->curr_imag_fw = fmt->curr_real_fw = digits <= 0 ? 4 : digits + 3;
      if (inf_or_nan && fmt->curr_imag_fw < 3)
	fmt->curr_imag_fw = fmt->curr_real_fw = 3;
      /* Only increment the real part field size **/
      fmt->curr_real_fw += sign;
      rd = 2;
    }
  else if ( (*all_iin) (M))
    {
      int digits = x_max > x_min ? x_max : x_min;
      fmt->curr_imag_fw = fmt->curr_real_fw = digits <= 0 ? 1 : digits;
      if (inf_or_nan && fmt->curr_imag_fw < 3)
	fmt->curr_imag_fw = fmt->curr_real_fw = 3;
      fmt->curr_real_fw += sign;
      rd = 0;
    }
  else
    {
      int ld_max, rd_max;
      int ld_min, rd_min;
      if (x_max > 0)
	{
	  ld_max = x_max;
          rd_max = Min(prec, output_max_width - x_max);
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
          rd_min = Min(prec, output_max_width - x_min);
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
      fmt->curr_imag_fw = fmt->curr_real_fw = ld + 1 + rd;
      if (inf_or_nan && fmt->curr_imag_fw < 3)
	fmt->curr_imag_fw = fmt->curr_real_fw = 3;
      fmt->curr_real_fw += sign;
    }

  if (! fmt->bank_format && (fmt->curr_real_fw > user_pref.output_max_field_width || fmt->print_e))
    {
      int exp_field = 4;
      if (x_max > 100 || x_min > 100) exp_field++;
      fmt->curr_imag_fw = fmt->curr_real_fw = 1 + prec + exp_field;
      if (inf_or_nan && fmt->curr_imag_fw < 3)
	fmt->curr_imag_fw = fmt->curr_real_fw = 3;
      fmt->curr_real_fw += sign;
      if (fmt->print_big_e)
	{
	  sprintf (fmt->curr_real_fmt, "%%%d.%dE", fmt->curr_real_fw, prec - 1);
	  sprintf (fmt->curr_imag_fmt, "%%%d.%dE", fmt->curr_imag_fw, prec - 1);
	}
      else
	{
	  sprintf (fmt->curr_real_fmt, "%%%d.%de", fmt->curr_real_fw, prec - 1);
	  sprintf (fmt->curr_imag_fmt, "%%%d.%de", fmt->curr_imag_fw, prec - 1);
	}
    }
  else
    {
      sprintf (fmt->curr_real_fmt, "%%%d.%df", fmt->curr_real_fw, rd);
      sprintf (fmt->curr_imag_fmt, "%%%d.%df", fmt->curr_imag_fw, rd);
    }
  if ( type == 'r' ) fmt->curr_imag_fw =0;
}


/*
 * used to print any float with format 
 * fmt 
 * fw : the number of column used by format fmt 
 *      is only used for Inf or Nan to fix the 
 *      field width 
 */


void nsp_gen_matrix_as_read_syntax(const nsp_num_formats *fmt,
				   const void *m, int nr, int nc, int inc,int indent, Mijfloat F)
{
  int i,j;
  for ( i = 0; i < nr; i++)
    {
      int col = 0;
      nsp_pr_white(indent);
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
	      (*F)(fmt,m,i,j);
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
	    Sciprintf(" ,...\n");
	}
    }
}


/*
 * Print routine for empty matrix 
 */

void nsp_print_empty_matrix (int nr, int nc)
{
  Sciprintf("[]\n");
}

void nsp_pr_any_float (const char *fmt, double d, int fw)
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
	  /* special case for zero as in matlab */
	  if ( d == 0 ) 
	    Sciprintf("%*d",fw,0);
	  else
	    Sciprintf(fmt,d);	  
	}
    }
  else
    {
      Sciprintf("%f",d);
    }
}


void nsp_pr_white(int fw)
{
  int i;
  for ( i = 0 ; i < fw ; i++) Sciprintf(" ");
}

void nsp_pr_float (const nsp_num_formats *fmt,double d)
{
  nsp_pr_any_float (fmt->curr_real_fmt,  d, fmt->curr_real_fw);
}

void nsp_pr_imag_float (const nsp_num_formats *fmt,double d)
{
  nsp_pr_any_float (fmt->curr_imag_fmt,  d,fmt->curr_imag_fw);
}

void nsp_pr_complex (const nsp_num_formats *fmt,doubleC c)
{
  double r = c.r;
  nsp_pr_float (fmt,r);
  if (! fmt->bank_format)
    {
      double i = c.i;
      if ( c.i == 0.0) 
	{
	  nsp_pr_white(fmt->curr_imag_fw+2);
	  return;
	}
      if (i < 0)
	{
	  Sciprintf("-");
	  nsp_pr_imag_float (fmt,-i);
	}
      else
	{
	  Sciprintf("+");
	  nsp_pr_imag_float (fmt, i);
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

void nsp_init_pr_format (nsp_num_formats *fmt)
{
  fmt->free_format = 0;
  fmt->plus_format = 0;
  fmt->bank_format = 0;
  fmt->latex_format = 0;
  fmt->latex_texmacs_format = 0;
  fmt->print_e = 0;
  fmt->print_big_e = 0;
}


void nsp_set_format(int output_max_field_width, int output_precision)
{
  user_pref.output_max_field_width = Max(output_max_field_width,0);
  user_pref.output_precision = Max(output_precision,0);
}









