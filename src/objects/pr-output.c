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
 *
 * A set of generic routines for computing formats 
 * used in formating numerical matrices 
 * 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include "nsp/math.h"
#include "nsp/pr-output.h"
#include "nsp/user-prefs.h"
#include "nsp/cnumeric.h"
#include "nsp/sciio.h"

user_preferences user_pref= {
  11 ,/* output_max_field_width; */
  4 ,/* output_precision;	*/
  1 ,/* split_long_rows;		*/
  0 ,/* print empty dimensions   */
  0 ,/* pr_as_read_syntax;	*/
  INT_MAX ,/* stop printing at given depth for recursive objects */
  FALSE ,/* flag for list display */
  print_std  ,/* std, latex or latex tables */
  TRUE, /* only print if active is true */
  TRUE, /* use color by default in print */
  FALSE, /* do not use latex by default in print */
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
  int real_is_zero = FALSE;
  int imag_is_zero = FALSE;
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
  if ( type == 'r' || imag_is_zero ) fmt->curr_imag_fw =0;
}


/*
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

/* print a double in LaTeX format using the numprint 
 * macro 
 */

static void simplify_exponent(char *buf);

void nsp_pr_any_float_latex (const char *fmt, double d, int exclude_one)
{
  if (d == -0.0) d = 0.0;

  if (isinf (d))
    {
      char *s = (d < 0.00) ? "-\\infty" : "+\\infty";
      Sciprintf(s);
    }
  else if (isnan (d))
    {
      Sciprintf("\\mathrm{NaN}");
    }
  else
    {
      if ( d == 0 )
	{
	  Sciprintf("\\numprint{%d}",0);
	}
      else if ( d == 1 )
	{
	  if ( exclude_one == FALSE ) Sciprintf("\\numprint{1}");
	}
      else
	{
	  if ( fmt != NULL )
	    { 
	      char buf[256];
	      sprintf(buf,fmt,d);
	      simplify_exponent(buf);
	      Sciprintf("\\numprint{%s}",buf);	  
	    }
	  else
	    {
	      Sciprintf("\\numprint{%f}",d);
	    }
	}
    }
}

static void simplify_exponent(char *buf)
{
  /* Sciprintf always put a "e" */
  char *s = strstr(buf,"e"), *s_end;
  s_end=s;
  if ( s != NULL)
    {
      s_end +=1;
      s +=1;
      if ( *(s)=='+')
	{
	  /* remove + and leading 0 */
	  s_end +=1;
	  while (*(s_end) == '0') s_end+=1;
	  strcpy(s,s_end);
	  /* remove empty exponent */
	  if (strcmp(s_end,"") == 0) *(s-1)='\0';
	  return;
	}
      if ( *(s)=='-')
	{
	  s_end += 1;
	  s += 1;
	  /* remove leading 0 */
	  while (*(s_end)== '0') s_end +=1;
	  strcpy(s,s_end);
	  /* remove empty exponent */
	  if (strcmp(s_end,"")== 0) *(s-2)='\0';
	}
    }
}


/* print a double using less than width fw when possible 
 * and return the number of spaces used.
 */

int nsp_pr_any_float_vs (const char *fmt, double d, int fw, int do_print)
{
  if (d == -0.0) d = 0.0;
  if (isinf (d))
    {
      char *s = (d < 0.00) ? "-Inf" : "Inf";
      if (user_pref.pr_as_read_syntax)
	{
	  if (do_print) Sciprintf("%%inf");
	  return 4;
	}
      else
	{
	  if (do_print) Sciprintf(s);
	  return strlen(s);
	}
    }
  else if (isnan (d))
    {
      if (user_pref.pr_as_read_syntax)
	{
	  if (do_print)	  Sciprintf("%%nan");
	  return 4;
	}
      else
	{
	  if (do_print)	  Sciprintf("NaN");
	  return 3;
	}
    }
  else
    {
      /* special case for zero as in matlab */
      if ( d == 0 ) 
	{
	  if (do_print)	  Sciprintf("0");
	  return 1;
	}
      else
	{
	  int i;
	  char str[128],*str1;
	  sprintf(str,(fmt) ? fmt: "%f" ,d);
	  if ((str1=strstr(str,"."))!= 0 && strstr(str,"e") == 0 ) 
	    {
	      for ( i = strlen(str1)-1; i >= 0 ; i--)
		{
		  if (str1[i]== '0' ) str1[i]='\0';
		  else break;
		}
	      if (str1[i]== '.') str1[i]='\0';
	    }
	  i=0;
	  while (str[i]==' ' && i < strlen(str)) i++;
	  str1 = str+i;
	  if (do_print)	  Sciprintf("%s",str1);
	  return strlen(str1);
	}
    }
  return 0;
}


void nsp_pr_white(int fw)
{
  int i;
  for ( i = 0 ; i < fw ; i++) Sciprintf(" ");
}

void nsp_pr_float (const nsp_num_formats *fmt,double d, int latex )
{
  if ( latex )
    nsp_pr_any_float_latex (fmt->curr_real_fmt,  d, FALSE);
  else
    nsp_pr_any_float (fmt->curr_real_fmt,  d, fmt->curr_real_fw);
}

void nsp_pr_imag_float (const nsp_num_formats *fmt,double d, int latex) 
{
  if ( latex )
    nsp_pr_any_float_latex  (fmt->curr_imag_fmt,  d, FALSE);
  else
    nsp_pr_any_float (fmt->curr_imag_fmt,  d,fmt->curr_imag_fw);
}

void nsp_pr_complex (const nsp_num_formats *fmt,doubleC c, int latex)
{
  double r = c.r, i = c.i;
  nsp_pr_float (fmt,r,latex);
  if (! fmt->bank_format)
    {
      if ( c.i == 0.0) 
	{
	  nsp_pr_white(fmt->curr_imag_fw+2);
	  return;
	}
      if (i < 0)
	{
	  Sciprintf("-");
	  nsp_pr_imag_float (fmt,-i,latex);
	}
      else
	{
	  Sciprintf("+");
	  nsp_pr_imag_float (fmt, i,latex);
	}
      if (  user_pref.pr_as_read_syntax && latex == FALSE )
	Sciprintf("*%%i");
      else 
	Sciprintf("i");
    }
  else
    {
      /** FIXME: to be done  **/
    }
}

/* print a double : with format computed by nsp */

static char nsp_double_iter_init(const void *M,int *work)
{
  *work = 0;
  return 'r';
}

static int nsp_double_any_element_is_negative (const void *M)
{
  return ( *((double *) M) < 0.0 ) ? 1 : 0 ;
}

static int nsp_double_any_element_is_inf_or_nan (const void *M)
{
  double r = *((double *) M);
  return (isinf (r) || isnan (r)) ? 1 : 0;
}

static int nsp_double_all_elements_are_int_or_inf_or_nan (const void *M)
{
  double r = *((double *) M);
  return (isinf (r)|| isnan (r) || anint(r) == r ) ? 1: 0;
}

static void nsp_double_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  int zero = FALSE;
  double r = *((double *)M);
  *dmax =  DBL_MIN;
  *dmin =  DBL_MAX;
  if ( r == 0.0 ) {zero = TRUE ;}
  if ( Abs(r) < *dmin ) *dmin = Abs(r);
  if ( Abs(r) > *dmax ) *dmax = Abs(r);
  if ( zero == TRUE ) 
    {
      /* if a zero was found and ignored check */
      if ( *dmax ==   DBL_MIN ) *dmax=0.0;
      if ( *dmin ==   DBL_MAX ) *dmin=0.0;
    }
}

static void nsp_double_set_format(nsp_num_formats *fmt,double *M)
{
  gen_set_format(fmt,M,nsp_double_any_element_is_negative,
		 nsp_double_any_element_is_inf_or_nan,
		 nsp_double_pr_min_max_internal,
		 nsp_double_all_elements_are_int_or_inf_or_nan,
		 nsp_double_iter_init);
}

void nsp_print_double(double d, int latex)
{
  nsp_num_formats fmt;
  nsp_init_pr_format (&fmt);
  nsp_double_set_format(&fmt,&d);
  if ( latex )
    nsp_pr_any_float_latex (fmt.curr_real_fmt,  d, FALSE);
  else
    nsp_pr_any_float (fmt.curr_real_fmt,  d, fmt.curr_real_fw);
}

/* initialization */

static int nsp_print_e_def = 0;

void nsp_init_pr_format (nsp_num_formats *fmt)
{
  fmt->free_format = 0;
  fmt->plus_format = 0;
  fmt->bank_format = 0;
  fmt->latex_format = 0;
  fmt->latex_texmacs_format = 0;
  fmt->print_e = nsp_print_e_def;
  fmt->print_big_e = 0;
}

void nsp_set_format(int output_max_field_width, int output_precision,int e)
{
  nsp_print_e_def = e ;
  user_pref.output_max_field_width = Max(output_max_field_width,0);
  user_pref.output_precision = Max(output_precision,0);
}

void nsp_get_format(int *output_max_field_width, int *output_precision,int *e)
{
  *e = nsp_print_e_def;
  *output_max_field_width = user_pref.output_max_field_width;
  *output_precision = user_pref.output_precision;
}









