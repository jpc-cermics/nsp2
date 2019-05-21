#ifndef NSP_INC_PR_OUTPUT
#define NSP_INC_PR_OUTPUT

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>
#include <nsp/objectf.h>

typedef struct _nsp_num_formats nsp_num_formats ;

struct _nsp_num_formats {
  char curr_real_fmt[128];/* Current format string for real numbers and the real part of complex numbers. */
  int curr_real_fw; 
  char curr_imag_fmt[128];/*Current format string for the imaginary part of complex numbers. */
  int  curr_imag_fw;  /*Nonzero means don't do any fancy formatting. */
  int free_format;   /*Nonzero means print plus sign for nonzero, blank for zero. */
  int plus_format;   /*Nonzero means always print like dollars and cents. */
  int bank_format;   /*Nonzero means always print in latex syntax */
  int latex_format;  /* used with latex_format to add the fact that output is for texmacs */
  int latex_texmacs_format;  /*Nonzero means use an e format. */
  int print_e;   /*Nonzero means print E instead of e for exponent field. */
  int print_big_e;
};

typedef  int (*nsp_it_next) (const void *,double *,doubleC *,int *);
typedef  char (*nsp_it_init) (const void *,int *);

extern int gen_any_element_is_negative(const void *M, nsp_it_init Init, nsp_it_next Next);
extern int gen_any_element_is_inf_or_nan (const void *M, nsp_it_init Init, nsp_it_next Next);
extern int gen_all_elements_are_int_or_inf_or_nan (const void *M, nsp_it_init Init, nsp_it_next Next);
extern void gen_pr_min_max_internal(const void *M, char flag, double *dmin, double *dmax, nsp_it_init Init, nsp_it_next Next);

typedef void (*Mijfloat) (const nsp_num_formats *fmt,const void *,int i,int j);

extern void nsp_gen_matrix_as_read_syntax(const nsp_num_formats *fmt,
				   const void *m, int nr, int nc, int inc,int indent, Mijfloat F);

typedef  int (*it_gen_f) (const void *);
typedef  void (*pr_mima) (const void *,char,double *,double *);

void gen_set_format (nsp_num_formats *fmt,void *M, it_gen_f is_neg, it_gen_f is_inf_or_nan, pr_mima min_max, 
		     it_gen_f all_iin, nsp_it_init Init);

extern void nsp_pr_float (const nsp_num_formats *fmt,double d, int latex );
extern void nsp_pr_imag_float (const nsp_num_formats *fmt,double d, int latex);
extern void nsp_pr_complex (const nsp_num_formats *fmt,doubleC c, int latex);
extern void nsp_pr_white(int fw);
extern void nsp_print_empty_matrix (int nr, int nc);
extern void nsp_print_double(double d, int latex);
extern void nsp_pr_any_float (const char *fmt, double d, int fw);
extern void nsp_pr_any_float_latex (const char *fmt, double d, int fw);
extern int  nsp_pr_any_float_vs (const char *fmt, double d, int fw, int do_print);
extern void nsp_init_pr_format (nsp_num_formats *fmt);
extern void nsp_matrix_set_format(nsp_num_formats *fmt,NspMatrix *M);
extern void nsp_print_string_as_read(const char *str,char string_delim);

extern int nsp_real_matrix_print_internal(nsp_num_formats *fmt,NspMatrix *m, int indent);
extern int nsp_complex_matrix_print_internal (nsp_num_formats *fmt,NspMatrix *cm, int indent);

typedef  void (*Mijplus) (const void *,int i,int j);
extern void nsp_matrix_plus_format(const void *m, int nr, int nc, Mijplus F, int indent);
extern int nsp_matrix_general(const nsp_num_formats *fmt,void *m, int nr, int nc, int inc, int total_width, int max_width, int winrows, int indent, Mijfloat F);

extern void nsp_set_format(int output_max_field_width, int output_precision,int e);
extern void nsp_get_format(int *output_max_field_width, int *output_precision,int *e);

#endif 

