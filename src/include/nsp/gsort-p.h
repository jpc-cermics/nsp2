#ifndef NSP_INC_SORT 
#define NSP_INC_SORT 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/string.h" 
#include "nsp/object.h" 

/* kept for compatibility */
extern int nsp_gsort(int *xI, double *xD, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord);

typedef enum {sort_g,sort_gs,sort_gm,sort_c,sort_r,sort_lr ,sort_lc ,sort_ldc,sort_ldr, sort_gb,sort_gd} nsp_sort;

extern int nsp_matrix_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir, nsp_sort type);
extern int nsp_matrix_column_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir);
extern int nsp_matrix_row_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir);
extern int nsp_matrix_lexical_column_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir,char mode);
extern int nsp_matrix_lexical_row_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir,char mode);

extern int nsp_smatrix_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir);
extern int nsp_smatrix_column_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir);
extern int nsp_smatrix_row_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir);
extern int nsp_smatrix_lexical_column_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir);
extern int nsp_smatrix_lexical_row_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir);

/* quicksort generic */

extern void nsp_qsort_gen_col_sort_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_col_sort_int(int *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_col_sort_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_int(int *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_lexicol_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_lexicol_int(int *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_lexicol_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_lexirow_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_lexirow_int(int *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_lexirow_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_row_sort_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_row_sort_int(int *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_row_sort_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

/* quicksort  */

extern void nsp_qsort_int(int *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_double(double *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_nsp_string(nsp_string *a,int *tab, int flag, int n,char dir);

/* quicksort B Pincon   */

extern void nsp_qsort_bp_int(int x[], int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_double(double x[], int n, int p[],int flag,char dir );

/* mergesort */

extern int nsp_mergesort_int(int *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_double(double *a,int *p,int flag, int fromIndex, int toIndex,char dir);

extern void nsp_qsort_stable_incr_int(int *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_decr_int(int *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_incr_double(double *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_decr_double(double *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_incr_nsp_string(nsp_string *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_decr_nsp_string(nsp_string *a,int *index,int flag, int fromIndex, int toIndex,char dir);

#endif

