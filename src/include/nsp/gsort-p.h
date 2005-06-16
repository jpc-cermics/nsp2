#ifndef NSP_INC_SORT 
#define NSP_INC_SORT 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/string.h" 
#include "nsp/object.h" 

int C2F(gsort) (int *xI,double *xD,int *ind,int *iflag,int *m,int *n,nsp_const_string type,nsp_const_string  iord);
void C2F(gsorts) (char **data,int *ind,int *iflag,int *m,int *n,nsp_const_string type,nsp_const_string iord);

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

#endif

