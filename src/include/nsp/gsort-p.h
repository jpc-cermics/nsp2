#ifndef NSP_INC_SORT 
#define NSP_INC_SORT 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <glib.h> /* for integers */
#include "nsp/string.h" 
#include "nsp/object.h" 

/* kept for compatibility */
extern int nsp_gsort(int *xI, double *xD, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord);

typedef enum {sort_g,sort_gs,sort_gm,sort_c,sort_r,sort_lr ,sort_lc ,sort_ldc,sort_ldr, sort_gb,sort_gd} nsp_sort;
typedef enum {test_sort_g, test_sort_c, test_sort_r, test_sort_lc , test_sort_lr} nsp_test_sort;

extern int nsp_matrix_sort(NspMatrix *A,NspObject **Index,int ind_flag,char dir, nsp_sort type, char ind_type);
extern int nsp_matrix_column_sort(NspMatrix *A,NspObject **Index,int ind_flag,char dir, char ind_type);
extern int nsp_matrix_row_sort(NspMatrix *A,NspObject **Index,int ind_flag,char dir, char ind_type);
extern int nsp_matrix_lexical_column_sort(NspMatrix *A,NspObject **Index,int ind_flag,char dir,char mode, char ind_type);
extern int nsp_matrix_lexical_row_sort(NspMatrix *A,NspObject **Index,int ind_flag,char dir,char mode, char ind_type);

extern int nsp_smatrix_sort(NspSMatrix *A,NspObject **Index,int ind_flag,char dir, int type,char ind_type);
extern int nsp_smatrix_column_sort(NspSMatrix *A,NspObject **Index,int ind_flag,char dir,char ind_type);
extern int nsp_smatrix_row_sort(NspSMatrix *A,NspObject **Index,int ind_flag,char dir,char ind_type);
extern int nsp_smatrix_lexical_column_sort(NspSMatrix *A,NspObject **Index,int ind_flag,char dir,char ind_type);
extern int nsp_smatrix_lexical_row_sort(NspSMatrix *A,NspObject **Index,int ind_flag,char dir,char ind_type);

extern int nsp_imatrix_sort(NspIMatrix *A,NspObject **Index,int ind_flag,char dir, nsp_sort type,char ind_type);
extern int nsp_imatrix_column_sort(NspIMatrix *A,NspObject **Index,int ind_flag,char dir,char ind_type);
extern int nsp_imatrix_row_sort(NspIMatrix *A,NspObject **Index,int ind_flag,char dir,char ind_type);
extern int nsp_imatrix_lexical_column_sort(NspIMatrix *A,NspObject **Index,int ind_flag,char dir,char mode,char ind_type);
extern int nsp_imatrix_lexical_row_sort(NspIMatrix *A,NspObject **Index,int ind_flag,char dir,char mode,char ind_type);

/* quicksort generic */

extern void nsp_qsort_gen_col_sort_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_col_sort_int(int *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_col_sort_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

#define NSP_ITYPE_DECLARE(name)						\
  extern void name##gint(gint *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##guint(guint *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##gshort(gshort *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##gushort(gushort *a,int *ind,int flag,int n,int p,char dir);	\
  extern void name##glong(glong *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##gulong(gulong *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##gint8(gint8 *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##guint8(guint8 *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##gint16(gint16 *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##guint16(guint16 *a,int *ind,int flag,int n,int p,char dir);	\
  extern void name##gint32(gint32 *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##guint32(guint32 *a,int *ind,int flag,int n,int p,char dir);	\
  extern void name##gint64(gint64 *a,int *ind,int flag,int n,int p,char dir); \
  extern void name##guint64(guint64 *a,int *ind,int flag,int n,int p,char dir)	

NSP_ITYPE_DECLARE(nsp_qsort_gen_col_sort_);

extern void nsp_qsort_gen_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_int(int *a,int *ind,int flag,int n,int p,char dir);
NSP_ITYPE_DECLARE(nsp_qsort_gen_);
extern void nsp_qsort_gen_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_lexicol_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_lexicol_int(int *a,int *ind,int flag,int n,int p,char dir);
NSP_ITYPE_DECLARE(nsp_qsort_gen_lexicol_);
extern void nsp_qsort_gen_lexicol_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_lexirow_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_lexirow_int(int *a,int *ind,int flag,int n,int p,char dir);
NSP_ITYPE_DECLARE(nsp_qsort_gen_lexirow_);
extern void nsp_qsort_gen_lexirow_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

extern void nsp_qsort_gen_row_sort_double(double *a,int *ind,int flag,int n,int p,char dir);
extern void nsp_qsort_gen_row_sort_int(int *a,int *ind,int flag,int n,int p,char dir);
NSP_ITYPE_DECLARE(nsp_qsort_gen_row_sort_);
extern void nsp_qsort_gen_row_sort_nsp_string(nsp_string *a,int *ind,int flag,int n,int p,char dir);

/* quicksort  */
extern void nsp_qsort_int(int *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_double(double *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_nsp_string(nsp_string *a,int *tab, int flag, int n,char dir);

extern void nsp_qsort_gint(gint *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_guint(guint *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gshort(gshort *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gushort(gushort *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_glong(glong *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gulong(gulong *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gint8(gint8 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_guint8(guint8 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gint16(gint16 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_guint16(guint16 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gint32(gint32 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_guint32(guint32 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_gint64(gint64 *a,int *tab, int flag, int n,char dir);
extern void nsp_qsort_guint64(guint64 *a,int *tab, int flag, int n,char dir);

/* quicksort B Pincon   */
extern void nsp_qsort_bp_int(int x[], int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_double(double x[], int n, int p[],int flag,char dir );

extern void nsp_qsort_bp_gint(gint *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_guint(guint *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gshort(gshort *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gushort(gushort *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_glong(glong *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gulong(gulong *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gint8(gint8 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_guint8(guint8 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gint16(gint16 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_guint16(guint16 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gint32(gint32 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_guint32(guint32 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_gint64(gint64 *a, int n, int p[],int flag,char dir );
extern void nsp_qsort_bp_guint64(guint64 *a, int n, int p[],int flag,char dir );

/* bruno 's stupid stable quicksort   */
extern void nsp_sqsort_bp_int(int x[], int n, int p[],char dir );
extern void nsp_sqsort_bp_double(double x[], int n, int p[], char dir );
extern void nsp_sqsort_bp_nsp_string(nsp_string x[], int n, int p[],char dir);

extern void nsp_sqsort_bp_gint(gint *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_guint(guint *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gshort(gshort *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gushort(gushort *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_glong(glong *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gulong(gulong *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gint8(gint8 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_guint8(guint8 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gint16(gint16 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_guint16(guint16 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gint32(gint32 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_guint32(guint32 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_gint64(gint64 *a,int n, int p[],char dir );
extern void nsp_sqsort_bp_guint64(guint64 *a,int n, int p[],char dir );

/* mergesort */
extern int nsp_mergesort_int(int *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_double(double *a,int *p,int flag, int fromIndex, int toIndex,char dir);

extern int nsp_mergesort_gint(gint *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_guint(guint *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gshort(gshort *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gushort(gushort *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_glong(glong *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gulong(gulong *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gint8(gint8 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_guint8(guint8 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gint16(gint16 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_guint16(guint16 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gint32(gint32 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_guint32(guint32 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_gint64(gint64 *a,int *p,int flag, int fromIndex, int toIndex,char dir);
extern int nsp_mergesort_guint64(guint64 *a,int *p,int flag, int fromIndex, int toIndex,char dir);


/* unused code */
extern void nsp_qsort_stable_incr_int(int *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_decr_int(int *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_incr_double(double *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_decr_double(double *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_incr_nsp_string(nsp_string *a,int *index,int flag, int fromIndex, int toIndex,char dir);
extern void nsp_qsort_stable_decr_nsp_string(nsp_string *a,int *index,int flag, int fromIndex, int toIndex,char dir);

#endif

