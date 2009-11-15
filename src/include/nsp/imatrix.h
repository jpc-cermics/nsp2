#ifndef NSP_INC_IMATRIX 
#define NSP_INC_IMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include <glib.h> 

/*
 * NspIMatrix inherits from NspObject 
 */

/**
 * NspIMatrix: 
 * @m: number of rows 
 * @n: number of columns
 * @mn: @m x @n
 * @B: array containing the values 
 *
 * inherits from #NspObject 
 */

typedef struct _NspIMatrix NspIMatrix;
typedef struct _NspTypeIMatrix  NspTypeIMatrix;

struct _NspTypeIMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};


/* all the integer types */

typedef enum   { nsp_gint, nsp_guint, nsp_gshort, nsp_gushort, nsp_glong , 
		 nsp_gulong, nsp_gint8, nsp_guint8, nsp_gint16,
		 nsp_guint16, nsp_gint32, nsp_guint32, nsp_gint64, 
		 nsp_guint64 } nsp_itype;

/* format to use for printing the integer types */

#define NSP_ITYPE_FORMATS(names)				\
  char *(fmt)[]={"%*d","%*ud", "%*d", "%*u","%*d",		\
		 "%*u", "%*d", "%*u", "%*d",			\
		 "%*d", "%*d", "%*u", "%*"G_GINT64_FORMAT,	\
		 "%*"G_GUINT64_FORMAT,NULL};			

/* names that can be used at nsp level to specify an integer type */

#define NSP_ITYPE_NAMES(names)				       \
  char *(names)[]={"int", "uint", "short", "ushort", "long",	\
                   "ulong", "int8", "uint8", "int16",           \
                   "uint16", "int32", "uint32", "int64",        \
                   "uint64",NULL};

typedef union { 
    gint     Gint;
    guint     Guint;
    gshort     Gshort;
    gushort     Gushort;
    glong     Glong;
    gulong     Gulong;
    gint8     Gint8;
    guint8     Guint8;
    gint16     Gint16;
    guint16     Guint16;
    gint32     Gint32;
    guint32     Guint32;
    gint64     Gint64;
    guint64     Guint64;
} nsp_int_union ;

typedef union { 
    void *    Iv;
    gint     *Gint;
    guint     *Guint;
    gshort     *Gshort;
    gushort     *Gushort;
    glong     *Glong;
    gulong     *Gulong;
    gint8     *Gint8;
    guint8     *Guint8;
    gint16     *Gint16;
    guint16     *Guint16;
    gint32     *Gint32;
    guint32     *Guint32;
    gint64     *Gint64;
    guint64     *Guint64;
} nsp_int_union_ptr ;

struct _NspIMatrix {
  /*< private >*/
  NspObject father; 
  NspTypeIMatrix *type; 
  /*< public >*/
  int m,n,mn;
  union { 
    void *    Iv;
    gint *    Gint;
    guint *    Guint;
    gshort *    Gshort;
    gushort *    Gushort;
    glong *    Glong;
    gulong *    Gulong;
    gint8 *    Gint8;
    guint8 *    Guint8;
    gint16 *    Gint16;
    guint16 *    Guint16;
    gint32 *    Gint32;
    guint32 *    Guint32;
    gint64 *    Gint64;
    guint64 *    Guint64;
  };
  int itype;
  int eltsize;
};


#define NSP_MAX_ITYPE(xx,itype)						\
  switch(itype) {							\
  case nsp_gint: xx.Gint= G_MAXINT; break;				\
  case nsp_guint: xx.Guint= G_MAXUINT; break;				\
  case  nsp_gshort: xx.Gshort= G_MAXSHORT; break;			\
  case nsp_gushort: xx.Gushort= G_MAXUSHORT; break;			\
  case  nsp_glong: xx.Glong= G_MAXLONG ;  break;			\
  case nsp_gulong: xx.Gulong= G_MAXULONG; break;			\
  case  nsp_gint8: xx.Gint8= G_MAXINT8; break;				\
  case nsp_guint8: xx.Guint8= G_MAXUINT8; break;			\
  case  nsp_gint16: xx.Gint16= G_MAXINT16; break;			\
  case nsp_guint16: xx.Guint16= G_MAXUINT16; break;			\
  case  nsp_gint32: xx.Gint32= G_MAXINT32; break;			\
  case nsp_guint32: xx.Guint32= G_MAXUINT32; break;			\
  case  nsp_gint64: xx.Gint64= G_MAXINT64; break;			\
  case  nsp_guint64: xx.Guint64= G_MAXUINT64; break;}

#define NSP_MIN_ITYPE(xx,itype)						\
  switch(itype) {							\
  case nsp_gint: xx.Gint= G_MININT; break;				\
  case nsp_guint: xx.Guint= G_MINUINT; break;				\
  case  nsp_gshort: xx.Gshort= G_MINSHORT; break;			\
  case nsp_gushort: xx.Gushort= G_MINUSHORT; break;			\
  case  nsp_glong: xx.Glong= G_MINLONG ;  break;			\
  case nsp_gulong: xx.Gulong= G_MINULONG; break;			\
  case  nsp_gint8: xx.Gint8= G_MININT8; break;				\
  case nsp_guint8: xx.Guint8= G_MINUINT8; break;			\
  case  nsp_gint16: xx.Gint16= G_MININT16; break;			\
  case nsp_guint16: xx.Guint16= G_MINUINT16; break;			\
  case  nsp_gint32: xx.Gint32= G_MININT32; break;			\
  case nsp_guint32: xx.Guint32= G_MINUINT32; break;			\
  case  nsp_gint64: xx.Gint64= G_MININT64; break;			\
  case  nsp_guint64: xx.Guint64= G_MINUINT64; break;}



/* expand X expression in a swith 
 * #define X(name) for ( i=0 ; i < A->mn ; i++) {if ( A->name[i] ) count++;}break;
 * NSP_ITYPE_SWITCH(s,itype,X);
 */

#define NSP_ITYPE_SWITCH(itype,X,arg)		\
  switch (itype ) {				\
  case nsp_gint: X(Gint,gint,arg);		\
  case nsp_guint: X(Guint,guint,arg);		\
  case nsp_gshort: X(Gshort,gshort,arg);	\
  case nsp_gushort: X(Gushort,gushort,arg);	\
  case nsp_glong : X(Glong,glong,arg );		\
  case nsp_gulong: X(Gulong,gulong,arg);	\
  case nsp_gint8: X(Gint8,gint8,arg);		\
  case nsp_guint8: X(Guint8,guint8,arg);	\
  case nsp_gint16: X(Gint16,gint16,arg);	\
  case nsp_guint16: X(Guint16,guint16,arg);	\
  case nsp_gint32: X(Gint32,gint32,arg);	\
  case nsp_guint32: X(Guint32,guint32,arg);	\
  case nsp_gint64 : X(Gint64,gint64,arg );	\
  case nsp_guint64 : X(Gint64,guint64,arg );}

/* NSP_ITYPE_SIZE(s,itype) 
 * get the size of an int in s 
 */

#define __NSP_ITYPE_SIZE(name,type,arg) s= sizeof(type);break;
#define NSP_ITYPE_SIZE(s,itype)	NSP_ITYPE_SWITCH(itype,__NSP_ITYPE_SIZE,"") 

/* itype to itype copy 
 *
 */


#define __NSP_COPY_ITYPE_TO_ITYPE(name,offset,tag,cast,i,min,step,max,old,itype_old) \
  switch (itype_old) {							  \
  case nsp_gint:   for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gint *) old)[i];break; \
  case nsp_guint:  for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((guint *) old)[i];break; \
  case nsp_gshort: for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gshort *) old)[i];break; \
  case nsp_gushort:for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gushort *) old)[i];break; \
  case nsp_glong : for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((glong *) old)[i];break; \
  case nsp_gulong: for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gulong *) old)[i];break; \
  case nsp_gint8:  for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gint8 *) old)[i];break; \
  case nsp_guint8: for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((guint8 *) old)[i];break; \
  case nsp_gint16: for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gint16 *) old)[i];break; \
  case nsp_guint16:for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((guint16 *) old)[i];break; \
  case nsp_gint32: for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gint32 *) old)[i];break; \
  case nsp_guint32:for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((guint32 *) old)[i];break; \
  case nsp_gint64: for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((gint64 *) old)[i];break; \
  case nsp_guint64:for (i=min; i <  max; i+=step) (name)->tag[i+offset] = (cast) ((guint64 *) old)[i];break; \
  } 

#define NSP_COPY_ITYPE_TO_ITYPE(name,offset,itype,i,min,step,max,old,itype_old) \
  switch (itype ) {							\
  case nsp_gint: __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gint,gint,i,min,step,max,old,itype_old) ; break; \
  case nsp_guint: __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Guint,guint,i,min,step,max,old,itype_old) ; break; \
  case nsp_gshort: __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gshort,gshort,i,min,step,max,old,itype_old) ; break; \
  case nsp_gushort:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gushort,gushort,i,min,step,max,old,itype_old) ; break; \
  case nsp_glong : __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Glong,glong,i,min,step,max,old,itype_old) ; break; \
  case nsp_gulong: __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gulong,gulong,i,min,step,max,old,itype_old) ; break; \
  case nsp_gint8: __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gint8,gint8,i,min,step,max,old,itype_old) ; break; \
  case nsp_guint8:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Guint8,guint8,i,min,step,max,old,itype_old) ; break; \
  case nsp_gint16:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gint16,gint16,i,min,step,max,old,itype_old) ; break; \
  case nsp_guint16:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Guint16,guint16,i,min,step,max,old,itype_old) ; break; \
  case nsp_gint32: __NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gint32,gint32,i,min,step,max,old,itype_old) ; break; \
  case nsp_guint32:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Guint32,guint32,i,min,step,max,old,itype_old) ; break; \
  case nsp_gint64:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Gint64,gint64,i,min,step,max,old,itype_old) ; break; \
  case nsp_guint64:__NSP_COPY_ITYPE_TO_ITYPE(name,offset,Guint64,guint64,i,min,step,max,old,itype_old) ; break; \
  }

#define NSP_ITYPE_NAME(names,itype) (names)[itype] 

/*  example : 
 *  NSP_COPY_ITYPES(for ( i = imin ; i < imax ; i++ ),A,i+(i+k)*A->m,Diag->Iv,Diag->itype,j++);
 *  performs 
 *  for ( i = imin ; i < imax ; i++ ) A->Gint[i+(i+k)*A->m] = Diag->Gint[j++] ;
 *  for all mixed cases.
 */

#define __NSP_COPY_ITYPES(iter,name,tag,expl,cast,rhs,itype_rhs,expr)	\
  switch (itype_rhs) {							  \
  case nsp_gint:   iter (name)->tag[expl] = (cast) ((gint *) rhs)[expr];break; \
  case nsp_guint:  iter (name)->tag[expl] = (cast) ((guint *) rhs)[expr];break; \
  case nsp_gshort: iter (name)->tag[expl] = (cast) ((gshort *) rhs)[expr];break; \
  case nsp_gushort:iter (name)->tag[expl] = (cast) ((gushort *) rhs)[expr];break; \
  case nsp_glong : iter (name)->tag[expl] = (cast) ((glong *) rhs)[expr];break; \
  case nsp_gulong: iter (name)->tag[expl] = (cast) ((gulong *) rhs)[expr];break; \
  case nsp_gint8:  iter (name)->tag[expl] = (cast) ((gint8 *) rhs)[expr];break; \
  case nsp_guint8: iter (name)->tag[expl] = (cast) ((guint8 *) rhs)[expr];break; \
  case nsp_gint16: iter (name)->tag[expl] = (cast) ((gint16 *) rhs)[expr];break; \
  case nsp_guint16:iter (name)->tag[expl] = (cast) ((guint16 *) rhs)[expr];break; \
  case nsp_gint32: iter (name)->tag[expl] = (cast) ((gint32 *) rhs)[expr];break; \
  case nsp_guint32:iter (name)->tag[expl] = (cast) ((guint32 *) rhs)[expr];break; \
  case nsp_gint64: iter (name)->tag[expl] = (cast) ((gint64 *) rhs)[expr];break; \
  case nsp_guint64:iter (name)->tag[expl] = (cast) ((guint64 *) rhs)[expr];break; \
  } 

#define NSP_COPY_ITYPES(iter,name,expl,rhs,itype_rhs,expr)			\
  switch ( (name)->itype ) {						\
  case nsp_gint: __NSP_COPY_ITYPES(iter,name,Gint,expl,gint,rhs,itype_rhs,expr) ; break; \
  case nsp_guint: __NSP_COPY_ITYPES(iter,name, Guint,expl,guint,rhs,itype_rhs,expr) ; break; \
  case nsp_gshort: __NSP_COPY_ITYPES(iter,name,Gshort,expl,gshort,rhs,itype_rhs,expr) ; break; \
  case nsp_gushort:__NSP_COPY_ITYPES(iter,name,Gushort,expl,gushort,rhs,itype_rhs,expr) ; break; \
  case nsp_glong : __NSP_COPY_ITYPES(iter,name,Glong,expl,glong,rhs,itype_rhs,expr) ; break; \
  case nsp_gulong: __NSP_COPY_ITYPES(iter,name,Gulong,expl,gulong,rhs,itype_rhs,expr) ; break; \
  case nsp_gint8: __NSP_COPY_ITYPES(iter,name,Gint8,expl,gint8,rhs,itype_rhs,expr) ; break; \
  case nsp_guint8:__NSP_COPY_ITYPES(iter,name,Guint8,expl,guint8,rhs,itype_rhs,expr) ; break; \
  case nsp_gint16:__NSP_COPY_ITYPES(iter,name,Gint16,expl,gint16,rhs,itype_rhs,expr) ; break; \
  case nsp_guint16:__NSP_COPY_ITYPES(iter,name,Guint16,expl,guint16,rhs,itype_rhs,expr) ; break; \
  case nsp_gint32: __NSP_COPY_ITYPES(iter,name,Gint32,expl,gint32,rhs,itype_rhs,expr) ; break; \
  case nsp_guint32:__NSP_COPY_ITYPES(iter,name,Guint32,expl,guint32,rhs,itype_rhs,expr) ; break; \
  case nsp_gint64:__NSP_COPY_ITYPES(iter,name,Gint64,expl,gint64,rhs,itype_rhs,expr) ; break; \
  case nsp_guint64:__NSP_COPY_ITYPES(iter,name,Guint64,expl,guint64,rhs,itype_rhs,expr) ; break; \
  }


/**
 * nsp_type_imatrix_id:
 *
 * to be done 
 */

extern int nsp_type_imatrix_id;
extern NspTypeIMatrix *nsp_type_imatrix;

int nsp_type_imatrix_init();

/* only useful when building a new class derived from imatrix */

NspTypeIMatrix *new_type_imatrix(type_mode mode);

/* initialize type for Object */

int nsp_type_imatrix_init(void);

/* only useful when building a new class derived from imatrix */

void nsp_type_imatrix_set(NspIMatrix *imatrix, NspTypeIMatrix *type);

NspIMatrix *new_imatrix();

/*
 * Object methods redefined for imatrix 
 */

#define NULLIMAT (NspIMatrix*) 0

/* from IMatObj.c */

extern NspIMatrix *IMatObj (NspObject *O); 
extern int IsIMatObj (Stack stack, int i); 
extern int IsIMat(const NspObject *O);
extern NspIMatrix *GetIMatCopy (Stack stack, int i); 
extern NspIMatrix *GetIMat (Stack stack, int i); 
extern NspIMatrix  *GetScalarIMat(Stack stack, int i);
extern int BoolScalar (NspObject *O, Boolean *val); 
extern int GetScalarBool (Stack stack, int i, int *val); 

/* from NspIMatrix.c */

extern NspIMatrix *nsp_imatrix_create(const char *name, int m, int n, nsp_itype itype); 
extern NspIMatrix *nsp_imatrix_clone(const char *name, NspIMatrix *A, int m, int n, int init);
extern NspIMatrix *nsp_imatrix_copy(NspIMatrix *A); 
extern unsigned int  nsp_imatrix_elt_size(NspMatrix *M);
extern int nsp_imatrix_fill_with (NspIMatrix *A, const NspIMatrix *B); 
extern int nsp_imatrix_resize(NspIMatrix *A, int m, int n); 
extern int nsp_imatrix_scalar_to_mn (NspIMatrix *A, int m, int n); 
extern void nsp_imatrix_destroy(NspIMatrix *IMat); 
extern int nsp_imatrix_info(NspIMatrix *IMat, int indent,const char *name, int rec_level); 
extern int nsp_imatrix_print(NspIMatrix *IMat, int indent,const char *name, int rec_level); 
extern int nsp_imatrix_latex_print(NspIMatrix *IMat); 
extern int nsp_imatrix_latex_tab_print(NspIMatrix *IMat); 
extern int nsp_imatrix_redim(NspIMatrix *A, int m, int n); 
extern int nsp_imatrix_enlarge(NspIMatrix *A, int m, int n); 
extern int nsp_imatrix_concat_right(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_add_columns(NspIMatrix *A, int n); 
extern NspIMatrix *nsp_imatrix_concat_down(NspIMatrix *A, NspIMatrix *B); 
extern NspIMatrix *nsp_imatrix_concat_diag(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_add_rows(NspIMatrix *A, int m); 
extern int nsp_imatrix_set_suimatrix(NspIMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspIMatrix *B); 
extern int nsp_imatrix_set_rows(NspIMatrix *A, NspMatrix *Rows, NspIMatrix *B); 
extern NspIMatrix *nsp_imatrix_extract(NspIMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspIMatrix *nsp_imatrix_extract_elements(NspIMatrix *A, NspMatrix *Elts); 
extern NspIMatrix *nsp_imatrix_extract_columns(NspIMatrix *A, NspMatrix *Cols); 
extern NspIMatrix *nsp_imatrix_extract_rows(NspIMatrix *A, NspMatrix *Rows); 
extern NspIMatrix *IMatLoopCol (char *str, NspIMatrix *Col, NspIMatrix *A, int icol, int *rep); 
extern NspIMatrix *nsp_imatrix_extract_diag(NspIMatrix *A, int k); 
extern int nsp_imatrix_set_diag(NspIMatrix *A, NspIMatrix *Diag, int k); 
extern NspIMatrix *nsp_imatrix_create_diag(NspIMatrix *Diag, int k); 
extern NspIMatrix *nsp_imatrix_transpose(NspIMatrix *A); 
extern NspIMatrix *nsp_matrix_to_imatrix(NspMatrix *M,nsp_itype itype); 
extern NspMatrix *nsp_imatrix_to_matrix(NspIMatrix *M); 
extern int MatIsTrue (NspMatrix *M); 
extern int nsp_imatrix_and(NspIMatrix *A,const NspIMatrix *B); 
extern int nsp_imatrix_scalar_and(NspIMatrix *A,const NspIMatrix *B); 
extern int nsp_imatrix_or(NspIMatrix *A,const NspIMatrix *B); 
extern int nsp_imatrix_scalar_or(NspIMatrix *A,const NspIMatrix *B); 
extern int nsp_imatrix_not(NspIMatrix *A); 
extern int IMatIsTrue (NspIMatrix *A); 
extern NspMatrix *nsp_imatrix_count_true(const NspIMatrix *A); 
extern int nsp_imatrix_find(NspIMatrix *A, int lhs, NspObject **Res1, NspObject **Res2, char ind_type);
extern int nsp_imatrix_find_2(const NspIMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
extern NspBMatrix *nsp_imatrix_comp(const NspIMatrix *A, const NspIMatrix *B,const char *op); 
extern int nsp_imatrix_fullcomp(const NspIMatrix *A,const  NspIMatrix *B, const char *op, int *err); 
extern int nsp_imatrix_change_itype(NspIMatrix *A,nsp_itype itype);

extern int nsp_xdr_save_array_ixx(XDR *xdrs, void *nx,nsp_itype itye, int l);
extern int nsp_xdr_load_array_ixx(XDR *xdrs, void *nx,nsp_itype itype, int l);

/* from IMatOps */

extern int nsp_imatrix_scale_rows(NspIMatrix *A, NspIMatrix *x);
extern int nsp_imatrix_scale_cols(NspIMatrix *A, NspIMatrix *x);
extern NspIMatrix *nsp_imatrix_diff(NspIMatrix *A, int order, int dim);
extern int nsp_imatrix_mult_scalar_bis(NspIMatrix *A, NspIMatrix *B);
extern int nsp_imatrix_add_scalar_bis(NspIMatrix *A, NspIMatrix *B);
extern int nsp_imatrix_add_mat(NspIMatrix *A, NspIMatrix *B);
extern int nsp_imatrix_sub_scalar_bis(NspIMatrix *A, NspIMatrix *B);
extern int nsp_scalar_sub_imatrix_bis(NspIMatrix *A, NspIMatrix *B);
extern int nsp_imatrix_sub_mat(NspIMatrix *A, NspIMatrix *B);
extern void nsp_imatrix_set_rval(NspIMatrix *A, double dval); 
extern int nsp_imatrix_set_ival(NspIMatrix *A, double dval); 
extern NspIMatrix *nsp_imatrix_mult(NspIMatrix *A, NspIMatrix *B, int flag);
extern int nsp_imatrix_add(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_dadd(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern int nsp_imatrix_dadd_maxplus(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern int nsp_imatrix_add_scalar(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern int nsp_imatrix_sub(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_dsub(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern int nsp_imatrix_sub_scalar(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern int nsp_imatrix_subs_calarm(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern int nsp_imatrix_maxitt1(NspIMatrix *A, NspIMatrix *B, NspMatrix *Ind, int j, int flag); 
extern int nsp_imatrix_minitt1(NspIMatrix *A, NspIMatrix *B, NspMatrix *Ind, int j, int flag); 
extern int nsp_imatrix_minmax(NspIMatrix *A, int dim, NspIMatrix **Amin, NspMatrix **Imin,
			      NspIMatrix **Amax, NspMatrix **Imax, int lhs);
extern NspIMatrix **nsp_imatrix_slec(char *file, int *Count); 
extern void nsp_csetd(const int *n,const double *z,doubleC *tab,const int *inc) ;
extern int nsp_imatrix_inv_el(NspIMatrix *A); 
extern NspIMatrix *nsp_imatrix_kron(NspIMatrix *A, NspIMatrix *B); 
extern NspIMatrix *nsp_imatrix_sum(NspIMatrix *A, int dim); 
extern NspIMatrix *nsp_imatrix_prod(NspIMatrix *A, int dim); 
extern NspIMatrix *nsp_imatrix_cum_prod(NspIMatrix *A,  int dim); 
extern NspIMatrix *nsp_imatrix_cum_sum(NspIMatrix *A,  int dim); 
extern NspIMatrix *nsp_imatrix_maxi(NspIMatrix *A, int dim_flag, NspMatrix **Imax, int lhs); 
extern NspIMatrix *nsp_imatrix_mini(NspIMatrix *A, int dim_flag, NspMatrix **Imax, int lhs); 
extern NspIMatrix *nsp_imatrix_createinit(char *name, nsp_itype type, int m, int n, int (*func)(int,int)); 
extern void nsp_imatrix_triu(NspIMatrix *A, int k); 
extern void nsp_imatrix_tril(NspIMatrix *A, int k); 
extern NspIMatrix *nsp_imatrix_eye(int m, int n,nsp_itype itype); 
extern NspIMatrix *nsp_imatrix_ones(int m, int n,nsp_itype itype); 
extern NspIMatrix *nsp_imatrix_zeros(int m, int n,nsp_itype itype); 
extern NspIMatrix *nsp_imatrix_rand(int m, int n,nsp_itype itype); 
extern int nsp_imatrix_pow_matscalar(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_pow_matmat(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_pow_scalarmat(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_pow_tt(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_pow_el(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_pow_scalar(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_pow_scalarm(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_div_tt(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_div_el(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_div_scalar(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_bdiv_tt(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_bdiv_el(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_bdiv_scalar(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_mult_tt(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_mult_el(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_mult_scalar(NspIMatrix *Mat1, NspIMatrix *Mat2); 
extern void nsp_imatrix_modulo(NspIMatrix *A,NspIMatrix *B); 
extern void nsp_imatrix_idiv(NspIMatrix *A, int n); 
extern void nsp_imatrix_mod(NspIMatrix *x, NspIMatrix *y);
extern void nsp_imatrix_int(NspIMatrix *A); 
extern int nsp_imatrix_sign(NspIMatrix *A); 
extern int nsp_imatrix_abs(NspIMatrix *A); 
extern int nsp_imatrix_iand(NspIMatrix *A, NspIMatrix *B); 
extern int nsp_imatrix_ior(NspIMatrix *A, NspIMatrix *B); 
extern NspIMatrix *nsp_imatrix_ior_unary(NspIMatrix *A, int dim);
extern NspIMatrix *nsp_imatrix_iand_unary(NspIMatrix *A, int dim);
extern int nsp_imatrix_ishift(NspIMatrix *A,int shift,char dir);
extern int nsp_imatrix_minus(NspIMatrix *A); 
/* extern int nsp_imatrix_find(NspIMatrix *A, int lhs, NspIMatrix **Res1, NspIMatrix **Res2);  */
extern int nsp_imatrix_mfind(const NspIMatrix *x, int m,const char **ops,const double *scalars, NspIMatrix **Ind);
extern int nsp_imatrix_ndind2ind(int *dims, int nd, NspIMatrix **ndind, NspIMatrix **Ind);
extern int nsp_imatrix_sub2ind(int *dims, int nd, NspIMatrix **ndind, int nb_ind, NspIMatrix **Ind);
extern int nsp_imatrix_nnz(NspIMatrix *A);
extern int nsp_imatrix_unique(NspIMatrix *x, NspObject **Ind, NspMatrix **Occ, Boolean first_ind, char ind_type);
extern NspIMatrix *nsp_imatrix_dot(NspIMatrix *A, NspIMatrix *B, int dim_flag);
extern NspIMatrix *nsp_imatrix_cross(NspIMatrix *X, NspIMatrix *Y, int dim);
extern NspBMatrix *nsp_imatrix_issorted(NspIMatrix *A, int dim_flag, Boolean strict_order);
extern NspBMatrix *nsp_imatrix_has(NspIMatrix *A, NspIMatrix *x, int lhs, NspMatrix **ind, NspMatrix **ind2);

extern nsp_int_union nsp_imatrix_intmax(const NspIMatrix *A);
extern nsp_int_union nsp_imatrix_intmin(const NspIMatrix *A);


#endif 

#ifdef IMatrix_Private 
static int init_imatrix(NspIMatrix *ob,NspTypeIMatrix *type);
static int imatrix_size(NspIMatrix *Mat, int flag);
static char *imatrix_type_as_string(void);
static char *imatrix_type_short_string(NspObject *v);
static NspObject *imatrix_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int imatrix_eq(NspIMatrix *A, NspObject *B);
static int imatrix_neq(NspIMatrix *A, NspObject *B);
static int imatrix_xdr_save(XDR  *xdrs, NspIMatrix *M);
static NspIMatrix  *imatrix_xdr_load(XDR  *F);
static AttrTab imatrix_attrs[];
static NspMethods *imatrix_get_methods(void); 
/*static NspObject *imatrix_path_extract(NspIMatrix *A, NspObject *O); */
static int imatrix_is_true(NspIMatrix *M);
static int nsp_imatrix_as_index(NspIMatrix * M, index_vector *index);
#endif /* IMatrix_Private */
