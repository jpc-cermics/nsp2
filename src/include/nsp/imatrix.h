#ifndef NSP_INC_IMATRIX 
#define NSP_INC_IMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include <glib.h> 
#include "nsp/matrix.h" 
#include "nsp/smatrix.h" 

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


typedef enum   { nsp_gint, nsp_guint, nsp_gshort, nsp_gushort, nsp_glong , 
		 nsp_gulong, nsp_gint8, nsp_guint8, nsp_gint16,
		 nsp_guint16, nsp_gint32, nsp_guint32, nsp_gint64, 
		 nsp_guint64 } nsp_itype;

#define NSP_ITYPE_NAMES(names)				       \
  char *(names)[]={"int", "uint", "short", "ushort", "long",	\
                   "ulong", "int8", "uint8", "int16",           \
                   "uint16", "int32", "uint32", "int64",        \
                   "uint64",NULL};

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

#define NSP_ITYPE_SIZE(s,itype)			\
  switch (itype ) {				\
  case nsp_gint: s= sizeof(gint);break;		\
  case nsp_guint: s= sizeof(guint);break;	\
  case nsp_gshort: s= sizeof(gshort);break;	\
  case nsp_gushort: s= sizeof(gushort);break;	\
  case nsp_glong : s= sizeof(glong );break;		\
  case nsp_gulong: s= sizeof(gulong);break;		\
  case nsp_gint8: s= sizeof(gint8);break;		\
  case nsp_guint8: s= sizeof(guint8);break;		\
  case nsp_gint16: s= sizeof(gint16);break;		\
  case nsp_guint16: s= sizeof(guint16);break;		\
  case nsp_gint32: s= sizeof(gint32);break;		\
  case nsp_guint32: s= sizeof(guint32);break;		\
  case nsp_gint64 : s= sizeof(gint64 );break;		\
  case nsp_guint64 : s= sizeof(gint64 );break;	}

#define NSP_COPY_TO_ITYPE(name,itype,i,min,step,max,orig)	\
  switch (itype ) {							\
 case nsp_gint: for (i=min; i <  max; i+=step) (name)->Gint[i] = (gint) (orig);break; \
 case nsp_guint: for (i=min; i <  max; i+=step) (name)->Guint[i] = (guint) (orig);break; \
 case nsp_gshort:for (i=min; i <  max; i+=step) (name)->Gshort[i] = (gshort) (orig);break; \
 case nsp_gushort:for (i=min; i <  max; i+=step) (name)->Gushort[i] = (gushort) (orig);break; \
 case nsp_glong : for (i=min; i <  max; i+=step) (name)->Glong[i] = (glong) (orig);break; \
 case nsp_gulong: for (i=min; i <  max; i+=step) (name)->Gulong[i] = (gulong) (orig);break;	\
 case nsp_gint8:for (i=min; i <  max; i+=step) (name)->Gint8[i] = (gint8) (orig);break; \
 case nsp_guint8: for (i=min; i <  max; i+=step) (name)->Guint8[i] = (guint8) (orig);break;	\
 case nsp_gint16: for (i=min; i <  max; i+=step) (name)->Gint16[i] = (gint16) (orig);break;	\
 case nsp_guint16:for (i=min; i <  max; i+=step) (name)->Guint16[i] = (guint16) (orig);break; \
 case nsp_gint32: for (i=min; i <  max; i+=step) (name)->Gint32[i] = (gint32) (orig);break; \
 case nsp_guint32:for (i=min; i <  max; i+=step) (name)->Guint32[i] = (guint32) (orig);break; \
 case nsp_gint64:for (i=min; i <  max; i+=step) (name)->Gint64[i] = (gint64) (orig);break; \
 case nsp_guint64:for (i=min; i <  max; i+=step) (name)->Guint64[i] = (guint64) (orig);break;} 

#define NSP_COPY_FROM_ITYPE(dest,cast,name,itype,i,min,step,max)	\
  switch (itype ) {							\
  case nsp_gint:   for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gint[i];break; \
  case nsp_guint:  for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Guint[i];break; \
  case nsp_gshort: for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gshort[i];break; \
  case nsp_gushort:for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gushort[i];break; \
  case nsp_glong : for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Glong[i];break; \
  case nsp_gulong: for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gulong[i];break; \
  case nsp_gint8:  for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gint8[i];break; \
  case nsp_guint8: for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Guint8[i];break; \
  case nsp_gint16: for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gint16[i];break; \
  case nsp_guint16:for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Guint16[i];break; \
  case nsp_gint32: for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gint32[i];break; \
  case nsp_guint32:for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Guint32[i];break; \
  case nsp_gint64: for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Gint64[i];break; \
  case nsp_guint64:for (i=min; i <  max; i+=step) (dest) = (cast) (name)->Guint64[i];break;} 

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
extern int BoolScalar (NspObject *O, Boolean *val); 
extern int GetScalarBool (Stack stack, int i, int *val); 

/* from NspIMatrix.c */

extern NspIMatrix *nsp_imatrix_create(const char *name, int m, int n, nsp_itype itype); 
extern NspIMatrix *nsp_imatrix_clone(const char *name, NspIMatrix *A, int m, int n, int init);
extern NspIMatrix *nsp_imatrix_copy(NspIMatrix *A); 
extern unsigned int  nsp_imatrix_elt_size(NspMatrix *M);
extern int nsp_imatrix_resize(NspIMatrix *A, int m, int n); 
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
extern NspMatrix *nsp_imatrix_find(const NspIMatrix *A); 
extern int nsp_imatrix_find_2(const NspIMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2); 
extern NspBMatrix *nsp_imatrix_compare(const NspIMatrix *A, const NspIMatrix *B, char *op); 
extern int nsp_imatrix_full_compare(const NspIMatrix *A,const  NspIMatrix *B, char *op, int *err); 
extern int nsp_imatrix_change_itype(NspIMatrix *A,nsp_itype itype);

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
