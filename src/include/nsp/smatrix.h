#ifndef NSP_INC_SMATRIX 
#define NSP_INC_SMATRIX

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/string.h" 

/*
 * NspMatrix inherits from NspObject 
 */

typedef struct _NspSmatrix NspSMatrix ;

typedef struct _NspTypeSMatrix { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeSMatrix;

struct _NspSmatrix {
  /*< private >*/
  NspObject father; 
  NspTypeSMatrix *type; 
  /*< public >*/
  int m,n,mn;
  nsp_string *S;  /* a table of mn+1 strings the last one is NULL */
};

#include "nsp/matrix.h" 
#include "nsp/bmatrix.h" 

extern int nsp_type_smatrix_id;
extern NspTypeSMatrix *nsp_type_smatrix;

/* only useful when building a new class derived from matrix */

NspTypeSMatrix *new_type_smatrix(type_mode mode) ;

/* only useful when building a new class derived from matrix */

NspSMatrix *new_smatrix();

/*
 * Object methods redefined for smatrix 
 */

#ifdef SMatrix_Private 
static int init_smatrix(NspSMatrix *ob,NspTypeSMatrix *type);
static int nsp_smatrix_size(NspSMatrix *Mat, int flag);
char *nsp_smatrix_type_as_string(void);
char *nsp_smatrix_type_short_string(void);
NspObject *nsp_smatrix_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_smatrix_eq(NspObject *A,NspObject *B);
int nsp_smatrix_neq(NspObject *A,NspObject *B);
int nsp_smatrix_is_true(NspSMatrix *M);
NspSMatrix *nsp_smatrix_xdr_load(XDR  *F);
int nsp_smatrix_xdr_save(XDR  *F, NspSMatrix *M);
#endif 

#define NULLSTRING (nsp_string) 0
#define NULLSMAT   (NspSMatrix *) 0

/* prototypes */

extern NspSMatrix *nsp_smatrix_object(NspObject *O); 
extern char *nsp_string_object(NspObject *O); 
extern int IsSMatObj (Stack stack, int i); 
extern int IsSMat (NspObject *O); 
extern int IsString(NspObject *O);
extern NspSMatrix *GetSMatCopy (Stack stack, int i); 
extern NspSMatrix *GetSMat (Stack stack, int i); 
extern char *GetString (Stack stack, int i); 
extern int GetStringInArray (Stack stack, int ith, char **Table, int flag); 
extern int GetStringInStruct(Stack stack, int ith,void *T,unsigned int size, int flag);
extern int is_string_in_array (const char *key, char **Table, int flag); 
extern void string_not_in_array(Stack stack,const char *key, char **Table,char *message);
extern int is_string_in_struct(const char *key,void **Table,unsigned int size, int flag);
extern NspSMatrix *nsp_smatrix_create(nsp_const_string name,int m,int n,nsp_const_string str,int flag);
extern NspSMatrix *nsp_smatrix_create_with_length(nsp_const_string , int m, int n, int strl); 
extern NspSMatrix *nsp_smatrix_create_from_table(char **T); 
extern NspSMatrix *nsp_smatrix_create_from_array(nsp_const_string name,int n,const char **T); 
extern NspSMatrix*nsp_smatrix_create_from_struct(nsp_const_string name,const void *T,unsigned int size);
extern unsigned int  nsp_smatrix_elt_size(NspMatrix *M);
extern NspSMatrix *nsp_smatrix_copy(const NspSMatrix *A); 
extern int nsp_smatrix_resize(NspSMatrix *A, int m, int n); 
extern void nsp_smatrix_destroy(NspSMatrix *A); 
extern void nsp_smatrix_info(const NspSMatrix *Mat, int indent); 
extern void nsp_smatrix_print(const NspSMatrix *Mat, int indent); 
extern int nsp_smatrix_redim(NspSMatrix *A, int m, int n); 
extern int nsp_smatrix_enlarge(NspSMatrix *A, int m, int n); 
extern int nsp_smatrix_concat_right(NspSMatrix *A,const NspSMatrix *B); 
extern int Scopy (int n, nsp_string *s1, nsp_string *s2); 
extern int nsp_smatrix_add_columns(NspSMatrix *A, int n); 
extern int Sset (int n, nsp_string s1, nsp_string *s2); 
extern NspSMatrix *nsp_smatrix_concat_down(const NspSMatrix *A,const NspSMatrix *B); 
extern int nsp_smatrix_add_rows(NspSMatrix *A, int m); 
extern int nsp_smatrix_set_submatrix(NspSMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspSMatrix *B); 
extern int nsp_smatrix_set_rows(NspSMatrix *A, NspMatrix *Rows, NspSMatrix *B); 
extern int nsp_smatrix_delete_columns(NspSMatrix *A, NspMatrix *Cols); 
extern int nsp_smatrix_delete_rows(NspSMatrix *A, NspMatrix *Rows); 
extern int nsp_smatrix_delete_elements(NspSMatrix *A, NspMatrix *Elts); 
extern int nsp_smatrix_delete_elements2(NspSMatrix *A, NspMatrix *EltsR, NspMatrix *EltsC);
extern NspSMatrix *nsp_smatrix_extract(NspSMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspSMatrix *nsp_smatrix_extract_elements(NspSMatrix *A, NspMatrix *Elts, int *err); 
extern NspSMatrix *nsp_smatrix_extract_columns(NspSMatrix *A, NspMatrix *Cols, int *err); 
extern NspSMatrix *SMatLoopCol (char *str, NspSMatrix *Col, NspSMatrix *A, int icol, int *rep); 
extern NspSMatrix *nsp_smatrix_extract_rows(NspSMatrix *A, NspMatrix *Rows, int *err); 
extern int nsp_smatrix_concat_strings(NspSMatrix *A, NspSMatrix *B,nsp_const_string str, int flag); 
extern int nsp_smatrix_concat_string_right(NspSMatrix *A, NspSMatrix *B, nsp_const_string str, int flag); 
extern int nsp_smatrix_concat_string_left(NspSMatrix *A, NspSMatrix *B, nsp_const_string str, int flag); 
extern NspMatrix *nsp_smatrix_strcmp(NspSMatrix *A, NspSMatrix *B); 
extern NspSMatrix *nsp_smatrix_column_concat_padded(NspSMatrix *A, nsp_const_string str, int flag); 
extern NspSMatrix *nsp_smatrix_column_concat(NspSMatrix *A, nsp_const_string str, int flag); 
extern NspSMatrix *nsp_smatrix_row_concat(NspSMatrix *A, nsp_const_string str, int flag); 
extern nsp_string nsp_smatrix_elts_concat(NspSMatrix *A, nsp_const_string rstr, int rflag, nsp_const_string cstr, int cflag); 
extern NspSMatrix *nsp_smatrix_part(NspSMatrix *A, NspMatrix *Ind); 
extern NspMatrix *nsp_smatrix_elts_length(NspSMatrix *A); 
extern NspSMatrix *nsp_matrix_to_smatrix(NspMatrix *A, nsp_const_string str, int flag); 
extern void nsp_smatrix_tolower(NspSMatrix *A); 
extern void nsp_smatrix_toupper(NspSMatrix *A); 
extern void nsp_smatrix_capitalize(NspSMatrix *A); 
extern NspMatrix *nsp_smatrix_strstr(NspSMatrix *A, nsp_const_string Str); 
extern NspMatrix *nsp_smatrix_strindex(nsp_const_string Str1, nsp_const_string Str2); 
extern NspSMatrix *nsp_ascii_to_smatrix(NspMatrix *A); 
extern NspMatrix *nsp_string_to_ascii(nsp_const_string S); 
extern NspMatrix *nsp_smatrix_sort_old(NspSMatrix *A,int flag,nsp_const_string  str1,nsp_const_string str2); 
extern NspSMatrix *nsp_smatrix_split(nsp_const_string string, nsp_const_string splitChars, int msep); 
extern int nsp_row_smatrix_append_string(NspSMatrix *A, nsp_const_string str); 

extern NspBMatrix *SMatCompOp (NspSMatrix *A, NspSMatrix *B, char *op); 
extern int SMatFullComp (NspSMatrix *A, NspSMatrix *B, char *op, int *err); 
extern NspSMatrix *nsp_smatrix_transpose(const NspSMatrix *A); 
extern NspSMatrix *nsp_smatrix_subst(const NspSMatrix *A,nsp_const_string needle,nsp_const_string  replace); 
extern int nsp_smatrix_strip_blanks(NspSMatrix *A); 

extern NspSMatrix *nsp_get_methods(NspObject *ob,NspTypeBase *type);
extern int nsp_read_lines(NspFile *F,NspSMatrix **S,int nlines);

#endif 

