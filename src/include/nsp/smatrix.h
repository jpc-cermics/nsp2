#ifndef INC_NSP_SMATRIX 
#define INC_NSP_SMATRIX

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

/*
 * NspMatrix inherits from NspObject 
 */
typedef struct _nsp_smatrix NspSMatrix ;

typedef int (*smatrix_save) (NspFile  *F, NspSMatrix *M);

typedef struct _nsp_type_SMatrix { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  smatrix_save *save;
} NspTypeSMatrix;

typedef char String ;

struct _nsp_smatrix {
  NspObject father; 
  NspTypeSMatrix *type; 
  integer m,n,mn;
  String **S;
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
NspSMatrix *nsp_smatrix_xdr_load(NspFile  *F);
int nsp_smatrix_xdr_save(NspFile  *F, NspSMatrix *M);
#endif 

#define NULLSTRING (char *) 0
#define NULLSMAT ( NspSMatrix *) 0

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
extern NspSMatrix *nsp_smatrix_create(const char *name, integer m, integer n,const char *str, integer flag);
extern NspSMatrix *nsp_smatrix_create_with_length(const char *name, integer m, integer n, integer strl); 
extern NspSMatrix *nsp_smatrix_create_from_table(char **T); 
extern NspSMatrix *nsp_smatrix_create_from_array(const char *name,int n,const char **T); 
extern NspSMatrix*nsp_smatrix_create_from_struct(const char *name,const void *T,unsigned int size);
extern NspSMatrix *nsp_smatrix_copy(const NspSMatrix *A); 
extern int nsp_smatrix_resize(NspSMatrix *A, integer m, integer n); 
extern void nsp_smatrix_destroy(NspSMatrix *A); 
extern void nsp_smatrix_info(const NspSMatrix *Mat, int indent); 
extern void nsp_smatrix_print(const NspSMatrix *Mat, int indent); 
extern int nsp_smatrix_redim(NspSMatrix *A, integer m, integer n); 
extern int nsp_smatrix_enlarge(NspSMatrix *A, integer m, integer n); 
extern int nsp_smatrix_concat_right(NspSMatrix *A,const NspSMatrix *B); 
extern int Scopy (integer n, String **s1, String **s2); 
extern int nsp_smatrix_add_columns(NspSMatrix *A, integer n); 
extern int Sset (integer n, String *s1, String **s2); 
extern NspSMatrix *nsp_smatrix_concat_down(const NspSMatrix *A,const NspSMatrix *B); 
extern int nsp_smatrix_add_rows(NspSMatrix *A, integer m); 
extern int nsp_smatrix_set_submatrix(NspSMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspSMatrix *B); 
extern int nsp_smatrix_set_rows(NspSMatrix *A, NspMatrix *Rows, NspSMatrix *B); 
extern int nsp_smatrix_delete_columns(NspSMatrix *A, NspMatrix *Cols); 
extern int nsp_smatrix_delete_rows(NspSMatrix *A, NspMatrix *Rows); 
extern int nsp_smatrix_delete_elements(NspSMatrix *A, NspMatrix *Elts); 
extern NspSMatrix *nsp_smatrix_extract(NspSMatrix *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspSMatrix *nsp_smatrix_extract_elements(NspSMatrix *A, NspMatrix *Elts, int *err); 
extern NspSMatrix *nsp_smatrix_extract_columns(NspSMatrix *A, NspMatrix *Cols, int *err); 
extern NspSMatrix *SMatLoopCol (char *str, NspSMatrix *Col, NspSMatrix *A, int icol, int *rep); 
extern NspSMatrix *nsp_smatrix_extract_rows(NspSMatrix *A, NspMatrix *Rows, int *err); 

extern char *NewString (const char *str); 
extern String *Basic2String (const char *str); 
extern String *CopyString (const String *str); 
extern void StringDestroy (String **str); 
extern String *NewStringN (int n); 
extern int StringResize (char **Hstr, unsigned int n); 
extern int nsp_smatrix_concat_strings(NspSMatrix *A, NspSMatrix *B, char *str, integer flag); 
extern int nsp_smatrix_concat_string_right(NspSMatrix *A, NspSMatrix *B, char *str, integer flag); 
extern int nsp_smatrix_concat_string_left(NspSMatrix *A, NspSMatrix *B, char *str, integer flag); 
extern NspMatrix *nsp_smatrix_strcmp(NspSMatrix *A, NspSMatrix *B); 
extern NspSMatrix *nsp_smatrix_column_concat_padded(NspSMatrix *A, char *str, integer flag); 
extern NspSMatrix *nsp_smatrix_column_concat(NspSMatrix *A, char *str, integer flag); 
extern NspSMatrix *nsp_smatrix_row_concat(NspSMatrix *A, char *str, integer flag); 
extern String *nsp_smatrix_elts_concat(NspSMatrix *A, char *rstr, integer rflag, char *cstr, integer cflag); 
extern NspSMatrix *nsp_smatrix_part(NspSMatrix *A, NspMatrix *Ind); 
extern NspMatrix *nsp_smatrix_elts_length(NspSMatrix *A); 
extern NspSMatrix *nsp_matrix_to_smatrix(NspMatrix *A, char *str, integer flag); 
extern void nsp_smatrix_tolower(NspSMatrix *A); 
extern void nsp_smatrix_toupper(NspSMatrix *A); 
extern void nsp_smatrix_capitalize(NspSMatrix *A); 
extern NspMatrix *nsp_smatrix_strstr(NspSMatrix *A, char *Str); 
extern NspMatrix *nsp_smatrix_strindex(char *Str1, char *Str2); 
extern NspSMatrix *nsp_ascii_to_smatrix(NspMatrix *A); 
extern NspMatrix *nsp_string_to_ascii(char *S); 
extern NspMatrix *nsp_smatrix_sort(NspSMatrix *A, int flag, char *str1, char *str2); 
extern NspSMatrix *nsp_smatrix_split(char *string, char *splitChars); 
extern int nsp_row_smatrix_append_string(NspSMatrix *A, char *str); 

extern NspBMatrix *SMatCompOp (NspSMatrix *A, NspSMatrix *B, char *op); 
extern int SMatFullComp (NspSMatrix *A, NspSMatrix *B, char *op, int *err); 
extern NspSMatrix *nsp_smatrix_transpose(const NspSMatrix *A); 
extern NspSMatrix *nsp_smatrix_subst(const NspSMatrix *A, const char *needle, const char *replace); 
extern int nsp_smatrix_strip_blanks(NspSMatrix *A); 

extern NspSMatrix *nsp_get_methods(NspObject *ob,NspTypeBase *type);
extern int nsp_read_lines(NspFile *F,NspSMatrix **S,int nlines);

#endif 

