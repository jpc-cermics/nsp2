#ifndef INC_NSP_LIST 
#define INC_NSP_LIST

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspList inherits from NspObject 
 */

typedef struct _nsp_list  NspList;

typedef int (*list_save) (NspFile  *F, NspList *M);

typedef struct _nsp_type_List { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  list_save *save;
} NspTypeList;

typedef struct cell {
  char *name;         /* name of the cell : for named list */
  struct cell *prev;  /* points to the previous cell or 0 */
  struct cell *next;  /* points to the next cell or 0 */
  NspObject *O;             /* points to the stored object */
} Cell ;

struct _nsp_list {
  NspObject father; 
  NspTypeList *type; 
  char *tname;        /* name of the tlist or NULL **/
  Cell *first; /* pointer to the first cell */
};

extern int nsp_type_list_id;
extern NspTypeList *nsp_type_list;

/* only useful when building a new class derived from list */

NspTypeList *new_type_list(type_mode mode);

NspList *new_list();

/*
 * Object methods redefined for list 
 */

#ifdef List_Private
static int init_list(NspList *ob,NspTypeList *type);
static int nsp_list_size(NspList *Mat, int flag);
char *nsp_list_type_as_string(void);
char *nsp_list_type_short_string(NspList *M);
NspObject *list_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_list_eq(NspObject *A,NspObject *B);
int nsp_list_neq(NspObject *A,NspObject *B);
static int nsp_list_is_true(NspList *l);
NspList*nsp_list_xdr_load(NspFile  *F);
int nsp_list_xdr_save(NspFile  *F, NspList *L);
#endif 

#define NULLLIST ( NspList *) 0 
#define NULLCELL ( Cell *) 0 
#define NULLOBJ  ( NspObject *) 0 

/** Functions declaration **/

extern NspObject *nsp_list_path_extract(NspList *L, NspObject *O); 
extern int nsp_list_eq(NspObject *A, NspObject *B); 
extern int nsp_list_neq(NspObject *A, NspObject *B); 
extern NspList *nsp_list_object(NspObject *O); 
extern int IsListObj (Stack stack, int i); 
extern int IsList (NspObject *O); 
extern NspList *GetListCopy (Stack stack, int i); 
extern NspList *GetList (Stack stack, int i); 
extern int ListFollowExtract(Stack stack, int rhs, int opt, int lhs);
extern NspObject *EvalMacro (NspPList *, NspObject **, NspList *, int *);
extern NspList *nsp_list_create(char *name, char *tname); 
extern Cell *nsp_cell_create(char *name, NspObject *O); 
extern void nsp_cell_destroy(Cell **c); 
extern void nsp_list_destroy(NspList *l); 
extern NspList *nsp_list_copy(NspList *L); 
extern NspList *nsp_list_extract(NspList *L, NspMatrix *Elts); 
extern int nsp_list_insert(NspList *L, NspObject *O, int n); 
extern NspObject *nsp_list_get_element(NspList *L, integer nel); 
extern int nsp_list_end_insert(NspList *L, NspObject *A); 
extern int nsp_list_store(NspList *L, NspObject *A, integer n); 
extern void nsp_list_delete_elt_by_name(NspList *L, char *str); 
extern int nsp_list_delete_elt(NspList *L, integer nel); 
extern int nsp_list_delete_cell(NspList *L, integer nel); 
extern int nsp_list_length(NspList *L); 
extern int nsp_list_concat(NspList *L1, NspList *L2); 
extern void nsp_list_info(NspList *L, int indent); 
extern void nsp_list_print(NspList *L, int indent); 
 extern NspObject *ListSearch_Old (NspList *L, String *str); 
 extern NspObject *nsp_list_search(NspList *L, String *str); 
 extern NspObject *nsp_list_search_and_remove(NspList *L, char *str); 
 extern int nsp_list_search_and_replace(NspList *L, NspObject *O); 
 extern void nsp_cell_only_destroy(Cell **c); 
 extern NspObject *nsp_sorted_list_search(NspList *L, String *str); 
 extern NspObject *nsp_sorted_list_search_and_remove(NspList *L, char *str); 
 extern int nsp_sorted_list_insert(NspList *L, NspObject *O); 
 extern NspList *nsp_list_map(NspList *L, NspPList *PL, NspList *args); 
 extern NspObject *nsp_list_fold_right(NspList *L, NspPList *PL, NspList *args); 
extern NspBMatrix *nsp_list_equal(NspList *L1, NspList *L2); 
 extern NspBMatrix *nsp_list_not_equal(NspList *L1, NspList *L2); 
 extern int nsp_list_full_equal(NspList *L1, NspList *L2); 
 extern int nsp_list_full_not_equal(NspList *L1, NspList *L2); 
extern int nsp_list_compact(NspList *L1, char flag );

#endif
