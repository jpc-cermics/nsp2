#ifndef NSP_INC_LIST 
#define NSP_INC_LIST

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *  Bruno Pinçon Esial/Iecn
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspList inherits from NspObject 
 */

typedef struct _NspList  NspList;

typedef struct _NspTypeList { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeList;

typedef struct cell Cell ;

struct cell {
  struct cell *prev;  /* points to the previous cell or 0 */
  struct cell *next;  /* points to the next cell or 0 */
  NspObject *O;             /* points to the stored object */
} ;

struct _NspList {
  /*< private >*/
  NspObject father; 
  NspTypeList *type; 
  /*< public >*/
  Cell *first;        /* pointer to the first cell */
  Cell *last;         /* pointer to the last cell */
  Cell *current;      /* pointer to the current cell */
  int nel;            /* number of list elements */
  int icurrent;       /* index of the current cell */
};

extern int nsp_type_list_id;
extern NspTypeList *nsp_type_list;

/* only useful when building a new class derived from list */

NspTypeList *new_type_list(type_mode mode);

NspList *new_list();

/*
 * Object methods redefined for list 
 */

#define NULLLIST ( NspList *) 0 
#define NULLCELL ( Cell *) 0 
#define NULLOBJ  ( NspObject *) 0 

/* Functions declaration */

extern NspObject *nsp_list_path_extract(NspList *L,int n, NspObject **Objs, int *copy); 
extern NspList *nsp_list_object(NspObject *O); 
extern int IsListObj (Stack stack, int i); 
extern int IsList (NspObject *O); 
extern NspList *GetListCopy (Stack stack, int i); 
extern NspList *GetList (Stack stack, int i); 
extern int ListFollowExtract(Stack stack, int rhs, int opt, int lhs);
extern NspObject *nsp_eval_macro_code(NspPList *, NspObject **, NspList *, int *);
extern NspList *nsp_list_create(const char *name); 
extern Cell *nsp_cell_create(NspObject *O); 
extern void nsp_cell_destroy(Cell **c); 
extern void nsp_list_destroy(NspList *l); 
extern void nsp_list_destroy_bis(NspList *l);
extern NspList *nsp_list_copy(NspList *L); 
extern NspList *nsp_list_full_copy(NspList *L); 
extern NspList *nsp_list_extract(NspList *L, NspMatrix *Elts); 
extern int nsp_list_insert(NspList *L, NspObject *O, int n); 
extern NspObject *nsp_list_get_element(NspList *L, int n);
extern int nsp_list_begin_insert(NspList *L, NspObject *A);
extern int nsp_list_end_insert(NspList *L, NspObject *A); 
extern void  nsp_list_remove_first(NspList *L);
extern void  nsp_list_remove_last(NspList *L);
extern int nsp_list_store(NspList *L, NspObject *A, int n); 
extern void nsp_list_delete_elt_by_name(NspList *L, char *str); 
extern int nsp_list_delete_elt(NspList *L, int nel); 
extern int nsp_list_delete_cell(NspList *L, int nel); 
extern int nsp_list_length(NspList *L); 
extern int nsp_list_concat(NspList *L1, NspList *L2); 
extern int nsp_list_info(NspList *L, int indent,char *name, int rec_level); 
extern int nsp_list_print(NspList *L, int indent,char *name, int rec_level); 
extern void nsp_list_latex_print(NspList *L);
extern void nsp_cell_only_destroy(Cell **c); 
extern NspObject *nsp_sorted_list_search(NspList *L, nsp_const_string str); 
extern NspObject *nsp_sorted_list_search_and_remove(NspList *L,nsp_const_string str); 
extern int nsp_sorted_list_insert(NspList *L, NspObject *O); 
extern NspList *nsp_list_map(NspList *L, NspPList *PL, NspList *args); 
extern NspObject *nsp_list_fold_right(NspList *L,NspObject *x, NspPList *PL, NspList *args); 
extern NspObject *nsp_list_fold_left(NspList *L, NspObject *x,NspPList *PL, NspList *largs);
extern NspBMatrix *nsp_list_equal(NspList *L1, NspList *L2); 
extern NspBMatrix *nsp_list_not_equal(NspList *L1, NspList *L2); 
extern int nsp_list_full_equal(NspList *L1, NspList *L2); 
extern int nsp_list_full_not_equal(NspList *L1, NspList *L2); 
extern int nsp_list_compact(NspList *L1, char flag );
extern Cell *nsp_list_get_cell_pointer(NspList *L, int n);
extern NspList *nsp_list_unique(NspList *L, NspObject **Ind, NspMatrix **Occ, char ind_type);
extern Boolean nsp_list_has(NspList *L, NspObject *Obj, int *ind);
extern void nsp_remove_cell_from_list(NspList *L, Cell *Loc);

#endif

/* private definitions */


#ifdef List_Private
static int init_list(NspList *ob,NspTypeList *type);
static int nsp_list_size(NspList *Mat, int flag);
static char *nsp_list_type_as_string(void);
static char *nsp_list_type_short_string(NspList *M);
static NspObject *list_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int nsp_list_eq(NspObject *A,NspObject *B);
static int nsp_list_neq(NspObject *A,NspObject *B);
static int nsp_list_is_true(NspList *l);
static NspList *nsp_list_xdr_load(XDR  *F);
static int nsp_list_xdr_save(XDR  *F, NspList *L);
static NspMethods *nsp_list_get_methods(void);
#endif 
