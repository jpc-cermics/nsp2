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
static int ListSize(NspList *Mat, int flag);
char *ListType(void);
char *ListShType(NspList *M);
NspObject *list_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int ListObjEq(NspObject *A,NspObject *B);
int ListObjNeq(NspObject *A,NspObject *B);
static int ListIsTrue(NspList *l);
NspList*ListXdrLoad(NspFile  *F);
int ListXdrSave(NspFile  *F, NspList *L);
#endif 

#define NULLLIST ( NspList *) 0 
#define NULLCELL ( Cell *) 0 
#define NULLOBJ  ( NspObject *) 0 

/** Functions declaration **/

 extern NspObject *ListPathExtract (NspList *L, NspObject *O); 
 extern int ListObjEq (NspObject *A, NspObject *B); 
 extern int ListObjNeq (NspObject *A, NspObject *B); 
 extern NspList *ListObj (NspObject *O); 
 extern int IsListObj (Stack stack, int i); 
 extern int IsList (NspObject *O); 
 extern NspList *GetListCopy (Stack stack, int i); 
 extern NspList *GetList (Stack stack, int i); 

extern NspObject *EvalMacro (NspPList *, NspObject **, NspList *, int *);
 extern NspList *EListCreate (char *name, char *tname); 
 extern Cell *NewCell (char *name, NspObject *O); 
 extern void CellDestroy (Cell **c); 
 extern void ListDestroy (NspList *l); 
 extern NspList *ListCopy (NspList *L); 
 extern NspList *ListExtract (NspList *L, NspMatrix *Elts); 
 extern int ListInsert (NspList *L, NspObject *O, int n); 
 extern NspObject *NthElement (NspList *L, integer nel); 
 extern int EndInsert (NspList *L, NspObject *A); 
 extern int NInsert (NspList *L, NspObject *A, integer n); 
 extern void ListDeleteNamed (NspList *L, char *str); 
 extern int DeleteNth (NspList *L, integer nel); 
 extern int DeleteNthCellOnly (NspList *L, integer nel); 
 extern int ListLength (NspList *L); 
 extern int ConcatList (NspList *L1, NspList *L2); 
 extern void ListInfo (NspList *L, int indent); 
 extern void ListPrint (NspList *L, int indent); 
 extern NspObject *ListSearch_Old (NspList *L, String *str); 
 extern NspObject *ListSearch (NspList *L, String *str); 
 extern NspObject *ListSearchAndRemove (NspList *L, char *str); 
 extern int ListSearchAndReplace (NspList *L, NspObject *O); 
 extern void CellDestroyButNotObj (Cell **c); 
 extern NspObject *SortedListSearch (NspList *L, String *str); 
 extern NspObject *SortedListSearchAndRemove (NspList *L, char *str); 
 extern int SortedListInsert (NspList *L, NspObject *O); 
 extern NspList *ListMap (NspList *L, NspPList *PL, NspList *args); 
 extern NspObject *ListFoldRight (NspList *L, NspPList *PL, NspList *args); 
extern NspBMatrix *ListEqual (NspList *L1, NspList *L2); 
 extern NspBMatrix *ListNequal (NspList *L1, NspList *L2); 
 extern int ListFullEqual (NspList *L1, NspList *L2); 
 extern int ListFullNequal (NspList *L1, NspList *L2); 

#endif
