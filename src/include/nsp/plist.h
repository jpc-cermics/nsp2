#ifndef NSP_INC_PList 
#define NSP_INC_PList

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include <glib.h> 
#include "nsp/sciio.h" 
#include "nsp/object.h"

/*
 * NspPList inherits from NspObject 
 */
typedef struct _NspPlist  NspPList;

typedef struct _NspTypePList NspTypePList;

struct _NspTypePList { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};


/*
 *  Parse List : Used to store parsed expressions 
 *  Element can be one of 
 *      short int   giving the code of an operator 
 *      char *name  where name is a Scilab object name (func|macro|matrix)
 *  PList 
 */

typedef struct parse_cell PCell;
typedef struct parse_cell *PList;

struct parse_cell {
  struct parse_cell *prev;  /* points to the previous cell or 0 */
  struct parse_cell *next;  /* points to the next cell or 0 */
  void *O;                  /* points to the stored object or can be used to store line number
			       for operators */
  int   type;               /* type of object */
  short int arity ;         /* used to store arity of operators */
};

/*
 * used for storing numbers a string and a double 
 */

typedef struct _parse_double parse_double ;

struct _parse_double {
  double val ; /* the value of the parsed double */
  nsp_string str;
};

/*
 * used for storing integers a string and an integer.
 */

typedef struct _parse_int parse_int ;

struct _parse_int {
  union { 
    gint32 Gint32;
    guint32 Guint32;
    gint64 Gint64;
    guint64 Guint64;
  };
  nsp_string str;
};


/*
 *  Store PList in the data structure of Scilab 
 */

struct _NspPlist {
  /*< private >*/
  NspObject father; 
  NspTypePList *type; 
  /*< public >*/
  char *file_name;  /* NULL or points to the function source file-name */
  PList D;          /* points to the parsed expression */
  int dir;          /* indice in directory array or -1 */
};

extern int nsp_type_plist_id;
extern NspTypePList *nsp_type_plist;

NspTypePList *new_type_plist(type_mode mode);

NspPList *new_plist();

/*
 * Object methods redefined for plist 
 */

#ifdef PList_Private 
static int init_plist(NspPList *ob,NspTypePList *type);
int NspPListSize(NspPList *Mat, int flag);
char *NspPListType(void);
char *NspPListShType(void);
NspObject *NspPListLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int NspPListObjEq(NspObject *A,NspObject *B);
int NspPListObjNeq(NspObject *A,NspObject *B);
NspPList *NspPListXdrLoad(XDR *xdrs);
int NspPListXdrSave(XDR *xdrs, NspPList *M);
#endif 


#define NULLP_PLIST (NspPList*) 0 
#define NULLPLIST (PList) 0 

/* Functions declaration **/

/* NspPListObj.c */
extern NspPList *NspPListObj(NspObject *O);
extern int IsNspPListObj (Stack stack, int i); 
extern int IsNspPList (NspObject *O); 
extern NspPList *GetNspPListCopy (Stack stack, int i); 
extern NspPList *GetNspPList (Stack stack, int i); 


/* P_PList.c */

extern NspPList *NspPListCreate (char *name, PList L, char *filename); 
extern NspPList *NspPListCopy (NspPList *A); 
extern NspPList *NspPListCopy_no_local_vars(NspPList *A);

extern void NspPListDestroy (NspPList *P_L); 
extern void NspPListPrInt (NspPList *P_L); 
extern void NspPListInfo (NspPList *P_L, int indent,const char *name, int rec_level); 
extern void NspPListPrint (NspPList *P_L, int indent,const char *name, int rec_level); 
extern int NspPListSave (NspPList *P_L); 
extern NspSMatrix * NspPList2SMatrix (NspPList *P_L, int indent); 

/* PList.c */

extern int nsp_parse_add(PList *plist, int op, int arity, int line);
extern int nsp_parse_add_last(PList *plist, int op, int arity, int line);
extern int nsp_parse_add_name(PList *plist, char *str);
extern int nsp_parse_add_name1(PList *plist, char *str, int arity);
extern int nsp_parse_add_string(PList *plist, char *str);
extern int nsp_parse_add_comment(PList *plist, char *str);
extern int nsp_parse_add_list(PList *plist, PList *l);
extern int nsp_parse_append(PList *plist, PList *l);
extern int nsp_parse_add_list1(PList *plist, PList *l);
extern int nsp_parse_add_doublei(PList *plist, char *str);
extern int nsp_parse_add_opname(PList *plist, char *str);
extern int nsp_parse_add_object(PList *plist, NspObject *obj );
extern int nsp_parse_add_inti(PList *plist, char *str, int type);


extern int ParseAddDouble (PList *plist);
extern PList nsp_eplist_create(void);
extern void nsp_plist_destroy(PList *List);
extern PList nsp_plist_copy(PList L);
extern PList nsp_plist_copy_no_local_vars(PList L);
extern PList nsp_last(PList plist);
extern void nsp_plist_print_internal(PList L);
extern PList nsp_firstel(PList L);
extern int nsp_pretty_print_opname(int type, int indent, int pos);
extern void nsp_plist_pretty_print(PList L, int indent);
extern int nsp_arg_pretty_print(PList L, int i, int pos, int posret);
extern void nsp_plist_print(PList L, int indent);
extern void nsp_plist_info(PList L, int indent);
extern void ShowLine (PList L);
extern int nsp_plist_save(PList L);
extern int nsp_plist_load(PList *L);
extern NspSMatrix *nsp_plist2smatrix(PList L, int indent) ;
extern void plist_get_nargs(PList List,int *lhs , int *rhsp1);

#endif /*  PLIST_H  */

