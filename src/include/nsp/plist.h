#ifndef INC_NSP_PList 
#define INC_NSP_PList

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspPList inherits from NspObject 
 */
typedef struct _nsp_plist  NspPList;

typedef int (*plist_save) (NspFile  *F, NspPList *M);

typedef struct _nsp_type_PList { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  plist_save *save;
} NspTypePList;


/*****************************************************************
 *  Parse List : Used to store parsed expressions 
 *  Element can be one of 
 *      short int   giving the code of an operator 
 *      char *name  where name is a Scilab object name (func|macro|matrix)
 *  PList 
 *****************************************************************/

typedef struct parse_cell {
  struct parse_cell *prev;  /* points to the previous cell or 0 */
  struct parse_cell *next;  /* points to the next cell or 0 */
  void *O;                  /* points to the stored object or can be used to store line number
			       for operators */
  int   type;               /* type of object */
  short int arity ;         /* used to store arity of operators */
} PCell,*PList ; /** definition for a Parse Cell and a ParseList **/

/*****************************************************************
 *  Store PList in the data structure of Scilab 
 *****************************************************************/

struct _nsp_plist {
  NspObject father; 
  NspTypePList *type; 
  char *file_name;  /* NULL or points to the function source file-name */
  PList D;          /* points to the parsed expression */
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
NspPList *NspPListXdrLoad(NspFile  *F);
int NspPListXdrSave(NspFile  *F, NspPList *M);
#endif 


#define NULLP_PLIST (NspPList*) 0 
#define NULLPLIST (PList) 0 

/** Functions declaration **/

/* NspPListObj.c */
extern NspPList *NspPListObj(NspObject *O);
extern int IsNspPListObj (Stack stack, int i); 
extern int IsNspPList (NspObject *O); 
extern NspPList *GetNspPListCopy (Stack stack, int i); 
extern NspPList *GetNspPList (Stack stack, int i); 

/* P_PList.c */

extern NspPList *NspPListCreate (char *name, PList L, char *filename); 
extern NspPList *NspPListCopy (NspPList *A); 
extern void NspPListDestroy (NspPList *P_L); 
extern void NspPListPrInt (NspPList *P_L); 
extern void NspPListInfo (NspPList *P_L, int indent); 
extern void NspPListPrint (NspPList *P_L, int indent); 
extern int NspPListSave (NspPList *P_L); 
extern NspSMatrix * NspPList2SMatrix (NspPList *P_L, int indent); 

/* PList.c */

 extern int ParseAdd (PList *plist, int op, int arity, int line);
 extern int ParseAddName (PList *plist, char *str);
 extern int ParseAddString (PList *plist, char *str);
 extern int ParseAddComment (PList *plist, char *str);
 extern int ParseAddList (PList *plist, PList *l);
 extern int ParseAppend (PList *plist, PList *l);
 extern int ParseAddList1 (PList *plist, PList *l);
 extern int ParseAddDoubleI (PList *plist, char *str);
 extern int ParseAddDouble (PList *plist);
 extern PList EPListCreate (void);
 extern void PListDestroy (PList *List);
 extern PList PListCopy (PList L);
 extern PList Last (PList plist);
 extern void PListPrInt_I (PList L, int indent);
 extern void PListPrInt (PList L);
 extern PList FirstEl (PList L);
 extern int PrettyPrintOPname (int type, int indent, int pos);
 extern void PListPrettyPrint (PList L, int indent);
 extern int ArgPrettyPrint (PList L, int i, int pos, int posret);
 extern void PListPrint (PList L, int indent);
 extern void ArgPrint (PList L, int i);
 extern void PListInfo (PList L, int indent);
 extern void ShowLine (PList L);
 extern int PListSave (PList L);
 extern int PListLoad (PList *L);

#endif /*  PLIST_H  */

