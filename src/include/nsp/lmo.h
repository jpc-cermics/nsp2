#ifndef INC_NSP_LMO 
#define INC_NSP_LMO

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for FILE declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspLmo inherits from NspObject 
 */

typedef struct _nsp_lmo  NspLmo;

typedef int (*lmo_save) (NspFile  *F, NspLmo *M);

typedef struct _nsp_type_Lmo { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  lmo_save *save;
} NspTypeLmo;

struct _nsp_lmo {
  NspObject father; 
  NspTypeLmo *type; 
  char *path;  
  char *module;
} ;

extern int nsp_type_lmo_id;
extern NspTypeLmo *nsp_type_lmo;

NspTypeLmo *new_type_lmo(type_mode mode);

NspLmo *new_lmo();

/*
 * Object methods redefined for lmo 
 */

#ifdef Lmo_Private

static int init_lmo(NspLmo *ob,NspTypeLmo *type);

static int LmoSize(NspLmo *Mat, int flag);
char *LmoType(void);
char *LmoShType(NspLmo *M);
NspObject *LmoLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int LmoObjEq(NspObject *A,NspObject *B);
int LmoObjNeq(NspObject *A,NspObject *B);
#endif 


NspLmo *LmoCreate  (char *name);
NspLmo *LmoCopy      (NspLmo *H);
void LmoDestroy      (NspLmo *H);
void LmoInfo      (NspLmo *H,int indent);
void LmoPrint      (NspLmo *H,int indent);
NspLmo  *LmoObj  ( NspObject *O);
int LmoFullComp(NspLmo * A,NspLmo * B,char *op,int *err);

#define NULLLMO ( NspLmo *) 0 
#define NULLCELL ( Cell *) 0 
#define NULLOBJ  ( NspObject *) 0 

/** Functions declaration **/

NspLmo *ELmoCreate  (char *name);
void LmoDestroy  (NspLmo *l);
NspLmo *LmoCopy  (NspLmo *L);
int LmoLength  (NspLmo *L);
void LmoInfo  (NspLmo *L,int indent);
void LmoPrint  (NspLmo *L,int indent);
NspLmo  *LmoObj  (NspObject *O);
int LmoInsertLast(NspLmo *L,char *dir,char **Mname);
NspObject * LmoSearchName(NspLmo *L,char **Mname);
int LmoImport(NspLmo *L,char *dir,char **Mname);

NspObject *lmo_path_search_name(NspLmo *L,NspSMatrix *Sm,char **oname);
NspObject *lmo_path_search_object(NspLmo *L,NspSMatrix *Sm,char **oname);

#endif
