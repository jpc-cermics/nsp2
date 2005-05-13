#ifndef INC_NSP_LMO 
#define INC_NSP_LMO

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /* for FILE declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspLmo inherits from NspObject 
 */

typedef struct _NspLmo  NspLmo;

typedef int (*lmo_save) (NspFile  *F, NspLmo *M);

typedef struct _NspTypeLmo { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  lmo_save *save;
} NspTypeLmo;

struct _NspLmo {
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

#define NULLLMO ( NspLmo *) 0 

/* Functions declaration **/

NspObject *module_path_search_name(NspList *L,NspSMatrix *Sm,char **oname);
NspObject *module_path_search_object(NspList *L,NspSMatrix *Sm,char **oname);

#endif
