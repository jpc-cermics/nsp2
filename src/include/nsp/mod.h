#ifndef NSP_INC_MOD 
#define NSP_INC_MOD

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for FILE declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspMod inherits from NspObject 
 */

typedef struct _NspMod NspMod;

typedef struct _NspTypeMod { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeMod;


/*
 * Module Object 
 * A pointer to a path 
 * A module name 
 * A Hash table with name entries 
 * A pointer to a List of Modules 
 */
typedef NspList Lmo ; /* Lmo is a List */

struct _NspMod {
  /*< private >*/
  NspObject father; 
  NspTypeMod *type; 
  /*< public >*/
  char *path;   /* absolute XXX path of the module */ 
  char *mname;  /* module name */ 
  NspHash *T;      /* hash table to store module objects of type me 
		   (module elts) */
  Lmo *L;       /* List of submodules */
  int flag;     /* are we a copy ? */ 
};

extern int nsp_type_mod_id;
extern NspTypeMod *nsp_type_mod;

int nsp_type_mod_init();

/* only useful when building a new class derived from mod */

NspTypeMod *new_type_mod(type_mode mode);

NspMod *new_mod();

/*
 * Object methods redefined for mod 
 */

#ifdef Mod_Private 
static int init_mod(NspMod *ob,NspTypeMod *type);
int ModSize(NspMod *Mat, int flag);
char *ModType(void);
char *ModShType(NspMod *M);
NspObject *ModLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int ModObjEq(NspObject *A,NspObject *B);
int ModObjNeq(NspObject *A,NspObject *B);
#endif 

#define NULLMOD (NspMod *) 0

int ModFullComp(NspMod * A,NspMod * B,char *op,int *err);
NspMod *ModCreate(char *name,char *dir,char *mname);
NspMod *ModCopy      (NspMod *H);
void ModDestroy      (NspMod *H);
void ModInfo      (NspMod *H,int indent);
void ModPrint      (NspMod *H,int indent);
int ModEnter  (NspMod *H,NspObject *O);
int ModEnterCopy  (NspMod *H,NspObject *O);
void ModDelete (NspMod *H,char *str);
int ModFind    (NspMod *H,char *str,NspObject **O);
int ModFindCopy    (NspMod *H,char *str,NspObject **O);
int ModMerge   (NspMod *H1,NspMod*H2);
int ModNextObj (NspMod *H,int *i,  NspObject **O);

int ModFill(NspMod *Mo);

#endif
