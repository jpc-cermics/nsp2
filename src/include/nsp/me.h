#ifndef NSP_INC_ME 
#define NSP_INC_ME

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for FILE declaration **/
#include "nsp/sciio.h" 

#include <nsp/objectf.h>

/*
 * NspMe inherits from NspObject 
 */

/* typedef struct _NspMe  NspMe; */

typedef struct _NspTypeMe { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeMe;

struct _NspMe {
  /*< private >*/
  NspObject father; 
  NspTypeMe *type; 
  /*< public >*/
  char *path;  
  char *module;
};

extern int nsp_type_me_id;
extern NspTypeMe *nsp_type_me;

int nsp_type_me_init();

/* only useful when building a new class derived from me */

NspTypeMe *new_type_me(type_mode mode);

NspMe *new_me();

/*
 * Object methods redefined for me 
 */

#ifdef Me_Private 
static int init_me(NspMe *ob,NspTypeMe *type);
static int MeSize(NspMe *Mat, int flag);
char *MeType(void);
char *MeShType(NspMe *M);
NspObject *MeLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int MeObjEq(NspObject *A,NspObject *B);
int MeObjNeq(NspObject *A,NspObject *B);
#endif 

#define NULLXME (NspMe *) 0



int MeFullComp(NspMe * A,NspMe * B,char *op,int *err);
NspMe *MeCreate  (char *name);
NspMe *MeCopy      (NspMe *H);
void MeDestroy      (NspMe *H);
void MeInfo      (NspMe *H,int indent);
void MePrint      (NspMe *H,int indent);
NspMe  *MeObj  ( NspObject *O);

#endif

