#ifndef INC_NSP_ZZZZ 
#define INC_NSP_ZZZZ

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspZzzz inherits from NspObject 
 */
typedef int (*zzzz_save) (NspFile  *F, NspZzzz *M);

typedef struct _nsp_type_Zzzz { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  zzzz_save *save;
} NspTypeZzzz;

typedef struct _nsp_zzzz {
  NspObject father; 
  NspTypeZzzz *type; 
  
} NspZzzz;

extern int nsp_type_zzzz_id;
extern NspTypeZzzz *nsp_type_zzzz;

int nsp_type_zzzz_init();

/* only useful when building a new class derived from zzzz */

NspTypeZzzz *new_type_zzzz(void) ;

/* initialize type for Object */

int nsp_type_zzzz_init(void);

/* only useful when building a new class derived from zzzz */

void nsp_type_zzzz_set(NspZzzz *zzzz, NspTypeZzzz *type);

NspZzzz *new_zzzz();

/*
 * Object methods redefined for zzzz 
 */

#ifdef Zzzz_Private 
static int init_zzzz(NspZzzz *ob,NspTypeZzzz *type);

static int ZzzzSize(NspZzzz *Mat, int flag);
char *ZzzzType(void);
char *ZzzzShType(NspZzzz *M);
NspObject *ZzzzLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int ZzzzObjEq(NspObject *A,NspObject *B);
int ZzzzObjNeq(NspObject *A,NspObject *B);
#endif 

#define NULLMAT (NspZzzz*) 0
