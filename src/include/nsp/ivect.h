#ifndef INC_NSP_IVECT 
#define INC_NSP_IVECT

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspIVect inherits from NspObject 
 */
typedef struct _nsp_ivect  NspIVect;


typedef int (*ivect_save) (NspFile  *F, NspIVect *M);

typedef struct _nsp_type_IVect { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  ivect_save *save;
} NspTypeIVect;

struct _nsp_ivect {
  NspObject father; 
  NspTypeIVect *type; 
  double first,step,last ; /* implicit bounds  */
  int flag ;               /* set to 1 if 1:$ or : else set to 0 */
};

extern int nsp_type_ivect_id;
extern NspTypeIVect *nsp_type_ivect;

NspTypeIVect *new_type_ivect(type_mode mode);

NspIVect *new_ivect();

/*
 * Object methods redefined for ivect 
 */

#ifdef IVect_Private 
static int init_ivect(NspIVect *ob,NspTypeIVect *type);
static int IVectSize(NspIVect *Mat, int flag);
char *IVectType(void);
char *IVectShType(NspIVect *M);
NspObject *IVectLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int IVectObjEq(NspObject *A,NspObject *B);
int IVectObjNeq(NspObject *A,NspObject *B);
static int IVectXdrSave(NspFile  *F, NspIVect *M);
static NspIVect *IVectXdrLoad(NspFile  *F);
#endif 


#define NULLIVECT (NspIVect*) 0

/* IVectObj.c */

extern NspIVect *IVectObj (NspObject *O); 

/* IVect.c */

 extern NspIVect *IVectCreate (char *name, double first, double step, double last, int flag); 
 extern int IsIVectF (NspObject *O); 
 extern NspIVect *IVectCopy (NspIVect *A); 
 extern void IVectDestroy (NspIVect *IV); 
 extern void IVectInfo (NspIVect *IV, int indent); 
 extern void IVectPrint (NspIVect *IV, int indent); 
 extern NspMatrix *IVect2Mat (NspIVect *IV); 

#endif





