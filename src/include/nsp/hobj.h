#ifndef INC_NSP_HOBJ 
#define INC_NSP_HOBJ

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspHobj inherits from NspObject 
 */
typedef struct _nsp_hobj  NspHobj;
typedef int (*hobj_save) (NspFile  *F, NspHobj *M);

typedef struct _nsp_type_Hobj { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  hobj_save *save;
} NspTypeHobj;

struct _nsp_hobj {
  NspObject father; 
  NspTypeHobj *type; 
  char htype  ;   /* 'o' : optional argument 
		  * 'h' : handler 
		  * 'g' : global variable pointer 
		  */
  NspObject *O;        /* pointed object */
};

extern int nsp_type_hobj_id;
extern NspTypeHobj *nsp_type_hobj;

NspTypeHobj *new_type_hobj(type_mode mode);

NspHobj *new_hobj();

/*
 * Object methods redefined for hobj 
 */

#ifdef Hobj_Private 
static int init_hobj(NspHobj *ob,NspTypeHobj *type);
static int nsp_hobj_size(NspHobj *Mat, int flag);
static char *nsp_hobj_type_as_string(void);
static char *nsp_hobj_type_short_string(NspHobj *M);
static int nsp_hobj_eq(NspObject *A,NspObject *B);
static int nsp_hobj_neq(NspObject *A,NspObject *B);
static int nsp_hobj_xdr_save(NspFile  *F, NspHobj *O);
static  int nsp_hobj_is_true(NspHobj *M);
#endif /* Hobj_Private */

#define NULLHOBJ (NspHobj *) 0
#define NULLHOPT (NspHobj *) 0

NspHobj *HobjCreate  (char *name,NspObject *O);
NspHobj *HoptCreate  (char *name,NspObject *O);
NspHobj *GobjCreate  (char *name,NspObject *O);
NspHobj *nsp_hobj_copy(NspHobj *H);
void nsp_hobj_destroy(NspHobj *H);
void nsp_hobj_info(NspHobj *H,int indent);
void nsp_hobj_print(NspHobj *H,int indent);
int IsHobj        (NspObject *O);
int IsHopt        (NspObject *O);
NspHobj  *nsp_hobj_object(NspObject *O);
int IsGlobal      (NspObject *O);

#endif

