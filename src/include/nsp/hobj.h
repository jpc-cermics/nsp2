#ifndef NSP_INC_HOBJ 
#define NSP_INC_HOBJ

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

#include "nsp/object.h"

/*
 * NspHobj inherits from NspObject 
 */
typedef struct _NspHobj  NspHobj;

typedef struct _NspTypeHobj { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeHobj;

struct _NspHobj {
  /*< private >*/
  NspObject father; 
  NspTypeHobj *type; 
  /*< public >*/
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


#define NULLHOBJ (NspHobj *) 0
#define NULLHOPT (NspHobj *) 0

NspHobj *HobjCreate  (const char *name,NspObject *O);
NspHobj *HoptCreate  (const char *name,NspObject *O);
NspHobj *GobjCreate  (const char *name,NspObject *O);
NspHobj *nsp_hobj_copy(NspHobj *H);
void nsp_hobj_destroy(NspHobj *H);
int nsp_hobj_info(NspHobj *H,int indent,char *name, int rec_level);
int nsp_hobj_print(NspHobj *H,int indent,char *name, int rec_level);
int IsHobj        (NspObject *O);
int IsHopt        (NspObject *O);
NspHobj  *nsp_hobj_object(NspObject *O);
int IsGlobal      (NspObject *O);

#define HOBJ_GET_OBJECT(Obj,rep)					\
  if (check_cast (Obj, nsp_type_hobj_id) == TRUE)			\
    {									\
      if (((NspHobj *)Obj)->htype != 'g') Obj = ((NspHobj *) Obj)->O;	\
      else {								\
	if ((Obj= nsp_global_frame_search_object(NSP_OBJECT(Obj)->name)) == NULLOBJ) \
	  {								\
	    Scierror("Pointer to a global non existant variable\n");	\
	    return rep;							\
	  }								\
      }									\
    } 

#endif /* NSP_INC_HOBJ */

#ifdef Hobj_Private 
/* Private part 
 * Object methods redefined for hobj 
 */
static int init_hobj(NspHobj *ob,NspTypeHobj *type);
static int nsp_hobj_size(NspHobj *Mat, int flag);
static char *nsp_hobj_type_as_string(void);
static char *nsp_hobj_type_short_string(NspHobj *M);
static int nsp_hobj_eq(NspObject *A,NspObject *B);
static int nsp_hobj_neq(NspObject *A,NspObject *B);
static int nsp_hobj_xdr_save(XDR  *F, NspHobj *O);
static  int nsp_hobj_is_true(NspHobj *M);
#endif /* Hobj_Private */
