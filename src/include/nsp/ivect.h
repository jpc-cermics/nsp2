#ifndef NSP_INC_IVECT 
#define NSP_INC_IVECT

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

#include <nsp/objectf.h>

/*
 * NspIVect inherits from NspObject 
 */
/* typedef struct _NspIvect  NspIVect; */

typedef struct _NspTypeIVect { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeIVect;

struct _NspIvect {
  /*< private >*/
  NspObject father; 
  NspTypeIVect *type; 
  /*< public >*/
  int first,step,last ;    /* implicit bounds  */
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
static int nsp_ivect_size(NspIVect *Mat, int flag);
static char *nsp_ivect_type_as_string(void);
static char *nsp_ivect_type_short_string(NspIVect *M);
static int nsp_ivect_eq(NspObject *A,NspObject *B);
static int nsp_ivect_neq(NspObject *A,NspObject *B);
static int nsp_ivect_xdr_save(XDR  *F, NspIVect *M);
static NspIVect *nsp_ivect_xdr_load(XDR  *F);
static int nsp_ivect_as_index(NspIVect * M, index_vector *index);
#endif 


#define NULLIVECT (NspIVect*) 0

/* IVectObj.c */

extern NspIVect *nsp_ivect_object(NspObject *O); 
extern NspObject *nsp_ivect_loop (char *str, NspObject * O, NspObject * O1, int i, int *rep);

/* IVect.c */

extern NspIVect *nsp_ivect_create(char *name, int first, int step, int last, int flag); 
extern int IsIVectF (NspObject *O); 
extern NspIVect *nsp_ivect_copy(NspIVect *A); 
extern void nsp_ivect_destroy(NspIVect *IV); 
extern int nsp_ivect_info(NspIVect *IV, int indent,const char *name, int rec_level); 
extern int nsp_ivect_print(NspIVect *IV, int indent,const char *name, int rec_level); 
extern NspMatrix *nsp_ivect_2_mat(NspIVect *IV); 

extern int IsIVect(NspObject *O);
extern int nsp_ivect_count(NspIVect *iv);
extern int nsp_ivect_count_with_min_max(NspIVect *iv, int *imin, int *imax);
extern int IsIVectObj(Stack stack, int i); 
extern NspIVect *GetIVect (Stack stack, int i);



#endif





