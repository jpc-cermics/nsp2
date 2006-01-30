#ifndef NSP_INC_Matint
#define NSP_INC_Matint

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* interface Matint */

#include "nsp/object.h"

typedef int matint_redim(void *M,int m,int n); 
typedef int matint_resize(void *M,int m,int n); 
typedef int matint_free_elt(void **elt);
typedef unsigned int matint_elt_size(void *M);

typedef struct _NspTypeMatint {
  NSP_TYPE_OBJECT__
  /* added */
  matint_redim *redim; 
  matint_resize *resize;
  matint_free_elt *free_elt;
  matint_elt_size *elt_size;
} NspTypeMatint ; 

#define MAT_INT(t) ((NspTypeMatint *) t)

extern int nsp_type_matint_id;
extern NspTypeMatint *nsp_type_matint;

NspTypeMatint *new_type_matint(type_mode mode);
extern NspMethods *matint_get_methods(void);

extern int nsp_matint_delete_columns(NspObject  *Obj, NspMatrix *Cols);
extern int nsp_matint_delete_rows(NspObject *Obj, NspMatrix *Rows);
extern int nsp_matint_delete_elements(NspObject *Obj, NspMatrix *Elts);
extern int nsp_matint_delete_elements2(NspObject *Obj, NspMatrix *EltsR, NspMatrix *EltsC);

#endif 

#ifdef   Matint_Private 
static char *matint_type_as_string (void);
#endif 

