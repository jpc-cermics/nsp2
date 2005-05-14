#ifndef NSP_INC_Matint
#define NSP_INC_Matint

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* interface Matint */

#include "nsp/object.h"

typedef int matint_redim(void *M,int m,int n); 


typedef struct _NspTypeMatint {
  NSP_TYPE_OBJECT__
  /* added */
  matint_redim *redim; 
} NspTypeMatint ; 

#define MAT_INT(t) ((NspTypeMatint *) t)

extern int nsp_type_matint_id;
extern NspTypeMatint *nsp_type_matint;

NspTypeMatint *new_type_matint(type_mode mode);

#ifdef   Matint_Private 

#endif 

#endif 




