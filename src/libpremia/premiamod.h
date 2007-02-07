/* -*- Mode: C -*- */
#ifndef NSP_INC_PremiaModel
#define NSP_INC_PremiaModel

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* PremiaModel */

#include "nsp/object.h"
#include "/usr/local/Premia/Src/common/optype.h" 
#include "/usr/local/Premia/Src/common/var.h" 
#include "/usr/local/Premia/Src/common/tools.h" 

/*
 * NspPremiaModel inherits from NspObject
 */

typedef struct _NspPremiaModel NspPremiaModel ;
typedef struct _NspTypePremiaModel NspTypePremiaModel ;

struct _NspTypePremiaModel {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_premiamodel nsp_premiamodel;
struct _nsp_premiamodel {
  int ref_count;
  Model mod;
  Option opt;
  PricingMethod meth;
  int compat ;
};

struct _NspPremiaModel {
  /*< private >*/
  NspObject father;
  NspTypePremiaModel*type;
  /*< public >*/
  nsp_premiamodel *obj;
};

extern int nsp_type_premiamodel_id;
extern NspTypePremiaModel *nsp_type_premiamodel;

/* type instances for object */

NspTypePremiaModel *new_type_premiamodel(type_mode mode);

/* instance for PremiaModel */

NspPremiaModel *new_premiamodel();

/*
* Object methods redefined for premiamodel 
*/


#define NULLPREMIAMODEL (NspPremiaModel*) 0

extern NspPremiaModel *premiamodel_create(char *name,NspTypeBase *type);

/* from PremiaModelObj.c */

extern NspPremiaModel *nsp_premiamodel_copy(NspPremiaModel *H);
extern void nsp_premiamodel_destroy(NspPremiaModel *H);
void nsp_premiamodel_print(NspPremiaModel *M,int indent,const char *name, int rec_level);
void nsp_premiamodel_info(NspPremiaModel *M, int indent,const char *name, int rec_level);
extern NspPremiaModel *nsp_premiamodel_object (NspObject *O); 
extern int IsPremiaModelObj (Stack stack, int i); 
extern int IsPremiaModel(NspObject *O);
extern NspPremiaModel *GetPremiaModelCopy (Stack stack, int i); 
extern NspPremiaModel *GetPremiaModel (Stack stack, int i); 
int int_premiamodel_create(Stack stack, int rhs, int opt, int lhs);

#endif /* NSP_INC_PremiaModel */ 

#ifdef PremiaModel_Private 
static int init_premiamodel(NspPremiaModel *o,NspTypePremiaModel *type);
static int nsp_premiamodel_size(NspPremiaModel *Mat, int flag);
static char *nsp_premiamodel_type_as_string(void);
static char *nsp_premiamodel_type_short_string(void);
static int nsp_premiamodel_eq(NspPremiaModel *A, NspObject *B);
static int nsp_premiamodel_neq(NspPremiaModel *A, NspObject *B);
static AttrTab premiamodel_attrs[];
static NspMethods *premiamodel_get_methods(void);
static NspPremiaModel *premiamodel_create_void(char *name,NspTypeBase *type);
#endif /* PremiaModel_Private */

