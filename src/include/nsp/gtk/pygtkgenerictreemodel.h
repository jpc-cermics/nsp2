/* -*- Mode: C -*- */
#ifndef INC_NSP_PyGtkGenericTreeModel
#define INC_NSP_PyGtkGenericTreeModel

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPyGtkGenericTreeModel inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPyGtkGenericTreeModel ;
typedef NspTypeGObject NspTypePyGtkGenericTreeModel ;

extern int nsp_type_pygtkgenerictreemodel_id;
extern NspTypePyGtkGenericTreeModel *nsp_type_pygtkgenerictreemodel;

/* type instances for gobject */

NspTypePyGtkGenericTreeModel *new_type_pygtkgenerictreemodel(type_mode mode);

/* instance for PyGtkGenericTreeModel */

NspPyGtkGenericTreeModel *new_pygtkgenerictreemodel();

/*
* Object methods redefined for pygtkgenerictreemodel 
*/

#ifdef PyGtkGenericTreeModel_Private 
static int init_pygtkgenerictreemodel(NspPyGtkGenericTreeModel *o,NspTypePyGtkGenericTreeModel *type);
static char *pygtkgenerictreemodel_type_as_string(void);
static char *pygtkgenerictreemodel_type_short_string(void);
static AttrTab pygtkgenerictreemodel_attrs[];
/* static int int_pygtkgenerictreemodel_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pygtkgenerictreemodel_get_methods(void); 
#endif /* PyGtkGenericTreeModel_Private */

#define NULLPYGTKGENERICTREEMODEL (NspPyGtkGenericTreeModel*) 0

NspPyGtkGenericTreeModel *pygtkgenerictreemodel_create(char *name,NspTypeBase *type);

/* from PyGtkGenericTreeModelObj.c */

extern NspPyGtkGenericTreeModel *pygtkgenerictreemodel_object (NspObject *O); 
extern int IsPyGtkGenericTreeModelObj (Stack stack, int i); 
extern int IsPyGtkGenericTreeModel(NspObject *O);
extern NspPyGtkGenericTreeModel *GetPyGtkGenericTreeModelCopy (Stack stack, int i); 
extern NspPyGtkGenericTreeModel *GetPyGtkGenericTreeModel (Stack stack, int i); 

#endif 
