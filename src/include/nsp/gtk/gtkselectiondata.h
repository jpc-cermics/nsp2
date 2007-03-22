/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSelectionData
#define INC_NSP_GtkSelectionData

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkSelectionData inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkSelectionData ;
typedef NspTypeGBoxed NspTypeGtkSelectionData ;

extern int nsp_type_gtkselectiondata_id;
extern NspTypeGtkSelectionData *nsp_type_gtkselectiondata;

/* type instances for gboxed */

NspTypeGtkSelectionData *new_type_gtkselectiondata(type_mode mode);

/* instance for GtkSelectionData */

NspGtkSelectionData *new_gtkselectiondata();

/*
* Object methods redefined for gtkselectiondata 
*/

#define NULLGTKSELECTIONDATA (NspGtkSelectionData*) 0

NspGtkSelectionData *gtkselectiondata_create(char *name,NspTypeBase *type);

/* from GtkSelectionDataObj.c */

extern NspGtkSelectionData *gtkselectiondata_object (NspObject *O); 
extern int IsGtkSelectionDataObj (Stack stack, int i); 
extern int IsGtkSelectionData(NspObject *O);
extern NspGtkSelectionData *GetGtkSelectionDataCopy (Stack stack, int i); 
extern NspGtkSelectionData *GetGtkSelectionData (Stack stack, int i); 

#endif 

#ifdef GtkSelectionData_Private 
static int init_gtkselectiondata(NspGtkSelectionData *o,NspTypeGtkSelectionData *type);
static char *gtkselectiondata_type_as_string(void);
static char *gtkselectiondata_type_short_string(NspObject *v);
static AttrTab gtkselectiondata_attrs[];
/* static int int_gtkselectiondata_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkselectiondata_get_methods(void); 
#endif /* GtkSelectionData_Private */
