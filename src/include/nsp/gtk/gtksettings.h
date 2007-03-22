/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSettings
#define INC_NSP_GtkSettings

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkSettings inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkSettings ;
typedef NspTypeGObject NspTypeGtkSettings ;

extern int nsp_type_gtksettings_id;
extern NspTypeGtkSettings *nsp_type_gtksettings;

/* type instances for gobject */

NspTypeGtkSettings *new_type_gtksettings(type_mode mode);

/* instance for GtkSettings */

NspGtkSettings *new_gtksettings();

/*
* Object methods redefined for gtksettings 
*/

#define NULLGTKSETTINGS (NspGtkSettings*) 0

NspGtkSettings *gtksettings_create(char *name,NspTypeBase *type);

/* from GtkSettingsObj.c */

extern NspGtkSettings *gtksettings_object (NspObject *O); 
extern int IsGtkSettingsObj (Stack stack, int i); 
extern int IsGtkSettings(NspObject *O);
extern NspGtkSettings *GetGtkSettingsCopy (Stack stack, int i); 
extern NspGtkSettings *GetGtkSettings (Stack stack, int i); 

#endif 

#ifdef GtkSettings_Private 
static int init_gtksettings(NspGtkSettings *o,NspTypeGtkSettings *type);
static char *gtksettings_type_as_string(void);
static char *gtksettings_type_short_string(NspObject *v);
static AttrTab gtksettings_attrs[];
/* static int int_gtksettings_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtksettings_get_methods(void); 
#endif /* GtkSettings_Private */
