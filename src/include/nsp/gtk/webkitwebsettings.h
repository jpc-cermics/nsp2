/* -*- Mode: C -*- */
#ifndef INC_NSP_WebKitWebSettings
#define INC_NSP_WebKitWebSettings

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspWebKitWebSettings inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspWebKitWebSettings ;
typedef NspTypeGObject NspTypeWebKitWebSettings ;

extern int nsp_type_webkitwebsettings_id;
extern NspTypeWebKitWebSettings *nsp_type_webkitwebsettings;

/* type instances for gobject */

NspTypeWebKitWebSettings *new_type_webkitwebsettings(type_mode mode);

/* instance for WebKitWebSettings */

NspWebKitWebSettings *new_webkitwebsettings();

/*
* Object methods redefined for webkitwebsettings 
*/

#define NULLWEBKITWEBSETTINGS (NspWebKitWebSettings*) 0

NspWebKitWebSettings *webkitwebsettings_create(char *name,NspTypeBase *type);

/* from WebKitWebSettingsObj.c */

extern NspWebKitWebSettings *webkitwebsettings_object (NspObject *O); 
extern int IsWebKitWebSettingsObj (Stack stack, int i); 
extern int IsWebKitWebSettings(NspObject *O);
extern NspWebKitWebSettings *GetWebKitWebSettingsCopy (Stack stack, int i); 
extern NspWebKitWebSettings *GetWebKitWebSettings (Stack stack, int i); 

#endif 

#ifdef WebKitWebSettings_Private 
static int init_webkitwebsettings(NspWebKitWebSettings *o,NspTypeWebKitWebSettings *type);
static char *webkitwebsettings_type_as_string(void);
static char *webkitwebsettings_type_short_string(NspObject *v);
static AttrTab webkitwebsettings_attrs[];
/* static int int_webkitwebsettings_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *webkitwebsettings_get_methods(void); 
#endif /* WebKitWebSettings_Private */
