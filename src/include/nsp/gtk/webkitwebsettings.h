/* -*- Mode: C -*- */
#ifndef NSP_INC_NspWebKitWebSettings
#define NSP_INC_NspWebKitWebSettings

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* NspWebKitWebSettings */

#include <nsp/gtk/gobject.h>

/*
 * NspWebKitWebSettings inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspWebKitWebSettings ;
typedef NspTypeGObject NspTypeWebKitWebSettings ;

extern int nsp_type_webkitwebsettings_id;
extern NspTypeWebKitWebSettings *nsp_type_webkitwebsettings;

/* type instances for gobject */

NspTypeWebKitWebSettings *new_type_webkitwebsettings(type_mode mode);

/* instance for NspWebKitWebSettings */

NspWebKitWebSettings *new_webkitwebsettings();

/*
 * Object methods redefined for webkitwebsettings 
 */

#define NULLWEBKITWEBSETTINGS (NspWebKitWebSettings*) 0


/* from NspWebKitWebSettingsObj.c */

extern NspWebKitWebSettings *nsp_webkitwebsettings_object (NspObject *O);
extern int IsWebKitWebSettingsObj (Stack stack, int i);
extern int IsWebKitWebSettings(NspObject *O);
extern NspWebKitWebSettings *GetWebKitWebSettingsCopy (Stack stack, int i);
extern NspWebKitWebSettings *GetWebKitWebSettings (Stack stack, int i);

#endif /* NSP_INC_NspWebKitWebSettings */ 

#ifdef NspWebKitWebSettings_Private 
static int init_webkitwebsettings(NspWebKitWebSettings *o,NspTypeWebKitWebSettings *type);
static char *nsp_webkitwebsettings_type_as_string(void);
static char *nsp_webkitwebsettings_type_short_string(NspObject *v);
static AttrTab webkitwebsettings_attrs[];
static NspMethods *webkitwebsettings_get_methods(void);
/* static int int_webkitwebsettings_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspWebKitWebSettings_Private */
