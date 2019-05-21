/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSettings
#define NSP_INC_NspGSettings

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

/* NspGSettings */

#include <nsp/gtk/gobject.h>

/*
 * NspGSettings inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSettings ;
typedef NspTypeGObject NspTypeGSettings ;

extern int nsp_type_gsettings_id;
extern NspTypeGSettings *nsp_type_gsettings;

/* type instances for gobject */

NspTypeGSettings *new_type_gsettings(type_mode mode);

/* instance for NspGSettings */

NspGSettings *new_gsettings();

/*
 * Object methods redefined for gsettings 
 */

#define NULLGSETTINGS (NspGSettings*) 0


/* from NspGSettingsObj.c */

extern NspGSettings *nsp_gsettings_object (NspObject *O);
extern int IsGSettingsObj (Stack stack, int i);
extern int IsGSettings(NspObject *O);
extern NspGSettings *GetGSettingsCopy (Stack stack, int i);
extern NspGSettings *GetGSettings (Stack stack, int i);

#endif /* NSP_INC_NspGSettings */ 

#ifdef NspGSettings_Private 
static int init_gsettings(NspGSettings *o,NspTypeGSettings *type);
static char *nsp_gsettings_type_as_string(void);
static char *nsp_gsettings_type_short_string(NspObject *v);
static AttrTab gsettings_attrs[];
static NspMethods *gsettings_get_methods(void);
/* static int int_gsettings_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSettings_Private */
