/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceSearchSettings
#define NSP_INC_NspGtkSourceSearchSettings

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

/* NspGtkSourceSearchSettings */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceSearchSettings inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceSearchSettings ;
typedef NspTypeGObject NspTypeGtkSourceSearchSettings ;

extern int nsp_type_gtksourcesearchsettings_id;
extern NspTypeGtkSourceSearchSettings *nsp_type_gtksourcesearchsettings;

/* type instances for gobject */

NspTypeGtkSourceSearchSettings *new_type_gtksourcesearchsettings(type_mode mode);

/* instance for NspGtkSourceSearchSettings */

NspGtkSourceSearchSettings *new_gtksourcesearchsettings();

/*
 * Object methods redefined for gtksourcesearchsettings 
 */

#define NULLGTKSOURCESEARCHSETTINGS (NspGtkSourceSearchSettings*) 0


/* from NspGtkSourceSearchSettingsObj.c */

extern NspGtkSourceSearchSettings *nsp_gtksourcesearchsettings_object (NspObject *O);
extern int IsGtkSourceSearchSettingsObj (Stack stack, int i);
extern int IsGtkSourceSearchSettings(NspObject *O);
extern NspGtkSourceSearchSettings *GetGtkSourceSearchSettingsCopy (Stack stack, int i);
extern NspGtkSourceSearchSettings *GetGtkSourceSearchSettings (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceSearchSettings */ 

#ifdef NspGtkSourceSearchSettings_Private 
static int init_gtksourcesearchsettings(NspGtkSourceSearchSettings *o,NspTypeGtkSourceSearchSettings *type);
static char *nsp_gtksourcesearchsettings_type_as_string(void);
static char *nsp_gtksourcesearchsettings_type_short_string(NspObject *v);
static AttrTab gtksourcesearchsettings_attrs[];
static NspMethods *gtksourcesearchsettings_get_methods(void);
/* static int int_gtksourcesearchsettings_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceSearchSettings_Private */
