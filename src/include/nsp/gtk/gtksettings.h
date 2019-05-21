/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSettings
#define NSP_INC_NspGtkSettings

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

/* NspGtkSettings */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSettings inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSettings ;
typedef NspTypeGObject NspTypeGtkSettings ;

extern int nsp_type_gtksettings_id;
extern NspTypeGtkSettings *nsp_type_gtksettings;

/* type instances for gobject */

NspTypeGtkSettings *new_type_gtksettings(type_mode mode);

/* instance for NspGtkSettings */

NspGtkSettings *new_gtksettings();

/*
 * Object methods redefined for gtksettings 
 */

#define NULLGTKSETTINGS (NspGtkSettings*) 0


/* from NspGtkSettingsObj.c */

extern NspGtkSettings *nsp_gtksettings_object (NspObject *O);
extern int IsGtkSettingsObj (Stack stack, int i);
extern int IsGtkSettings(NspObject *O);
extern NspGtkSettings *GetGtkSettingsCopy (Stack stack, int i);
extern NspGtkSettings *GetGtkSettings (Stack stack, int i);

#endif /* NSP_INC_NspGtkSettings */ 

#ifdef NspGtkSettings_Private 
static int init_gtksettings(NspGtkSettings *o,NspTypeGtkSettings *type);
static char *nsp_gtksettings_type_as_string(void);
static char *nsp_gtksettings_type_short_string(NspObject *v);
static AttrTab gtksettings_attrs[];
static NspMethods *gtksettings_get_methods(void);
/* static int int_gtksettings_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSettings_Private */
