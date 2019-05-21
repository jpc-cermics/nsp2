/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSettingsBackend
#define NSP_INC_NspGSettingsBackend

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

/* NspGSettingsBackend */

#include <nsp/gtk/gobject.h>

/*
 * NspGSettingsBackend inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSettingsBackend ;
typedef NspTypeGObject NspTypeGSettingsBackend ;

extern int nsp_type_gsettingsbackend_id;
extern NspTypeGSettingsBackend *nsp_type_gsettingsbackend;

/* type instances for gobject */

NspTypeGSettingsBackend *new_type_gsettingsbackend(type_mode mode);

/* instance for NspGSettingsBackend */

NspGSettingsBackend *new_gsettingsbackend();

/*
 * Object methods redefined for gsettingsbackend 
 */

#define NULLGSETTINGSBACKEND (NspGSettingsBackend*) 0


/* from NspGSettingsBackendObj.c */

extern NspGSettingsBackend *nsp_gsettingsbackend_object (NspObject *O);
extern int IsGSettingsBackendObj (Stack stack, int i);
extern int IsGSettingsBackend(NspObject *O);
extern NspGSettingsBackend *GetGSettingsBackendCopy (Stack stack, int i);
extern NspGSettingsBackend *GetGSettingsBackend (Stack stack, int i);

#endif /* NSP_INC_NspGSettingsBackend */ 

#ifdef NspGSettingsBackend_Private 
static int init_gsettingsbackend(NspGSettingsBackend *o,NspTypeGSettingsBackend *type);
static char *nsp_gsettingsbackend_type_as_string(void);
static char *nsp_gsettingsbackend_type_short_string(NspObject *v);
static AttrTab gsettingsbackend_attrs[];
static NspMethods *gsettingsbackend_get_methods(void);
/* static int int_gsettingsbackend_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSettingsBackend_Private */
