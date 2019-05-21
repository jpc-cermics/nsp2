/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDrive
#define NSP_INC_NspGDrive

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

/* NspGDrive */

#include <nsp/gtk/gobject.h>

/*
 * NspGDrive inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDrive ;
typedef NspTypeGObject NspTypeGDrive ;

extern int nsp_type_gdrive_id;
extern NspTypeGDrive *nsp_type_gdrive;

/* type instances for gobject */

NspTypeGDrive *new_type_gdrive(type_mode mode);

/* instance for NspGDrive */

NspGDrive *new_gdrive();

/*
 * Object methods redefined for gdrive 
 */

#define NULLGDRIVE (NspGDrive*) 0


/* from NspGDriveObj.c */

extern NspGDrive *nsp_gdrive_object (NspObject *O);
extern int IsGDriveObj (Stack stack, int i);
extern int IsGDrive(NspObject *O);
extern NspGDrive *GetGDriveCopy (Stack stack, int i);
extern NspGDrive *GetGDrive (Stack stack, int i);

#endif /* NSP_INC_NspGDrive */ 

#ifdef NspGDrive_Private 
static int init_gdrive(NspGDrive *o,NspTypeGDrive *type);
static char *nsp_gdrive_type_as_string(void);
static char *nsp_gdrive_type_short_string(NspObject *v);
static AttrTab gdrive_attrs[];
static NspMethods *gdrive_get_methods(void);
/* static int int_gdrive_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDrive_Private */
