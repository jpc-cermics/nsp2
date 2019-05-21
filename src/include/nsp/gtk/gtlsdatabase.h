/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGTlsDatabase
#define NSP_INC_NspGTlsDatabase

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

/* NspGTlsDatabase */

#include <nsp/gtk/gobject.h>

/*
 * NspGTlsDatabase inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGTlsDatabase ;
typedef NspTypeGObject NspTypeGTlsDatabase ;

extern int nsp_type_gtlsdatabase_id;
extern NspTypeGTlsDatabase *nsp_type_gtlsdatabase;

/* type instances for gobject */

NspTypeGTlsDatabase *new_type_gtlsdatabase(type_mode mode);

/* instance for NspGTlsDatabase */

NspGTlsDatabase *new_gtlsdatabase();

/*
 * Object methods redefined for gtlsdatabase 
 */

#define NULLGTLSDATABASE (NspGTlsDatabase*) 0


/* from NspGTlsDatabaseObj.c */

extern NspGTlsDatabase *nsp_gtlsdatabase_object (NspObject *O);
extern int IsGTlsDatabaseObj (Stack stack, int i);
extern int IsGTlsDatabase(NspObject *O);
extern NspGTlsDatabase *GetGTlsDatabaseCopy (Stack stack, int i);
extern NspGTlsDatabase *GetGTlsDatabase (Stack stack, int i);

#endif /* NSP_INC_NspGTlsDatabase */ 

#ifdef NspGTlsDatabase_Private 
static int init_gtlsdatabase(NspGTlsDatabase *o,NspTypeGTlsDatabase *type);
static char *nsp_gtlsdatabase_type_as_string(void);
static char *nsp_gtlsdatabase_type_short_string(NspObject *v);
static AttrTab gtlsdatabase_attrs[];
static NspMethods *gtlsdatabase_get_methods(void);
/* static int int_gtlsdatabase_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGTlsDatabase_Private */
