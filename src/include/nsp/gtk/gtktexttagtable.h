/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextTagTable
#define NSP_INC_NspGtkTextTagTable

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

/* NspGtkTextTagTable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTextTagTable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTextTagTable ;
typedef NspTypeGObject NspTypeGtkTextTagTable ;

extern int nsp_type_gtktexttagtable_id;
extern NspTypeGtkTextTagTable *nsp_type_gtktexttagtable;

/* type instances for gobject */

NspTypeGtkTextTagTable *new_type_gtktexttagtable(type_mode mode);

/* instance for NspGtkTextTagTable */

NspGtkTextTagTable *new_gtktexttagtable();

/*
 * Object methods redefined for gtktexttagtable 
 */

#define NULLGTKTEXTTAGTABLE (NspGtkTextTagTable*) 0


/* from NspGtkTextTagTableObj.c */

extern NspGtkTextTagTable *nsp_gtktexttagtable_object (NspObject *O);
extern int IsGtkTextTagTableObj (Stack stack, int i);
extern int IsGtkTextTagTable(NspObject *O);
extern NspGtkTextTagTable *GetGtkTextTagTableCopy (Stack stack, int i);
extern NspGtkTextTagTable *GetGtkTextTagTable (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextTagTable */ 

#ifdef NspGtkTextTagTable_Private 
static int init_gtktexttagtable(NspGtkTextTagTable *o,NspTypeGtkTextTagTable *type);
static char *nsp_gtktexttagtable_type_as_string(void);
static char *nsp_gtktexttagtable_type_short_string(NspObject *v);
static AttrTab gtktexttagtable_attrs[];
static NspMethods *gtktexttagtable_get_methods(void);
/* static int int_gtktexttagtable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextTagTable_Private */
