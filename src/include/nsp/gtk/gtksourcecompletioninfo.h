/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionInfo
#define NSP_INC_NspGtkSourceCompletionInfo

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

/* NspGtkSourceCompletionInfo */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkSourceCompletionInfo inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkSourceCompletionInfo ;
typedef NspTypeGtkWindow NspTypeGtkSourceCompletionInfo ;

extern int nsp_type_gtksourcecompletioninfo_id;
extern NspTypeGtkSourceCompletionInfo *nsp_type_gtksourcecompletioninfo;

/* type instances for gtkwindow */

NspTypeGtkSourceCompletionInfo *new_type_gtksourcecompletioninfo(type_mode mode);

/* instance for NspGtkSourceCompletionInfo */

NspGtkSourceCompletionInfo *new_gtksourcecompletioninfo();

/*
 * Object methods redefined for gtksourcecompletioninfo 
 */

#define NULLGTKSOURCECOMPLETIONINFO (NspGtkSourceCompletionInfo*) 0


/* from NspGtkSourceCompletionInfoObj.c */

extern NspGtkSourceCompletionInfo *nsp_gtksourcecompletioninfo_object (NspObject *O);
extern int IsGtkSourceCompletionInfoObj (Stack stack, int i);
extern int IsGtkSourceCompletionInfo(NspObject *O);
extern NspGtkSourceCompletionInfo *GetGtkSourceCompletionInfoCopy (Stack stack, int i);
extern NspGtkSourceCompletionInfo *GetGtkSourceCompletionInfo (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionInfo */ 

#ifdef NspGtkSourceCompletionInfo_Private 
static int init_gtksourcecompletioninfo(NspGtkSourceCompletionInfo *o,NspTypeGtkSourceCompletionInfo *type);
static char *nsp_gtksourcecompletioninfo_type_as_string(void);
static char *nsp_gtksourcecompletioninfo_type_short_string(NspObject *v);
static AttrTab gtksourcecompletioninfo_attrs[];
static NspMethods *gtksourcecompletioninfo_get_methods(void);
/* static int int_gtksourcecompletioninfo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionInfo_Private */
