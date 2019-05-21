/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionModel
#define NSP_INC_NspGtkSourceCompletionModel

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

/* NspGtkSourceCompletionModel */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceCompletionModel inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceCompletionModel ;
typedef NspTypeGObject NspTypeGtkSourceCompletionModel ;

extern int nsp_type_gtksourcecompletionmodel_id;
extern NspTypeGtkSourceCompletionModel *nsp_type_gtksourcecompletionmodel;

/* type instances for gobject */

NspTypeGtkSourceCompletionModel *new_type_gtksourcecompletionmodel(type_mode mode);

/* instance for NspGtkSourceCompletionModel */

NspGtkSourceCompletionModel *new_gtksourcecompletionmodel();

/*
 * Object methods redefined for gtksourcecompletionmodel 
 */

#define NULLGTKSOURCECOMPLETIONMODEL (NspGtkSourceCompletionModel*) 0


/* from NspGtkSourceCompletionModelObj.c */

extern NspGtkSourceCompletionModel *nsp_gtksourcecompletionmodel_object (NspObject *O);
extern int IsGtkSourceCompletionModelObj (Stack stack, int i);
extern int IsGtkSourceCompletionModel(NspObject *O);
extern NspGtkSourceCompletionModel *GetGtkSourceCompletionModelCopy (Stack stack, int i);
extern NspGtkSourceCompletionModel *GetGtkSourceCompletionModel (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionModel */ 

#ifdef NspGtkSourceCompletionModel_Private 
static int init_gtksourcecompletionmodel(NspGtkSourceCompletionModel *o,NspTypeGtkSourceCompletionModel *type);
static char *nsp_gtksourcecompletionmodel_type_as_string(void);
static char *nsp_gtksourcecompletionmodel_type_short_string(NspObject *v);
static AttrTab gtksourcecompletionmodel_attrs[];
static NspMethods *gtksourcecompletionmodel_get_methods(void);
/* static int int_gtksourcecompletionmodel_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionModel_Private */
