/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeModel
#define NSP_INC_NspGtkTreeModel

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

/* NspGtkTreeModel */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeModel inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeModel ;
typedef NspTypeGObject NspTypeGtkTreeModel ;

extern int nsp_type_gtktreemodel_id;
extern NspTypeGtkTreeModel *nsp_type_gtktreemodel;

/* type instances for gobject */

NspTypeGtkTreeModel *new_type_gtktreemodel(type_mode mode);

/* instance for NspGtkTreeModel */

NspGtkTreeModel *new_gtktreemodel();

/*
 * Object methods redefined for gtktreemodel 
 */

#define NULLGTKTREEMODEL (NspGtkTreeModel*) 0


/* from NspGtkTreeModelObj.c */

extern NspGtkTreeModel *nsp_gtktreemodel_object (NspObject *O);
extern int IsGtkTreeModelObj (Stack stack, int i);
extern int IsGtkTreeModel(NspObject *O);
extern NspGtkTreeModel *GetGtkTreeModelCopy (Stack stack, int i);
extern NspGtkTreeModel *GetGtkTreeModel (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeModel */ 

#ifdef NspGtkTreeModel_Private 
static int init_gtktreemodel(NspGtkTreeModel *o,NspTypeGtkTreeModel *type);
static char *nsp_gtktreemodel_type_as_string(void);
static char *nsp_gtktreemodel_type_short_string(NspObject *v);
static AttrTab gtktreemodel_attrs[];
static NspMethods *gtktreemodel_get_methods(void);
/* static int int_gtktreemodel_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeModel_Private */
