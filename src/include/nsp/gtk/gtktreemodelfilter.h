/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeModelFilter
#define NSP_INC_NspGtkTreeModelFilter

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

/* NspGtkTreeModelFilter */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeModelFilter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeModelFilter ;
typedef NspTypeGObject NspTypeGtkTreeModelFilter ;

extern int nsp_type_gtktreemodelfilter_id;
extern NspTypeGtkTreeModelFilter *nsp_type_gtktreemodelfilter;

/* type instances for gobject */

NspTypeGtkTreeModelFilter *new_type_gtktreemodelfilter(type_mode mode);

/* instance for NspGtkTreeModelFilter */

NspGtkTreeModelFilter *new_gtktreemodelfilter();

/*
 * Object methods redefined for gtktreemodelfilter 
 */

#define NULLGTKTREEMODELFILTER (NspGtkTreeModelFilter*) 0


/* from NspGtkTreeModelFilterObj.c */

extern NspGtkTreeModelFilter *nsp_gtktreemodelfilter_object (NspObject *O);
extern int IsGtkTreeModelFilterObj (Stack stack, int i);
extern int IsGtkTreeModelFilter(NspObject *O);
extern NspGtkTreeModelFilter *GetGtkTreeModelFilterCopy (Stack stack, int i);
extern NspGtkTreeModelFilter *GetGtkTreeModelFilter (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeModelFilter */ 

#ifdef NspGtkTreeModelFilter_Private 
static int init_gtktreemodelfilter(NspGtkTreeModelFilter *o,NspTypeGtkTreeModelFilter *type);
static char *nsp_gtktreemodelfilter_type_as_string(void);
static char *nsp_gtktreemodelfilter_type_short_string(NspObject *v);
static AttrTab gtktreemodelfilter_attrs[];
static NspMethods *gtktreemodelfilter_get_methods(void);
/* static int int_gtktreemodelfilter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeModelFilter_Private */
