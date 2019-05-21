/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRecentFilter
#define NSP_INC_NspGtkRecentFilter

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

/* NspGtkRecentFilter */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkRecentFilter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkRecentFilter ;
typedef NspTypeGObject NspTypeGtkRecentFilter ;

extern int nsp_type_gtkrecentfilter_id;
extern NspTypeGtkRecentFilter *nsp_type_gtkrecentfilter;

/* type instances for gobject */

NspTypeGtkRecentFilter *new_type_gtkrecentfilter(type_mode mode);

/* instance for NspGtkRecentFilter */

NspGtkRecentFilter *new_gtkrecentfilter();

/*
 * Object methods redefined for gtkrecentfilter 
 */

#define NULLGTKRECENTFILTER (NspGtkRecentFilter*) 0


/* from NspGtkRecentFilterObj.c */

extern NspGtkRecentFilter *nsp_gtkrecentfilter_object (NspObject *O);
extern int IsGtkRecentFilterObj (Stack stack, int i);
extern int IsGtkRecentFilter(NspObject *O);
extern NspGtkRecentFilter *GetGtkRecentFilterCopy (Stack stack, int i);
extern NspGtkRecentFilter *GetGtkRecentFilter (Stack stack, int i);

#endif /* NSP_INC_NspGtkRecentFilter */ 

#ifdef NspGtkRecentFilter_Private 
static int init_gtkrecentfilter(NspGtkRecentFilter *o,NspTypeGtkRecentFilter *type);
static char *nsp_gtkrecentfilter_type_as_string(void);
static char *nsp_gtkrecentfilter_type_short_string(NspObject *v);
static AttrTab gtkrecentfilter_attrs[];
static NspMethods *gtkrecentfilter_get_methods(void);
/* static int int_gtkrecentfilter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRecentFilter_Private */
