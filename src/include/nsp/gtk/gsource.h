/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSource
#define NSP_INC_NspGSource

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

/* NspGSource */

#include <nsp/gtk/gboxed.h>

/*
 * NspGSource inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGSource ;
typedef NspTypeGBoxed NspTypeGSource ;

extern int nsp_type_gsource_id;
extern NspTypeGSource *nsp_type_gsource;

/* type instances for gboxed */

NspTypeGSource *new_type_gsource(type_mode mode);

/* instance for NspGSource */

NspGSource *new_gsource();

/*
 * Object methods redefined for gsource 
 */

#define NULLGSOURCE (NspGSource*) 0


/* from NspGSourceObj.c */

extern NspGSource *nsp_gsource_object (NspObject *O);
extern int IsGSourceObj (Stack stack, int i);
extern int IsGSource(NspObject *O);
extern NspGSource *GetGSourceCopy (Stack stack, int i);
extern NspGSource *GetGSource (Stack stack, int i);

#endif /* NSP_INC_NspGSource */ 

#ifdef NspGSource_Private 
static int init_gsource(NspGSource *o,NspTypeGSource *type);
static char *nsp_gsource_type_as_string(void);
static char *nsp_gsource_type_short_string(NspObject *v);
static AttrTab gsource_attrs[];
static NspMethods *gsource_get_methods(void);
/* static int int_gsource_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSource_Private */
