/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkStreamableContent
#define NSP_INC_NspAtkStreamableContent

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

/* NspAtkStreamableContent */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkStreamableContent inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkStreamableContent ;
typedef NspTypeGObject NspTypeAtkStreamableContent ;

extern int nsp_type_atkstreamablecontent_id;
extern NspTypeAtkStreamableContent *nsp_type_atkstreamablecontent;

/* type instances for gobject */

NspTypeAtkStreamableContent *new_type_atkstreamablecontent(type_mode mode);

/* instance for NspAtkStreamableContent */

NspAtkStreamableContent *new_atkstreamablecontent();

/*
 * Object methods redefined for atkstreamablecontent 
 */

#define NULLATKSTREAMABLECONTENT (NspAtkStreamableContent*) 0


/* from NspAtkStreamableContentObj.c */

extern NspAtkStreamableContent *nsp_atkstreamablecontent_object (NspObject *O);
extern int IsAtkStreamableContentObj (Stack stack, int i);
extern int IsAtkStreamableContent(NspObject *O);
extern NspAtkStreamableContent *GetAtkStreamableContentCopy (Stack stack, int i);
extern NspAtkStreamableContent *GetAtkStreamableContent (Stack stack, int i);

#endif /* NSP_INC_NspAtkStreamableContent */ 

#ifdef NspAtkStreamableContent_Private 
static int init_atkstreamablecontent(NspAtkStreamableContent *o,NspTypeAtkStreamableContent *type);
static char *nsp_atkstreamablecontent_type_as_string(void);
static char *nsp_atkstreamablecontent_type_short_string(NspObject *v);
static AttrTab atkstreamablecontent_attrs[];
static NspMethods *atkstreamablecontent_get_methods(void);
/* static int int_atkstreamablecontent_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkStreamableContent_Private */
