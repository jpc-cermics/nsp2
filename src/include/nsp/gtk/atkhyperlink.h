/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkHyperlink
#define NSP_INC_NspAtkHyperlink

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

/* NspAtkHyperlink */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkHyperlink inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkHyperlink ;
typedef NspTypeGObject NspTypeAtkHyperlink ;

extern int nsp_type_atkhyperlink_id;
extern NspTypeAtkHyperlink *nsp_type_atkhyperlink;

/* type instances for gobject */

NspTypeAtkHyperlink *new_type_atkhyperlink(type_mode mode);

/* instance for NspAtkHyperlink */

NspAtkHyperlink *new_atkhyperlink();

/*
 * Object methods redefined for atkhyperlink 
 */

#define NULLATKHYPERLINK (NspAtkHyperlink*) 0


/* from NspAtkHyperlinkObj.c */

extern NspAtkHyperlink *nsp_atkhyperlink_object (NspObject *O);
extern int IsAtkHyperlinkObj (Stack stack, int i);
extern int IsAtkHyperlink(NspObject *O);
extern NspAtkHyperlink *GetAtkHyperlinkCopy (Stack stack, int i);
extern NspAtkHyperlink *GetAtkHyperlink (Stack stack, int i);

#endif /* NSP_INC_NspAtkHyperlink */ 

#ifdef NspAtkHyperlink_Private 
static int init_atkhyperlink(NspAtkHyperlink *o,NspTypeAtkHyperlink *type);
static char *nsp_atkhyperlink_type_as_string(void);
static char *nsp_atkhyperlink_type_short_string(NspObject *v);
static AttrTab atkhyperlink_attrs[];
static NspMethods *atkhyperlink_get_methods(void);
/* static int int_atkhyperlink_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkHyperlink_Private */
