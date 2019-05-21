/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkHyperlinkImpl
#define NSP_INC_NspAtkHyperlinkImpl

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

/* NspAtkHyperlinkImpl */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkHyperlinkImpl inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkHyperlinkImpl ;
typedef NspTypeGObject NspTypeAtkHyperlinkImpl ;

extern int nsp_type_atkhyperlinkimpl_id;
extern NspTypeAtkHyperlinkImpl *nsp_type_atkhyperlinkimpl;

/* type instances for gobject */

NspTypeAtkHyperlinkImpl *new_type_atkhyperlinkimpl(type_mode mode);

/* instance for NspAtkHyperlinkImpl */

NspAtkHyperlinkImpl *new_atkhyperlinkimpl();

/*
 * Object methods redefined for atkhyperlinkimpl 
 */

#define NULLATKHYPERLINKIMPL (NspAtkHyperlinkImpl*) 0


/* from NspAtkHyperlinkImplObj.c */

extern NspAtkHyperlinkImpl *nsp_atkhyperlinkimpl_object (NspObject *O);
extern int IsAtkHyperlinkImplObj (Stack stack, int i);
extern int IsAtkHyperlinkImpl(NspObject *O);
extern NspAtkHyperlinkImpl *GetAtkHyperlinkImplCopy (Stack stack, int i);
extern NspAtkHyperlinkImpl *GetAtkHyperlinkImpl (Stack stack, int i);

#endif /* NSP_INC_NspAtkHyperlinkImpl */ 

#ifdef NspAtkHyperlinkImpl_Private 
static int init_atkhyperlinkimpl(NspAtkHyperlinkImpl *o,NspTypeAtkHyperlinkImpl *type);
static char *nsp_atkhyperlinkimpl_type_as_string(void);
static char *nsp_atkhyperlinkimpl_type_short_string(NspObject *v);
static AttrTab atkhyperlinkimpl_attrs[];
static NspMethods *atkhyperlinkimpl_get_methods(void);
/* static int int_atkhyperlinkimpl_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkHyperlinkImpl_Private */
