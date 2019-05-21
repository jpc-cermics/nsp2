/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkHypertext
#define NSP_INC_NspAtkHypertext

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

/* NspAtkHypertext */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkHypertext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkHypertext ;
typedef NspTypeGObject NspTypeAtkHypertext ;

extern int nsp_type_atkhypertext_id;
extern NspTypeAtkHypertext *nsp_type_atkhypertext;

/* type instances for gobject */

NspTypeAtkHypertext *new_type_atkhypertext(type_mode mode);

/* instance for NspAtkHypertext */

NspAtkHypertext *new_atkhypertext();

/*
 * Object methods redefined for atkhypertext 
 */

#define NULLATKHYPERTEXT (NspAtkHypertext*) 0


/* from NspAtkHypertextObj.c */

extern NspAtkHypertext *nsp_atkhypertext_object (NspObject *O);
extern int IsAtkHypertextObj (Stack stack, int i);
extern int IsAtkHypertext(NspObject *O);
extern NspAtkHypertext *GetAtkHypertextCopy (Stack stack, int i);
extern NspAtkHypertext *GetAtkHypertext (Stack stack, int i);

#endif /* NSP_INC_NspAtkHypertext */ 

#ifdef NspAtkHypertext_Private 
static int init_atkhypertext(NspAtkHypertext *o,NspTypeAtkHypertext *type);
static char *nsp_atkhypertext_type_as_string(void);
static char *nsp_atkhypertext_type_short_string(NspObject *v);
static AttrTab atkhypertext_attrs[];
static NspMethods *atkhypertext_get_methods(void);
/* static int int_atkhypertext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkHypertext_Private */
