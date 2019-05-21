/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkEditableText
#define NSP_INC_NspAtkEditableText

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

/* NspAtkEditableText */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkEditableText inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkEditableText ;
typedef NspTypeGObject NspTypeAtkEditableText ;

extern int nsp_type_atkeditabletext_id;
extern NspTypeAtkEditableText *nsp_type_atkeditabletext;

/* type instances for gobject */

NspTypeAtkEditableText *new_type_atkeditabletext(type_mode mode);

/* instance for NspAtkEditableText */

NspAtkEditableText *new_atkeditabletext();

/*
 * Object methods redefined for atkeditabletext 
 */

#define NULLATKEDITABLETEXT (NspAtkEditableText*) 0


/* from NspAtkEditableTextObj.c */

extern NspAtkEditableText *nsp_atkeditabletext_object (NspObject *O);
extern int IsAtkEditableTextObj (Stack stack, int i);
extern int IsAtkEditableText(NspObject *O);
extern NspAtkEditableText *GetAtkEditableTextCopy (Stack stack, int i);
extern NspAtkEditableText *GetAtkEditableText (Stack stack, int i);

#endif /* NSP_INC_NspAtkEditableText */ 

#ifdef NspAtkEditableText_Private 
static int init_atkeditabletext(NspAtkEditableText *o,NspTypeAtkEditableText *type);
static char *nsp_atkeditabletext_type_as_string(void);
static char *nsp_atkeditabletext_type_short_string(NspObject *v);
static AttrTab atkeditabletext_attrs[];
static NspMethods *atkeditabletext_get_methods(void);
/* static int int_atkeditabletext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkEditableText_Private */
