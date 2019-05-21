/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIMMulticontext
#define NSP_INC_NspGtkIMMulticontext

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

/* NspGtkIMMulticontext */

#include <nsp/gtk/gtkimcontext.h>

/*
 * NspGtkIMMulticontext inherits from GtkIMContext
 * just change some type attributes 
 */

typedef NspGtkIMContext NspGtkIMMulticontext ;
typedef NspTypeGtkIMContext NspTypeGtkIMMulticontext ;

extern int nsp_type_gtkimmulticontext_id;
extern NspTypeGtkIMMulticontext *nsp_type_gtkimmulticontext;

/* type instances for gtkimcontext */

NspTypeGtkIMMulticontext *new_type_gtkimmulticontext(type_mode mode);

/* instance for NspGtkIMMulticontext */

NspGtkIMMulticontext *new_gtkimmulticontext();

/*
 * Object methods redefined for gtkimmulticontext 
 */

#define NULLGTKIMMULTICONTEXT (NspGtkIMMulticontext*) 0


/* from NspGtkIMMulticontextObj.c */

extern NspGtkIMMulticontext *nsp_gtkimmulticontext_object (NspObject *O);
extern int IsGtkIMMulticontextObj (Stack stack, int i);
extern int IsGtkIMMulticontext(NspObject *O);
extern NspGtkIMMulticontext *GetGtkIMMulticontextCopy (Stack stack, int i);
extern NspGtkIMMulticontext *GetGtkIMMulticontext (Stack stack, int i);

#endif /* NSP_INC_NspGtkIMMulticontext */ 

#ifdef NspGtkIMMulticontext_Private 
static int init_gtkimmulticontext(NspGtkIMMulticontext *o,NspTypeGtkIMMulticontext *type);
static char *nsp_gtkimmulticontext_type_as_string(void);
static char *nsp_gtkimmulticontext_type_short_string(NspObject *v);
static AttrTab gtkimmulticontext_attrs[];
static NspMethods *gtkimmulticontext_get_methods(void);
/* static int int_gtkimmulticontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIMMulticontext_Private */
