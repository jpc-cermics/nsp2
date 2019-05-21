/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionContainer
#define NSP_INC_NspGtkSourceCompletionContainer

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

/* NspGtkSourceCompletionContainer */

#include <nsp/gtk/gtkscrolledwindow.h>

/*
 * NspGtkSourceCompletionContainer inherits from GtkScrolledWindow
 * just change some type attributes 
 */

typedef NspGtkScrolledWindow NspGtkSourceCompletionContainer ;
typedef NspTypeGtkScrolledWindow NspTypeGtkSourceCompletionContainer ;

extern int nsp_type_gtksourcecompletioncontainer_id;
extern NspTypeGtkSourceCompletionContainer *nsp_type_gtksourcecompletioncontainer;

/* type instances for gtkscrolledwindow */

NspTypeGtkSourceCompletionContainer *new_type_gtksourcecompletioncontainer(type_mode mode);

/* instance for NspGtkSourceCompletionContainer */

NspGtkSourceCompletionContainer *new_gtksourcecompletioncontainer();

/*
 * Object methods redefined for gtksourcecompletioncontainer 
 */

#define NULLGTKSOURCECOMPLETIONCONTAINER (NspGtkSourceCompletionContainer*) 0


/* from NspGtkSourceCompletionContainerObj.c */

extern NspGtkSourceCompletionContainer *nsp_gtksourcecompletioncontainer_object (NspObject *O);
extern int IsGtkSourceCompletionContainerObj (Stack stack, int i);
extern int IsGtkSourceCompletionContainer(NspObject *O);
extern NspGtkSourceCompletionContainer *GetGtkSourceCompletionContainerCopy (Stack stack, int i);
extern NspGtkSourceCompletionContainer *GetGtkSourceCompletionContainer (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionContainer */ 

#ifdef NspGtkSourceCompletionContainer_Private 
static int init_gtksourcecompletioncontainer(NspGtkSourceCompletionContainer *o,NspTypeGtkSourceCompletionContainer *type);
static char *nsp_gtksourcecompletioncontainer_type_as_string(void);
static char *nsp_gtksourcecompletioncontainer_type_short_string(NspObject *v);
static AttrTab gtksourcecompletioncontainer_attrs[];
static NspMethods *gtksourcecompletioncontainer_get_methods(void);
/* static int int_gtksourcecompletioncontainer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionContainer_Private */
