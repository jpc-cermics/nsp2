/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGTlsInteraction
#define NSP_INC_NspGTlsInteraction

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

/* NspGTlsInteraction */

#include <nsp/gtk/gobject.h>

/*
 * NspGTlsInteraction inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGTlsInteraction ;
typedef NspTypeGObject NspTypeGTlsInteraction ;

extern int nsp_type_gtlsinteraction_id;
extern NspTypeGTlsInteraction *nsp_type_gtlsinteraction;

/* type instances for gobject */

NspTypeGTlsInteraction *new_type_gtlsinteraction(type_mode mode);

/* instance for NspGTlsInteraction */

NspGTlsInteraction *new_gtlsinteraction();

/*
 * Object methods redefined for gtlsinteraction 
 */

#define NULLGTLSINTERACTION (NspGTlsInteraction*) 0


/* from NspGTlsInteractionObj.c */

extern NspGTlsInteraction *nsp_gtlsinteraction_object (NspObject *O);
extern int IsGTlsInteractionObj (Stack stack, int i);
extern int IsGTlsInteraction(NspObject *O);
extern NspGTlsInteraction *GetGTlsInteractionCopy (Stack stack, int i);
extern NspGTlsInteraction *GetGTlsInteraction (Stack stack, int i);

#endif /* NSP_INC_NspGTlsInteraction */ 

#ifdef NspGTlsInteraction_Private 
static int init_gtlsinteraction(NspGTlsInteraction *o,NspTypeGTlsInteraction *type);
static char *nsp_gtlsinteraction_type_as_string(void);
static char *nsp_gtlsinteraction_type_short_string(NspObject *v);
static AttrTab gtlsinteraction_attrs[];
static NspMethods *gtlsinteraction_get_methods(void);
/* static int int_gtlsinteraction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGTlsInteraction_Private */
