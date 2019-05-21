/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAssistant
#define NSP_INC_NspGtkAssistant

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

/* NspGtkAssistant */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkAssistant inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkAssistant ;
typedef NspTypeGtkWindow NspTypeGtkAssistant ;

extern int nsp_type_gtkassistant_id;
extern NspTypeGtkAssistant *nsp_type_gtkassistant;

/* type instances for gtkwindow */

NspTypeGtkAssistant *new_type_gtkassistant(type_mode mode);

/* instance for NspGtkAssistant */

NspGtkAssistant *new_gtkassistant();

/*
 * Object methods redefined for gtkassistant 
 */

#define NULLGTKASSISTANT (NspGtkAssistant*) 0


/* from NspGtkAssistantObj.c */

extern NspGtkAssistant *nsp_gtkassistant_object (NspObject *O);
extern int IsGtkAssistantObj (Stack stack, int i);
extern int IsGtkAssistant(NspObject *O);
extern NspGtkAssistant *GetGtkAssistantCopy (Stack stack, int i);
extern NspGtkAssistant *GetGtkAssistant (Stack stack, int i);

#endif /* NSP_INC_NspGtkAssistant */ 

#ifdef NspGtkAssistant_Private 
static int init_gtkassistant(NspGtkAssistant *o,NspTypeGtkAssistant *type);
static char *nsp_gtkassistant_type_as_string(void);
static char *nsp_gtkassistant_type_short_string(NspObject *v);
static AttrTab gtkassistant_attrs[];
static NspMethods *gtkassistant_get_methods(void);
/* static int int_gtkassistant_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAssistant_Private */
