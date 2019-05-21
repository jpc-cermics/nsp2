/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPrintContext
#define NSP_INC_NspGtkPrintContext

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

/* NspGtkPrintContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkPrintContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkPrintContext ;
typedef NspTypeGObject NspTypeGtkPrintContext ;

extern int nsp_type_gtkprintcontext_id;
extern NspTypeGtkPrintContext *nsp_type_gtkprintcontext;

/* type instances for gobject */

NspTypeGtkPrintContext *new_type_gtkprintcontext(type_mode mode);

/* instance for NspGtkPrintContext */

NspGtkPrintContext *new_gtkprintcontext();

/*
 * Object methods redefined for gtkprintcontext 
 */

#define NULLGTKPRINTCONTEXT (NspGtkPrintContext*) 0


/* from NspGtkPrintContextObj.c */

extern NspGtkPrintContext *nsp_gtkprintcontext_object (NspObject *O);
extern int IsGtkPrintContextObj (Stack stack, int i);
extern int IsGtkPrintContext(NspObject *O);
extern NspGtkPrintContext *GetGtkPrintContextCopy (Stack stack, int i);
extern NspGtkPrintContext *GetGtkPrintContext (Stack stack, int i);

#endif /* NSP_INC_NspGtkPrintContext */ 

#ifdef NspGtkPrintContext_Private 
static int init_gtkprintcontext(NspGtkPrintContext *o,NspTypeGtkPrintContext *type);
static char *nsp_gtkprintcontext_type_as_string(void);
static char *nsp_gtkprintcontext_type_short_string(NspObject *v);
static AttrTab gtkprintcontext_attrs[];
static NspMethods *gtkprintcontext_get_methods(void);
/* static int int_gtkprintcontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPrintContext_Private */
