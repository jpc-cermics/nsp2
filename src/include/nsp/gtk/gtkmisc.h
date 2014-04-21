/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMisc
#define NSP_INC_NspGtkMisc

/*
 * Copyright (C) 1998-2014 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGtkMisc */

#include <nsp/gtk/gtkwidget.h>

/*
* NspGtkMisc inherits from GtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkMisc ;
typedef NspTypeGtkWidget NspTypeGtkMisc ;

extern int nsp_type_gtkmisc_id;
extern NspTypeGtkMisc *nsp_type_gtkmisc;

/* type instances for gtkwidget */

NspTypeGtkMisc *new_type_gtkmisc(type_mode mode);

/* instance for NspGtkMisc */

NspGtkMisc *new_gtkmisc();

/*
* Object methods redefined for gtkmisc 
*/

#define NULLGTKMISC (NspGtkMisc*) 0


/* from NspGtkMiscObj.c */

extern NspGtkMisc *nsp_gtkmisc_object (NspObject *O); 
extern int IsGtkMiscObj (Stack stack, int i); 
extern int IsGtkMisc(NspObject *O);
extern NspGtkMisc *GetGtkMiscCopy (Stack stack, int i); 
extern NspGtkMisc *GetGtkMisc (Stack stack, int i); 

#endif /* NSP_INC_NspGtkMisc */

#ifdef NspGtkMisc_Private 
static int init_gtkmisc(NspGtkMisc *o,NspTypeGtkMisc *type);
static char *nsp_gtkmisc_type_as_string(void);
static char *nsp_gtkmisc_type_short_string(NspObject *v);
static AttrTab gtkmisc_attrs[];
static NspMethods *gtkmisc_get_methods(void); 
/* static int int_gtkmisc_create(Stack stack, int rhs, int opt, int lhs);*/
#endif /* NspGtkMisc_Private */
