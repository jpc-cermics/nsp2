/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkEventBox
#define NSP_INC_NspGtkEventBox

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

/* NspGtkEventBox */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkEventBox inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkEventBox ;
typedef NspTypeGtkBin NspTypeGtkEventBox ;

extern int nsp_type_gtkeventbox_id;
extern NspTypeGtkEventBox *nsp_type_gtkeventbox;

/* type instances for gtkbin */

NspTypeGtkEventBox *new_type_gtkeventbox(type_mode mode);

/* instance for NspGtkEventBox */

NspGtkEventBox *new_gtkeventbox();

/*
 * Object methods redefined for gtkeventbox 
 */

#define NULLGTKEVENTBOX (NspGtkEventBox*) 0


/* from NspGtkEventBoxObj.c */

extern NspGtkEventBox *nsp_gtkeventbox_object (NspObject *O);
extern int IsGtkEventBoxObj (Stack stack, int i);
extern int IsGtkEventBox(NspObject *O);
extern NspGtkEventBox *GetGtkEventBoxCopy (Stack stack, int i);
extern NspGtkEventBox *GetGtkEventBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkEventBox */ 

#ifdef NspGtkEventBox_Private 
static int init_gtkeventbox(NspGtkEventBox *o,NspTypeGtkEventBox *type);
static char *nsp_gtkeventbox_type_as_string(void);
static char *nsp_gtkeventbox_type_short_string(NspObject *v);
static AttrTab gtkeventbox_attrs[];
static NspMethods *gtkeventbox_get_methods(void);
/* static int int_gtkeventbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkEventBox_Private */
