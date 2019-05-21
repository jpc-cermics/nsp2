/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRevealer
#define NSP_INC_NspGtkRevealer

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

/* NspGtkRevealer */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkRevealer inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkRevealer ;
typedef NspTypeGtkBin NspTypeGtkRevealer ;

extern int nsp_type_gtkrevealer_id;
extern NspTypeGtkRevealer *nsp_type_gtkrevealer;

/* type instances for gtkbin */

NspTypeGtkRevealer *new_type_gtkrevealer(type_mode mode);

/* instance for NspGtkRevealer */

NspGtkRevealer *new_gtkrevealer();

/*
 * Object methods redefined for gtkrevealer 
 */

#define NULLGTKREVEALER (NspGtkRevealer*) 0


/* from NspGtkRevealerObj.c */

extern NspGtkRevealer *nsp_gtkrevealer_object (NspObject *O);
extern int IsGtkRevealerObj (Stack stack, int i);
extern int IsGtkRevealer(NspObject *O);
extern NspGtkRevealer *GetGtkRevealerCopy (Stack stack, int i);
extern NspGtkRevealer *GetGtkRevealer (Stack stack, int i);

#endif /* NSP_INC_NspGtkRevealer */ 

#ifdef NspGtkRevealer_Private 
static int init_gtkrevealer(NspGtkRevealer *o,NspTypeGtkRevealer *type);
static char *nsp_gtkrevealer_type_as_string(void);
static char *nsp_gtkrevealer_type_short_string(NspObject *v);
static AttrTab gtkrevealer_attrs[];
static NspMethods *gtkrevealer_get_methods(void);
/* static int int_gtkrevealer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRevealer_Private */
