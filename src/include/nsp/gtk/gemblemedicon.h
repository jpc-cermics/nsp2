/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGEmblemedIcon
#define NSP_INC_NspGEmblemedIcon

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

/* NspGEmblemedIcon */

#include <nsp/gtk/gobject.h>

/*
 * NspGEmblemedIcon inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGEmblemedIcon ;
typedef NspTypeGObject NspTypeGEmblemedIcon ;

extern int nsp_type_gemblemedicon_id;
extern NspTypeGEmblemedIcon *nsp_type_gemblemedicon;

/* type instances for gobject */

NspTypeGEmblemedIcon *new_type_gemblemedicon(type_mode mode);

/* instance for NspGEmblemedIcon */

NspGEmblemedIcon *new_gemblemedicon();

/*
 * Object methods redefined for gemblemedicon 
 */

#define NULLGEMBLEMEDICON (NspGEmblemedIcon*) 0


/* from NspGEmblemedIconObj.c */

extern NspGEmblemedIcon *nsp_gemblemedicon_object (NspObject *O);
extern int IsGEmblemedIconObj (Stack stack, int i);
extern int IsGEmblemedIcon(NspObject *O);
extern NspGEmblemedIcon *GetGEmblemedIconCopy (Stack stack, int i);
extern NspGEmblemedIcon *GetGEmblemedIcon (Stack stack, int i);

#endif /* NSP_INC_NspGEmblemedIcon */ 

#ifdef NspGEmblemedIcon_Private 
static int init_gemblemedicon(NspGEmblemedIcon *o,NspTypeGEmblemedIcon *type);
static char *nsp_gemblemedicon_type_as_string(void);
static char *nsp_gemblemedicon_type_short_string(NspObject *v);
static AttrTab gemblemedicon_attrs[];
static NspMethods *gemblemedicon_get_methods(void);
/* static int int_gemblemedicon_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGEmblemedIcon_Private */
