/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMenuLinkIter
#define NSP_INC_NspGMenuLinkIter

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

/* NspGMenuLinkIter */

#include <nsp/gtk/gobject.h>

/*
 * NspGMenuLinkIter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGMenuLinkIter ;
typedef NspTypeGObject NspTypeGMenuLinkIter ;

extern int nsp_type_gmenulinkiter_id;
extern NspTypeGMenuLinkIter *nsp_type_gmenulinkiter;

/* type instances for gobject */

NspTypeGMenuLinkIter *new_type_gmenulinkiter(type_mode mode);

/* instance for NspGMenuLinkIter */

NspGMenuLinkIter *new_gmenulinkiter();

/*
 * Object methods redefined for gmenulinkiter 
 */

#define NULLGMENULINKITER (NspGMenuLinkIter*) 0


/* from NspGMenuLinkIterObj.c */

extern NspGMenuLinkIter *nsp_gmenulinkiter_object (NspObject *O);
extern int IsGMenuLinkIterObj (Stack stack, int i);
extern int IsGMenuLinkIter(NspObject *O);
extern NspGMenuLinkIter *GetGMenuLinkIterCopy (Stack stack, int i);
extern NspGMenuLinkIter *GetGMenuLinkIter (Stack stack, int i);

#endif /* NSP_INC_NspGMenuLinkIter */ 

#ifdef NspGMenuLinkIter_Private 
static int init_gmenulinkiter(NspGMenuLinkIter *o,NspTypeGMenuLinkIter *type);
static char *nsp_gmenulinkiter_type_as_string(void);
static char *nsp_gmenulinkiter_type_short_string(NspObject *v);
static AttrTab gmenulinkiter_attrs[];
static NspMethods *gmenulinkiter_get_methods(void);
/* static int int_gmenulinkiter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMenuLinkIter_Private */
