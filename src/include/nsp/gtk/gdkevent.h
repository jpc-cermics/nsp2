/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkEvent
#define NSP_INC_NspGdkEvent

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

/* NspGdkEvent */

#include <nsp/gtk/gboxed.h>

/*
 * NspGdkEvent inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGdkEvent ;
typedef NspTypeGBoxed NspTypeGdkEvent ;

extern int nsp_type_gdkevent_id;
extern NspTypeGdkEvent *nsp_type_gdkevent;

/* type instances for gboxed */

NspTypeGdkEvent *new_type_gdkevent(type_mode mode);

/* instance for NspGdkEvent */

NspGdkEvent *new_gdkevent();

/*
 * Object methods redefined for gdkevent 
 */

#define NULLGDKEVENT (NspGdkEvent*) 0


/* from NspGdkEventObj.c */

extern NspGdkEvent *nsp_gdkevent_object (NspObject *O);
extern int IsGdkEventObj (Stack stack, int i);
extern int IsGdkEvent(NspObject *O);
extern NspGdkEvent *GetGdkEventCopy (Stack stack, int i);
extern NspGdkEvent *GetGdkEvent (Stack stack, int i);

#endif /* NSP_INC_NspGdkEvent */ 

#ifdef NspGdkEvent_Private 
static int init_gdkevent(NspGdkEvent *o,NspTypeGdkEvent *type);
static char *nsp_gdkevent_type_as_string(void);
static char *nsp_gdkevent_type_short_string(NspObject *v);
static AttrTab gdkevent_attrs[];
static NspMethods *gdkevent_get_methods(void);
/* static int int_gdkevent_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkEvent_Private */
