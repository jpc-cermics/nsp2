/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketControlMessage
#define NSP_INC_NspGSocketControlMessage

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

/* NspGSocketControlMessage */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocketControlMessage inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocketControlMessage ;
typedef NspTypeGObject NspTypeGSocketControlMessage ;

extern int nsp_type_gsocketcontrolmessage_id;
extern NspTypeGSocketControlMessage *nsp_type_gsocketcontrolmessage;

/* type instances for gobject */

NspTypeGSocketControlMessage *new_type_gsocketcontrolmessage(type_mode mode);

/* instance for NspGSocketControlMessage */

NspGSocketControlMessage *new_gsocketcontrolmessage();

/*
 * Object methods redefined for gsocketcontrolmessage 
 */

#define NULLGSOCKETCONTROLMESSAGE (NspGSocketControlMessage*) 0


/* from NspGSocketControlMessageObj.c */

extern NspGSocketControlMessage *nsp_gsocketcontrolmessage_object (NspObject *O);
extern int IsGSocketControlMessageObj (Stack stack, int i);
extern int IsGSocketControlMessage(NspObject *O);
extern NspGSocketControlMessage *GetGSocketControlMessageCopy (Stack stack, int i);
extern NspGSocketControlMessage *GetGSocketControlMessage (Stack stack, int i);

#endif /* NSP_INC_NspGSocketControlMessage */ 

#ifdef NspGSocketControlMessage_Private 
static int init_gsocketcontrolmessage(NspGSocketControlMessage *o,NspTypeGSocketControlMessage *type);
static char *nsp_gsocketcontrolmessage_type_as_string(void);
static char *nsp_gsocketcontrolmessage_type_short_string(NspObject *v);
static AttrTab gsocketcontrolmessage_attrs[];
static NspMethods *gsocketcontrolmessage_get_methods(void);
/* static int int_gsocketcontrolmessage_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketControlMessage_Private */
