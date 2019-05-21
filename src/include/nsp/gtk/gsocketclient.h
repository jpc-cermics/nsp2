/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketClient
#define NSP_INC_NspGSocketClient

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

/* NspGSocketClient */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocketClient inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocketClient ;
typedef NspTypeGObject NspTypeGSocketClient ;

extern int nsp_type_gsocketclient_id;
extern NspTypeGSocketClient *nsp_type_gsocketclient;

/* type instances for gobject */

NspTypeGSocketClient *new_type_gsocketclient(type_mode mode);

/* instance for NspGSocketClient */

NspGSocketClient *new_gsocketclient();

/*
 * Object methods redefined for gsocketclient 
 */

#define NULLGSOCKETCLIENT (NspGSocketClient*) 0


/* from NspGSocketClientObj.c */

extern NspGSocketClient *nsp_gsocketclient_object (NspObject *O);
extern int IsGSocketClientObj (Stack stack, int i);
extern int IsGSocketClient(NspObject *O);
extern NspGSocketClient *GetGSocketClientCopy (Stack stack, int i);
extern NspGSocketClient *GetGSocketClient (Stack stack, int i);

#endif /* NSP_INC_NspGSocketClient */ 

#ifdef NspGSocketClient_Private 
static int init_gsocketclient(NspGSocketClient *o,NspTypeGSocketClient *type);
static char *nsp_gsocketclient_type_as_string(void);
static char *nsp_gsocketclient_type_short_string(NspObject *v);
static AttrTab gsocketclient_attrs[];
static NspMethods *gsocketclient_get_methods(void);
/* static int int_gsocketclient_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketClient_Private */
