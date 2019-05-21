/* -*- Mode: C -*- */
#ifndef NSP_INC_NspWebKitNetworkRequest
#define NSP_INC_NspWebKitNetworkRequest

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

/* NspWebKitNetworkRequest */

#include <nsp/gtk/gobject.h>

/*
 * NspWebKitNetworkRequest inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspWebKitNetworkRequest ;
typedef NspTypeGObject NspTypeWebKitNetworkRequest ;

extern int nsp_type_webkitnetworkrequest_id;
extern NspTypeWebKitNetworkRequest *nsp_type_webkitnetworkrequest;

/* type instances for gobject */

NspTypeWebKitNetworkRequest *new_type_webkitnetworkrequest(type_mode mode);

/* instance for NspWebKitNetworkRequest */

NspWebKitNetworkRequest *new_webkitnetworkrequest();

/*
 * Object methods redefined for webkitnetworkrequest 
 */

#define NULLWEBKITNETWORKREQUEST (NspWebKitNetworkRequest*) 0


/* from NspWebKitNetworkRequestObj.c */

extern NspWebKitNetworkRequest *nsp_webkitnetworkrequest_object (NspObject *O);
extern int IsWebKitNetworkRequestObj (Stack stack, int i);
extern int IsWebKitNetworkRequest(NspObject *O);
extern NspWebKitNetworkRequest *GetWebKitNetworkRequestCopy (Stack stack, int i);
extern NspWebKitNetworkRequest *GetWebKitNetworkRequest (Stack stack, int i);

#endif /* NSP_INC_NspWebKitNetworkRequest */ 

#ifdef NspWebKitNetworkRequest_Private 
static int init_webkitnetworkrequest(NspWebKitNetworkRequest *o,NspTypeWebKitNetworkRequest *type);
static char *nsp_webkitnetworkrequest_type_as_string(void);
static char *nsp_webkitnetworkrequest_type_short_string(NspObject *v);
static AttrTab webkitnetworkrequest_attrs[];
static NspMethods *webkitnetworkrequest_get_methods(void);
/* static int int_webkitnetworkrequest_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspWebKitNetworkRequest_Private */
