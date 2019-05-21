/* -*- Mode: C -*- */
#ifndef NSP_INC_NspWebKitWebView
#define NSP_INC_NspWebKitWebView

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

/* NspWebKitWebView */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspWebKitWebView inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspWebKitWebView ;
typedef NspTypeGtkContainer NspTypeWebKitWebView ;

extern int nsp_type_webkitwebview_id;
extern NspTypeWebKitWebView *nsp_type_webkitwebview;

/* type instances for gtkcontainer */

NspTypeWebKitWebView *new_type_webkitwebview(type_mode mode);

/* instance for NspWebKitWebView */

NspWebKitWebView *new_webkitwebview();

/*
 * Object methods redefined for webkitwebview 
 */

#define NULLWEBKITWEBVIEW (NspWebKitWebView*) 0


/* from NspWebKitWebViewObj.c */

extern NspWebKitWebView *nsp_webkitwebview_object (NspObject *O);
extern int IsWebKitWebViewObj (Stack stack, int i);
extern int IsWebKitWebView(NspObject *O);
extern NspWebKitWebView *GetWebKitWebViewCopy (Stack stack, int i);
extern NspWebKitWebView *GetWebKitWebView (Stack stack, int i);

#endif /* NSP_INC_NspWebKitWebView */ 

#ifdef NspWebKitWebView_Private 
static int init_webkitwebview(NspWebKitWebView *o,NspTypeWebKitWebView *type);
static char *nsp_webkitwebview_type_as_string(void);
static char *nsp_webkitwebview_type_short_string(NspObject *v);
static AttrTab webkitwebview_attrs[];
static NspMethods *webkitwebview_get_methods(void);
/* static int int_webkitwebview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspWebKitWebView_Private */
