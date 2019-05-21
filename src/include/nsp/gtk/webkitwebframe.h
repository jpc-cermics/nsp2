/* -*- Mode: C -*- */
#ifndef NSP_INC_NspWebKitWebFrame
#define NSP_INC_NspWebKitWebFrame

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

/* NspWebKitWebFrame */

#include <nsp/gtk/gobject.h>

/*
 * NspWebKitWebFrame inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspWebKitWebFrame ;
typedef NspTypeGObject NspTypeWebKitWebFrame ;

extern int nsp_type_webkitwebframe_id;
extern NspTypeWebKitWebFrame *nsp_type_webkitwebframe;

/* type instances for gobject */

NspTypeWebKitWebFrame *new_type_webkitwebframe(type_mode mode);

/* instance for NspWebKitWebFrame */

NspWebKitWebFrame *new_webkitwebframe();

/*
 * Object methods redefined for webkitwebframe 
 */

#define NULLWEBKITWEBFRAME (NspWebKitWebFrame*) 0


/* from NspWebKitWebFrameObj.c */

extern NspWebKitWebFrame *nsp_webkitwebframe_object (NspObject *O);
extern int IsWebKitWebFrameObj (Stack stack, int i);
extern int IsWebKitWebFrame(NspObject *O);
extern NspWebKitWebFrame *GetWebKitWebFrameCopy (Stack stack, int i);
extern NspWebKitWebFrame *GetWebKitWebFrame (Stack stack, int i);

#endif /* NSP_INC_NspWebKitWebFrame */ 

#ifdef NspWebKitWebFrame_Private 
static int init_webkitwebframe(NspWebKitWebFrame *o,NspTypeWebKitWebFrame *type);
static char *nsp_webkitwebframe_type_as_string(void);
static char *nsp_webkitwebframe_type_short_string(NspObject *v);
static AttrTab webkitwebframe_attrs[];
static NspMethods *webkitwebframe_get_methods(void);
/* static int int_webkitwebframe_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspWebKitWebFrame_Private */
