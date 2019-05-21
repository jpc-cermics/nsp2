/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMainContext
#define NSP_INC_NspGMainContext

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

/* NspGMainContext */

#include <nsp/gtk/gboxed.h>

/*
 * NspGMainContext inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGMainContext ;
typedef NspTypeGBoxed NspTypeGMainContext ;

extern int nsp_type_gmaincontext_id;
extern NspTypeGMainContext *nsp_type_gmaincontext;

/* type instances for gboxed */

NspTypeGMainContext *new_type_gmaincontext(type_mode mode);

/* instance for NspGMainContext */

NspGMainContext *new_gmaincontext();

/*
 * Object methods redefined for gmaincontext 
 */

#define NULLGMAINCONTEXT (NspGMainContext*) 0


/* from NspGMainContextObj.c */

extern NspGMainContext *nsp_gmaincontext_object (NspObject *O);
extern int IsGMainContextObj (Stack stack, int i);
extern int IsGMainContext(NspObject *O);
extern NspGMainContext *GetGMainContextCopy (Stack stack, int i);
extern NspGMainContext *GetGMainContext (Stack stack, int i);

#endif /* NSP_INC_NspGMainContext */ 

#ifdef NspGMainContext_Private 
static int init_gmaincontext(NspGMainContext *o,NspTypeGMainContext *type);
static char *nsp_gmaincontext_type_as_string(void);
static char *nsp_gmaincontext_type_short_string(NspObject *v);
static AttrTab gmaincontext_attrs[];
static NspMethods *gmaincontext_get_methods(void);
/* static int int_gmaincontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMainContext_Private */
