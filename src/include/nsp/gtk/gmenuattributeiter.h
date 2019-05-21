/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMenuAttributeIter
#define NSP_INC_NspGMenuAttributeIter

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

/* NspGMenuAttributeIter */

#include <nsp/gtk/gobject.h>

/*
 * NspGMenuAttributeIter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGMenuAttributeIter ;
typedef NspTypeGObject NspTypeGMenuAttributeIter ;

extern int nsp_type_gmenuattributeiter_id;
extern NspTypeGMenuAttributeIter *nsp_type_gmenuattributeiter;

/* type instances for gobject */

NspTypeGMenuAttributeIter *new_type_gmenuattributeiter(type_mode mode);

/* instance for NspGMenuAttributeIter */

NspGMenuAttributeIter *new_gmenuattributeiter();

/*
 * Object methods redefined for gmenuattributeiter 
 */

#define NULLGMENUATTRIBUTEITER (NspGMenuAttributeIter*) 0


/* from NspGMenuAttributeIterObj.c */

extern NspGMenuAttributeIter *nsp_gmenuattributeiter_object (NspObject *O);
extern int IsGMenuAttributeIterObj (Stack stack, int i);
extern int IsGMenuAttributeIter(NspObject *O);
extern NspGMenuAttributeIter *GetGMenuAttributeIterCopy (Stack stack, int i);
extern NspGMenuAttributeIter *GetGMenuAttributeIter (Stack stack, int i);

#endif /* NSP_INC_NspGMenuAttributeIter */ 

#ifdef NspGMenuAttributeIter_Private 
static int init_gmenuattributeiter(NspGMenuAttributeIter *o,NspTypeGMenuAttributeIter *type);
static char *nsp_gmenuattributeiter_type_as_string(void);
static char *nsp_gmenuattributeiter_type_short_string(NspObject *v);
static AttrTab gmenuattributeiter_attrs[];
static NspMethods *gmenuattributeiter_get_methods(void);
/* static int int_gmenuattributeiter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMenuAttributeIter_Private */
