/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMainLoop
#define NSP_INC_NspGMainLoop

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

/* NspGMainLoop */

#include <nsp/gtk/gboxed.h>

/*
 * NspGMainLoop inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGMainLoop ;
typedef NspTypeGBoxed NspTypeGMainLoop ;

extern int nsp_type_gmainloop_id;
extern NspTypeGMainLoop *nsp_type_gmainloop;

/* type instances for gboxed */

NspTypeGMainLoop *new_type_gmainloop(type_mode mode);

/* instance for NspGMainLoop */

NspGMainLoop *new_gmainloop();

/*
 * Object methods redefined for gmainloop 
 */

#define NULLGMAINLOOP (NspGMainLoop*) 0


/* from NspGMainLoopObj.c */

extern NspGMainLoop *nsp_gmainloop_object (NspObject *O);
extern int IsGMainLoopObj (Stack stack, int i);
extern int IsGMainLoop(NspObject *O);
extern NspGMainLoop *GetGMainLoopCopy (Stack stack, int i);
extern NspGMainLoop *GetGMainLoop (Stack stack, int i);

#endif /* NSP_INC_NspGMainLoop */ 

#ifdef NspGMainLoop_Private 
static int init_gmainloop(NspGMainLoop *o,NspTypeGMainLoop *type);
static char *nsp_gmainloop_type_as_string(void);
static char *nsp_gmainloop_type_short_string(NspObject *v);
static AttrTab gmainloop_attrs[];
static NspMethods *gmainloop_get_methods(void);
/* static int int_gmainloop_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMainLoop_Private */
