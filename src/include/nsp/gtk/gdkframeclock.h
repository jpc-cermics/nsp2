/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkFrameClock
#define NSP_INC_NspGdkFrameClock

/*
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGdkFrameClock */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkFrameClock inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkFrameClock ;
typedef NspTypeGObject NspTypeGdkFrameClock ;

extern int nsp_type_gdkframeclock_id;
extern NspTypeGdkFrameClock *nsp_type_gdkframeclock;

/* type instances for gobject */

NspTypeGdkFrameClock *new_type_gdkframeclock(type_mode mode);

/* instance for NspGdkFrameClock */

NspGdkFrameClock *new_gdkframeclock();

/*
 * Object methods redefined for gdkframeclock 
 */

#define NULLGDKFRAMECLOCK (NspGdkFrameClock*) 0


/* from NspGdkFrameClockObj.c */

extern NspGdkFrameClock *nsp_gdkframeclock_object (NspObject *O);
extern int IsGdkFrameClockObj (Stack stack, int i);
extern int IsGdkFrameClock(NspObject *O);
extern NspGdkFrameClock *GetGdkFrameClockCopy (Stack stack, int i);
extern NspGdkFrameClock *GetGdkFrameClock (Stack stack, int i);

#endif /* NSP_INC_NspGdkFrameClock */ 

#ifdef NspGdkFrameClock_Private 
static int init_gdkframeclock(NspGdkFrameClock *o,NspTypeGdkFrameClock *type);
static char *nsp_gdkframeclock_type_as_string(void);
static char *nsp_gdkframeclock_type_short_string(NspObject *v);
static AttrTab gdkframeclock_attrs[];
static NspMethods *gdkframeclock_get_methods(void);
/* static int int_gdkframeclock_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkFrameClock_Private */
