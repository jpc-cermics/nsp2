/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkFrameClockIdle
#define NSP_INC_NspGdkFrameClockIdle

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

/* NspGdkFrameClockIdle */

#include <nsp/gtk/gdkframeclock.h>

/*
 * NspGdkFrameClockIdle inherits from GdkFrameClock
 * just change some type attributes 
 */

typedef NspGdkFrameClock NspGdkFrameClockIdle ;
typedef NspTypeGdkFrameClock NspTypeGdkFrameClockIdle ;

extern int nsp_type_gdkframeclockidle_id;
extern NspTypeGdkFrameClockIdle *nsp_type_gdkframeclockidle;

/* type instances for gdkframeclock */

NspTypeGdkFrameClockIdle *new_type_gdkframeclockidle(type_mode mode);

/* instance for NspGdkFrameClockIdle */

NspGdkFrameClockIdle *new_gdkframeclockidle();

/*
 * Object methods redefined for gdkframeclockidle 
 */

#define NULLGDKFRAMECLOCKIDLE (NspGdkFrameClockIdle*) 0


/* from NspGdkFrameClockIdleObj.c */

extern NspGdkFrameClockIdle *nsp_gdkframeclockidle_object (NspObject *O);
extern int IsGdkFrameClockIdleObj (Stack stack, int i);
extern int IsGdkFrameClockIdle(NspObject *O);
extern NspGdkFrameClockIdle *GetGdkFrameClockIdleCopy (Stack stack, int i);
extern NspGdkFrameClockIdle *GetGdkFrameClockIdle (Stack stack, int i);

#endif /* NSP_INC_NspGdkFrameClockIdle */ 

#ifdef NspGdkFrameClockIdle_Private 
static int init_gdkframeclockidle(NspGdkFrameClockIdle *o,NspTypeGdkFrameClockIdle *type);
static char *nsp_gdkframeclockidle_type_as_string(void);
static char *nsp_gdkframeclockidle_type_short_string(NspObject *v);
static AttrTab gdkframeclockidle_attrs[];
static NspMethods *gdkframeclockidle_get_methods(void);
/* static int int_gdkframeclockidle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkFrameClockIdle_Private */
