/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCalendar
#define NSP_INC_NspGtkCalendar

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

/* NspGtkCalendar */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkCalendar inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkCalendar ;
typedef NspTypeGtkWidget NspTypeGtkCalendar ;

extern int nsp_type_gtkcalendar_id;
extern NspTypeGtkCalendar *nsp_type_gtkcalendar;

/* type instances for gtkwidget */

NspTypeGtkCalendar *new_type_gtkcalendar(type_mode mode);

/* instance for NspGtkCalendar */

NspGtkCalendar *new_gtkcalendar();

/*
 * Object methods redefined for gtkcalendar 
 */

#define NULLGTKCALENDAR (NspGtkCalendar*) 0


/* from NspGtkCalendarObj.c */

extern NspGtkCalendar *nsp_gtkcalendar_object (NspObject *O);
extern int IsGtkCalendarObj (Stack stack, int i);
extern int IsGtkCalendar(NspObject *O);
extern NspGtkCalendar *GetGtkCalendarCopy (Stack stack, int i);
extern NspGtkCalendar *GetGtkCalendar (Stack stack, int i);

#endif /* NSP_INC_NspGtkCalendar */ 

#ifdef NspGtkCalendar_Private 
static int init_gtkcalendar(NspGtkCalendar *o,NspTypeGtkCalendar *type);
static char *nsp_gtkcalendar_type_as_string(void);
static char *nsp_gtkcalendar_type_short_string(NspObject *v);
static AttrTab gtkcalendar_attrs[];
static NspMethods *gtkcalendar_get_methods(void);
/* static int int_gtkcalendar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCalendar_Private */
