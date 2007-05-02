/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCalendar
#define INC_NSP_GtkCalendar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkCalendar inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkCalendar ;
typedef NspTypeGtkWidget NspTypeGtkCalendar ;

extern int nsp_type_gtkcalendar_id;
extern NspTypeGtkCalendar *nsp_type_gtkcalendar;

/* type instances for gtkwidget */

NspTypeGtkCalendar *new_type_gtkcalendar(type_mode mode);

/* instance for GtkCalendar */

NspGtkCalendar *new_gtkcalendar();

/*
* Object methods redefined for gtkcalendar 
*/

#define NULLGTKCALENDAR (NspGtkCalendar*) 0

NspGtkCalendar *gtkcalendar_create(char *name,NspTypeBase *type);

/* from GtkCalendarObj.c */

extern NspGtkCalendar *gtkcalendar_object (NspObject *O); 
extern int IsGtkCalendarObj (Stack stack, int i); 
extern int IsGtkCalendar(NspObject *O);
extern NspGtkCalendar *GetGtkCalendarCopy (Stack stack, int i); 
extern NspGtkCalendar *GetGtkCalendar (Stack stack, int i); 

#endif 

#ifdef GtkCalendar_Private 
static int init_gtkcalendar(NspGtkCalendar *o,NspTypeGtkCalendar *type);
static char *gtkcalendar_type_as_string(void);
static char *gtkcalendar_type_short_string(NspObject *v);
static AttrTab gtkcalendar_attrs[];
/* static int int_gtkcalendar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcalendar_get_methods(void); 
#endif /* GtkCalendar_Private */
