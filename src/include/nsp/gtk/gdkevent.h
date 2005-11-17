/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkEvent
#define INC_NSP_GdkEvent

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGdkEvent inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGdkEvent ;
typedef NspTypeGBoxed NspTypeGdkEvent ;

extern int nsp_type_gdkevent_id;
extern NspTypeGdkEvent *nsp_type_gdkevent;

/* type instances for gboxed */

NspTypeGdkEvent *new_type_gdkevent(type_mode mode);

/* instance for GdkEvent */

NspGdkEvent *new_gdkevent();

/*
* Object methods redefined for gdkevent 
*/

#define NULLGDKEVENT (NspGdkEvent*) 0

NspGdkEvent *gdkevent_create(char *name,NspTypeBase *type);

/* from GdkEventObj.c */

extern NspGdkEvent *gdkevent_object (NspObject *O); 
extern int IsGdkEventObj (Stack stack, int i); 
extern int IsGdkEvent(NspObject *O);
extern NspGdkEvent *GetGdkEventCopy (Stack stack, int i); 
extern NspGdkEvent *GetGdkEvent (Stack stack, int i); 

#endif 

#ifdef GdkEvent_Private 
static int init_gdkevent(NspGdkEvent *o,NspTypeGdkEvent *type);
static char *gdkevent_type_as_string(void);
static char *gdkevent_type_short_string(void);
static AttrTab gdkevent_attrs[];
/* static int int_gdkevent_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkevent_get_methods(void); 
#endif /* GdkEvent_Private */
