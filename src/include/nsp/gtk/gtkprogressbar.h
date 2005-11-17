/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkProgressBar
#define INC_NSP_GtkProgressBar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkprogress.h"

/*
* NspGtkProgressBar inherits from NspGtkProgress
* just change some type attributes 
*/

typedef NspGtkProgress NspGtkProgressBar ;
typedef NspTypeGtkProgress NspTypeGtkProgressBar ;

extern int nsp_type_gtkprogressbar_id;
extern NspTypeGtkProgressBar *nsp_type_gtkprogressbar;

/* type instances for gtkprogress */

NspTypeGtkProgressBar *new_type_gtkprogressbar(type_mode mode);

/* instance for GtkProgressBar */

NspGtkProgressBar *new_gtkprogressbar();

/*
* Object methods redefined for gtkprogressbar 
*/

#define NULLGTKPROGRESSBAR (NspGtkProgressBar*) 0

NspGtkProgressBar *gtkprogressbar_create(char *name,NspTypeBase *type);

/* from GtkProgressBarObj.c */

extern NspGtkProgressBar *gtkprogressbar_object (NspObject *O); 
extern int IsGtkProgressBarObj (Stack stack, int i); 
extern int IsGtkProgressBar(NspObject *O);
extern NspGtkProgressBar *GetGtkProgressBarCopy (Stack stack, int i); 
extern NspGtkProgressBar *GetGtkProgressBar (Stack stack, int i); 

#endif 

#ifdef GtkProgressBar_Private 
static int init_gtkprogressbar(NspGtkProgressBar *o,NspTypeGtkProgressBar *type);
static char *gtkprogressbar_type_as_string(void);
static char *gtkprogressbar_type_short_string(void);
static AttrTab gtkprogressbar_attrs[];
/* static int int_gtkprogressbar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkprogressbar_get_methods(void); 
#endif /* GtkProgressBar_Private */
