/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIconTheme
#define INC_NSP_GtkIconTheme

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkIconTheme inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkIconTheme ;
typedef NspTypeGObject NspTypeGtkIconTheme ;

extern int nsp_type_gtkicontheme_id;
extern NspTypeGtkIconTheme *nsp_type_gtkicontheme;

/* type instances for gobject */

NspTypeGtkIconTheme *new_type_gtkicontheme(type_mode mode);

/* instance for GtkIconTheme */

NspGtkIconTheme *new_gtkicontheme();

/*
* Object methods redefined for gtkicontheme 
*/

#define NULLGTKICONTHEME (NspGtkIconTheme*) 0

NspGtkIconTheme *gtkicontheme_create(char *name,NspTypeBase *type);

/* from GtkIconThemeObj.c */

extern NspGtkIconTheme *gtkicontheme_object (NspObject *O); 
extern int IsGtkIconThemeObj (Stack stack, int i); 
extern int IsGtkIconTheme(NspObject *O);
extern NspGtkIconTheme *GetGtkIconThemeCopy (Stack stack, int i); 
extern NspGtkIconTheme *GetGtkIconTheme (Stack stack, int i); 

#endif 

#ifdef GtkIconTheme_Private 
static int init_gtkicontheme(NspGtkIconTheme *o,NspTypeGtkIconTheme *type);
static char *gtkicontheme_type_as_string(void);
static char *gtkicontheme_type_short_string(NspObject *v);
static AttrTab gtkicontheme_attrs[];
/* static int int_gtkicontheme_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkicontheme_get_methods(void); 
#endif /* GtkIconTheme_Private */
