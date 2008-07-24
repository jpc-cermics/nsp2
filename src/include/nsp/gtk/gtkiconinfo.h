/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIconInfo
#define INC_NSP_GtkIconInfo

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkIconInfo inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkIconInfo ;
typedef NspTypeGBoxed NspTypeGtkIconInfo ;

extern int nsp_type_gtkiconinfo_id;
extern NspTypeGtkIconInfo *nsp_type_gtkiconinfo;

/* type instances for gboxed */

NspTypeGtkIconInfo *new_type_gtkiconinfo(type_mode mode);

/* instance for GtkIconInfo */

NspGtkIconInfo *new_gtkiconinfo();

/*
* Object methods redefined for gtkiconinfo 
*/

#define NULLGTKICONINFO (NspGtkIconInfo*) 0

NspGtkIconInfo *gtkiconinfo_create(char *name,NspTypeBase *type);

/* from GtkIconInfoObj.c */

extern NspGtkIconInfo *gtkiconinfo_object (NspObject *O); 
extern int IsGtkIconInfoObj (Stack stack, int i); 
extern int IsGtkIconInfo(NspObject *O);
extern NspGtkIconInfo *GetGtkIconInfoCopy (Stack stack, int i); 
extern NspGtkIconInfo *GetGtkIconInfo (Stack stack, int i); 

#endif 

#ifdef GtkIconInfo_Private 
static int init_gtkiconinfo(NspGtkIconInfo *o,NspTypeGtkIconInfo *type);
static char *gtkiconinfo_type_as_string(void);
static char *gtkiconinfo_type_short_string(NspObject *v);
static AttrTab gtkiconinfo_attrs[];
/* static int int_gtkiconinfo_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkiconinfo_get_methods(void); 
#endif /* GtkIconInfo_Private */
