/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkEventBox
#define INC_NSP_GtkEventBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkEventBox inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkEventBox ;
typedef NspTypeGtkBin NspTypeGtkEventBox ;

extern int nsp_type_gtkeventbox_id;
extern NspTypeGtkEventBox *nsp_type_gtkeventbox;

/* type instances for gtkbin */

NspTypeGtkEventBox *new_type_gtkeventbox(type_mode mode);

/* instance for GtkEventBox */

NspGtkEventBox *new_gtkeventbox();

/*
* Object methods redefined for gtkeventbox 
*/

#ifdef GtkEventBox_Private 
static int init_gtkeventbox(NspGtkEventBox *o,NspTypeGtkEventBox *type);
static char *gtkeventbox_type_as_string(void);
static char *gtkeventbox_type_short_string(void);
static AttrTab gtkeventbox_attrs[];
/* static int int_gtkeventbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkeventbox_get_methods(void); 
#endif /* GtkEventBox_Private */

#define NULLGTKEVENTBOX (NspGtkEventBox*) 0

NspGtkEventBox *gtkeventbox_create(char *name,NspTypeBase *type);

/* from GtkEventBoxObj.c */

extern NspGtkEventBox *gtkeventbox_object (NspObject *O); 
extern int IsGtkEventBoxObj (Stack stack, int i); 
extern int IsGtkEventBox(NspObject *O);
extern NspGtkEventBox *GetGtkEventBoxCopy (Stack stack, int i); 
extern NspGtkEventBox *GetGtkEventBox (Stack stack, int i); 

#endif 
