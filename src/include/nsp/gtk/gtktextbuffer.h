/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextBuffer
#define INC_NSP_GtkTextBuffer

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTextBuffer inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTextBuffer ;
typedef NspTypeGObject NspTypeGtkTextBuffer ;

extern int nsp_type_gtktextbuffer_id;
extern NspTypeGtkTextBuffer *nsp_type_gtktextbuffer;

/* type instances for gobject */

NspTypeGtkTextBuffer *new_type_gtktextbuffer(type_mode mode);

/* instance for GtkTextBuffer */

NspGtkTextBuffer *new_gtktextbuffer();

/*
* Object methods redefined for gtktextbuffer 
*/

#define NULLGTKTEXTBUFFER (NspGtkTextBuffer*) 0

NspGtkTextBuffer *gtktextbuffer_create(char *name,NspTypeBase *type);

/* from GtkTextBufferObj.c */

extern NspGtkTextBuffer *gtktextbuffer_object (NspObject *O); 
extern int IsGtkTextBufferObj (Stack stack, int i); 
extern int IsGtkTextBuffer(NspObject *O);
extern NspGtkTextBuffer *GetGtkTextBufferCopy (Stack stack, int i); 
extern NspGtkTextBuffer *GetGtkTextBuffer (Stack stack, int i); 

#endif 

#ifdef GtkTextBuffer_Private 
static int init_gtktextbuffer(NspGtkTextBuffer *o,NspTypeGtkTextBuffer *type);
static char *gtktextbuffer_type_as_string(void);
static char *gtktextbuffer_type_short_string(NspObject *v);
static AttrTab gtktextbuffer_attrs[];
/* static int int_gtktextbuffer_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktextbuffer_get_methods(void); 
#endif /* GtkTextBuffer_Private */
