/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellRendererPixbuf
#define INC_NSP_GtkCellRendererPixbuf

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcellrenderer.h"

/*
* NspGtkCellRendererPixbuf inherits from NspGtkCellRenderer
* just change some type attributes 
*/

typedef NspGtkCellRenderer NspGtkCellRendererPixbuf ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererPixbuf ;

extern int nsp_type_gtkcellrendererpixbuf_id;
extern NspTypeGtkCellRendererPixbuf *nsp_type_gtkcellrendererpixbuf;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererPixbuf *new_type_gtkcellrendererpixbuf(type_mode mode);

/* instance for GtkCellRendererPixbuf */

NspGtkCellRendererPixbuf *new_gtkcellrendererpixbuf();

/*
* Object methods redefined for gtkcellrendererpixbuf 
*/

#define NULLGTKCELLRENDERERPIXBUF (NspGtkCellRendererPixbuf*) 0

NspGtkCellRendererPixbuf *gtkcellrendererpixbuf_create(char *name,NspTypeBase *type);

/* from GtkCellRendererPixbufObj.c */

extern NspGtkCellRendererPixbuf *gtkcellrendererpixbuf_object (NspObject *O); 
extern int IsGtkCellRendererPixbufObj (Stack stack, int i); 
extern int IsGtkCellRendererPixbuf(NspObject *O);
extern NspGtkCellRendererPixbuf *GetGtkCellRendererPixbufCopy (Stack stack, int i); 
extern NspGtkCellRendererPixbuf *GetGtkCellRendererPixbuf (Stack stack, int i); 

#endif 

#ifdef GtkCellRendererPixbuf_Private 
static int init_gtkcellrendererpixbuf(NspGtkCellRendererPixbuf *o,NspTypeGtkCellRendererPixbuf *type);
static char *gtkcellrendererpixbuf_type_as_string(void);
static char *gtkcellrendererpixbuf_type_short_string(void);
static AttrTab gtkcellrendererpixbuf_attrs[];
/* static int int_gtkcellrendererpixbuf_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellrendererpixbuf_get_methods(void); 
#endif /* GtkCellRendererPixbuf_Private */
