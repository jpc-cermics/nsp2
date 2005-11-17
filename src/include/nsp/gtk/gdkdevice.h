/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkDevice
#define INC_NSP_GdkDevice

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkDevice inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkDevice ;
typedef NspTypeGObject NspTypeGdkDevice ;

extern int nsp_type_gdkdevice_id;
extern NspTypeGdkDevice *nsp_type_gdkdevice;

/* type instances for gobject */

NspTypeGdkDevice *new_type_gdkdevice(type_mode mode);

/* instance for GdkDevice */

NspGdkDevice *new_gdkdevice();

/*
* Object methods redefined for gdkdevice 
*/

#define NULLGDKDEVICE (NspGdkDevice*) 0

NspGdkDevice *gdkdevice_create(char *name,NspTypeBase *type);

/* from GdkDeviceObj.c */

extern NspGdkDevice *gdkdevice_object (NspObject *O); 
extern int IsGdkDeviceObj (Stack stack, int i); 
extern int IsGdkDevice(NspObject *O);
extern NspGdkDevice *GetGdkDeviceCopy (Stack stack, int i); 
extern NspGdkDevice *GetGdkDevice (Stack stack, int i); 

#endif 

#ifdef GdkDevice_Private 
static int init_gdkdevice(NspGdkDevice *o,NspTypeGdkDevice *type);
static char *gdkdevice_type_as_string(void);
static char *gdkdevice_type_short_string(void);
static AttrTab gdkdevice_attrs[];
/* static int int_gdkdevice_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkdevice_get_methods(void); 
#endif /* GdkDevice_Private */
