#ifndef NSP_INC_OPENGL
#define NSP_INC_OPENGL

/*
 * This Software is GPL (Copyright ENPC 2016-2016) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <nsp/config.h> 

/* using gtk2 with gtkglext */
#if defined(WITH_GTKGLEXT)
#define WITH_OPENGL 1
#endif

#include <gtk/gtk.h>
#if GTK_CHECK_VERSION(3,16,0)
/* #define WITH_OPENGL 1 */
#endif

#endif /* NSP_INC_OPENGL */




