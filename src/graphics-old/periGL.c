/* opengl rendering in a gtk widget */

#include <nsp/config.h> 

#ifdef WITH_GTKGLEXT 
/* rendering with gtkglext is not done if GTKGLEXT is not present */

#define PERIGL 
#if 1
/* render in gtk window */
#include "periGtkstd.c" 
#else 
/* always render in pixmap: 
 * this mode crashes on windows and on X11/Intel. 
 */
#define PERIGLGTK 
#include "periGtkstd.c"
#endif 

#endif 


