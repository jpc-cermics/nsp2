/* opengl rendering in a gtk widget */
#define PERIGL 

#if 1 
/* render in gtk window */
#include "periGtkstd.c" 
#else 
/* always render in pixmap: 
 * this mode crashes on windows and on X11/Intel. 
 */
#include "periGLGtk.c"
#endif 



