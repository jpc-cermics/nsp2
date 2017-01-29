/* opengl rendering in a gtk widget 
 * used when gtkglext exists i.e for gtk2 
 */

#include <nsp/config.h> 

#ifdef WITH_GTKGLEXT 
/* rendering with gtkglext is not done if GTKGLEXT is not present */
#define PERIGL 
#include "periGtkstd.c" 
#endif 



