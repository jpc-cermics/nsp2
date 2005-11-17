
#include <gtk/gtk.h>

#if GTK_CHECK_VERSION(2,6,0)
#include "codegen/gtk.c"
#else 
#include "codegen/gtk24.c"
#endif 
