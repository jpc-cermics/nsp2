
#include <gtk/gtk.h>

#include <nsp/object.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/hash.h>
#include <nsp/plist.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/mpmatrix.h>
#include <nsp/matrix.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>

#if GTK_CHECK_VERSION(2,6,0)
#include "codegen/gtk.c"
#else 
#include "codegen/gtk24.c"
#endif 
