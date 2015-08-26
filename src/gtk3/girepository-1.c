#include <nsp/nsp.h>
#include <nsp/config.h>

#ifdef WITH_GI
#include "girepository.c"
int nsp_gi_def(void) { return TRUE;}
#else
int nsp_gi_def(void) { return FALSE;}
#endif
