#ifndef NSP_INC_GSORT_H 
#define NSP_INC_GSORT_H 

extern void sciqsort(char *a, char *tab, int flag, int n, int es, int es1, 
		     int (*cmp) (), int (*swapcode) (), int (*swapcodeind) ());

#define swapcodeind CNAME(swapcode,int)

#include "nsp/gsort-int.h"
#include "nsp/gsort-double.h"
#include "nsp/gsort-string.h"
#include "nsp/gsort-p.h"

#endif 
