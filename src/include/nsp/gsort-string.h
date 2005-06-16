#ifndef NSP_INC_GSORT_STRING
#define NSP_INC_GSORT_STRING

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#ifdef  ELT_TYPE 
#undef  ELT_TYPE 
#endif 

#define STRING_ONLY 
#define ELT_TYPE nsp_string
#include "gsort-gen.h" 
#undef STRING_ONLY

#endif 






