#ifndef NSP_INC_PR_OUTPUT
#define NSP_INC_PR_OUTPUT

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>
#include "nsp/object.h"

void nsp_print_internal (double d);
void nsp_print_internalM (NspMatrix *m,int indent);
void nsp_print_internalC (doubleC c);
void nsp_print_internalCM (NspMatrix *cm,int indent);
int nsp_print_internalPM (NspPMatrix *m,int indent);
int nsp_print_internalSM (const NspSMatrix *m,int indent);
void nsp_print_internalBM (NspBMatrix *m,int indent);
void nsp_print_internalSpM (NspSpMatrix *m,int indent);


/* used to get the format used for displaying numbers of a given 
 * matrix 
 */

const char *get_curr_real_fmt(void);
const char *get_curr_imag_fmt(void);
void M_set_format(NspMatrix *M);

#endif 
