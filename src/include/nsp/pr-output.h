#ifndef SCI_PR_OUTPUT
#define SCI_PR_OUTPUT

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics        
 *********************************************************************/

#include <stdio.h>
#include "nsp/object.h"

void scilab_print_internal (double d);
void scilab_print_internalM (NspMatrix *m,int indent);
void scilab_print_internalC (doubleC c);
void scilab_print_internalCM (NspMatrix *cm,int indent);
int scilab_print_internalPM (NspPMatrix *m,int indent);
int scilab_print_internalSM (NspSMatrix *m,int indent);
void scilab_print_internalBM (NspBMatrix *m,int indent);
void scilab_print_internalSpM (NspSpMatrix *m,int indent);

#endif 
