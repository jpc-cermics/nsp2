#ifndef __SCICOS_H 
#define __SCICOS_H 

#include "simul.h"

extern scicos_run *Scicos;

/* maximum value for sum of number of inputs 
 * and outputs ports of a given 
 * block of type 2 
 */

#define SZ_SIZE 60

/* maximum value for sum of number of inputs 
 * and outputs of a given block 
 * of type 0 */

#define TB_SIZE 500

typedef void (*voidf)();

#include "scicos-proto.h"

#endif 
