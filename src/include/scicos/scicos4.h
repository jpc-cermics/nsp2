#ifndef __SCICOS4_H 
#define __SCICOS4_H 

#include "simul4.h"

/* Define scicos simulator data type number (_N) */
#define SCSREAL_N 10
#define SCSCOMPLEX_N 11
#define SCSINT_N 80
#define SCSINT8_N 81
#define SCSINT16_N 82
#define SCSINT32_N 84
#define SCSUINT_N 800
#define SCSUINT8_N 811
#define SCSUINT16_N 812
#define SCSUINT32_N 814
#define SCSBOOL_N 84
#define SCSUNKNOW_N -1

/* Define scicos simulator data type C operators (_COP) */

#define SCSREAL_COP double
#define SCSCOMPLEX_COP double
#define SCSINT_COP int
#define SCSINT8_COP char
#define SCSINT16_COP short
#define SCSINT32_COP int
#define SCSUINT_COP unsigned int
#define SCSUINT8_COP unsigned char
#define SCSUINT16_COP unsigned short
#define SCSUINT32_COP unsigned int
#define SCSBOOL_COP int
#define SCSUNKNOW_COP double

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
