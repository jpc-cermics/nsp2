/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/
#include <string.h> /* pour memmove */
#include "nsp/math.h"

/***** XXXXXXX  memmove est plus lent mais accepte les overlap de region ***/
/***** Voir les copies pour tous le reste **/

void C2F(dcopymem)(n, dx,dy )
     int *n;
     double *dx,*dy;
{
  /*   memcpy((void *) dy,(void *) dx, (*n)*sizeof(double)) ; */
  memmove((void *) dy,(void *) dx, (*n)*sizeof(double)) ; 
} 
