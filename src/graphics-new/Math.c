/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

double Mini(const double vect[], int n)
{
  int i;
  double vmin;
  vmin = LARGEST_REAL;
  for (i = 0 ; i < n ; i++)
    /*    if ( isinf(vect[i])== 0 && isnan(vect[i])==0 && vect[i] < vmin)  */
    if ( finite(vect[i])== 1 && vect[i] < vmin) 
      vmin=vect[i];
  return(vmin);
}

double Maxi(const double vect[], int n)
{
  int i;
  double maxi;
  maxi= - LARGEST_REAL;
  for (i =0 ; i < n ; i++)
    /* if ( isinf(vect[i])== 0 && isnan(vect[i])==0 && vect[i] > maxi) */
    if ( finite(vect[i])== 1 && vect[i] > maxi) 
      maxi=vect[i];
  return(maxi);
}
