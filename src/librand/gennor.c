/* ********************************************************************** */
/*     DOUBLE PRECISION FUNCTION GENNOR( AV, SD ) */
/*         GENerate random deviate from a NORmal distribution */
/*                              Function */
/*     Generates a single random deviate from a normal distribution */
/*     with mean, AV, and standard deviation, SD. */
/*                              Arguments */
/*     AV --> Mean of the normal distribution. */
/*                              DOUBLE PRECISION AV */
/*     SD --> Standard deviation of the normal distribution. */
/*                              DOUBLE PRECISION SD */
/*     JJV                      (SD >= 0) */
/*     GENNOR <-- Generated normal deviate. */
/*                              DOUBLE PRECISION GENNOR */
/*                              Method */
/*     Renames SNORM from TOMS as slightly modified by BWB to use RANF */
/*     instead of SUNIF. */
/*     For details see: */
/*               Ahrens, J.H. and Dieter, U. */
/*               Extensions of Forsythe's Method for Random */
/*               Sampling from the Normal Distribution. */
/*               Math. Comput., 27,124 (Oct. 1973), 927 - 937. */
#include "grand.h"

double
rand_gennor (double av, double sd)
{
  double ret_val;
  ret_val = sd * rand_snorm () + av;
  return ret_val;
}		

