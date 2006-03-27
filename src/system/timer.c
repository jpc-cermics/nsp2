/* 
 * modified Scilab file 
 * This Software is ( Copyright INRIA/ENPC )
 * 
 */

#include <glib.h>
#include <stdio.h>
#include <time.h>
#include "nsp/machine.h"
#include "nsp/system.h"

#ifdef HAVE_SYS_TIME_H 
#include <sys/time.h>
#endif 
#ifndef CLOCKS_PER_SEC
#if defined(sun)
#define CLOCKS_PER_SEC 1000000
#endif
#endif

/**
 * nsp_timer:
 * 
 * returns the elapsed processor time between successive calls.
 * 
 * Return value: a double.
 **/

double nsp_timer()
{
  double etime;
  static clock_t t1 = (clock_t) -1 ;
  clock_t t2 = clock();
  if ( t1 == (clock_t) -1 ) t1 = t2;
  etime=(double)((double)(t2 - t1)/(double)CLOCKS_PER_SEC);
  t1 = t2;
  return etime;
}

/**
 * nsp_stimer:
 * @void: 
 * 
 * returns the microseconds part of timeofday using g_get_current_time().
 * Equivalent to the UNIX gettimeofday() function, but portable.
 * Represents a precise time, with seconds and microseconds. Same as
 * Return value: 
 **/

int nsp_stimer(void)
{
  GTimeVal time;
  g_get_current_time(&time);
  return time.tv_usec;
}


