/* 
 * modified Scilab file 
 * This Software is ( Copyright INRIA/ENPC )
 * 
 */

#include <glib.h>
#include <stdio.h>
#include <time.h>
#include "nsp/machine.h"
#include "nsp/math.h"
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

/**
 * nsp_tictoc:
 * 
 *    start if etime == NULL 
 * or stop (and compute elapsed time) if etime != NULL
 * a clock using g_get_current_time(). Equivalent to the UNIX gettimeofday() 
 * function, but portable.
 *
 * Return value: %OK or %FAIL
 **/
#define TIC 0
#define TOC 1
int nsp_tictoc(double *etime)
{
  static double start_time=0; 
  static int last_call = TOC;
  
  GTimeVal time;

  g_get_current_time(&time);

  if ( etime != NULL )  /* toc */
    {
      if ( last_call != TIC )
	return FAIL;
      *etime = ((double) time.tv_sec + 1e-6 * (double) time.tv_usec) - start_time;
      last_call = TOC;
    }
  else                  /* tic */
    {
      start_time = (double) time.tv_sec + 1e-6 * (double) time.tv_usec;
      last_call = TIC;
    }

  return OK;
}




