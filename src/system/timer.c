/* Nsp
 * Copyright (C) 2007-2019 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2007-2019 Bruno Pin�on Esial/Iecn
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * 
 *------------------------------------------------------------------*/

#include <glib.h>
#include <stdio.h>
#include <time.h>
#include "nsp/machine.h"
#include "nsp/math.h"
#include <nsp/system.h>
#include "nsp/sciio.h"

#ifdef HAVE_SYS_RESOURCE_H 
#include <sys/resource.h>
#endif 

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
 * If getrusage() is found it is used else clock() is used. 
 * Note that acording to clock man page:  the  time  can  wrap  around.  
 * On a 32bit system  where CLOCKS_PER_SEC  equals 1000000 this function 
 * will return the same value approximately every 72 minutes.
 * 
 * Return value: a double.
 **/

#ifdef HAVE_GETRUSAGE
/* exists in  psapi.dll for full getrusage function emulation on windows 
 */

double nsp_timer(void)
{
  static struct rusage usage1={{0,0},{0,0}};
  struct rusage usage2;
  double etime;
  if ( getrusage(RUSAGE_SELF,&usage2) !=0) return 0;
  etime= (double)(usage2.ru_utime.tv_sec - usage1.ru_utime.tv_sec);
  etime += (double)(usage2.ru_stime.tv_sec - usage1.ru_stime.tv_sec);
  etime +=  1.0e-6 *(usage2.ru_utime.tv_usec - usage1.ru_utime.tv_usec);
  etime +=  1.0e-6 *(usage2.ru_stime.tv_usec - usage1.ru_stime.tv_usec);
  usage1=usage2;
  return etime;
}

#else 
#ifdef HAVE_CLOCK

double nsp_timer(void)
{
  double etime;
  static clock_t t1 = (clock_t) -1 ;
  clock_t t2 = clock();
  if ( t1 == (clock_t) -1 ) t1 = t2;
  etime=(double)((double)(t2 - t1)/(double)CLOCKS_PER_SEC);
  t1 = t2;
  return etime;
}

#else  /* HAVE_CLOCK */

double nsp_timer_void(void)
{
  Sciprintf("Warning: timer is not available\n");
  return 0;
}

#endif  /* HAVE_CLOCK */
#endif  /* HAVE_GETRUSAGE */

/**
 * nsp_cputime:
 * 
 * returns the elapsed processor time since nsp started. 
 * If getrusage() is found it is used else clock() is used. 
 * Note that acording to clock man page:  the  time  can  wrap  around.  
 * On a 32bit system  where CLOCKS_PER_SEC  equals 1000000 this function 
 * will return the same value approximately every 72 minutes.
 * 
 * Return value: a double.
 **/

#ifdef HAVE_GETRUSAGE
/* exists in  psapi.dll for full getrusage function emulation on windows 
 */

double nsp_cputime(void)
{
  double etime;
  struct rusage usage2;
  if ( getrusage(RUSAGE_SELF,&usage2) !=0) return 0;
  etime= (double)(usage2.ru_utime.tv_sec);
  etime += (double)(usage2.ru_stime.tv_sec);
  etime +=  1.0e-6 *(usage2.ru_utime.tv_usec);
  etime +=  1.0e-6 *(usage2.ru_stime.tv_usec);
  return etime;
}

#else 

#ifdef HAVE_CLOCK

double nsp_cputime(void)
{
  clock_t t2 = clock();
  return (double)((double)(t2)/(double)CLOCKS_PER_SEC);
}

#else 

double nsp_cputime_void(void)
{
  Sciprintf("Warning: timer is not available\n");
  return 0;
}

#endif  /* HAVE_CLOCK */
#endif  /* HAVE_GETRUSAGE */


/**
 * nsp_stimer:
 * @void: 
 * 
 * returns the microseconds part of timeofday using g_get_current_time().
 * Equivalent to the UNIX gettimeofday() function, but portable.
 * Represents a precise time, with seconds and microseconds. 
 * 
 * Return value: the microseconds part of timeofday as an integer.
 **/

int nsp_stimer(void)
{
  GTimeVal time;
  g_get_current_time(&time);
  return time.tv_usec;
}

/**
 * nsp_toc:
 * @etime: a pointer to a double (previously returned by tic).
 * 
 * 
 * returns the elapsed time between a start_time and the current time. 
 * if @etime is non null then it is used as a start time if it is null then 
 * the start time is given by @start_time.
 * 
 * Returns: a double 
 **/

static double start_time=0; 

double nsp_toc(double *etime)
{
  double res;
  GTimeVal time;
  g_get_current_time(&time);
  res = (double) time.tv_sec + 1e-6 * (double) time.tv_usec;
  res -= (etime != NULL) ? *etime : start_time;
  return res;
}

/**
 * nsp_tic:
 * @set_start_time: an integer 
 * 
 * returns the current time in a double and if @set_start_time is TRUE then @start_time 
 * is set to the value of the current time. 
 *
 * Returns: a double 
 **/

double nsp_tic(int set_start_time)
{
  double res; 
  GTimeVal time;
  g_get_current_time(&time);
  res =  (double) time.tv_sec + 1e-6 * (double) time.tv_usec;
  if ( set_start_time == TRUE )  start_time = res ; 
  return res; 
}




