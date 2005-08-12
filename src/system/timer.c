/*
 * This Software is ( Copyright INRIA/ENPC 1998 ) 
 * 
 */

#include <stdio.h>
#include <time.h>
#include "nsp/machine.h"

#ifdef HAVE_SYS_TIME_H 
#include <sys/time.h>
#endif 

#if defined(THINK_C) || defined(__MWERKS__)
#include <Threads.h> 
#endif

#ifndef CLOCKS_PER_SEC
#if defined(sun)
#define CLOCKS_PER_SEC 1000000
#endif
#endif

/**
 * nsp_timer:
 * 
 * returns the elapsed processor time betwwen successive calls.
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

/* define X_GETTIMEOFDAY macro, a portable gettimeofday() */

#if  defined(VMS)
#define X_GETTIMEOFDAY(t) gettimeofday(t)
#else
#if defined(THINK_C) || defined(__MWERKS__)
#define X_GETTIMEOFDAY(t) 0 
#else
#if defined(WIN32)
#ifndef __MSC__
#ifndef  __MINGW32__
#define X_GETTIMEOFDAY(t) gettimeofday(t, &tmz )
static struct timezone tmz;
#else
#define X_GETTIMEOFDAY(t) 0
#endif /* __MINGW32__ */
#else
#define X_GETTIMEOFDAY(t) 0
#endif /* MSC__ */
#else
#define X_GETTIMEOFDAY(t) gettimeofday(t, (struct timezone*)0)
#endif
#endif
#endif 

/*
 * stimer for non cygwin win32 compilers 
 */

#ifdef WIN32 
#ifndef __CYGWIN32__
#include <windows.h>

static int stimerwin(void)
{
#ifndef __MINGW32__
  int i;
  union {FILETIME ftFileTime;
    __int64  ftInt64;
  } ftRealTime; 
  SYSTEMTIME st;
  GetSystemTime(&st);
  SystemTimeToFileTime(&st,&ftRealTime.ftFileTime);
  /* Filetimes are in 100NS units */
  i= (int) (ftRealTime.ftInt64  & ((LONGLONG) 0x0ffffffff));
  return( i/10); /* convert to microseconds */
#else 
  return(0);
#endif
}
#endif
#endif

/**
 * nsp_stimer:
 * @void: 
 * 
 * returns the timeofday in  microseconds 
 * 
 * Return value: 
 **/

int nsp_stimer(void)
{
#if defined(THINK_C)||defined(__MWERKS__) 
  YieldToAnyThread();
  return(0);
#else 
#ifndef __MSC__
#ifndef __MINGW32__
  struct timeval ctime;
  X_GETTIMEOFDAY(&ctime);
  return(ctime.tv_usec);
#endif
#endif

#ifdef WIN32 
#ifndef __CYGWIN32__
  return(stimerwin());
#endif 
#endif
#endif /* defined(THINK_C)||defined(__MWERKS__) */
}




