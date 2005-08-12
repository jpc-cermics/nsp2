/* 
 * modified Scilab file 
 * This Software is ( Copyright INRIA/ENPC  ) 
 * 
 * <<realtime>> primitives.
 * XXXX should be changed using glib portable 
 * primitives.
 */

#include <math.h>
#include <stdio.h>
#include <string.h> 
#include <time.h>
#include "nsp/machine.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/system.h"

#ifdef HAVE_SYS_TIME_H 
#include <sys/time.h>
#endif 

#ifndef HAVE_INLINE 
#define inline
#endif

#ifndef WIN32
#include <unistd.h>
#else 
#include <windows.h>
#endif /* WIN32 */

int nsp_realtime(double *t);
int nsp_realtime_init(double *t,double *scale); 

#ifndef WIN32

static inline unsigned long long TIME2ULL(struct timeval t)
{
  unsigned long long result;
  result = (unsigned long long)t.tv_sec * 1000000 + t.tv_usec;
  return result;
}

static inline struct timeval ULL2TIME(unsigned long long t)
{
  struct timeval result;
  result.tv_sec = t / 1000000;
  result.tv_usec = t % 1000000;
  return result;
}

static double simulation_start = 0;
static double simulation_scale = 0;
static unsigned long long realtime_start = 0;
static int simulation_doinit = 1;

int nsp_realtime_init( double *t,  double *scale)
{
  struct timeval now;
  gettimeofday(&now, 0); 
  /*  printf("Realtime init %f %f\n", *t, *scale);*/
  realtime_start = TIME2ULL(now);
  simulation_doinit = 1;
  simulation_scale = *scale;
  return 0;
}

int nsp_realtime(double *t)
{
  struct timeval now;
  unsigned long long realtime_diff;
  double simulation_diff;
  long long delay;
  if (simulation_doinit) {
    simulation_doinit = 0;
    simulation_start = *t;
  }
  gettimeofday(&now, 0); 
  realtime_diff = TIME2ULL(now) - realtime_start;
  simulation_diff = (*t - simulation_start) * simulation_scale;
  delay = (long long)(simulation_diff * 1000000) - realtime_diff;
  /*  printf("Realtime diff %Ld %f -> %Ld (t=%f)\n", 
   *  realtime_diff, simulation_diff, delay, *t);
   */
  if (delay > 0) {
    struct timeval d ;
    d = ULL2TIME(delay);
    select(0, 0, 0, 0, &d);
  }
  return 0;
}

#else

/* WIN32 */

static double simulation_start = 0;
static double simulation_scale = 0;
static __int64 realtime_start = 0;
static int simulation_doinit = 1;
union {FILETIME ftFileTime;
  __int64  ftInt64;
} ftRealTime; 

int nsp_realtime_init( double *t,  double *scale)
{
  SYSTEMTIME st;
  GetSystemTime(&st);
  SystemTimeToFileTime(&st,&ftRealTime.ftFileTime); 

  realtime_start = ftRealTime.ftInt64;
  simulation_doinit = 1;
  simulation_scale = *scale;
  return 0;
}

int nsp_realtime(double *t)
{
  __int64 realtime_diff;
  double simulation_diff;
  __int64 delay;
  SYSTEMTIME st;

  if (simulation_doinit) {
    simulation_doinit = 0;
    simulation_start = *t;
  }
  GetSystemTime(&st);
  SystemTimeToFileTime(&st,&ftRealTime.ftFileTime);

  realtime_diff = ftRealTime.ftInt64  - realtime_start;
  simulation_diff = (*t - simulation_start) * simulation_scale;
  delay = (__int64)(simulation_diff * 10000000) - realtime_diff;
  /*  printf("Realtime diff %Ld %f -> %Ld (t=%f)\n", 
   *  realtime_diff, simulation_diff, delay, *t);
   */
  if (delay > 0) {
    Sleep((DWORD)(delay/10000));
  }
  return 0;
}
#endif
