#ifndef NSP_INC_MATH 
#define NSP_INC_MATH 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <nsp/machine.h> 

#ifndef TRUE
#define TRUE (1)
#endif 

#ifndef FALSE 
#define FALSE (0)
#endif 

#define OK 0
#define FAIL 1

/* sur sun solaris finite est dans ieeefp **/

#if (defined(sun) && defined(SYSV)) 
#include <ieeefp.h>
#endif 

#ifndef HAVE_FINITE
#if defined(WIN32) && !(defined __CYGWIN32__) && !(defined __ABSC__)
#include <float.h>
#define finite(x) _finite(x) 
#else 
/* should do something better here */
#define finite(x) FINITE_IS_UNDEFINED
#endif
#endif 

#ifndef HAVE_ISNAN 
#if defined(WIN32) && !(defined __CYGWIN32__) && !(defined __ABSC__)
#include <float.h>
#define isnan(x) _isnan(x)
#else 
/* should do something better here */
#define isnan(x)  ISNAN_IS_UNDEFINED
#endif 
#endif 

/* just a synonym */

#define ISNAN(x) isnan(x)

#ifndef Abs 
#define Abs(x) ( ( (x) >= 0) ? (x) : -( x) )
#endif

#ifndef Min
#define Min(x,y)	(((x)<(y))?(x):(y))
#endif 

#ifndef Max 
#define Max(x,y)	(((x)>(y))?(x):(y))
#endif 

extern double Mini  (const double vect[],int);
extern double Maxi  (const double vect[],int);

#define PI0 (int *) 0
#define PD0 (double *) 0
#define SMDOUBLE 1.e-200 /* Smalest number to avoid dividing by zero */

#define linint(x) ((int) floor(x + 0.5 )) 
#define inint(x) ((int) floor(x + 0.5 ))  

/* round */
#if 0
#define anint(x) rint(x)   
#else 
#define anint(x) ((x) >= 0.0 ? floor((x)+0.5) : ceil((x)-0.5))
#endif 

/* partie entiere **/
#define aint(x) ((x>= 0 ) ? floor(x)  : ceil(x))
/* from fortran but no pointer here */
#define d_nint(x) (x)>=0 ? floor(x + .5) : -floor(.5 - x)


/* missing prototype */
double tgamma(double);

/* Les arguments des fonction XWindows doivent etre des int16 ou unsigned16 */

#define int16max   0x7FFF
#define uns16max   0xFFFF

#ifdef lint5
#include <sys/stdtypes.h>
#define MALLOC(x) malloc(((size_t) x))
#define FREE(x) {if (x  != NULL) { free((void *) x); x= NULL;};}
#define REALLOC(x,y) realloc((void *) x,(size_t) y)
#else
#define MALLOC(x) malloc(((unsigned) x))
#define FREE(x) {if (x  != NULL) {free((char *) x); x = NULL;}}
#define REALLOC(x,y) realloc((char *) x,(unsigned) y)
#endif

/* void name for object **/
#define NVOID ""

#if defined(WIN32) || defined(__STDC__)
# ifndef HAS_STDARG
#  define HAS_STDARG
# endif
#else 
#endif 



/* M_PI and M_E are defined in math.h */

#if defined(HAVE_VALUES_H)
#include <values.h>
#endif 

#ifndef M_PI
#define M_PI    3.14159265358979323846 
#endif

#ifndef M_E
#define M_E     2.7182818284590452354
#endif 

#ifndef M_LOG10E
#define M_LOG10E 0.43429448190325182765
#endif

#ifdef WIN32 
extern double acosh(double);
extern double asinh(double);
extern double atanh(double);
#endif 

/*
 * 
 */

extern  double nsp_dlamch (char *cmach);


/* A union which permits us to convert between a double and two 32 bit ints.  
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * 
 * This union and SET_LOW_WORD are used to imporve the computation of exp(x*x) 
 * as in libc (See below).
 */

/* XXX  endian.h should be checked at configure time 
 * if endian.h does not exists the we can do as in WIN32 case 
 */

#ifndef WIN32 

#ifdef __APPLE__
#include <machine/endian.h>
#else
#include <endian.h>
#endif /* __APPLE__ */
#include <sys/types.h>

/* compile time check */

typedef union
{
  double value;
  struct
  {
#if __FLOAT_WORD_ORDER == BIG_ENDIAN
    u_int32_t msw;
    u_int32_t lsw;
#else 
    u_int32_t lsw;
    u_int32_t msw;
#endif 
  } parts;
} ieee_double_shape_type;


#define SET_LOW_WORD(d,v)					\
  do {								\
    ieee_double_shape_type sl_u;				\
    sl_u.value = (d);						\
    sl_u.parts.lsw = (v);					\
    (d) = sl_u.value;						\
  } while (0)

#else 

/* here we use a runntime check */
#include <stdint.h> 

typedef union
{
  double value;
  struct
  {
    uint32_t msw;
    uint32_t lsw;
  } parts;
} ieee_double_shape_type;

#define SET_LOW_WORD(d,v)					\
  do {								\
    ieee_double_shape_type sl_u;				\
    sl_u.value = (d);						\
    if ( is_little_endian())					\
      sl_u.parts.msw = (v);					\
    else							\
      sl_u.parts.lsw = (v);					\
    (d) = sl_u.value;						\
  } while (0)

#endif /* WIN32 */


#endif /* NSP_MATH */



