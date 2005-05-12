#ifndef NSP_MATH 
#define NSP_MATH 

/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 *--------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include "machine.h"

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

#ifdef WIN32 
#if !(defined __CYGWIN32__) && !(defined __ABSC__)
#include <float.h>
#define finite(x) _finite(x) 
#endif 
#else  /* WIN32 */ 
/* This should only be provided when finite prototype is missing **/
/* XXX : to be tested **/
#ifndef __cplusplus
int finite (double);
#endif
#endif /* WIN32 */

#ifdef WIN32 
#if !(defined __CYGWIN32__) && !(defined __ABSC__)
#include <float.h>
#define ISNAN(x) _isnan(x)
#else 
#define ISNAN(x) isnan(x)
#endif /* __CYGWIN32__ */
#else 
#define ISNAN(x) isnan(x)
#endif 

#define Abs(x) ( ( (x) >= 0) ? (x) : -( x) )
#ifndef Min
#define Min(x,y)	(((x)<(y))?(x):(y))
#endif 

#ifndef Max 
#define Max(x,y)	(((x)>(y))?(x):(y))
#endif 

extern double Mini();  /* XXXX a mettre ailleurs **/
extern double Maxi();  /* XXXX a mettre ailleurs **/

#define PI0 (integer *) 0
#define PD0 (double *) 0
#define SMDOUBLE 1.e-200 /* Smalest number to avoid dividing by zero */

#define linint(x) ((integer) floor(x + 0.5 )) 
#define inint(x) ((int) floor(x + 0.5 ))  

/* a revoir precisement un jour ou l''autre XXXXXX **/
/* nearest : **/
#define anint(x) rint(x) 
/* partie entiere **/
#define aint(x) ((x>= 0 ) ? floor(x)  : ceil(x))

#ifndef exp10
extern double exp10 (double);
#endif 

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

#endif /* SCI_MATH */



