#ifndef NSP_MATH 
#define NSP_MATH 

/*------------------------------------------------------------------------
 *    Copyright (C) 1998-2003-2003 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <math.h>
#include "machine.h"
#include "numeric.h" /* set of constants */


#ifdef __STDC__
#include <stdlib.h>
#else
#include <malloc.h>
#endif

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

/* 
 * isinf must return -1 0 or 1 
#ifndef isinf 
#define isinf(x) ( !  finite(x) && !isnan(x))
#endif 
*/

#ifndef exp10
extern double exp10 (double);
#endif 

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

#if defined(HAS_STDARG)
#include <stdarg.h>
#else
#include <varargs.h>
#endif 

/* A set of macro for functions with variable numbers of
 * arguments to be used with either varargs.h or stdarg.h.
 * should be moved elsewhere since It could be more widely used XXXXXX
 */

#if defined(HAS_STDARG)
#   define SCI_VARARGS_DEF(type, name) (type name, ...)
#   define SCI_VARARGS_2DEF(type1,name1,type2, name2) (type1 name1,type2 name2 , ...)
#   define SCI_VARARGS_START(type, name, list) va_start(list, name)
#   define SCI_VARARGS_2START(type1, name1,type2,name2, list) va_start(list, name2)
#else
#   define SCI_VARARGS_DEF(type, name) (va_alist) va_dcl
#   define SCI_VARARGS_2DEF(type1, name1,type2,name2) (va_alist) va_dcl
#   define SCI_VARARGS_START(type1, name1, list) \
	type1 name1 ;va_start(list), name1 = va_arg(list, type1);
#   define SCI_VARARGS_2START(type1, name1,type2,name2, list) \
	type1 name1 ;type2 name2 ; va_start(list), name1 = va_arg(list, type1);\
        name2 = va_arg(list, type2);
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

#ifdef WIN32 
extern double acosh(double);
extern double asinh(double);
extern double atanh(double);
#endif 

#endif /* SCI_MATH */



