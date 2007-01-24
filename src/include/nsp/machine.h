/* src/include/nsp/machine.h.  Generated by configure.  */
/* -*- Mode: C -*- */

#ifndef MACHINE_H
#define MACHINE_H

/*
 * This Software is (Copyright ENPC 1998-2005)
 * Jean-Philippe Chancelier Enpc/Cermics
 */

/* Define if you have inline */
#define HAVE_INLINE 1

/* headers */
#define HAVE_VALUES_H 1
#define HAVE_FLOAT_H 1
#define HAVE_LIMITS_H 1
#define HAVE_SYS_TIME_H 1

/* Define for using dld for sunos */
/* #undef SUNOSDLD */

/* Define if leading underscores */
/* #undef WLU */

/* Define if trailing underscores */
#define WTU 1

/* Define if use sharpsigns */
#define USE_SHARP_SIGN 1

/* Define if have exp10 */
#define HAVE_EXP10 1

/* Define if have tgamma (true gamma) */
#define HAVE_TGAMMA 1

/* Define if have lgamma (log gamma) */
#define HAVE_LGAMMA 1

/* Define if have getwd */
#define HAVE_GETWD 1

/* Define if have getwd */
#define HAVE_GETCWD 1

/* Define if have mkstemp */
#define HAVE_MKSTEMP 1

/* Define if have sleep: used in graphics/periX11.c */
#define HAVE_SLEEP 1

/* Define if have strerror: used in sound/misc.c */
#define HAVE_STRERROR 1

/* Define if have termcap library: used in sun/zzledt.c */
#define HAVE_TERMCAP 1

/* Define if have usleep: used in graphics/periX11.c */
#define HAVE_USLEEP 1

/* Define if have isinf */
#define HAVE_ISINF 1

/* Define if have isnan */
#define HAVE_ISNAN 1

/* Define if have finite*/
#define HAVE_FINITE 1

/* Define if have and use TCL/TK */
/* #undef WITH_TK */

/* Define if have and use PVM */
#define WITH_PVM 1

/* Define if have and use GTKGLEXT */
#define WITH_GTKGLEXT 1

/* Define if have and use GTKHTML */
#define WITH_GTKHTML 1

/* Define if use maxplus lib  */
/* #undef WITH_MAXPLUSLIB */

/* Define if use maxplus lib  */
#define WITH_UMFPACK 1

/* Define if use fftw3 lib  */
#define WITH_FFTW3 1

/* Define if use sndfile lib  */
#define WITH_SNDFILE 1

/* Define if use asound lib  */
#define WITH_ASOUND 1

/* Define if use gmp lib  */
#define WITH_GMP 1

/* Define if sizeof(int*)==sizeof(int) */
#define POINTER_INT 1

/* storing and retrieving an int in a pointer */

#if defined(POINTER_INT)
#define NSP_INT_TO_POINTER(i) ((int *) (i))
#define NSP_POINTER_TO_INT(i) ((int) (i))
#else
#define NSP_INT_TO_POINTER(i) ((int *) (long) (i))
#define NSP_POINTER_TO_INT(i) ((int) (long) (i))
#endif

/* CNAME(x,y) ==> xy **/

#if defined(USE_SHARP_SIGN)
#define CNAME(name1,name2) name1##name2
#else
#define CNAME(name1,name2) name1/**/name2
#endif

/* Define  C2F and F2C entry point conversion */
#if defined(WTU)
#if defined(USE_SHARP_SIGN)
#define C2F(name) name##_
#define F2C(name) name##_
#else
#define C2F(name) name/**/_
#define F2C(name) name/**/_
#endif
#else
#define C2F(name) name
#define F2C(name) name
#endif

/* Define some functions */

#if !defined(HAVE_EXP10)
#define exp10(x) pow((double) 10.0,x)
#else
/* missing in some math.h : */
extern double exp10(double);
#endif

#if !defined(HAVE_GETWD)
#define getwd(x) getcwd(x,1024) /* you must define char x[1024] */
#endif

/*
 * obsolete
 */

typedef int integer;

/* Intel Blas library on win32
 */

#ifdef MKL
#include "MKL.h"
#endif


#if defined(HAVE_FLOAT_H) && defined(HAVE_LIMITS_H)
#   include <limits.h>
#   include <float.h>
#else
#   define  DBL_EPSILON             2.2204460492503131e-16
#   define  DBL_MAX                 1.7976931348623158e+308
#   define  DBL_MIN                 2.2250738585072014e-308
#   define  SHRT_MAX                32767
#   define  LONG_MAX                2147483647
#endif

#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX


#define WITH_SYMB_TABLE 1
/* matlab mode */
#define MTLB_MODE 1


#endif /* MACHINE_H  */
