#ifndef NUMERIC_VALUES_DEFS
#define NUMERIC_VALUES_DEFS

/* 
 * machine constants:
 * code extracted from Sparse library 
 * FIXME: this should be changed using glib 
 * portable code defines the same constants 
 */

/*
 *  DATA STRUCTURE AND MACRO DEFINITIONS for Sparse.
 *
 *  Author:                     Advising professor:
 *      Kenneth S. Kundert          Alberto Sangiovanni-Vincentelli
 *      UC Berkeley
 *
 *  This file contains common type definitions and macros for the sparse
 *  matrix routines.  These definitions are of no interest to the user.
 */

/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985,86,87,88
 *  by Kenneth S. Kundert and the University of California.
 *
 *  Permission to use, copy, modify, and distribute this software and
 *  its documentation for any purpose and without fee is hereby granted,
 *  provided that the copyright notices appear in all copies and
 *  supporting documentation and that the authors and the University of
 *  California are properly credited.  The authors and the University of
 *  California make no representations as to the suitability of this
 *  software for any purpose.  It is provided `as is', without express
 *  or implied warranty.
 *
 *  $Date$
 *  $Revision$
 */

/*
 *  MACHINE CONSTANTS
 *  These numbers must be updated when the program is ported to a new machine.
 */

/* Begin machine constants. */

#ifdef notdef /* __STDC__ */
/*
 * This code is currently deleted because most ANSI standard C compilers
 * do not provide the standard header files yet.
 */
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX
#else /* NOT defined(__STDC__) */

/* Apple MacOSX */ 

#ifdef __APPLE__  /* __STDC__ */
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX
#endif /* NOT defined(__STDC__) */

/* VAX machine constants */
#if  (defined(vax) && !defined(netbsd)) 
#   define  MACHINE_RESOLUTION      6.93889e-18
#   define  LARGEST_REAL            1.70141e+38
#   define  SMALLEST_REAL           2.938743e-39
#   define  LARGEST_SHORT_INTEGER   32766
#   define  LARGEST_LONG_INTEGER    2147483646
#endif

/* MIPS machine constants */
#if (defined(mips) && !defined(netbsd)) 
#   define  MACHINE_RESOLUTION      6.93889e-18
#   define  LARGEST_REAL            1.70141e+38
#   define  SMALLEST_REAL           2.938743e-39
#   define  LARGEST_SHORT_INTEGER   32766
#   define  LARGEST_LONG_INTEGER    2147483646
#endif

/* hp9000 machine constants */
#ifdef hpux
/* These values are correct for hp9000/300.  Should be correct for others. */
#   define  MACHINE_RESOLUTION      8.9e-15
#   define  LARGEST_REAL            1.79769313486231e+308
#   define  SMALLEST_REAL           2.22507385850721e-308
#   define  LARGEST_SHORT_INTEGER   32766
#   define  LARGEST_LONG_INTEGER    2147483646
#endif

/* IBM machine constants */
#ifdef aix
/** The STDC option works on aix on gives the values that i've copied here */
#   define  MACHINE_RESOLUTION      2.2204460492503131e-16   
#   define  LARGEST_REAL            1.7976931348623158e+308  
#   define  SMALLEST_REAL           2.2250738585072014e-308
#   define  LARGEST_SHORT_INTEGER   32767
#   define  LARGEST_LONG_INTEGER    2147483647
#endif

/* Sun machine constants */
#if (defined(sun) && !defined(netbsd))
/* These values are rumored to be the correct values. */
#   define  MACHINE_RESOLUTION      8.9e-15
#   define  LARGEST_REAL            1.79769313486231e+308
#   define  SMALLEST_REAL           2.22507385850721e-308
#   define  LARGEST_SHORT_INTEGER   32766
#   define  LARGEST_LONG_INTEGER    2147483646
#endif
/* DEC alpha machine constant*/
#if (defined(__alpha) && !defined(netbsd)) 
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX
#endif
#ifdef linux
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX
#endif
#if defined(netbsd) || defined(freebsd)
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX
#endif
#ifdef WIN32 
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
/* XXXXX : a v'erifier */ 
#   define  LARGEST_SHORT_INTEGER   32766
#   define  LARGEST_LONG_INTEGER    2147483646
#endif
#if defined(__MWERKS__)||defined(THINK_C)
#   include <limits.h>
#   include <float.h>
#   define  MACHINE_RESOLUTION      DBL_EPSILON
#   define  LARGEST_REAL            DBL_MAX
#   define  SMALLEST_REAL           DBL_MIN
#   define  LARGEST_SHORT_INTEGER   SHRT_MAX
#   define  LARGEST_LONG_INTEGER    LONG_MAX
#endif

#endif /* NOT defined(__STDC__) */

#endif 




