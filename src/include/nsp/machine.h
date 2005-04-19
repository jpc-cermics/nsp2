/* src/include/nsp/machine.h.  Generated by configure.  */
/* -*- Mode: C -*- */
/* Copyright INRIA */
#ifndef MACHINE_H
#define MACHINE_H

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
/* #undef WITH_UMFPACK */

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
#endif

#if !defined(HAVE_GETWD)
#define getwd(x) getcwd(x,1024) /* you must define char x[1024] */
#endif

/*
   Define integer C type which must fit Fortran integer
   For Scilab to work, the rule is:
          size of Fortran double precision = 2 * size of Fortran integer

   At the present time, we suppose:
       size of Fortran integer = 4 bytes
       size of Fortran double precision = 8 bytes
       size of C int = 4 bytes
*/

typedef int integer;

/** Intel Blas library on win32 */

#ifdef MKL
#include "MKL.h"
#endif

#endif /* MACHINE_H  */
