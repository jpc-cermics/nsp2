#ifndef SWIGLIB_INC 
#define SWIGLIB_INC 
/*------------------------------------------------------------------------
 * Copyright (C) 2007-2007 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library emulates Matlab' API functions.
 * It is a fully rewriten version of Scilab mexlib.c file 
 * since Scilab and nsp object are totally different 
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
 * library for SWIG 
 *--------------------------------------------------------------------------*/

#define SWIG_OK                    (0) 
#define SWIG_ERROR                 (-1)
#define SWIG_IsOK(r)               (r >= 0)
#define SWIG_ArgError(r)           ((r != SWIG_ERROR) ? r : SWIG_TypeError)  

#define SWIG_as_voidptr(a) (void *)((const void *)(a)) 
#define SWIG_as_voidptrptr(a) ((void)SWIG_as_voidptr(*a),(void**)(a)) 

#include <limits.h>
#ifndef LLONG_MIN
# define LLONG_MIN	LONG_LONG_MIN
#endif
#ifndef LLONG_MAX
# define LLONG_MAX	LONG_LONG_MAX
#endif
#ifndef ULLONG_MAX
# define ULLONG_MAX	ULONG_LONG_MAX
#endif

/*  Errors in SWIG */
#define  SWIG_UnknownError    	   -1 
#define  SWIG_IOError        	   -2 
#define  SWIG_RuntimeError   	   -3 
#define  SWIG_IndexError     	   -4 
#define  SWIG_TypeError      	   -5 
#define  SWIG_DivisionByZero 	   -6 
#define  SWIG_OverflowError  	   -7 
#define  SWIG_SyntaxError    	   -8 
#define  SWIG_ValueError     	   -9 
#define  SWIG_SystemError    	   -10
#define  SWIG_AttributeError 	   -11
#define  SWIG_MemoryError    	   -12 
#define  SWIG_NullReferenceError   -13

/* The CastRankLimit says how many bits are used for the cast rank */
#define SWIG_CASTRANKLIMIT         (1 << 8)
/* The NewMask denotes the object was created (using new/malloc) */
#define SWIG_NEWOBJMASK            (SWIG_CASTRANKLIMIT  << 1)
/* The TmpMask is for in/out typemaps that use temporal objects */
#define SWIG_TMPOBJMASK            (SWIG_NEWOBJMASK << 1)
/* Simple returning values */
#define SWIG_BADOBJ                (SWIG_ERROR)
#define SWIG_OLDOBJ                (SWIG_OK)
#define SWIG_NEWOBJ                (SWIG_OK | SWIG_NEWOBJMASK)
#define SWIG_TMPOBJ                (SWIG_OK | SWIG_TMPOBJMASK)

/* errors 
 *
 */

#define SWIG_SetErrorObj           SWIG_Nsp_SetErrorObj                            
#define SWIG_SetErrorMsg           SWIG_Nso_SetErrorMsg				   
#define SWIG_ErrorType(code)       SWIG_Nsp_ErrorType(code)                        
#define SWIG_Error(code, msg)      SWIG_Nsp_SetErrorMsg(SWIG_ErrorType(code), msg) 
#define SWIG_fail                  goto fail					   
#define SWIG_exception_fail(code, msg) do { SWIG_Error(code, msg); SWIG_fail; } while(0) 

const char * SWIG_Nsp_ErrorType(int code) ;
void SWIG_Nsp_AddErrorMsg(const char* mesg);
void SWIG_Nsp_SetErrorMsg(const char *errtype, const char *msg) ;


/* convertion library 
 */

extern int SWIG_AsVal_double (NspObject *obj, double *val);
extern int SWIG_CanCastAsInteger(double *d, double min, double max) ;
extern int SWIG_AsVal_long (NspObject *obj, long* val);
extern int SWIG_AsVal_int (NspObject * obj, int *val);
extern int SWIG_AsVal_short (NspObject * obj, short *val);
extern int SWIG_AsVal_unsigned_SS_long (NspObject *obj, unsigned long *val) ;
extern int SWIG_AsVal_unsigned_SS_int (NspObject * obj, unsigned int *val);
extern int SWIG_AsVal_signed_SS_char (NspObject * obj, signed char *val);
extern int SWIG_AsVal_unsigned_SS_short (NspObject * obj, unsigned short *val);
extern int SWIG_AsVal_unsigned_SS_char (NspObject * obj, unsigned char *val);
extern int SWIG_AsCharPtrAndSize(NspObject *obj, char** cptr, size_t* psize, int *alloc);
extern int SWIG_AsCharArray(NspObject * obj, char *val, size_t size);
extern int SWIG_AsVal_char (NspObject * obj, char *val);
extern int SWIG_AsVal_float (NspObject * obj, float *val);
extern NspObject *SWIG_From_double  (float value);
extern NspObject *SWIG_From_float  (float value);
extern NspObject *SWIG_From_long (long value);
extern NspObject *SWIG_From_int  (int value);
extern NspObject *SWIG_From_short  (short value);
extern NspObject *SWIG_From_unsigned_SS_long  (unsigned long value);
extern NspObject *SWIG_From_unsigned_SS_int  (unsigned int value);
extern NspObject *SWIG_From_unsigned_SS_short  (unsigned short value);
extern NspObject *SWIG_From_signed_SS_char  (signed char value);
extern NspObject *SWIG_From_unsigned_SS_char  (unsigned char value);
extern NspObject *SWIG_FromCharPtrAndSize(const char* carray, size_t size);
extern NspObject *SWIG_FromCharPtr(const char *cptr);
extern NspObject *SWIG_From_char  (char c) ;



#endif

