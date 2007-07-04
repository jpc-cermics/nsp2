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

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <setjmp.h>
#include <errno.h>

#include "nsp/matrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/datas.h"
#include "nsp/parse.h"
#include "nsp/gtk/gobject.h" /* FIXME: nsp_gtk_eval_function */
#include "swiglib.h" 


int SWIG_AsVal_double (NspObject *obj, double *val)
{
  if ( IsMat(obj) && ((NspMatrix *) obj)->mn >= 1
       && ((NspMatrix *) obj)->rc_type == 'r')
    {
      if (val) *val = ((NspMatrix *) obj)->R[0];
    }
  else if ( IsBMat(obj)  && ((NspBMatrix *) obj)->mn >= 1)
    {
      if (val) *val = (double) ((NspBMatrix *) obj)->B[0];
    }
  else {
    Scierror("Expecting a double\n");
    return SWIG_TypeError;
  }
  return SWIG_OK;
}

int SWIG_CanCastAsInteger(double *d, double min, double max) 
{
  double x = *d;
  if ((min <= x && x <= max)) {
    double fx = floor(x);
    double cx = ceil(x);
    double rd =  ((x - fx) < 0.5) ? fx : cx; /* simple rint */
    if ((errno == EDOM ) || (errno == ERANGE)) {
      errno = 0;
    } else {
      double summ, reps, diff;
      if (rd < x) {
	diff = x - rd;
      } else if (rd > x) {
	diff = rd - x;
      } else {
	return 1;
      }
      summ = rd + x;
      reps = diff/summ;
      if (reps < 8*DBL_EPSILON) {
	*d = rd;
	return 1;
      }
    }
  }
  return 0;
}


int SWIG_AsVal_long (NspObject *obj, long* val)
{
  if ( IsMat(obj) && ((NspMatrix *) obj)->mn >= 1
       && ((NspMatrix *) obj)->rc_type == 'r')
    {
      double d = ((NspMatrix *) obj)->R[0];
      if ( SWIG_CanCastAsInteger(&d, LONG_MIN, LONG_MAX) )
	{
	  if (val) *val = (long) ((NspMatrix *) obj)->R[0];
	}
      else 
	{
	  Scierror("Given double cannot be casted to a long\n");
	  return SWIG_OverflowError;
	}
    }
  else if ( IsBMat(obj)  && ((NspBMatrix *) obj)->mn >= 1)
    {
      if (val)      *val = (long) ((NspBMatrix *) obj)->B[0];
    }
  else {
    Scierror("Expecting a long\n");
    return SWIG_TypeError;
  }
  return SWIG_OK;
}


int SWIG_AsVal_int (NspObject * obj, int *val)
{
  
  long v;
  int res = SWIG_AsVal_long (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v < INT_MIN || v > INT_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (int)(v);
    }
  }  
  return res;
}

int SWIG_AsVal_short (NspObject * obj, short *val)
{
  long v;
  int res = SWIG_AsVal_long (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v < SHRT_MIN || v > SHRT_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (short)(v);
    }
  }  
  return res;
}

int SWIG_AsVal_unsigned_SS_long (NspObject *obj, unsigned long *val) 
{
  if ( IsMat(obj) && ((NspMatrix *) obj)->mn >= 1
       && ((NspMatrix *) obj)->rc_type == 'r')
    {
      double d = ((NspMatrix *) obj)->R[0];
      if ( SWIG_CanCastAsInteger(&d, 0 , LONG_MAX) )
	{
	  if (val) *val = (long) ((NspMatrix *) obj)->R[0];
	}
      else 
	{
	  Scierror("Given double cannot be casted to an unsigned long\n");
	  return SWIG_OverflowError;
	}
    }
  else if ( IsBMat(obj)  && ((NspBMatrix *) obj)->mn >= 1)
    {
      if (val) *val = (unsigned long) ((NspBMatrix *) obj)->B[0];
    }
  else {
    Scierror("Expecting an unsigned long\n");
    return SWIG_TypeError;
  }
  return SWIG_OK;
}


int SWIG_AsVal_unsigned_SS_int (NspObject * obj, unsigned int *val)
{
  unsigned long v;
  int res = SWIG_AsVal_unsigned_SS_long (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v > UINT_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (unsigned int)(v);
    }
  }  
  return res;
}

int SWIG_AsVal_signed_SS_char (NspObject * obj, signed char *val)
{
  long v;
  int res = SWIG_AsVal_long (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v < SCHAR_MIN || v > SCHAR_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (signed char)(v);
    }
  }  
  return res;
}


int SWIG_AsVal_unsigned_SS_short (NspObject * obj, unsigned short *val)
{
  unsigned long v;
  int res = SWIG_AsVal_unsigned_SS_long (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v > USHRT_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (unsigned short)(v);
    }
  }  
  return res;
}

int SWIG_AsVal_unsigned_SS_char (NspObject * obj, unsigned char *val)
{
  unsigned long v;
  int res = SWIG_AsVal_unsigned_SS_long (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v > UCHAR_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (unsigned char)(v);
    }
  }  
  return res;
}


/* swig_type_info* SWIG_pchar_descriptor(void) */
/* { */
/*   static int init = 0; */
/*   static swig_type_info* info = 0; */
/*   if (!init) { */
/*     info = SWIG_TypeQuery("_p_char"); */
/*     init = 1; */
/*   } */
/*   return info; */
/* } */

int SWIG_AsCharPtrAndSize(NspObject *obj, char** cptr, size_t* psize, int *alloc)
{
  if (IsSMat(obj) && ((NspSMatrix *) obj)->mn == 1)
    {
      char *cstr; size_t len;
      cstr = ((NspSMatrix *) obj)->S[0];
      len = strlen(cstr);
      if (cptr)  
	{
	  if (alloc) 
	    {
	      if (*alloc == SWIG_NEWOBJ) 
		{
		  *cptr = (char *)memcpy((char *)malloc((len + 1)*sizeof(char)), cstr, sizeof(char)*(len + 1));
		  *alloc = SWIG_NEWOBJ;
		}
	      else {
		*cptr = cstr;
		*alloc = SWIG_OLDOBJ;
	      }
	    } 
	  else 
	    {
	      *cptr = cstr; 
	    }
	}
      if (psize) *psize = len + 1;
      return SWIG_OK;
    } 
  else 
    {
      /*        ????? */
      /*       swig_type_info* pchar_descriptor = SWIG_pchar_descriptor(); */
      /*       if (pchar_descriptor) { */
      /* 	void* vptr = 0; */
      /* 	if (SWIG_ConvertPtr(obj, &vptr, pchar_descriptor, 0) == SWIG_OK) { */
      /* 	  if (cptr) *cptr = (char *) vptr; */
      /* 	  if (psize) *psize = vptr ? (strlen((char *)vptr) + 1) : 0; */
      /* 	  if (alloc) *alloc = SWIG_OLDOBJ; */
      /* 	  return SWIG_OK; */
      /* 	} */
      /*       } */
    }
  return SWIG_TypeError;
}

int SWIG_AsCharArray(NspObject * obj, char *val, size_t size)
{ 
  char* cptr = 0; size_t csize = 0; int alloc = SWIG_OLDOBJ;
  int res = SWIG_AsCharPtrAndSize(obj, &cptr, &csize, &alloc);
  if (SWIG_IsOK(res)) {
    if ((csize == size + 1) && cptr && !(cptr[csize-1])) --csize;
    if (csize <= size) {
      if (val) {
	if (csize) memcpy(val, cptr, csize*sizeof(char));
	if (csize < size) memset(val + csize, 0, (size - csize)*sizeof(char));
      }
      if (alloc == SWIG_NEWOBJ) {
	free((char*)cptr);
	res = SWIG_OK;
      }      
      return res;
    }
    if (alloc == SWIG_NEWOBJ) free((char*)cptr);
  }
  return SWIG_TypeError;
}


int SWIG_AsVal_char (NspObject * obj, char *val)
{    
  int res = SWIG_AsCharArray(obj, val, 1);
  if (!SWIG_IsOK(res)) 
    {
      long v;
      res = SWIG_AsVal_long (obj, &v);
      if (SWIG_IsOK(res)) 
	{
	  if ((CHAR_MIN <= v) && (v <= CHAR_MAX)) {
	    if (val) *val = (char)(v);
	  } else {
	    res = SWIG_OverflowError;
	  }
	}
    }
  return res;
}


int SWIG_AsVal_float (NspObject * obj, float *val)
{
  double v;
  int res = SWIG_AsVal_double (obj, &v);
  if (SWIG_IsOK(res)) {
    if ((v < -FLT_MAX || v > FLT_MAX)) {
      return SWIG_OverflowError;
    } else {
      if (val) *val = (float)(v);
    }
  }  
  return res;
}


NspObject *SWIG_From_double  (float value)
{    
  return nsp_create_object_from_double(NVOID,value);
    }

NspObject *SWIG_From_float  (float value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_long (long value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_int  (int value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_short  (short value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_unsigned_SS_long  (unsigned long value)
{
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_unsigned_SS_int  (unsigned int value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_unsigned_SS_short  (unsigned short value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_signed_SS_char  (signed char value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_From_unsigned_SS_char  (unsigned char value)
{    
  return nsp_create_object_from_double(NVOID,value);
}

NspObject *SWIG_FromCharPtrAndSize(const char* carray, size_t size)
{
  if (carray) 
    {
      if (size > INT_MAX) 
	{
	  /* 	  swig_type_info* pchar_descriptor = SWIG_pchar_descriptor(); */
	  /* 	  return pchar_descriptor ?  */
	  /* 	    SWIG_NewPointerObj((char *)(carray), pchar_descriptor, 0) : NULLOBJ; */
	  return NULLOBJ;
	} 
      else 
	{
	  return nsp_create_object_from_str_and_size(NVOID,carray,size);
	}
    }
  else 
    {
      return NULLOBJ;
    }
}

NspObject *SWIG_FromCharPtr(const char *cptr)
{ 
  return SWIG_FromCharPtrAndSize(cptr, (cptr ? strlen(cptr) : 0));
}


NspObject *SWIG_From_char  (char c) 
{ 
  return SWIG_FromCharPtrAndSize(&c,1);
}


/* -----------------------------------------------------------------------------
 * error manipulation
 * ----------------------------------------------------------------------------- */

const char * SWIG_Nsp_ErrorType(int code) 
{
  switch(code) {
  case SWIG_MemoryError:
    return "MemoryError:";
    break;
  case SWIG_IOError:
    return "IOError:";
    break;
  case SWIG_RuntimeError:
    return "RuntimeError:";
    break;
  case SWIG_IndexError:
    return "IndexError:";
    break;
  case SWIG_TypeError:
    return "TypeError:";
    break;
  case SWIG_DivisionByZero:
    return "ZeroDivisionError:";
    break;
  case SWIG_OverflowError:
    return "OverflowError:";
    break;
  case SWIG_SyntaxError:
    return "SyntaxError:";
    break;
  case SWIG_ValueError:
    return "ValueError:";
    break;
  case SWIG_SystemError:
    return "SystemError:";
    break;
  case SWIG_AttributeError:
    return "AttributeError:";
    break;
  default:
    return "RuntimeError:";
  }
  return "Error:";
}

void SWIG_Nsp_AddErrorMsg(const char* mesg)
{
  Scierror(mesg);
}

void SWIG_Nsp_SetErrorMsg(const char *errtype, const char *msg) 
{
  Scierror("%s %s\n",errtype,msg);
}
