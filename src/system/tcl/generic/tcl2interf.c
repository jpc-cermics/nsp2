/* 
 *----------------------------------------------------------------------
 * Interface Scilab : 
 * XXXXX : voir si les fonctions qui sont la sont a rammener ds Interf.c 
 * i.e si on peut les utiliser ailleurs 
 *----------------------------------------------------------------------
 */

#include "tclInt.h"
#include "tclPort.h"

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/interf.h"

extern NspObject * Tcl_NewStringObj _ANSI_ARGS_((char *bytes, int length));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SetStringObj --
 *      Replace object at position n on the stack by a new string matrix object 
 *      filled with bytes 
 * Results:
 *	return 1 or RET_BUG ;
 *
 * Side effects:
 *	The object's string representation will be set to a copy of
 *	the "length" bytes starting at "bytes". If "length" is negative, use
 *	bytes up to the first NULL byte; i.e., assume "bytes" points to a
 *	C-style NULL-terminated string. The object's old string and internal
 *	representations are freed and the object's type is set NULL.
 *
 * bytes: Points to the first of the length bytes used to initialize the object.
 * length : The number of bytes to copy from "bytes" when initializing the object. If  
 *          negative, use bytes up to the first  NULL byte.
 *
 *----------------------------------------------------------------------
 */

int Tcl_SetStringObj(Stack stack,int n,char *bytes,int length)
{
  NspObject *O;
  if (( O = Tcl_NewStringObj(bytes, length)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,n,O);
  return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_NewStringObj
 *      Creates a Scilab Objects filled with a 1x1 String matrix [ bytes] 
 * Results:
 *	return 1 or RET_BUG ;
 *
 * Side effects:
 *	The object's string representation will be set to a copy of
 *	the "length" bytes starting at "bytes". If "length" is negative, use
 *	bytes up to the first NULL byte; i.e., assume "bytes" points to a
 *	C-style NULL-terminated string. The object's old string and internal
 *	representations are freed and the object's type is set NULL.
 *
 * bytes: Points to the first of the length bytes used to initialize the object.
 * length : The number of bytes to copy from "bytes" when initializing the object. If  
 *          negative, use bytes up to the first  NULL byte.
 *
 *----------------------------------------------------------------------
 */

NspObject  *Tcl_NewStringObj(char *bytes,int length)
{
  NspObject *O;
  NspSMatrix *S;
  if ( length < 0  ) 
    {
      if (( S =nsp_smatrix_create("void",1,1,bytes,1) ) == NULLSMAT ) 
	return NULLOBJ;
    }
  else
    {
      String *loc;
      if (( S =nsp_smatrix_create("void",1,1,bytes,0) ) == NULLSMAT ) 
	return NULLOBJ;
      if (( loc = NewStringN(length)) == (String *) 0) 
	return NULLOBJ;
      strncpy(loc,bytes,length);
      loc[length] ='\0';
      StringDestroy(&S->S[0]);
      S->S[0] = loc ;
    }
  return (NspObject*) O;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SetDoubleObj --
 *      Replace object at position n on the stack by a new matrix object 
 *      filled with a double. 
 * Results:
 *	return 1 or RET_BUG ;
 *
 *----------------------------------------------------------------------
 */

int Tcl_SetDoubleObj(Stack stack,int n,double d)
{
  NspMatrix *M;
  if (( M = nsp_matrix_create("void",'r',1,1) ) == NULLMAT ) return RET_BUG;
  M->R[0] = d;
  MoveObj(stack,n,(NspObject*) M);
  return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SetBooleanObj --
 *      Replace object at position n on the stack by a new boolean matrix object 
 *      filled with a boolean value
 * Results:
 *	return 1 or RET_BUG ;
 *
 *----------------------------------------------------------------------
 */

int Tcl_SetBooleanObj(Stack stack,int n,int ival)
{
  NspObject *O;
  if ((O =nsp_create_true_object("void"))==NULLOBJ) return RET_BUG;
  if ( ival == 0 )   ((NspBMatrix *) O)->B[0] = ival ;
  MoveObj(stack,n,O);
  return 1;
}







