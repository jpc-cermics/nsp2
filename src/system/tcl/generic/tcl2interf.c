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
  NspObject *Ob;
  if (( Ob = Tcl_NewStringObj(bytes, length)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,n,Ob);
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
  NspSMatrix *S;
  if ( length < 0  ) 
    {
      if (( S =nsp_smatrix_create(NVOID,1,1,bytes,1) ) == NULLSMAT ) 
	return NULLOBJ;
    }
  else
    {
      if (( S =nsp_smatrix_create_with_length(NVOID,1,1,length) ) == NULLSMAT ) 
	return NULLOBJ;
      strncpy(S->S[0],bytes,length);
      S->S[0][length] ='\0';
    }
  return (NspObject*) S;
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
  if (( M = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) return RET_BUG;
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
  NspObject *Ob;
  if ((Ob =nsp_create_boolean_object(NVOID,ival))==NULLOBJ) return RET_BUG;
  MoveObj(stack,n,Ob);
  return 1;
}







