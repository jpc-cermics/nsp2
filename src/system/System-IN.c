/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cergrene       
 *********************************************************************/

#include <math.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#ifdef __STDC__
#include <stdlib.h>
#endif 

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/interf.h"

extern int C2F(timer) (double *d);
extern char *TclGetEnv (char *name);

extern function int_syscd;
extern function int_sysfile;
extern function int_pwd;
extern function int_regexp;
extern function int_regsub;
extern function int_glob;

/***************************************************
 * A set of system functions 
 * the tcl subdirectory was made from tcl code 
 * to get a portable set of system functions 
 * This code is unfinished XXXXX
 ***************************************************/

/*************************************************
 * Interface for timer 
 ************************************************/

int int_timer(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  double d;
  CheckRhs(0,0);
  CheckLhs(0,1);
  C2F(timer)(&d);
  if ( (OM= ObjDouble(NVOID,d)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,OM);
  return 1;
}

/*************************************************
 * Interface for system(str)
 ************************************************/

int int_system(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  double rep;
  char *command;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((command = GetString(stack,1)) == (char*)0) return RET_BUG;
  rep = (double) system(command);
  if ( (OM= ObjDouble(NVOID,rep)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1, OM);
  return 1;
}

/*************************************************
 * Interface for getenv(A [,def ] ) 
 ************************************************/

int int_getenv(Stack stack,int rhs,int opt,int lhs) 
{
  NspSMatrix *S;
  char *envname,*env,*def;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ( rhs == 2 ) 
    if ((def = GetString(stack,2)) == (char*)0) return RET_BUG;
  env = TclGetEnv(envname);
  if ( env != NULL ) 
    {
      if ((S=nsp_smatrix_create(NVOID,1,1,env,(integer)1)) == NULLSMAT ) 
	return RET_BUG;
      MoveObj(stack,1,(NspObject*) S);
      return 1;
    }
  else 
    {
      if ( rhs == 2) 
	{
	  NthObj(2)->ret_pos = 1;
	  return 1;
	}
      else
	{
	  Scierror("Error: environment variable %s is not defined\r\n",envname);
	  return RET_BUG;
	}
    }
}

/*************************************************
 * Interface for setenv(A,value)
 ************************************************/

int int_setenv(Stack stack,int rhs,int opt,int lhs) 
{
  char *envname,*val;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((val = GetString(stack,2)) == (char*)0) return RET_BUG;
  TclSetEnv(envname, val);
  TclPlatformInit() ; /* XXXXXXXX : temporaire pour tester */
  /*** XXXXXX setenv does not exists on all Ops 
  if ( setenv(envname,val,1) == -1 ) 
    {
      Scierror("Error: setenv failed, there was insufficient space in the environment\n");
      return RET_BUG;
    }
  ***/
  return 0;
}


/*************************************************
 * Interface for unsetenv(A)
 ************************************************/

int int_unsetenv(Stack stack,int rhs,int opt,int lhs) 
{
  char *envname;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  TclUnsetEnv(envname);
  /*** XXXXXX setenv does not exists on all Ops 
  unsetenv(envname);
  ***/
  return 0;
}


/*************************************************************
 * The Interface for basic matrices operation 
 *************************************************************/

static OpTab System_func[]={
#include "System-IN.nam" 
  {(char *) 0, NULL}
};

int System_Interf(int i,Stack stack,int rhs,int opt,int lhs) 
{
  return (*(System_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void System_Interf_Info(int i, char **fname, function **f)
{
  *fname = System_func[i].name;
  *f = System_func[i].fonc;
}





