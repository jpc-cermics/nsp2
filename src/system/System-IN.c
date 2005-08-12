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
 *
 * interface for system  functions.
 * the tcl subdirectory was made from tcl code 
 * to get a portable set of system functions 
 * This code is unfinished. 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/interf.h"

extern double nsp_timer(void);
extern int nsp_realtime(double *t);
extern int nsp_realtime_init( double *t,  double *scale);

/* defined in the tcl subdir 
 */

extern char *TclGetEnv (char *name);
extern void TclSetEnv (const char *name,const char *value);
extern void TclUnsetEnv (const char *name);
extern void TclPlatformInit();

extern function int_syscd;
extern function int_sysfile;
extern function int_pwd;
extern function int_regexp;
extern function int_regsub;
extern function int_glob;

/*
 * Interface for timer 
 */

static int int_timer(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( (OM=nsp_create_object_from_double(NVOID,nsp_timer())) == NULLOBJ) 
    return RET_BUG;
  MoveObj(stack,1,OM);
  return 1;
}

/*
 * Interface for system(str)
 */

static int int_system(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  double rep;
  char *command;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((command = GetString(stack,1)) == (char*)0) return RET_BUG;
  rep = (double) system(command);
  if ( (OM=nsp_create_object_from_double(NVOID,rep)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1, OM);
  return 1;
}

/*
 * Interface for getenv(A [,def ] ) 
 */

static int int_getenv(Stack stack,int rhs,int opt,int lhs) 
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

/*
 * Interface for setenv(A,value)
 */

static int int_setenv(Stack stack,int rhs,int opt,int lhs) 
{
  char *envname,*val;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((val = GetString(stack,2)) == (char*)0) return RET_BUG;
  TclSetEnv(envname, val);
  TclPlatformInit() ; /* XXXXXXXX : temporaire pour tester */
  /* XXXXXX setenv does not exists on all Ops 
  if ( setenv(envname,val,1) == -1 ) 
    {
      Scierror("Error: setenv failed, there was insufficient space in the environment\n");
      return RET_BUG;
    }
  */
  return 0;
}

/*
 * Interface for unsetenv(A)
 */

static int int_unsetenv(Stack stack,int rhs,int opt,int lhs) 
{
  char *envname;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  TclUnsetEnv(envname);
  /* XXXXXX setenv does not exists on all Ops 
  unsetenv(envname);
  */
  return 0;
}

/*
 *
 */

int int_realtime_init(Stack stack, int rhs, int opt, int lhs) 
{
 double zer=0.0,rtv;
 CheckRhs(1,1);
 CheckLhs(1,1);
 /*  checking variable scale */
 if (GetScalarDouble(stack,1,&rtv) == FAIL) return RET_BUG;
 /* cross variable size checking */
 nsp_realtime_init(&zer,&rtv);
 return 0;
}

/*
 *
 */
 
int int_realtime(Stack stack, int rhs, int opt, int lhs) 
{
  double rtv;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /*  checking variable t */
  if (GetScalarDouble(stack,1,&rtv) == FAIL) return RET_BUG;
  /* cross variable size checking */
  nsp_realtime(&rtv);
  return 0;
}               


/*
 * The Interface for system functions 
 */ 

static OpTab System_func[]={
  {"chdir", int_syscd},
  {"getcwd", int_pwd},
  {"file", int_sysfile},
  {"regexp", int_regexp},
  {"regsub", int_regsub},
  {"glob",int_glob},
  {"getenv",int_getenv},
  {"setenv",int_setenv},
  {"unsetenv",int_unsetenv},
  {"timer", int_timer},
  {"system",int_system},
  {"realtime",int_realtime},
  {"realtimeinit",int_realtime_init},
  {(char *) 0, NULL}
};

int System_Interf(int i,Stack stack,int rhs,int opt,int lhs) 
{
  return (*(System_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void System_Interf_Info(int i, char **fname, function **f)
{
  *fname = System_func[i].name;
  *f = System_func[i].fonc;
}





