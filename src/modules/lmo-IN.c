/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/mod-in.h" 
#include "nsp/matutil.h" 

/*********************************************************************
 * Interface for Lmo (List of mmodules)
 *********************************************************************/

/*********************************************************************
 * Create a NspMod Table 
 *********************************************************************/

int int_lmocreate(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  CheckRhs(0,0);
  CheckLhs(1,1);
  if(( O = (NspObject*) ELmoCreate(NVOID)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
} 

/*********************************************
 * ModInfo 
 *********************************************/

int int_lmoinfo(Stack stack, int rhs, int opt, int lhs)
{
  NspLmo *H;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((H = GetLmo(stack,1)) == NULLLMO) return RET_BUG;
  LmoInfo(H,0);
  return 0;
}

/**************************************************
 * LmoPrint 
 **************************************************/

int int_lmoprint(Stack stack, int rhs, int opt, int lhs)
{
  NspLmo *H;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((H = GetLmo(stack,1)) == NULLLMO) return RET_BUG;
  LmoPrint(H,0);
  return 0;
}


/******************************************************
 *  Interface 
 ******************************************************/

static OpTab Lmo_func[]={
  {"lmocreate",int_lmocreate},  		
  {"insert",int_lmoinsertlast},  		
  {"search",int_lmosearch},  		
  {"pathsearch",int_lmopathsearch},  		
  {"import",int_lmoimport},  		
  {(char *) 0, NULL}
};

/** call ith function in the Lmo interface **/

int Lmo_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Lmo_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Lmo_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Lmo_func[i].name;
  *f = Lmo_func[i].fonc;
}

