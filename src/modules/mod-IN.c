/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include <math.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/mod-in.h" 
#include "../zcalelm/convert.h" 

/*********************************************************************
 * Interface for ModTables 
 * used to test Modules 
 *********************************************************************/

/*********************************************************************
 * Create a NspMod Table 
 *********************************************************************/

int int_mocreate(Stack stack, int rhs, int opt, int lhs)
{
  char *path,*m_name;
  NspObject *O;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((path=GetString(stack,1))== (char *) 0 ) return RET_BUG;           
  if ((m_name=GetString(stack,2))== (char *) 0 ) return RET_BUG;           
  if(( O = (NspObject *) ModCreate(NVOID,path,m_name)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
} 

/*************************************************
 * Copy of a ModTable. The copy has  name NVOID
 ***************************************************/

int int_mocopy(Stack stack, int rhs, int opt, int lhs)
{
  NspMod *H;
  NspObject *O;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( H  = GetMod(stack,1)) == NULLMOD) return RET_BUG;
  if (( O  = (NspObject *) ModCopy(H))   == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
 }

/**************************************
 * Delete ModTable and stored objects 
 **************************************/

int int_modestroy(Stack stack, int rhs, int opt, int lhs)
{
  NspMod *H;
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ((H = GetMod(stack,1)) == NULLMOD) return RET_BUG;
  ObjDestroy(&NthObj(1));
  return 0;
}

/*********************************************
 * ModInfo 
 *********************************************/

int int_moinfo(Stack stack, int rhs, int opt, int lhs)
{
  NspMod *H;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((H = GetMod(stack,1)) == NULLMOD) return RET_BUG;
  ModInfo(H,0);
  return 0;
}

/**************************************************
 * ModPrint 
 **************************************************/

int int_moprint(Stack stack, int rhs, int opt, int lhs)
{
  NspMod *H;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((H = GetMod(stack,1)) == NULLMOD) return RET_BUG;
  ModPrint(H,0);
  return 0;
}

/******************************************************
 *  Interface 
 ******************************************************/

static OpTab Mod_func[]={
#include "mod-IN.nam"
  {(char *) 0, NULL}
};

/** call ith function in the NspMod interface **/

int Mod_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Mod_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Mod_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Mod_func[i].name;
  *f = Mod_func[i].fonc;
}

