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
#include "../zcalelm/convert.h" 

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


/**************************************************
 * insert 
 **************************************************/

int int_lmoinsertlast(Stack stack, int rhs, int opt, int lhs)
{

  NspLmo *H;
  NspSMatrix *Mname; 
  char *dir;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H = GetLmo(stack,1)) == NULLLMO) return RET_BUG;
  if ((dir=GetString(stack,2))== (char *) 0 ) return RET_BUG;     
  if ((Mname = GetSMat(stack,3))  == NULLSMAT) return RET_BUG;
  if ( LmoInsertLast(H,dir,Mname->S) == FAIL) 
    {
      Scierror("Error:Lmo insertion failed\n");
      return RET_BUG;
    }
  return 0;
}

/**************************************************
 * search 
 **************************************************/

int int_lmosearch(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  NspLmo *H;
  NspSMatrix *Mname; 
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((H = GetLmo(stack,1)) == NULLLMO) return RET_BUG;
  if ((Mname = GetSMat(stack,2))  == NULLSMAT) return RET_BUG;
  if ((O= LmoSearchName(H,Mname->S)) == NULLOBJ) 
    {
      Scierror("Error:Lmo search failed\n");
      return RET_BUG;
    }
  MoveObj(stack,1,O);
  return 1;
}

/**************************************************
 * import 
 **************************************************/

int int_lmoimport(Stack stack, int rhs, int opt, int lhs)
{
  NspLmo *H;
  NspSMatrix *Mname; 
  char *dir;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H = GetLmo(stack,1)) == NULLLMO) return RET_BUG;
  if ((dir=GetString(stack,2))== (char *) 0 ) return RET_BUG;     
  if ((Mname = GetSMat(stack,3))  == NULLSMAT) return RET_BUG;
  if ( LmoImport(H,dir,Mname->S) == FAIL ) 
    {
      Scierror("Error:Lmo import failed\n");
      return RET_BUG;
    }
  return 0;
}



/**************************************************
 * search for a full name using a path variable 
 * and dynamically add modules in the lmo variable 
 * during the search 
 * ==> If object is found it is loaded in the 
 *     current env 
 **************************************************/

int int_lmopathsearch(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O1,*O;
  NspLmo *H;
  NspSMatrix *Mname,*path;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H = GetLmo(stack,1)) == NULLLMO) return RET_BUG;
  if ((path = GetSMat(stack,2))  == NULLSMAT) return RET_BUG;
  if ((Mname = GetSMat(stack,3))  == NULLSMAT) return RET_BUG;
  /*if XXX ((O= lmo_path_search_name(H,path,Mname->S)) == NULLOBJ) */
  if ((O = ObjTrue(NVOID))== NULLOBJ) return RET_BUG;
  if ((O1= lmo_path_search_object(H,path,Mname->S)) == NULLOBJ) 
    {
      ((NspBMatrix *)O)->B[0] = FALSE;
    }
  MoveObj(stack,1,O);
  return 1;
}


/******************************************************
 *  Interface 
 ******************************************************/

static OpTab Lmo_func[]={
#include "lmo-IN.nam"
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

