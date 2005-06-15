#include "nsp/interf.h"
#include "lib/pipo.h"

int int_f4(Stack stack, int rhs, int opt, int lhs)
{
  int i1,i2;
  CheckRhs(1,1);
  CheckLhs(1,1); 
  if ( GetScalarInt(stack,1,&i1)== FAIL) return RET_BUG;
  if ( GetScalarInt(stack,1,&i2)== FAIL) return RET_BUG;
  i1= foo(i2); 
  i2= bar(i2); 
  if ( nsp_move_double(stack,1,(double)i1 )== FAIL) return RET_BUG;
  if ( lhs == 2) 
    {
      if ( nsp_move_double(stack,2,(double)i2 )== FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}
