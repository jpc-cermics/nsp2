#include "nsp/interf.h"

static void not(NspBMatrix *B) ;

int int_second(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* since not modify its argument we must use a copy */ 
  if ((A = GetBMatCopy(stack,1)) == NULLBMAT)  return RET_BUG;
  not(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1; 
}

static void not(NspBMatrix *B) 
{
  int i;
  for ( i=0 ; i < B->mn ; i++)  B->B[i] = !  B->B[i];
}

