#ifndef SCI_CALLFUNC
#define SCI_CALLFUNC

/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include "nsp/interf.h" /* for function */

typedef int  (sci_interface) (int num,Stack stack,int rhs,int opt,int lhs);
typedef void  (interface_info) (int num,char **fname,function **f);

typedef  struct  {
  sci_interface *fonc;
  interface_info *info;
} InterfTab ;

extern InterfTab Interfaces[];



#endif 
