/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2019 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"


typedef  struct  {
  char *name;
  void  (*fonc)();} TestOpTab ;

static void vide()
{}

extern TestOpTab GraphicTestTab[]; 


int main()
{
  GraphicTestTab[0].fonc();
}

void Sciprintf(void){} 

