#ifndef NSP_INC_COMPLEX
#define NSP_INC_COMPLEX 

/*
 * This Software is GPL (Copyright ENPC 1998-2010) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 
#include "nsp/objectf.h"  /* forward declarations */

/**
 * doubleC: 
 * @r: real part 
 * @i: imaginary part
 * 
 * structure used to store complex values i.e two doubles.
 */

/* typedef struct _doubleC doubleC; */

struct _doubleC { 
  double r;
  double i; 
};

#endif 
