#include "signal.h"

/*
 *realized frequencies omega 
 */

int signal_romeg (const int *nmaxi,const int *maxdeg,const int *ityp,const int *nzm,
		    const double *zm, double *rom)
{
  
  int n2, n3;
  int zm_dim1 = *maxdeg;
  int zm_offset = zm_dim1 + 1;
  zm -= zm_offset;

  --nzm;
  --rom;
  
  /* Function Body */
  n2 = nzm[2];
  n3 = nzm[3];
  switch (*ityp)
    {
    case 1:
      rom[1] = zm[n2 + (zm_dim1 << 1)];
      rom[2] = zm[zm_dim1 * 3 + 1];
      break;
    case 2:
      rom[1] = zm[zm_dim1 * 3 + 1];
      rom[2] = zm[n2 + (zm_dim1 << 1)];
      break;
    case 3:
      rom[1] = zm[n3 + zm_dim1 * 3];
      rom[2] = zm[(zm_dim1 << 1) + 1];
      rom[3] = zm[n2 + (zm_dim1 << 1)];
      rom[4] = zm[zm_dim1 * 3 + 1];
      break;
    case 4:
      n2 /= 2;
      rom[1] = zm[n2 + (zm_dim1 << 1)];
      rom[4] = zm[n2 + 1 + (zm_dim1 << 1)];
      rom[3] = zm[zm_dim1 * 3 + 1];
      rom[2] = zm[n3 + zm_dim1 * 3];
      break;
    }
  return 0;
}

