
#include <strings.h>
#include "grand.h"


/* ********************************************************************** */
/*     SUBROUTINE PHRTSD( PHRASE, SEED1, SEED2 ) */
/*               PHRase To SeeDs */
/*                              Function */
/*     Uses a phrase (character string) to generate two seeds for the RGN */
/*     random number generator. */
/*                              Arguments */
/*     PHRASE --> Phrase to be used for random number generation */
/*                         CHARACTER*(*) PHRASE */
/*     SEED1 <-- First seed for RGN generator */
/*                         INTEGER SEED1 */
/*     SEED2 <-- Second seed for RGN generator */
/*                         INTEGER SEED2 */
/*                              Note */
/*     Generated seed values will fall in the range 1..2^30 */
/*     (1..1,073,741,824) */
/* ********************************************************************** */

int rand_phrtsd(char *phrase,int *seed1, int *seed2) 
{
  const int twop30=1073741824;
  static const int shift[5] = { 1,64,4096,262144,16777216 };
  /* System generated locals */
  int ichr, lphr, i, j, values[5];
  *seed1 = 1234567890;
  *seed2 = 123456789;
  lphr = strlen(phrase);
  if ( lphr == 0) return 0;
  for (i = 0 ; i < lphr ; ++i) 
    {
      ichr = ((int) phrase[i]) % 64 ;
      if (ichr == 0) {  ichr = 63;}
      for (j = 1; j <= 5; ++j) 
	{
	  values[j - 1] = ichr - j;
	  if (values[j - 1] < 1) 
	    {
	      values[j - 1] += 63;
	    }
	}
      for (j = 1; j <= 5; ++j) {
	*seed1 = (*seed1 + shift[j - 1] * values[j - 1]) % twop30;
	*seed2 = (*seed2 + shift[j - 1] * values[6 - j - 1]) % twop30;
      }
    }
  return 0;
} 


