/* dlblks.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

int nsp_calpack_dlblks (char *name__, int *nbc, long int name_len)
{
  /* System generated locals */
  int i__1, i__2;

  /* Builtin functions */
  int i_len (char *, long int), i_indx (char *, char *, long int, long int);
  int s_copy (char *, char *, long int, long int);

  /* Local variables */
  int i__, j, k, ll;

  /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
   * 
   *       ENLEVER LES BLANCS EN DEBUT D'UNE CHAINE DE CARACTERES 
   * 
   * 
   *ENTREE : NAME  LA CHAINE DE CARACTERES 
   * 
   *SORTIE : NAME  LA CHAINE SANS LES BLANCS 
   *         NBC   NOMBRE DE CARACTERES DE LA CHAINE 
   * 
   *FORTRAN  INDEX, LEN 
   *.................................................................... 
   *    Copyright INRIA 
   * 
   */
  ll = i_len (name__, name_len);
  i__ = 0;
L1:
  ++i__;
  k = i_indx (name__ + (i__ - 1), " ", ll - (i__ - 1), 1L);
  if (k == 0)
    {
      k = ll - i__ + 2;
    }
  if (k == 1 && i__ < ll)
    {
      goto L1;
    }
  /* 
   */
  *nbc = k - 1;
  --i__;
  i__1 = *nbc;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = j + i__ - 1;
      s_copy (name__ + (j - 1), name__ + i__2, 1L, j + i__ - i__2);
      /* L3: */
    }
  i__1 = ll;
  for (j = *nbc + 1; j <= i__1; ++j)
    {
      *(unsigned char *) &name__[j - 1] = ' ';
      /* L5: */
    }
  /* 
   * 
   */
  return 0;
}				/* dlblks_ */
#endif
