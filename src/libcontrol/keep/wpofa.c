/* wpofa.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;

int nsp_ctrlpack_wpofa (double *ar, double *ai, int *lda, int *n, int *info)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int j, k;
  double s, ti, tr;
  int jm1;

  /*! 
   *    Copyright INRIA 
   */
  /* Parameter adjustments */
  ai_dim1 = *lda;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *lda;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;

  /* Function Body */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      *info = j;
      s = 0.;
      jm1 = j - 1;
      if (jm1 < 1)
	{
	  goto L20;
	}
      i__2 = jm1;
      for (k = 1; k <= i__2; ++k)
	{
	  i__3 = k - 1;
	  tr =
	    ar[k + j * ar_dim1] - nsp_calpack_wdotcr (&i__3,
						      &ar[k * ar_dim1 + 1],
						      &ai[k * ai_dim1 + 1],
						      &c__1,
						      &ar[j * ar_dim1 + 1],
						      &ai[j * ai_dim1 + 1],
						      &c__1);
	  i__3 = k - 1;
	  ti =
	    ai[k + j * ai_dim1] - nsp_calpack_wdotci (&i__3,
						      &ar[k * ar_dim1 + 1],
						      &ai[k * ai_dim1 + 1],
						      &c__1,
						      &ar[j * ar_dim1 + 1],
						      &ai[j * ai_dim1 + 1],
						      &c__1);
	  nsp_calpack_wdiv (&tr, &ti, &ar[k + k * ar_dim1],
			    &ai[k + k * ai_dim1], &tr, &ti);
	  ar[k + j * ar_dim1] = tr;
	  ai[k + j * ai_dim1] = ti;
	  s = s + tr * tr + ti * ti;
	  /* L10: */
	}
    L20:
      s = ar[j + j * ar_dim1] - s;
      if (s <= 0. || ai[j + j * ai_dim1] != 0.)
	{
	  goto L40;
	}
      ar[j + j * ar_dim1] = sqrt (s);
      /* L30: */
    }
  *info = 0;
 L40:
  return 0;
}				/* wpofa_ */
