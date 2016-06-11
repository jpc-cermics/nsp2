/* isova0.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_isova0 (double *a, int *lda, int *m, int *n, double *path,
		    int *kpath, int *ir, int *ic, int *dir, int *pend,
		    int *h__, int *v, double *c__)
{
  /* Initialized data */

  static int north = 0;
  static int south = 1;
  static int east = 2;
  static int west = 3;

  /* System generated locals */
  int a_dim1, a_offset, h_dim1, h_offset, v_dim1, v_offset;

  /*    Copyright INRIA 
   *% but 
   *    Sous programme appele par le sous programme isoval 
   *% 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  v_dim1 = *m - 1;
  v_offset = v_dim1 + 1;
  v -= v_offset;
  h_dim1 = *m;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;
  path -= 3;

  /* Function Body */
  /*    extend the path at this level by one edge element 
   */
  if (*dir == north)
    {
      if (v[*ir + *ic * v_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      h__[*ir + *ic * h_dim1] = 0;
	    }
	  /*    path to east 
	   */
	  goto L30;
	}
      else if (v[*ir + (*ic + 1) * v_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      h__[*ir + *ic * h_dim1] = 0;
	    }
	  /*    path to west 
	   */
	  goto L40;
	}
      else if (h__[*ir + 1 + *ic * h_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      h__[*ir + *ic * h_dim1] = 0;
	    }
	  /*    path to north 
	   */
	  goto L10;
	}
      else
	{
	  *pend = TRUE;
	}
    }
  else if (*dir == west)
    {
      if (h__[*ir + 1 + *ic * h_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      v[*ir + *ic * v_dim1] = 0;
	    }
	  /*    path to north 
	   */
	  goto L10;
	}
      else if (h__[*ir + *ic * h_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      v[*ir + *ic * v_dim1] = 0;
	    }
	  /*    path to south 
	   */
	  goto L20;
	}
      else if (v[*ir + (*ic + 1) * v_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      v[*ir + *ic * v_dim1] = 0;
	    }
	  /*    path to west 
	   */
	  goto L40;
	}
      else
	{
	  *pend = TRUE;
	}
    }
  else if (*dir == south)
    {
      if (v[*ir + (*ic + 1) * v_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      h__[*ir + 1 + *ic * h_dim1] = 0;
	    }
	  /*    path to west 
	   */
	  goto L40;
	}
      else if (v[*ir + *ic * v_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      h__[*ir + 1 + *ic * h_dim1] = 0;
	    }
	  /*    path to east 
	   */
	  goto L30;
	}
      else if (h__[*ir + *ic * h_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      h__[*ir + 1 + *ic * h_dim1] = 0;
	    }
	  /*    path to south 
	   */
	  goto L20;
	}
      else
	{
	  *pend = TRUE;
	}
    }
  else if (*dir == east)
    {
      if (h__[*ir + *ic * h_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      v[*ir + (*ic + 1) * v_dim1] = 0;
	    }
	  /*    path to south 
	   */
	  goto L20;
	}
      else if (h__[*ir + 1 + *ic * h_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      v[*ir + (*ic + 1) * v_dim1] = 0;
	    }
	  /*    path to north 
	   */
	  goto L10;
	}
      else if (v[*ir + *ic * v_dim1] < 0)
	{
	  if (*kpath > 1)
	    {
	      v[*ir + (*ic + 1) * v_dim1] = 0;
	    }
	  /*    path to east 
	   */
	  goto L30;
	}
      else
	{
	  *pend = TRUE;
	}
    }
  return 0;
  /* 
   */
L10:
  /* 
   *    NORTH 
   * 
   */
  ++(*kpath);
  path[(*kpath << 1) + 2] = (double) (*ir + 1);
  path[(*kpath << 1) + 1] =
    *ic + (*c__ -
	   a[*ir + 1 + *ic * a_dim1]) / (a[*ir + 1 + (*ic + 1) * a_dim1] -
					 a[*ir + 1 + *ic * a_dim1]);
  if (*ir + 1 < *m)
    {
      ++(*ir);
      *dir = north;
    }
  else
    {
      *pend = TRUE;
    }
  return 0;
L20:
  /* 
   *    SOUTH 
   * 
   */
  ++(*kpath);
  path[(*kpath << 1) + 2] = (double) (*ir);
  path[(*kpath << 1) + 1] =
    *ic + (*c__ - a[*ir + *ic * a_dim1]) / (a[*ir + (*ic + 1) * a_dim1] -
					    a[*ir + *ic * a_dim1]);
  if (*ir > 1)
    {
      --(*ir);
      *dir = south;
    }
  else
    {
      *pend = TRUE;
    }
  return 0;
  /* 
   */
L30:
  /* 
   *    EAST 
   * 
   */
  ++(*kpath);
  path[(*kpath << 1) + 2] =
    *ir + (*c__ - a[*ir + *ic * a_dim1]) / (a[*ir + 1 + *ic * a_dim1] -
					    a[*ir + *ic * a_dim1]);
  path[(*kpath << 1) + 1] = (double) (*ic);
  if (*ic > 1)
    {
      --(*ic);
      *dir = east;
    }
  else
    {
      *pend = TRUE;
    }
  return 0;
  /* 
   */
L40:
  /* 
   *    WEST 
   * 
   */
  ++(*kpath);
  path[(*kpath << 1) + 2] =
    *ir + (*c__ -
	   a[*ir + (*ic + 1) * a_dim1]) / (a[*ir + 1 + (*ic + 1) * a_dim1] -
					   a[*ir + (*ic + 1) * a_dim1]);
  path[(*kpath << 1) + 1] = (double) (*ic + 1);
  if (*ic + 1 < *n)
    {
      ++(*ic);
      *dir = west;
    }
  else
    {
      *pend = TRUE;
    }
  return 0;
  /* 
   */
}				/* isova0_ */
