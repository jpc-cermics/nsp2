/* Nsp
 * Copyright (C) 2008 Bruno Pincon Esial/Iecn
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
   
/*
 *    PURPOSE
 *      a 2d convex hull code (Graham algorithm (*) )
 *      (*) as explain in "Introduction to algorithmic" (T. Cormen,
 *      C. Leiserson, R. Rivest) with slight modifications.
 *    
 */

#include <stdlib.h>  /* C std qsort is used */

#define LEFT -1
#define COLINEAR 0
#define RIGHT 1

#define det(u,v) ( u[0]*v[1] - u[1]*v[0] )
#define max(a,b) ( (a) < (b) ? (b) : (a) )
#define push(i) ( ind[++top] = i )
#define pop() ( top-- )

/* global var used (for the comparizon func)  */
double x_bl, y_bl;
double *X, *Y;

static void init_perm(int p[], int N)
{
  int i;
  for ( i = 0 ; i < N ; i++ )
    p[i] = i;
}

static int find_bottom_left_point(double *x, double *y, int N)
{
  int i, i_bl = 0;
  
  for (i = 1; i < N; i++)
    {
      if (     y[i] < y[i_bl]  
	   || (y[i] == y[i_bl] && x[i] < x[i_bl]) )
	i_bl = i;
    }
  return i_bl;
}

static int comp_angle(const void * a1, const void * a2)
{
  /* 
   *  comparaison entre les angles polaires P0P1 et  P0P2
   *  on veut trier en ordre croissant et donc ap P0P1 < P0P2 si
   *     P2 est a gauche de P0P1, si on a le meme angle polaire 
   *     alors  P0P1 < P0P2  si P1 est + proche de P0 que P2
   */    

  int k1, k2;
  double v1[2], v2[2], dv[2], area, r;
  
  k1 = * (int *)a1; k2 = * (int *)a2;

  v1[0] = X[k1] - x_bl;
  v1[1] = Y[k1] - y_bl;
  v2[0] = X[k2] - x_bl;
  v2[1] = Y[k2] - y_bl;
  area = det(v1,v2);
  if ( area > 0 )
    return -1;
  else if ( area < 0 ) 
    return 1;
  else
    {  
      /* v1, v2 colinear : v2 = r v1  */
      dv[0] =  v2[0] - v1[0]; dv[1] = v2[1] - v1[1];
      r = max(dv[0],dv[1]);
      if ( r > 0 ) 
	return -1;
      else if ( r < 0 )
	return 1;
      else
	return 0;
    }
}
    
static int angle(k0, k1, k2)
{
  double v1[2], v2[2], area;

  v1[0] = X[k1] - X[k0];
  v1[1] = Y[k1] - Y[k0];
  v2[0] = X[k2] - X[k0];
  v2[1] = Y[k2] - Y[k0];
  area = det(v1,v2);

  if ( area > 0 )
    return LEFT;
  else if ( area < 0 )
    return RIGHT;
  else
    return COLINEAR;
}
	  
static void trivial_cases(int n, int *nhull, double *x, double *y, int *ind)
{
  /* treats cases where the number of points (n) is <= 2 */
  if ( n == 0 )
    *nhull = 0;
  else if ( n == 1 )
    {
      *nhull = 1;
      ind[0] = 1;
    }
  /* case n = 2 ; look if the 2 points are equal or not */
  else if (x[0] == x[1]  && y[0] == y[1] )
    {
      *nhull = 1;
      ind[0] = 1;
    }
  else
    {
      *nhull = 2;
      ind[0] = 1; ind[1] = 2;
    }
}

	
void nsp_convhull2d(int n, double *x, double *y, int *nhull, int *ind, int *p)
{
  /*  n     : number of points
   *  x,y   : coordinates (x[i], y[i]) are the coord of point i
   *  nhull : on output number of edges of the convex hull
   *  ind   : on output indices of the points of the convex hull  in trigo 
   *          orientation(the calling routine must reserve n places but only
   *          the first nhull indices are meaningful)
   *  p     : work array of length n : used to renumbering the points 
   */
  int i, i_bl, j, top;

  if ( n <= 2 )
    {
      trivial_cases(n, nhull, x, y, ind);
      return ;
    }

  X = x; Y = y;
  init_perm(p, n);

  /* step 1 : find the bottom left point then  place it as the point number 0 */
  i_bl = find_bottom_left_point(x, y, n);
  p[0] = i_bl ; p[i_bl] = 0;
  x_bl = x[i_bl]; y_bl = y[i_bl];

  /* step 2 : sort the points 1..n-1 with increasing polar angle P0Pi */
  qsort(&(p[1]), n-1, sizeof(int), comp_angle );  


  /* step 3 : init the stack with 3 points P0 and then generally P1 and P2
              but special cases may occur    */
  top = -1;   /* init stack */
  push(p[0]); 

  /* push the next first point != left bottom point (p[0])  on the stack */
  j = 1;
  while ( j < n &&  x[p[j]] == x_bl  &&  y[p[j]] == y_bl)
    j++;
  if ( j == n )  /* all the points are equal */
    goto end;

  push(p[j]);

  if ( j == n-1 )   /* only 2 points are distincts */
    goto end;

  /* found the third init point for the stack */
  j++;
  while ( j < n   &&   angle(p[top-1], p[top], p[j]) == COLINEAR )
    {
      pop(); push(p[j]); j++;  /* we stay with 2 points but with the previus sort
                                  p[j] is far from p[0] than p[top]  */
    }

  if ( j == n )
    goto end;
    
  push(p[j]);


  /*   step 4 : the Graham scan */
  for ( i = j+1 ; i < n ; i++ )
    {
      while ( angle(ind[top-1], ind[top], p[i]) != LEFT )
	pop();
      push(p[i]);
    }


  /* this is the end */
 end:
  for ( i = 0; i <= top; i++)   /* got 1 based indices */
    ind[i]++;

  *nhull = top + 1;
}
