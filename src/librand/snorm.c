#include "grand.h"

/* 
 *     (STANDARD-)  N O R M A L  DISTRIBUTION                           
 *
 *     FOR DETAILS SEE:                                                 
 *                                                                      
 *               AHRENS, J.H. AND DIETER, U.                            
 *               EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             
 *               SAMPLING FROM THE NORMAL DISTRIBUTION.                 
 *               MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          
 *                                                                      
 *     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  
 *     (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  
 *                                                                      
 *     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   
 *     SUNIF.  The argument IR thus goes away.                          
 *                                                                      
 */

double rand_snorm (void)
{
  /*     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND 
   *     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
   */
  static const double a[32] =
    { 0., .03917609, .07841241, .1177699, .1573107, .1970991, .2372021,
      .2776904, .3186394, .3601299, .4022501, .4450965, .4887764, .5334097, .5791322, .626099,
      .6744898, .7245144, .7764218, .8305109, .8871466, .9467818, 1.00999, 1.077516, 1.150349,
      1.229859, 1.318011, 1.417797, 1.534121, 1.67594, 1.862732, 2.153875 };
  static const double d__[31] =
    { 0., 0., 0., 0., 0., .2636843, .2425085, .2255674, .2116342, .1999243,
      .1899108, .1812252, .1736014, .1668419, .1607967, .1553497, .1504094, .1459026, .14177,
      .1379632, .1344418, .1311722, .128126, .1252791, .1226109, .1201036, .1177417, .1155119,
      .1134023, .1114027, .1095039 };
  static const double t[31] =
    { 7.673828e-4, .00230687, .003860618, .005438454, .007050699, .008708396,
      .01042357, .01220953, .01408125, .01605579, .0181529, .02039573, .02281177, .02543407,
      .02830296, .03146822, .03499233, .03895483, .04345878, .04864035, .05468334, .06184222,
      .07047983, .08113195, .09462444, .1123001, .136498, .1716886, .2276241, .330498, .5847031 };
  static const double h__[31] =
    { .03920617, .03932705, .03950999, .03975703, .04007093, .04045533,
      .04091481, .04145507, .04208311, .04280748, .04363863, .04458932, .04567523, .04691571,
      .04833487, .04996298, .05183859, .05401138, .05654656, .0595313, .06308489, .06737503,
      .07264544, .07926471, .08781922, .09930398, .1155599, .1404344, .1836142, .2790016,
      .7010474 };

  double ret_val;
  int i__;
  double s, u, w, y, ustar, aa, tt;

  /* L10: */
  u = rand_ranf ();
  s = 0.;
  if (u > .5)
    {
      s = 1.;
    }
  u = u + u - s;
  /* L20: */
  u *= 32.;
  i__ = (int) u;
  if (i__ == 32)
    {
      i__ = 31;
    }
  if (i__ == 0)
    {
      goto L100;
    }
  /*                                START CENTER */

  /* L30: */
  ustar = u - (double) i__;
  aa = a[i__ - 1];
 L40:
  if (ustar <= t[i__ - 1])
    {
      goto L60;
    }
  w = (ustar - t[i__ - 1]) * h__[i__ - 1];
  /*                                EXIT   (BOTH CASES) */
 L50:
  y = aa + w;
  ret_val = y;
  if (s == 1.)
    {
      ret_val = -y;
    }
  return ret_val;
  /*                                CENTER CONTINUED */
 L60:
  u = rand_ranf ();
  w = u * (a[i__] - aa);
  tt = (w * .5 + aa) * w;
  goto L80;
 L70:
  tt = u;
  ustar = rand_ranf ();
 L80:
  if (ustar > tt)
    {
      goto L50;
    }
  /* L90: */
  u = rand_ranf ();
  if (ustar >= u)
    {
      goto L70;
    }
  ustar = rand_ranf ();
  goto L40;
  /*                                START TAIL */
 L100:
  i__ = 6;
  aa = a[31];
  goto L120;
 L110:
  aa += d__[i__ - 1];
  ++i__;
 L120:
  u += u;
  if (u < 1.)
    {
      goto L110;
    }
  /* L130: */
  u += -1.;
 L140:
  w = u * d__[i__ - 1];
  tt = (w * .5 + aa) * w;
  goto L160;
 L150:
  tt = u;
 L160:
  ustar = rand_ranf ();
  if (ustar > tt)
    {
      goto L50;
    }
  /* L170: */
  u = rand_ranf ();
  if (ustar >= u)
    {
      goto L150;
    }
  u = rand_ranf ();
  goto L140;
}	


