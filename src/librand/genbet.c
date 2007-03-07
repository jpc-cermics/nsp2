#include "grand.h"


/* 
 *     DOUBLE PRECISION FUNCTION GENBET( A, B ) 
 *               GeNerate BETa random deviate 
 *                              Function 
 *     Returns a single random deviate from the beta distribution with 
 *     parameters A and B.  The density of the beta is 
 *               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1 
 *                              Arguments 
 *     A --> First parameter of the beta distribution 
 *                         DOUBLE PRECISION A 
 *     JJV                 (A > 1.0E-37) 
 *     B --> Second parameter of the beta distribution 
 *                         DOUBLE PRECISION B 
 *     JJV                 (B > 1.0E-37) 
 *                              Method 
 *     R. C. H. Cheng 
 *     Generating Beta Variates with Nonintegral Shape Parameters 
 *     Communications of the ACM, 21:317-322  (1978) 
 *     (Algorithms BB and BC) 
 */

double rand_genbet (double *aa, double *bb)
{
  /*   expmax:   Close to the largest number that can be exponentiated */
  /*     JJV changed this - 89 was too high, and LOG(1.0E38) = 87.49823 */
  /*( (expmax=87.49823) */
  /*     Close to the largest representable single precision number */
  /*( (infnty=1.0E38) */
  /*     JJV added the parameter minlog */
  /*     Close to the smallest number of which a LOG can be taken. */
  /*( (minlog=1.0E-37) */

  static double olda = -1e37, oldb = -1e37,beta,a, b, gamma,alpha,k1,k2;
  double ret_val, d__1, r__, s, t, v, w, delta, y, z__, u1, u2;
  int qsame;
  const double expmax=87.49823;
  const double infnty=1.0E38;
  const double minlog=1.0E-37;

  qsame = olda == *aa && oldb == *bb;
  if (qsame)
    {
      goto L20;
    }
  /*     JJV added small minimum for small log problem in calc of W */
  /*     in Rand.c */
  /* L10: */
  olda = *aa;
  oldb = *bb;
 L20:
  if (!(Min (*aa, *bb) > 1.))
    {
      goto L100;
    }
  /*     Alborithm BB */

  /*     Initialize */

  if (qsame)
    {
      goto L30;
    }
  a = Min (*aa, *bb);
  b = Max (*aa, *bb);
  alpha = a + b;
  beta = sqrt ((alpha - 2.) / (a * 2. * b - alpha));
  gamma = a + 1. / beta;
 L30:
 L40:
  u1 = rand_ranf ();

  /*     Step 1 */

  u2 = rand_ranf ();
  v = beta * log (u1 / (1. - u1));
  /*     JJV altered this */
  if (v > expmax)
    {
      goto L55;
    }
  /*     JJV added checker to see if a*exp(v) will overflow */
  /*     JJV 50 _was_ w = a*exp(v); also note here a > 1.0 */
  /* L50: */
  w = exp (v);
  if (w > infnty / a)
    {
      goto L55;
    }
  w = a * w;
  goto L60;
 L55:
  w = infnty;
 L60:
  /* Computing 2nd power */
  d__1 = u1;
  z__ = d__1 * d__1 * u2;
  r__ = gamma * v - 1.3862944;
  s = a + r__ - w;

  /*     Step 2 */

  if (s + 2.609438 >= z__ * 5.)
    {
      goto L70;
    }

  /*     Step 3 */

  t = log (z__);
  if (s > t)
    {
      goto L70;
    }

  /*     Step 4 */

  /*     JJV added checker to see if log(alpha/(b+w)) will */
  /*     JJV overflow.  If so, we count the log as -INF, and */
  /*     JJV consequently evaluate conditional as true, i.e. */
  /*     JJV the algorithm rejects the trial and starts over */
  /*     JJV May not need this here since ALPHA > 2.0 */
  if (alpha / (b + w) < minlog)
    {
      goto L40;
    }
  if (r__ + alpha * log (alpha / (b + w)) < t)
    {
      goto L40;
    }

  /*     Step 5 */

 L70:
  if (!(*aa == a))
    {
      goto L80;
    }
  ret_val = w / (b + w);
  goto L90;
 L80:
  ret_val = b / (b + w);
 L90:
  goto L230;
  /*     Algorithm BC */

  /*     Initialize */

 L100:
  if (qsame)
    {
      goto L110;
    }
  a = Max (*aa, *bb);
  b = Min (*aa, *bb);
  alpha = a + b;
  beta = 1. / b;
  delta = a + 1. - b;
  k1 = delta * (b * .0416667 + .0138889) / (a * beta - .777778);
  k2 = (.25 / delta + .5) * b + .25;
 L110:
 L120:
  u1 = rand_ranf ();

  /*     Step 1 */

  u2 = rand_ranf ();
  if (u1 >= .5)
    {
      goto L130;
    }

  /*     Step 2 */

  y = u1 * u2;
  z__ = u1 * y;
  if (u2 * .25 + z__ - y >= k1)
    {
      goto L120;
    }
  goto L170;

  /*     Step 3 */

 L130:
  /* Computing 2nd power */
  d__1 = u1;
  z__ = d__1 * d__1 * u2;
  if (!(z__ <= .25))
    {
      goto L160;
    }
  v = beta * log (u1 / (1. - u1));
  /*     JJV instead of checking v > expmax at top, I will check */
  /*     JJV if a < 1, then check the appropriate values */
  if (a > 1.)
    {
      goto L135;
    }
  /*     JJV A < 1 so it can help out if EXP(V) would overflow */
  if (v > expmax)
    {
      goto L132;
    }
  w = a * exp (v);
  goto L200;
 L132:
  w = v + log (a);
  if (w > expmax)
    {
      goto L140;
    }
  w = exp (w);
  goto L200;
  /*     JJV in this case A > 1 */
 L135:
  if (v > expmax)
    {
      goto L140;
    }
  w = exp (v);
  if (w > infnty / a)
    {
      goto L140;
    }
  w = a * w;
  goto L200;
 L140:
  w = infnty;
  goto L200;
 L160:
  if (z__ >= k2)
    {
      goto L120;
    }

  /*     Step 4 */


  /*     Step 5 */

 L170:
  v = beta * log (u1 / (1. - u1));
  /*     JJV same kind of checking as above */
  if (a > 1.)
    {
      goto L175;
    }
  /*     JJV A < 1 so it can help out if EXP(V) would overflow */
  if (v > expmax)
    {
      goto L172;
    }
  w = a * exp (v);
  goto L190;
 L172:
  w = v + log (a);
  if (w > expmax)
    {
      goto L180;
    }
  w = exp (w);
  goto L190;
  /*     JJV in this case A > 1 */
 L175:
  if (v > expmax)
    {
      goto L180;
    }
  w = exp (v);
  if (w > infnty / a)
    {
      goto L180;
    }
  w = a * w;
  goto L190;
 L180:
  w = infnty;
  /*     JJV here we also check to see if log overlows; if so, we treat it */
  /*     JJV as -INF, which means condition is true, i.e. restart */
 L190:
  if (alpha / (b + w) < minlog)
    {
      goto L120;
    }
  if (alpha * (log (alpha / (b + w)) + v) - 1.3862944 < log (z__))
    {
      goto L120;
    }

  /*     Step 6 */

 L200:
  if (!(a == *aa))
    {
      goto L210;
    }
  ret_val = w / (b + w);
  goto L220;
 L210:
  ret_val = b / (b + w);
 L220:
 L230:
  return ret_val;
}				/* genbet_ */
