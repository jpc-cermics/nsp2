#include "cdf.h"

const double c_b5 = 1.;

/**
 * cdf_erf:
 * @x: a double 
 * 
 * 
 *     cdf_erf function returns the error function of x; defined as
 *        erf(x) = 2/sqrt(pi)* integral from 0 to x of exp(-t*t) dt
 * 
 * Returns: 
 **/

static double f_approx(double x)
{
  double t1 =  x*x;
  return((0.2132192804233654E1+(0.2507950205818281+(0.7564786793930115E-1+(
0.210035614139579E-2+0.1234228302254547E-3*t1)*t1)*t1)*t1)/(0.9448033366842427+
(0.4260650955659615+(0.8106195356401662E-1+(0.784015525419341E-2+
0.3321975634818686E-3*t1)*t1)*t1)*t1));
}

static double f_approx1(double x)
{
  return((0.3568217177627678E-2+(0.5444297501411179E-2+(0.4135716726130396E-2
+(0.1888516179006951E-2+(0.5404695127508915E-3+(0.9187011004034143E-4+(
0.7350874797709247E-5-0.1128729588597506E-11*x)*x)*x)*x)*x)*x)*x)/(
0.356821717529337E-2+(0.9470599459420979E-2+(0.112539264524628E-1+(
0.7800815126334948E-2+(0.3428993607308159E-2+(0.9644447087996436E-3+(
0.1628374909101921E-3+0.130289958082267E-4*x)*x)*x)*x)*x)*x)*x));
}

static double f_approx2(double x)
{
  return((0.1920128783798018E-1+(0.2769423798968193+(0.1364407135799869E1+(
0.2867943355305776E1+(0.263259262902629E1+(0.9670512406343465+(
0.1104364005166393+0.1680026092294234E-2*x)*x)*x)*x)*x)*x)*x)/(
0.1920128897453642E-1+(0.286542962836701+(0.1493279359010387E1+(
0.343564527820764E1+(0.3642221565674471E1+(0.1691183279839614E1+(
0.2925780360848024+0.1223013739426987E-1*x)*x)*x)*x)*x)*x)*x));
}


double cdf_erf (double x)
{
  const double c = .564189583547756; /* 1/sqrt(%pi) */
  const double r__[5] = { 2.10144126479064, 26.2370141675169, 21.3688200555087, 
			  4.6580782871847,  .282094791773523 };
  const double s[4] =   { 94.153775055546, 187.11481179959, 99.0191814623914, 
			  18.0124575948747 };

  double ret_val, t, x2, ax, bot, top;

  ax = Abs (x);
  if (ax <= 0.5)
    {
      return x*f_approx(x)/2.0;
    }
  if (ax > 4.)
    {
      if (ax >= 7) return ( x > 0) ? 1 : -1 ;
      /* asymptotic series 
       *
       * erf(x) = 1 - (exp(-x^2)/sqrt(Pi))*(1/x)* 
       *           (1 - (1/2)*x^-2 +(3/4)*x^-4 -(15/8) x^(-6)+ (105/16)*x^(-8)) 
       * the asymptotic serie is approximated by pade approximation using 
       * Maple. 
       */
      x2 = x * x;
      t = 1. / x2;
      ret_val =  1 - exp(-x2)*c*(1/ax)*f_approx2(t);
      return  (x < 0.) ?  -ret_val : ret_val ;
    }
  
  ret_val= 0.5 - exp (-(ax) * ax)*f_approx1(ax)+0.5;
  return ( x < 0) ?  -ret_val : ret_val ;

}


/**
 * cdf_erfc:
 * @ind: an integer 
 * @x: a double 
 * 
 * complementary error function 
 *     cdf_erfc(0,x) function returns the complementary error function of  x,
 *       that is 1.0 - erf(x).
 *     cdf_erfc(1,x) function returns 
 *        y = exp(x^2 ) * erfc(x)
 *                   1
 *        y  -->  ---------    when x --> +oo
 *                x sqrt(pi) 
 *
 * 
 * 
 * Returns: a double 
 **/

double cdf_erfc (int ind, double x)
{
  const int c__1 = 1;
  const double c__ = .564189583547756;
  const double a[5] =  { 7.7105849500132e-5, -.00133733772997339, .0323076579225834,
			 .0479137145607681, .128379167095513  };
  const double b[3] =  { .00301048631703895, .0538971687740286, .375795757275549 };
  const double p[8] =  { -1.36864857382717e-7, .564195517478974, 7.21175825088309,
			 43.1622272220567, 152.98928504694, 339.320816734344, 
			 451.918953711873,  300.459261020162 };
  const double q[8] =  { 1., 12.7827273196294, 77.0001529352295, 277.585444743988,
			 638.980264465631, 931.35409485061, 790.950925327898, 
			 300.459260956983 };
  const double r__[5] = { 2.10144126479064, 26.2370141675169, 21.3688200555087, 
			  4.6580782871847, .282094791773523 };
  const double s[4] =  { 94.153775055546, 187.11481179959, 99.0191814623914, 
			 18.0124575948747 };
  double ret_val, d__1;
  double e, t, w, ax;
  double bot, top;

  /*                     ABS(X) .LE. 0.5 */

  ax = Abs (x);
  if (ax > .5)
    {
      goto L10;
    }
  t = x * x;
  top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.;
  bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.;
  ret_val = .5 - x * (top / bot) + .5;
  if (ind != 0)
    {
      ret_val = exp (t) * ret_val;
    }
  return ret_val;
  /*                  0.5 .LT. ABS(X) .LE. 4 */
 L10:
  if (ax > 4.)
    {
      goto L20;
    }
  top =
    ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax +
      p[5]) * ax + p[6]) * ax + p[7];
  bot =
    ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax +
      q[5]) * ax + q[6]) * ax + q[7];
  ret_val = top / bot;
  goto L40;
  /*                      ABS(X) .GT. 4 */
 L20:
  if (x <= -5.6)
    {
      goto L60;
    }
  if (ind != 0)
    {
      goto L30;
    }
  if (x > 100.)
    {
      goto L70;
    }
  if (x * x > -cdf_exparg (c__1))
    {
      goto L70;
    }
 L30:
  d__1 = 1. / x;
  t = d__1 * d__1;
  top = (((r__[0] * t + r__[1]) * t + r__[2]) * t + r__[3]) * t + r__[4];
  bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.;
  ret_val = (c__ - t * top / bot) / ax;
  
 L40:
  if (ind == 0)
    {
      goto L50;
    }
  if (x < 0.)
    {
      ret_val = exp (x * x) * 2. - ret_val;
    }
  return ret_val;
L50:
  w = x * x;
  t = w;
  e = w - t;
  ret_val = (.5 - e + .5) * exp (-t) * ret_val;
  if (x < 0.)  ret_val = 2. - ret_val;
  return ret_val;
 L60:
  /*             LIMIT VALUE FOR LARGE NEGATIVE X */
  ret_val = 2.;
  if (ind != 0)
    {
      ret_val = exp (x * x) * 2.;
    }
  return ret_val;
 L70:
  /*             LIMIT VALUE FOR LARGE POSITIVE X */
  /*                       WHEN IND = 0 */
  ret_val = 0.;
  return ret_val;
}	


/*
 * Using Maple code for approximation of log(GAMMA(x)) in [2,3] 
 *
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) (erf(x)-erf(-x))/x ;end proc;

  Digits:=120;
  ggp:=chebpade(f(x),x=-0.5..0.5,[10,10]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

  
  g:= proc(x) (1-erf(x))*exp(x^2) ;end proc;

  Digits:=120;
  ggp:=chebpade(g(x),x=0.5..4,[7,7]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

  h:= proc(x) (1 - erf(x))*(exp(x^2)*sqrt(Pi))*x; end proc;
  h1:= proc(x) h(1/sqrt(x));end proc; 
  Digits:=120;
  ggp:=chebpade(h1(x),x=(1/sqrt(5.8))..(1/2),[7,7]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

  
  

*/
