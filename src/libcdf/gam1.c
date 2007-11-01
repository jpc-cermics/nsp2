#include "cdf.h"

/**
 * cdf_gam1:
 * @a: a double 
 * 
 * computation of 1/gamma(a+1) - 1  for -0.5 <= a <= 1.5 
 * 
 * Returns: a double 
 **/



double cdf_gam1 (double a)
{
  double x = a , d = a-0.5,w;
  if (d > 0.)
    {
      x = d - .5;
    }
  if (x < 0.)
    {
      w=((-0.4590122967487913
	  +(-0.9119365963421156
	    +(-0.4018670559644266
	      +(0.839538288239654E-1
		+(0.2010434480179195E-1
		  +(-0.1271324310564952E-1
		    +(0.7618561333361629E-3
		      +(0.5669913109268278E-3-0.1158851179261901E-3*x)*x)*x)*x)*x)*x)*x)*x)
	 /(0.1085688987606144E1
	   +(0.472716182433005
	     +(0.1093254307448214
	       +0.125260780814972E-1*x)*x)*x));
      if (d > 0.)
	{
	  return  x*w / a;
	}
      return  a*(w + .5 + .5);
    }
  else if (x == 0)
    {
      return 0.0;
    }
  else
    {
      w = ((0.4726486434030833
	    +(-0.1807940100821864
	      +(-0.293727877904757
		+(-0.1086407155778191E-1
		  +(0.1540013425957419E-1-0.2662837428318595E-2*x)*x)*x)*x)*x)
	   /(0.8188423706132649
	     +(0.6172160017445146
	       +(0.2520446154036438
		 +(0.762319784705497E-1
		   +(0.1342400775656667E-1
		     +0.2239198998005776E-2*x)*x)*x)*x)*x));
      if (d > 0.)
	{
	  return  x / a*(w - .5 - .5);
	}
      return  a*w;
    }
}

 
/* 
 * Using Maple 
 * Note that f(x) = ((x-1)/x) *(f(x-1)/(x-1) -1 ) 

  with(numapprox);
  with(orthopoly);
  f:= proc(x) 1/GAMMA(1+x) -1;end proc;
  Digits:=50;
  ggp:=chebpade(f(x)/x-1,x=-0.5..0,[8,2]);
  Digits:=17;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  c_approx:= unapply(gg,x);
  codegen[C](c_approx,optimized);

  Digits:=40;
  ggp:=chebpade(f(x)/x,x=0..0.5,[6,6]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  c_approx:= unapply(gg,x);
  codegen[C](c_approx,optimized);

*/


/**
 * cdf_gam1:
 * @a: a double 
 * 
 * computation of 1/gamma(a+1) - 1  for -0.5 <= a <= 1.5 
 * 
 * Returns: a double 
 **/

/* acm code used for testing
 *
 */

double cdf_gam1_old (double a)
{
  const double p[7] = { .577215664901533, -.409078193005776, -.230975380857675,
			.0597275330452234, .0076696818164949, -.00514889771323592, 
			5.89597428611429e-4 };
  const double q[5] = { 1., .427569613095214, .158451672430138, .0261132021441447,
			.00423244297896961 };
  const double r[9] = { -.422784335098468, -.771330383816272, -.244757765222226,
			  .118378989872749, 9.30357293360349e-4, -.0118290993445146,
			  .00223047661158249,   2.66505979058923e-4, -1.32674909766242e-4 };
  const double s1 = .273076135303957;
  const double s2 = .0559398236957378;

  double d, t, w, bot, top;

  t = a;
  d = a - .5;
  if (d > 0.)
    {
      t = d - .5;
    }
  if (t < 0.)
    {
      top =(((((((r[8]*t + r[7])*t + r[6])*t + r[5])*t +  r[4])*t + r[3])*t + r[2])*t + r[1])*t + r[0];
      bot = (s2*t + s1)*t + 1.;
      w = top / bot;
      if (d > 0.)
	{
	  return  t*w / a;
	}
      return  a*(w + .5 + .5);
    }
  else if (t == 0)
    {
      return 0.0;
    }
  else
    {
      top =(((((p[6]*t + p[5])*t + p[4])*t + p[3])*t + p[2])*t + p[1])*t + p[0];
      bot =(((q[4]*t + q[3])*t + q[2])*t + q[1])*t + 1.;
      w = top / bot;
      if (d > 0.)
	{
	  return  t / a*(w - .5 - .5);
	}
      return  a*w;
    }
}
