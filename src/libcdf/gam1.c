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



/* 
 * Using Maple 
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) 1/GAMMA(1+x) -1;end proc;
  Digits:=70;
  ggp:=chebpade(f(x)/x,x=0..0.5,[6,4]);
  Digits:=17;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(x*gg,x);
  infnorm(f(x) -f_approx(x),x=-0.5..0);
  codegen[C](f_approx,optimized);
  codegen[C](g_cheb,optimized);
  codegen[C](confracform(u*subs(x=u,gg)));

  f_err:=proc(x) local u1,u2; u1:=evalf(f(x),70); u2:=evalf(f_approx(x),17);
    evalf((u1-u2)/u1,70);
  end proc;
  
  m_err:=proc(a,b,c,nn)
    s:=[seq(i/(c*nn),i=(a*nn)..(b*nn))];
    serr:= map(f_err,s);
    convert(max(op(serr)),float);
  end proc;
  
  m_err(01,500,1000,1);

  # test current code 
  #--------------------

  p := [ .577215664901533, -.409078193005776, -.230975380857675,
         .0597275330452234, .0076696818164949, -.00514889771323592, 5.89597428611429e-4 ];
  q := [ 1., .427569613095214, .158451672430138, .0261132021441447, 0.00423244297896961 ];
  top :=(((((p[7]*x + p[6])*x + p[5])*x + p[4])*x + p[3])*x + p[2])*x + p[1];
  bot :=(((q[5]*x + q[4])*x + q[3])*x + q[2])*x + q[1];
  f_approx:= unapply(x* top/bot,x);

  infnorm(f(x) - f_approx(x),x=0..0.5);

  m_err(01,500,1000,1);

*/

