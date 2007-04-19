#include "cdf.h"

/**
 * cdf_rlog:
 * @x: a pointer to a double 
 * 
 * evaluation of the function  x - 1 - LN(x)
 * 
 * Returns: a double 
 **/


double cdf_rlog (double x)
{
  /*test:  A=[0.1:0.01:2]; norm(A-1 - log(A) - cdf_rlog(A))  */
  const double a = .0566749439387324;
  const double b = .0456512608815524;
  const double p0 = .333333333333333;
  const double p1 = -.224696413112536;
  const double p2 = .00620886815375787;
  const double q1 = -1.27408923933623;
  const double q2 = .354508718369557;
  double ret_val;
  double r, t, u, w, w1;
  if (x < .61 || x > 1.57)
    {
      r = x - .5 - .5;
      ret_val = r - log (x);
      return ret_val;
    }
  if (x < .82)
    {
      u = x - .7;
      u /= .7;
      w1 = a - u * .3;
    }
  else if (x > 1.18)
    {
      u = x * .75 - 1.;
      w1 = b + u / 3.;
    }
  else 
    {
      /*   argument reduction */
      u = x - .5 - .5;
      w1 = 0.;
    }
  /*       series expansion */
  r = u / (u + 2.);
  t = r * r;
  w = ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.);
  ret_val = t * 2. * (1. / (1. - r) - r * w) + w1;
  return ret_val;
}	

/*
 * Maple code 

 # the approximation of rlog1(x) x - log(1+x) 
 
 with(numapprox);
 with(orthopoly);

  f_approx1:= proc(x,res) local u,u2,res1;  u := x/(2.0+x);u2:=u*u;
    res1 :=u*((-0.1006397968649471E1
	    +(0.6784309300435146
	      -0.187491012609664E-1*u2)*u2)
	   /(0.1006397968649471E1
	     +(-0.128226971123169E1
	       +0.3567975116629138*u2)*u2));
    res + 2*u2*( 1/(1-u) + res1/3);
  end proc;

  rlog1 := proc(x) local y;
    if ( x >= - 0.39 and x <= - 0.18) then 
      y:= f_approx1((x + .3)/0.7,-.42857142857142857*x -.07189648463269617);
    elif ( x > -0.18 and x < 0.18 ) then  y:= f_approx1(x,0.0);
    elif ( x >= 0.18 and x <= 0.57 ) then  y:= f_approx1(0.75*x -0.25, 0.25*x -.03768207245178093);
    else y:= x-log(1+x);
    end if ;
    y ;
  end proc;

  f:= proc(x) x-1-log(x); end proc;
  f_approx:= proc(x) rlog1(x-1); end proc ;
  
  f_err:=proc(x) local u1,u2; u1:=evalf(f(x),70); u2:=evalf(f_approx(x),17);
    evalf((u1-u2)/u1,70);
  end proc;
  
  m_err:=proc(am,ap,b,nn)
    mvm:=am*nn;mvp:=ap*nn;mvd:= b*nn;
    s:=[seq(1+ (i/mvd),i=mvm..-1),seq(1+ (i/mvd),i=1..mvp)];
    serr:= map(f_err,s);
    convert(max(op(serr)),float);
  end proc;

  m_plot_err:=proc(am,ap,b,nn)
    mvm:=am*nn;mvp:=ap*nn;mvd:= b*nn;
    s:=[seq(1+ (i/mvd),i=mvm..-1),seq(1+ (i/mvd),i=1..mvp)];
    ff:= proc(x); [x,f_err(x)];end proc;
    serr:= map(ff,s);
    plot(serr);
  end proc;
  
  m_err(-40,58,100,1);
  m_plot_err(-40,58,100,1);
  

*/

/* Maxima code 
 * we use rlog1 

   f_approx1(x,res):= (u : x/(2.0+x),u2:u*u,
    res1 :u*((-0.1006397968649471E1
	    +(0.6784309300435146
	      -0.187491012609664E-1*u2)*u2)
	   /(0.1006397968649471E1
	     +(-0.128226971123169E1
	       +0.3567975116629138*u2)*u2)),
    res + 2*u2*( 1/(1-u) + res1/3));

   f_approx2(x):= (0.285211335E-10+(0.107106064E-8+(0.1152628215700186E1+(
  0.224530290626052E1+(0.1355646781758151E1+0.245173096410477*x)*x)*x)*x)*x)/(
  0.2305256395750588E1+(0.6027443064646071E1+(0.557695860354239E1+(
  0.2116690842297106E1+(0.265182661285532-0.1316156876141345E-2*x)*x)*x)*x)*x);

  rlog1(x):=  if ( x >= - 0.39 and x <= - 0.18) then f_approx2(x)
   else ( if ( x > -0.18 and x < 0.18 ) then  f_approx1(x,0.0)
   else (if ( x >= 0.18 and x <= 0.57 ) then  f_approx1(0.75*x -0.25, 0.25*x -.03768207245178093)
   else x-log(1+x)));

  # compute rlog 

  f(x):= x-1-log(x);
  f_approx(x) := if ( x <= -0.39 ) then x-1-log(x) else rlog1(x-1);

  fpprec:100;
  float2bf: true;

  points(xmin,xmax,xdiv,nn,xe):= ( L: makelist(x/(xdiv*nn),x,xmin*nn,xmax*nn), 
                                   L: sublist(L,lambda([x],if x=xe then false else true)))$
  
  testprec(L,f,fa):= (fpprec:100,bfT: bfloat(map(f,L)), ft: ev(map(fa,L),numer),
                       Lrerr:map(lambda([x,y],(x-y)/x),bfT,ft),
		       plot2d([discrete,ev(L,numer),ev(Lrerr,numer)]),
		       ev(lmax(Lrerr),numer))$

  L: points(-40+100,50+100,100,10,1)$
  print("without rational approximation")$
  testprec(L,f,f); 
  print("with rational approximation")$
  testprec(L,f,f_approx);

  # original code 

  a : .0566749439387324;
  b : .0456512608815524;
  p0 : .333333333333333;
  p1 : -.224696413112536;
  p2 : .00620886815375787;
  q1 : -1.27408923933623;
  q2 : .354508718369557;
  
  f_approx(u,w1):= (  r : u / (u + 2.),  t : r * r, 
  w : ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.), 
  t * 2. * (1. / (1. - r) - r * w) + w1)$

  rlog(x):=  if (x < .61 or  x > 1.57) then x-1-log(x) 
  else if (x < .82) then (u: (x -0.7)/0.7, f_approx(u,a - u * .3))
  else if (x > 1.18) then (u:  x * .75 - 1., f_approx(u, b + u / 3.))
  else (u: x - .5 - .5, f_approx(u,0))$

  
  print("with rational approximation")$
  testprec(L,f,rlog);

*/


