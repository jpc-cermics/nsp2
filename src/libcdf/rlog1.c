/* Nsp
 * Copyright (C) 2007-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * a version of x - ln(1 + x) approximated with Mapple 
 * 
 *
 *
 */

#include "cdf.h"

/**
 * cdf_rlog1:
 * @x: a pointer to a double 
 * 
 * evaluation of the function x - ln(1 + x) 
 * Returns: a double 
 **/

double cdf_rlog1(double x)
{
  if ( x >= -0.50 && x <= 0.9 )
    {
      /* pade approximation computed with maxima */
      return x*x*((((((((((105. *x + 900900.)*x 
			  + 35493744.)*x  + 497522740.)*x 
			+ 3465717255.)*x  + 13740887160.)*x 
		      + 32991198240.)*x + 48839879088.)*x 
		    + 43561307790.)*x   + 21455714280.)*x + 4481256780.)/ 
	((((((((((914760. *x + 39639600.)*x 
		 + 624323700.)*x  + 4994589600.)*x  
	       + 23308084800.)*x + 67926418560.)*x  
	     + 127362034800.)*x + 153643089600.)*x  
	   + 115232317200.)*x  + 48886437600.)*x + 8962513560.); 
    }
  else if ( x > 0.9 && x < 1.5 ) 
    {
      /* approximated with maple */
      return((-0.1747079958949E-7
	      +(0.20815559100658E-6
		+(0.9012473837984834E-1
		  +(0.1109076338388868
		    +(0.3634606137578365E-1
		      +0.2899502173890215E-2*x)*x)*x)*x)*x)/
	     (0.1802518394637529
	      +(0.341974601285795
		+(0.2105717106006427
		  +(0.4724766963520003E-1
		    +(0.2995915689046589E-2-0.1427995963366621E-5*x)*x)*x)*x)*x));
    }
  else if ( x >= 1.5 && x <= 2 ) 
    {
      /* approximated with maple */
      return((-0.18389570578276E-6
	      +(0.15859762096169E-5
		+(0.6070826729167312E-1
		  +(0.6605777291645029E-1
		    +(0.184083392966051E-1
		      +0.1202937057927628E-2*x)*x)*x)*x)*x)/
	     (0.121429830283389
	      +(0.2130323063153602
		+(0.1181978078727743
		  +(0.2313732341347625E-1
		    +(0.1235445243451395E-2-0.3726321785441992E-6*x)*x)*x)*x)*x));
    }
  else 
    {
      return x- log (1+x) ;
    }
}


/**
 * cdf_rlog:
 * @x: a pointer to a double 
 * 
 * evaluation of the function  x - 1 - ln(x)
 * 
 * Returns: a double 
 **/

double cdf_rlog (double x)
{
  if ( x > 1.6 || x <= 0.3) 
    {
      return  x - 1 - log (x);
    }
  if (x > 0.3 && x < 0.5)
    {
      /* maple pade approximation in [0.3,0.5] */
      return (.0051541577362501057115363479649
	      + (.239504488200645717699290858091
		 + ( 1.66077494260106229915584508067 
		     + (.54149163480695597100613524352
			+ (-4.95236463480462082958384240165 
			   + (.40869829393292117880713311482 
			      + 2.09674187790033243657888509757*x)*x)*x)*x)*x)*x) 
	/
	(.001068613962639160819177520631 
	 + (.09626149128499055042895935763 
	    + (1.48109665384134686189927994934
	       + (6.3011992379012529948753686335 
		  + (7.98306213555752679054641618271 
		     + (2.37482785308857877722085691150 
			- .0193827615389298708205129340040*x)*x)*x)*x)*x)*x);
    }
  return cdf_rlog1(x-1);
}	


/* 
 * Using Maple code for approximation 
 * giving an approximation of x - log(1+x) in [-0.18,0.18].
 * 
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) x- log(1+x);end proc;
  g:=proc(x) (f(x)-f(-x))/(2*x^3/3);end proc;
  Digits:=50;
  ggp:=chebpade(g(x),x=-0.1..0.1,[8,8]);
  Digits:=17;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  g_cheb:= unapply(gg,x);
  f_approx:= proc(x) local u; u:= x/(2+x); 2*u^2*(1/(1-u) + u*g_cheb(u)/3); end proc;
  infnorm(f(x)-f_approx(x),x=-0.18..0.18);
  codegen[C](f_approx,optimized);
  codegen[C](g_cheb,optimized);
  codegen[C](confracform(u*subs(x=u,gg)));
  
  f_err:=proc(x) local u1,u2; u1:=evalf(f(x),70); u2:=evalf(f_approx(x),16);
    evalf((u1-u2)/u1,70);
  end proc;
  
  m_err:=proc(am,ap,b,nn) local mvm,mvp,mvd,s,serr;
    mvm:=am*nn;mvp:=ap*nn;mvd:= b*nn;
    s:=[seq(i/mvd,i=mvm..-1),seq(i/mvd,i=1..mvp)];
    serr:= map(f_err,s);
    convert(max(op(serr)),float);
  end proc;

  am:=-18;ap:=18;ad:=100;
  m_err(am,ap,ad,100);

  # in [-0.18,0.18]

  f_approx1:= proc(x,res) local u,u2,res1;  u := x/(2.0+x);u2:=u*u;
    res1 :=u*((-0.1006397968649471E1
	    +(0.6784309300435146
	      -0.187491012609664E-1*u2)*u2)
	   /(0.1006397968649471E1
	     +(-0.128226971123169E1
	       +0.3567975116629138*u2)*u2));
    res + 2*u2*( 1/(1-u) + res1/3);
  end proc;

  # in [-0.4,-0.18] 
  # better than 
  # y:= f_approx1((x + .3)/0.7,-.42857142857142857*x -.07189648463269617);

  f_approx2:=  proc(x) (0.285211335E-10+(0.107106064E-8+(0.1152628215700186E1+(
  0.224530290626052E1+(0.1355646781758151E1+0.245173096410477*x)*x)*x)*x)*x)/(
  0.2305256395750588E1+(0.6027443064646071E1+(0.557695860354239E1+(
  0.2116690842297106E1+(0.265182661285532-0.1316156876141345E-2*x)*x)*x)*x)*x); end proc;

  # full approximation in [-0.39,0.57]
  
  f_approx := proc(x) local y;
    if ( x >= - 0.39 and x <= - 0.18) then y:=f_approx2(x);
    elif ( x > -0.18 and x < 0.18 ) then  y:= f_approx1(x,0.0);
    elif ( x >= 0.18 and x <= 0.57 ) then  y:= f_approx1(0.75*x -0.25, 0.25*x -.03768207245178093);
    else y:= x -log(1+x);
    end if ;
    y ;
  end proc;

  m_err(-40,58,100,10);

*/

/* Some maxima code that can be used for testing and ploting 

   f(x):= x- log(1+x);
   fpprec:100;
   float2bf: true;
   
   f_approx1(x):= x*x*((((((((((105. *x + 900900.)*x + 35493744.)*x  + 497522740.)*x + 3465717255.)*x  + 13740887160.)*x + 32991198240.)*x + 48839879088.)*x + 43561307790.)*x   + 21455714280.)*x + 4481256780.)/ ((((((((((914760. *x + 39639600.)*x  + 624323700.)*x  + 4994589600.)*x  + 23308084800.)*x + 67926418560.)*x   + 127362034800.)*x + 153643089600.)*x    + 115232317200.)*x  + 48886437600.)*x + 8962513560.); 

   points(xmin,xmax,xdiv,nn,xe):= ( L: makelist(x/(xdiv*nn),x,xmin*nn,xmax*nn), 
                                    L: sublist(L,lambda([x],if x=xe then false else true)))$

    testprec(L,f,fa):= (fpprec:100,bfT: bfloat(map(f,L)), ft: ev(map(fa,L),numer), Lrerr:map(lambda([x,y],(x-y)/x),bfT,ft), plot2d([discrete,ev(L,numer),ev(Lrerr,numer)]),ev(lmax(Lrerr),numer))$

   L: points(-40,50,100,10,0)$
   print("without rational approximation")$
   testprec(L,f,f); 
   print("with rational approximation")$
   testprec(L,f,rlog1);

*/
