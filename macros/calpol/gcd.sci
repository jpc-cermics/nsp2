function [z,f1,f2,res] = gcd(u,v, delta=1.e-9)
// Copyright  2010 Paola Boito 
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Adapted and revisited for Nsp (Jean-Philippe Chancelier 2010)
// This is a modified version of fast_gcd_wls.m but without the fast 
// displacement algebra tools. 
//
// - performs LU on the Sylvester matrix associated with
//   f and g and determines a tentative GCD degree;
//
// - solve the linear system that gives the cofactors using 
//   the tentative degree and finds the cofactors; 
// - performs iterative refinement;
// - repeats the above computation with an increased or
//   decreased gcd degree, depending on whether the
//   last residual found is smaller or greater than delta.
//

  debug = %t
  
  if nargin <= 2 then delta = 1.e-9;end 
  // tolerance for iterative refinement 
  ftol = 1.e-14; 
  // compute degrees of input polynomials
  n=u.degree[];//length(f)-1;
  m=v.degree[];// length(g)-1;
  N=m+n;
  // normalize f and g
  f=u/norm(u);
  g=v/norm(v);
  
  if n==0 then 
    z= u; f1=m2p(1); f2=v;res =[0,0];
    return;
  end
  if m==0 then 
    z= v; f1=u; f2 = m2p(1);res=[0;0];
    return;
  end
      
  // introduce a heuristic correction on delta
  delta_c = max(delta,1.e-9);
  
  // factorize the Sylvester matrix S
  // and find a tentative degree tdeg
  [Pr,P2r,L,U]=lu_sylvester(f,g);
  dU=abs(diag(U));
  
  eta=delta_c*sqrt(N);
  k = max(find([eta;dU] >= eta))-1;
  tdeg=N-k;
  
  // handle special or forbidden values for tdeg
  tdeg = min(max(tdeg,0),m);
  
  if debug then printf("tested degrees: %2d ",tdeg);end 
  
  // compute cofactors and 
  // perform iterative refinement
  [z,f1,f2]=cofactors(f,g,tdeg, ftol = ftol)
  res = [norm(f- f1*z) norm(g - f2*z)];

  if and( res < [delta,delta])
    // try to increase degree
    if debug then printf(" (+) ");tref=tdeg;end 
    while %t 
      tdeg=tdeg+1;
      if tdeg > min(m,n) then 
	if debug then 
	  printf(" stop with %d (%d) norm=(%g,%g)\n", tdeg-1,tdeg-1-tref, res(1),res(2));
	end
	break;
      end 
      [newz,f1,f2]=cofactors(f,g,tdeg, ftol = ftol);
      newres = [norm(f- f1*z) norm(g - f2*z)];
      if ((newres(1)>delta)||(newres(2)>delta)) then 
	if debug then 
	  printf(" stop %d (%d) norm=(%g,%g)\n", tdeg-1,tdeg-1-tref, res(1),res(2));
	end 
	break;
      end
      z= newz;
      res = newres;
    end
  else
    if debug then printf(" (-) ");  tref=tdeg;end 
    while ((res(1)>delta)||(res(2)>delta))
      tdeg=tdeg-1;
      if tdeg < 0 then tdeg=0; break;end 
      [z,f1,f2]=cofactors(f,g,tdeg, ftol = ftol);
      res = [norm(f- f1*z) norm(g - f2*z)];
    end
    if debug then printf(" stop %d (%d) norm=(%g,%g)\n",tdeg,tdeg-tref,res(1),res(2));end
  end
endfunction

function gcd_test(delta=1.e-9)
// test gcd computations 
// more tests 
  x= poly(0);
  p1=(1+x);  p2=(2+x);  p3=(3+x);  p4=(x);
  n=100 ; T= ones(1,n) > 0; N= zeros(1,n);
  for i=1:n
    cp=grand(1,4,'uin',0,5);
    p= p1^cp(1)*p2^cp(2)*p3^cp(3)*p4^cp(4);
    cq=grand(1,4,'uin',0,5);
    q= p1^cq(1)*p2^cq(2)*p3^cq(3)*p4^cq(4);
    cpq = min(cq,cp);
    gcpq1 = p1^cpq(1)*p2^cpq(2)*p3^cpq(3)*p4^cpq(4);
    [g,ppr,qqr,res] = gcd(p,q,delta=delta);
    g.normalize[];
    N(i)= norm(g -gcpq1)/(1+g.degree[]);
    if N(i) > 1.e-7 then 
      T(i)=%f;
      pause;
    end 
  end
  xclear();
  Nok = size(find(T),'*');
  plot2d(1:n,log(N)/log(10))
  xtitle(sprintf('Number of correct tests %d/%d',Nok,n));
endfunction 

if %f then 
  // Paola Boito example 8.1.1
  x= poly(0);
  r = ((-1).^(1:10)).*(1:10)/2;
  u =poly(r,'x',roots=%t);
  r = r - 10.^(-(1:10));
  v =poly(r,'x',roots=%t);
  for i=1:12 
    [g,p,q,res] = gcd_jpc(u,v,delta=10^(-i));
    dr(1,i)=i;
    dr(2,i)=g.degree[];
    dr(3,i)=norm(res,2);
  end

  x= poly(0);
  p1=(1+x);  p2=(2+x);  p3=(3+x);  p4=(x);
  cp=[0,2,5,1];
  cq=[1,0,0,0];
  p= p1^cp(1)*p2^cp(2)*p3^cp(3)*p4^cp(4);
  q= p1^cq(1)*p2^cq(2)*p3^cq(3)*p4^cq(4);
  cpq = min(cq,cp);
  gcpq1 = p1^cpq(1)*p2^cpq(2)*p3^cpq(3)*p4^cpq(4);
  [g,ppr,qqr,res] = gcd(p,q,delta=1.e-9);
  g.normalize[];
  norm(g -gcpq1)/(1+g.degree[])
  
  
end

