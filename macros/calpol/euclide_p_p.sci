
function [r,q]=monodiv_p(a,alpha)
// Copyright  2010 Jean-Philippe Chancelier Cermics/Enpc 
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
// divide a by (x-alpha). faster than pdiv and 
// could be accelerated by the nsp band solver.
//  
  ca = a.coeffs{1};
  na=size(ca,'*');
  b(na-1)=ca(na);
  for i=na-2:-1:1
    b(i) = alpha*b(i+1) + ca(i+1);
  end
  gamma= ca(1)+alpha*b(1)
  q=m2p(b);
  r= gamma;
endfunction

function [q,r]=pdiv_soft_p_p(a,b) 
// Copyright  2010 Jean-Philippe Chancelier Cermics/Enpc 
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
// polynomial division a=q*b + r
// a and b are two polynoms 
// should be extended to term to term 
  
  q=[];
  cb = b.coeffs{1};
  nb=size(cb,'*');
  while %t 
    ca = a.coeffs{1};
    na=size(ca,'*');
    if (na < nb ) then r = a; break;end
    c = ca($)/cb($) 
    q(na-nb+1) =c;
    b1 = c*b;
    b1.shift[na-nb];
    a = a-b1;
    if a.degree[]==b1.degree[] then 
      // need to set to zero the leading element 
      ca = a.coeffs{1};
      if size(ca,'*')== 1 then r=a;break;end 
      ca($)=0;
      a = m2p(ca);
    end
  end
  if isempty(q) then q=0;end 
  q=m2p(q);
endfunction

function [g,fact]=euclide_p_p(a,b,eps=10*%eps,monic=%f)
// epsilon euclid method 
// See Paola Boito Thesis Chap 3 or 
// original paper of Hribernig and Stetter.
// Detection and validation of clusters of 
// Polynomial Zeros (J. Symbolic Computation (1997) 24 667-682.
  
  da= a.degree[];
  db= b.degree[];
  if da >= db then
    f1 = a; f2= b; ind=[1,2];
  else
    f1 = b; f2= a; ind=[2,1];
  end
  M= [ m2p(1),m2p(0);
       m2p(0),m2p(1)];
  v= [ f1;f2];
  vp = [f1;f2];
  vpp = [f1;f2];
  // we have [f1;f2]= M*v 
  // and it should be true during iterations
  while %t then 
    // Compute the matrix such that 
    // [f1,f2]' = M [fj, fjp1]' 
    // note that M is unimodular 
    [q,r]=pdiv(v(1),v(2));
    M = [ q*M(1,1)+ M(1,2), M(1,1); 
	  q*M(2,1)+ M(2,2), M(2,1)];
    v  = [ v(2); r];
    // first way to stop 
    [q1,r1]=pdiv(f1,v(1));
    [q2,r2]=pdiv(f2,v(1));
    if norm(r1,1) < eps && norm(r2,1) < eps then 
      break;
    end
    // [f1;f2]= M*v is an 
    if norm(M(1,2)*v(2),1) < eps && norm(M(2,2)*v(2),1) < eps then 
      break;
    end
    if v(1).degree[]== 0 && v(2).degree[]== 0 then ;
      break;
    end 
  end
  // when we break v(1) is the gcd and 
  // [f1;f2]= M(:,1) *v(1) 
  g=v(1);
  fact = M(ind,1);
endfunction

if %f then 
  a=m2p([1,4,6,4,1]);//(1+x)^4
  b=m2p([0,0,1,1]); // (1+x)*x^2

  [xx,f]=euclide(a,b);
  norm( f*xx -[a;b])
  ablcm = f(1)*f(2)*xx
  x=m2p([0,1]);
  ablcm.normalize[];
  norm(ablcm - (1+x)^4*x^2)
      
  x=poly(0);
  pp1= [ x*(1+x)^4;
	 x^3*(1+x)^3;
	 x^3*(1+x)^2;
	 x^3*(1+x)^2;
	 x^4*(1+x)^2
	 x^4*(1+x);
	 x^3*(1+x)];
  
  xx = pp1(1);
  for i=1:size(pp1,'*'); xx=euclide(xx,pp1(i),monic=%t);  end
  xx.normalize[];
  norm( xx - x*(1+x))
    
  qq = [m2p([-12,0,3]),m2p([-12,0,3]),m2p([0,-12,0,3])];
  qq = [3*(x^2-4);3*x*(x^2-4)];
  y = qq(1);
  for i=1:size(qq,'*'); y=euclide(y,qq(i),monic=%t);  end
    
  // more tests 
  x= poly(0);
  p1=(1+x);
  p2=(2+x);
  p3=(3+x);
  p4=(x);
  T= ones(1,100) > 0;
  for i=1:100
    cp=grand(1,4,'uin',0,5);
    p= p1^cp(1)*p2^cp(2)*p3^cp(3)*p4^cp(4);
    cq=grand(1,4,'uin',0,5);
    q= p1^cq(1)*p2^cq(2)*p3^cq(3)*p4^cq(4);
    eps= 100*%eps;
    [gcpq,b1]=euclide(p/norm(p,1),q/norm(q,1),eps=eps);
    // [gcpq]=gcd_qr(p/norm(p,1),q/norm(q,1),eps=eps);
    cpq = min(cq,cp);
    gcpq.normalize[];
    gcpq1 = p1^cpq(1)*p2^cpq(2)*p3^cpq(3)*p4^cpq(4);
    if norm(gcpq1 -gcpq) > 1000*eps then 
      T(i)=%f; 
    end 
  end
  size(find(T),'*')

  x= poly(0);
  r= 1:10;
  r= -5:4;
  n= 10000;
  veps= 10.^(-[0:14]);
  for k=1:size(veps,'*')
    T= ones(1,n);
    for i=1:n 
      cp=grand(1,10,'uin',0,1);
      p = m2p(1); for j=1:10 ; p= p*(r(j)+x)^cp(j);end 
      cq=grand(1,10,'uin',0,1);
      q = m2p(1); for j=1:10 ; q= q*(r(j)+x)^cq(j);end 
      eps = veps(k);
      [gcpq,b1]=euclide(p/norm(p,1),q/norm(q,1),eps=eps);
      cpq = min(cq,cp);
      pq = m2p(1); for j=1:10 ; pq= pq*(r(j)+x)^cpq(j);end 
      gcpq1 = pq;
      gcpq.normalize[];
      T(i)=norm(gcpq1 -gcpq,1);
    end
    Ts=sort(T,'g','i');
    res(k)=Ts(8000);
  end
end 

