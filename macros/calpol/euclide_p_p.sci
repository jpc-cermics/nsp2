
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

function [p,u]=euclide_p_p(a,b,eps=100*%eps,monic=%f)
// 
// 
  r1 =[ m2p(1),m2p(0)];
  r2 =[ m2p(0),m2p(1)];
  while %t  then 
    if norm(b,%inf) < eps then ; break;end 
    if b.degree[]== 0 && a.degree[]==0 then ; break;end 
    d=det([r1;r2]);
    d0=d.coeffs{1};
    if norm(d-d0(1)) > eps then 
      // we went one step too far 
      r2=r1;r1=rs; b=a;a=as;break;
    end
    [q,r]=pdiv(a,b);
    rs = r1;
    as = a;
    r1 = r2;
    r2 = rs - q.*r2;
    a = b;
    b = r ;
    printf("nouveau a et b et det\n")
    print(a);
    print(b);
    d=det([r1;r2]);
    d0=d.coeffs{1};
    print(norm(d -d0(1) ));
    //abs(abs(det(horner([r1;r2],10*%eps,ttmode=%t)))-1)
  end
  p = a;
  u = [r1;r2]';
  if monic then 
    cm = p.coeffs{1};
    p = (1/cm($))*p;
    u = (1/cm($))*u;
  end
endfunction

if %f then 
  a=m2p([1,4,6,4,1]);//(1+x)^4
  b=m2p([0,0,1,1]); // (1+x)*x^2

  [xx,u]=euclide(a,b);
  norm([a,b]*u(:,1) -xx );
  ablcm = a*u(1,2);
  x=m2p([0,1]);
  ablcm.normalize[];
  norm(ablcm - (1+x)^4*x^2);
    
  pp = [m2p([0,1,4,6,4,1]); // 
	m2p([0,0,0,1, 3,3,1]);
	m2p([0,0,0,1, 2,1]);
	m2p([0,0,0,1,2,1]);
	m2p([0,0,0,0,1,2,1]);
	m2p([0,0,0,0,1,1]);
	m2p([0,0,0,1,1])];
  
  x=poly(0);
  pp1= [ x*(1+x)^4;
	 x^3*(1+x)^3;
	 x^3*(1+x)^2;
	 x^3*(1+x)^2;
	 x^4*(1+x)^2
	 x^4*(1+x);
	 x^3*(1+x)];
  
  xx = pp(1);
  for i=1:size(pp,'*'); xx=euclide(xx,pp(i),monic=%t);  end

  xx = pp1(1);
  for i=1:size(pp,'*'); xx=euclide(xx,pp1(i),monic=%t);  end
  
  qq = [m2p([-12,0,3]),m2p([-12,0,3]),m2p([0,-12,0,3])];
  qq = [3*(x^2-4);3*x*(x^2-4)];
  y = qq(1);
  for i=1:size(qq,'*'); y=euclide(y,qq(i),monic=%t);  end
  
  // ce qui suit donne un resultat faux 
  
  p=m2p([1,1]);
  q=m2p([0,1,1]);
  a=p*p*m2p([45,7]);
  b=q*q;
  [a1,b1]=euclide(a,b,monic=%t);
  
  // mais juste si on demande moins de précision 
  
  [a1,b1]=euclide(a,b,eps=1000*%eps,monic=%t);
  
end 

