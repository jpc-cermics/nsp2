
function [g,p,q]=cofactors(u,v, td, iter = %t , ftol = 1.e-14 ) 
// Copyright  2010 Jean-Philippe Chancelier 
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
// compute cofactors using linear algebra 
// td is a gcd degree guess. iterative refinement 
// is performed if iter=%t. ftol is used by iterative refinement.
// 
  n=u.degree[];
  m=v.degree[];
  uc= u.coeffs{1};
  vc= v.coeffs{1};
  Ms= [ toeplitz([uc,zeros(1,m- td)],[uc(1),zeros(1,m- td)]), ...
        toeplitz([vc,zeros(1,n- td)],[vc(1),zeros(1,n- td)])];
  xx=[Ms;ones(1,n+m-2*(td-1))] \ [zeros(m+n-(td-1),1);1];
  q=- m2p(xx(1:m-(td-1)));
  p=  m2p(xx(m-(td-1)+1:$));
  if iter then 
    [g,p,q]=cofactors_iter(u,v,p,q,ftol=ftol);
  else
    g=epdiv_lsq(u,p);
  end
endfunction

function [g,p,q]=cofactors_iter(u,v,p,q, ftol = 1.e-14)
// Copyright  2010 Jean-Philippe Chancelier 
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
// 
// find a solution of 
//  p*g -u = 0
//  q*g -v = 0 
// this function can be used after cofactors to 
// improve a solution by fsolve iterations 
    
  function y=f(x) 
  //  p*g -u = 0
  //  q*g -v = 0 
  //  x=[ g;p;q] 
    gc = x(1:r+1);
    pc = x(r+2:r+2+n);
    qc = x(r+2+n+1:r+2+n+1+m);
    Cp = toeplitz([pc;zeros(r,1)],[pc(1),zeros(1,r)]);
    Cq = toeplitz([qc;zeros(r,1)],[qc(1),zeros(1,r)]);
    y=[Cp* gc;Cq*gc] - [ uc;vc];
  endfunction

  function J=jac(x) 
  // jacobian of function f 
    gc = x(1:r+1);
    pc = x(r+2:r+2+n);
    qc = x(r+2+n+1:r+2+n+1+m);
    // The convolution matrix associated to p (dim g)
    Cp = toeplitz([pc;zeros(r,1)],[pc(1),zeros(1,r)]);
    // The convolution matrix associated to q (dim g)
    Cq = toeplitz([qc;zeros(r,1)],[qc(1),zeros(1,r)]);
    // The convolution matrix associated to g (dim p)
    Cg1 = toeplitz([gc;zeros(n,1)],[gc(1),zeros(1,n)]);
    // The convolution matrix associated to g (dim q)
    Cg2 = toeplitz([gc;zeros(m,1)],[gc(1),zeros(1,m)]);
    J = [Cp,Cg1,zeros(r+n+1 ,m+1)
	 Cq,zeros(r+m+1,n+1),Cg2];
  endfunction

  n=p.degree[];
  m=q.degree[];
  pc= p.coeffs{1}.';
  qc= q.coeffs{1}.';
  // estimate g 
  g=epdiv_fft(u,p);
  gc = g.coeffs{1}.';
  r=g.degree[];
  uc = u.coeffs{1}.';
  vc = v.coeffs{1}.';
  mm=size(uc,'*')+size(vc,'*');
  
  x0 = [gc;pc;qc];
  [xf,ff]=fsolve_lsq(x0,f,mm,jac=jac,ftol=ftol,warn=%f);

  g = m2p(xf(1:r+1));
  p = m2p(xf(r+2:r+2+n));
  q = m2p(xf(r+2+n+1:r+2+n+1+m));
endfunction

if %f then 
  x=poly(0);
  u=(1+x+x^3+x^4)*(1+6*x);
  v=(1+x+x^3+x^4)*(1+7*x);
  k=4; // tentative degree 
  [g,p,q]=cofactors(u,v,k);
  [norm( g*p -u ), norm(g*q -v )]
end


