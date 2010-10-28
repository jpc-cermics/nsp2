function [T_pa]=taylor(p,a)
// Copyright  2010 Francois Delebecque
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
// Taylor coefficients of polynomial p(x) at x=a.
// Note that a can be a vector 
// T_pa is a matrix of size size(a,'*')x(n+1) 
// 
// p = polynomial (n=degree()); a=constant (real or complex)
// T_pa = [a_0,a_1,...,a_(n-1),a_n] = Taylor coefficients of p at a.
// p(x) = a_0+a_1*(x-a)+a_1*(x-a)^2+...+a_n*(x-a)^n
// 
  a=a(:);
  curd=p;
  T_pa=horner(p,a){1};
  for k=1:p.degree[]
    curd= curd.derivative[];
    T_pa=[T_pa,horner(curd,a){1}/prod(1:k)];
  end
endfunction

if %f then 
  function ok=check_taylor(p,a)
    [T]= taylor(p,a);
    q=m2p(T);
    pc = compose(q,m2p([-a,1]));
    ok=  p == pc 
  endfunction;
  
  p= m2p(rand(1,5));  a=4;
  check_taylor(p,a,taylor(p,a)); 
  a=4+2*%i;
  check_taylor(p,a,taylor(p,a)); 
  p= m2p(rand(1,5)+%i*rand(1,5));a=4;
  check_taylor(p,a,taylor(p,a));
end

function [T_pdivq]=rtaylor(p,q,a,n)
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
// Taylor of a rational fraction;
// n is the taylor order requested 
// n is set to max(n,max(degree(p),degree(q)));
  T_p = taylor(p,a); np = size(T_p,'*');
  T_q = taylor(q,a); nq = size(T_q,'*');
  n = max(1,n);
  // The convolution matrix associated to q*r (with degree(r)=n)
  Cq = toeplitz([T_q,zeros(1,n)],[T_q(1),zeros(1,n-1)]);
  Cq = Cq(1:n,:);
  b = [T_p(1:min(np,n)), zeros(1,max(n-np,0))];
  T_pdivq = Cq\b';
endfunction

if %f then 
  p=m2p(1:5);
  q=m2p(1:4);
  a=0;
  n=7;
  T_pdiv = rtaylor(p,q,a,n);
  r= m2p(T_pdiv);
  rx = compose(r, m2p([-a,1]));
  p - q*rx;
  rr=  p - q*rx
  norm(rr.coeffs{1}(1:n))
end

function [res,L_c,T,p_a]=residue(p,q,a,tol=%eps)
// Copyright  2010 Francois Delebecque
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
// returned values:
// res: Residue  of p(x)/q(x) at x=a 
// L_c= Laurent coefficients. 
// tol (optional) small tolerance for detecting zeros of q(x) close to a
// 
// the last component of the linear system
// T*L_c = p_a  where T is a Toeplitz, lower triangular matrix : res=x($);
// The entries of Lc are the Laurent coefficients of the principal part
// of p(x)/q(x) at x=a.
// If k = length(L_c) one has p(x)/q(x) = L_c(1)/(x-a)^k +...+ L_c(k)/(x-a)+...
// near x=a;
// Example: x=poly(0,'x');p=(x-3)*(x+5);
// q=((x-1)^3)*(x^2-5*x+2); [res,Lc,T,p_a]=residue(p,q,1); 
// // expected value res=19;
// p=(x-3)*(x+5)*(x-2);q=((x-1)^4)*(x^9-5*x+2);[res,Lc,T,p_a]=residue(p,q,1) 
// // res=-559.5 
  
  if type(p,"string") == "Mat" then p=p+m2p(0);end
  if type(q,"string") == "Mat" then q=q+m2p(0);end

  // Fast return: a is not a zero of q.
  if abs(horner(q,a){1}) > sqrt(tol) then
    res=0;T=[];p_a=[];L_c=[];return;
  end

  p_a=taylor(p,a)(:);

  wtmp=roots(q);

  // k = estimated multiplicity of a
  for K=1:20
    [pp,qq]=find(abs(wtmp-a) < tol);
    k=length(pp);
    if k==0 then tol=(tol)^(3/4);else break;end
  end
  // taylor expansion of q for each zero of 
  // q close to a and then use the mean as 
  // taylor expansion of q at a.
  q_=mean(taylor(q,wtmp(pp)),1);
  //q_=taylor(q,a);  more accurate if a is known exactly
  
  //q_ has length degree(q)+1 and k first entries are (almost) zero.
  q_(1:k)=[];
  // q_ are now the coefficient of the taylor expansion of 
  // q1 at x=a where  q = (x-a)^k q1 
  // 
  // we need now to compute the taylor expansion of 
  // p/q1 at x=a up to (x-a)^(k-1)
  // we follow rtaylor but do not exactly use 
  // rtaylor since q1=m2p(q_) is already  the coeficients 
  // os taylor expansion of q1 ( q = (x-a)^k q1).
  // [T_pdivq]=rtaylor(p,q1,a,k);
  T_p = taylor(p,a); np = size(T_p,'*');
  T_q = q_ ; nq = size(T_q,'*');
  n = max(1,k);
  // The convolution matrix associated to q*r (with degree(r)=n)
  T = toeplitz([T_q,zeros(1,n)],[T_q(1),zeros(1,n-1)]);
  T = T(1:n,:);
  p_a = [T_p(1:min(np,n)), zeros(1,max(n-np,0))]';
  L_c = solve(T,p_a,mode="lo");
  res=L_c(k);
endfunction

if %f then 
  x=poly(0,'x');
  p=(x-3)*(x+5);
  q=((x-1)^3)*(x^2-5*x+2); 
  [res]=residue(p,q,1)   //res=19;
  p=(x-3)*(x+5)*(x-2);
  q=((x-1)^4)*(x^9-5*x+2);
  [res]=residue(p,q,1) //  (res=-559.5)
end

function dg=degree_m(m)
// is it really usefull !
  dg = degree(m2p(m));
endfunction



