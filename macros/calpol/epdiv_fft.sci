
function u=epdiv_fft(p,q) 
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
// 
// Adapted and revisited for Nsp (Jean-Philippe Chancelier 2010)
// 
// Computes polynomial division using the FFT
// (that is, with an evaluation/interpolation technique)
// u=epdiv_fft(f,g)  where u=f/g.
// 
// Notice that:
// 1) f must be exactly divisible by g;
// 2) if a root of 1 is also a root of g, the function
//    attempts scaling of f and g and division with
//    evaluation/iterpolation;
// 3) if even this attmpt fails, a least square method is used 
//
// In the original code in case 3 a GKO algorithm was used 
// here we just use standard linear algebra.

  tol=1e-8;
  f= p.coeffs{1};
  g= q.coeffs{1};
  n=length(f);
  m=length(g);
  k=n-m+1; // length(u)
  c=ceil(n/k);
  r=k*c;
  fpad=[zeros(1,max(r-n,0)) f];
  gpad=[zeros(1,max(r-m,0)) g];
  fval=fft(fpad);
  gval=fft(gpad);
  if min(abs(gval))>tol
    uval=fval(1:c:r)./gval(1:c:r);
    uu=ifft(uval);
    u=[uu(2:k), uu(1)];
    u = m2p(u);
    return;
  end
  
  // the evaluation of g on fft points 
  // gives two small values.
  // move the evaluation points. 
  scale=(1+1/n).^(0:n-1);
  fs=f.*scale;
  gs=g.*scale(1:m);
  fpad=[zeros(1,r-n) fs];
  gpad=[zeros(1,r-m) gs];
  fval=fft(fpad);
  gval=fft(gpad);
  if min(abs(gval))>tol
    uval=fval(1:c:r)./gval(1:c:r);
    uu=ifft(uval);
    u=[uu(2:k), uu(1)]./scale(1:k);
    u = m2p(u);
    return;
  end
  // using an other method 
  u=epdiv_lsq(p,q);
endfunction

function u=epdiv_lsq(p,q)
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

// Computes polynomial division by solving a
// linear least squares system by standard algebra.
// The function computes u= p/q assuming that 
// p = u*q
//
  n=p.degree[];
  m=q.degree[];
  pc= p.coeffs{1};
  qc= q.coeffs{1};
  // The convolution matrix associated to q 
  A = toeplitz([qc($:-1:1),zeros(1,n-m)],[qc($),zeros(1,n-m)]);
  // we solve A*x = p in the least square sense
  u = m2p( flipud(A \ flipud(pc') ));
  u.normalize[];
  u=pc($)/qc($)*u;
endfunction

function u=epdiv_gko(p,q)
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
// 
// Adapted and revisited for Nsp (Jean-Philippe Chancelier 2010)
// 
// Computes polynomial division by solving a
// linear least squares system with GKO.
// The function computes u=f/g.
//
  n=p.degree[];
  m=q.degree[];
  f=p.coeffs{1}.';
  g=q.coeffs{1}.';

  k=n-m; 
  threshold=1e-13;

  N=n+1;
  M=k+1;  // size of convolution matrix
  MN=lcm(N,M);
  theta=exp(M*%pi*%i/MN);

  // choose Toeplitz-like generators
  G1=zeros(n+1,1);
  G1(1)=g(m+1);
  G1(k+2:n+1)=g(1:m);
  G1(1:m+1)=G1(1:m+1)-theta*g;
  G2=[zeros(1,k),1];
  
  // compute the auxiliary matrix D0
  D0= diag(exp(%pi*%i*(0:M-1)/MN))
  // compute D1 (which is NxN)
  D1= exp(2*%pi*%i*(0:N-1)/N);
  // compute D2 (which is MxM and contains the shifted M-th
  // roots of 1)
  D2=exp(%pi*%i/MN)*exp(2*%pi*%i*(0:M-1)/M);
  // now D1 and D2 define a displacement operator for the 
  // Cauchy-like matrix C associated with the cofator matrix
  // compute displacement generators G1_c and G2_c for C
  sN=sqrt(N);
  sM=sqrt(M);
  FD0=sN*ifft(D0,'m');
  G1_c=sN*ifft(G1,'m');
  G2_c=G2*FD0';

  // perform fast LU 
  L=zeros(N);
  U=zeros(N,M);
  P=eye(N);
  P2=eye(M);

  for k=1:M
    // the first generator is made orthogonal
    [Q,R]=qr(G1_c,mode="e");
    G1_c=Q;
    G2_c=R*G2_c;

    // look for column of max magnitude in G2_c
    c=zeros(1,M-k+1);
    c(k:M)=conj(G2_c(k:M)).*G2_c(k:M);
    
    //max for complex [val,col]=max(c);
    [val,col]=max(abs(c));
    val = c(col);

    U(k,k:M)=(G1_c(k)*G2_c(k:M))./((D1(k)*ones(1,M-k+1)-D2(k:M)));
    // swap s_k and s_col
    D2.perm_elem[k,col];
    // swap psi_k and psi_col
    G2_c.perm_elem[k,col,'c']
    // swap columns in U
    U.perm_elem[k,col,'c']
    // swap columns in P2
    P2.perm_elem[k,col,'c']
    L(k:N,k)=(G1_c(k:N)*G2_c(k))./((D1(k:N)-D2(k)*ones(1,N-k+1)).');

    [u,s]=max(abs(L(k:N,k)));
    s=s+k-1;
    U(k,k)=L(s,k);
    // swap t_k and t_q
    D1.perm_elem[k,s];
    // swap phi_k and phi_q
    G1_c.perm_elem[k,s,'r']
    // swap rows in L
    L.perm_elem[k,s,'r']
    // swap rows in P
    P.perm_elem[k,s,'r'];
    if ~isempty(k+1:M) then 
      U(k,k+1:M)=(G1_c(k)*G2_c(k+1:M))./(D1(k)*ones(1,M-k)-D2(k+1:M));
      G2_c(k+1:M)=G2_c(k+1:M)-G2_c(k)*(U(k,k+1:M)/U(k,k));
    end
    L(k,k)=1;
    if ~isempty(k+1:N) then 
      L(k+1:N,k)=L(k+1:N,k)/U(k,k);
      G1_c(k+1:N)=G1_c(k+1:N)-G1_c(k)*L(k+1:N,k);
    end
  end
  L(M+1:N,M+1:N)=eye(N-M);
  y=L\(P*sN*ifft(f));

  // detect nullity 
  nullity=0;
  while abs(U(M-nullity,M-nullity))<threshold
    nullity=nullity+1;
  end
  //  nullity = M- (max(find([threshold;abs(diag(U))] >= threshold ))-1);

  y=y(1:M-nullity);
  U=U(1:M-nullity,1:M-nullity);
  y=U\y;
  y=[y;zeros(nullity,1)];
  u=D0'*(1/sM)*fft(P2*y);
  u= m2p(u);
  u.normalize[];
  u=f($)/g($)*u;
endfunction


function [ok,T,no]=epdiv_test(n,fn)
  ok=%t;
  no= -%inf;
  x= poly(0);
  p1=(1+x);
  p2=(2+x);
  p3=(3+x);
  p4=(x);
  timer();
  for i=1:n
    cp=grand(1,4,'uin',0,5);
    p= p1^cp(1)*p2^cp(2)*p3^cp(3)*p4^cp(4);
    cq=grand(1,4,'uin',0,5);
    cq = min(cq,cp);
    q= p1^cq(1)*p2^cq(2)*p3^cq(3)*p4^cq(4);
    u = fn(p,q);
    cr = cp-cq;
    uref = p1^cr(1)*p2^cr(2)*p3^cr(3)*p4^cr(4)
    if norm( u- uref  ) > 1.e-2 then 
      ok = %f;
    end 
    no = max(no,norm( u- uref ));
  end
  T=timer();
  xclear();
endfunction


if %f then 
  x=poly(0);
  p=(1+x)*(1+2*x)*(1+5*x);
  q=(1+2*x);
  u=epdiv_fft(p,q);
  if norm(p -q*u) > 1.e-10 then pause;end 
  u=epdiv_lsq(p,q);
  if norm(p -q*u) > 1.e-10 then pause;end 
  u=epdiv_gko(p,q);
  if norm(p -q*u) > 1.e-10 then pause;end 

  [ok1,T1,no1]= epdiv_test(1000,epdiv_fft);
  if ~ok1 then pause;end 
  
  [ok2,T2,no2]= epdiv_test(1000,epdiv_lsq);
  if ~ok2 then pause;end 
  
  [ok3,T3,no3]= epdiv_test(1000,epdiv_gko);
  if ~ok3 then pause;end 
  
end 




