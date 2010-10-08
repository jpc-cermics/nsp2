
function [Pr,P2r,L,U]=lu_sylvester(pf,pg) 
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
// 
// Fast LU factorization of the sylvester matrix associated to (p,q) 
// we follow here the description of 
// Paola Boito thesis chap 2.
// Let Ms be the sylvester matrix 
//
// C = Fm'*Ms*inv(D0)*Fm ;
// D0= diag(exp(%pi*%i*(0:N-1)/N))
// Fm is the fourrier matrix 
// 
// a fast LU of C is performed.
// C(Pr,P2r) = L*U
// 
// Fm*A = fft(A,'m')/sqrt(n) 
// Fm'*A =  ifft(A,'m')*sqrt(n)
// Fm*Fm' = Id
//
// compute degrees of input polynomials
  n=pf.degree[];//length(f)-1;
  m=pg.degree[];// length(g)-1;
  N=m+n;
  f = pf.coeffs{1};
  g = pg.coeffs{1};
  f = fliplr(f);
  g = fliplr(g);
  
  // compute displacement generators for the Sylvester matrix
  G2=zeros(2,N);
  G2(1,N-m:N)=g;
  G2(1,1:n)=G2(1,1:n)-f(2:n+1);
  G2(1,N)=G2(1,N)+f(1);
  G2(2,N-n:N)=f;
  G2(2,1:m)=G2(2,1:m)-g(2:m+1);
  G2(2,N)=G2(2,N)+g(1);
  G1=zeros(N,2);
  G1([1;m+1],:)=eye(2);
  
  if %t then 
    // test displacement equation 
    Ms = sylvester(pf,pg);
    Zu = zeros(N,N);
    Zu.set_diag[1,-1];
    Zu.set_diag[1,N-1];
    Zmu = Zu;
    Zmu.set_diag[-1,N-1];
    // 
    if  norm( Zu*Ms - Ms*Zmu - G1*G2) > 1000*%eps then pause;end 
  end
  
  // compute the auxiliary matrix D0
  D0= diag(exp(%pi*%i*(0:N-1)/N))
  // compute D1
  D1= exp(2*%pi*%i*(0:N-1)/N);
  // compute D2
  D2= exp(%pi*%i/N)*D1;
  // now D1 and D2 are the diagonals of the
  // matrices that define a displacement operator for
  // the Cauchy-like matrix C associated with
  // the Sylvester matrix
  
  // compute generators G1_c and G2_c  for C
  sN=sqrt(N);
  FD0=sN*ifft(D0,'m'); // Fm'*D0
  G1_c=sN*ifft(G1,'m');// Fm'*G1 
  G2_c=G2*FD0';       // G2*D0'*Fm
  
  if %t then 
    // test the new displacement function 
    // Fm=Fmat(N);
    Fm=ones(N,N);
    for i=1:N; Fm(i,1:N)=exp(2*%pi*%i*(i-1)*((1:N)-1)/N)/sqrt(N);end 
    Fm=Fm';
    C = Fm'*Ms*inv(D0)*Fm ;
    if norm(diag(D1)*C -C*diag(D2) - G1_c*G2_c) > 10000*%eps then pause;end 
  end 
  
  // perform fast LU of Matrix C 
  
  L=zeros(N); U=zeros(N);
  //P=eye(N);
  Pr=1:N;
  //P2=eye(N);
  P2r=1:N;

  for k=1:N
    // the first generator is made orthogonal
    [Q,R]=qr(G1_c,mode="e");
    G1_c=Q;
    G2_c=R*G2_c;
    // look for column of max magnitude in G2_c
    c=zeros(1,N-k+1);
    c(k:N)=conj(G2_c(1,k:N)).*G2_c(1,k:N)+conj(G2_c(2,k:N)).*G2_c(2,k:N);
    //XX [val,col]=max(c);
    [val,col]=max(abs(c));
    val = c(col);
    U(k,k:N)=(G1_c(k,:)*G2_c(:,k:N))./(D1(k)-D2(k:N));

    // swap s_k and s_col
    D2.perm_elem[k,col];
    // swap psi_k and psi_col
    G2_c.perm_elem[k,col,'c']
    // swap columns in U
    U.perm_elem[k,col,'c']
    // swap columns in P2
    //P2.perm_elem[k,col,'c']
    P2r.perm_elem[k,col]
    L(k:N,k)=(G1_c(k:N,:)*G2_c(:,k))./((D1(k:N)-D2(k)*ones(1,N-k+1)).');
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
    //P.perm_elem[k,s,'r']
    Pr.perm_elem[k,s];
    //
    U(k,k+1:N)=(G1_c(k,:)*G2_c(:,k+1:N))./(D1(k)*ones(1,N-k)-D2(k+1:N));
    L(k,k)=1;
    L(k+1:N,k)=L(k+1:N,k)./U(k,k);
    G2_c(:,k+1:N)=G2_c(:,k+1:N)-G2_c(:,k)*(U(k,k+1:N)./U(k,k));
    G1_c(k+1:N,1)=G1_c(k+1:N,1)-G1_c(k,1)*L(k+1:N,k);
    G1_c(k+1:N,2)=G1_c(k+1:N,2)-G1_c(k,2)*L(k+1:N,k);
  end
  if %t then 
    if norm(C(Pr,P2r) -L*U)  > 10000*%eps then pause;end 
  end 
endfunction

function x=lu_sylvester_solve(Pr,P2r,L,U,b)
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
// 
// [Pr,P2r,L,U]=lu_sylvester(pf,pg)
// C = Fm'*Ms*inv(D0)*Fm ;
// D0= diag(exp(%pi*%i*(0:N-1)/N))
// C(Pr,P2r) = L*U
// Fm*A = fft(A,'m')/sqrt(n) 
// Fm'*A =  ifft(A,'m')*sqrt(n)
// 
// solve Ms x= b 
// Ms = Fm*C*Fm'*inv(D0)
// Ms x = b 
// C*Fm'*inv(D0)*x = Fm'*b  
  n = size(Pr,'*');
  b1=ifft(b,'m')*sqrt(n);
  // C* y = b1 
  // now we need to solve C*x=b1 
  // with L*U = C(Pr,P2r)
  x1=solve(L,b1(Pr,:),mode="lo");
  x1=solve(U,x1,mode="up");
  x2=x1;
  x2(P2r,:)=x1;
  // now Fm'*inv(D0)*x = x2 
  // 
  x2=  fft(x2,'m')/sqrt(n) ;
  x2.scale_rows[exp(-%pi*%i*(0:n-1)/n)]
  x= x2;
endfunction 


// detail de la factorization L*U de la matrice de sylvester 
// pour deux polynomes f et g 
// 

if %f then 
  function M=Fmat(n)
  // Fourier matrix 
    M=ones(n,n);
    for i=1:n 
      M(i,1:n)=exp(2*%pi*%i*(i-1)*((1:n)-1)/n)/sqrt(n);
    end
    M=M';
  endfunction
  // link between Fourrier matrix and  fft 
  n=4;
  x=testm('magic',n);
  Fm=Fmat(n);
  if norm( fft(x,'m')/sqrt(n) - Fm*x )> 1000*%eps then pause;end 
  if norm( ifft(x,'m')*sqrt(n) - Fm'*x )> 1000*%eps then pause;end 

  x=poly(0);
  pf = m2p(1:3);
  pg = m2p(7:9);

  [Pr,P2r,L,U]=lu_sylvester(pf,pg) ;
  b =  testm('magic',size(Pr,'*'));
  x=lu_sylvester_solve(Pr,P2r,L,U,b);
  Ms=sylvester(pf,pg);
  norm(Ms*x-b);

end








