
function t=test1()
// Displacement Structure for the p/q division. 
// The idea is to minimize || p - qh ||_2 
// The equivalent linear algebra problem 
// is to minimize || p -A h ||_2 where 
// A is the convolution matrix associated to q.
// The matrix T = A'*A is Toeplitz and thus 
// we check here the displacement associated to T

  p = m2p(1:6);
  q = m2p(7:9);
  
  n=p.degree[];
  m=q.degree[];
  pc= p.coeffs{1};
  qc= q.coeffs{1};
  pc= pc/norm(pc,2);
  qc= qc/norm(qc,2);
  // The convolution matrix associated to q 
  A = toeplitz([qc($:-1:1),zeros(1,n-m)],[qc($),zeros(1,n-m)])
  // T = A' A is Toeplitz and such that 
  // T - Z_{n-m+1} T Z'_{n-n1+1} = G [1,0;0,-1] G' 
  Z = zeros(n-m+1,n-m+1); 
  Z.set_diag[1,-1];
  G= A'*A(:,1);
  G=[G,G]; G(1,2)=0;
  T = A'*A;
  t=norm( T - Z*T*Z' - G *[1,0;0,-1]* G')
  
  // we also have a displacement with Zu and Zmu 
  // Zu*T - T*Zmu =  G1*G2'

  c = T(:,1)';
  r = T(1,:);
  nt = size(c,'*');
  T= toeplitz(c,r);
  r1=flipud([r(2:$),0]');
  G1 = [c(:) + r1 ,eye(nt,1)];
  G2 = [eye(nt,1),c(:) - r1];
  G2 = conj(flipud(G2));
  Zu = zeros(nt,nt);
  Zu.set_diag[1,-1];
  Zu.set_diag[1,nt-1];
  Zmu = Zu;
  Zmu.set_diag[-1,nt-1];
  t2 = norm( Zu*T - T*Zmu - G1*G2')
  t = max(t2,t);
  
endfunction

function t=test2()
// displacement structure for Sylvester Matrix 
  p = m2p(1:6);
  q = m2p(7:9);
  S=sylvester(p,q);
  n=p.degree[];
  m=q.degree[];
  pc= p.coeffs{1}.';
  qc= q.coeffs{1}.';
  pc=flipud(pc);
  qc=flipud(qc);
  Z = zeros(n+m,n+m); 
  Z.set_diag[1,-1];
  G1=zeros(n+m,2);
  G1(1,1)=1;
  G1(m+1,2)=1;
  u=[ pc;zeros(m-1,1)];
  v=[ qc; -pc(2:$-1)];
  v(m+1)=v(m+1)-pc(1);
  G2=[u,v]';
  t = norm( S -Z*S*Z' - G1*G2)
endfunction

function t=test3()
// displacement structure for toeplitz Matrix 
  c = [1,2:5];
  r =[1,6:9];
  n = size(c,'*');
  T= toeplitz(c,r);
  r1=flipud([r(2:$),0]');
  G1 = [c(:) + r1 ,eye(n,1)];
  G2 = [eye(n,1),c(:) - r1];
  G2 = conj(flipud(G2));
  Zu = zeros(n,n);
  Zu.set_diag[1,-1];
  Zu.set_diag[1,n-1];
  Zmu = Zu;
  Zmu.set_diag[-1,n-1];
  t = norm( Zu*T - T*Zmu - G1*G2')
endfunction

