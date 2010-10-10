//
// Fast computation of approximate univariate polynomial GCD.
//
//
//  [z,res] = fast_gcd(f,g,delta);
//
//  f ,g --> input polynomials,
//  delta --> tolerance,
//  z --> approximate gcd,
//  res --> residual w.r.t. the gcd system.
//
//
// Computes an approximate polynomial GCD (according to
// the standard definition of epsilon-gcd) with a quadratic
// computational cost, as follows:
//
// - performs fast LU on the Sylvester matrix associated with
//   f and g and determines a tentative GCD degree;
//
// - performs fast LU on the rectangular matrix associated
//   with the linear system that gives the cofactors
//   and finds the cofactors;
//
// - finds a tentative GCD and performs fast iterative refinement;
//
//
// - repeats the above computation with an increased or
//   decreased gcd degree, depending on whether the
//   last residual found is smaller or greater than delta.
//
//
// Fast LU is computed using the GKO algorithm stabilized
// through orthogonalization of the first generator.
//
// Companion software to the paper "A fast algorithm for approximate
// polynomial gcd based on structured matrix computations", by
// D.A. Bini and P. Boito (Special issue in memory of Georg Heinig).
//

function [z,res] = fast_gcd_wls(pf,pg,delta)
  if nargin <= 2 then delta = 1.e-9;end 
  // compute degrees of input polynomials
  n=pf.degree[];//length(f)-1;
  m=pg.degree[];// length(g)-1;
  N=m+n;

  // normalize f and g
  f=pf/norm(pf);
  g=pg/norm(pg);
  
  // introduce a heuristic correction on delta
  delta_c = max(delta,1.e-9);
  
  // factorize the Sylvester matrix S
  // and find a tentative degree tdeg
  [Pr,P2r,L,U]=lu_sylvester(f,g);
  dU=abs(diag(U));
    
  eta=delta_c*sqrt(N);
  k = max(find([eta;dU] >= eta))-1;
  tdeg=N-k;
    
  clear dU;
  // handle special or forbidden values for tdeg
  tdeg = min(max(tdeg,1),m);
  
  printf("test a gcd degree of %d\n",tdeg);
  pause 
  
  if tdeg==m
    z=g;
    dfg=epdiv_fft(f,g);
    // f_nearness=[norm(f-conv(dfg,z)) 0];
    f_nearness=[norm(f- dfg*z), 0];
    [z,res]=c_f_newton_iter(f,g,z,dfg,m2p(1),f_nearness,1);
    if res<[delta delta]
      return
    end

    // if 0<tdeg<m+1 start the standard procedure
  else
    
    // compute cofactors
    [f1,f2]=c_cofactors(f,g,tdeg);
    // compute a tentative gcd
    z=epdiv_fft(f,f2.');
    // compute the residual
    f_nearness=[norm(f-conv(f2.',z)) norm(g+conv(f1.',z))];
    // perform iterative refinement
    [z,res]=c_f_newton_iter(f,g,z,f2.',-f1.',f_nearness,1);
  end

  // correct degree

  if (res<[delta,delta])
    newz=z;
    newres=res;
    while (newres<[delta,delta])
      z=newz;
      res=newres;
      tdeg=tdeg+1;
      [f1,f2]=c_cofactors(f,g,tdeg);
      newz=epdiv_fft(f,f2.');
      f_nearness=[norm(f-conv(f2.',newz)) norm(g+conv(f1.',newz))];
      [newz,newres]=c_f_newton_iter(f,g,newz,f2.',-f1.',f_nearness,0);

    end
  else
    while ((res(1)>delta)||(res(2)>delta))
      tdeg=tdeg-1;
      if tdeg==0
	fprintf('\n Approximately coprime polynomials.\n')
	z=1;
	res=0;
	return
      end
      [f1,f2]=c_cofactors(f,g,tdeg);
      z=epdiv_fft(f,f2.');
      f_nearness=[norm(f-conv(f2.',z)) norm(g+conv(f1.',z))];
      [z,res]=c_f_newton_iter(f,g,z,f2.',-f1.',f_nearness,1);

    end
  end
endfunction

// -------------------------------------------------
// function c_f_newton_iter
// -------------------------------------------------
//
// Computes fast iterative refinement using Newton's method.
//
// [z,res]=c_f_newton_iter(f,g,z,q1,q2)
//
// f, g -> polynomials whose GCD is sought,
// z -> tentative GCD (to be refined),
// q1, q2 -> tentative cofactors (to be refined).
//

function [z,res]=c_f_newton_iter(pf,pg,pz,pq1,pq2,f_nearness,optflag);
  f=pf.coeffs{1}.';
  g=pg.coeffs{1}.';
  z=pz.coeffs{1}.';
  q1=pq1.coeffs{1}.';
  q2=pq2.coeffs{1}.';
  K=norm(z)^2+norm(q1)^2+norm(q2)^2;
  // set tolerance for nearness and maximum number of iterations
  tol=1e-15;
  max_num_iter=20;
  grado=length(z)-1;
  N=length(f)-1;
  M=length(g)-1;
  allow=1;
  num_iter=0;
  while allow==1
    num_iter=num_iter+1;
    altro_Z=c_iterfast(f,g,q1,q2,z,K,optflag);
    altro_z=fliplr((altro_Z(1:grado+1)).'); // new gcd
    altro_q1=fliplr((altro_Z(grado+2:2+N)).');
    altro_q2=fliplr((altro_Z(N+3:N+3+M-grado)).'); // new cofactors
    s_nearness=[norm(conv(altro_q1,altro_z)-f), norm(conv(altro_q2,altro_z)-g)];
    if (norm(s_nearness)<tol)||(num_iter>max_num_iter)
      allow=0;
      z=altro_z;
      q1=altro_q1;
      q2=altro_q2;
      f_nearness=s_nearness;
    elseif (s_nearness>=f_nearness)
      allow=0;
    else
      z=altro_z;
      q1=altro_q1;
      q2=altro_q2;
      f_nearness=s_nearness;
    end
  end
  res=f_nearness;
endfunction

// -----------------------------------------------
// function c_j_lu
// -----------------------------------------------
//
// Fast pseudo-solution of a least squares problem whose
// associated matrix is the Jacobian matrix of the
// gcd system (based on fast LU factorization).
//
// This function is used in the fast iterative refinement.
//

function [z]=c_j_lu(p,q,g,x,K)

// make sure that p,q,g are column vectors
  [s1,s2]=size(p);
  if s2>s1
    p=p.';
  end
  [s1,s2]=size(q);
  if s2>s1
    q=q.';
  end
  [s1,s2]=size(g);
  if s2>s1
    g=g.';
  end

  p=flipud(p);
  q=flipud(q);
  g=flipud(g);

  // choose threshold used when checking whether
  // the nullity of the Jacobian is too large

  threshold=1e-9;
  
  // compute degrees and other useful parameters
  k=length(g)-1;
  n=length(p)-1;
  m=length(q)-1;
  N=n+m+2*k+2+1;
  M=n+m+k+3;
  MN=lcm(N,M);
  theta=exp(M*%pi*%i/MN);

  // compute displacement generators for the Jacobian
  zr=zeros(k,1);
  G1=zeros(N,5);
  G1(1,:)=[1 0 0 0 0];
  G1(N,:)=[0 0 0 0 1];
  G1(2:N-1,2)=[zr;p;zr;q(1:m)]-[g(2:k+1);zeros(N-k-2,1)];
  G1(2:N-1,3)=[zeros(n,1);g;zeros(m+k,1)]-[zeros(n+k,1);g;zeros(m,1)];
  G1(2:N-1,4)=[zeros(N-k-2,1);g(1:k)]-[theta*p(2:n+1); zr; theta*q;zeros(k,1,1)];
  G2=zeros(5,M);
  G2(2:4,[k+1, k+n+2, M])=eye(3);
  G2(1,:)=[g(1:k).',g(k+1)-g(1),-p.',-q(1:m).',-q(m+1)-theta*p(1)];
  G2(5,:)=[-g(2:k+1).', p(1)+q(m+1),p(2:n+1).', q.',g(k+1)-theta*g(1)];

  // compute D0 (which is MxM), an auxiliary matrix
  d0=ones(1,M);
  a=0+(%pi*%i)/(MN);
  alpha=exp(a);
  for j=2:M
    d0(j)=d0(j-1)*alpha;
  end
  D0=diag(d0);

  // compute D1 (which is NxN)
  d1=ones(1,N);
  beta=exp((2*%pi*%i)/N);
  for j=2:N
    d1(j)=d1(j-1)*beta;
  end
  D1=d1;

  // compute D2 (which is MxM and contains the
  // shifted M-th roots of 1)
  d2=ones(1,M);
  beta=exp((2*%pi*%i)/M);
  for j=2:M
    d2(j)=d2(j-1)*beta;
  end
  D2=exp(%pi*%i/MN)*d2;

  // now D1 and D2 define a displacement operator for
  // the Cauchy-like matrix C associated with the Jacobian

  // compute displacement generators for this Cauchy-like matrix:
  sN=sqrt(N);
  sM=sqrt(M);
  FD0=sM*ifft(D0);
  G1_c=sN*ifft(G1);
  G2_c=G2*FD0';

  // now G1_c and G2_c are generators for C
  // (i.e. D1*C - C*D2' = G1_c*G2_c)

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
    gg=abs(G2_c(:,k:M));
    gg=gg.*gg;
    c(k:M)=sum( gg,1);

    //XX [val,col]=max(c);
    [val,col]=max(abs(c));
    val = c(col);

    U(k,k:M)=(G1_c(k,:)*G2_c(:,k:M))./(D1(k)*ones(1,M-k+1)-D2(k:M));

    // swap s_k and s_col
    u=D2(k);
    D2(k)=D2(col);
    D2(col)=u;

    // swap psi_k and psi_col
    u=G2_c(:,k);
    G2_c(:,k)=G2_c(:,col);
    G2_c(:,col)=u;

    // swap columns in U
    u=U(:,k);
    U(:,k)=U(:,col);
    U(:,col)=u;

    // swap columns in P2
    u=P2(:,k);
    P2(:,k)=P2(:,col);
    P2(:,col)=u;
    L(k:N,k)=(G1_c(k:N,:)*G2_c(:,k))./((D1(k:N)-D2(k)*ones(1,N-k+1)).');
    [u,s]=max(abs(L(k:N,k)));
    s=s+k-1;
    U(k,k)=L(s,k);
    // swap t_k and t_q
    u=D1(k);
    D1(k)=D1(s);
    D1(s)=u;
    // swap phi_k and phi_q
    u=G1_c(k,:);
    G1_c(k,:)=G1_c(s,:);
    G1_c(s,:)=u;
    // swap rows in L
    u=L(k,:);
    L(k,:)=L(s,:);
    L(s,:)=u;
    // swap rows in P
    u=P(k,:);
    P(k,:)=P(s,:);
    P(s,:)=u;
    U(k,k+1:M)=(G1_c(k,:)*G2_c(:,k+1:M))./(D1(k)*ones(1,M-k)-D2(k+1:M));
    L(k,k)=1;
    L(k+1:N,k)=L(k+1:N,k)/U(k,k);
    G1_c(k+1:N,1)=G1_c(k+1:N,1)-G1_c(k,1)*L(k+1:N,k);
    G1_c(k+1:N,2)=G1_c(k+1:N,2)-G1_c(k,2)*L(k+1:N,k);
    G1_c(k+1:N,3)=G1_c(k+1:N,3)-G1_c(k,3)*L(k+1:N,k);
    G1_c(k+1:N,4)=G1_c(k+1:N,4)-G1_c(k,4)*L(k+1:N,k);
    G1_c(k+1:N,5)=G1_c(k+1:N,5)-G1_c(k,5)*L(k+1:N,k);
    G2_c(:,k+1:M)=G2_c(:,k+1:M)-G2_c(:,k)*(U(k,k+1:M)/U(k,k));

  end
  L(M+1:N,M+1:N)=eye(N-M);
  //y=L\(P*sN*ifft([x;abs(norm(p)^2+norm(q)^2+norm(g)^2-K)]));
  y=L\(P*sN*ifft([x;0]));

  // detect nullity of Jacobian
  nullity=0;
  while abs(U(M-nullity,M-nullity))<threshold
    nullity=nullity+1;
  end
  
  y=y(1:M-nullity);
  U=U(1:M-nullity,1:M-nullity);
  y=U\y;
  y=[y;zeros(nullity,1)];
  z=D0'*(1/sM)*fft(P2*y);

  // compute a vector in Ker(S)
  nsp=[g; -p; -q];
  nsp=nsp/norm(nsp);
  z=z-dot(nsp,z)*nsp;   // projection of z on range(S)
endfunction

// --------------------------------------------------
// function c_cofactors
// --------------------------------------------------
//
// Uses a fast algorithm (GKO + almost complete pivoting)
// to compute the cofactors of polynomials f and g
// with respect to an approximate gcd of degree tdeg:
//
// [f1,f2]=c_cofactors(f,g,tdeg);
//

function [f1,f2]=c_cofactors(f,g,tdeg)

  threshold=1e-9;  // tolerance for rank determination
    
  // make sure that f and g are column vectors
  f = f.coeffs{1}.';
  g = g.coeffs{1}.';
  
  // reverse polynomial for agreement with other programs
  f=flipud(f);
  g=flipud(g);
  
  // compute degrees and other useful parameters
  n=length(f)-1;
  m=length(g)-1;
  N=n+m-tdeg+1;
  M=n+m-2*tdeg+2;
  MN=lcm(N,M);
  theta=exp(M*%pi*%i/MN);
  
  // compute displacement generators for the cofactor matrix
  G1=zeros(N,2);
  G1(:,1)=[f(n+1); zeros(m-tdeg,1); f(1:n)]-[g; zeros(n-tdeg,1)];
  G1(:,2)=[g(m+1); zeros(n-tdeg,1); g(1:m)]-theta*[f; zeros(m-tdeg,1)];
  G2=zeros(2,M);
  G2(:,[m-tdeg+1,m+n-2*tdeg+2])=eye(2);

  // compute D0 (which is MxM), an auxiliary matrix
  d0=ones(1,M);
  a=0+(%pi*%i)/(MN);
  alpha=exp(a);
  for j=2:M
    d0(j)=d0(j-1)*alpha;
  end
  D0=diag(d0);

  // compute D1 (which is NxN)
  d1=ones(1,N);
  beta=exp((2*%pi*%i)/N);
  for j=2:N
    d1(j)=d1(j-1)*beta;
  end
  D1=d1;

  // compute D2 (which is MxM and contains the shifted M-th
  // roots of 1)
  d2=ones(1,M);
  beta=exp((2*%pi*%i)/M);
  for j=2:M
    d2(j)=d2(j-1)*beta;
  end
  D2=exp(%pi*%i/MN)*d2;

  // now D1 and D2 define a displacement operator for the
  // Cauchy-like matrix C associated with the cofator matrix

  // compute displacement generators G1_c and G2_c for C
  sN=sqrt(N);
  FD0=sN*ifft(D0);
  G1_c=sN*ifft(G1);
  G2_c=G2*FD0';

  // perform fast LU
  L=zeros(N);
  U=zeros(N,M);

  P2=eye(M);

  for k=1:M
    // the first generator is made orthogonal
    [Q,R]=qr(G1_c,mode="e");
    G1_c=Q;
    G2_c=R*G2_c;

    // look for column of max magnitude in G2_c
    c=zeros(1,M-k+1);
    c(k:M)=conj(G2_c(1,k:M)).*G2_c(1,k:M)+conj(G2_c(2,k:M)).*G2_c(2,k:M);

    //XX [val,col]=max(c);
    [val,col]=max(abs(c));
    val = c(col);

    U(k,k:M)=(G1_c(k,:)*G2_c(:,k:M))./((D1(k)*ones(1,M-k+1)-D2(k:M)));


    // swap s_k and s_col
    u=D2(k);
    D2(k)=D2(col);
    D2(col)=u;

    // swap psi_k and psi_col
    u=G2_c(:,k);
    G2_c(:,k)=G2_c(:,col);
    G2_c(:,col)=u;

    // swap columns in U
    u=U(:,k);
    U(:,k)=U(:,col);
    U(:,col)=u;

    // swap columns in P2
    u=P2(:,k);
    P2(:,k)=P2(:,col);
    P2(:,col)=u;
    L(k:N,k)=(G1_c(k:N,:)*G2_c(:,k))./((D1(k:N)-D2(k)*ones(1,N-k+1)).');


    [u,s]=max(abs(L(k:N,k)));
    s=s+k-1;
    U(k,k)=L(s,k);
    // swap t_k and t_q
    u=D1(k);
    D1(k)=D1(s);
    D1(s)=u;
    // swap phi_k and phi_q
    u=G1_c(k,:);
    G1_c(k,:)=G1_c(s,:);
    G1_c(s,:)=u;
    // swap rows in L
    u=L(k,:);
    L(k,:)=L(s,:);
    L(s,:)=u;


    U(k,k+1:M)=(G1_c(k,:)*G2_c(:,k+1:M))./(D1(k)*ones(1,M-k)-D2(k+1:M));


    L(k,k)=1;
    L(k+1:N,k)=L(k+1:N,k)/U(k,k);
    G1_c(k+1:N,1)=G1_c(k+1:N,1)-G1_c(k,1)*L(k+1:N,k);
    G1_c(k+1:N,2)=G1_c(k+1:N,2)-G1_c(k,2)*L(k+1:N,k);

    G2_c(:,k+1:M)=G2_c(:,k+1:M)-G2_c(:,k)*(U(k,k+1:M)/U(k,k));

  end

  // compute a vector in the null space of the cofactor matrix
  emme=m+n-2*tdeg+2;
  nullity=0;
  while abs(U(emme-nullity,emme-nullity))<threshold
    nullity=nullity+1;
  end
  if nullity==0
    nullity=1;
  end
  nullity=nullity-1; // this is not the nullity now
  r=-U(1:emme-1-nullity,emme-nullity);
  U=U(1:emme-1-nullity,1:emme-1-nullity);
  x=U\r;
  x=[x;1;zeros(nullity,1)];
  x=D0'*fft(P2*x);  // this vector contains the cofactors
  f1=flipud((x(1:m-tdeg+1)));
  f2=flipud((x(m-tdeg+2:emme)));
endfunction

// -----------------------------------------------------
// function c_iterfast
// -----------------------------------------------------
//
// writes the system associated with the GCD
//

function newz=c_iterfast(a,b,q1,q2,g,K,optflag)

  threshold=1e-6;
  c=flipud([conv(q2,g).';conv(q1,g).']);
  direction=c_j_lu(q1,q2,g,(c-[(fliplr(a)).'; (fliplr(b)).']),K);
  c1=costfun(1,a,b,q1,q2,g,direction);
  if (optflag==1)&&(c1>costfun(0,a,b,q1,q2,g,direction))&&(c1>threshold)
    //if (optflag==1)&&(c1>costfun(0,a,b,q1,q2,g,direction))
    options=optimset('Display','off','MaxIter',3);
    fprintf('*\n')
    // XXXX alpha=fminbnd(@(apar) costfun(apar,a,b,q1,q2,g,direction),0,5,options);
  else
    alpha=1;
  end
  newz=[(fliplr(g).'); fliplr(q1).'; fliplr(q2).'] - alpha*direction;
endfunction

// -----------------------------------------------------
// function costfun
// -----------------------------------------------------
//
// computes the cost function (to be used for line search
// in c_iterfast)
//

function F=costfun(apar,a,b,q1,q2,g,direction)

  z_t=[(fliplr(g).'); fliplr(q1).'; fliplr(q2).'] - apar*direction;
  lg=length(g);
  lq1=length(q1)+lg;
  newg=flipud(z_t(1:lg));
  polya=conv(flipud(z_t(lg+1:lq1)),newg);
  polyb=conv(flipud(z_t(lq1+1:$)),newg);
  sz=size(polya);
  if sz(1)>sz(2)
    polya=polya.';
    polyb=polyb.';
  end
  F=norm([a-polya, b-polyb]);
endfunction


function y=lcm_m_m(a,b)
// XXXXX 
  g = euclide(a,b);
  y = (a/g)*b;
endfunction
