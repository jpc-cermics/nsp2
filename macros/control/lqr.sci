function [K,X]=lqr(P12)
  //lqr gain for full-state LQ problem
  //(discrete or continuous)
  //          discrete                        continuous
  //      |I   0   0|   | A    0   B  |      |I   0   0|   | A    0    B  |
  //     z|0   A'  0| - |-C'C  I   -S'|    s |0   I   0| - |-C'C -A'  -S' |
  //      |0   B'  0|   | S    0   D'D|      |0   0   0|   | S   -B'   D'D|
  // Copyright INRIA

  if type(P12,'short') <> 'linearsys' then
    error('lqr: state-space only!');
  end
  [A,B2,C1,D12]=abcd(P12);
  Q=C1'*C1;R=D12'*D12;S=D12'*C1;

  [n,nu]=size(B2);[ny,n]=size(C1);
  select P12.dom
    case 'u' then
     error('lqr: time domain is not defined (set P.dom=''c'' or ''d'')')
    case 'c' then
     Z=0*A;I=eye(n,n);O=zeros(n,nu);
     bigE=[I,Z,O;Z,I,O;zeros(nu,2*n+nu)];

     bigA=[A,Z,B2;-Q,-A',-S';S,B2',R];
     Ri=inv(R);
     Left=[I,Z,-B2*Ri;Z,I,S'*Ri;zeros(nu,2*n),Ri];
     LA=Left*bigA;LE=Left*bigE;N=1:2*n;
     //[wsmall,ks1]=schur(LA(N,N),LE(N,N),'c');
     [wsmall,ks1]=schur(LA(N,N),sort = 'c');
     if ks1 <> n then error('Error: stable subspace too small!');end
     X12=wsmall(1:n,1:n);phi12=wsmall(n+1:$,1:n);X=phi12/X12;
     K=-Ri*(B2'*X+S);
     return;
     // Other implementation ...
     //[Q,Z,Qd,Zd,numbeps,numbeta]=kroneck(bigE,bigA);
     [w,ks]=schur(bigA,bigE,sort = 'c');
     if ks <> n then error('Error: stable subspace too small!');end
     ws=w(:,1:n);
     X12=ws(1:n,:);
     phi12=ws(n+1:2*n,:);
     u12=ws(2*n+1:2*n+nu,:);
     if rcond(X12) < 1.E-5 then printf("Warning: lqr: bad conditionning!\n");end
     K=u12/X12;
     X=phi12/X12;
     return
    case 'd' then
     I=eye(size(A));Z=0*I;
     Q=C1'*C1;R=D12'*D12;S=D12'*C1;

     bigE=[I,Z,0*B2;Z,A',0*B2;0*B2',-B2',0*B2'*B2];
     bigA=[A,Z,B2;-Q,I,-S';S,0*B2',R];
     Ri=inv(R);
     Left=[I,Z,-B2*Ri;Z,I,S'*Ri;zeros(nu,2*n),Ri];
     LA=Left*bigA;LE=Left*bigE;N=1:2*n;
     // Take care schur with two arguments in nsp is qz
     // [wsmall,ks1]=schur(LA(N,N),LE(N,N),sort = 'd');
     // qz returns more arguments 
     [Asx,Esx,Qx,wsmall,ks1]= qz(LA(N,N),LE(N,N),sort = 'd');
     if ks1 <> n then error('Error: stable subspace too small!');end
     X12=wsmall(1:n,1:n);phi12=wsmall(n+1:$,1:n);X=phi12/X12;
     K=-pinv(B2'*X*B2+R)*(B2'*X*A+S);
     return
     // Other form ...
     // [w,ks]=schur(bigA,bigE,sort = 'd');
     [Asx,Esx,Qx,w,ks]=qz(bigA,bigE,sort = 'd');
     if ks <> n then error('Error: stable subspace too small!');end
     ws=w(:,1:n);
     X12=ws(1:n,:);
     phi12=ws(n+1:2*n,:);
     u12=ws(2*n+1:2*n+nu,:);
     if rcond(X12) < 1.E-5 then printf("Warning: lqr: bad conditionning!\n");end
     K=u12/X12;
     X=phi12/X12;
     return
  end
endfunction
