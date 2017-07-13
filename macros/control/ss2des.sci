function S=ss2des(Sl,flag)
  // Returns S=list('des',A,B,C,D,E) for Sl a state-space
  // system with Ds polynomial.
  // if flag=="withD" a maximal rank D matrix is returned in S
  // otherwise D=0;
  // Copyright INRIA

  if nargin==1 then flag="void";end
  if nargin==2 && flag <> "withD" then printf("ss2des: unknown flag!\n");end
  Ds=Sl(5);
  if type(Ds,'short')=='m' then
    if norm(Ds,1)==0 then S=Sl;return;end
    if norm(Ds,1) <> 0 then Ds=Ds+poly(0,'s')*0*Ds;end
  end
  A2=Sl.A,B2=Sl.B;C2=Sl.C;
  if flag.equal["withD"] then D=coeff(Ds,0);Ds=Ds-D;end
  [N,B1,C1]=pol2des(Ds);
  [n1,vn1]=size(N);
  [n2,vn2]=size(A2);
  E=[N,0*ones(n1,n2);0*ones(n2,n1),eye(n2,n2)];
  A=[eye(size(N)),0*ones(n1,n2);0*ones(n2,n1),A2];
  C=[C1,C2];
  B=[B1;B2];
  if ~flag.equal["withD"] then D=0*C*B;end
  S=tlist(['des','A','B','C','D','E'],A,B,C,D,E);
endfunction
