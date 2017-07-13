function K=lqg(P,r)
  // returns the (strictly proper) lqg (H2) controller
  // for the augmented plant P
  // Copyright INRIA
  if type(P,'short')=='r' then
    flag=%f
    P=tf2ss(P);
  elseif type(P,'short')=='linearsys' then
    flag=%t
  else
    error('Error: expecting a linear system.');
    return;
  end
  [A,B1,B2,C1,C2,D11,D12,D21,D22]=smga(P,r);
  if norm(D11,1) <> 0 then printf("Warning: lqg: D11 is not zero! (set to zero)\n");end
  dom=P.dom;
  if dom=='u' then
    printf("Warning: lqg: time domain unspecified, assumed continuous\n");
    dom='c';
  end
  P12=syslin(dom,A,B2,C1,D12);
  Kc=lqr(P12);
  P21=syslin(dom,A,B1,C2,D21);
  Kf=lqe(P21);
  P22=syslin(dom,A,B2,C2,D22);
  K=obscont(P22,Kc,Kf);
  if ~flag then K=ss2tf(K);end
endfunction
