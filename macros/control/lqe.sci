function [K,X]=lqe(P21)
  // Copyright INRIA
  [A,B1,C2,D21]=abcd(P21)
  xo=P21.X0;
  sl=linear_system(A',C2',B1',D21',xo,dom = P21.dom,sample = P21.dt);
  [kk,X]=lqr(sl);
  K=kk';
endfunction
