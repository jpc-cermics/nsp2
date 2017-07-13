function y=dsimul(sld,u)
  // Copyright INRIA
  [a,b,c,d]=abcd(sld);
  x0=sld.X0;
  y=c*ltitr(a,b,u,x0)+d*u;
endfunction
