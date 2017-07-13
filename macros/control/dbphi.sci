function [db,phi]=dbphi(repf,mod)
  // Copyright INRIA
  if nargin==1 then mod='c',end
  phi=phasemag(repf,mod);
  db=20*log(abs(repf))/log(10);
endfunction
