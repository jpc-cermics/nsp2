function [nk,W]=hankelsv(sl,tol)
//!
// Copyright INRIA
  if type(sl,'short')<>'linearsys' then 
    error('Error: first argument should be state-space');
    return;
  end
  if sl(7)=='d' then 
    error('Error: Continuous time only');return;
  end
  //
  if nargin==1 then tol=1000*%eps,end,
  lf=spec(sl(2)),
  if min(abs(lf))<=tol then
    error('Error: Imaginary-axis poles!')
  end
  if max(real(lf)) > tol then printf('Warning:  unstable system\n'),end
  [sla,sls,d]=dtsi(sl);
  lc=ctr_gram(sls);
  lo=obs_gram(sls),W=lc*lo;
  nk=sort(real(spec(W)));
endfunction
