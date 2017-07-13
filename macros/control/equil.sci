function t=equil(p,q)
  // Copyright INRIA
  t=chol(q);
  [u,s,v]=svd(t*p*t');
  // s=diag(s); svd returns the diag in nsp 
  ll=ones(size(s)) ./sqrt(sqrt(s));
  t=diag(ll)*u'*t
endfunction
