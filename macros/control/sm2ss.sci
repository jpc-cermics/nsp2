function des=sm2ss(sysmat)
  // Generalized system-matrix to descriptor
  // sysmat=[-s I + A , B; C , D]
  // [n,n]=size(A) 
  // Copyright INRIA

  [nl,nc]=size(sysmat);
  E=-coeff(sysmat,1);
  if nargin==1 then
    k=nc;
    while E(:,k)==0*E(:,k) do
      k=k-1;n=k;
    end
    l=nl;
    while E(l,:)==0*E(l,:) do
      l=l-1;n1=l;
    end
  end
  if n1 <> n then printf('sm2ss: cannot find n!\n');end
  E=E(1:n,1:n);
  if E <> eye(n,n) then error('Error: sm2ss: seams not a state-space system?');end
  A=coeff(sysmat(1:n,1:n),0);
  B=coeff(sysmat(1:n,n+1:nc),0);
  C=coeff(sysmat(n+1:nl,1:n),0);
  D=coeff(sysmat(n+1:nl,n+1:nc),0);
  des=syslin([],A,B,C,D);
endfunction
