function A=testmat1(a,n)
//eigen values are given by a dilation of nth roots of 1
  A=diag(a*ones(1,n-1),1)+diag((1/a)*ones(1,n-1),-1)
  A(1,n)=1/a;A(n,1)=a
endfunction

function r=Checktestmat1(a,n)
  A=testmat1(a,n);S=spec(A);
  SR=real(S);SI=imag(S);
  dt=2*%i*%pi/n;Z=exp(dt*(1:n)');ZR=real(Z*((1+a*a')/a));
  ZI=-imag(Z*((a*a'-1)/a));
  r=max(norm(sort(SR)-sort(ZR)),norm(sort(SI)-sort(ZI)))
endfunction

function A=testmat2(a,n)
//eigen values are given by a dilation of nth roots of 1
  A=testmat1(a,n);A=A+A'
endfunction

function r=Checktestmat2(a,n)
  A=testmat2(a,n);S=spec(A);
  SR=real(S);SI=imag(S);
  dt=2*%i*%pi/n;Z=exp(dt*(1:n)');
  ZR=2*real(Z*((1+a*a')/a));ZI=0*ZR;
  r=max(norm(sort(SR)-sort(ZR)),norm(sort(SI)-sort(ZI)))
endfunction
