function C = sylv(At,Bt,Ct)
  if (isreal(At,%f) & isreal(Bt,%f) & isreal(Ct,%f)) then
    C= dtrsyl(At,Bt,Ct);
  else
    C= ztrsyl(At+0*%i,Bt+0*%i,Ct+0*%i);
  end
endfunction

if %f then
  n=4;m=3;
  At=rand(n,n)+%i*rand(n,n);Ct=rand(n,m)+%i*rand(n,m);Bt=rand(m,m)+%i*rand(m,m);
  X = sylv(At,Bt,Ct);
  norm(At*X+X*Bt-Ct)
  Y = sylv(real(At),Bt,Ct);  
  norm(real(At)*Y+Y*Bt-Ct)
  Z = sylv(At,real(Bt),Ct);  
  norm(At*Z+Z*real(Bt)-Ct)
  W = sylv(At,Bt,real(Ct));  
  norm(At*W+W*Bt-real(Ct))
end

function C = dtrsyl(At,Bt,Ct)
  if nargin < 3 then error('dtrsyl requires at least 3 input arguments');end
  if isempty(At)  ||  isempty(Bt) then X=[];return;end
  if ~and([isreal(At),isreal(Bt),isreal(Ct)]) then 
    error('dtrsyl: input matrix should be real');
  end
  A=At;
  [MA,NA]=size(A);
  if MA~=NA then
    error("Matrix A must be square!");
  end
  B=Bt;
  [MB,NB]=size(B);
  if MB~=NB then
    error("Matrix B must be square!");
  end
  Ctmp=Ct;
  [MC,NC]=size(Ct);
  if (MC~=MA) || (NC~=NB) then
    error("Invalid C matrix (dimensions)");
  end
  SCALE=1;INFO=-999;
  [X,A]=schur(A);[Y,B]=schur(B);Ctmp=X'*Ctmp*Y;
  lapack_dtrsyl('N','N',1,MA,MB,A,MA,B,MB,Ctmp,MC,SCALE,INFO,1,1);
// DTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,
//         LDC, SCALE, INFO )
  if INFO~=0 then
  error('dtrsyl returns with INFO='+string(INFO));
  end
  C=X*Ctmp*Y';
endfunction

if %f then 
  A=rand(4,4);C=rand(4,3);B=rand(3,3);
  X = dtrsyl(A,B,C);
  norm(A*X+X*B-C)
end


function C = ztrsyl(At,Bt,Ct)
  if nargin < 3 then error('ztrsyl requires at least 3 input arguments');end
  if isempty(At)  ||  isempty(Bt) then X=[];return;end
  A=At;
  [MA,NA]=size(A);
  if MA~=NA then
    error("Matrix A must be square!");
  end
  B=Bt;
  [MB,NB]=size(B);
  if MB~=NB then
    error("Matrix B must be square!");
  end
  Ctmp=Ct;
  [MC,NC]=size(Ct);
  if (MC~=MA) || (NC~=NB) then
    error("Invalid C matrix (dimensions)");
  end
  SCALE=1;INFO=-999;
  [X,A]=schur(A);[Y,B]=schur(B);Ctmp=X'*Ctmp*Y;
  lapack_ztrsyl('N','N',1,MA,MB,A,MA,B,MB,Ctmp,MC,SCALE,INFO,1,1);
  if INFO~=0 then
  error('ztrsyl returns with INFO='+string(INFO));
  end
  C=X*Ctmp*Y';
endfunction

if %f then
  n=4;m=3;
  A=rand(n,n)+%i*rand(n,n);C=rand(n,m)+%i*rand(n,m);B=rand(m,m)+%i*rand(m,m);
  X = ztrsyl(A,B,C);
  norm(A*X+X*B-C)
end

