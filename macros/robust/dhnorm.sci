function gama=dhnorm(Sl,tol,gamamax)
//discrete-time case
// included in h_norm!

  if nargin==1 then tol=0.000001;gamamax=10000000;end
  if nargin==2 then gamamax=1000;end
  gamamin=sqrt(%eps);
  n=0;
  while %T
    gama=(gamamin+gamamax)/2;n=n+1;
    if n>1000 then printf("Warning: dhnorm: more than 1000 iterations!\n');return;end
    if dhtest(Sl,gama) then
      gamamax=gama; else gamamin=gama
    end
    if (gamamax-gamamin)<tol then return;end
  end

endfunction
function ok=dhtest(Sl,gama)
//test if discrete hinfinity norm of Sl is < gama
  [A,B,C,D]=abcd(Sl);B=B/sqrt(gama);C=C/sqrt(gama);D=D/gama;
  R= D'*D;R=eye(size(R)) -R;
  [n,n]=size(A);Id=eye(n,n);Z=0*Id;
  Ak=A+B*inv(R)*D'*C;
  e=[Id,-B*inv(R)*B';Z,Ak'];
  Aa=[Ak,Z;-C'*inv(eye(D*D')-D*D')*C,Id];
  [As,Es,w,k]=schur(Aa,e,sort='d');
  //Testing magnitude 1 eigenvalues.
  [al,be]=spec(As,Es);
  finite=find(abs(be)>0.00000001);
  finite_eigen=al(finite)./be(finite);
  bad=find( abs(abs(finite_eigen)-1) < 0.0000001);
  if  ~isempty(bad) then ok=%f;return;end
  //if k<>n then ok=%f;return;end
  ws=w(:,1:n);
  x12=ws(1:n,:);
  phi12=ws(n+1:2*n,:);
  if rcond(x12) > 1.d-6 then
    X=phi12/x12;
    // z=eye()-B'*X*B
    z=-B'*X*B; z = eye(size(z))+z;
    ok= min(real(spec(z))) > -%eps
  else
    ok=%t;end
endfunction