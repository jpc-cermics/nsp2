function [Closed,F,G]=ddp(Sys,zeroed,B1,D1,flag,Alfa,Beta)
  //--------------Exact disturbance decoupling----------
  // Given a linear system, and a subset of outputs, z, which are to
  // be zeroed, characterize the inputs w of Sys such that the 
  // transfer function from w to z is zero.
  //
  // Sys = linear system {A,B2,C,D2} with one input and two outputs 
  // i.e  Sys: u-->(z,y) in the following 
  //
  //  xdot =  A x + B1  w + B2  u
  //     z = C1 x + D11 w + D12 u
  //     y = C2 x + D21 w + D22 u
  //
  //  outputs of Sys are partitioned into (z,y) where z is to be zeroed,
  //  i.e. the matrices C and D2 are:
  //
  //     C=[C1;C2]         D2=[D12;D22]
  //     C1=C(zeroed,:)    D12=D1(zeroed,:)
  //
  // The control is u=Fx+Gw and one 
  // looks for F,G such that the closed loop system: w-->z given by 
  //
  //  xdot= (A+B2*F)  x + (B1 + B2*G) w
  //    z = (C1+D12F) x + (D11+D12*G) w
  //
  // has zero transfer transfer function.
  //
  // flag='ge' : no stability constraints
  //     ='st' : look for stable closed loop system (A+B2*F stable)
  //     ='pp' : eigenvalues of A+B2*F are assigned to Alfa and Beta
  //
  // Closed = w-->y closed loop system
  //
  //  xdot= (A+B2*F)  x + (B1 + B2*G) w
  //    y = (C2+D22*F) x + (D21+D22*G) w
  //
  // Stability (resp. pole placement) requires stabilizability 
  // (resp. controllability) of (A,B2).
  // Author: F.D.
  //

  function G=lowlev()
    ww=[B21,B22;D11,D12];
    [xx,dd]=colcomp(ww);
    K=kernel(ww);
    rowG=size(B22,2);colG=size(B1,2);
    if size(K,2) > colG then K=K(:,1:colG);end
    Kup=K(1:size(K,2),:);
    if rcond(Kup) <= 1.E-10 then
      x
      printf('Warning: Bad conditioning!\n');
      K1=K*pinv(Kup);G=K1(size(K,2)+1:$,:);return
    end
    K1=K*inv(Kup);//test conditioning here!
    G=K1(size(K,2)+1:$,:);
  endfunction

  if nargin==5 then Beta=-1;end
  if nargin==4 then Beta=-1;Alfa=-1;end
  if nargin==3 then Beta=-1;Alfa=-1;flag='st';end
  if nargin==2 then
    Beta=-1;Alfa=-1;flag='st';D1=zeros(size(Sys.C,1),size(B1,2));
  end
  if size(B1,1) <> size(Sys.A,1) then
    error('Error: dims of B1 and A are not compatible');end
  if size(D1,2) <> size(B1,2) then error('Error: dims of D1 and B1 are not compatible');end
  // Sys1=Sys(zeroed,:);
  Sys1=linear_system(Sys.A,Sys.B,Sys.C(zeroed,:),Sys.D(zeroed,:),Sys.X0,dom = Sys.dom, ...
                     sample = Sys.dt);
  not_zeroed=1:size(Sys,1);not_zeroed(zeroed)=[];
  [X,dims,F,U,k,Z]=abinv(Sys1,Alfa,Beta,flag);nv=dims(3);
  Sys_new=ss2ss(Sys,X);Fnew=F*X;
  B1new=X'*B1;B2new=Sys_new.B;
  D11=D1(zeroed,:);D12=Sys1(5);// to be sure we have matrix 
  B21=B1new(nv+1:$,:);B22=B2new(nv+1:$,:);
  // G s.t. B21+B22*G=0        D11+D12*G=0
  G=lowlev();
  [Anew,Bnew,Cnew,Dnew]=abcd(Sys_new);
  Anew=Anew+B2new*Fnew;Cnew=Cnew+Dnew*Fnew;
  B1new=B1new+B2new*G;
  A11=Anew(1:nv,1:nv);C21=Cnew(not_zeroed,1:nv);
  B11=B1new(1:nv,:);D21=D1(not_zeroed,:);
  D22=Sys.D;D22=D22(not_zeroed,:);D21=D21+D22*G;
  Closed=linear_system(A11,B11,C21,D21,zeros(nv,1),dom = Sys.dom,sample = Sys.dt);
endfunction
