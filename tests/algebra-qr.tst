// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction
rand('normal')
//==========================================================================
//==============================      qr      ============================== 
//==========================================================================
//Empty matrix
e=[];
if qr(e)<>[] then pause,end
if qr(e,mode="e")<>[] then pause,end

[Q,R]=qr(e);
if Q<>[]|R<>[] then pause,end
[Q,R]=qr(e,mode="e");
if Q<>[]|R<>[] then pause,end

[Q,R,x]=qr(e);
if Q<>[]|R<>[]|~isempty(x) then pause,end
[Q,R,x]=qr(e,mode="e");
if Q<>[]|R<>[]|~isempty(x) then pause,end

//Small dimension
//---------------
A=rand(3,2);Ac=A+rand(A)*%i;

//Real Case
Q=qr(A);
if Err(Q*Q'-eye(size(Q)))> 200*%eps then pause,end
Q=qr(A,mode="e");
if Err(Q'*Q-eye(size(Q'*Q)))> 200*%eps then pause,end

[Q,R]=qr(A);
if Err(Q*R-A)> 200*%eps then pause,end

[Q,R]=qr(A,mode="e");
if Err(Q*R-A)> 200*%eps then pause,end
if Err(Q'*Q-eye(size(Q'*Q)))> 200*%eps then pause,end

Q=qr(A');
if Err(Q*Q'-eye(size(Q)))> 200*%eps then pause,end
Q=qr(A',mode="e");
if Err(Q*Q'-eye(size(Q)))> 200*%eps then pause,end

[Q,R]=qr(A');
if Err(Q*R-A')> 200*%eps then pause,end

[Q,R]=qr(A',mode="e");
if Err(Q*R-A')> 200*%eps then pause,end

[Q,R,p]=qr(A);
if Err(Q*R-A(:,p))> 200*%eps then pause,end

[Q,R,p]=qr(A,mode="e");
if Err(Q*R-A(:,p))> 200*%eps then pause,end

//Complex case
Q=qr(Ac);
if Err(Q*Q'-eye(size(Q)))> 200*%eps then pause,end
Q=qr(Ac,mode="e");
if Err(Q'*Q-eye(size(Q'*Q)))> 200*%eps then pause,end
[Q,R]=qr(Ac);
if Err(Q*R-Ac)> 200*%eps then pause,end
[Q,R]=qr(Ac,mode="e");
if Err(Q*R-Ac)> 200*%eps then pause,end
if Err(Q'*Q-eye(size(Q'*Q)))> 200*%eps then pause,end

Q=qr(Ac');
if Err(Q*Q'-eye(size(Q'*Q)))> 200*%eps then pause,end
Q=qr(Ac',mode="e");
if Err(Q*Q'-eye(size(Q'*Q)))> 200*%eps then pause,end

[Q,R]=qr(Ac');
if Err(Q*R-Ac')> 200*%eps then pause,end
[Q,R]=qr(Ac',mode="e");
if Err(Q*R-Ac')> 200*%eps then pause,end

[Q,R,x]=qr(Ac);
if Err(Q*R-Ac(:,x)) > 200*%eps then pause,end
[Q,R,x]=qr(Ac,mode="e");
if Err(Q*R-Ac(:,x)) > 200*%eps then pause,end

Act=Ac';
[Q,R,x]=qr(Act);
if Err(Q*R-Act(:,x))> 200*%eps then pause,end
[Q,R,x]=qr(Act,mode="e");
if Err(Q*R-Act(:,x))> 200*%eps then pause,end

//Rank detection (obsolete)
[Q,R,x,rk]=qr(A);
if Err(Q*R-A(:,x))> 200*%eps | rk<>2  then pause,end
[Q,R,x,rk]=qr(A,tol=1.d-8);
if Err(Q*R-A(:,x))> 200*%eps | rk<>2  then pause,end

[Q,R,x,rk]=qr(Ac);
if Err(Q*R-Ac(:,x))> 200*%eps | rk<>2  then pause,end

[Q,R,x,rk]=qr(Ac,tol=1.d-8);
if Err(Q*R-Ac(:,x))> 200*%eps | rk<>2  then pause,end

//Large dimension
//---------------
A=rand(150,60);Ac=A+rand(A)*%i;
//Real Case
Q=qr(A);
if Err(Q*Q'-eye(size(Q*Q')))> 1000*%eps then pause,end
Q=qr(A,mode="e");
if Err(Q'*Q-eye(size(Q'*Q)))> 1000*%eps then pause,end

[Q,R]=qr(A);
if Err(Q*R-A)> 2000*%eps then pause,end

[Q,R]=qr(A,mode="e");
if Err(Q*R-A)> 2000*%eps then pause,end
if Err(Q'*Q-eye(size(Q'*Q)))> 1000*%eps then pause,end

Q=qr(A');
if Err(Q*Q'-eye(size(Q*Q')))> 1000*%eps then pause,end
Q=qr(A',mode="e");
if Err(Q*Q'-eye(size(Q*Q')))> 1000*%eps then pause,end

[Q,R]=qr(A');
if Err(Q*R-A')> 1000*%eps then pause,end

[Q,R]=qr(A',mode="e");
if Err(Q*R-A')> 1000*%eps then pause,end

[Q,R,x]=qr(A);
if Err(Q*R-A(:,x))> 2000*%eps then pause,end

[Q,R,x]=qr(A,mode="e");
if Err(Q*R-A(:,x))> 2000*%eps then pause,end

//Complex case
Q=qr(Ac);
if Err(Q*Q'-eye(size(Q*Q')))> 2000*%eps then pause,end
Q=qr(Ac,mode="e");
if Err(Q'*Q-eye(size(Q'*Q)))> 2000*%eps then pause,end

[Q,R]=qr(Ac);
if Err(Q*R-Ac)> 2000*%eps then pause,end
[Q,R]=qr(Ac,mode="e");
if Err(Q*R-Ac)> 2000*%eps then pause,end
if Err(Q'*Q-eye(size(Q'*Q)))> 2000*%eps then pause,end

Q=qr(Ac');
if Err(Q*Q'-eye(size(Q*Q')))> 2000*%eps then pause,end
Q=qr(Ac',mode="e");
if Err(Q*Q'-eye(size(Q*Q')))> 2000*%eps then pause,end


[Q,R]=qr(Ac');
if Err(Q*R-Ac')> 2000*%eps then pause,end
[Q,R]=qr(Ac',mode="e");
if Err(Q*R-Ac')> 2000*%eps then pause,end

[Q,R,x]=qr(Ac);
if Err(Q*R-Ac(:,x))> 2000*%eps then pause,end
[Q,R,x]=qr(Ac,mode="e");
if Err(Q*R-Ac(:,x))> 2000*%eps then pause,end

Act=Ac';
[Q,R,x]=qr(Act);
if Err(Q*R-Act(:,x))> 2000*%eps then pause,end
[Q,R,x]=qr(Act,mode="e");
if Err(Q*R-Act(:,x))> 2000*%eps then pause,end

//Rank detection (obsolete)
[Q,R,x,rk]=qr(A);
if Err(Q*R-A(:,x))> 2000*%eps | rk<>60  then pause,end
[Q,R,x,rk]=qr(A,tol=1.d-8);
if Err(Q*R-A(:,x))> 2000*%eps | rk<>60  then pause,end

[Q,R,x,rk]=qr(Ac);
if Err(Q*R-Ac(:,x))> 2000*%eps | rk<>60  then pause,end

[Q,R,x,rk]=qr(Ac,tol=1.d-8);
if Err(Q*R-Ac(:,x))> 2000*%eps | rk<>60  then pause,end
