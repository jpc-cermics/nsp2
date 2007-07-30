// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction
rand('normal')


//==========================================================================
//==============================     hess     ============================== 
//==========================================================================

//Empty matrix
if hess([])<>[] then pause,end
[U,H]=hess([]);
if U<>[]|H<>[] then pause,end

if execstr('hess(rand(2,5))',errcatch=%t)==%t then pause,end
if execstr('[U,H]=hess(rand(2,5))',errcatch=%t)==%t then pause,end
if execstr('hess(rand(2,5)+%i)',errcatch=%t)==%t then pause,end
if execstr('[U,H]=hess(rand(2,5)+%i)',errcatch=%t)==%t then pause,end

//Small dimension
//Real case
A=rand(5,5);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>200*%eps then pause,end
if Err(U'*U-eye(U'*U)) >200*%eps then pause,end
if Err(U'*A*U-H1)  >200*%eps then pause,end
//complex case
A=rand(5,5)+%i*rand(5,5);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>200*%eps then pause,end
if Err(U'*U-eye(U'*U)) >200*%eps then pause,end
if Err(U'*A*U-H1)  >200*%eps then pause,end
//Large dimension
A=rand(20,20);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>200*%eps then pause,end
if Err(U'*U-eye(U'*U)) >1000*%eps then pause,end
if Err(U'*A*U-H1)  >1000*%eps then pause,end
//complex case
A=rand(20,20)+%i*rand(20,20);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>5000*%eps then pause,end
if Err(U'*U-eye(U'*U)) >5000*%eps then pause,end
if Err(U'*A*U-H1)  >5000*%eps then pause,end
