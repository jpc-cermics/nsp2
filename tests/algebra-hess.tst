// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction


//==========================================================================
//==============================     hess     ============================== 
//==========================================================================

//Empty matrix
if hess([])<>[] then pause,end
[U,H]=hess([]);
if U<>[]|H<>[] then pause,end

if execstr('hess(randn(2,5))',errcatch=%t)==%t then pause,end
if execstr('[U,H]=hess(randn(2,5))',errcatch=%t)==%t then pause,end
if execstr('hess(randn(2,5)+%i)',errcatch=%t)==%t then pause,end
if execstr('[U,H]=hess(randn(2,5)+%i)',errcatch=%t)==%t then pause,end

//Small dimension
//Real case
A=randn(5,5);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>200*%eps then pause,end
if Err(U'*U-eye(size(U'*U))) >200*%eps then pause,end
if Err(U'*A*U-H1)  >200*%eps then pause,end
//complex case
A=randn(5,5)+%i*randn(5,5);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>200*%eps then pause,end
if Err(U'*U-eye(size(U'*U))) >200*%eps then pause,end
if Err(U'*A*U-H1)  >200*%eps then pause,end
//Large dimension
A=randn(20,20);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>200*%eps then pause,end
if Err(U'*U-eye(size(U'*U))) >1000*%eps then pause,end
if Err(U'*A*U-H1)  >1000*%eps then pause,end
//complex case
A=randn(20,20)+%i*randn(20,20);
H=hess(A);
[U,H1]=hess(A);
if Err(H-H1)>5000*%eps then pause,end
if Err(U'*U-eye(size(U'*U))) >5000*%eps then pause,end
if Err(U'*A*U-H1)  >5000*%eps then pause,end
