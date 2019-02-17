// -*- Mode: nsp -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================    chol      ============================== 
//==========================================================================

//Empty matrix
if chol([])<>[] then pause,end

if execstr('chol([1 2;3 4])',errcatch=%t)==%t then pause,end
if execstr('chol([1 2;3 4]+%i)',errcatch=%t)==%t then pause,end

//Small dimension
//REAL
A=randn(5,5);A=A*A';
U=chol(A);
if Err(triu(U)-U)>200*%eps then pause,end
if Err(U'*U-A)>200*%eps then pause,end

//Complex
A=randn(5,5)+%i*randn(5,5);A=A*A';
U=chol(A);
if Err(triu(U)-U)>200*%eps then pause,end
if Err(U'*U-A)>200*%eps then pause,end

//Large dimension
//REAL
A=randn(50,50);A=A*A';
U=chol(A);
if Err(triu(U)-U)>10*%eps then pause,end
if Err(U'*U-A)>1000*%eps then pause,end

//Complex
A=randn(5,5)+%i*randn(5,5);A=A*A';
U=chol(A);
if Err(triu(U)-U)>10*%eps then pause,end
if Err(U'*U-A)>1000*%eps then pause,end

