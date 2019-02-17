// -*- Mode: nsp -*- 
function r=Err(x),r=norm(x,1),endfunction
//define tools
exec('algebra-funs.sce');

//==========================================================================
//==============================     inv      ============================== 
//==========================================================================
//Empty matrix
A=[];
if inv(A)<>[] then pause,end
//Singular matrix
if execstr('inv([0 0;2 3])',errcatch=%t)==%t then pause,end
if execstr('inv([0 0;%i 3])',errcatch=%t)==%t then pause,end
//Rectangular matrix
if execstr('inv(randn(2,3))',errcatch=%t)==%t then pause,end
if execstr('inv(randn(2,3)+%i*eye(2,3))',errcatch=%t)==%t then pause,end
//Small dimension
//---------------
//Unsymetric
A=testmat1(3,5);Ac=testmat1(3+%i,5);
//Real Case
if Err(A*inv(A)-eye(size(A))) >200*%eps then pause,end
//Complex Case
if Err(Ac*inv(Ac)-eye(size(A))) >200*%eps then pause,end
//Symetric
A=A*A';Ac=Ac*Ac';
//Real Case
if Err(A*inv(A)-eye(size(A))) >1000*%eps then pause,end
//Complex Case
if Err(Ac*inv(Ac)-eye(size(A))) >1000*%eps then pause,end
//Large dimension
//---------------
//Unsymetric
A=testmat1(3,50);Ac=testmat1(3+%i,50);
//Real Case
if Err(A*inv(A)-eye(size(A))) >1000*%eps then pause,end
//Complex Case
if Err(Ac*inv(Ac)-eye(size(A))) >2000*%eps then pause,end
//Symetric
A=A*A';Ac=Ac*Ac';
//Real Case
if Err(A*inv(A)-eye(size(A))) >1.d-10 then pause,end
//Complex Case
if Err(Ac*inv(Ac)-eye(size(A))) >4.d-10 then pause,end
