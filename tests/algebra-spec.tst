// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction
rand('normal')
//define tools
exec('algebra-funs.sce');

//==========================================================================
//==============================     spec     ============================== 
//==========================================================================

//Empty matrix
A=[];
S=spec(A);
if S<>[] then pause,end
//Matrix with Inf or Nan (test de la detection d'erreur
if execstr('spec([%inf 1;2 3])',errcatch=%t)==%t then pause,end
if execstr('spec([1 %nan;2 3])',errcatch=%t)==%t then pause,end

if execstr('spec([%inf %i;2 3])',errcatch=%t)==%t then pause,end
if execstr('spec([%i %i;%nan 3])',errcatch=%t)==%t then pause,end

//Small dimension
//---------------
//Real Case
//Unsymetric
if Checktestmat1(3,5)>200*%eps then pause,end
[S,U]=spec(testmat1(3,5));
if Err(U*diag(S)*U'-testmat1(3,5))>200*%eps then pause,end 
//Symmetric
if Checktestmat2(3,5)>200*%eps then pause,end
[S,U]=spec(testmat2(3,5));
if Err(U*diag(S)*U'-testmat2(3,5))>200*%eps then pause,end 

//Complex Case
//Unsymetric
if Checktestmat1(3+2*%i,5)>200*%eps then pause,end
[S,U]=spec(testmat1(3+2*%i,5));
if Err(U*diag(S)*U'-testmat1(3+2*%i,5))>200*%eps then pause,end 

//Symmetric
if Checktestmat2(3+2*%i,5)>200*%eps then pause,end
[S,U]=spec(testmat2(3+2*%i,5));
if Err(U*diag(S)*U'-testmat2(3+2*%i,5))>200*%eps then pause,end 

//Large dimension
//---------------
//Real Case
//Unsymetric
if Checktestmat1(3,50)>1000*%eps then pause,end
[S,U]=spec(testmat1(3,50));
if Err(U*diag(S)*U'-testmat1(3,50))>3000*%eps then pause,end 

//Symmetric
if Checktestmat2(3,50)>1000*%eps then pause,end
[S,U]=spec(testmat2(3,50));
if Err(U*diag(S)*U'-testmat2(3,50))>10000*%eps then pause,end 

//Complex Case
//Unsymetric
if Checktestmat1(3+2*%i,50)>1000*%eps then pause,end
[S,U]=spec(testmat1(3+2*%i,50));
if Err(U*diag(S)*U'-testmat1(3+2*%i,50))>10000*%eps then pause,end 

//Symmetric
if Checktestmat2(3+2*%i,50)>1000*%eps then pause,end
[S,U]=spec(testmat2(3+2*%i,50));
if Err(U*diag(S)*U'-testmat2(3+2*%i,50))>100000*%eps then pause,end 
