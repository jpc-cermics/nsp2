// -*- Mode: nsp -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================   balanc     ============================== 
//==========================================================================

function A=testmat3(a,n)
//eigen values are given by a dilation of nth roots of 1
  A=diag(a*ones_new(1,n-1),1)+diag((1/a)*ones_new(1,n-1),-1)
  A(1,n)=1/a;
endfunction

A=[];[Ab,X]=balanc(A);
if Ab<>[]|X<>[] then pause,end
// MATRIX
//Small dimension
//---------------
//Real Case
A=testmat3(10000,5);
[Ab,X]=balanc(A);
if Err(diag(diag(X))-X)>200*%eps then pause,end 
if Err(Ab-inv(X)*A*X)>200*%eps then pause,end 

//Complex Case
A=testmat3(10000+0.01*%i,5);
[Ab,X]=balanc(A);
if Err(diag(diag(X))-X)>200*%eps then pause,end 
if Err(Ab-inv(X)*A*X)>200*%eps then pause,end 

//LArge dimension
A=testmat3(10000,30);
[Ab,X]=balanc(A);
if Err(diag(diag(X))-X)>200*%eps then pause,end 
if Err(Ab-inv(X)*A*X)>1000*%eps then pause,end 

//Complex Case
A=testmat3(10000+0.01*%i,30);
[Ab,X]=balanc(A);
if Err(diag(diag(X))-X)>200*%eps then pause,end 
if Err(Ab-inv(X)*A*X)>1000*%eps then pause,end 

// PENCILS
//Small dimension
//---------------
A=[1/2^10,1/2^10;2^10,2^10];
B=A; 
[Ab,Bb,X,Y]=balanc(A,B);
if Err(Bb-X'*B*Y)>200*%eps then pause,end 
if Err(Ab-X'*A*Y)>200*%eps then pause,end 

//Real Case
A=testmat3(10000,5);B=testmat3(1000,5);
[Ab,Bb,X,Y]=balanc(A,B);
if Err(Bb-X'*B*Y)>200*%eps then pause,end 
if Err(Ab-X'*A*Y)>200*%eps then pause,end 

//complex case
A=testmat3(10000+0.001*%i,5);B=testmat3(1000+100*%i,5);
[Ab,Bb,X,Y]=balanc(A,B);
if Err(Bb-X'*B*Y)>200*%eps then pause,end 
if Err(Ab-X'*A*Y)>200*%eps then pause,end 

//Large dimension
//---------------
//Real Case
A=testmat3(10000,20);B=testmat3(1000,20);
[Ab,Bb,X,Y]=balanc(A,B);
if Err(Bb-X'*B*Y)>1000*%eps then pause,end 
if Err(Ab-X'*A*Y)>1000*%eps then pause,end 

//complex case
A=testmat3(10000+0.001*%i,20);B=testmat3(1000+100*%i,20);
[Ab,Bb,X,Y]=balanc(A,B);
if Err(Bb-X'*B*Y)>1000*%eps then pause,end 
if Err(Ab-X'*A*Y)>1000*%eps then pause,end 


