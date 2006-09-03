// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction
rand('normal')

//==========================================================================
//==============================   det        ============================== 
//==========================================================================
// XXXXXXX 
if %f then 

if execstr('det([1 2;3 4;5 6])',errcatch=%t)==%t then pause,end
//Small dimension 
//Real
A=[1 1; 1 2];
if Err(det(A)-1)>10*%eps then pause,end
[e,m]=det(A);
if e<>0 |Err(m-1)>10*%eps then pause,end
//Complex
A=A+%i;
if Err(det(A)-1-%i)>10*%eps then pause,end
[e,m]=det(A);
if e<>0 |Err(m-1-%i)>10*%eps then pause,end
//Large dimension
//Real
v=rand(1,21);
A=rand(21,21); A=(triu(A,1)+diag(v))*(tril(A,-1)+diag(ones(1,21)));
if Err(det(A)-prod(v))>400000*%eps then pause,end
[e,m]=det(A);
if Err(m*(10^e)-prod(v))>400000*%eps then pause,end
//Complex
v=(v+rand(v)*%i)/2;
A=rand(21,21); A=(triu(A,1)+diag(v))*(tril(A,-1)+diag(ones(1,21)));
if Err(det(A)-prod(v))>10000*%eps then pause,end
[e,m]=det(A);
if Err(m*(10^e)-prod(v))>10000*%eps then pause,end

end 
