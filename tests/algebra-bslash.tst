// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================    \         ============================== 
//==========================================================================
function x=ldiv(A,B),x=A\B,endfunction
//scalar division
  
//-----Square matrix-----
n=5;  A=randn(n,n);b=randn(2,n+1);
if execstr('ldiv(A,B)',errcatch=%t)==%t then pause,end
//Small dimensions real
n=5;
b=randn(n,2);A=randn(n,n);
x=ldiv(A,b);
if Err(A*x-b)>200*%eps then pause,end
//Small dimensions complex
b=randn(n,2)+%i;A=randn(n,n);
x=ldiv(A,b);
if Err(A*x-b)>500*%eps then pause,end

b=randn(n,2);A=randn(n,n)+%i;
x=ldiv(A,b);
if Err(A*x-b)>200*%eps then pause,end


b=randn(n,2)+%i;A=randn(n,n)+%i;
x=ldiv(A,b);
if Err(A*x-b)>200*%eps then pause,end


//Large dimensions real
n=50;
b=randn(n,2);A=randn(n,n);
x=ldiv(A,b);
if Err(A*x-b)>10000*%eps then pause,end
//Small dimensions complex
b=randn(n,2)+%i;A=randn(n,n);
x=ldiv(A,b);
if Err(A*x-b)>50000*%eps then pause,end

b=randn(n,2);A=randn(n,n)+%i;
x=ldiv(A,b);
if Err(A*x-b)>50000*%eps then pause,end

b=randn(n,2)+%i;A=randn(n,n)+%i;
x=ldiv(A,b);
if Err(A*x-b)>50000*%eps then pause,end

//-----Rectangular matrix-----
n=5;m=3; A=randn(m,n);b=randn(n+1,2);
//if ldiv(A,[]) <>[] then pause,end
if execstr('ldiv(A,b)',errcatch=%t)==%t then pause,end

//Small dimensions real
n=5;m=3;
b=randn(m,2);A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>200*%eps then pause,end

n=3;m=5;
b=randn(m,2);A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end
//Small dimensions complex
n=5;m=3;
b=randn(m,2)+%i;A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end
n=5;m=3;
b=randn(m,2);A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end
b=randn(m,2)+%i;A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end

n=3;m=5;
b=randn(m,2)+%i;A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end

n=3;m=5;
b=randn(m,2);A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end

n=3;m=5;
b=randn(m,2)+%i;A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>1000*%eps then pause,end

//LArge dimension real

n=40;m=20;
b=randn(m,2);A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end

b=randn(m,2);A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end
//Large dimensions complex

b=randn(m,2)+%i;A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end


b=randn(m,2);A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end

b=randn(m,2)+%i;A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end


b=randn(m,2)+%i;A=randn(m,n);
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end

b=randn(m,2);A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end

b=randn(m,2)+%i;A=randn(m,n)+%i;
x=ldiv(A,b);
if Err(A'*A*x-A'*b)>10000*%eps then pause,end
