// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================    /         ============================== 
//==========================================================================
// XXX to be done 
if %f then 
//function x=rdiv(A,B),x=slash(A,B),endfunction
function x=rdiv(A,B),x=A/B,endfunction
//scalar division
  
//-----Square matrix-----
n=5;  A=randn(n,n);b=randn(2,n+1);
if rdiv([],A) <>[] then pause,end
if execstr('rdiv(b,A)',errcatch=%t)==%t then pause,end
//Small dimensions real
n=5;
b=randn(2,n);A=randn(n,n);
x=rdiv(b,A);
if Err(x*A-b)>200*%eps then pause,end
//Small dimensions complex
b=randn(2,n)+%i;A=randn(n,n);
x=rdiv(b,A);
if Err(x*A-b)>500*%eps then pause,end

b=randn(2,n);A=randn(n,n)+%i;
x=rdiv(b,A);
if Err(x*A-b)>500*%eps then pause,end

b=randn(2,n)+%i;A=randn(n,n)+%i;
x=rdiv(b,A);
if Err(x*A-b)>500*%eps then pause,end

//Large dimensions real
n=50;
b=randn(2,n);A=randn(n,n);
x=rdiv(b,A);
if Err(x*A-b)>10000*%eps then pause,end
//Small dimensions complex
b=randn(2,n)+%i;A=randn(n,n);
x=rdiv(b,A);
if Err(x*A-b)>10000*%eps then pause,end

b=randn(2,n);A=randn(n,n)+%i;
x=rdiv(b,A);
if Err(x*A-b)>10000*%eps then pause,end

b=randn(2,n)+%i;A=randn(n,n)+%i;
x=rdiv(b,A);
if Err(x*A-b)>10000*%eps then pause,end

//-----Rectangular matrix-----
n=5;m=3; A=randn(m,n);b=randn(2,n+1);
if rdiv([],A) <>[] then pause,end
if execstr('rdiv(b,A)',errcatch=%t)==%t then pause,end

//Small dimensions real
n=5;m=3;
b=randn(2,n);A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>200*%eps then pause,end

n=3;m=5;
b=randn(2,n);A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>200*%eps then pause,end
//Small dimensions complex
n=5;m=3;
b=randn(2,n)+%i;A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>200*%eps then pause,end
n=5;m=3;
b=randn(2,n);A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>200*%eps then pause,end
b=randn(2,n)+%i;A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>200*%eps then pause,end

n=3;m=5;
b=randn(2,n)+%i;A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

n=3;m=5;
b=randn(2,n);A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

n=3;m=5;
b=randn(2,n)+%i;A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

//LArge dimension real

n=50;m=30;
b=randn(2,n);A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

n=30;m=50;
b=randn(2,n);A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end
//Large dimensions complex
n=50;m=30;
b=randn(2,n)+%i;A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end
n=50;m=30;
b=randn(2,n);A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end
b=randn(2,n)+%i;A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

n=30;m=50;
b=randn(2,n)+%i;A=randn(m,n);
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

n=30;m=50;
b=randn(2,n);A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end

n=30;m=50;
b=randn(2,n)+%i;A=randn(m,n)+%i;
x=rdiv(b,A);
if Err(x*A*A'-b*A')>1000*%eps then pause,end
end 
