// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================     lsq      ============================== 
//==========================================================================

//Empty matrix

if lsq([],[])<>[] then pause,end
if execstr('lsq([],1)',errcatch=%t)==%t then pause,end
if execstr('lsq(1,[])',errcatch=%t)==%t then pause,end
if execstr('lsq(randn(3,2),randn(2,1))',errcatch=%t)==%t then pause,end

//Small dimensions
//Real full rank fat
A=randn(3,5);b=randn(3,2);
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end
//Real rank deficient fat
A=[1 2 3;1 2 3];b=[4;5];
X=lsq(A,b);
if Err(A'*A*X-A'*b)> 200*%eps then pause,end
//Real  tall
A=[1 2;4 2;0 1];b=[1;1;1];
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:2,:))*b1(1:2);
if Err(X-X1)>200*%eps then pause,end
//Complex full rank fat
A=randn(3,5)+%i*randn(3,5);b=randn(3,2);
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end

A=randn(3,5);b=randn(3,2)+%i*randn(3,2);
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end

A=randn(3,5)+%i*randn(3,5);b=randn(3,2)+%i*randn(3,2);
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end
//Complex  rank deficient fat
A=[1 2 3;1 2 3]+%i;b=[4;5];
X=lsq(A,b);
A=[1 2 3;1 2 3];b=[4;5]+%i;
X=lsq(A,b);
if Err(A'*A*X-A'*b)>200*%eps then pause,end

if Err(A'*A*X-A'*b)>200*%eps then pause,end
A=[1 2 3;1 2 3]+%i;b=[4;5]+%i;
X=lsq(A,b);
if Err(A'*A*X-A'*b)>1000*%eps then pause,end

//Complex  full rank tall
A=[1 2;4 2;0 1]+%i;b=[1;1;1];
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:2,:))*b1(1:2);
if Err(X-X1)>200*%eps then pause,end

A=[1 2;4 2;0 1];b=[1;1;1]+%i;
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:2,:))*b1(1:2);
if Err(X-X1)>200*%eps then pause,end

A=[1 2;4 2;0 1]+%i;b=[1;1;1]+%i;
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:2,:))*b1(1:2);
if Err(X-X1)>200*%eps then pause,end



//LArge dimension
//Real full rank fat
A=randn(3,50);b=randn(3,2);
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end
//Real full rank tall
A=randn(50,3);b=randn(50,2);
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:3,:))*b1(1:3,:);
if Err(X-X1)>200*%eps then pause,end

//Complex full rank fat
A=randn(3,50)+%i;b=randn(3,2);
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end
A=randn(3,50);b=randn(3,2)+%i;
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end

A=randn(3,50);b=randn(3,2)+%i;
X=lsq(A,b);
if Err(A*X-b)>200*%eps then pause,end
//Complex full rank tall
A=randn(50,3)+%i;b=randn(50,2);
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:3,:))*b1(1:3,:);
if Err(X-X1)>200*%eps then pause,end

A=randn(50,3);b=randn(50,2)+%i;
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:3,:))*b1(1:3,:);
if Err(X-X1)>200*%eps then pause,end
A=randn(50,3)+%i;b=randn(50,2)+%i;
X=lsq(A,b);
[Q,R]=qr(A);b1=Q'*b;X1=inv(R(1:3,:))*b1(1:3,:);
if Err(X-X1)>200*%eps then pause,end
