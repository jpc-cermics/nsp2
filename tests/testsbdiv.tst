// -*- Mode: scilab -*- 
// some tests for A\b (Bruno 21 March 2005)

// Note that currently A\b is not supported with A or b a scalar
// (while the other is not). 

eps_bdiv= 1.e-12;

function [st] = subtype(A)
   if isreal(A) then st = "real", else, st = "cmplx", end
endfunction

function [x,e,er]=test_bdiv(A,b,num_test,normtype)
   [m,n] = size(A)
   [mm,nrhs] = size(b)
//   printf("\n\n test %d: m=%d, n=%d, nrhs=%d, A is %s, b is %s", ...
//	  num_test,m,n,nrhs,subtype(A),subtype(b))
   x = A\b
   if m == n then
      r = A*x - b
      strr = "Ax-b"
   else
      r = A'*A*x - A'*b
      strr = "A''Ax-A''b"
   end
   
   if normtype == "l1" then
      e=sum(abs(r),"r")
      er = e./sum(abs(b),"r")
   elseif normtype == "l2" then
      e=sqrt(sum(abs(r).^2,"r"))
      er = e./sqrt(sum(abs(b).^2,"r"))
   else
      e=max(abs(r),"r")
      er=e./max(abs(b),"r")   
   end
   
//    printf("\n       ||%s|| = ",strr)
//    for i=1:size(x,2), printf(" %e , ",e(i)), end
//    printf("\n ||%s||/||b|| = ",strr)
//    for i=1:size(x,2), printf(" %e , ",er(i)), end
endfunction

normtype="l1";

// square matrices

//Small dimensions real
n=5;
b=randn(n,2);A=randn(n,n);
num_test = 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

//Small dimensions complex
b=randn(n,2)+%i;A=randn(n,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

b=randn(n,2);A=randn(n,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

b=randn(n,2)+%i;A=randn(n,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

//Large dimensions real
n=50;
b=randn(n,2);A=randn(n,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

//Large dimensions complex
b=randn(n,2)+%i;A=randn(n,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

b=randn(n,2);A=randn(n,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

b=randn(n,2)+%i;A=randn(n,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


// banded case

// A and b real
n=50;
b=randn(n,2);A=tril(triu(randn(n,n),-2),2);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

// A real, b complex
b= b + %i*randn(n,2);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

// A complex, b complex
A=A+%i*tril(triu(randn(n,n),-2),2);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

// A complex, b real
b=randn(n,2);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


//-----Rectangular matrix-----

//Small dimensions real
n=5;m=3;
b=randn(m,2);A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


n=3;m=5;
b=randn(m,2);A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


//Small dimensions complex
n=5;m=3;
b=randn(m,2)+%i;A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


n=5;m=3;
b=randn(m,2);A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


b=randn(m,2)+%i;A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


n=3;m=5;
b=randn(m,2)+%i;A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


n=3;m=5;
b=randn(m,2);A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


n=3;m=5;
b=randn(m,2)+%i;A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


//LArge dimension real

n=40;m=20;
b=randn(m,2);A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


b=randn(m,2);A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

//Large dimensions complex

b=randn(m,2)+%i;A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

b=randn(m,2);A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


b=randn(m,2)+%i;A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


b=randn(m,2)+%i;A=randn(m,n);
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


b=randn(m,2);A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end


b=randn(m,2)+%i;A=randn(m,n)+%i;
num_test = num_test + 1;
[x,e,err]=test_bdiv(A,b,num_test,normtype);
if err > eps_bdiv then pause;end

