// some tests for A\b (Bruno 21 March 2005)

// Note that currently A\b is not supported with A or b a scalar
// (while the other is not). 

function [b] = isreal(A)  // needed for the test but still not in nsp
   b = and(imag(A) == 0)
endfunction

function [st] = subtype(A)
   if isreal(A) then st = "real", else, st = "cmplx", end
endfunction

function [x]=test_bdiv(A,b,num_test,normtype)
   [m,n] = size(A)
   [mm,nrhs] = size(b)
   printf("\n\n test %d: m=%d, n=%d, nrhs=%d, A is %s, b is %s", ...
	  num_test,m,n,nrhs,subtype(A),subtype(b))
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
   
   printf("\n       ||%s|| = ",strr)
   for i=1:size(x,2), printf(" %e , ",e(i)), end
   printf("\n ||%s||/||b|| = ",strr)
   for i=1:size(x,2), printf(" %e , ",er(i)), end
endfunction

rand('normal')
normtype="l1";

// square matrices

//Small dimensions real
n=5;
b=rand(n,2);A=rand(n,n);
num_test = 1;
[x]=test_bdiv(A,b,num_test,normtype);

//Small dimensions complex
b=rand(n,2)+%i;A=rand(n,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);

b=rand(n,2);A=rand(n,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);

b=rand(n,2)+%i;A=rand(n,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);

//Large dimensions real
n=50;
b=rand(n,2);A=rand(n,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);

//Small dimensions complex
b=rand(n,2)+%i;A=rand(n,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(n,2);A=rand(n,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(n,2)+%i;A=rand(n,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


//-----Rectangular matrix-----

//Small dimensions real
n=5;m=3;
b=rand(m,2);A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


n=3;m=5;
b=rand(m,2);A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


//Small dimensions complex
n=5;m=3;
b=rand(m,2)+%i;A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


n=5;m=3;
b=rand(m,2);A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(m,2)+%i;A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


n=3;m=5;
b=rand(m,2)+%i;A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


n=3;m=5;
b=rand(m,2);A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


n=3;m=5;
b=rand(m,2)+%i;A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


//LArge dimension real

n=40;m=20;
b=rand(m,2);A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(m,2);A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);

//Large dimensions complex

b=rand(m,2)+%i;A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);

b=rand(m,2);A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(m,2)+%i;A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(m,2)+%i;A=rand(m,n);
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(m,2);A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);


b=rand(m,2)+%i;A=rand(m,n)+%i;
num_test = num_test + 1;
[x]=test_bdiv(A,b,num_test,normtype);
