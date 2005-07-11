// -*- Mode: scilab -*- 
// tests for svd (Bruno July 11 2005). Note that 
// this should be integrated one day in the algebra.tst file...


// FIRST PART : - verify that in, s = svd(A) and [U,sv,V] = svd(A,mode="e")
//                that s and sv are enought closed (must be equal in theory
//                but this is not always the same lapack routine which is used)
//              - verify for  [U,sv,V] = svd(A,mode="e") that
//                U*diag(sv)*V' is enough closed to A
//              - verify for  [U,sv,V] = svd(A) that
//                U(:,p)*diag(sv)*(V(:,p))' is enough closed to A 
//                (p = min(m,n))
tols = 1.e-13;
tol = 1.e-12;

m = 60; n = 7;
A = rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 60; n = 7;
A = rand(m,n) + %i*rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 70; n = 68;
A = rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 70; n = 68;
A = rand(m,n) + %i*rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 13; n = 43;
A = rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 13; n = 43;
A = rand(m,n) + %i*rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 130; n = 132;
A = rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end

m = 130; n = 132;
A = rand(m,n) + %i*rand(m,n);
s = svd(A);
[U,sv,V] = svd(A,mode="e");
if max(abs( (s - sv)./(sv+0.001) )) > tols then, pause; end
if norm( U*diag(sv)*V' - A , 1)/norm(A) > tol then, pause, end
[U,sv,V] = svd(A);
p = min(m,n);
if norm( U(:,1:p)*diag(sv)*(V(:,1:p))' - A , 1)/norm(A) > tol then, pause, end


// SECOND PART :  tests on rank : on a random m x n matrix we force
//                when m > n, some columns to be a linear combinaison 
//                of some others and when m < n, some rows ...

m = 60; n = 17; // real case
A = rand(m,n);
A(:,3) = A(:,1)+0.5*A(:,2);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= n-1 ) then, pause, end

A(:,10:17) = A(:,1:8)*rand(8,8);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= 8 ) then, pause, end

m = 60; n = 17; // complex case
A = rand(m,n) + %i*rand(m,n);
A(:,3) = A(:,1)+(0.5-%i)*A(:,2);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= n-1 ) then, pause, end

A(:,10:17) = A(:,1:8)*(rand(8,8) + %i*rand(8,8));
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= 8 ) then, pause, end


m = 30; n = 60; // real case
A = rand(m,n);
A(4,:) = A(1,:)+0.5*A(2,:);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= m-1 ) then, pause, end

A(2:2:30,:) = rand(15,15)*A(1:2:29,:);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= 15 ) then, pause, end

m = 30; n = 60; // complex case
A = rand(m,n);
A(4,:) = A(1,:)+(0.5+0.3*%i)*A(2,:);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= m-1 ) then, pause, end

A(2:2:30,:) = (rand(15,15) + %i*rand(15,15))*A(1:2:29,:);
[U,sv,V,rk] = svd(A,mode="e");
if ( rk ~= 15 ) then, pause, end


// THIRD PART :  we compute a matrix A from its svd, and see if the singular
//               values of A computed by svd is closed enough from the original

// build a matrix A from its svd
m = 45; n = 30;
B = rand(m,n);
[U,R] = qr(B,mode="e");
B = rand(n,n);
[V,R] = qr(B);
p = min(m,n);
sv = sort(abs(grand(p,1,"nor",0,1)));
A = U*diag(sv)*V';

[UU,svv,VV] = svd(A,mode="e");
if max(abs( (sv - svv)./(sv+0.001) )) > tols then, pause; end


m = 45; n = 30;
B = rand(m,n)+%i*rand(m,n);
[U,R] = qr(B,mode="e");
B = rand(n,n)+%i*rand(n,n);
[V,R] = qr(B);
p = min(m,n);
sv = sort(abs(grand(p,1,"nor",0,1)));
A = U*diag(sv)*V';

[UU,svv,VV] = svd(A,mode="e");
if max(abs( (sv - svv)./(sv+0.001) )) > tols then, pause; end


