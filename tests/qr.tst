// -*- Mode: scilab -*- 
// tests for qr (Bruno July 06 2005). Note that 
// this should be integrated one day in the algebra.tst file...

eps_qr = 1.e-14;
norm_type = 1;

// FIRST PART : verify that Q * R = A (qr without columns permutation) or 
//              Q * R = A * P  (qr with columns permutation)
//              Remark : in nsp the permutation is returned as a
//                       permutation p and not as a associated matrix permutation P
//                       so A * P is done with A(:,p)

// square real matrix
A = rand(8,8);

[Q,R] = qr(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// square complex matrix
A = rand(8,8) + %i*rand(8,8);

[Q,R] = qr(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular real matrice m x n with m > n
A = rand(9,5);

[Q,R] = qr(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular complex matrice m x n with m > n
A = rand(9,5) + %i*rand(9,5);

[Q,R] = qr(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end


// rectangular real matrice m x n with m < n
A = rand(5,9);

[Q,R] = qr(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular complex matrice m x n with m < n
A = rand(5,9) + %i*rand(5,9);

[Q,R] = qr(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end


//
// SECOND PART : some tests with rank detection
// --------------------------------------------

// with real matrices

// here the matrix should be full rank
m = 20; n = 10;
A = rand(m,n);
[Q,R,p,rk] = qr(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a column so that it is a linear
// combinaison of some others
A(:,3) = 2*A(:,1) +0.7*A(:,n-1) -0.4*A(:,n);
[Q,R,p,rk] = qr(A,mode="e",tol=10*%eps);
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(A,tol=10*%eps); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// here the matrix should be full rank
m = 10; n = 20;
A = rand(m,n);
[Q,R,p,rk] = qr(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a line so that it is a linear
// combinaison of some others
A(3,:) = 2*A(1,:) +0.7*A(m-1,:) -0.4*A(m,:);
[Q,R,p,rk] = qr(A,mode="e",tol=10*%eps);
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(A,tol=10*%eps); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end


// with complex matrices

// here the matrix should be full rank
m = 20; n = 10;
A = rand(m,n)+%i*rand(m,n);
[Q,R,p,rk] = qr(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a column so that it is a linear
// combinaison of some others
A(:,3) = (2+%i)*A(:,1) +0.7*A(:,n-1) -0.4*%i*A(:,n);
[Q,R,p,rk] = qr(A,mode="e",tol=10*%eps);
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(A,tol=10*%eps); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// here the matrix should be full rank
m = 10; n = 20;
A = rand(m,n)+%i*rand(m,n);
[Q,R,p,rk] = qr(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a line so that it is a linear
// combinaison of some others
A(3,:) = (2+%i)*A(1,:) +0.7*A(m-1,:) -0.4*%i*A(m,:);
[Q,R,p,rk] = qr(A,mode="e",tol=10*%eps);
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(A,tol=10*%eps); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end


//
// THIRD PART : verify that the approximated min and max singular
//              values are OK. We impose only a 100% relative accuracy
//              (observations shows that smax is good but smin
//               may be badly approximate, nevertheless this is
//               useful to have an idea of the condition number 
//               in 2-norm and for other things)
racc = 1;
// for real matrix
A = rand(5,5);
[U,R] = qr(A); // just to get an orthononal matrice U ...
A = rand(5,5);
[V,R] = qr(A); // just to get an other orthononal matrice V ...
sg = [100, 1, 0.01, 0.0001, 0.000001];
A = U*diag(sg)*V;
[Q,R,p,rk,sval] = qr(A);

if abs(sval(1)-sg(1))/sg(1) > racc then pause; end
if abs(sval(2)-sg($))/sg($) > racc then pause; end

// now we impose a tol = 1.e-7 to say that the matrix have rank 4
// in place of rank 5 and to have an estimation of the 2 smallest
// singular values.
[Q,R,p,rk,sval] = qr(A,tol=1.e-7);

if rk ~= 4 then pause;end
if abs(sval(1)-sg(1))/sg(1) > racc then pause; end
if abs(sval(2)-sg($-1))/sg($-1) > racc then pause; end
if abs(sval(3)-sg($))/sg($) > racc then pause; end


// for complex matrix
A = rand(5,5)+%i*rand(5,5);
[U,R] = qr(A); // just to get an orthononal matrice U ...
A = rand(5,5)+%i*rand(5,5);
[V,R] = qr(A); // just to get an other orthononal matrice V ...
sg = [100, 1, 0.01, 0.0001, 0.000001];
A = U*diag(sg)*V;
[Q,R,p,rk,sval] = qr(A);

if abs(sval(1)-sg(1))/sg(1) > racc then pause; end
if abs(sval(2)-sg($))/sg($) > racc then pause; end

// now we impose a tol = 1.e-7 to say that the matrix have rank 4
// in place of rank 5 and to have an estimation of the 2 smallest
// singular values.
[Q,R,p,rk,sval] = qr(A,tol=1.e-7);

if rk ~= 4 then pause;end
if abs(sval(1)-sg(1))/sg(1) > racc then pause; end
if abs(sval(2)-sg($-1))/sg($-1) > racc then pause; end
if abs(sval(3)-sg($))/sg($) > racc then pause; end




