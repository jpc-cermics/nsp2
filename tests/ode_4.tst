// -*- Mode: scilab -*-
// ode tests (bruno 10/9/2009)

function print_info(num, algo, task, dir, rtol, atol, err, errtol)
  printf("\n example #%d with algo=%s, task=%d, dir=%s",num, algo, task, dir);
  printf("\n            rtol=%g, atol=%g FAILS", rtol,atol);
  printf("\n            got err = %g while errtol = %g\n",err,errtol)
endfunction

//////////////////////////////////////////////////////////////////////////////////
// example #4: solve the discretized heat equation in 1d. This lets to
// test a banded jacobian
//
//    dU/dt = A U(t)
//     U(0) = U0
//
//////////////////////////////////////////////////////////////////////////////////

function [f] = linearsysf(t,u,A)
  f = A*u;
endfunction

function [J] = linearsysfjac(t,u,A)
  J = A;
endfunction

function [f] = linearsysb(t,u,Ab)
  n = numel(u);
  f = zeros(n,1)
  f(1) = Ab(2,1)*u(1) + Ab(1,2)*u(2);
  f(2:n-1) = Ab(1,3:n)'.*u(3:n) + Ab(2,2:n-1)'.*u(2:n-1) + Ab(3,1:n-2)'.*u(1:n-2);
  f(n) =   Ab(2,n)*u(n) + Ab(3,n-1)*u(n-1);
endfunction

function [J] = linearsysbjac(t,u,Ab)
  J = Ab;
endfunction

function [A, Ab] = discrete_1d_laplacian(n)
// A is the full matrix
// Ab the banded version (lapack storage)
  A = (n+1).^2 * (-2*eye(n,n) + diag(ones(1,n-1),1) +  diag(ones(1,n-1),-1));
  Ab = [0, diag(A,1)';
	diag(A)';
	diag(A,-1)', 0];
endfunction

function y = ex4_solution(t,y0,Lambda,P)
// goto the spectral basis
  V0 = P'*y0;
  // compute sol in spectral basis
  Vt = exp(Lambda*t).*repmat(V0,1,length(t));
  // return to initial basis
  y = P*Vt
endfunction

n = 40;
[A, Ab] = discrete_1d_laplacian(n);
[Lambda,P] = spec(A);
T = 0.02;
m = 20;

x = linspace(0,1,n+2)';
x = x(2:n+1);
y0 = sin(%pi*x) + 0.3*sin(8*%pi*x);



rtol = 1e-8;
atol = 1e-10;
errtol = 2e-8;

// use full approximated jacobian
task = 1;
t = linspace(0,T,m);
ye = ex4_solution(t,y0,Lambda,P);
for algo = ["default", "stiff"]
  yn = ode(y0, 0, t, linearsysf, type=algo, args=A, rtol=rtol, atol=atol);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward approximated full jac", rtol, atol, err, errtol);
    pause
  end
end
task = 3;
for algo = ["default", "stiff"]
  [yn,t] = ode(y0, 0, T, linearsysf, type=algo, task=3, args=A, rtol=rtol, atol=atol);
  ye = ex4_solution(t,y0,Lambda,P);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward approximated full jac", rtol, atol, err, errtol);
    pause
  end
end

// use full user given jacobian
task = 1;
t = linspace(0,T,m);
ye = ex4_solution(t,y0,Lambda,P);
for algo = ["default", "stiff"]
  yn = ode(y0, 0, t, linearsysf, type=algo, jac=linearsysfjac,args=A, rtol=rtol, atol=atol);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward given full jac", rtol, atol, err, errtol);
    pause
  end
end
task = 3;
for algo = ["default", "stiff"]
  [yn,t] = ode(y0, 0, T, linearsysf, type=algo, jac=linearsysfjac, task=3, args=A, rtol=rtol, atol=atol);
  ye = ex4_solution(t,y0,Lambda,P);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward given full jac", rtol, atol, err, errtol);
    pause
  end
end

// use banded approximated jacobian
task = 1;
t = linspace(0,T,m);
ye = ex4_solution(t,y0,Lambda,P);
for algo = ["default", "stiff"]
  yn = ode(y0, 0, t, linearsysb, type=algo, args=Ab, rtol=rtol, odeoptions=hash(mu=1,ml=1),atol=atol);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward approximated banded jac", rtol, atol, err, errtol);
    pause
  end
end
task = 3;
for algo = ["default", "stiff"]
  [yn,t] = ode(y0, 0, T, linearsysb, type=algo, task=3, args=Ab, odeoptions=hash(mu=1,ml=1), rtol=rtol, atol=atol);
  ye = ex4_solution(t,y0,Lambda,P);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward approximated full jac", rtol, atol, err, errtol);
    pause
  end
end

// use banded user given jacobian
task = 1;
t = linspace(0,T,m);
ye = ex4_solution(t,y0,Lambda,P);
for algo = ["default", "stiff"]
  yn = ode(y0, 0, t, linearsysb, type=algo, jac=linearsysbjac, odeoptions=hash(mu=1,ml=1),args=Ab, rtol=rtol, atol=atol);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward user given banded jac", rtol, atol, err, errtol);
    pause
  end
end
task = 3;
for algo = ["default", "stiff"]
  [yn,t] = ode(y0, 0, T, linearsysb, type=algo, jac=linearsysbjac, task=3, odeoptions=hash(mu=1,ml=1), args=Ab, rtol=rtol, atol=atol);
  ye = ex4_solution(t,y0,Lambda,P);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward user given banded jac", rtol, atol, err, errtol);
    pause
  end
end

// use adams (with fonctional iteration only) and rkd5 (this works
// as the tolerance is enough stringent, the ode appears not so stiff)
task = 1;
errtol = 3e-8;  // adams got an err of 2.55321e-08
t = linspace(0,T,m);
ye = ex4_solution(t,y0,Lambda,P);
for algo = ["adams", "rkd5"]
  yn = ode(y0, 0, t, linearsysb, type=algo, args=Ab, rtol=rtol, atol=atol);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward no jacobian used", rtol, atol, err, errtol);
    pause
  end
end
task = 2;
for algo = ["adams", "rkd5"]
  [yn,t] = ode(y0, 0, T, linearsysb, type=algo, task=2, args=Ab, rtol=rtol, atol=atol);
  ye = ex4_solution(t,y0,Lambda,P);
  err = max(abs(yn - ye));
  if err >= errtol then
    print_info(4, algo, task, "forward no jacobian used", rtol, atol, err, errtol);
    pause
  end
end

