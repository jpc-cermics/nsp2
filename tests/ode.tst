// ode tests (bruno 10/9/2009)


function print_info(num, algo, task, dir, rtol, atol, err, errtol)
   printf("\n example #%d with algo=%s, task=%d, dir=%s",num, algo, task, dir);
   printf("\n            rtol=%g, atol=%g FAILS", rtol,atol);
   printf("\n            got err = %g while errtol = %g\n",err,errtol)
endfunction


////////////////////////////////////////////////////////////////////////
// example #1: this is a non stiff example due to Felhberg, found in
// the Hairer,Wanner,Norsett book.
////////////////////////////////////////////////////////////////////////

function [f] = exfehlberg(t,y)
   f = [ 2*t*y(1)*log(max(y(2),1e-3));
	-2*t*y(2)*log(max(y(1),1e-3))]
endfunction

function y = ex1_solution(t)
   y = [ exp(sin(t.^2));
	 exp(cos(t.^2))];
endfunction

y0 = [1;%e];
T = 5;
rtol = [1e-6, 1e-10];
atol = [1e-8, 1e-12];
errtol = [3e-4, 2e-7];

for algo = ["adams", "default", "rkd5", "stiff"]

   task = 1;
   for j = 1:length(rtol)
      dir = "forward";
      t = linspace(0,T,200);
      y = ode(y0, 0, t, exfehlberg, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      err = max(abs(y - ex1_solution(t)));
      if err >= errtol(j) then
	 print_info(1, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
      // backward integration
      dir = "backward";
      tb = t($:-1:1);
      yb = ode(y(:,$), T, tb, exfehlberg, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      err = max(abs(yb -  ex1_solution(tb)));
      if err >= errtol(j) then
	 print_info(1, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end
   
   task = 2;
   for j = 1:length(rtol)
      dir = "forward";
      [y,t] = ode(y0, 0, T, exfehlberg, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if t($) < T, then, pause, end
      err = max(abs(y - ex1_solution(t)));
      if err >= errtol(j), then, pause, end
      if err >= errtol(j) then
	 print_info(1, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
      // backward integration
      dir = "backward";
      [yb,tb] = ode(y(:,$), t($), 0, exfehlberg, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if tb($) > 0, then, pause, end
      err = max(abs(yb - ex1_solution(tb)));
      if err >= errtol(j) then
	 print_info(1, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end

   task = 3;
   for j = 1:length(rtol)
      [y,t] = ode(y0, 0, T, exfehlberg, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if t($) ~= T, then, pause, end
      err = max(abs(y - ex1_solution(t)));
      if err >= errtol(j) then
	 print_info(1, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
      // backward integration
      [yb,tb] = ode(y(:,$), T, 0, exfehlberg, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if tb($) ~= 0, then, pause, end
      err = max(abs(yb - ex1_solution(tb)));
      if err >= errtol(j) then
	 print_info(1, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end

end



//////////////////////////////////////////////////////////////////////////////////
// example #2: the Jacobi functions sn, cn and dn  (found in
// the Hairer, Wanner, Norsett book). Exact solution is given
// by jacobi elliptic functions sn, dn, cn which are available
// in nsp (until now) with the cephes-amos-scipy toolbox...
// So this test is done only if the function scipy_ellpj is present
//////////////////////////////////////////////////////////////////////////////////
function b=isfunc(obj) 
  try 
    if type(obj).equal[%types.Func] || type(obj).equal[%types.PList] then
       b = %t
    else, b = %f, end
  catch 
    b = %f
  end
endfunction

if isfunc(scipy_ellpj) then

m = 0.51;

function f = jacobi(t,u,m)
   f = [   u(2)*u(3);...
	  -u(1)*u(3);...
	-m*u(1)*u(2) ]
endfunction

function y = ex2_solution(t,m)
   [y1,y2,y3] = scipy_ellpj(t,m);
   y = [y1; y2; y3] 
endfunction

y0 = [0;1;1];


T = 20;
rtol = [1e-6, 1e-10];
atol = [1e-8, 1e-12];
errtol = [3e-4, 1e-7];

for algo = ["adams", "default", "rkd5", "stiff"]

   task = 1;
   for j = 1:length(rtol)
      dir = "forward";
      t = linspace(0,T,200);
      y = ode(y0, 0, t, jacobi, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      err = max(abs(y - ex2_solution(t,m)));
      if err >= errtol(j) then
	 print_info(2, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
      // backward integration
      dir = "backward";
      tb = t($:-1:1);
      yb = ode(y(:,$), T, tb, jacobi, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      err = max(abs(yb -  ex2_solution(tb,m)));
      if err >= errtol(j) then
	 print_info(2, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end
   
   task = 2;
   for j = 1:length(rtol)
      dir = "forward";
      [y,t] = ode(y0, 0, T, jacobi, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if t($) < T, then, pause, end
      err = max(abs(y - ex2_solution(t,m)));
      if err >= errtol(j), then, pause, end
      if err >= errtol(j) then
	 print_info(2, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
      // backward integration
      dir = "backward";
      [yb,tb] = ode(y(:,$), t($), 0, jacobi, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if tb($) > 0, then, pause, end
      err = max(abs(yb - ex2_solution(tb,m)));
      if err >= errtol(j) then
	 print_info(2, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end

   task = 3;
   for j = 1:length(rtol)
      dir = "forward";
      [y,t] = ode(y0, 0, T, jacobi, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if t($) ~= T, then, pause, end
      err = max(abs(y - ex2_solution(t,m)));
      if err >= errtol(j) then
	 print_info(2, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
      // backward integration
      dir = "backward";
      [yb,tb] = ode(y(:,$), T, 0, jacobi, task = task, type = algo, rtol=rtol(j), atol=atol(j));
      if tb($) ~= 0, then, pause, end
      err = max(abs(yb - ex2_solution(tb,m)));
      if err >= errtol(j) then
	 print_info(2, algo, task, dir, rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end

end

end // if isfunc(scipy_ellpj)



//////////////////////////////////////////////////////////////////////////////////
// example #3: the vanderpol from the Geneva test suite (Hairer and Wanner)
// this is a stiff example.
//////////////////////////////////////////////////////////////////////////////////

function [f] = vanderpol2(t,u,alpha)
   // usual vanderpol but with the time scaling t -> t' = t/eps (alpha = 1/eps^2) 
   f = [ u(2);...
	 ((1-u(1)^2)*u(2) - u(1))/alpha ];
endfunction

function [J] = vanderpol2jac(t,u,alpha)
   J = [  0                   , 1;...
	 -(2*u(1)*u(2)+1)/alpha , (1-u(1)^2)/alpha ];
endfunction

// exact solution for alpha = 1e-6, u0 = [2;0], and at time 1, 2, ..., 11
// provided at Ernst Hairer 's home page
// (http://www.unige.ch/~hairer/testset/stiff/vdpol/res_exact_pic)
uex = [      
  -0.1863646254808130e+01, ...
   0.7535430865435460e+00, ...
   0.1706167732170456e+01, ...
  -0.8928097010248257e+00, ...
  -0.1510606936744095e+01, ...
   0.1178380000730945e+01, ...
   0.1194414677695905e+01, ...
  -0.2799585996540082e+01, ...
   0.1890428596416747e+01, ...
  -0.7345118680166940e+00, ...
  -0.1737716306805883e+01, ...
   0.8604008653025923e+00, ...
   0.1551614645548223e+01, ...
  -0.1102382892533321e+01, ...
  -0.1278631984330405e+01, ...
   0.2013890883009155e+01, ...
  -0.1916552949489830e+01, ...
   0.7169573003463228e+00, ...
   0.1768163792391936e+01, ...
  -0.8315276407898496e+00, ...
  -0.1590150544829062e+01, ...
   0.1040279389212485e+01 ];

uex.redim[2,-1];

t = 1:11;
u0 = [2;0];
alpha = 1e-6;

rtol = [1e-6, 1e-10];
atol = [1e-8, 1e-12];
errtol = [8e-4, 2e-7];

for algo = ["default", "stiff"]
   for j = 1:length(rtol)
      un = ode(u0, 0, t, vanderpol2, type=algo, jac = vanderpol2jac, args=alpha, ...
	       rtol=rtol(j), atol=atol(j));
      err = max(abs(un - uex));
      if err >= errtol(j) then
	 print_info(3, algo, 1, "forward with jac", rtol(j), atol(j), err, errtol(j));
	 pause
      end
      un = ode(u0, 0, t, vanderpol2, type=algo, args=alpha, rtol=rtol(j), atol=atol(j));
      err = max(abs(un - uex));
      if err >= errtol(j) then
	 print_info(3, algo, 1, "forward without jac", rtol(j), atol(j), err, errtol(j));
	 pause
      end
   end
end

// test if rkd5 detect stiffness (error code is 7...)
[un, ier] = ode(u0, 0, t, vanderpol2, type="rkd5", rtol=1e-6, atol=1e-10, ...
		warn=%f);
if ier ~= 7 then, pause, end

// disable the stiffness test using nstiff=0 (so mxstep should be reached
// and now error code is 1)
[un, ier] = ode(u0, 0, t, vanderpol2, type="rkd5", rtol=1e-6, atol=1e-10, ...
		warn=%f,odeoptions=hash(nstiff=0));
if ier ~= 1 then, pause, end


// tests adams with fonctional iteration  (jacobian is not given and not
// approximated) so step size become small and more than maxstep iterations are
// needed (error code is 1)
[un, ier] = ode(u0, 0, 1, vanderpol2, type="adams", rtol=1e-6, atol=1e-10, ...
		warn=%f);
if ier ~= 1 then, pause, end

// the same example should pass if the jacobian is given
un = ode(u0, 0, t, vanderpol2, type="adams", jac=vanderpol2jac,rtol=1e-8,atol=1e-10, warn=%f);
err = max(abs(un - uex));
if err >= 0.00005 then
   print_info(3, "adams", 1, "forward with jac", 1e-8, 1e-10, err,  0.00005);
   pause
end



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

