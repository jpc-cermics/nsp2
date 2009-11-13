// -*- Mode: scilab -*-
// ode tests (bruno 10/9/2009)

function print_info(num, algo, task, dir, rtol, atol, err, errtol)
  printf("\n example #%d with algo=%s, task=%d, dir=%s",num, algo, task, dir);
  printf("\n            rtol=%g, atol=%g FAILS", rtol,atol);
  printf("\n            got err = %g while errtol = %g\n",err,errtol)
endfunction


//////////////////////////////////////////////////////////////////////////////////
// example #2: the Jacobi functions sn, cn and dn  (found in
// the Hairer, Wanner, Norsett book). Exact solution is given
// by jacobi elliptic functions sn, dn, cn which are available
// in nsp (until now) with the cephes-amos-scipy toolbox...
// So this test is done only if the function scipy_ellpj is present
//////////////////////////////////////////////////////////////////////////////////

if ~exists('scipy_ellpj') then quit;end 
  
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

