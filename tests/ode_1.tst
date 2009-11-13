// -*- Mode: scilab -*-
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


