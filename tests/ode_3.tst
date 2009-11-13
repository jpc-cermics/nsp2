// -*- Mode: scilab -*-
// ode tests (bruno 10/9/2009)

function print_info(num, algo, task, dir, rtol, atol, err, errtol)
  printf("\n example #%d with algo=%s, task=%d, dir=%s",num, algo, task, dir);
  printf("\n            rtol=%g, atol=%g FAILS", rtol,atol);
  printf("\n            got err = %g while errtol = %g\n",err,errtol)
endfunction

//////////////////////////////////////////////////////////////////////////////////
// example #3: the vanderpol from the Geneva test suite (Hairer and Wanner)
// this is a stiff example.
//////////////////////////////////////////////////////////////////////////////////

function [f] = vanderpol2(t,u,alpha)
// usual vanderpol but with the time scaling t -> t' = t/eps (alpha = 1/eps^2) 
  f = [ u(2);
	((1-u(1)^2)*u(2) - u(1))/alpha ];
endfunction

function [J] = vanderpol2jac(t,u,alpha)
  J = [  0                   , 1;
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

