// -*- Mode: scilab -*- 

// tests function for fsolve 
// the test function use pre-coded C functions.

testf=['rosenbrock'; 
       'powell_singular';
       'powell_badly_scaled';
       'wood';
       'helical_valley';
       'watson';
       'chebyquad';
       'brown';
       'discrete_boundary';
       'discrete_integral';
       'trigonometric';
       'variably_dimensioned';
       'broyden_tridiagonal';
       'broyden_banded';
       'fsol1'];

if ~c_link('minpack_rosenbrock') 
  link('nsp',['minpack_'+testf; 
	      'minpack_jac_'+testf]);
end 

// rosenbrock function.
// ------------------------

x =[ -1.2; 1];
[x,f,info]=fsolve(x,'minpack_rosenbrock');
if norm(f) > 1.e-8 then pause;end 

x =[ -1.2; 1];
[x,f,info]=fsolve(x,'minpack_rosenbrock',jac='minpack_jac_rosenbrock');
if norm(f) > 1.e-8 then pause;end 

// It is possible to use 
// fsolve_jac: to obtain a jacobian evaluation computed 
// internally by fsolve when jacobian is not given 
//
// for all tests:
// fsolve_test(x,'rosenbrock') to get at nsp level the value 
//    of the test function rosenbrock.
// fsolve_jac_test(x,'jac_rosenbrock')
//    to get at nsp level the value of the jacobian ofthe test 
//    function rosenbrock.

x =[ -1.2; 1];
[f,j]=fsolve_jac(x,'minpack_rosenbrock') ; // evaluated jacobian 
f1 = fsolve_test(x,'rosenbrock') ;
j1 = fsolve_jac_test(x,'jac_rosenbrock') ; // coded jacobian 

if norm(j-j1) > 1e-6 then pause;end 
if norm(f-f1) > 1e-8 then pause;end 

// rosenbrock test with mixed hard coded and soft coded function 

function y=rosenbrock(x); y = fsolve_test(x,'rosenbrock'); endfunction;
function y=jac_rosenbrock(x); y = fsolve_jac_test(x,'jac_rosenbrock'); endfunction;

x =[ -1.2; 1];
[x,f,info]=fsolve(x,rosenbrock);
if norm(f) > 1.e-8 then pause;end 

x =[ -1.2; 1];
[x,f,info]=fsolve(x,rosenbrock,jac='minpack_jac_rosenbrock');
if norm(f) > 1.e-8 then pause;end 

x =[ -1.2; 1];
[x,f,info]=fsolve(x,'minpack_rosenbrock',jac=jac_rosenbrock);
if norm(f) > 1.e-8 then pause;end 

x =[ -1.2; 1];
[x,f,info]=fsolve(x,rosenbrock,jac=jac_rosenbrock);
if norm(f) > 1.e-8 then pause;end 

//  powell singular function. 
// ------------------------
x =[ 3;-1;0.0; 1];
[x,f,info]=fsolve(x,'minpack_powell_singular');
if norm(f) > 1.e-8 then pause;end 

x =[ 3;-1;0.0; 1];
[x,f,info]=fsolve(x,'minpack_powell_singular',jac='minpack_jac_powell_singular');
if norm(f) > 1.e-8 then pause;end 

//     powell badly scaled function. 
// ------------------------
x= [0.0;1];
[x,f,info]=fsolve(x,'minpack_powell_badly_scaled');
if norm(f) > 1.e-8 then pause;end 


x= [0.0;1];
[x,f,info]=fsolve(x,'minpack_powell_badly_scaled',jac='minpack_jac_powell_badly_scaled');
if norm(f) > 1.e-8 then pause;end 


//     wood function.
// ------------------------
x=[ -3;-1;-3;-1];
[x,f,info]=fsolve(x,'minpack_wood');
if norm(f) > 1.e-8 then pause;end 


x=[ -3;-1;-3;-1];
[x,f,info]=fsolve(x,'minpack_wood',jac='minpack_jac_wood');
if norm(f) > 1.e-8 then pause;end 

//     helical valley function. */
// ------------------------
x = [-1;0;0];
[x,f,info]=fsolve(x,'minpack_helical_valley');
if norm(f) > 1.e-8 then pause;end 

x = [-1;0;0];
[x,f,info]=fsolve(x,'minpack_helical_valley',jac='minpack_jac_helical_valley');
if norm(f) > 1.e-8 then pause;end 

//     watson function.
// ------------------------
n=10;
x=zeros(n,1);
[x,f,info]=fsolve(x,'minpack_watson');
if norm(f) > 1.e-8 then pause;end 

n=10;
x=zeros(n,1);
[x,f,info]=fsolve(x,'minpack_watson',jac='minpack_jac_watson');
if norm(f) > 1.e-8 then pause;end 
  
//     chebyquad function.
// ------------------------
n=5;
x=(1:n)/(n+1);
[x,f,info]=fsolve(x,'minpack_chebyquad');
if norm(f) > 1.e-8 then pause;end 
  
n=5;
x=(1:n)/(n+1);
[x,f,info]=fsolve(x,'minpack_chebyquad',jac='minpack_jac_chebyquad');
if norm(f) > 1.e-8 then pause;end 
    
//     brown almost-linear function.
// ------------------------
n=10;
x=0.5*ones(n,1);
[x,f,info]=fsolve(x,'minpack_brown');
if norm(f) > 1.e-8 then pause;end 

n=10;
x=0.5*ones(n,1);
[x,f,info]=fsolve(x,'minpack_brown',jac='minpack_jac_brown');
if norm(f) > 1.e-8 then pause;end 

//     discrete boundary value and integral equation functions.
// ------------------------
n=10;
x=(1:n)/(n+1);
x=(x).*(x-1);
[x,f,info]=fsolve(x,'minpack_discrete_boundary');
if norm(f) > 1.e-8 then pause;end 
[x,f,info]=fsolve(x,'minpack_discrete_integral');
if norm(f) > 1.e-8 then pause;end 
  
n=10;
x=(1:n)/(n+1);
x=(x).*(x-1);
[x,f,info]=fsolve(x,'minpack_discrete_boundary',jac='minpack_jac_discrete_boundary');
if norm(f) > 1.e-8 then pause;end 
[x,f,info]=fsolve(x,'minpack_discrete_integral',jac='minpack_jac_discrete_integral');
if norm(f) > 1.e-8 then pause;end 
  
//     trigonometric function. 
// ------------------------
n=10;
x=ones(n,1)/n;
[x,f,info]=fsolve(x,'minpack_trigonometric');
if norm(f) > 1.e-2 then pause;end 

n=10;
x=ones(n,1)/n;
[x,f,info]=fsolve(x,'minpack_trigonometric',jac='minpack_jac_trigonometric');
if norm(f) > 1.e-2 then pause;end 
  
//     variably dimensioned function. 
// ------------------------
n=10;
x= 1 - (1:n)/n;
[x,f,info]=fsolve(x,'minpack_variably_dimensioned');
if norm(f) > 1.e-8 then pause;end 
  
n=10;
x= 1 - (1:n)/n;
[x,f,info]=fsolve(x,'minpack_variably_dimensioned',jac='minpack_jac_variably_dimensioned');
if norm(f) > 1.e-8 then pause;end 
  
//     broyden tridiagonal and banded functions. */
// ------------------------

n=10;
x= -ones(n,1);
[x,f,info]=fsolve(x,'minpack_broyden_tridiagonal');
if norm(f) > 1.e-7 then pause;end 
[x,f,info]=fsolve(x,'minpack_broyden_banded');
if norm(f) > 1.e-7 then pause;end 

n=10;
x= -ones(n,1);
[x,f,info]=fsolve(x,'minpack_broyden_tridiagonal',jac='minpack_jac_broyden_tridiagonal');
if norm(f) > 1.e-7 then pause;end 
[x,f,info]=fsolve(x,'minpack_broyden_banded',jac='minpack_jac_broyden_banded');
if norm(f) > 1.e-7 then pause;end 
  

