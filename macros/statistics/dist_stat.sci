// Copyright (C) Bruno Pincon
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

// computes mean and std of some probability distribution
//
// dist: string specifying the distribution (same strings
//       than the pdf, cdf, icdf or grand function)
//
// varargin: parameters of the specified distribution
//

function [m,sd] = dist_stat(dist, varargin)
   //
   //
   if nargin < 1 then 
      error("Error: dist_stat needs at least one input arguments")
   end
   
   if ~( is(dist,%types.SMat) && isscalar(dist) ) then
      error("Error: first argument should be a string")
   end
   
   select dist
     case "nor" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""nor"",mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2);
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for dist_stat(""nor"",mu,sigma), mu should be real and sigma a positive real")
	end
	m = mu; sd = sigma;
	
     case "chi" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""chi"",nu)")
	end
	nu = varargin(1);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ) then
	      error("Error: for dist_stat(""chi"",nu), nu should be a positive real")
	end
	m = nu; sd = sqrt(2*nu);
	
     case "nch" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""nch"",nu,lambda)")
	end
	nu = varargin(1);
	lambda = varargin(2);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ... 
	      && is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0 ) then
	      error("Error: for dist_stat(""nch"",nu,lambda), nu should be a positive real and lambda a non negative real")
	end
	m = nu + lambda; sd = sqrt(2*(nu+2*lambda));
	
     case "bet" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""bet"",a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for dist_stat(""bet"",a,b), a and b should be positive real")
	end
	//m = a/(a+b);
	m = 1/(1 + b/a);
	//sd = sqrt(a*b/(a+b+1))/(a+b);
	if a >= b then
	   sd = sqrt(b/(1 + (b+1)/a))/(a+b);
	else
	   sd = sqrt(a/(1 + (a+1)/b))/(a+b);
	end
	
     case "bin" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""bin"",n,p)")
	end
	n = varargin(1); p = varargin(2)   
	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n > 0 && floor(n)==n &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 <= p && p <= 1 ) then
	      error("Error: for dist_stat(""bin"",n,p), bad parameters n and/or p")
	end
	m = n*p; sd = sqrt(n*p*(1-p));
	
     case "nbn" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""nbn"",r,p)")
	end
	r = varargin(1); p = varargin(2)   
	if ~( is(r,%types.Mat) && isreal(r) && isscalar(r) && r > 0 &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 < p && p <= 1 ) then
	      error("Error: for dist_stat(""nbn"",r,p), bad parameters r and/or p")
	end
	m = r*(1-p)/p; sd = sqrt(r*(1-p))/p;
	
     case "gam" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""gam"",a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b >= 0 ) then
	      error("Error: for dist_stat(""gam"",a,b), a should be positive real and b a non negative real")
	end
	m = a/b; sd = sqrt(a)/b;
	
     case "logi" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""logi"",a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for dist_stat(""logi"",a,b), a should be a real and b a positive real")
	end
	m = a; sd = %pi*b/sqrt(3);
	
     case "par" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""par"",a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for dist_stat(""par"",a,b), a and b should be positive real")
	end
	if a > 1 then, m = a*b/(a-1), else, m = %inf, end
	if a > 2 then, sd = sqrt(a/(a-2))*b/(a-1), else, sd = %inf, end
	
     case "poi" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: dist_stat(""poi"",P,mu)")
	end
	mu = varargin(1);
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) && mu >= 0 ) then
	      error("Error: for dist_stat(""poi"",P,mu), mu should be a non negative real")
	end
	m = mu;
	sd = sqrt(mu);
     
     case "f" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""f"",nu1,nu2)")
	end
	nu1 = varargin(1); nu2 = varargin(2)   
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 > 0 ) then
	      error("Error: for dist_stat(""f"",nu1,nu2), nu1 and nu2 should be positive real")
	end
	if nu2 > 2 then, m = nu2/(nu2-2), else, m = %inf, end
	if nu2 > 4 then
	   sd = nu2*sqrt(2*(nu1+nu2-2)/(nu1*(nu2-4)))/(nu2-2)
	elseif nu2 > 2 then
	   sd = %inf
	else
	   sd = %nan
	end
	
     case "nf" then
	if numel(varargin) ~= 3 then
	   error("Error: bad call, usage dist_stat(""nf"",nu1,nu2,lambda)")
	end
	nu1 = varargin(1); nu2 = varargin(2); lambda = varargin(3)
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
 	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 > 0 &&
	      is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0  ) then
	      error("Error: for dist_stat(""nf"",P,nu1,nu2,lambda), nu1 and nu2 should be positive and lambda non negative")
	end
	if nu2 > 2 then, m = nu2*(nu1+lambda)/(nu1*(nu2-2)), else, m = %inf, end
	if nu2 > 4 then
	   sd = sqrt( 2*((nu1+lambda)^2+(nu1+2*lambda)*(nu2-2))/(nu2-4) ) * nu2/(nu1*(nu2-2))
	elseif nu2 > 2 then
	   sd = %inf
	else
	   sd = %nan
	end
	
     case "t" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""t"",nu)")
	end
	nu = varargin(1);  
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0) then
	   error("Error: for dist_stat(""t"",nu), nu should be positive real")
	end
	if nu > 1 then, m = 0, else, m = %nan, end; 
	if nu > 2 then
	   sd = sqrt(nu/(nu-2))
	elseif nu > 1 then
	   sd = %inf
	else
	   sd = %nan
	end
     
     case "nt" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: dist_stat(""nt"",nu,lambda)")
	end
	nu = varargin(1); lambda = varargin(2); 
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 && ...
	      is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda)) then
	   error("Error: for dist_stat(""nt"",nu,lambda), nu should be positive and lambda a real scalar")
	end
	if nu > 1 then
	   temp = exp(lngamma(0.5*(nu-1))-lngamma(0.5*nu))
	   m = lambda*sqrt(0.5*nu)*temp
	else
	   m = %nan
	end 
	if nu > 2 then
	   sd = sqrt(nu*( (1+lambda^2)/(nu-2) - 0.5*(lambda*temp)^2 ))
	elseif nu > 1 then
	   sd = %inf
	else
	   sd = %nan
	end

     case "exp" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""exp"",tau)")
	end
	tau = varargin(1);
	if ~( is(tau,%types.Mat) && isreal(tau) && isscalar(tau) && tau > 0 ) then
	      error("Error: for dist_stat(""exp"",tau), tau should be a positive real")
	end
	m = tau; sd = tau;

     case "geom" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""geom"",p)")
	end
	pr = varargin(1);
	if ~( is(pr,%types.Mat) && isreal(pr) && isscalar(pr) && 0 < pr && pr <= 1 ) then
	      error("Error: for dist_stat(""geom"",p), p should be a real in (0,1]")
	end
	m = 1/p; sd = (1-p)/p
	
     case "cau" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""cau"",sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for dist_stat(""cau"",sigma), sigma should be a positive real")
	end
	m = %nan; sd = %nan;
	
     case "lap" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""lap"",a)")
	end
	a = varargin(1);
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 ) then
	      error("Error: for dist_stat(""lap"",a), a should be a positive real")
	end
	m = 0; sd = a*sqrt(2)
	
     case "logn" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""logn"",mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for dist_stat(""logn"",mu,sigma), mu should be real and sigma a positive real")
	end
	temp = exp(0.5*sigma^2)
	m = exp(mu)*temp;
	sd = m * sqrt(temp-1) * sqrt(temp+1)
	
      case "ray" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage dist_stat(""ray"",sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for dist_stat(""ray"",sigma), sigma should be a positive real")
	end
	m = sigma*sqrt(0.5*%pi);
	sd = sigma*sqrt(2 - 0.5*%pi)
	     
     case "tray" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""tray"",sigma,a)")
	end
	sigma = varargin(1); a = varargin(2);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) &&  sigma > 0 &&
	    is(a,%types.Mat) && isreal(a) && isscalar(a) && a >= 0 ) then
	      error("Error: for dist_stat(""tray"",sigma,a), sigma should be positive and a non negative")
	end
	m = a + sqrt(%pi/2)*sigma*erfcx(a/(sqrt(2)*sigma))
        // compute numerically the standard dev
        function y=vartray(x,par)
           sigma=par(1); a = par(2); m = par(3)
	   y = (x-m).^2 .* pdf("tray",x,sigma,a)
        endfunction
	sd = sqrt( intg(a,m,vartray,args=[sigma,a,m],vecteval=%t) ...
		  +intg(m,%inf,vartray,args=[sigma,a,m],vecteval=%t))
	   
     case "uin" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: dist_stat(""uin"",n1,n2)")
	end
	n1 = varargin(1); n2 = varargin(2)   
	if ~( is(n1,%types.Mat) && isreal(n1) && isscalar(n1) && floor(n1)==n1 &&
	      is(n2,%types.Mat) && isreal(n2) && isscalar(n2) && floor(n2)==n2 && n1 <= n2 ) then
	      error("Error: for dist_stat(""uin"",n1,n2), n1 and n2 should be integer with n1 <= n2")
	end
	m = 0.5*(n1+n2)
	sd = sqrt((n1+n2)*(n2-n1)/12)
	
     case "unf" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: dist_stat(""unf"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && a < b ) then
	      error("Error: for dist_stat(""unf"",a,b), a and b should be real with a < b")
	end
	m = 0.5*(a+b)
	sd = (b-a)/sqrt(12)
		
     case "wei" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage dist_stat(""wei"",a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for dist_stat(""wei"",a,b), a and b should be positive real")
	end
	m = a*gamma(1+1/b)
	sd = a*sqrt(gamma(1+2/b) - gamma(1+1/b)^2)
     else
	error("Error: not implemented")
	
   end
   
endfunction
