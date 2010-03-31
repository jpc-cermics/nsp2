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

// cumulative distribution functions
//
// dist: string specifying the distribution (same strings
//       than the pdf function or grand function)
//
// x: real scalar, vector or matrix, argument of the cdf function
//    to be evaluated
//
// varargin: parameters of the specified distribution
//
// p = Probability( X <= x )
// q = 1 - p
//
function [p,q] = cdf(dist, x, varargin)
   //
   if nargin < 2 then 
      error("Error: cdf needs at least 2 input arguments")
   end
   
   if ~( is(dist,%types.SMat) && isscalar(dist) ) then
      error("Error: first argument should be a string")
   end
   
   if ~( is(x,%types.Mat) && isreal(x) ) then
      error("Error: second argument should be a real scalar, vector or matrix")
   end
   
   select dist
     case "nor" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""nor"",x,mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""nor"",x,mu,sigma), mu should be real and sigma a positive real")
	end
	size_to_x  = ones(size(x));
	[p,q] = cdfnor("PQ", x, mu*size_to_x, sigma*size_to_x);
	
     case "chi" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""chi"",x,nu)")
	end
	nu = varargin(1);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ) then
	      error("Error: for cdf(""chi"",x,nu), nu should be a positive real")
	end
	[p,q] = cdfchi("PQ", x, nu*ones(size(x)));
	
     case "nch" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""nch"",x,nu,lambda)")
	end
	nu = varargin(1);
	lambda = varargin(2);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ... 
	      && is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0 ) then
	      error("Error: for cdf(""nch"",x,nu,lambda), nu should be a positive real and lambda a non negative real")
	end
	size_to_x = ones(size(x));
	[p,q] = cdfchn("PQ", x, nu*size_to_x, lambda*size_to_x);
	
     case "bet" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""bet"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""bet"",x,a,b), a and b should be positive real")
	end
	size_to_x  = ones(size(x));
	[p,q] = cdfbet("PQ", x, 1-x, a*size_to_x, b*size_to_x);
	
     case "bin" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""bin"",x,n,p)")
	end
	n = varargin(1); p = varargin(2)   
	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n > 0 && floor(n)==n &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 <= p && p <= 1 ) then
	      error("Error: for cdf(""bin"",x,n,p), bad parameters n and/or p")
	end
	size_to_x  = ones(size(x));
	[p,q] = cdfbin("PQ", x, n*size_to_x, p*size_to_x, (1-p)*size_to_x);
	
     case "nbn" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""nbn"",x,r,p)")
	end
	r = varargin(1); p = varargin(2)   
	if ~( is(r,%types.Mat) && isreal(r) && isscalar(r) && r > 0 &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 <= p && p <= 1 ) then
	      error("Error: for cdf(""nbn"",x,r,p), bad parameters r and/or p")
	end
	size_to_x  = ones(size(x));
	[p,q] = cdfnbn("PQ", x, r*size_to_x, p*size_to_x, (1-p)*size_to_x);
	
     case "gam" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""gam"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b >= 0 ) then
	      error("Error: for cdf(""gam"",x,a,b), a should be positive real and b a non negative real")
	end
	size_to_x  = ones(size(x));
	[p,q] = cdfgam("PQ", x, a*size_to_x, b*size_to_x);
	
     case "logi" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""logi"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""logi"",x,a,b), a should be a real and b a positive real")
	end
	t =  exp(-(x-a)/b)
	d = 1 + t;
	p = 1 ./ d
	q = t ./ d   // q = 1 - p
	
     case "par" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""par"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""par"",x,a,b), a and b should be positive real")
	end
	p  = zeros(size(x)); q = ones(size(x))
	ind = find(~(x < b),ind_type="int");  // use ~(x < b) to transmit %nan
	q(ind) = (b./x(ind)).^a
	p(ind) = 1 - q(ind)
	
     case "f" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""f"",x,nu1,nu2)")
	end
	nu1 = varargin(1); nu2 = varargin(2)   
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 >= 0 ) then
	      error("Error: for cdf(""f"",x,nu1,nu2), nu1 and nu2 should be positive real")
	end
	size_to_x  = ones(size(x));
	[p,q] = cdff("PQ", x, nu1*size_to_x, nu2*size_to_x);

     case "t" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""t"",x,nu)")
	end
	nu = varargin(1);  
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0) then
	   error("Error: for cdf(""t"",x,nu), nu should be positive real")
	end
	[p,q] = cdft("PQ", x, nu*ones(size(x)));

     case "exp" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""exp"",x,tau)")
	end
	tau = varargin(1);
	if ~( is(tau,%types.Mat) && isreal(tau) && isscalar(tau) && tau > 0 ) then
	      error("Error: for cdf(""exp"",x,tau), tau should be a positive real")
	end
	p = zeros(size(x)); q = ones(size(x));
	ind = find(~(x <= 0),ind_type="int")  // use ~(x <= 0) to transmit %nan
	q(ind) = exp(-x(ind)/tau)
	p(ind) = 1 - q(ind);

     case "geom" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""geom"",x,p)")
	end
	pr = varargin(1);
	if ~( is(pr,%types.Mat) && isreal(pr) && isscalar(pr) && 0 < pr && pr <= 1 ) then
	      error("Error: for cdf(""geom"",x,p), p should be a real in (0,1]")
	end
	p = zeros(size(x)); q = ones(size(x));
	ind = find(~(x < 1),ind_type="int")  // use ~(x < 1) to transmit %nan
	q(ind) = (1 - pr).^floor(x(ind))
	p(ind) = 1 - q(ind);
	
     case "cau" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""cau"",x,sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""cau"",x,sigma), sigma should be a positive real")
	end
	temp = atan(x/sigma)/%pi;
	p = 0.5 + temp;
	q = 0.5 - temp;
	
     case "k" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""k"",x,n)")
	end
	n = varargin(1);
	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n >= 1 && floor(n)==n) then
	      error("Error: for cdf(""k"",x,n), n should be a positive integer")
	end
	p = kcdf(x,n);
	q = 1 - p;
	
     case "klim" then
	if numel(varargin) ~= 0 then
	   error("Error: bad call, usage cdf(""klim"",x)")
	end
	[p,q] = kcdflim(x);
	
     case "lap" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""lap"",x,a)")
	end
	a = varargin(1);
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 ) then
	      error("Error: for cdf(""lap"",x,a), a should be a positive real")
	end
	p = zeros(size(x)); q = ones(size(x));
	[ipos,ineg] = mfind(x, ">=", 0, ind_type="int")
	q(ipos) = 0.5*exp(-x(ipos)/a)
	p(ipos) = 1 - q(ipos);
	p(ineg) = 0.5*exp(x(ineg)/a)
	q(ineg) = 1 - p(ineg);
	
     case "logn" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""logn"",x,mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""logn"",x,mu,sigma), mu should be real and sigma a positive real")
	end
	p = zeros(size(x)); q = ones(size(x));
	ind = find(~(x <= 0),ind_type="int")
	xind = x(ind);
	[p(ind),q(ind)] = cdfnor("PQ", (log(xind)-mu)/sigma, zeros(size(xind)), ones(size(xind)));
	
      case "ray" then
	if numel(varargin) ~= 1 then
	   error("Error: bad call, usage cdf(""ray"",x,sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""ray"",x,sigma), sigma should be a positive real")
	end
	p = zeros(size(x)); q = ones(size(x));
	ind = find(~(x <= 0),ind_type="int")
	temp = exp(-0.5*(x(ind)/sigma).^2)
	q(ind) = temp;
	p(ind) = 1 - temp;
	
     case "tray" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""tray"",x,sigma,a)")
	end
	sigma = varargin(1); a = varargin(2);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) &&  sigma > 0 &&
	    is(a,%types.Mat) && isreal(a) && isscalar(a) && a >= 0 ) then
	      error("Error: for cdf(""tray"",x,sigma,a), sigma should be positive and a non negative")
	end
	p = zeros(size(x)); q = ones(size(x));
	ind = find(~(x <= a),ind_type="int")
	xind = x(ind);
	temp =  exp(-0.5*((xind-a).*(xind+a)/sigma^2))
	q(ind) = temp;
	p(ind) = 1 - temp;
	
     case "uin" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""uin"",x,n1,n2)")
	end
	n1 = varargin(1); n2 = varargin(2)   
	if ~( is(n1,%types.Mat) && isreal(n1) && isscalar(n1) && floor(n1)==n1 &&
	      is(n2,%types.Mat) && isreal(n2) && isscalar(n2) && floor(n2)==n2 && n1 <= n2 ) then
	      error("Error: for cdf(""uin"",x,n1,n2), n1 and n2 should be integer with n1 <= n2")
	end
	n = n2 - n1 + 1;
	p = max(0, min( floor(x-n1+1)/n, 1 ) )
	p(isnan(x)) = %nan;
	q = 1 - p;
	
     case "unf" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""unf"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && a < b ) then
	      error("Error: for cdf(""unf"",x,a,b), a and b should be real with a < b")
	end
	p = max(0, min( (x-a)/(b-a), 1 ) )
	p(isnan(x)) = %nan;
	q = 1 - p;
	
     case "wei" then
	if numel(varargin) ~= 2 then
	   error("Error: bad call, usage cdf(""wei"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""wei"",x,a,b), a and b should be positive real")
	end
	p = zeros(size(x)); q = ones(size(x));
	ind = find(~(x <= 0),ind_type="int")
	temp =  exp(-(x(ind)/a).^b)
	q(ind) = temp;
	p(ind) = 1 - temp;
	
     else
	error("Error: unknown or not implemented distribution")
	
   end
   
endfunction
