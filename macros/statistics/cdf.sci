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
// P = Probability( X <= x )
// Q = 1 - P
//
function [P,Q] = cdf(dist, x, varargin)
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
	   error("bad number of input args, usage: cdf(""nor"",x,mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""nor"",x,mu,sigma), mu should be real and sigma a positive real")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdfnor("PQ", x, mu*size_to_x, sigma*size_to_x);
	
     case "chi" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""chi"",x,nu)")
	end
	nu = varargin(1);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ) then
	      error("Error: for cdf(""chi"",x,nu), nu should be a positive real")
	end
	[P,Q] = cdfchi("PQ", x, nu*ones(size(x)));
	
     case "nch" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""nch"",x,nu,lambda)")
	end
	nu = varargin(1);
	lambda = varargin(2);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ... 
	      && is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0 ) then
	      error("Error: for cdf(""nch"",x,nu,lambda), nu should be a positive real and lambda a non negative real")
	end
	size_to_x = ones(size(x));
	[P,Q] = cdfchn("PQ", x, nu*size_to_x, lambda*size_to_x);
	
     case "bet" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""bet"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""bet"",x,a,b), a and b should be positive real")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdfbet("PQ", x, 1-x, a*size_to_x, b*size_to_x);
	
     case "bin" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""bin"",x,n,p)")
	end
	n = varargin(1); p = varargin(2)   
	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n > 0 && floor(n)==n &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 <= p && p <= 1 ) then
	      error("Error: for cdf(""bin"",x,n,p), bad parameters n and/or p")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdfbin("PQ", x, n*size_to_x, p*size_to_x, (1-p)*size_to_x);
	
     case "nbn" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""nbn"",x,r,p)")
	end
	r = varargin(1); p = varargin(2)   
	if ~( is(r,%types.Mat) && isreal(r) && isscalar(r) && r > 0 &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 < p && p <= 1 ) then
	      error("Error: for cdf(""nbn"",x,r,p), bad parameters r and/or p")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdfnbn("PQ", x, r*size_to_x, p*size_to_x, (1-p)*size_to_x);
	
     case "gam" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""gam"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b >= 0 ) then
	      error("Error: for cdf(""gam"",x,a,b), a should be positive real and b a non negative real")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdfgam("PQ", x, a*size_to_x, b*size_to_x);
	
     case "logi" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""logi"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""logi"",x,a,b), a should be a real and b a positive real")
	end
	t =  exp(-(x-a)/b)
	d = 1 + t;
	P = 1 ./ d
	Q = t ./ d   // Q = 1 - P
	
     case "par" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""par"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""par"",x,a,b), a and b should be positive real")
	end
	P  = zeros(size(x)); Q = ones(size(x))
	ind = find(~(x < b),ind_type="int");  // use ~(x < b) to transmit %nan
	Q(ind) = (b./x(ind)).^a
	P(ind) = 1 - Q(ind)
// $$$ 	// for x near b use another formula for P
// $$$ 	ind = find(x > b & P < 0.1)
// $$$ 	P(ind) = -cdf_expm1(-a*log1p((x(ind)-b)/b))
// $$$ 	Q(ind) = 1 - P(ind)
	
     case "poi" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""poi"",x,mu)")
	end
	mu = varargin(1);
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) && mu >= 0 ) then
	      error("Error: for cdf(""poi"",x,mu), mu should be a non negative real")
	end
	[P,Q] = cdfpoi("PQ",x,mu*ones(size(x)))
	
     case "f" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""f"",x,nu1,nu2)")
	end
	nu1 = varargin(1); nu2 = varargin(2)   
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 > 0 ) then
	      error("Error: for cdf(""f"",x,nu1,nu2), nu1 and nu2 should be positive real")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdff("PQ", x, nu1*size_to_x, nu2*size_to_x);
	
     case "nf" then
	if numel(varargin) ~= 3 then
	   error("bad number of input args, usage: cdf(""nf"",x,nu1,nu2,lambda)")
	end
	nu1 = varargin(1); nu2 = varargin(2); lambda = varargin(3);   
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 > 0 &&
	      is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0 ) then
	      error("Error: for cdf(""nf"",x,nu1,nu2,lambda), nu1 and nu2 should be positive and lambda non negative")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdffnc("PQ", x, nu1*size_to_x, nu2*size_to_x, lambda*size_to_x);

     case "t" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""t"",x,nu)")
	end
	nu = varargin(1);  
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0) then
	   error("Error: for cdf(""t"",x,nu), nu should be positive real")
	end
	[P,Q] = cdft("PQ", x, nu*ones(size(x)));

     case "nt" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""nt"",x,nu,delta)")
	end
	nu = varargin(1); delta = varargin(2); 
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 && ...
	      is(delta,%types.Mat) && isreal(delta) && isscalar(delta)) then
	   error("Error: for cdf(""nt"",x,nu,delta), nu should be positive and delta a real scalar")
	end
	size_to_x  = ones(size(x));
	[P,Q] = cdftnc("PQ", x, nu*size_to_x, delta*size_to_x);

     case "exp" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""exp"",x,tau)")
	end
	tau = varargin(1);
	if ~( is(tau,%types.Mat) && isreal(tau) && isscalar(tau) && tau > 0 ) then
	      error("Error: for cdf(""exp"",x,tau), tau should be a positive real")
	end
	P = zeros(size(x)); Q = ones(size(x));
	[ineg,ip,iq] = mfind(x,"<=",0,"<=",0.7*tau,ind_type="int")
	P(ip) = -expm1(-x(ip)/tau); Q(ip) = 1 - P(ip);
	Q(iq) = exp(-x(iq)/tau); P(iq) = 1 - Q(iq);

     case "geom" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""geom"",x,p)")
	end
	p = varargin(1);
	if ~( is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 < p && p <= 1 ) then
	      error("Error: for cdf(""geom"",x,p), p should be a real in (0,1]")
	end
	P = zeros(size(x)); Q = ones(size(x));
	ind = find(~(x < 1),ind_type="int")  // use ~(x < 1) to transmit %nan
	Q(ind) = (1 - p).^floor(x(ind))
	P(ind) = 1 - Q(ind);
	
     case "cau" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""cau"",x,sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""cau"",x,sigma), sigma should be a positive real")
	end
	temp = atan(x/sigma)/%pi;
	P = 0.5 + temp;
	Q = 0.5 - temp;
	
     case "k" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""k"",x,n)")
	end
	n = varargin(1);
	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n >= 1 && floor(n)==n) then
	      error("Error: for cdf(""k"",x,n), n should be a positive integer")
	end
	P = kcdf(x,n);
	Q = 1 - P;
	
     case "klim" then
	if numel(varargin) ~= 0 then
	   error("bad number of input args, usage: cdf(""klim"",x)")
	end
	[P,Q] = kcdflim(x);
	
     case "lap" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""lap"",x,a)")
	end
	a = varargin(1);
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 ) then
	      error("Error: for cdf(""lap"",x,a), a should be a positive real")
	end
	P = zeros(size(x)); Q = ones(size(x));
	[ipos,ineg] = mfind(x, ">=", 0, ind_type="int")
	Q(ipos) = 0.5*exp(-x(ipos)/a)
	P(ipos) = 1 - Q(ipos);
	P(ineg) = 0.5*exp(x(ineg)/a)
	Q(ineg) = 1 - P(ineg);
	
     case "logn" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""logn"",x,mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""logn"",x,mu,sigma), mu should be real and sigma a positive real")
	end
	P = zeros(size(x)); Q = ones(size(x));
	ind = find(~(x <= 0),ind_type="int")
	xind = x(ind);
	[P(ind),Q(ind)] = cdfnor("PQ", (log(xind)-mu)/sigma, zeros(size(xind)), ones(size(xind)));
	
      case "ray" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: cdf(""ray"",x,sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for cdf(""ray"",x,sigma), sigma should be a positive real")
	end
	P = zeros(size(x)); Q = ones(size(x)); // median = sigma*1.1774
	[ineg,ip,iq] = mfind(x,"<=",0,"<=",1.1774*sigma,ind_type="int")
	P(ip) = -expm1(-0.5*(x(ip)/sigma).^2); Q(ip) = 1 - P(ip);
	Q(iq) = exp(-0.5*(x(iq)/sigma).^2); P(iq) = 1 - Q(iq);
	
     case "tray" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""tray"",x,sigma,a)")
	end
	sigma = varargin(1); a = varargin(2);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) &&  sigma > 0 &&
	    is(a,%types.Mat) && isreal(a) && isscalar(a) && a >= 0 ) then
	      error("Error: for cdf(""tray"",x,sigma,a), sigma should be positive and a non negative")
	end
	P = zeros(size(x)); Q = ones(size(x));
	md = a*sqrt(1 - 2*(sigma/a)^2*log(0.5)); // median
	[ia,ip,iq] = mfind(x,"<=",a,"<=",md,ind_type="int")
	temp =  -0.5*((x(ip)-a).*(x(ip)+a)/sigma^2)
	P(ip) = -expm1(temp); Q(ip)=1-P(ip);
	temp =  -0.5*((x(iq)-a).*(x(iq)+a)/sigma^2)	
	Q(iq) = exp(temp); P(iq) = 1 - Q(iq)
	
     case "uin" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""uin"",x,n1,n2)")
	end
	n1 = varargin(1); n2 = varargin(2)   
	if ~( is(n1,%types.Mat) && isreal(n1) && isscalar(n1) && floor(n1)==n1 &&
	      is(n2,%types.Mat) && isreal(n2) && isscalar(n2) && floor(n2)==n2 && n1 <= n2 ) then
	      error("Error: for cdf(""uin"",x,n1,n2), n1 and n2 should be integer with n1 <= n2")
	end
	n = n2 - n1 + 1;
	P = max(0, min( floor(x-n1+1)/n, 1 ) )
	P(isnan(x)) = %nan;
	Q = 1 - P;
	
     case "unf" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""unf"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && a < b ) then
	      error("Error: for cdf(""unf"",x,a,b), a and b should be real with a < b")
	end
	P = max(0, min( (x-a)/(b-a), 1 ) )
	P(isnan(x)) = %nan;
	Q = 1-P;
// $$$ 	ind = find(P > 0.5)
// $$$ 	temp =  max(0, (b-x(ind))/(b-a) )
// $$$ 	Q(ind) = temp
// $$$ 	P(ind) = 1 - temp
	
     case "wei" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: cdf(""wei"",x,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for cdf(""wei"",x,a,b), a and b should be positive real")
	end
	P = zeros(size(x)); Q = ones(size(x));
	md = a*log(2)^(1/b)  // median
	[ineg,ip,iq] = mfind(x,"<=",0,"<=",md,ind_type="int")
	temp =  -(x(ip)/a).^b
	P(ip) = -expm1(temp); Q(ip) = 1 - P(ip)
	temp =  -(x(iq)/a).^b
	Q(iq) = exp(temp); P(iq) = 1 - Q(iq)
   
   else
	error("Error: unknown or not implemented distribution")
	
   end
   
endfunction
