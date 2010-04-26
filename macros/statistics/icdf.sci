// Copyright (C) 2010 Bruno Pincon  Esial/Iecn
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

// inverse cumulative distribution functions (quantile functions)
//
// dist: string specifying the distribution (same strings
//       than the pdf, cdf, or grand function)
//
// P: real scalar, vector or matrix, argument of the icdf function
//    to be evaluated (elements of P should be in [0,1]
//
// varargin: parameters of the specified distribution
//
// for continuous distributions:
//    x(i) should be such that  p(i) = Probability( X <= x(i) ) = F(x(i))
//
// for discrete distributions:
//    x(i) should be the infimum of the set { x s.t. p(i) <= F(x(i)) }
//
// In many cases this function is an easy driver to the cdfxxx functions.
//
// icdf could be improved by adding an optional Q= parameter
//

function x = icdf(dist, P, varargin, Q=[])

   if nargin < 2 then 
      error("Error: icdf needs at least 2 input arguments")
   end
   
   if ~( is(dist,%types.SMat) && isscalar(dist) ) then
      error("Error: first argument should be a string")
   end
   
   if ~( is(P,%types.Mat) && isreal(P) ) then
      error("Error: second argument should be a real scalar, vector or matrix")
   end

   if ~and(0 <= P & P <= 1) then
      error("Error: second argument should be in [0,1]")
   end
   
   if isempty(Q)
      Q = 1.0 - P
   else
      if ~ size(Q).equal[size(P)]
	 error("Error: optional Q argument should be of same size than second arg P")
      end
      if ~and(0 <= Q & Q <= 1) then
	 error("Error: optional Q argument should be in [0,1]")
      end 
      if ~and(P+Q == 1) then
	 error("Error: we should have P+Q=1")
      end
   end

   x = zeros(size(P));
   
   ind0 = find(P == 0,ind_type="int")
   ind1 = find(Q == 0,ind_type="int")
   ind = find(P > 0 & Q > 0,ind_type="int")
   Pind = P(ind); Qind = Q(ind);
   size_to_Pind = ones(size(Pind));

   select dist
     case "nor" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""nor"",P,mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for icdf(""nor"",P,mu,sigma), mu should be real and sigma a positive real")
	end
	x(ind0) = -%inf
	x(ind) = cdfnor("X", mu*size_to_Pind, sigma*size_to_Pind, Pind, Qind);
	x(ind1) = %inf
	
     case "chi" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""chi"",P,nu)")
	end
	nu = varargin(1);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ) then
	      error("Error: for icdf(""chi"",P,nu), nu should be a positive real")
	end
	x(ind0) = 0
	x(ind) = cdfchi("X", nu*size_to_Pind, Pind, Qind);
	x(ind1) = %inf
	
     case "nch" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""nch"",P,nu,lambda)")
	end
	nu = varargin(1);
	lambda = varargin(2);
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 ... 
	      && is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0 ) then
	      error("Error: for icdf(""nch"",P,nu,lambda), nu should be a positive real and lambda a non negative real")
	end
	x(ind0) = 0
	x(ind) = cdfchn("X", nu*size_to_Pind, lambda*size_to_Pind, Pind, Qind);
	x(ind1) = %inf
	
     case "bet" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""bet"",P,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for icdf(""bet"",P,a,b), a and b should be positive real")
	end
	x(ind0) = 0
	x(ind) = cdfbet("XY", a*size_to_Pind, b*size_to_Pind, Pind, Qind);
	x(ind1) = 1
	
     case "bin" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""bin"",P,n,p)")
	end
	n = varargin(1); p = varargin(2)   
	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n > 0 && floor(n)==n &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 <= p && p <= 1 ) then
	      error("Error: for icdf(""bin"",P,n,p), bad parameters n and/or p")
	end
	size_to_P = ones(size(P))
	x = ceil(cdfbin("S", n*size_to_P, p*size_to_P, (1-p)*size_to_P, P, Q));
	// correction
	[PP,QQ] = cdf("bin",x-1,n,p);
	indc = (P <= 0.5 & PP >= P & P > 0) | (Q < 0.5 & QQ <= Q & Q > 0)
	x(indc) = x(indc) - 1;
	
     case "nbn" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""nbn"",P,r,p)")
	end
	r = varargin(1); p = varargin(2)   
	if ~( is(r,%types.Mat) && isreal(r) && isscalar(r) && r > 0 &&
	      is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 <= p && p <= 1 ) then
	      error("Error: for icdf(""nbn"",P,r,p), bad parameters r and/or p")
	end
	x(ind0) = -1
	x(ind) = ceil(cdfnbn("S", r*size_to_Pind, p*size_to_Pind, (1-p)*size_to_Pind, Pind, Qind));
	x(ind1) = %inf
	// correction
	[PP,QQ] = cdf("nbn",x-1,r,p);
	indc = (P <= 0.5 & PP >= P & P > 0) | (Q < 0.5 & QQ <= Q & Q > 0)	
	x(indc) = x(indc) - 1;
	
     case "gam" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""gam"",P,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b >= 0 ) then
	      error("Error: for icdf(""gam"",P,a,b), a should be positive real and b a non negative real")
	end
	x(ind0) = 0
	x(ind) = cdfgam("X", a*size_to_Pind, b*size_to_Pind, Pind, Qind);
	x(ind1) = %inf
	
     case "logi" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""logi"",P,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for icdf(""logi"",P,a,b), a should be a real and b a positive real")
	end
	x = a - b*log(Q./P)
	
     case "par" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""par"",P,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for icdf(""par"",P,a,b), a and b should be positive real")
	end
	x = b./Q.^(1/a)
	
     case "poi" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""poi"",P,mu)")
	end
	mu = varargin(1);
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) && mu >= 0 ) then
	      error("Error: for icdf(""poi"",P,mu), mu should be a non negative real")
	end
	x(ind0) = -1
	x(ind) = ceil(cdfpoi("S",mu*size_to_Pind,Pind,Qind))
	x(ind1) = %inf;
	// correction
	[PP,QQ] = cdf("poi",x-1,mu);
	indc = (P <= 0.5 & PP >= P & P > 0) | (Q < 0.5 & QQ <= Q & Q > 0)	
	x(indc) = x(indc) - 1;
	
     case "f" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""f"",P,nu1,nu2)")
	end
	nu1 = varargin(1); nu2 = varargin(2)   
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 > 0 ) then
	      error("Error: for icdf(""f"",P,nu1,nu2), nu1 and nu2 should be positive real")
	end
	x(ind0) = 0
	x(ind) = cdff("F", nu1*size_to_Pind, nu2*size_to_Pind, Pind, Qind);
	x(ind1) = %inf
	
     case "nf" then
	if numel(varargin) ~= 3 then
	   error("bad number of input args, usage: icdf(""nf"",P,nu1,nu2,lambda)")
	end
	nu1 = varargin(1); nu2 = varargin(2); lambda = varargin(3);   
	if ~( is(nu1,%types.Mat) && isreal(nu1) && isscalar(nu1) && nu1 > 0 &&
	      is(nu2,%types.Mat) && isreal(nu2) && isscalar(nu2) && nu2 > 0 &&
	      is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0 ) then
	      error("Error: for icdf(""nf"",P,nu1,nu2,lambda), nu1 and nu2 should be positive and lambda non negative")
	end
	x(ind0) = 0
	x(ind) = cdffnc("F", nu1*size_to_Pind, nu2*size_to_Pind, lambda*size_to_Pind, Pind, Qind);
	x(ind1) = %inf

     case "t" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""t"",P,nu)")
	end
	nu = varargin(1);  
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0) then
	   error("Error: for icdf(""t"",P,nu), nu should be positive real")
	end
	x(ind0) = -%inf
	x(ind) = cdft("T", nu*size_to_Pind, Pind, Qind);
	x(ind1) = %inf

     case "nt" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""nt"",x,nu,lambda)")
	end
	nu = varargin(1); lambda = varargin(2); 
	if ~( is(nu,%types.Mat) && isreal(nu) && isscalar(nu) && nu > 0 && ...
	      is(lambda,%types.Mat) && isreal(lambda) && isscalar(lambda) && lambda >= 0) then
	   error("Error: for icdf(""nt"",P,nu,lambda), nu should be positive and lambda non negative")
	end
	x(ind0) = -%inf
	x(ind) = cdftnc("T", nu*size_to_Pind, lambda*size_to_Pind, Pind, Qind);
	x(ind1) = %inf

     case "exp" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""exp"",P,tau)")
	end
	tau = varargin(1);
	if ~( is(tau,%types.Mat) && isreal(tau) && isscalar(tau) && tau > 0 ) then
	      error("Error: for icdf(""exp"",P,tau), tau should be a positive real")
	end
	[indp, indq] = mfind(P,"<=",0.5)
	x(indp) = -tau*log1p(-P(indp));
	x(indq) = -tau*log(Q(indq))

     case "geom" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""geom"",P,p)")
	end
	p = varargin(1);
	if ~( is(p,%types.Mat) && isreal(p) && isscalar(p) && 0 < p && p <= 1 ) then
	      error("Error: for icdf(""geom"",P,p), p should be a real in (0,1]")
	end
	[indp, indq] = mfind(P,"<=",0.5)
	x(indp) = ceil( log1p(-P(indp)) / log1p(-p) );
	x(indq) = ceil( log(Q(indq)) / log1p(-p) );	
	// correction
	[PP,QQ] = cdf("geom",x-1,p);
	indc = (P <= 0.5 & PP >= P & P > 0) | (Q < 0.5 & QQ <= Q & Q > 0)
	x(indc) = x(indc) - 1;
	
     case "cau" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""cau"",P,sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for icdf(""cau"",P,sigma), sigma should be a positive real")
	end
	x(ind0) = -%inf;
	x(ind) = sigma*tan(%pi*(Pind-0.5));
	x(ind1) = %inf;

// $$$      case "k" then
// $$$ 	if numel(varargin) ~= 1 then
// $$$ 	   error("bad number of input args, usage: icdf(""k"",P,n)")
// $$$ 	end
// $$$ 	n = varargin(1);
// $$$ 	if ~( is(n,%types.Mat) && isreal(n) && isscalar(n) && n >= 1 && floor(n)==n) then
// $$$ 	      error("Error: for icdf(""k"",P,n), n should be a positive integer")
// $$$ 	end
// $$$ 	
// $$$      case "klim" then
// $$$ 	if numel(varargin) ~= 0 then
// $$$ 	   error("bad number of input args, usage: icdf(""klim"",P)")
// $$$ 	end
	
     case "lap" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""lap"",P,a)")
	end
	a = varargin(1);
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 ) then
	      error("Error: for icdf(""lap"",P,a), a should be a positive real")
	end
	[indl,indr] = mfind(P, "<", 0.5);
	x(indl) = a*log(2*P(indl));
	x(indr) =-a*log(2*Q(indr));
	
     case "logn" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""logn"",P,mu,sigma)")
	end
	mu = varargin(1); sigma = varargin(2)   
	if ~( is(mu,%types.Mat) && isreal(mu) && isscalar(mu) &&
	      is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for icdf(""logn"",P,mu,sigma), mu should be real and sigma a positive real")
	end
	x(ind0) = 0;
	x(ind)= exp(mu+sigma*cdfnor("X", zeros(size(Pind)), ones(size(Pind)), Pind, Qind))
	x(ind1) = %inf;
	
      case "ray" then
	if numel(varargin) ~= 1 then
	   error("bad number of input args, usage: icdf(""ray"",P,sigma)")
	end
	sigma = varargin(1);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) && sigma > 0 ) then
	      error("Error: for icdf(""ray"",P,sigma), sigma should be a positive real")
	end
	[indp, indq] = mfind(P,"<=",0.5)
	x(indp) = sigma*sqrt(-2*log1p(-P(indp)));
	x(indq) = sigma*sqrt(-2*log(Q(indq)));
	
     case "tray" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""tray"",P,sigma,a)")
	end
	sigma = varargin(1); a = varargin(2);
	if ~( is(sigma,%types.Mat) && isreal(sigma) && isscalar(sigma) &&  sigma > 0 &&
	    is(a,%types.Mat) && isreal(a) && isscalar(a) && a >= 0 ) then
	      error("Error: for icdf(""tray"",P,sigma,a), sigma should be positive and a non negative")
	end
	[indp, indq] = mfind(P,"<=",0.5)
	x(indp) = a*sqrt(1 - 2*(sigma/a)^2*log1p(-P(indp)))
	x(indq) = a*sqrt(1 - 2*(sigma/a)^2*log(Q(indq)))
	
     case "uin" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""uin"",P,n1,n2)")
	end
	n1 = varargin(1); n2 = varargin(2)   
	if ~( is(n1,%types.Mat) && isreal(n1) && isscalar(n1) && floor(n1)==n1 &&
	      is(n2,%types.Mat) && isreal(n2) && isscalar(n2) && floor(n2)==n2 && n1 <= n2 ) then
	      error("Error: for icdf(""uin"",P,n1,n2), n1 and n2 should be integer with n1 <= n2")
	end
	x = ceil( (n1-1)*Q + n2*P )
	// correction
	[PP,QQ] = cdf("uin",x-1,n1,n2);
	indc = (P <= 0.5 & PP >= P & P > 0) | (Q < 0.5 & QQ <= Q & Q > 0)
	x(indc) = x(indc) - 1;
	
     case "unf" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""unf"",P,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && a < b ) then
	      error("Error: for icdf(""unf"",P,a,b), a and b should be real with a < b")
	end
	x = a*Q + b*P;
	
     case "wei" then
	if numel(varargin) ~= 2 then
	   error("bad number of input args, usage: icdf(""wei"",P,a,b)")
	end
	a = varargin(1); b = varargin(2)   
	if ~( is(a,%types.Mat) && isreal(a) && isscalar(a) && a > 0 &&
	      is(b,%types.Mat) && isreal(b) && isscalar(b) && b > 0 ) then
	      error("Error: for icdf(""wei"",P,a,b), a and b should be positive real")
	end
	[indp, indq] = mfind(P,"<=",0.5)
	x(indp) = a*(-log1p(-P(indp))).^(1/b)
	x(indq) = a*(-log(Q(indq))).^(1/b)
	
     else
	error("Error: unknown or not implemented distribution")
	
   end
   
endfunction
