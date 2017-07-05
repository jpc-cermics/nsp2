// Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2005, 2006,
//               2007, 2008, 2009 John W. Eaton
//
// This file is part of Octave.
//
// Octave is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or (at
// your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <http://www.gnu.org/licenses/>.

// nsp version of octave polyfit
// the result is returned as a nsp polynomial
// take care that in the returned Vandermonde columns are
// in reverse order from octave version.

// -*- texinfo -*-
// @deftypefn {Function File} {[@var{p}, @var{s}, @var{mu}] =} polyfit (@var{x}, @var{y}, @var{n})
// Return the coefficients of a polynomial @var{p}(@var{x}) of degree
// @var{n} that minimizes the least-squares-error of the fit.
//
// The polynomial coefficients are returned in a row vector.
//
// The second output is a structure containing the following fields:
//
// @table @samp
// @item R
// Triangular factor R from the QR decomposition.
// @item X
// The Vandermonde matrix used to compute the polynomial coefficients.
// @item df
// The degrees of freedom.
// @item normr
// The norm of the residuals.
// @item yf
// The values of the polynomial for each value of @var{x}.
// @end table
//
// The second output may be used by @code{polyval} to calculate the
// statistical error limits of the predicted values.
//
// When the third output, @var{mu}, is present the
// coefficients, @var{p}, are associated with a polynomial in
// @var{xhat} = (@var{x}-@var{mu}(1))/@var{mu}(2).
// Where @var{mu}(1) = mean (@var{x}), and @var{mu}(2) = std (@var{x}).
// This linear transformation of @var{x} improves the numerical
// stability of the fit.
// @seealso{polyval, residue}
// @end deftypefn

// Author: KH <Kurt.Hornik@wu-wien.ac.at>
// Created: 13 December 1994
// Adapted-By: jwe

function [p,s,mu]=polyfit(x,y,n)
  //
  if (nargin<3||nargin>4) then
    error("polyfit requires three input arguments\n");
    return;
  end

  if (nargout>2) then
    // Normalized the x values.
    mu=[mean(x),std(x)];
    x=(x-mu(1))/mu(2);
  end

  if (~and(size(x)==size(y))) then
    error("polyfit: x and y must be vectors of the same size\n");
    return;
  end

  if (~(isscalar(n)&&n>=0&&~isinf(n)&&n==round(n))) then
    error("polyfit: n must be a nonnegative integer\n");return;
  end

  y_is_row_vector=(size(y,1)==1);

  // Reshape x & y into column vectors.
  l=numel(x);
  x=x(:);
  y=y(:);

  function M=vander(x,p)
    M=ones(size(x,1),p);M(:,2)=x;
    for i=3:p do M(:,i)=M(:,i-1).*x;end
  endfunction

  // Construct the Vandermonde matrix.
  // take care that in octave vander columns are
  // in opposite order.
  v=vander(x,n+1);

  // Solve by QR decomposition.
  // [q, r, k] = qr (v, 0);
  [q,r,k]=qr(v,mode = "e");
  p=r\(q'*y);
  p(k)=p;

  if (nargout>1) then
    yf=v*p;
    s=hash(10);
    if (y_is_row_vector) then
      s.yf=yf.';
    else
      s.yf=yf;
    end
    s.R=r;
    s.X=v;
    s.df=l-n-1;
    s.normr=norm(yf-y);
  end
  // Return a polynomial
  p=m2p(p);
endfunction

if exists('test_lib') then
  // test
  xv=[-2,-1,0,1,2];
  x=m2p([0,1]);
  pr=1+x+x^2;
  yv=horner(pr,xv){1};
  pf=polyfit(xv,yv,pr.degree[]);
  if norm(pf-pr)>sqrt(%eps) then pause ;end

  // test
  pf=polyfit(xv,yv,pr.degree[]+1);
  if norm(pf-pr)>sqrt(%eps) then pause ;end

  // test
  pr=4+0.5*x+x^2;
  yv=horner(pr,xv){1};
  pf=polyfit(xv,yv,pr.degree[]);
  if norm(pf-pr)>sqrt(%eps) then pause ;end

  // test: complex numbers
  xv=1:4;
  pr=(0+1*%i)+2*%i*x^2+4*x^3;
  yv=horner(pr,xv){1};
  pf=polyfit(xv,yv,pr.degree[]);
  if norm(pf-pr)>sqrt(%eps) then pause ;end

  //test
  xv=[1,2,3;4,5,6];
  yv=[0,0,1;1,0,0];
  pf=polyfit(xv,yv,5);
  pr=m2p([60,-112,65,-14,1,0]/12);
  if norm(pf-pr)>sqrt(%eps) then pause ;end

  // difficult case where scaling is really needed. This example
  // demonstrates the rather poor result which occurs when the dependent
  // variable is not normalized properly.
  // Also check the usage of 2nd & 3rd output arguments.
  //test
  x=[-1196.4,-1195.2,-1194,-1192.8,-1191.6,-1190.4,-1189.2,-1188,-1186.8,-1185.6,-1184.4, ...
     -1183.2,-1182];
  y=[315571.7086,315575.9618,315579.4195,315582.6206,315585.4966,315588.3172,315590.9326, ...
     315593.5934,315596.0455,315598.4201,315600.7143,315602.9508,315605.1765];
  [p1,s1]=polyfit(x,y,10);
  [p2,s2,mu]=polyfit(x,y,10);
  if ~(s2.normr<s1.normr) then pause ;end


  //test
  xv=1000+(-5:5);
  xn=(xv-mean(xv))/std(xv);
  pn=m2p(ones(1,5));
  yv=horner(pn,xn){1};
  [p,s,mu]=polyfit(xv,yv,pn.degree[]);
  [p2,s2]=polyfit(xv,yv,pn.degree[]);
  if norm(p-pn)>s.normr then pause ;end
  if norm(s.yf-yv)>s.normr then pause ;end
  if norm(mu-[mean(xv),std(xv)])>%eps then pause ;end
  if (s.normr/s2.normr>sqrt(%eps)) then pause ;end
end
