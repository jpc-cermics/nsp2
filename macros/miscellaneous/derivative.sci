function [J,H] = derivative(F, x, h=[], order=2, Q=[], args=[])
//
// Copyright (C) Rainer von Seggern, Bruno Pinçon
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
//  PURPOSE
//     First and second order numerical derivatives of a function  F: R^n --> R^m 
//     by finite differences.
//     J=J(x) is the m x n Jacobian (the gradient for m=1), and H=H(x) the Hessean 
//     of the m components of F at x. The output form of H is a m*n x n
//     matrix with hessian matrices of F component stacked by row:
//                     [ H(F_1) ]           [ F_1(x) ]
//                 H = [ H(F_2) ]    F(x) = [ F_2(x) ]
//                     [ ...... ]           [ ...... ]
//                     [ H(F_m) ]           [ F_m(x) ]
//
//  NOTES
//        This function uses the 3 "internal" functions (following
//        this one in this file) :
//   
//          %DF_      => used to compute the Hessean by "differentiating
//                       the derivative"
//          %deriv1_  => contains the various finite difference formulae
//          %R_       => to deal with optional args for F
//
//  AUTHORS
//     Rainer von Seggern, Bruno Pincon
//
   if nargin<2 | nargin>6 then, error("Wrong number of input arguments"), end

   if ~((is(F,%types.PList) | is(F,%types.Func)) & (is(x,%types.Mat) && isreal(x))) then
      error("bad type for the first and/or the second argument")
   end
   
   [n,p] = size(x)
   if p ~= 1 then, error("x must be a column vector"), end
   
   if (order ~= 1 & order ~= 2 & order ~= 4) then
      error("order must be 1, 2 or 4")
   end
   
   if isempty(Q) then 
      Q = eye(n,n);
   elseif norm(clean(Q*Q'-eye(n,n)))>0 then
      error("Q must be orthogonal");
   end   
   
   if isempty(h) then
      h_not_given = %t
      select order  // stepsizes for approximation of first derivatives
	case 1 , h = sqrt(%eps)*(1 + 1e-3*norm(x))
	case 2 , h = %eps^(1/3)*(1 + 1e-3*norm(x))
	case 4 , h = %eps^(1/5)*(1 + 1e-3*norm(x))
      end	 
   else
      h_not_given = %f	
   end
   
   J = %deriv1_(F, x, h, order, Q, args)
   m = size(J,1);
   
   if nargout == 1 then, return, end

   if h_not_given then
      select order  // stepsizes for approximation of second derivatives
	case 1 , h = %eps^(1/3)*(1 + 1e-3*norm(x))
	case 2 , h = %eps^(1/4)*(1 + 1e-3*norm(x))
	case 4 , h = %eps^(1/6)*(1 + 1e-3*norm(x))
      end	 
   end
   H = %deriv1_(%DF_, x, h, order, Q, args)
   // impose symmetry for the Hessean of each component of F
   for k = 1:m
      Hk = H((k-1)*n+1:k*n,:)
      H((k-1)*n+1:k*n,:) = 0.5*(Hk + Hk')
   end
endfunction

function z=%DF_(x)
   z = %deriv1_(F, x, h, order, Q, args)';      // Transpose !
   z.redim[-1,1]
endfunction 

function g=%deriv1_(F_, x, h, order, Q, args)
   n=size(x,'*') 
   Dy=[];
   D = h*Q;
   select order
     case 1
	y=%R_(F_,x, args);
	for d=D, Dy=[Dy, %R_(F_,x+d,args)-y], end             
	g=Dy*Q'/h                    
     case 2
	for d=D, Dy=[Dy, %R_(F_,x+d,args)-%R_(F_,x-d,args)], end       
	g=Dy*Q'/(2*h)
     case 4
	for d=D
	   dFh =  (%R_(F_,x+d,args)-%R_(F_,x-d,args))/(2*h)
	   dF2h = (%R_(F_,x+2*d,args)-%R_(F_,x-2*d,args))/(4*h)
	   Dy=[Dy, (4*dFh - dF2h)/3]
	end
	g = Dy*Q'
   end//select
endfunction

function y=%R_(F_,x,args)  
   if isempty(args) then
      y=F_(x)
   else
      y = F_(x,args)
   end
endfunction
