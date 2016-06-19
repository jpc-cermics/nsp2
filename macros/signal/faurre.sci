function [P,R,T]=faurre(n,H,F,G,R0)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - G. Le Vey (INRIA)
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
  
//[P,R,T]=faurre(n,H,F,G,R0)
//macro which computes iteratively the minimal solution of the algebraic
//Riccati equation and gives the matrices Rt and Tt of the filter model.
//   n     : number of iterations.
//   H,F,G : estimated triple from the covariance sequence of y.
//   R0    : E(yk*yk')
//   P    : solution of the Riccati equation after n iterations.
//   R,T  : gain matrix of the filter.
// Author: G. Le Vey  Date: 16 Febr. 1989
// 

//initialization
  Pn=G*inv(R0)*G'
  //recursion
  for k=1:n, 
    A=(G-F*Pn*H');
    Pn=F*Pn*F'+A/(R0-H*Pn*H')*A',
  end
  P=Pn
  //gain matrices of the filter.
  R=R0-H*P*H';
  T=(G-F*P*H')/R;
endfunction
