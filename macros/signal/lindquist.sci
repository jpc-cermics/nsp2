function [P,R,T]=lindquist(n,H,F,G,R0)
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
  
//[Pn,Rn,Tn]=lindquist(n,H,F,G,R0)
//macro which computes iteratively the minimal solution of the algebraic
//Riccati equation and gives the matrices Rn and Tt of the filter model,
//by the lindquist algorithm.
//   n     : number of iterations.
//   H,F,G : estimated triple from the covariance sequence of y.
//   R0    : E(yk*yk')
//   Pn    : solution of the Riccati equation after n iterations.
//   Rn,Tn : gain matrices of the filter.
// Author: G. Le Vey  Date: 16 Febr. 1989
// 
  [d,m]=size(H);
  //initialization
  Gn=G;
  Rn=R0;
  Pn=zeros_deprecated(m,m) 
  Kn=0*ones(m,d);

  //recursion
  for j=1:n,
    //  Pn=Pn+Gn/Rn*Gn'
    //  Kn=Pn*H'
    Kn=Kn+Gn/Rn*Gn'*H';
    r1=R0-H*Kn;
    Rn=Rn-Gn'*H'/r1*H*Gn;
    Gn=(F-(G-F*Kn)/r1*H)*Gn;
  end

  //gain matrices of the filter.
  //P=Pn
  //R=R0-H*P*H'
  //T=(G-F*P*H')/R
  R=R0-H*Kn
  T=(G-F*Kn)/R
  P=Kn
endfunction
