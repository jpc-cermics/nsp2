function [v]=ell1mag(eps,m1,z)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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
  
//Function used for squared magnitude of an elliptic filter
//Usually m1=eps*eps/(a*a-1);
//  eps     :Passband ripple=1/(1+eps**2)
//  m1      :Stopband ripple=1/(1+(eps**2)/m1)
//  z       :Sample vector of values in the complex plane
//  v       :Elliptic filter values at sample points
//
// 
  s=%sn(z,m1);un=ones_deprecated(z);
  v=real(un./(un+eps*eps*s.*s))

  
endfunction
