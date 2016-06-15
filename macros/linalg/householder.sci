function u=householder(v,w)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (Inria)
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
//Syntax 
//u=householder(v [,w])
//Description
//given 2 column vectors v w of same size householder(v,w) returns a unitary 
//column vector u, such that (eye-2*u*u')*v is proportional to w.
//(eye-2*u*u') is the orthogonal Householder reflexion matrix 
//
// w default value is eye(v). In this case vector (eye-2*u*u')*v is the 
// vector  eye(v)*(+-norm(v))
//
  if nargin < 2 then w=eye(size(v)),end
  a=-sqrt((v'*v)/(w'*w))    
  u=v+a*w 
  u=u/norm(u)
endfunction
