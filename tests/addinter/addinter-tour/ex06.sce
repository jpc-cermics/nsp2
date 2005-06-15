// -*- Mode: scilab -*- 
// Copyright (C) 2005 Jean-Philippe Chancelier 
//                    Francois Delebecque 
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
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 
// 
// test with sparse

N=4;
A=rand(N,N);B=rand(N,N);
A(A>=0.7)=0;
B(B>=0.7)=0;
W=sparse(A);

W1=ex6c_1(W); 

if norm(full(W1)-full(W)*(2+%i*3)) > %eps  then pause,end

// a complex sparse as input-argument 

Ac = A+%i*B;
W=sparse(Ac);
W1=ex6c_1(W); 
if norm(full(W1)-full(W)*(2+%i*3)) > %eps  then pause,end

