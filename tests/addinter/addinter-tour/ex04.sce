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
// create nsp objects from C arrays 

x1=ex4c_1();
x1ref=matrix(1:5*3,3,5);
if norm(x1-x1ref) > %eps then pause,end

//matrix (int) created by C function
x2=ex4c_2();
x2ref = matrix((1:5*3),3,5);
if norm(x2- x2ref) > %eps then pause,end

//Character string created by C function
x3=ex4c_3();
if x3<>"Nsp is ..." then pause,end

// all together 

[y1,y2,y3]=ex4c_4();
if y1<>"Nsp is ..." then pause,end
if norm(y2-x2ref) > %eps then pause,end
if norm(y3-x1ref) > %eps then pause,end

