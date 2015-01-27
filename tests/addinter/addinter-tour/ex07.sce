// -*- Mode: scilab -*- 
// Copyright (C) 2005-2015 Jean-Philippe Chancelier 
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
// test with list 

A=testmatrix('magic',5);
L=list(testmatrix('magic',5),'poo',list(8,'foo'));

L1=ex7c_1(L);
if length(L1)<> 4 then pause,end
if norm(L1(4)-sin(A))> 100*%eps then pause,end










