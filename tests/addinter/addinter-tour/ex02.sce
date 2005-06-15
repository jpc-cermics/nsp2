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

a=ex2c([1,2,3]+%i*[4,5,5]);
if norm(a-2*[1,2,3]-%i*[4,5,5]*3) > %eps then pause,end

a=ex2c([1,2,3]);
if norm(a-2*[1,2,3]) > %eps then pause,end















