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

Bref =[ "Un","Trois","Cinq"; "Deux", "Quatre","Six" ];

// first example 

[A,B]=ex5c_1(string(1:10));
if B<>string(1:10) then pause,end
if A<>Bref then pause,end

// second example 

I= ex5c_2("Cinq",Bref) ;
if I<>5 then pause,end


