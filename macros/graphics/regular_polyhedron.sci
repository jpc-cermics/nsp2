function [V,F] = regular_polyhedron(polyhedron_name)
//
// Copyright (C) 2007 Bruno Pinçon
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
// PURPOSE 
//    provides coordinates and faces connectivity of the 5 regular
//    polyhedrons
// AUTHOR
//    Bruno Pincon <bruno.pincon@iecn.u-nancy.fr>
//
   if polyhedron_name == "hexahedron" then, polyhedron_name = "cube", end

   select polyhedron_name
      case "tetrahedron"
	V = [ 1, -1, -1,  1;...
	     -1,  1, -1,  1;...
	     -1, -1,  1,  1]
	F = [ 1  1  2  1;...
	      3  4  3  2;...
	      2  3  4  4]     
     
     case "cube"
	V = [ 1,  1, -1, -1,  1,  1, -1, -1;...
	     -1,  1,  1, -1, -1,  1,  1, -1;...
	      1,  1,  1,  1, -1, -1, -1, -1]
	F = [ 1  5  1  2  3  1;... 
	      2  8  4  6  7  5;...
	      3  7  8  7  8  6;...
	      4  6  5  3  4  2]
	
     case "octahedron"
       V = [1, -1,  0,  0,  0,  0 ; ...
	    0,  0,  1, -1,  0,  0 ; ...
	    0,  0,  0,  0,  1, -1 ]
       F = [4  4  1  1  5  3  4  2 ; ...
	    1  6  3  6  3  6  5  6 ; ...
	    5  1  5  3  2  2  2  4 ]
	
     case "dodecahedron"
       t = (sqrt(5)-1)/2; v = (sqrt(5)+1)/2   
       V = [ 0,  0,  1,  1, -1, -1,  v,  t, -t, -v, -t,  t,  v, -v,  1, -1, -1,  1,  0,  0;...
	    -t,  t, -1,  1,  1, -1,  0,  v,  v,  0, -v, -v,  0,  0,  1,  1, -1, -1, -t,  t;...
	     v,  v,  1,  1,  1,  1,  t,  0,  0,  t,  0,  0, -t, -t, -1, -1, -1, -1, -v, -v]
       F = [ 1  1  2  1  3  4 10  6 12 13 14  8;...
	     3  2  4  6 12  7  5 10 11 18 16 15;...
	     7  5  8 11 18 13  9 14 17 19 20 20;...
	     4 10  9 12 13 15 16 17 19 20 19 16;...
	     2  6  5  3  7  8 14 11 18 15 17  9]
       
       case "icosahedron"
	 t = (sqrt(5)-1)/2
	 v = 1-t
	 V = [ v, -v,  0,  0,  t,  t, -t, -t,  0,  v, -v,  0;...
	       0,  0,  t, -t, -v,  v,  v, -v, -t,  0,  0,  t;...
	       t,  t,  v,  v,  0,  0,  0,  0, -v, -t, -t, -v]
	 F = [ 1  3  1  6  2  8  1  3 12  2  4  9 10 11  6 12 11  5 11 10;...
	       2  2  4  3  3  4  5  6  7  7  8  5  6  8 10 11  9  9 10 11;...
	       4  1  5  1  7  2  6 12  3  8  9  4  5  7 12  7  8 10  9 12]
	 
   else
      error("bad argument")
   end
endfunction
