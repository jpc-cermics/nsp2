// -*- Mode:scilab -*- 
// Copyright (C) 2005-2015 J.P Chancelier Cermics/Enpc
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

// Object used to store functions defined on grids or defined by cuts 

// TOBEDONE: we could use int64 indices and we should check that the 
// range of indices covers the range of discrete points. 
// when we have too many points indices could be turned to 
// array of int64 

// grid functions dimension 1
//---------------------------

// the grid contains 10 points in the range [0,100] (regular grid)
V=gridfn([11],[0],[100]);
// fill the function looping on possible indices 1:10
I=1:V.get_nx[];
// convert indices to points 
Pt=V.i2p[I];
// set the values 
V.pt_set_value[Pt,sin(Pt)];
// check 
if max(abs(V.pt_get_value[Pt] - sin(Pt))) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - sin(Pt))) > 10*%eps then pause;end 

// set the values through indices 
V.i_set_value[I,sin(Pt)];
// check 
if max(abs(V.pt_get_value[Pt] - sin(Pt))) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - sin(Pt))) > 10*%eps then pause;end 

// grid functions dimension 2 
//---------------------------

// cuts functions dimension 1
//---------------------------
// we give the heights and the slopes to define the function by its cuts 
// approximate exp(x) 

x=linspace(-1,1,11);

// y -> exp(x) + <exp(x),y-x> 

heights = exp(x) - x.* exp(x);
slopes = exp(x);

V=cutsfn(heights,slopes);
values=V.get_value[x];

if max(abs(values - exp(x))) > 10*%eps then pause;end
if max(abs(heights - V.get_heights[])) > 10*%eps then pause;end
if max(abs(slopes - V.get_slopes[])) > 10*%eps then pause;end

V=cutsfn(heights(1),slopes(1));
// then add slopes 
V.add_slopes[heights,slopes];
values=V.get_value[x];

// check 
if max(abs(values - exp(x))) > 10*%eps then pause;end
