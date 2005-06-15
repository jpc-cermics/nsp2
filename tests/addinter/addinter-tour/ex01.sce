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

a=1:10;b=a+1;c=ones(2,3)+2;
// calling C function 
//--------------------
[x,y,z,t]=ex1c('mul',a,b,c);

// Check the result 

function z=f(x,y);z=x+y;endfunction;
xref=feval(0:1,0:2,f);
yref=xref.* c*2;

if norm(t-(b*2)) > %eps then pause,end
if norm(z-(c*2) ) > %eps then pause,end
if norm(y-yref) > %eps then pause,end
if norm(x-xref) > %eps then pause,end

[x,y,z,t]=ex1c('add',a,b,c);
yref=xref + 2 + c;

// Check the result 
if norm(t-(b+2)) > %eps then pause,end
if norm(z-(c+2) ) > %eps then pause,end
if norm(y-yref) > %eps then pause,end
if norm(x-xref) > %eps then pause,end




