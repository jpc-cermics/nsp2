// -*- Mode: scilab -*-
// Copyright (C) 2005 J.P Chancelier Cermics/Enpc
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

// basic tests for exists and acquire 

function y=f(x,flag);foo=35; y=g(x,flag);endfunction ;
function y=g(x,flag);y=exists(x,flag);endfunction ;

goo=34;
  
if ~exists('goo','all') then pause;end 
if exists('foo','all') then pause;end 
if ~f('goo','all') then pause;end 
if ~f('goo','callers') then pause;end 
if f('goo','caller') then pause;end 
if ~f('foo','caller') then pause;end 

function y=g(x,flag);y=acquire(x);endfunction ;

if f('goo','')<>goo then pause;end 
if f('foo','')<>35 then pause;end 
