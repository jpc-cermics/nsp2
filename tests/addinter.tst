// -*- Mode: scilab -*-
// Copyright (C) 2005-2010 J.P Chancelier Cermics/Enpc
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
 
// basic test for link and addinter 

// create a shared library 
rep=system('../bin/nsplibtool OBJS=addinter.o LIBRARY=libaddinter > /dev/null');
if rep<> 0 then pause;end 
// link shared library 
if ~c_link('libaddinter_Interf') then 
   ilib=addinter('./libaddinter'+%shext,'libaddinter');
else 
   printf('libaddinter already loaded\n');
end
// test that first and second work
if first(1:5)<>2*(1:5) then pause;end
if second([%t,%f])<>~[%t,%f] then pause,end

if %f then 
  // get all the entry points 
  H=link();
  // check that the interface is present in H
  if ~H.iskey['libaddinter_Interf'] then pause;end
  if H('libaddinter_Interf')<>ilib then pause;end
end

// dlclose the shared archive 
ulink(ilib)
// clean files 
rep=system('../bin/nsplibtool OBJS=addinter.o LIBRARY=libaddinter distclean > /dev/null');

// test that shared archive is unlinked 
if c_link('libaddinter_Interf') then pause,end

// check if we can access nsp symbols 
// with dynamic linking. 

rep=execstr('ilib=link(''nsp'',''nsp_matrix_print'')',errcatch=%t);
if rep == %f then pause,end
// access other symbols through ilib 
link(ilib,'nsp_matrix_info');
ulink(ilib);


