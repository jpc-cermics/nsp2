// -*- Mode: scilab -*-
// Copyright (C) 2012-2012 J.-Ph. Chancelier Cermics/Enpc
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
 
// basic test for ast 

function y=f()
  if x > 0 then 
    y=1;
  elseif x < 0 then 
    y=-1;
  else
    y=0;
  end
endfunction

g=f;  
if ~g.equal[f] then pause;end 
// get ast associated to f 
ast=pl2ast(f);
// recreate f by print/eval loop 
execstr(ast.sprint[])
if ~g.equal[f] then pause;end 

if %t then 
  // loop on macros 
  F=glob('SCI/macros/miscellaneous/*.sci');
  for i=1:size(F,'*')
    name = file('rootname",file('tail',F(1)));
    execstr(sprintf('ft=%s;ast=pl2ast(%s);',name,name));
    execstr(ast.sprint[])
    execstr(sprintf('tt=ft.equal[%s];',name));
    if ~tt then pause;end 
  end
end
