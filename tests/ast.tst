// -*- Mode: nsp -*-
// Copyright (C) 2012-2015 J.-Ph. Chancelier Cermics/Enpc
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
// Note: when space are equivalent to , in matrices 
//       the result for the ast is not the same when parsing
//  [x y,z] -> (colconcat x (colconcat y z))
//  [x,y,z] -> (colconcat (colconcat x y) z)).
// but when printing ast the comma is always added 
// thus parse -> ast -> print -> parse do not give identity.

function y=f()
  if x > 0 then 
    y=1;
  elseif x < 0 then 
    y=-1;
  else
    y=0;
  end
endfunction;

g=f;  
if ~g.equal[f] then pause;end 

// get ast associated to f 
ast=pl2ast(f);
// recreate f by print/eval loop 
[ok,H]=execstr(ast.sprint[]);
if ~ok then pause;end 
if ~f.equal[H('f')] then pause;end 

// iterate 
astn=pl2ast(H('f'));
if ~ast.equal[astn] then pause;end 

if %f then 
// loop on macros testing 
// pl2ast and then ast print and eval 
F=glob('SCI/macros/*/*.sci');

for i=1:size(F,'*')
  name = file('rootname",file('tail',F(i)));
  if name == "00util" then continue;end;
  ok=execstr(sprintf('ft=%s;",name),errcatch=%t);
  if ~ok then continue;end // macro is not loaded
  ok=execstr(sprintf('ast=pl2ast(%s);',name),errcatch=%t);
  if ~ok then printf("Error step 1 for %s\n",name); pause;end 
  [ok,H]=execstr(ast.sprint[],errcatch=%t);
  if ~ok then printf("Error step 2 for %s\n",name);pause;end 
  ok=execstr(sprintf('tt=ft.equal[H(''%s'')];',name,name),errcatch=%t);
  if ~ok then printf("Error step 3 for %s\n",name);pause;end 
  if ~tt then printf("Error step 4 for %s\n",name);pause;end 
  // iterate one more time 
  ok=execstr(sprintf('ast1=pl2ast(H(''%s''));',name),errcatch=%t);
  if ~ok then printf("Error step 5 for %s\n",name);pause;end 
  if ~ast1.equal[ast] then  printf("Error step 6 for %s\n",name);pause;end 
end
// test parse_file 
for i=1:size(F,'*')
  name = file('rootname",file('tail',F(i)));
  if name == "00util" then continue;end;
  ok=execstr(sprintf('ft=%s;",name),errcatch=%t);
  if ~ok then continue;end // macro is not loaded
  // here we parse the file, thus ast will contain more 
  // than just one function
  ast=parse_file(F(i));
  [ok,H]=execstr(ast.sprint[],errcatch=%t);
  if ~ok then printf("Error step 7 for %s\n",name);pause;end 
  ok=execstr(sprintf('tt=ft.equal[H(''%s'')];',name,name),errcatch=%t);
  if ~ok then printf("Error step 8 for %s\n",name);pause;end 
  if ~tt then printf("Error step 9 for %s\n",name);pause;end 
end
end 

