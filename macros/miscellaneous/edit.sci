function edit(x,varargopt) 
//
// Copyright (C) 2010 Jean-Philippe Chancelier
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
  fname = 'TMPDIR/_editvar.sce';
  fill_file= %t;
  if exists(x,'callers') then 
    M=acquire(x); 
  elseif exists(x,'nsp-function') then 
    execstr('M='+x);
    path = M.get_path[];
    if length(path)<>0 then 
      // a library function 
      fname = path;
      fill_file = %f 
    end
  else
    return
  end 
  if fill_file then 
    // we need a way to create unique files
    fd=fopen(fname,mode="w");
    fprint(fd,M,as_read=%t,name=x,color=%f);
    fd.close[];
  end
  editfile(fname,wait=%t);
  // execute back the file and return 
  // the computed value for x 
  [ok,renv]=exec(fname,errcatch=%t,pausecatch=%t);
  if ~ok then 
    printf("Error: failed to get new value from editor\n");
    printf("%s\n",catenate(lasterror()));
  else
    execstr('resume('+x+'=renv.'+x+')');
  end
endfunction
