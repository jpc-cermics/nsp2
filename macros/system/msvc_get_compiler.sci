function [name,path,is64] = msvc_get_compiler() 

// Copyright (C) 2006-2010 Jean-Philippe Chancelier Enpc/Cermics
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
// used to detect a microsoft c compiler 
    
  function [path,is64] = msvc_64bits_path(name,path) 
  // changes the path for 64 bits machines 
  // if nsp was compiled as a 64bits program.
  // is64 will be set to %t if 64 bits are supported 
  // and found 
    is64=%f;
    %win64=%f // not already defined 
    if ~%win64 then return; end 
    x86_path = getenv('ProgramFiles(x86)', '');
    if name == 'msvc100express' then 
      if file('exists', x86_path + '\\Microsoft Visual Studio 10.0\\VC\\bin\\amd64\\cl.exe') then
	is64=%t
	path = x86_path + '\\Microsoft Visual Studio 10.0';
      end
    end
    if name == 'msvc90express' then
      if file('exists', x86_path + '\\Microsoft Visual Studio 9.0\\VC\\bin\\amd64\\cl.exe') then
	is64=%t;
	path = x86_path + '\\Microsoft Visual Studio 9.0';
      end
    end
    if ~isempty(find(name == ['msvc100pro', 'msvc90pro', 'msvc90std'])) then
      is64=%t;
    end
  endfunction
  
  function [name,path]=msvc_query_version(query_name) 
  // returns the most recent msvc compiler found 
  // if query_name is given then only query for the 
  // given compiler 
    name="unknown";path="";
    table = [ "vc100pro", "Software\\Microsoft\\VisualStudio\\10.0\\Setup\\VS" 
	      "vc100express", "Software\\Microsoft\\VCExpress\\10.0\\Setup\\VS" 
	      "vc90pro", "Software\\Microsoft\\VisualStudio\\9.0\\Setup\\VS\\Pro" 
	      "vc90std", "Software\\Microsoft\\VisualStudio\\9.0\\Setup\\VS\\Std"
	      "vc90express", "Software\\Microsoft\\VCExpress\\9.0\\Setup\\VS" 
	      "vc80pro", "Software\\Microsoft\\VisualStudio\\8.0\\Setup\\VS\\Pro" 
	      "vc80std", "Software\\Microsoft\\VisualStudio\\8.0\\Setup\\VS\\Std"
	      "vc80express", "Software\\Microsoft\\VCExpress\\8.0\\Setup\\VS" 
	      "vc71", "SOFTWARE\\Microsoft\\VisualStudio\\7.1\\Setup\\VC"
	      "vc70","SOFTWARE\\Microsoft\\VisualStudio\\7.0\\Setup\\VC"  ]
    
    vals=1:size(table,1);
    if nargin >=1 then vals=find(query_name == table(:,1));end 
    for i=vals
      key=table(i,2);
      ok = execstr("path =registry(''HKEY_LOCAL_MACHINE'',key,''ProductDir'')",errcatch=%t);
      if ~ok then 
	lasterror();
      else
	name = table(i,1); 
	return;
      end
    end
  endfunction
  
  // get a name and path for compiler 
  [name,path ]= msvc_query_version();
  if name == "vc100pro" then 
    // if we have both vc10pro and vc10express choose 
    // vc10express.
    [name1,path1]= msvc_query_version("vc100express")
    if name1 <> "unknown" then 
      name = name1;
      path = path1;
    end
  end
  // change pathes if 64 bits required 
  [path,is64] = msvc_64bits_path(name,path);

endfunction

