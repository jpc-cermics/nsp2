function [name,path,is64] = msvc_get_compiler() 

// Copyright (C) 2006-2011 Jean-Philippe Chancelier Enpc/Cermics
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
    // %win64 is equal to %t when nsp is a 64 bits program 
    if ~%win64 then return; end 
    // x86_path = getenv('ProgramFiles(x86)', '');
    if file('exists' , path + '\\bin\\amd64\\cl.exe') then
      is64 = %t;
    end 
    if file('exists' , path + '\\bin\\x86_amd64\\cl.exe') then
      is64 = %t;
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

    table = [ "msvc120pro" "Software\\Wow6432Node\\Microsoft\\VisualStudio\\12.0\\Setup\\VC"
	      "msvc120express" "Software\\Wow6432Node\\Microsoft\\VCExpress\\12.0\\Setup\\VC"
              "msvc120pro" "Software\\Microsoft\\VisualStudio\\12.0\\Setup\\VC"
	      "msvc120express" "Software\\Microsoft\\VCExpress\\12.0\\Setup\\VC"
	      
	      "msvc110pro" "Software\\Wow6432Node\\Microsoft\\VisualStudio\\11.0\\Setup\\VC"
	      "msvc110express" "Software\\Wow6432Node\\Microsoft\\VCExpress\\11.0\\Setup\\VC"
	      "msvc110pro" "Software\\Microsoft\\VisualStudio\\11.0\\Setup\\VC"
	      "msvc110express" "Software\\Microsoft\\VCExpress\\11.0\\Setup\\VC"

	      "msvc100pro", "Software\\Wow6432Node\\Microsoft\\VisualStudio\\10.0\\Setup\\VS" 
	      "msvc100pro", "Software\\Microsoft\\VisualStudio\\10.0\\Setup\\VS" 
	      "msvc100express", "Software\\Wow6432Node\\Microsoft\\VCExpress\\10.0\\Setup\\VS" 
	      "msvc100express", "Software\\Microsoft\\VCExpress\\10.0\\Setup\\VS" 

	      "msvc90pro", "Software\\Microsoft\\VisualStudio\\9.0\\Setup\\VS\\Pro" 
	      "msvc90std", "Software\\Microsoft\\VisualStudio\\9.0\\Setup\\VS\\Std"
	      "msvc90express", "Software\\Microsoft\\VCExpress\\9.0\\Setup\\VS" 

	      "msvc80pro", "Software\\Microsoft\\VisualStudio\\8.0\\Setup\\VS\\Pro" 
	      "msvc80std", "Software\\Microsoft\\VisualStudio\\8.0\\Setup\\VS\\Std"
	      "msvc80express", "Software\\Microsoft\\VCExpress\\8.0\\Setup\\VS" 
	      
	      "msvc71", "SOFTWARE\\Microsoft\\VisualStudio\\7.1\\Setup\\VC"
	      "msvc70","SOFTWARE\\Microsoft\\VisualStudio\\7.0\\Setup\\VC"  ]

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
    // if we have both vc10pro and vc100express choose 
    // vc100express.
    [name1,path1]= msvc_query_version("vc100express")
    if name1 <> "unknown" then 
      name = name1;
      path = path1;
    end
  end
  // change pathes if 64 bits required 
  [path,is64] = msvc_64bits_path(name,path);

endfunction

