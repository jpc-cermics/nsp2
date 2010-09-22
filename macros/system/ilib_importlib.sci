function str1=ilib_importlib(str, testw32=%f)
// try to generate proper calls 
// for import libraries describes in str 
// str can contain a list of import libraries 
// 
  [msvc]=msvc_get_compiler();
  if testw32 then msvc="forced";end 
  libs = split(str,sep=' ',msep=%t);
  str1=m2s([]);
  for s=libs 
    if part(s,1:2)== "-l" then 
      if msvc == "unknown" then 
	str1.concatr[s];
      else
	str1.concatr['lib'+part(s,3:length(s))+'.lib'];
      end
    else
      d = file('dirname',s);
      f = file('rootname',file('tail',s));
      if  part(f,1:3)== 'lib' then 
	f = part(f,4:length(f));
      else
	error("name should start with lib");
      end
      if msvc == "unknown" then 
	if d <> "." then 
	  str1.concatr['-L'+d];
	end
	str1.concatr['-l'+f];
      else
	str1.concatr[file('join',[d,'lib'+f+'.lib'])];
      end
    end
  end
endfunction
    

    
    
