// -*- Mode: scilab -*- 
function [y,z]=f(x) 
  try 
    y='try';
    z='try';
    bug=[4:10]+x;
    return;
  catch 
    y='catch'
    z=lasterror(),
  finally 
    y='final";
  end;
endfunction
