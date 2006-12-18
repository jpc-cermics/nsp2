function [ret, flag, new_data] = kim_sys(y, fct, data)
// Wrapper around the actual user-provided Matlab function
// This should be removed since we are able to call 
// fct at C-level.
  if isempty(data) then 
    execstr('[ret, flag] ='+fct+'(y)');
    new_data = [];
  else
    execstr('[ret, flag, new_data] ='+fct+'(y,data)');
  end
endfunction

function [flag, new_data] = kim_pset(y, yscale, fy, fscale, fct, data)
//
// Wrapper around the actual user-provided Matlab function
//
  if isempty(data) then 
    execstr('[flag] ='+fct+'(y,yscale,fy,fscale)');
    new_data = [];
  else
    execstr('[flag, new_data] ='+fct+'(y,yscale,fy,fscale,data)');
  end
endfunction

function [ret, flag, new_data] = kim_djac(y, fy, fct, data)
//
// Wrapper around the actual user-provided Matlab function
//
  if isempty(data) then 
    execstr('[ret, flag] ='+fct+'(y,fy)');
    new_data = [];
  else
    execstr('[ret, flag, new_data] ='+fct+'(y,fy,data)');
  end
endfunction

function [ret, flag, new_data] = kim_bjac(y, fy, fct, data)
//
// Wrapper around the actual user-provided Matlab function
//
  if isempty(data) then 
    execstr('[ret, flag] ='+fct+'(y,fy)');
    new_data = [];
  else
    execstr('[ret, flag, new_data] ='+fct+'(y,fy,data)');
  end

endfunction

function [flag, new_data] = kim_gcom(y, f, data)
//
// Wrapper around the actual user-provided Matlab function
//
  if isempty(data) then 
    execstr('[flag] ='+fct+'(y)');
    new_data = [];
  else
    execstr('[flag, new_data] ='+fct+'(y,data)');
  end
endfunction

function [gval, flag, new_data] = kim_gloc(y, fct, data)
//
// Wrapper around the actual user-provided Matlab function
//
  if isempty(data) then 
    execstr('[gval,flag] ='+fct+'(y)');
    new_data = [];
  else
    execstr('[gval,flag, new_data] ='+fct+'(y,data)');
  end
endfunction

function [ret, flag, new_data] = kim_psol(y, yscale, fy, fscale, v, fct, data)
//
// Wrapper around the actual user-provided Matlab function
//
  if isempty(data) then 
    execstr('[ret,flag] ='+fct+'(y,yscale,fy,fscale,v)');
    new_data = [];
  else
    execstr('[ret,flag, new_data] ='+fct+'(y,yscale,fy,fscale,v,data)');
  end
endfunction

function [ret, new_y, flag, new_data] = kim_jtv(y, v, new_y, fct, data)
//
// Wrapper around the actual user-provided Matlab function
// 
  if isempty(data) then 
    execstr('[ret,new_y, flag] ='+fct+'(y,v,new_y)');
    new_data = [];
  else
    execstr('[ret,new_y, flag, new_data] ='+fct+'(y,v,new_y,data)');
  end
endfunction

function varargout = kim_info(action, fin, message);
  printf("In kim info \n");
endfunction

