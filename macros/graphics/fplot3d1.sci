function []=fplot3d1(xr,yr,f,theta,alpha,leg,flag,ebox)
  if nargin == 0 then 
    s_mat=['function [z]=surf(x,y); z=sin(x)*cos(y); endfunction';
	   't=-%pi:0.3:%pi;';
	   'plot3d1(t,t,surf);'];
    printf('%s\n',s_mat);execstr(s_mat);
  else
    printf('fplot3d: Obsolete, use plot3d(x,y,f)\n');    
  end
endfunction

