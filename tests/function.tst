
H=hcreate(A=1,B=2,C=3);
function y=f(x,varargopt);y=varargopt;endfunction;
function y=g(x,varargopt);y=f(x,varargopt(:));endfunction;
y=g(5,H(:));
if y<>H then pause;end

function [y,z]=f(x,varargin,varargopt);y=varargopt;z=varargin;endfunction;

[y,z]=f(6,1,2,3,H(:));
if y<>H then pause;end 
if z<>list(1,2,3) then pause;end 


