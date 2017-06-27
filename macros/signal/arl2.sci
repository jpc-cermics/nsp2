function [den,num,err]=arl2(y,den0,n,imp=%f,all=%f)
// steer, jpc, fd 1997 (Nov)
// Copyright INRIA
// test the system type 'c' 'd' or dt 
  imp=b2m(imp);
  if all then
    [den,num,err]=arl2_ius(y,den0,n,imp,'all');
  else
    [den,num,err]=arl2_ius(y,den0,n,imp);
  end
  if nargout<=1 then
    den= num ./ den ;
  end
endfunction
