
N=50;
x=int(10*rand(N,3));
[xs,ks]=gsort(x,'lr','d');
xx=xs*[100;10;1];
I=find(xx(2:$)-xx(1:$-1) > 0);
if I <> [] then pause,end 

[xs,ks]=gsort(x,'lr','i');
xx=xs*[100;10;1];
I=find(xx(2:$)-xx(1:$-1) < 0);
if I <> [] then pause,end 

function [x,ind] = lex_sort1(x,varargin)
// should not work since 
// sort in nsp is not stable (sort is gsort)
  ind = 1:size(x,1);
  for i=size(x,2):-1:1,
    [s,k] = sort(x(ind,i));
    ind = ind(k);
  end
  x=x(ind,:)
endfunction

if ~exists('%nsp') then 
  N=500;
  x=int(10*rand(N,3));
  xs=lex_sort1(x);
  xx=xs*[100;10;1];
  I=find(xx(2:$)-xx(1:$-1) > 0);
  if I <> [] then pause,end 
end
