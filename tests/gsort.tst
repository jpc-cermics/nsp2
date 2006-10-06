// -*- Mode: scilab -*- 

N=50;
x=int(10*rand(N,3));
[xs,ks]=gsort(x,'lr','d');
xx=xs*[100;10;1];
I=find(xx(2:$)-xx(1:$-1) > 0);
if ~isempty(I) then pause,end 

[xs,ks]=gsort(x,'lr','i');
xx=xs*[100;10;1];
I=find(xx(2:$)-xx(1:$-1) < 0);
if ~isempty(I) then pause,end 

function [x,ind] = lex_sort1(x,dir)
  ind = 1:size(x,1);
  for i=size(x,2):-1:1,
    // using a stable sort 
    [s,k] = gsort(x(ind,i),'gs',dir);
    ind = ind(k);
  end
  x=x(ind,:)
endfunction

N=5;
x=int(4*rand(N,3));
xs=lex_sort1(x,'i');
xx=xs*[100;10;1];
I=find(xx(2:$)-xx(1:$-1) < 0);
if ~isempty(I) then pause,end 

xs=lex_sort1(x,'d');
xx=xs*[100;10;1];
I=find(xx(2:$)-xx(1:$-1) > 0);
if ~isempty(I) then pause,end 


