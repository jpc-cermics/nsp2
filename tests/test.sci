function [T]=Triu(a,k)
[m,n]=size(a);T=a;
if k>=0;for i=1:m;for j=1:mini(i+k-1,n),T(i,j)=0;end;end;
else if -k+1<=m ; for i=(-k+1):m;for j=1:mini(i+k-1,n),T(i,j)=0;end;end;end;end
