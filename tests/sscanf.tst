// -*- Mode: scilab -*-
// Copyright (C) 2010 J.P Chancelier Cermics/Enpc
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
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

//------- integers 
format= '%X %2X %X';

for str=['i','d','u'] 
  for str1 = ['','l','h']
    f = strsubst(format,'X',str1+str)
    [a,b,c]=sscanf('314 312',f);
    r=[314,31,2]
    if ~r.equal[[a,b,c]] then pause,end
  end
end

//------- octal 

for str=['o']
  for str1 = ['','l','h']
    f = strsubst(format,'X',str1+str)
    [a,b,c]=sscanf('314 312',f);
    r=[3*64+8+4,3*8+1,2]
    if ~r.equal[[a,b,c]] then pause,end
  end
end

//------- hexa 

for str=['x']
  for str1 = ['','l','h']
    f = strsubst(format,'X',str1+str)
    [a,b,c]=sscanf('314 312',f);
    r=[3*16^2+16+4,3*16+1,2]
    if ~r.equal[[a,b,c]] then pause,end
  end
end

//------- doubles 

for str=['e','f']
  for str1 = ['','l']
    f = strsubst(format,'X',str1+str)
    [a,b,c]=sscanf('3.1416 312.67',f);
    r=[3.1416, 31, 2.67];
    if norm(r -[a,b,c]) > 1.e-6 then pause,end
  end
end

//------- strings 

[a,b,c]=sscanf('foo nspscicoslab','%s %3s %s');
if a<>'foo' then pause,end
if b<>'nsp' then pause,end
if c<>'scicoslab' then pause,end

//------- caracters 

L=list();
str = 'x y\n\324';
L(1:5)=sscanf(str,'%c%c%c%c%c');
L.compact[];
S=catenate(L(1));
if ~S.equal[str] then pause; end 

//-------------  [ directive 

str = '0123456789abcdef';
[a,b]=sscanf(str+'nsp','%[0-9a-f]%s');
if a<>str || b<>'nsp' then pause,end

[a,b]=sscanf(str+'nsp','%[^n]%s');
if a<>str || b<>'nsp' then pause,end

[a]=sscanf(str+'nsp','%*[^n]%s');
if a<>'nsp' then pause,end

//------------- * directive 

[a]=sscanf('10 20 30','%*d%d%*d');
if a<>20  then pause,end

[a]=sscanf('10 20 30','%*d%f%*d');
if a<>20  then pause,end

[a]=sscanf('10 20 foo','%*d%*f%s');
if a<>'foo'  then pause,end

//------------- n directive 

[a,n]=sscanf('0123456789','%s%n');
if a<>'0123456789' || n <> 10  then pause,end

L=list();
L(1:6)=sscanf('10 20 30','%d%n%d%n%f%n');
if ~L.equal[list(10,2,20,5,30,8)]  then pause,end

//-------------- examples with iterations

n=3;
A=testmatrix('magic',n);
S=string(A);
S=catenate(S,col=" "); // change to column matrix
format=catenate(smat_create(1,n,"%d"),sep=" ");
[a,b,c]=sscanf(-1,S,format);
if ~A.equal[[a,b,c]] then pause;end 

[a,b,c]=sscanf(2,S,format);
A=A(1:2,:);
if ~A.equal[[a,b,c]] then pause;end 


n=10;
A=int(100*rand(4,n));
S=string(A);
S=catenate(S,col=" "); // change to column matrix
format=catenate(smat_create(1,n,"%d"),sep=" ");
L=list();
L(1:n)=sscanf(-1,S,format);
L.compact[];
if ~A.equal[L(1)] then pause;end 




