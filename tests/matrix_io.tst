// -*- Mode: scilab -*- 
// Copyright (C) 2005 J.P Chancelier Cermics/Enpc 
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

// test of fileio 
// --------------
// FIXME: get_matrix when the put_matrix used a sep = ';' 

// write matrix in a file with fprintf 
//------------------------------------

n=50;
a=rand(n,n,'u');
// now the data 
fd=fopen('TMPDIR/Mat',mode='w');
texte=['Some text ';'Some more text'];
for t=texte 
  fprintf(fd,'%s\n',t);
end 
for i=1:n ,
  for j=1:n, fprintf(fd,'%5.2f ',a(i,j));end;
  fprintf(fd,'\n');	
end
fd.close[];

// read matrix back with method get_matrix 
// ---------------------------------------

// bypass header 
fd=fopen('TMPDIR/Mat',mode='r');
a1=fd.get_matrix[];
fd.close[];
if max(a1-a) > 1.e-1 then pause,end 

// read header 
fd=fopen('TMPDIR/Mat',mode='r');
[a1,txt]=fd.get_matrix[];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

// reread with given format 

fd=fopen('TMPDIR/Mat',mode='r');
[a1,txt]=fd.get_matrix[format='%5.2f'];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

// write matrix  with put_matrix method 
//--------------------------------------

a=rand(n,n,'u');
fd=fopen('TMPDIR/Mat',mode='w');
// options 
// fd.put_matrix[a,format='%5.2f',title="// poo",sep=' '];
fd.put_matrix[a]
fd.close[];

// read back to test 
fd=fopen('TMPDIR/Mat',mode='r');
a1=fd.get_matrix[];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

// write matrix  with put_matrix method 
// with options 
// ---------------------------------------

a=rand(n,n,'u');
fd=fopen('TMPDIR/Mat',mode='w');
text="// my title ";
fd.put_matrix[a,format='%5.2f',title=texte,sep=' '];
fd.close[];

fd=fopen('TMPDIR/Mat',mode='r');
[a1,txt]=fd.get_matrix[];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

// write matrix  with print  method option as_read 
// ---------------------------------------------

a=rand(n,n,'u');
fd=fopen('TMPDIR/Mat',mode='w');
text="// my title ";
fd.print[a,'as_read'];
fd.close[];

// reread with exec ! 
a1=a;
rep= exec('TMPDIR/Mat',errcatch=%t)
if ~rep then pause;end 
if max(a1-a) > 1.e-1 then pause,end 

// write matrix with printf method 
// -------------------------------

n=5;
a=rand(n,n,'u');
// now the data 
fd=fopen('TMPDIR/Mat',mode='w');
texte=['Some text ';'Some more text'];
for t=texte 
  fd.printf['%s\n',t];
end 
for i=1:n ,
  for j=1:n, fd.printf['%5.2f ',a(i,j)];end;
  fd.printf['\n'];	
end
fd.close[];

// read to check 
fd=fopen('TMPDIR/Mat',mode='r');
a1=fd.get_matrix[];
fd.close[];
if max(a1-a) > 1.e-1 then pause,end 


// demo: read a matrix with mixed colmuns of strings and numbers 
// -------------------------------

Ar=rand(5,4);

B=m2s(Ar);
col1='foo' + m2s([1:5]');
col2='poo' + m2s([1:5]');
coln='koo' + m2s([1:5]');
B=[col1,col2,B,coln];
B=catenate(B,col=' ');

fd=fopen('test.data',mode='w');
fd.put_smatrix[B];
fd.close[];

// reading back 

fd=fopen('test.data',mode='r');
A=fd.get_smatrix[];
fd.close[];

A=split(A,msep=%t,sep=' ')

S=A(:,[1,2,$])
D = strtod(A); 
D = D(:,3:$-1)

if or(S<>[col1,col2,coln]) then pause;end 
if max(abs(D-Ar)) >= 1.e-4 then pause;end 











