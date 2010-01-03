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

// test of input output 
// type1 =  'i', 'l', 's', 'c', 'd', 'f', 
// type2 =  'i', 'l', 's', 'c', ' '
// type = (type1 | 'u' + type2 ) + ('' | 'b' | 'l') 
 
fd = fopen('TMPDIR/test.bin',mode='wb');
data = [56, 567, 34, 45, 56.78, 45];
type1 = [ 'i', 'l', 's', 'c', 'd', 'f'];
for i=1:length(data)
  fd.put[data(i), type = type1(i)];
  fd.put[data(i), type = type1(i)+' '+'b']; // force bug-endian
  fd.put[data(i), type = type1(i)+' '+'l']; // force little-endian
end
fd.close[];
fd = fopen('TMPDIR/test.bin',mode='rb');
for i=1:length(data)
  data1(i)=fd.get[n=1, type = type1(i)];
  data2(i)=fd.get[n=1, type = type1(i)+' '+'b']; // force bug-endian
  data3(i)=fd.get[n=1, type = type1(i)+' '+'l']; // force little-endian
end
fd.close[];

if ~data(1:5).equal[data1(1:5)] then pause;end 
if norm(data(6)-data1(6))> 1.e-6 then pause; end 
if ~data(1:5).equal[data2(1:5)] then pause;end 
if norm(data(6)-data2(6))> 1.e-6 then pause; end 
if ~data(1:5).equal[data3(1:5)] then pause;end 
if norm(data(6)-data3(6))> 1.e-6 then pause; end 

// strings 

fd = fopen('TMPDIR/test.bin',mode='wb');
fd.putstr['nsp'];
fd.put[1:10];
fd.putstr['nsp'];
fd.put[ascii('nsp'),type='c'];
fd.close[];

fd = fopen('TMPDIR/test.bin',mode='rb');
str1=fd.getstr[n=3];
x=fd.get[n=10];
str2=fd.getstr[n=3];
c=fd.get[n=3,type='c'];
str3=ascii(c);
fd.close[];

if ~str1.equal['nsp'] then pause;end 
if ~str2.equal['nsp'] then pause;end 
if ~str3.equal['nsp'] then pause;end 
if ~x.equal[1:10] then pause;end 

// mseek and tell

fd = fopen('TMPDIR/test.bin',mode='wb');
fd.put[1:10,type='d'];
if fd.tell[] <> 80 then pause;end 
fd.seek[0]; //  'set', 'cur', or 'end'
if fd.tell[] <> 0 then pause;end
fd.seek[8];
fd.put[20,type='d'];
fd.seek[0,'end'];
if fd.tell[] <> 80 then pause;end 
fd.put[2*(1:10),type='d'];
fd.close[];
fd = fopen('TMPDIR/test.bin',mode='rb');
x= fd.get[n=20,type='d'];
if ~x.equal[[1,20,3:10,2*(1:10)]] then pause;end 
fd.seek[-80,'end'];
x= fd.get[n=10,type='d'];
if ~x.equal[[2*(1:10)]] then pause;end 
x=fd.get[n=1,type='c'];
if ~isempty(x) then pause; end 
if ~fd.eof[] then pause;end 
if fd.tell[] <> 160 then pause;end 
fd.clearerr[];
if fd.eof[] then pause;end 
fd.close[];


//mixing with printf and put_matrix 

fd = fopen('TMPDIR/test.bin',mode='wb');
fd.put[1:3,type='d'];
fd.putstr['foo'];
A=rand(3,4);
fd.put_matrix[A];
fd.printf["%d",789];
fd.put[1:3,type='c'];
fd.close[];

fd = fopen('TMPDIR/test.bin',mode='rb');
x=fd.get[n=3,type='d'];
str=fd.getstr[n=3];
A1=fd.get_matrix[];
y=fscanf(fd,"%d"); // XXX method is missing 
z=fd.get[n=3,type='c'];
fd.close[];

if ~x.equal[1:3] then pause;end 
if norm(A-A1) > 1.e-5 then pause;end 
if y <> 789 then pause;end 
if ~z.equal[1:3] then pause;end 






