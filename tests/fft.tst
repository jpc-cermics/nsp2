// -*- Mode: scilab -*- 
// Copyright (C) 2005 Bruno Pincon 
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
// 
// script to test fft and fft2

// test 0
if (fft([],-1) ~= []) then pause,end 

// test 1
n = 200;
x = rand(n,1);
y = fft(x,-1);
z = fft(y,1);
e=max(abs(x-z));
//printf("\n test 1: z=fft(fft(x,-1),1), max(|x-z|) = %g",e)
if e > 10*%eps then pause,end 


// test 2
n = 209;
m = 100;
x = rand(m,n);
y = fft(x,-1,"row");
z = fft(y,1,"row");
e=max(abs(x-z));
//printf("\n test 2: z=fft(fft(x,-1,""row""),1,""row""), max(|x-z|) = %g",e)
if e > 10*%eps then pause,end 

// test 3
n = 209;
m = 100;
x = rand(m,n);
y = fft(x,-1,"col");
z = fft(y,1,"col");
e=max(abs(x-z));
//printf("\n test 3: z=fft(fft(x,-1,""col""),1,""col""), max(|x-z|) = %g",e)
if e > 10*%eps then pause,end 

// test 4
n = 209;
m = 100;
x = rand(m,n);
y = fft(x,-1,"*");
z = fft(y,1,"*");
e=max(abs(x-z));
//printf("\n test 4: z=fft(fft(x,-1,""*""),1,""*""), max(|x-z|) = %g",e)
if e > 10*%eps then pause,end 

// test 5
n = 209;
m = 100;
x = rand(m,n);
y = fft2(x,-1);
z = fft2(y,1);
e=max(abs(x-z));
//printf("\n test 5: z=fft2(fft2(x,-1),1), max(|x-z|) = %g",e)
if e > 10*%eps then pause,end 

// test 6
if (fft2([],-1) ~= []) then pause,end



