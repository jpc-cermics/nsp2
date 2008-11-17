// -*- Mode: scilab -*- 
// Copyright (C) 2008 Bruno Pincon 
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
// script to test fftnew, ifftnew and fft2new, ifft2new
// now fftnew is fft, ifftnew is ifft, fft2new is fft2 and ifft2new is
// ifft2. So this is a file to test these functions.


// test 0
if ~fft([]).equal[[]] then pause,end 
if ~fft(zeros(0,2)).equal[zeros(0,2)] then pause,end 
if ~fft(zeros(2,0)).equal[zeros(2,0)] then pause,end 
if ~ifft([]).equal[[]] then pause,end 
if ~ifft(zeros(0,2)).equal[zeros(0,2)] then pause,end 
if ~ifft(zeros(2,0)).equal[zeros(2,0)] then pause,end 


// test 1
n = 200;
x = rand(n,1);
y = fft(x);
z = ifft(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then,if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 1bis
n = 200;
x = rand(n,1)+%i*rand(n,1);
y = fft(x);
z = ifft(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 1ter
n = 200;
x = rand(1,n);
y = fft(x);
z = ifft(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then,if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 2
n = 209;
m = 100;
x = rand(m,n);
y = fft(x,1);
yb = fft(x, dim=1);
if ~y.equal[yb] then, pause, end
yb = fft(x, dim="r");
if ~y.equal[yb] then, pause, end
clear yb
z = ifft(y,dim=1);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then,if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 2bis
n = 209;
m = 100;
x = rand(m,n)+%i*rand(m,n);
y = fft(x,1);
yb = fft(x, dim=1);
if ~y.equal[yb] then, pause, end
yb = fft(x, dim="r");
if ~y.equal[yb] then, pause, end
clear yb
z = ifft(y,dim=1);
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 3
n = 209;
m = 100;
x = rand(m,n);
y = fft(x,dim=2);
yb = fft(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = ifft(y, "c");
zb = ifft(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 3bis
n = 209;
m = 100;
x = rand(m,n)+%i*rand(m,n);
y = fft(x,dim=2);
yb = fft(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = ifft(y, "c");
zb = ifft(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 4
n = 209;
m = 100;
x = rand(m,n);
y = fft(x,dim="*");
yb = fft(x,0);
if ~y.equal[yb] then, pause, end
yb = fft(x);
if ~y.equal[yb] then, pause, end

z = ifft(y,"*");
zb = ifft(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 4bis
n = 209;
m = 100;
x = rand(m,n)+%i*rand(m,n);
y = fft(x,dim="*");
yb = fft(x,0);
if ~y.equal[yb] then, pause, end
yb = fft(x);
if ~y.equal[yb] then, pause, end

z = ifft(y,"*");
zb = ifft(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 5
n = 209;
m = 100;
x = rand(m,n);
y = fft2(x);
z = ifft2(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 5bis
n = 209;
m = 100;
x = rand(m,n) + %i*rand(m,n);
y = fft2(x);
z = ifft2(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 6
if ~fft2([]).equal[[]] then pause,end
if ~fft2(zeros(0,4)).equal[zeros(0,4)] then pause,end
if ~fft2(zeros(3,0)).equal[zeros(3,0)] then pause,end
if ~ifft2([]).equal[[]] then pause,end
if ~ifft2(zeros(0,4)).equal[zeros(0,4)] then pause,end
if ~ifft2(zeros(3,0)).equal[zeros(3,0)] then pause,end


// test 7 (test hermitian redundancy for big vectors)
x = rand(1e5,1);
y = fft(x);
z = ifft(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(1e5,1) + %i*0;  // x should be complex
if isreal(x,%t) then, pause, end
y = fft(x);
z = ifft(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(1e6,1) + %i*0;  // x should be complex
if isreal(x,%t) then, pause, end
y = fft(x);
z = ifft(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(5e2,5e2);
y = fft2(x);
z = ifft2(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(5e2,5e2) + %i*0;  // x should be complex
if isreal(x,%t) then, pause, end
y = fft2(x);
z = ifft2(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 



// tests 8 (do ifft first)
n = 200;
x = rand(n,1);
y = ifft(x);
z = fft(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 180;
x = rand(n,1)+%i*rand(n,1);
y = ifft(x);
z = fft(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 190;
x = rand(1,n);
y = fft(x);
z = ifft(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 209;
m = 100;
x = rand(m,n);
y = ifft(x,1);
yb = ifft(x, dim=1);
if ~y.equal[yb] then, pause, end
z = fft(y,dim=1);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 209;
m = 110;
x = rand(m,n)+%i*rand(m,n);
y = ifft(x,1);
yb = ifft(x, dim=1);
if ~y.equal[yb] then, pause, end
yb = ifft(x, dim="r");
if ~y.equal[yb] then, pause, end
clear yb
z = fft(y,dim=1);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 209;
m = 100;
x = rand(m,n);
y = ifft(x,dim=2);
yb = ifft(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = fft(y, "c");
zb = fft(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 208;
m = 150;
x = rand(m,n)+%i*rand(m,n);
y = ifft(x,dim=2);
yb = ifft(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = fft(y, "c");
zb = fft(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 139;
m = 120;
x = rand(m,n);
y = ifft(x,dim="*");
yb = ifft(x,0);
if ~y.equal[yb] then, pause, end
yb = ifft(x);
if ~y.equal[yb] then, pause, end

z = fft(y,"*");
zb = fft(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 100*%eps then pause,end 

n = 126;
m = 200;
x = rand(m,n)+%i*rand(m,n);
y = ifft(x,dim="*");
yb = ifft(x,0);
if ~y.equal[yb] then, pause, end
yb = ifft(x);
if ~y.equal[yb] then, pause, end

z = fft(y,"*");
zb = fft(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 200;
m = 120;
x = rand(m,n);
y = ifft2(x);
z = fft2(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 200;
m = 100;
x = rand(m,n) + %i*rand(m,n);
y = ifft2(x);
z = fft2(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 
