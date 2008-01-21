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

// test 0
if ~fftnew([]).equal[[]] then pause,end 
if ~fftnew(zeros(0,2)).equal[zeros(0,2)] then pause,end 
if ~fftnew(zeros(2,0)).equal[zeros(2,0)] then pause,end 
if ~ifftnew([]).equal[[]] then pause,end 
if ~ifftnew(zeros(0,2)).equal[zeros(0,2)] then pause,end 
if ~ifftnew(zeros(2,0)).equal[zeros(2,0)] then pause,end 


// test 1
n = 200;
x = rand(n,1);
y = fftnew(x);
z = ifftnew(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then,if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 1bis
n = 200;
x = rand(n,1)+%i*rand(n,1);
y = fftnew(x);
z = ifftnew(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 1ter
n = 200;
x = rand(1,n);
y = fftnew(x);
z = ifftnew(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then,if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 2
n = 209;
m = 100;
x = rand(m,n);
y = fftnew(x,1);
yb = fftnew(x, dim=1);
if ~y.equal[yb] then, pause, end
yb = fftnew(x, dim="r");
if ~y.equal[yb] then, pause, end
clear yb
z = ifftnew(y,dim=1);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then,if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 2bis
n = 209;
m = 100;
x = rand(m,n)+%i*rand(m,n);
y = fftnew(x,1);
yb = fftnew(x, dim=1);
if ~y.equal[yb] then, pause, end
yb = fftnew(x, dim="r");
if ~y.equal[yb] then, pause, end
clear yb
z = ifftnew(y,dim=1);
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 3
n = 209;
m = 100;
x = rand(m,n);
y = fftnew(x,dim=2);
yb = fftnew(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = ifftnew(y, "c");
zb = ifftnew(y, dim=2);
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
y = fftnew(x,dim=2);
yb = fftnew(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = ifftnew(y, "c");
zb = ifftnew(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 4
n = 209;
m = 100;
x = rand(m,n);
y = fftnew(x,dim="*");
yb = fftnew(x,0);
if ~y.equal[yb] then, pause, end
yb = fftnew(x);
if ~y.equal[yb] then, pause, end

z = ifftnew(y,"*");
zb = ifftnew(y, dim=0);
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
y = fftnew(x,dim="*");
yb = fftnew(x,0);
if ~y.equal[yb] then, pause, end
yb = fftnew(x);
if ~y.equal[yb] then, pause, end

z = ifftnew(y,"*");
zb = ifftnew(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 5
n = 209;
m = 100;
x = rand(m,n);
y = fft2new(x);
z = ifft2new(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 20*%eps then pause,end 

// test 5bis
n = 209;
m = 100;
x = rand(m,n) + %i*rand(m,n);
y = fft2new(x);
z = ifft2new(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 


// test 6
if ~fft2new([]).equal[[]] then pause,end
if ~fft2new(zeros(0,4)).equal[zeros(0,4)] then pause,end
if ~fft2new(zeros(3,0)).equal[zeros(3,0)] then pause,end
if ~ifft2new([]).equal[[]] then pause,end
if ~ifft2new(zeros(0,4)).equal[zeros(0,4)] then pause,end
if ~ifft2new(zeros(3,0)).equal[zeros(3,0)] then pause,end


// test 7 (test hermitian redundancy for big vectors)
x = rand(1e5,1);
y = fftnew(x);
z = ifftnew(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(1e5,1) + %i*0;  // x should be complex
if isreal(x,%t) then, pause, end
y = fftnew(x);
z = ifftnew(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(1e6,1) + %i*0;  // x should be complex
if isreal(x,%t) then, pause, end
y = fftnew(x);
z = ifftnew(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(5e2,5e2);
y = fft2new(x);
z = ifft2new(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 

x = rand(5e2,5e2) + %i*0;  // x should be complex
if isreal(x,%t) then, pause, end
y = fft2new(x);
z = ifft2new(y);
// hermitian redundancy (backward fft is then real) 
// is detected only for the fftw interface
if %fftw then, if ~isreal(z) then, pause, end, end
e=max(abs(x-z));
if e > 100*%eps then pause,end 



// tests 8 (do ifft first)
n = 200;
x = rand(n,1);
y = ifftnew(x);
z = fftnew(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 180;
x = rand(n,1)+%i*rand(n,1);
y = ifftnew(x);
z = fftnew(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 190;
x = rand(1,n);
y = fftnew(x);
z = ifftnew(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 209;
m = 100;
x = rand(m,n);
y = ifftnew(x,1);
yb = ifftnew(x, dim=1);
if ~y.equal[yb] then, pause, end
z = fftnew(y,dim=1);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 209;
m = 110;
x = rand(m,n)+%i*rand(m,n);
y = ifftnew(x,1);
yb = ifftnew(x, dim=1);
if ~y.equal[yb] then, pause, end
yb = ifftnew(x, dim="r");
if ~y.equal[yb] then, pause, end
clear yb
z = fftnew(y,dim=1);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 209;
m = 100;
x = rand(m,n);
y = ifftnew(x,dim=2);
yb = ifftnew(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = fftnew(y, "c");
zb = fftnew(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 208;
m = 150;
x = rand(m,n)+%i*rand(m,n);
y = ifftnew(x,dim=2);
yb = ifftnew(x, "c");
if ~y.equal[yb] then, pause, end
clear yb
z = fftnew(y, "c");
zb = fftnew(y, dim=2);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 139;
m = 120;
x = rand(m,n);
y = ifftnew(x,dim="*");
yb = ifftnew(x,0);
if ~y.equal[yb] then, pause, end
yb = ifftnew(x);
if ~y.equal[yb] then, pause, end

z = fftnew(y,"*");
zb = fftnew(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 100*%eps then pause,end 

n = 126;
m = 200;
x = rand(m,n)+%i*rand(m,n);
y = ifftnew(x,dim="*");
yb = ifftnew(x,0);
if ~y.equal[yb] then, pause, end
yb = ifftnew(x);
if ~y.equal[yb] then, pause, end

z = fftnew(y,"*");
zb = fftnew(y, dim=0);
if ~zb.equal[z] then, pause, end
clear zb
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 200;
m = 120;
x = rand(m,n);
y = ifft2new(x);
z = fft2new(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 

n = 200;
m = 100;
x = rand(m,n) + %i*rand(m,n);
y = ifft2new(x);
z = fft2new(y);
e=max(abs(x-z));
if e > 20*%eps then pause,end 
