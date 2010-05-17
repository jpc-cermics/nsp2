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

// logm 

tol=1.e4;
m=20

for j=1:m
  n=10;
  A=rand(n,n);
  A=A*A';
  B=logm(A);
  if ~isreal(B) then pause,end
  if norm(expm(B)-A) > tol*%eps  then pause,end
end

for j=1:m
  n=10;
  s=rand(1,n,'u');
  [Q,R]=qr(rand(n,n));
  A=Q*diag(s)*Q'
  B=logm(A);
  if ~isreal(B) then pause,end
  if norm(expm(B)-A) > tol*%eps  then pause,end
  if norm(B - Q*diag(log(s))*Q') > tol*%eps   then pause,end
end

for j=1:m
  n=10;
  A=rand(n,n)+%i*rand(n,n);
  A=A*A';
  B=logm(A);
  if norm(expm(B)-A) > 1.e-11 then pause,end
end

for j=1:m
  n=10;
  s=rand(1,n,'u')+%i*rand(1,n,'u');
  [Q,R]=qr(rand(n,n));
  A=Q*diag(s)*Q'
  B=logm(A);
  if norm(expm(B)-A) > tol*%eps  then pause,end
  if norm(B - Q*diag(log(s))*Q') > tol*%eps   then pause,end
end


for j=1:m
  n=4;
  U=rand(n,n);
  v=1:n;
  A=U*diag(v)*inv(U);
  B=logm(A);
  //if ~isreal(B) then pause,end
  if norm(expm(B)-A) > 1.e-4 then pause,end
  if norm(B- U*diag(log(v))*inv(U))  > 1.e-4 then pause,end
end

// sqrtm 
// -------

for j=1:m
  n=10;
  A=rand(n,n);
  A=A*A';
  B=sqrtm(A);
  if ~isreal(B) then pause,end
  if norm(B*B -A) > tol*%eps then pause,end
end

for j=1:m
  n=10;
  s=rand(1,n,'u');
  [Q,R]=qr(rand(n,n));
  A=Q*diag(s)*Q'
  B=sqrtm(A);
  if ~isreal(B) then pause,end
  if norm(B*B -A) > tol*%eps  then pause,end
  if norm(B - Q*diag(sqrt(s))*Q') > tol*%eps   then pause,end
end

for j=1:m
  n=10;
  A=rand(n,n)+%i*rand(n,n);
  A=A*A';
  B=sqrtm(A);
  if norm(B*B -A) > tol*%eps then pause,end
end

for j=1:m
  n=10;
  s=rand(1,n,'u')+%i*rand(1,n,'u');
  [Q,R]=qr(rand(n,n));
  A=Q*diag(s)*Q'
  B=sqrtm(A);
  if norm(B*B-A) > tol*%eps  then pause,end
  if norm(B - Q*diag(sqrt(s))*Q') > tol*%eps   then pause,end
end

for j=1:m
  n=4;
  U=rand(n,n);
  v=1:n
  A=U*diag(v)*inv(U);
  B=sqrtm(A);
  //if ~isreal(B) then pause,end
  if norm(B*B -A) > 1.e-5 then pause,end
  if norm(B - U*diag(sqrt(v))*inv(U))  > 1.e-5 then pause,end
end



