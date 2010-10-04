function res=det_p(P,k)
// Copyright  2010 Francois Delebecque Inria.
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
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// determinant of a polynomial matrix using fft evaluations.
// P should be a quare polynomial matrix.
// k=if given, is an optional predicted degree 
// of the determinant of P i.e. k is an integer larger 
// or equal to the actual degree of det(P).
// 
// Method: evaluate the determinant of P for the Fourier frequencies
// and apply inverse fft to the coefficients of the determinant to get 
// back the polynomial.
// 
// adapted to nsp (jpc 2010).
// 
  if isempty(P) then
    res=[]; 
    return;
  end;
  if size(P,1)<>size(P,2) then 
    error('Error: argument must be a square matrix');
    return;
  end
  n1=size(P,1);
  // small cases
  if n1==1 then
    res=P;
    return;
  elseif n1==2 then
    res = P(1,1)*P(2,2) - P(1,2)*P(2,1);
    return;
  end
  //upper bound of the determinant degree
  maj = n1*max(P.degree[])+1;
  if nargin==1 then 
    // k= ceil(log(maj)/log(2));  k= 2^k;
    k = maj; // with fftw3 we do not need a power of 2 
  end
  e=zeros(k,1);
  e(2)=1;
  // parameters for clean.
  epsa=1.d-10;
  epsr=0;//no relative rounding
  if k==1 then
    ksi=1;
  else
    ksi=fft(e);
  end
  fi=[];
  if ~isreal(P,%t) then
    // Cas Complexe
    C=horner(P,ksi,vdim=%t);
    for kk=1:k,  fi=[fi,det(C{kk})]; end
    F= ifft(fi);
    Temp1 = m2p(clean(real(F),epsa,epsr))+%i*m2p(clean(imag(F),epsa,epsr));
  else
    // Cas Réel
    C=horner(P,ksi,vdim=%t);
    //for kk=1:k,fi=[fi,det(freq(P,ones(P),ksi(kk)))];end
    for kk=1:k,fi=[fi,det(C{kk})];end 
    Temp1 = m2p(clean(real(ifft(fi)),epsa,epsr));
  end
  if nargin==1 then
    // k was estimated. we set to zero the coefficients
    // with degree above maj. 
    Temp2 = Temp1.coeffs{1};
    Temp2(maj+1:$)=[];
    res = m2p(Temp2);
  else
    // k is given 
    res = Temp1;
  end
endfunction

if %f then 
  n=3;
  x=m2p([0,1]);
  P=rand(n,n)*m2p([1])+ rand(n,n)*x+rand(n,n)*x*x;
  Q=det(P);
  H=horner(Q,linspace(-1,1,30));
  H2=horner(P,linspace(-1,1,30),vdim=%t);
  d=[];for k=1:size(H2,'*'), d(k)=det(H2{k});end
  if norm(d-H{1}) > 100*%eps then pause;end 
end

  
  
  
  
