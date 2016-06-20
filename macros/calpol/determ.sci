function res=determ(W,k)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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

// determinant of a polynomial or rational matrix by FFT
// W=square polynomial matrix
// k=``predicted'' degree of the determinant of W i.e. k is
// an integer larger or equal to the actual degree of W.
// Method: evaluate the determinant of W for the Fourier frequencies
// and apply inverse fft to the coefficients of the determinant.
// See also detr
// F.D.!
// 
  
  if isempty(W) then  res=1; return; end;
  
  if size(W,1)<>size(W,2) then 
    error('argument must be a square matrix'), 
  end
  
  n1=size(W,1)
  // small cases
  if n1==1 then
    res=W;
    return;
  elseif n1==2 then
    res = W(1,1)*W(2,2) - W(1,2)*W(2,1);
    return;
  end
  //upper bound of the determinant degree
  
  maj = n1*max(degree(W))+1;
  
  if argn(2)==1 then 
    k=1;
    while k < maj,
      k=2*k;
    end
  end
  
  if n1>8 then
    write(%io(2),'Computing determinant: Be patient...');
  end
  
  // Default Values
  e=0*ones(k,1);
  e(2)=1;
  
  // Paramètres de clean
  epsa=1.d-10;
  epsr=0;//no relative rounding
  
  if k==1 then
    ksi=1;
  else
    ksi=fft(e,-1);
  end
  
  fi=[];
  
  if ~isreal(W,0) then
    // Cas Complexe
    for kk=1:k,
      fi=[fi,det(horner(W,ksi(kk)))];
    end
    Temp0 = poly(fft(fi,1),W.get_var[],'c');
    Temp1 = clean(real(Temp0),epsa,epsr)+%i*clean(imag(Temp0),epsa,epsr);
    
  else
    // Cas Réel
    for kk=1:k,fi=[fi,det(freq(W,ones(W),ksi(kk)))];end
    Temp1 = clean(real(poly(fft(fi,1),W.get_var[],'c')),epsa,epsr);
  end
  
  if argn(2)==1 then
    
    // Cas où k est défini dans les paramètres d'entrée.
    // On va maintenant annuler tous les coefficients
    // dont le degré est supérieur à maj
    
    Temp2 = coeff(Temp1);
    for i=1:maj,
      Temp2(i) = 0;
    end
    res = Temp1 - poly(Temp2,W.get_var[],"coeff");
    return;
    
  else
    // Cas où k n'est pas défini dans les paramètres d'entrée
    res = Temp1;
    return;
  end
  
endfunction
