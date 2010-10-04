
function res=compose_fft_p(p,q)
// Copyright  2010 Jean-Philippe Chancelier Cermics/Enpc 
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
// computes R=P(Q) using fft method 
// in the same spirit as the det function 
//
  dr = p.degree[]*q.degree[];
  k = dr+1; // no need to use a power of 2 for 
  // fftw3.
  e=0*ones(k,1);
  e(2)=1;
  if k==1 then
    ksi=1;
  else
    ksi=fft(e);
  end
  fi=[];
  flag = isreal(p,%t) && isreal(q,%t)
  C=horner(q,ksi);
  D=horner(p,C{1});
  F= ifft(D{1});
  // Paramètres de clean
  epsa=1.d-10;
  epsr=0;
  if ~flag then
    // Cas Complexe
    Temp1 = m2p(clean(real(F),epsa,epsr))+%i*m2p(clean(imag(F),epsa,epsr));
  else
    // Cas Réel
    Temp1 = m2p(clean(real(F),epsa,epsr));
  end
  // k is bigger than dr we set to zero the coefficients
  // with degree above maj. 
  Temp2 = Temp1.coeffs{1};
  Temp2(dr+2:$)=[];
  res = m2p(Temp2);
endfunction

function res=compose_p(p,q)
// Copyright  2010 Jean-Philippe Chancelier Cermics/Enpc 
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
// computes R=P(Q) by standard way.
// 
  pc = p.coeffs{1};
  res = pc(1)*m2p([1]);
  for i=2:size(pc,'*') 
    res = res + pc(i)*q^(i-1);
  end
endfunction

if %f then 
  p=m2p(1:4);
  q=m2p(1:3);
  compose(p,q);
end

  
  
  
  
