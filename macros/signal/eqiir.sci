function [cells,fact,zzeros,zpoles]=eqiir(ftype,approx,om,deltap,deltas)
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
  
//[cells,fact,zzeros,zpoles]=eqiir(ftype,approx,om,deltap,deltas)
//Design of iir filter :interface with eqiir (syredi)
// ftype  :filter type ('lp','hp','sb','bp')
// approx :design approximation ('butt','cheb1','cheb2','ellip')
// om     :4-vector of cutoff frequencies (in radians)
//        :      om=<om1,om2,om3,om4>
//        :      0 <= om1 <= om2 <= om3 <= om4 <= pi
//        :When ftype=3 or 4, om3 and om4 are not used
//        :and may be set to 0.
// deltap :ripple in the passband.  0<= deltap <=1.
// deltas :ripple in the stopband.  0<= deltas <=1.
//Outputs :
// cells  :realization of the filter as second order cells
// fact   :normalization constant
// zzeros :zeros in the z-domain
// zpoles :poles in the z-domain
//The filter obtained is h(z)=fact*product of the elements of
//cells.  That is
//
//     hz=fact*prod(cells(2))./prod(cells(3))
//
// 
  select part(approx,1);
   case 'b'
    iapro=1
   case 'e'
    iapro=4
   case 'c'
    last=part(approx,length(approx));
    if last=='1' then iapro=2,end
    if last=='2' then iapro=3,end
  else write(%io(2),'iir : unknown filter approximation');
    return;
  end
  select ftype;
   case 'lp'
    ityp=1
   case 'hp'
    ityp=2
   case 'bp'
    ityp=3
   case 'sb'
    ityp=4
  else write(%io(2),'iir : wrong first input parameter');
    return;
  end
  if max(size(om))==2 then
    om=matrix([matrix(om,1,2),0,0],1,4),
  end
  [fact,b2,b1,b0,c1,c0,zzeros,zpoles]= syredi(ityp,iapro,om,deltap,deltas);
  nb=max(size(b0));
  coeffs=[b0;b1;b2;c0;c1];
  coeffs=coeffs(:,1:nb);
  coeffs=[coeffs;ones(1,nb)];
  cells=[];
  for col=coeffs,
    nf=col(1:3);nd=col(4:6);
    [n1,d1]=simp(poly(nf,'z','c'),poly(nd,'z','c'));
    cells=[cells,syslin([],n1,d1)];
  end
  //crapaud...
  if iapro==1| iapro==2  then
    zzeros=[];
    [k,j]=size(cells);
    w=cells.num;
    // for k= not implemented for polynomials or rationals 
    for k=1:size(w,'*');
      zzeros=[zzeros,roots(w(k))'];
    end
  end
endfunction
