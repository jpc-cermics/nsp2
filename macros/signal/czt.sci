function [czx]=czt(x,m,w,phi,a,theta)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks (INRIA)
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
  
//czx=czt(x,m,w,phi,a,theta)
//chirp z-transform algorithm which calcultes the
//z-transform on a spiral in the z-plane at the points
//[a*exp(j*theta)][w**kexp(j*k*phi)] for k=0,1,...,m-1.
//  x     :Input data sequence
//  m     :czt is evaluated at m points in z-plane
//  w     :Magnitude multiplier
//  phi   :Phase increment
//  a     :Initial magnitude
//  theta :Initial phase
//  czx   :Chirp z-transform output
// Author: C. Bunks  date: 10 July 1988
// 

//get the size of x and find the maxmum of (n,m)
  
  n=max(size(x));
  nm=max([n,m]);
  
  //create sequence h(n)=[w*exp(-j*phi)]**(-n*n/2)
  
  w=w*exp(-%i*phi);
  idx=(-nm+1:0);
  idx=-idx.*idx/2;
  h=exp(idx*log(w));
  h(nm+1:2*nm-1)=h(nm-1:-1:1);
  
  //create g(n)
  
  a=a*exp(%i*theta);
  idx=(0:n-1);
  g=exp(-idx*log(a))./h(nm:nm+n-1);
  g=x.*g;
  
  //convolve h(n) and g(n)
  
  hc=h(nm:nm+m-1);
  hc(m+1:m+n-1)=h(nm-n+1:nm-1);
  gc=0*ones_deprecated(hc);
  gc(1:n)=g;
  hf=fft(hc);
  gf=fft(gc);
  hcg=ifft(hf.*gf);
  
  //preserve m points and divide by h(n)
  
  czx=hcg(1:m)./h(nm:nm+m-1);
endfunction
