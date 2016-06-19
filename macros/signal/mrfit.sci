function [num,den]=mrfit(w,mod,r)
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
  
//Calling sequence:
//[num,den]=mrfit(w,mod,r)
//sys = mrfit(w,mod,r)
//
//w: vector of frequencies 
//mod: vector of frequency response magnitude at these frequencies.   
//weight: vector of weights given to each point
//
//Fits frequency response magnitude data points by a bi-stable transfer 
//function
//G(s) = num(s)/den(s) of order r.
//
//  abs(freq(num,den,%i*w)) should be close to mod
//
// 

  function y=log10(x); y = log(x)./log(10);endfunction;
  w=w(:);mod=mod(:);

  if w(1)==0 then w(1)=%eps;end

  if r==0 then num=sum(mod)/size(mod,'*');den=1;return;end

  sl0=round(log10(mod(2)/mod(1))/log10(w(2)/w(1)));w(1)=w(2)/10; 
  if sl0~=0 then mod(1)=mod(2)/10^sl0;end

  n=length(w);
  slinf=round(log10(mod($)/mod($-1))/log10(w($)/w($-1)));
  w($)=w($-1)*10; 
  if slinf~=0 then mod($)=mod($-1)*10^slinf;end
  logw=log10(w);logmod=log10(mod);delw=diff(logw);delmod=diff(logmod); 

  weight=ones(length(w),1);

  junk=find(abs(diff(delmod./delw)) > .6);
  ind=1+junk;
  if isempty(junk) then ind=[];end
  weight(ind)=10*ones(length(ind),1); 

  lwnew=[]; modnew=[];
  for i=1:length(w)-1,
    nadd=floor(delw(i)/.3)-1;
    if nadd>0 then
      slope=delmod(i)/delw(i);
      lwnew=[lwnew logw(i)+.3*(1:nadd)];
      modnew=[modnew logmod(i)+.3*slope*(1:nadd)];
      weight=[weight ; ones(nadd,1)];
    end
  end
  log10is=log(10);
  w=[w ; exp(lwnew'*log10is)];
  mod=[mod ; exp(modnew'*log10is)];

  [w,ind]=sort(-w);w=-w; mod=mod(ind); weight=weight(ind);
  ind=find(diff(w)>0); ind=[ind(:);length(w)];
  w=w(ind); mod=mod(ind); weight=weight(ind);

  fresp=cepstrum(w,mod);

  [num,den]=frfit(w,fresp,r,weight);
  if nargout==1 then
    num=p2r(num,den);// syslin('c',num/den);
  end

endfunction
function ww=diff(ww)
//Utility fct
  p=size(ww(:),'*');
  ww=ww(2:p)-ww(1:p-1);
endfunction
