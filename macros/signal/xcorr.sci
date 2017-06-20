function [c,lagindex]=xcorr(x,varargin,maxlags=[],mode="none")
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2012-2016 - S. Steer (INRIA)
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
  
  modes=["biased","unbiased","coeff","none"]
  if isempty(mode==modes) then 
    error(sprintf("Error: unrecognized mode ""%s""\n",mode));
    return;
  end
  //test de validité de x
  szx=size(x)
  if type(x,'short')<>'m' then
    error(sprintf("Error: first argument should be a real vector\n"));
  end
  autocorr=%t
  if length(varargin)==0 then
    autocorr=%t
    if type(maxlags,'short')<>'m' || size(maxlags,"*") > 1 || ~isreal(maxlags) || ...
	  maxlags<>int(maxlags) || maxlags < 1 then
      error(sprintf("Error: maxlags should be a positive integer\n"));
      return;
    end
  else
    autocorr=%f
    y=varargin(1)
    if type(y,'short')<>'m' then 
      error(sprintf("Error: second argument should be a real vector\n"));
    end
  end
  
  if autocorr then
    x=matrix(x,-1,1);n=size(x,"*")
    if isempty(maxlags) then maxlags=n-1,end
    t=fft(x);
    c=ifft(real(t.*conj(t)))
    if isreal(x) then c=real(c);end
  else 
    x=matrix(x,-1,1);nx=size(x,1)
    xx=sum(abs(x).^2)
    y=matrix(y,-1,1);ny=size(y,1)
    yy=sum(abs(y).^2)
    if nx<ny then
      x(ny)=0;
    elseif ny<nx then
      y(nx)=0;
    end
    n=max(nx,ny)
    if isempty(maxlags) then maxlags=n-1,end
    c=ifft(fft(x).*conj(fft(y)))
    if isreal(x)&isreal(y) then c=real(c),end
  end
  //extract requested lags
  padding=zeros(maxlags-n+1,1)
  if maxlags < n then
    c=[c($-maxlags+1:$);c(1:maxlags+1)];
  else
    padding=zeros(maxlags-n+1,1)
    c = [padding;
	 c($-n+2:$);
	 c(1:n);
	 padding];
  end
  //normalization
  select mode
   case "biased" then
    c=c/n;
   case "unbiased" then
    scale=n-abs(-maxlags:maxlags)
    scale(scale==0)=1;
    c=c./scale'
   case "coeff" then
    if autocorr then
      c=c/c(maxlags+1)
    else
      c=c/sqrt(xx*yy)
    end
  end
  // give result same orientation as x
  if szx(1)==1 then c=matrix(c,1,-1),end
  if nargout==2 then lagindex=-maxlags:maxlags,end
endfunction
