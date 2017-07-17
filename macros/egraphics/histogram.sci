function  [x,h]=histogram(a,l,u,n)
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
  // Adapted from Enrico Segre scicoslab toolbox.
  // Copyright (C) 1998-2017 - Enrico Segre

  // manca....
  // sintassi: histogram(a,l,u,n)  histogram(a,l,u)  histogram(a,n)
  //
  if nargin==0 then
    printf('[x,h]=histogram(rand(1,10000));\n')
    printf('plot(x,h)\n')
    [x,h]=histogram(rand(1,10000));
    xbasc();plot(x,h) 
    x=[];h=x;
    return
  end
  mina=min(a); maxa=max(a);
  if nargin==3 then n=50; end
  if nargin==2 then n=l; l=min(a),u=max(a); end
  if nargin==1 then l=mina; u=maxa; n=50; end

  h=zeros(n,1); hh=zeros(n+1,1);

  x=l+((1:n)'-0.5)*(u-l)/n;
  dx=(u-l)/n;
  xl=l+(0:n)*dx; xu=xl+dx;

  for i=1:n+1;
    // this is faster
    if xl(i)>=mina then a=a(a>=xl(i)); end
    hh(i)=length(a)
  end  
  h=hh(1:$-1)-hh(2:$);

  if nargout==1 then x=h; end         // comodita' per calls veloci
endfunction



