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
// Adapted from Stephane Mottelet scicoslab plotlib toolbox.
// Copyright (C) 2016-2017 - Stephane Mottelet, Jean-Philippe Chancelier

function bar(varargin,varargopt)
  // WIP style: 'histc' and 'hist'

  if length(varargin)==0 then
    y=[2,2,3;2,5,6;2,8,9;2,11,12];
    bar(y);
    return;
  end

  fig();
  fig(axes_init = '2d');

  barRelativeWidth=0.8;
  barStyle="group";
  faceColorIndex=2;

  H=hash(10);
  X=varargin(1)
  if type(X,'short') <> 'm' then
    error("bar: first argument should be a real matrix");
    return;
  end
  if or(size(X)==1) then X=X(:);end

  if length(varargin) >= 2 then
    Y=varargin(2);
    if type(Y,'short')=='m' then
      if or(size(Y)==1) then Y=Y(:);end
      if or(size(X)==1) then
        if or(size(Y)==1) then
          // two vectors of equal length ?
          if length(X) <> length(Y) then
            if length(Y)==1 then
              // Y must be a width
              barRelativeWidth=Y;
              Y=X;X=[];
            else
              error("bar: second argument should be of same length as"+" first or scalar");
              return;
            end
          end
        else
          // X vector and Y matrix 
          if length(X) <> size(Y,'r') then
            error("bar: lenght(X)<>size(Y,''r'')");return
          end
        end
      else
        if ~and(size(X)==size(Y)) then
          error("bar: X and Y should have the same size");
          return;
        end
      end
    else
      // X is in fact Y and Y is an option 
      if Y.equal['stacked'] then barStyle="stack";end
      if Y.equal['group'] then barStyle="group";end
      I=find(Y==['k','w','gray','b','g','c','r','m','y']);
      if ~isempty(I) then H.color=xget('lastpattern')+I;end;
      Y=X;X=[];
    end
  else
    // just one argument
    if or(size(X)==1) then X=X(:);end
    Y=X;X=[];
  end

  while length(varargin) >= 3 do
    arg=varargin(3);
    varargin(3)=null();
    if type(arg,'short')=='m' then
      else
      if arg.equal['stacked'] then barStyle="stack";end
      if arg.equal['group'] then barStyle="group";end
      I=find(arg==['k','w','gray','b','g','c','r','m','y']);
      if ~isempty(I) then H.color=xget('lastpattern')+I;end;
    end
  end

  if or(size(Y)==1) && ~H.iskey['color'] then H.color=xget('lastpattern')+4;end

  nc=size(Y,2);
  if isempty(X) then X=(1:size(Y,1))';end

  barDX=min(X(2:$)-X(1:$-1))*barRelativeWidth;
  _X=[];_Y=[];_F=[];

  if barStyle=="group" then
    // barStyle = "group"
    // if nc~=1 then  barDX=0.8*barDX; end
    XP=[-1,1,1,-1]'*barDX/2/nc*barRelativeWidth;
    for k=1:nc do
      col=xget('lastpattern')+3+k
      _X=[_X,XP(:,ones(length(X),1))+X(:,ones(4,1))'+barDX/nc*(k-.5-.5*nc)];
      _Y=[_Y,[0,0,1,1]' .*.Y(:,k)'];
      _F=[_F,col(ones(1,length(X)))];
    end
  else
    // barStyle="stack"
    for k=1:length(X) do
      XP=[-1,1,1,-1]'*barDX/2+X(k);
      for l=1:nc do
        col=xget('lastpattern')+3+l
        _X=[_X,XP];
        _Y=[_Y,([0,0,Y(k,l),Y(k,l)]+sum(Y(k,1:l-1)))'];
        _F=[_F,col];
      end
    end
  end
  plot2d([],[],rect = [min(_X),min(_Y),max(_X),max(_Y)]);
  if H.iskey['color'] then _F=H.color;end
  xfpolys(_X,_Y,color = 1,fill_color = _F);
endfunction
