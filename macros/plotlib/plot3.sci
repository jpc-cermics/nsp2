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

function plot3(varargin,varargopt)
  function [ok,msg,H]=getColorNumber(ch,count)
    // decode the string ch to obtain 
    // - line_color: color to be used (-1: default color, -2: no line).
    // - line_thickness: the thickness to be used
    // - mark: mark to be used along the curve (-1: default mark, -2: no mark).
    // - mark_color: mark color to be used (-1: default color).
    // - mark_size: size to be used for the mark (-1: default size).
    ok=%t;msg="";
    H=hash(10);// line_color=-2, mark=-2
    c=-1;m=[];force_line=[];ms=[];
    // Line type modifiers
    if strindex(ch,'--') then
      ch=strsubst(ch,'--','');
      force_line=2;
    elseif strindex(ch,'-.') then
      ch=strsubst(ch,'-.','');
      force_line=4;
    elseif strindex(ch,':') then
      ch=strsubst(ch,':','');
      m=0;
    elseif strindex(ch,'-') then
      ch=strsubst(ch,'-','');
      force_line=1;
    end
    // a set of default colors are always defined 
    for i=1:length(ch) do
      ci=part(ch,i);
      I=find(ci==['k','w','gray','b','g','c','r','m','y']);
      if ~isempty(I) then c=xget('lastpattern')+I;continue;end;
      I=find(ci==['.','+','x','*','^','v','t','d','f','o']);
      if ~isempty(I) then m=I-1;continue;end;
      I=find(ci==string(1:9));
      if ~isempty(I) then ms=I;continue;end;
      ok=%f;
      msg=sprintf("Error: character ''%s'' not recognized",ci);
      return;
    end
    // first marks 
    if ~isempty(m) then H.mark=m;H.mark_color=c;end
    if ~isempty(ms) then H.mark_size=ms;end
    // do we have a line 
    if isempty(m) || ~isempty(force_line) then
      if c==-1 then H.line_color=count;else H.line_color=c;end
      if isempty(m) && ~isempty(ms) then H.line_thickness=ms;end
    else
      H.line_color=-2;// no line 
    end
  endfunction

  if length(varargin)==0 then
    S=["t=linspace(0,50*%pi,2000)'';";"x=t.*sin(t)/50;";"y=sin(t).*cos(t);";
       "z= t.^2 ./(50*%pi);";
       "plot3(x,3*y,z/max(z),''r3'',View=[45,60],Axis={[-3,3,-3,3,-3,3],''off''});"];
    printf("%s\n",S);
    execstr(S);
    return
  end

  fig();
  F=get_current_figure();
  fig(axes_init = '3d');
  A=get_current_objs3d();
  if F.gc.auto_clear then A.children=list();end

  [ok,msg,varargin,H]=detect_options('plot3',varargin,varargopt);
  if ~ok then error(msg);return;end

  // Z est a priori une matrice 
  // on peut accepter une fonction 
  // C est a priori une matrice || mxnx3 matrice 
  // where Z is m-by-n.

  options=hash(10);
  if H.iskey['axesflag'] && H.axesflag==0 then options.box_style="none";end
  if H.iskey['colormap'] then options.colormap=H.colormap;end
  if H.iskey['shade'] then options.shade=H.shade;end
  if H.iskey['azimuth'] then options.theta=H.azimuth;end
  if H.iskey['elevation'] then options.alpha=90-H.elevation;end
  if H.iskey['iso'] then options.iso=H.iso;end
  if H.iskey['lightVect'] then lightVect=H.lightVect;else lightVect=[1;1;1];end
  if H.iskey['rect'] && length(H.rect)==6 then option.ebox=H.rect;end
  auto_clear=F.gc.auto_clear
  F.gc.set[auto_clear = %f];

  count=0;
  while %t do
    //printf("number of arguments %d\n",length(varargin));
    if length(varargin)==0 then break;end
    if length(varargin) >= 4 then
      X=varargin(1);Y=varargin(2);Z=varargin(3);
      if type(varargin(4),'short')=='s' then
        color=varargin(4);
        [ok,msg,H]=getColorNumber(color,count)
        if ~ok then F.gc.set[auto_clear = auto_clear];error(msg);return;end;
        if H.iskey['mark'] then options.mark=H.mark;end
        if H.iskey['line_color'] then options.style=H.line_color;end
        if H.iskey['mark_color'] then options.mark_color=H.mark_color;end
        if H.iskey['mark_size'] then options.mark_size=H.mark_size;end
        if H.iskey['line_thickness'] then options.line_thickness=H.line_thickness;end
        varargin=varargin.sublist[5:length(varargin)];
      else
        varargin=varargin.sublist[4:length(varargin)];
      end
    elseif length(varargin) >= 3 then
      X=varargin(1);Y=varargin(2);Z=varargin(3);
      varargin=varargin.sublist[4:length(varargin)];
    else
      F.gc.set[auto_clear = auto_clear]
      error("plot3: wron number of arguments\n");
      return;
    end
    if size(X,1)==1 then X=X(:);end
    if size(Y,1)==1 then Y=Y(:);end
    if size(Z,1)==1 then Z=Z(:);end
    if options.iskey['style'] then options.style=options.style*ones(1,size(Z,2));end
    param3d(X,Y,Z,options(:));
    count=count+1;
  end
  F.gc.set[auto_clear = auto_clear];
endfunction
