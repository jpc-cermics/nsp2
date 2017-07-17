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

function main_plot(typeOfPlot,argList,argOpt)
  // internal function for Matlab plot emulation 

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

  function [ok,msg,nArgOut,accept_vector]=check2dFun(funName,func,X)
    // checks that the function func signature is 
    //   [y]=func(x) or [x,y]=func(t).
    // then check if the function accepts vectors as input 
    ok=%t;msg="";nArgOut=1;accept_vector=%f;
    [nArgOut,nArgIn]=inout(func);
    if nArgIn <> 1 then
      ok=%f;
      msg=sprintf('%s: functional argument must accept one input arguments',funName);
      return;
    end
    if nArgOut <> 1 && nArgOut <> 2 then
      ok=%f;msg=sprintf('%s: functional argument must return 1 or 2 values',funName);
      return;
    end
    // Now we test using inputs provided by the user 
    // if the function works with vector input 
    select nArgOut
      case 1 then
       call_ok=execstr('yf=func(X)',errcatch = %t);
       if call_ok then
         accept_vector=and(size(yf)==size(X));
       else
         lasterror();end
      case 2 then
       call_ok=execstr('[xf,yf]=func(X)',errcatch = %t);
       if call_ok then
         accept_vector=and([size(xf)==size(yf),size(yf)==size(X)]);
       else
         lasterror();end
    end
  endfunction

  function [ok,msg,X,Y]=checkXYPair(typeOfPlot,x,y,argNumber)
    ok=%t;msg="";X=[];Y=[];
    if type(y,'short')=='pl' then
      f=y;
      if and(size(x) <> 1) then
        ok=%f;msg=sprintf('%s: x must be a vector when y is a function\n',typeOfPlot);
        return;
      end
      T=x(:);// to ensure that t is a column vector
      [ok,msg,nArgOut,vectInput]=check2dFun(typeOfPlot,f,T);
      if ~ok then return;end
      select nArgOut
        case 1 then
         X=T;
         if vectInput then Y=f(T);else Y=feval(T,f);end
        case 2 then
         if vectInput then
           [X,Y]=f(T);
         else
           X=zeros(size(T));Y=X;
           for i=1:length(T) do [xt,yt]=f(t(i));X(i)=xt;Y(i)=yt;end
         end
      end
    else
      // y is not a function 
      X=x;
      Y=y;
      if size(X,1)==1 then X=X',end
      if size(Y,1)==1 then Y=Y',end
      if and(size(X)==size(Y)) then return;end
      if (size(X,2)==1) && (size(Y,1)==size(X,1)) then return;end
      if (size(X,2)==1) && (size(Y,2)==size(X,1)) then Y=Y';return;end
      msg=sprintf('%s: first and second arguments incompatible sizes',typeOfPlot,argNumber, ...
                  argNumber+1);
      ok=%f;
    end
  endfunction

  fig();
  F=get_current_figure();
  fig(axes_init = '2d');
  auto_clear=F.gc.auto_clear;
  axes=get_current_axes();

  if ~auto_clear then
    // count already present curves
    count=0;
    if type(axes,'short') <> 'none' then
      // count the curves 
      for i=1:size(axes.children) do
        if type(axes.children(i),'short')=='curve' then count=count+1;end
      end
    end
  else
    // make the auto_clear here because automatic 
    // auto clear is disabled below since we may draw several curves
    count=0
    axes.fixed=%f
    axes.children=list();
  end

  [ok,msg,argList,H]=detect_options(typeOfPlot,argList,argOpt);
  if ~ok then error(msg);return;end

  liste=list();

  while length(argList) do
    // printf("In the while %d\n",length(argList));
    if type(argList(1),'short')=='m' then
      if isempty(argList(1)) then return;end
      if length(argList)==1 then
        if or(size(argList(1))==1) then
          argList(0)=1:length(argList(1));// insert an abcsissa vector of same length,
        else
          // if this is a matrix,
          argList(0)=1:size(argList(1),1);// insert an abcsissa vector with
        end;// length = number of lines of 1st argument
      else
        if (type(argList(2),'short')=='s') then
          if or(size(argList(1))==1) then
            argList(0)=1:length(argList(1));
          else
            argList(0)=1:size(argList(1),1);
          end
        end
      end
      if (type(argList(2),'short')=='m' || type(argList(2),'short')=='pl') then
        [ok,msg,X,Y]=checkXYPair(typeOfPlot,argList(1),argList(2));
        if ~ok then error(msg);return;end
        argList(1)=null();// Deletion of the two top arguments
        argList(1)=null();// in the argument list
        Hc=hash(10);
        if length(argList) <> 0 && type(argList(1),'short')=='s' then
          [ok,msg,Hc]=getColorNumber(argList(1));
          if ~ok then error(msg);return;end
          argList(1)=null();// in the argument list
        else
          colors=['b','g','c','r','m','y','k'];
          color=colors(min(count+1,7));count=count+1;
          [ok,msg,Hc]=getColorNumber(color);
        end
        if size(X,2)==1 then
          for j=1:size(Y,2) do
            liste($+1)=list(X,Y(:,j),Hc);
          end
        else
          // Abscissa and ordina are both matrices
          for j=1:size(X,2) do
            liste($+1)=list(X(:,j),Y(:,j),Hc);
          end
        end
      end
    end;// while length(argList)
  end

  //if ~H.iskey['foreground'] then H.foreground = state('foreground');end
  //if ~H.iskey['background'] then H.background = state('background');end
  if ~H.iskey['gridFlag'] then H.gridFlag=%f;end
  if ~H.iskey['Xscale'] then H.Xscale="linear";end
  if ~H.iskey['Yscale'] then H.Yscale="linear";end

  // use log or not 
  modeScale='g';
  if H.Xscale=='log' then
    modeScale=modeScale+'l';
  else
    modeScale=modeScale+'n';
  end
  if H.Yscale=='log' then
    modeScale=modeScale+'l';
  else
    modeScale=modeScale+'n';
  end

  // remove auto_clear during the loop 
  auto_clear=F.gc.auto_clear;
  F.gc.set[auto_clear = %f];

  for k=1:length(liste) do
    [X,Y,plot_args]=liste(k)(:);
    plot_args.logflag=part(modeScale,2:3);
    if H.iskey['Legend'] then
      legends=H.Legend
      if size(legends,'*') >= k then
        plot_args.leg=legends{k}
      end
    end
    if H.iskey['Location'] then plot_args.leg_pos=H.Location;end
    if H.iskey['rect'] then plot_args.rect=H.rect;end
    if H.iskey['iso'] then plot_args.iso=H.iso;end
    if H.iskey['axesflag'] then plot_args.axesflag=H.axesflag;end
    if H.iskey['manual'] then A=get_current_axes();plot_args.rect=A.frect;end
    plot2d1(X,Y,plot_args(:));
  end
  F.gc.set[auto_clear = auto_clear];

  if H.gridFlag then
    // gc=addcolor(gridColor);
    xgrid();
  end
  if H.iskey['Xlabel'] && H.Xlabel <> "" then xlabel(H.Xlabel);end
  if H.iskey['Ylabel'] && H.Ylabel <> "" then xlabel(H.Ylabel);end
  if H.iskey['Title'] && H.Title <> "" then title(H.Title);end
endfunction
