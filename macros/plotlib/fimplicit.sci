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
// Copyright (C) 2016-2017 - Jean-Philippe Chancelier

function fimplicit(f,varargin,varargopt)
  if nargin <= 0 then
    S=["function z=f(x,y) z=x.^2 - y.^2 - 1;endfunction";"fimplicit(f);"];
    printf("%s\n",S);
    execstr(S);
    return
  end

  if length(varargin) > 0 && type(varargin(1),'short')=='m' then
    rect=varargin(1);
    varargin.remove_first[];
  else
    rect=[-5,5,-5,5];
  end

  if varargopt.iskey['args'] then
    fargs=varargopt.args;
    varargopt.delete['args']
    use_args=%t;
  else
    use_args=%f;
  end

  [ok,msg,varargin,H]=detect_options('fimplicit',varargin,varargopt);
  if ~ok then error(msg);return;end

  if H.iskey['rect'] then rect=H.rect;end

  npts=100;
  xg=linspace(rect(1),rect(2),npts);
  yg=linspace(rect(3),rect(4),npts);
  if use_args then
    z=feval(xg,yg,f,args = fargs);
  else
    z=feval(xg,yg,f);
  end
  fig();
  F=get_current_figure();
  fig(axes_init = '2d');
  auto_clear=F.gc.auto_clear;
  axes=get_current_axes();

  
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

  plot_args.logflag=part(modeScale,2:3);
  if H.iskey['Legend'] then
    legends=H.Legend
    if size(legends,'*') >= 1 then
      plot_args.leg=legends{1}
    end
  end
  if H.iskey['Location'] then plot_args.leg_pos=H.Location;end
  if H.iskey['rect'] then plot_args.rect=H.rect;end
  if H.iskey['iso'] then plot_args.iso=H.iso;end
  if H.iskey['axesflag'] then plot_args.axesflag=H.axesflag;end
  if H.iskey['manual'] then A=get_current_axes();plot_args.rect=A.frect;end

  if H.gridFlag then xgrid();end
  if H.iskey['Xlabel'] && H.Xlabel <> "" then xlabel(H.Xlabel);end
  if H.iskey['Ylabel'] && H.Ylabel <> "" then xlabel(H.Ylabel);end
  if H.iskey['Title'] && H.Title <> "" then title(H.Title);end

  xset('fpf'," ");
  contour2d(xg,yg,z,[0,0]);
endfunction
