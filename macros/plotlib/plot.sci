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

function plot(varargin,varargopt)
  // emulates the Matlab plot function 
  if length(varargin)==0 then
    S=["t=linspace(0,2*%pi,64);";"subplot(2,1,1);";
       "plot(t,cos(t),title=''plot(t,cos(t))'');";"subplot(2,1,2);";
       "plot(t,cos(t),t,cos(2*t),''og'',title=''plot(t,cos(t),t,cos(t),''''og'''')'');"];
    printf("%s\n",S);
    execstr(S);
    return
  end
  main_plot('plot',varargin,varargopt);
endfunction

function semilogy(varargin,varargopt)
  if length(varargin)==0 then
    S=["w=logspace(-2,2,200)";"s=%i*w";"g=1../(s.^2+0.01*s+1)";"semilogy(w,abs(g));"];
    printf("%s\n",S);
    execstr(S);
    return
  end
  if ~varargopt.iskey['grid'] then varargopt.grid='on';end
  varargopt.Yscale='log';
  main_plot('semilogy',varargin,varargopt);
endfunction

function semilogx(varargin,varargopt)
  if length(varargin)==0 then
    S=["w=logspace(-2,2,200);";"s=%i*w;";"g=1../(s.^2+0.01*s+1);";"semilogx(w,abs(g));"]
    printf("%s\n",S);
    execstr(S);
    return
  end
  varargopt.Xscale='log';
  if ~varargopt.iskey['grid'] then varargopt.grid='on';end
  main_plot('semilogx',varargin,varargopt);
endfunction

function loglog(varargin,varargopt)
  if length(varargin)==0 then
    S=["w=logspace(-2,2,200);";"s=%i*w;";"g=1../(s.^2+0.01*s+1);";"loglog(w,abs(g));"];
    printf("%s\n",S);
    execstr(S);
    return
  end
  varargopt.Xscale='log';
  varargopt.Yscale='log';
  if ~varargopt.iskey['grid'] then varargopt.grid='on';end
  main_plot('semilogx',varargin,varargopt);
endfunction
