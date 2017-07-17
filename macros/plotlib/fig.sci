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

function fig(varargin,varargopt)
  function A=check_objs3d()
    A=get_current_axes_or_objs3d(create = %f);
    if type(A,'short')=='none' then
      // we have nothing we create a obj3d
      A=get_current_objs3d();
    elseif type(A,'short')=='axes' then
      // we have an axes, we try to replace it 
      // by a objs3d or we superpose if auto_clear is false 
      wrect=A.wrect;xsetech(wrect = wrect,a3d = %t);
      if length(A.children)==0 || F.gc.auto_clear then
        F.remove[A];
      end
    end
    A=get_current_objs3d();
  endfunction

  function A=check_axes()
    A=get_current_axes_or_objs3d(create = %f);
    if type(A,'short')=='none' then
      // we have nothing we create an axes
      A=get_current_axes();
    elseif type(A,'short')=='objs3d' then
      // we have an objs3d, we try to replace it 
      // by an axe or we superpose if auto_clear is false 
      wrect=A.wrect;xsetech(wrect = wrect,a3d = %f);
      if length(A.children)==0 || F.gc.auto_clear then
        F.remove[A];
      end
    end
    A=get_current_axes();
  endfunction

  function F=check_plotlib_figure()
    // check that current figure 
    // if in mtlb_mode 
    if isempty(winsid()) then
      F=get_current_figure();
      F.gc.set[auto_clear = %t];
      F.gc.set[mtlb_mode = 0];
    else
      F=get_current_figure();
      if F.gc.mtlb_mode==-1 then
        F.gc.set[auto_clear = %t];
        F.gc.set[mtlb_mode = 0];
      end
    end
  endfunction

  if varargopt.iskey['axes_init'] then
    value=varargopt.axes_init;
    varargopt.delete['axes_init'];
    if value.equal['2d'] then ret=check_axes();end
    if value.equal['3d'] then ret=check_objs3d();end
  end
  [ok,msg,varargin,H]=detect_options('fig',varargin,varargopt);
  if ~ok then error(msg);return;end

  select length(varargin)
    case 0 then
     // set the current figure (create if necessary) to be in matlab mode 
     // i.e it is in auto_clear mode 
     F=check_plotlib_figure();
    case 1 then
     if type(varargin(1),'short')=='m' then
       xset('window',varargin(1));
       check_plotlib_figure()
     end
  end

  // if H.iskey['background'] then state('background')=H.background;end 
  // if H.iskey['foreground'] then state('foreground')=H.foreground;end 
  // if H.iskey['frameColor'] then state('frameColor')=H.frameColor;end 
  // if H.iskey['margin']     then state('margin')=H.margin;end
  if H.iskey['colormap'] then xset('colormap',H.colormap);end
endfunction
