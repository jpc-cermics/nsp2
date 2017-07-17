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

function legend(varargin,varargopt)
  // 
  if isempty(winsid()) then return;end
  A=get_current_axes(create = %f);
  if type(A,'short')=='none' then return;end
  legends=m2s([]);
  for i=1:length(varargin) do
    if type(varargin(i),'short')=='ce' then
      for j=1:size(varargin(i),'*') do
        legends($+1)=varargin(i){j};
      end
    elseif type(varargin(i),'short')=='s' then
      legends($+1)=varargin(i);
    else
      error("legend: arguments should be string or cell of strings");
      return
    end
  end
  count=1;
  for i=1:length(A.children) do
    if count > size(legends,'*') then break;end
    if type(A.children(i),'short')=='curve' then
      A.children(i).legend=legends(count);
      count=count+1;
      A.invalidate[];
    end
  end
endfunction
