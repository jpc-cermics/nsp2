// Copyright (C) Jerome Lelong, Bruno Pincon
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

function [S] = st_deviation(x, varargin, varargopt)
// obsolete function replaced by the next one 
  printf("\nWARNING: st_deviation is obsolete, use std instead\n")
  S = std(x, varargin(:), varargopt(:))
endfunction

function [S,m] = std(x, varargin, varargopt)
      
   if nargin < 1 then
      error("Error: std needs at least one input argument")
   end
   
   if ~is(x,%types.Mat) then
      error("Error: first argument should be of type Mat")
   end

   [V,m] = var(x, varargin(:), varargopt(:));
   S = sqrt(V);
endfunction
