// Copyright (C) 1999-2017 Stephane Mottelet (UTC)
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

function c=bluecolormap(N)
  c=[zeros(N,1),zeros(N,1),(1:N)'/N]+graycolormap(N);
  c=c/max(c);
endfunction

function b=bonecolormap(m)
  //Gray-scale with a tinge of blue color map.
  b=(7*graycolormap(m)+fliplr(hotcolormap(m)))/8;
endfunction

function c=coolcolormap(m)
  // Shades of cyan and magenta color map.
  r=(0:m-1)'/max(m-1,1);
  c=[r,1-r,ones(m,1)];
endfunction

function c=coppercolormap(m)
  // copper color map.
  c=min(1,graycolormap(m)*diag([1.2500,0.8,0.5]));
endfunction

function c=redcolormap(N)
  c=[(1:N)'/N,zeros(N,1),zeros(N,1)]+graycolormap(N);
  c=c/max(c);
endfunction
