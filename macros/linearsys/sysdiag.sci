// Copyright  2010-2015 
// Jean-Philippe Chancelier Cermics/Enpc, François Delebeceque, Serge Steer (Inria)
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

function res=sysdiag(varargin)
  // returns the block-diagonal system made with subsystems put in the main diagonal
  // r=sysdiag(a1,a2,...,an)
  // ai: subsystems (i.e. gains, or linear systems in state-space or transfer form)
  //Example
  // s=poly(0,'s')
  // sysdiag(rand(2,2),1/(s+1),[1/(s-1);1/((s-2)*(s-3))])
  // sysdiag(tf2ss(1/s),1/(s+1),[1/(s-1);1/((s-2)*(s-3))])
  res=varargin(1);
  [m1,n1]=size(res);
  for k=2:size(varargin)
    ak=varargin(k)
    // # operator will not cover the mixed types
    // res=[res # ak]
    [mk,nk]=size(ak);
    res=[res,0*ones(m1,nk);0*ones(mk,n1),ak]
    m1=m1+mk
    n1=n1+nk
  end
endfunction
