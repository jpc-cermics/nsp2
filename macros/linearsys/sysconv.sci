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

function [s1,s2]=sysconv(s1,s2)
// Converts s1 and s2 into common representation in order that
// system interconnexion operations can be applied.
// The conversion rules in given in the following table.
//   'c'  -> continuous time system
//   'd'  -> discrete time system
//    n   -> sampled system with sampling period n
//    []  -> system with undefined time domain
//For mixed systems s1 and s2 are put in state-space representation.
// Meaning:
// n1,n2    -> sampling period
// c2e(s,n) -> the continuous-time system s is transformed into
//            a sampled system with sampling period n.
// c(s)     -> conversion to continuous (time domain is 'c')
// d(s)     -> conversion to discrete (time domain is 'd')
// e(s,n)   -> conversion to samples system with period n
//
//
// s1\s2|    'c'     |     'd'     |      n2        |     []     |
// ---------------------------------------------------------------
// 'c'  | nothing    |uncompatible | c2e(s1,n2)     |  c(s2)     |
// ---------------------------------------------------------------
// 'd'  |uncompatible| nothing     | e(s1,n2)       |  d(s2)     |
// ---------------------------------------------------------------
// n1   | c2e(s2,n1) | e(s2,n1)    | n1<>n2 uncomp  |  e(s2,n1)  |
//      |            |             | n1=n2  nothing |            |
// ---------------------------------------------------------------
// []   | c(s1)      | d(s1)       | e(s1,n2)       |  nothing   |
// ---------------------------------------------------------------
  
  if type(s1,'short') == 'r' then s1=tf2ss(s1);end
  if type(s2,'short') == 'r' then s2=tf2ss(s2);end

  select s1.dom 
   case  'c' then 
    select s2.dom 
     case 'c' then %t;
     case 'd' then printf('time domains are not compatible\n')
     case 's' then s1=dscr(s1,s2.dt);
     case 'u' then s2.dom = 'c';
    end
   case  'd' then 
    select s2.dom 
     case 'c' then printf('time domains are not compatible\n')
     case 'd' then %t;
     case 's' then s1.dt = s2.dt; s1.dom = 's'
     case 'u' then s2.dom = s1.dom;
    end
   case  's' then 
    select s2.dom 
     case 'c' then s2=dscr(s2,s1.dt);
     case 'd' then 
     case 's' then if s1.dt <> s2.dt then printf('time domains are not compatible\n');end
     case 'u' then s2.dt=s1.dt; s2.dom = 's';
    end
   case  'u' then 
    select s2.dom 
     case 'c' then s1.dom='c';
     case 'd' then s1.dom='d';
     case 's' then s1.dt = s2.dt; s1.dom = 's'
     case 'u' then %t
    end
end
endfunction

