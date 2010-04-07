// -*- Mode: scilab -*- 
// Copyright (C) 2010 Jean-Philippe Chancelier Cermics/Enpc
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
// basic tests of markup. 

G=gmarkup("gmarkup.xml");

// G is a markup node 
if type(G,'short') <> 'gmn' then pause;end 
// top level node is a model 
if G.name <> 'model' then pause;end 
L=G.children;
if type(L(2),'short') <> 'gmn' then pause;end 
if type(L(6),'short') <> 'gmn' then pause;end 
H=L(6);
if H.name <> 'elements' then pause;end 

function node=get_node(G,node_name,depth)
// search in node G of type Gmarkup the first subnode 
// named node_name and returns the associated node 
  L= G.children;
  node=[];
  for i=1:length(L)
    elt = L(i);
    if type(elt,'short') == 'gmn' then
      if elt.name == node_name then node= elt;return;end;
      node = get_node(elt,node_name,depth+1);
      if ~node.equal[[]] then 
	return;
      end;
    end
  end
endfunction;

node = get_node(G,"id",0);

if node.children(1) <> "Bache1.__der_Hm" then pause;end 

node2 = get_node(G,"id",0);

if ~node.equal[node2] then pause;end 

// test of m2base64 and base642m 

str = G.children(4);
// get the string inside CDATA 
str1=part(str,10:length(str)-3)
val = base642m(str1)
if ~val.equal[[%pi;%e;4]] then pause;end 
str2=m2base64(val); 
if str2 <> str1 then pause;end 



