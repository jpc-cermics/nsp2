function [tg,fr]=group(npts,a1i,a2i,b1i,b2i)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks (INRIA)
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
  
//Calculate the group delay of a digital filter
//with transfer function h(z).
//The filter specification can be in coefficient form,
//polynomial form, rational polynomial form, cascade
//polynomial form, or in coefficient polynomial form.
//  npts :Number of points desired in calculation of group delay
//  a1i  :In coefficient, polynomial, rational polynomial, or
//       :cascade polynomial form this variable is the transfer
//       :function of the filter.  In coefficient polynomial
//       :form this is a vector of coefficients (see below).
//  a2i  :In coeff poly form this is a vector of coeffs
//  b1i  :In coeff poly form this is a vector of coeffs
//  b2i  :In coeff poly form this is a vector of coeffs
//  tg   :Values of group delay evaluated on the grid fr
//  fr   :Grid of frequency values where group delay is evaluated
//
//In the coefficient polynomial form the tranfer funtion is
//formulated by the following expression:
//
//       h(z)=prod(a1i+a2i*z+z**2)/prod(b1i+b2i*z+z^2)
//
// Author: C. Bunks  date: 2 March 1988

//get frequency grid values in [0,.5)
  
  fr=(0:.5/npts:.5*(npts-1)/npts);
  
  //get the of arguments used to called group
  //if the number of arguments is 2 then
  //the case may be non-cascade
  
  hcs=1;
  if nargin==2 then
    
    //get the type of h and the variable name
    
    h=a1i;
    ht=type(h,'short');
    //if ht==1 then h is a vector containing filter coefficients
    if ht=='m' then
      //make h a rational polynomial
      hs=max(size(h));
      z=poly(0,'z');
      h=poly(h,'z','c');
      h=gtild(h,'d')*(1/z^(hs-1));
      ht=type(h,'short');
    end
    //if ht=='r' then h is a rational polynomial
    //(perhaps in cascade form)
    if ht== 'r' then
      z=h.num.get_var[];
      hcs=max(size(h.num));
    end
    
    //if the rational polynomial is not in cascade form then
    if hcs==1 then
      //if ht==2 then h is a regular polynomial
      if ht=='p' then
	z=h.get_var[];
      end
      //get the derivative of h(z)
      hzd=derivat(h);
      //get the group delay of h(z)
      z=poly(0,z);
      tgz=-z*hzd/h;
      //evaluate tg
      rfr=exp(2*%pi*%i*fr);
      tg=real(freq(tgz.num,tgz.den,rfr));
      //done with non-cascade calculation of group delay
    end
  end
  
  //re-organize if h is in cascade form
  
  if hcs>1 then
    xc=[coeff(h.num),coeff(h.den)];
    a2i=xc(1:hcs);
    a1i=xc(hcs+1:2*hcs);
    b2i=xc(3*hcs+1:4*hcs);
    b1i=xc(4*hcs+1:5*hcs);
    nargin=5;
  end
  
  //if implementation is in cascade form
  
  if nargin==5 then
    
    //a1i,a2i,b1i,b2i are the coefficients of a
    //second order decomposition of the filter
    //(i.e. in cascade form, see Deczky)
    
    phi=2*%pi*fr;
    z=poly(0,'z');
    ejw=freq(z,1,exp(%i*phi));
    emjw=freq(z,1,exp(-%i*phi));
    
    a1=a1i'*ones_deprecated(phi);
    b1=b1i'*ones_deprecated(phi);
    a2=a2i'*ones_deprecated(phi);
    b2=b2i'*ones_deprecated(phi);
    ejw=ones_deprecated(a1i)'*ejw;
    emjw=ones_deprecated(a1i)'*emjw;
    
    aterm=(a1+2*ejw)./(a1+ejw+a2.*emjw);
    bterm=(b1+2*ejw)./(b1+ejw+b2.*emjw);
    
    tgi=real(bterm-aterm);
    tg=ones_deprecated(a1i)*tgi;
    
    //done with cascade calculation of group delay
  end
endfunction
