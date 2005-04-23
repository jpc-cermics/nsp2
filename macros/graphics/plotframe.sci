function plotframe(rect,axisdata,flags,legs,subwindow) 
// plotframe - fixes scales, tics and grid on a graphic,
//%Syntax
//  plotframe(rect,axisdata [,flags or leg or subwindow, ...)
//%Parameters
//  rect    : [xmin,ymin,xmax,ymax] data boudaries 
//  axisdata: [nx,mx,ny,my]  mx and my x and y tics, nx,ny : x and y subtics
//  flags   : [quad,bounds] ou quad is a boolean if %t a grid is added
//	      bounds a booleen also : if bounds is %t then rect can be modified
//	      in order to have better scales on both axes which contains the 
//	      rect initial data.
//  subwindow : see xsetech (wrect)
//!
// Copyright INRIA
  f_subwin=%f,f_flags=%f,f_legs=%f;
  r_flags=[%f,%f];
  if nargin == 5 then
    select type(subwindow,'short'),
     case 'm' , r_subwin=subwindow, f_subwin=%t;
     case 'b' , r_flags =subwindow, f_flags =%t;
     case 's', r_legs = subwindow, f_legs= %t;
    end 
  end
  if nargin >= 4 then
    select type(legs,'short'),
     case 'm' , r_subwin=legs, f_subwin=%t;
     case 'b' , r_flags =legs, f_flags =%t;
     case 's', r_legs = legs, f_legs= %t;
    end 
  end
  if nargin >= 3 then
    select type(flags,'short'),
     case 'm' , r_subwin=flags, f_subwin=%t;
     case 'b' , r_flags =flags, f_flags =%t;
     case 's', r_legs = flags, f_legs= %t;
    end 
  end
  if nargin <= 1 then 
    error('Wrong number of arguments ');
    return;
  end
  if f_subwin then 
    xsetech(r_subwin,rect);
  end
  // -- trace du cadre et des echelles
  if r_flags(2) then
    plot2d(0,0,style=0,strf='051',rect=rect,nax=axisdata)
  else
    plot2d(0,0,style=0,strf='011',rect=rect,nax=axisdata)
  end
  // -- trace des legendes d'axes et du titre
  if f_legs then 
    select prod(size(r_legs)),
     case 1, xtitle(r_legs(1)),
     case 2, xtitle(r_legs(1),r_legs(2)),
     case 3, xtitle(r_legs(1),r_legs(2),r_legs(3));
    end 
  end
  if r_flags(1) then  xgrid(); end
endfunction
