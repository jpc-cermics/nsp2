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

function [ok,str,arglist,H]=detect_options(typeOfPlot,arglist,Hin)
  function [ok,msg,arglist1,H]=extract_options_from_arglist(arglist)
    ok=%t;msg="";
    tags=['colorbar';'light';'backfacelighting';'backfacecolor';'backfaceculling';'view';
          'facecolor';'edgecolor';'shading';'axis';'ticksx';'ticksy';'background';
          'foreground';'xlabel';'ylabel';'title';'zlabel';'hidden';'grid';'displayname';
          'legend';'location';'colormap';'xscale';'yscale';'margin'];
    // find firts tag 
    lstart=0;
    for i=1:length(arglist) do
      if type(arglist(i),'short')=='s' && or(arglist(i)==tags) then
        lstart=i;break;
      end
    end
    if lstart==0 then arglist1=arglist;H=hash(1);return;end
    arglist1=arglist.sublist[1:lstart-1];
    n=length(arglist);
    H=hash(10);

    for i=lstart:2:n do
      if type(arglist(i),'short')=='s' && ~isempty(arglist(i)==tags) then
        if i+1 <= n then
          if H.iskey[arglist(i)] then
            if type(H(arglist(i)),'short')=='ce' then
              H(arglist(i)){$+1}=arglist(i+1);
            else
              H(arglist(i))={H(arglist(i)),arglist(i+1)};
            end
          else
            H(arglist(i))=arglist(i+1);
          end
        else
          ok=%f;
          msg=sprintf("Error: %s should be followed by a value",arglist(i));
          return;
        end
      else
        ok=%f;
        msg=sprintf("Error: argument %d should be a keyword",i);
        return
      end
    end
  endfunction

  function [ok,msg,leg_position]=parse_Location(funcName,key,value)
    ok=%t;msg="",leg_position='ur';
    locations=['north','ur';'south','dr';'east','ur';'west','ul';'northeast','ur';
               'northwest','ul';'southeast','dr';'southwest','dl';'northoutside','urm';
               'southoutside','drm';'eastoutside','urm';'westoutside','drm';
               'northeastoutside','urm';'northwestoutside','urm';'southeastoutside','drm';
               'southwestoutside','drm';
               'best','ur',//Inside axes where least conflict occurs with plot data
               'bestoutside','urm';'none','ur']
    // nsp_locations [ 'dl', 'dr', 'drm', 'ul', 'ur', or 'urm']
    if type(value,'short') <> 's' then
      ok=%f;
      msg="legend position should be a string\n";
      return;
    end

    I=find(value==locations(:,1));
    if ~isempty(I) then
      leg_position=locations(I,2);
    else
      msg=sprintf('%s: invalid legend position specification %s',typeOfPlot,value);
      ok=%f;
    end
  endfunction

  function [ok,msg,cmap]=parse_Colormap(funcName,key,value)
    ok=%t;msg="";cmap=[];
    nc=32;
    select type(value,'short')
      case 's' then
       select value
         case 'hot' then cmap=hotcolormap(nc);
         case 'gray' then cmap=graycolormap(nc);
         case 'copper' then cmap=coppercolormap(nc);
         case 'cool' then cmap=coolcolormap(nc);
         case 'bone' then cmap=bonecolormap(nc);
         case 'red' then cmap=redcolormap(nc);
         case 'red' then cmap=redcolormap(nc);
         case 'green' then cmap=greencolormap(nc);
         case 'blue' then cmap=bluecolormap(nc);
         case 'jet' then cmap=jetcolormap(nc);
         else
           msg=sprintf('%s: unknown colormap name',funcName);
           ok=%f;
       end
      case 'm' then
       if size(value,2)==3 then
         cmap=value;
       else
         msg=sprintf('%s: colormap spec must be a n x 3 matrix',funcName);
         ok=%f;
       end
      else
        msg=sprintf('%s: colormap property must be a string or a nx3 matrix',funcName);
        ok=%f;
    end
  endfunction

  function [ok,msg,col]=parse_Color(funcName,key,value)
    ok=%t;msg="";col="";
    select type(value,'short')
      case 'm' then
       // a matrix (must be a 3 element vector)
       if (length(value)==3) && or(size(value)==1) then
         col=value;
       else
         msg=sprintf('%s: %s color spec must be a 3 element vector or a scalar',funcName, ...
                     colName);
         ok=%f;
         return
       end
      case 's' then
       // a string
       if key=="facecolor" then
         if ~or(value==["none","flat","interp"]) then
           msg=sprintf('%s: ''%s'' is an unknown facecolor specification. \n\tExpecting ""none"" or ""flat"" or ""interp""', ...
                       funcName,value);
           ok=%f;
           return
         end
       elseif key=="edgecolor" then
         if value <> "none" then
           msg=sprintf('%s: %s is an unknown edgecolor specification',funcName,value);
           ok=%f;
           return
         end
       elseif key=="backfacecolor" then
         if value <> "none" then
           msg=sprintf('%s: %s is an unknown backfacecolor specification',funcName,value);
           ok=%f;
           return
         end
       end
       col=value;
      else
        msg=sprintf('%s: %s property has a wrong type (%s), a three element vector or string is expected', ...
                    funcName,colName,type(value,'string'));
        ok=%f;
        return
    end
  endfunction

  function [ok,msg,cbar]=parse_ColorBar(funcName,key,value)
    // Parse colorbar 
    // --------------
    ok=%t;msg="";cbar=[];
    value=value{1};
    if type(value,'short') <> 's' then
      msg=sprintf('%s: missing colorbar spec',funcName);
      ok=%f;
      return;
    end
    cbar_T=['off','on','right','left','bottom','bot','top'];
    cbar_v=['off','left','right','left','bot','bot','top'];
    I=find(value==cbar_T)
    if ~isempty(I) then
      cbar=cbar_v(I(1));
    else
      msg=sprintf('%s: %s is an unknown colorbar spec',funcName,value);
      ok=%f;
      return
    end
  endfunction

  function [ok,msg,vect]=parse_Light(typeOfPlot,key,value)
    //
    // --------------
    ok=%t;msg="";vect=[];
    if ~or(typeOfPlot==["props","surfl","trisurfl"]) then
      msg=sprintf('%s: light property is not allowed (see surfl or trisurfl)',typeOfPlot);
      ok=%f;
      return
    end
    if type(value,'short')=='m' then
      if length(value)==2 then
        az=value(1)*%pi/180;
        el=value(2)*%pi/180;
        vect=[cos(az)*sin(el);sin(az)*sin(el);cos(el)];
      elseif length(value)==3 then
        vect=value;
        vect=vect(:);
      else
        msg=sprintf('%s: light specification must be a 2 or 3-vector',typeOfPlot);
        ok=%f;
        return
      end
    else
      msg=sprintf('%s: light specification must be a vector',typeOfPlot);
      ok=%f;
      return
    end
  endfunction;// end of parseLight

  function [ok,msg,bfc]=parse_BackFaceLighting(typeOfPlot,key,value)
    // --------------
    ok=%t;msg="";bfc=[];
    value=value{1};

    if ~or(typeOfPlot==["props","surfl","trisurfl"]) then
      msg=sprintf('%s: BackFaceLighting property is not allowed (see surfl or trisurfl)', ...
                  typeOfPlot);
      ok=%f;
      return;
    end

    if type(value,'short')=='s' then
      select value
        case 'lit' then
         bfc='none';
        case 'unlit' then
         bfc='auto';
        else
          msg=sprintf('%s: unknown BackFaceLighting specification ''%s''',typeOfPlot,value);
          ok=%f;
          return
      end
    else
      msg=sprintf('%s: BackFaceLighting specification must be a string',typeOfPlot);
      ok=%f;
      return
    end
  endfunction;// end of parseBackFaceLighting

  function [ok,msg,bfc]=parse_BackFaceCulling(typeOfPlot,key,value)
    // --------------
    ok=%t;msg="";bfc=[];
    value=value{1};
    if type(value,'short')=='s' then
      select value
        case 'yes' then
         bfc='yes';
        case 'no' then
         bfc='no';
        else
          msg=sprintf('%s: unknown BackFaceCulling specification ''%s''',typeOfPlot,value);
          ok=%f;
          return
      end
    else
      msg=sprintf('%s: BackFaceCulling specification must be a string',typeOfPlot);
      ok=%f;
      return
    end
  endfunction;// end of parseBackFaceCulling

  function [ok,msg,az,el]=parse_View(typeOfPlot,key,value)
    // --------------
    ok=%t;msg="";
    az=45;// default azimuth
    el=54.7;// default elevation
    if type(value,'short')=='m' then
      p=value;
      if size(p,1)==1 then
        p=p';
      end
      [li,co]=size(p);
      if li==2 then
        az=p(1,:);
        el=p(2,:);
      elseif li==3 then
        ez=zeros(1,co);
        al=zeros(1,co);
        for k=1:co do
          el(k)=acos(p(3,k)/norm(p(:,k)))/%pi*180;
          az(k)=atan(p(2,k),p(1,k))/%pi*180;
        end
      else
        msg=sprintf('%s: view specification must be a 2 or 3-vector',typeOfPlot);
        ok=%f;
        return
      end
    else
      msg=sprintf('%s: view specification must be a vector',typeOfPlot);
      ok=%f;
      return
    end
  endfunction;// end of parseView

  function [ok,msg,shading]=parse_Shading(typeOfPlot,key,value)
    // --------------
    ok=%t;msg="";shading=%f;
    if type(value,'short')=='s' then
      select value
        case 'flat' then
         shading=%f;
        case 'faceted' then
         shading=%f;
        case 'interp' then
         shading=%t;
        else
          msg=sprintf('%s: unknown shading specification ''%s'' use ''flat'''+ ...
                      ', or ''faceted'', or ''interp''.',typeOfPlot,value);
          ok=%f;
          return
      end
    else
      msg=sprintf('%s: shading specification must be a string',typeOfPlot);
      ok=%f;
      return
    end
  endfunction;// end of parseShading

  function [ok,msg,H]=parse_Axis(funcName,key,values)
    // Parsing function for the 'axis' property 
    // axisStyle -> axesflag parameter of nsp 
    styles_mtlb=['off';'box';'trihedral';'normal';'right';'center';'origin'];
    styles_nsp=[0;2;1;1;3;4;5];
    // --------------
    ok=%t;msg="";
    H=hash(10);
    // value may be here a set of values 
    if type(values,'short') <> 'ce' then values={values};end
    for i=1:size(values,'*') do
      value=values{i};
      if type(value,'short')=='s' then
        I=find(value==styles_mtlb);
        if ~isempty(I) then H.axesflag=styles_nsp(I);continue;end
        if value.equal['equal'] then H.iso=%t;continue;end
        if value.equal['manual'] || value.equal['vis3d'] then H.manual=%t;continue;end
        ok=%f;
        msg=sprintf("%s: unrecognized axis value %s",funcName,value);
        return;
      elseif type(value,'short')=='m' then
        // can also be of size 8 to fix the color remaping
        if length(value)==6 || length(value)==4 then
          H.rect=value;
        else
          msg=sprintf('%s: axis limits must be a 6 or 4 element vector',funcName);
          ok=%f;
          return
        end
      else
        msg=sprintf('%s: axis property must be a string or a vector',funcName);
        ok=%f;
        return
      end
    end
  endfunction

  function [ok,msg,legends]=parse_Legend(typeOfPlot,key,value)
    // DisplayName or legend are accepted 
    // argument can be a cell array of legends or a string matrix 
    // --------------
    ok=%t;msg="";
    if type(value,'short')=='s' then
      v={};
      for i=1:size(value,'*') do v{i}=value(i);end
      value=v;
    end
    legends=value;
  endfunction

  function [ok,msg,label]=parse_Label(typeOfPlot,key,value)
    // Parsing label for the 'xlabel,ylabel,zlabel' properties 
    // --------------
    ok=%t;msg="";label="";
    if type(value,'short')=='s' then
      label=value;
    else
      msg=sprintf('%s: label should be a string',typeOfPlot);
      ok=%f;
      return
    end
  endfunction

  function [ok,msg,hide]=parse_HideMode(typeOfPlot,key,value)
    // Parsing function for the 'hidden' property 
    // --------------
    ok=%t;msg="";hide=%t;
    select type(value,'short')
      case 's' then
       select value
         case 'yes' then hide=%t;
         case 'no' then hide=%f;
         case 'off' then hide=%f;
         else
           msg=sprintf('%s: unrecognized hide mode ''%s''',typeOfPlot,value);
           ok=%f;
           return
       end
    end
  endfunction

  function [ok,msg,gridFlag,gridColor]=parse_Grid(funcName,key,value)
    // --------------
    gridColor=xget('lastpattern')+3;
    ok=%t;msg="";gridFlag=%t;gridColor=[];
    select type(value,'short')
      case 's' then
       select value
         case 'on' then gridFlag=%t;
         case 'off' then gridFlag=%f;
         else
           msg=sprintf('%s: unknown grid spec',funcName);
           ok=%f;
           return
       end
      case 'm' then
       if length(value)==3 || length(value)==1 then
         gridColor=value;
         gridFlag=%t;
       else
         msg=sprintf('%s: grid color must be a 3-vector',funcName);
         ok=%f;
       end
    end
  endfunction

  function [ok,msg,scale]=parse_Scale(funcName,key,value)
    // Parsing function for the 'Xscale' or 'Yscale' property
    // value can be 'log' or 'linear'
    ok=%t;msg="";scale='linear'
    select type(value,'short')
      case 's' then
       select value
         case 'log' then scale='log'
         case 'linear' then scale='linear'
         else
           msg=sprintf('%s : unknown scale type ''%s''',funcName,value);
           ok=%f;return;
       end
    end
  endfunction

  function [ok,msg,margin]=parseMargin(funcName,key,value)
    // Parsing function for the 'margin' property 
    ok=%t;msg="";marg=0;
    select type(value,'short')
      case 'm' then
       if (length(value)==1) & (value >= 0) & (value < .5) then
         margin=value;
       else
         error(sprintf('%s : margin value must be a positive scalar smaller than .5', ...
                       funcName));
       end
      else
        error(sprintf('%s : missing margin value',funcName));
    end
  endfunction

  // now the code 
  // --------------

  ok=%t;str="";
  [ok,str,arglist,H]=extract_options_from_arglist(arglist);
  if ~ok then return;end
  Hin.merge[H];
  // argList is a Hash table 
  keys=Hin.__keys;
  H=hash(20);

  for i=1:size(keys,'*') do
    key=keys(i);
    tag=tolower(key);
    value=Hin(key);
    select tag
      case 'colorbar' then
       [ok,str,colorBar]=parse_ColorBar(typeOfPlot,key,value);
       if ~ok then return;end
       H.colorBar=colorBar;
      case 'light' then
       [ok,str,lightVect]=parse_Light(typeOfPlot,key,value);
       if ~ok then return;end
       H.lightVect=lightVect;
      case 'backfacelighting' then
       [ok,str,BackFaceColor]=parse_BackFaceLighting(typeOfPlot,key,value);
       if ~ok then return;end
       H.BackFaceColor=BackFaceColor;
      case 'backfacecolor' then
       [ok,str,BackFaceColor]=parse_Color(typeOfPlot,key,value);
       H.BackFaceColor=BackFaceColor;
       if ~ok then return;end
      case 'backfaceculling' then
       [ok,str,BackFaceCulling]=parse_BackFaceCulling(typeOfPlot,key,value);
       if ~ok then return;end
       H.BackFaceCulling=BackFaceCulling;
      case 'view' then
       [ok,str,azimuth,elevation]=parse_View(typeOfPlot,key,value)
       if ~ok then return;end
       H.azimuth=azimuth;H.elevation=elevation;
      case 'facecolor' then
       [ok,str,facecolor]=parse_Color(typeOfPlot,key,value);
       if ~ok then return;end
       select facecolor
         case "none" then H.mesh_only=%t;
         case "interp" then H.shade=%t;
         case "flat" then H.shade=%f;
       end
      case 'edgecolor' then
       [ok,str,edgecolor]=parse_Color(typeOfPlot,key,value);
       if ~ok then return;end
       if edgecolor.equal["none"] then H.mesh=%f;end
      case 'shading' then
       [ok,str,shading]=parse_Shading(typeOfPlot,key,value);
       if ~ok then return;end
       H.shade=shading;
      case 'axis' then
       [ok,msg,Ha]=parse_Axis(typeOfPlot,key,value);
       if ~ok then return;end
       H.merge[Ha];
      case 'ticksx' then
       [ok,str,ticksx]=parse_Ticks(typeOfPlot,key,value);
       if ~ok then return;end
       H.ticksx=ticksx;
      case 'ticksy' then
       [ok,str,ticksx]=parse_Ticks(typeOfPlot,key,value);
       if ~ok then return;end
       H.ticksx=ticksx;
      case 'background' then
       [ok,str,background]=parse_Color(typeOfPlot,key,value);
       if ~ok then return;end
       H.background=background;
      case 'foreground' then
       [ok,str,foreground]=parse_Color(typeOfPlot,key,value);
       if ~ok then return;end
       H.foreground=foreground;
      case 'xlabel' then
       [ok,str,Xlabel]=parse_Label(typeOfPlot,key,value);
       if ~ok then return;end
       H.Xlabel=Xlabel;
      case 'ylabel' then
       [ok,str,Ylabel]=parse_Label(typeOfPlot,key,value);
       if ~ok then return;end
       H.Ylabel=Ylabel;
      case 'title' then
       [ok,str,Title]=parse_Label(typeOfPlot,key,value);
       if ~ok then return;end
       H.Title=Title;
      case 'zlabel' then
       [ok,str,Zlabel]=parse_Label(typeOfPlot,key,value);
       if ~ok then return;end
       H.Zlabel=Zlabel;
      case 'hidden' then
       [ok,str,hidden]=parse_HideMode(typeOfPlot,key,value);
       if ~ok then return;end
       H.hidden=hidden;
      case 'grid' then
       [ok,str,gridFlag,gridColor]=parse_Grid(typeOfPlot,key,value);
       if ~ok then return;end
       H.gridFlag=gridFlag;
       H.gridColor=gridColor;
      case {'displayname','legend'} then
       [ok,msg,legends]=parse_Legend(typeOfPlot,key,value)
       if ~ok then return;end
       H.Legend=legends;
       // for k=1:nbProc; argList.remove_first[]; end;
      case 'location' then
       [ok,msg,leg_position]=parse_Location(typeOfPlot,key,value);
       if ~ok then return;end
       H.Location=leg_position;
      case 'colormap' then
       [ok,msg,cmap]=parse_Colormap(typeOfPlot,key,value);
       if ~ok then return;end
       H.colormap=cmap;
      case 'xscale' then
       // Parsing function for the 'Xscale' or 'Yscale' property
       [ok,msg,scale]=parse_Scale(typeOfPlot,key,value)
       if ~ok then return;end
       H.Xscale=scale;
      case 'yscale' then
       [ok,msg,scale]=parse_Scale(typeOfPlot,key,value)
       if ~ok then return;end
       H.Yscale=scale;
      case 'margin' then
       [ok,msg,margin]=parseMargin(typeOfPlot,key,value)
      else
        str=sprintf('%s: %s is an unknown property name',typeOfPlot,key);
        ok=%f
        return;
    end
  end
endfunction
