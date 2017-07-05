function H=sci_load(fname,varargin)
  
// load a Scilab saved File 
// Copyright (C) 2006-2015  Jean-Philippe Chancelier Enpc/Cermics
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

  function str=sci_load_get_name(F1)    
  // load a name from xdr file 
  // utility function for sci_load
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    id1=F1.get[n=6,type='il'];
    if isempty(id1) then str=[];return;end
    id = ones_new(4,6);
    for j=1:6 do
      idl=id1(j);
      ch=ones_new(4,1);
      for i=1:4 do
	k=idiv(idl+128,256)
	if k < 0 then  k=k-1;end
	ch(i)=idl-256*k
	idl=k;
      end
      id(:,j)=ch;
    end
    id = matrix(id,1,6*4);
    str=scilab_code2str(id);
  endfunction

  function x=sci_load_mat(F1)
  // load a scimatrix 
  // utility function for sci_load
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    s=F1.get[n=3,type='il'];
    mn=s(1)*s(2);
    x=F1.get[n=mn*(s(3)+1),type='dl'];
    if s(3)==1 then x = x(1:mn)+%i*x(mn+1:$);end 
    x=matrix(x,s(1),s(2));
  endfunction

  function x=sci_load_spmat_old(F1)
  // load a scimatrix 
  // utility function for sci_load
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    s=F1.get[n=4,type='il']
    m=s(1);n=s(2);
    // nnz 
    nel=s(4);
    // number of non-null elements for each row 
    rows=F1.get[n=m,type='il'];
    // column indices of non-nul elements 
    ind=F1.get[n=nel,type='il'];
    // non-nul values 
    val=F1.get[n=nel*(s(3)+1),type='dl'];
    if s(3)==1 then val = val(1:nel)+%i*val(nel+1:$);end 
    IJ=[] 
    Im=find(rows <> 0);
    for i=Im do
      nbi=rows(i) 
      IJi=zeros_new(nbi,2);
      indi=ind(1:nbi);ind(1:nbi)=[];
      IJi(:,1)=i,IJi(:,2)=indi';
      IJ=[IJ;IJi];
    end
    x=sparse(IJ,val(:),[m,n]);
    //x=matrix(x,s(1),s(2));
  endfunction

  function x=sci_load_spmat(F1)
  // load a scimatrix 
  // utility function for sci_load
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    s=F1.get[n=4,type='il']
    m=s(1);n=s(2);
    // nnz 
    nel=s(4);
    // number of non-null elements for each row 
    rows=F1.get[n=m,type='il'];
    // column indices of non-nul elements 
    ind=F1.get[n=nel,type='il'];
    // non-nul values 
    val=F1.get[n=nel*(s(3)+1),type='dl'];
    if s(3)==1 then val = val(1:nel)+%i*val(nel+1:$);end 
    // from rows/ind/val to matlab triplet for the 
    // transposed matrix 
    Jc = [0,cumsum(rows)];
    x=spfrommtlb(Jc,ind-1,val,[n,m]);
    x=x';
    //x=matrix(x,s(1),s(2));
  endfunction



  function x=sci_load_bmat(F1)
  // load a boolean matrix 
  // utility function for sci_load
    s=F1.get[n=2,type='il'];
    mn=s(1)*s(2);
    x=F1.get[n=mn,type='il'];
    x=m2b(matrix(x,s(1),s(2)));
  endfunction

  function x=sci_load_imat(F1)
  // load an int matrix 
  // utility function for sci_load
    types_id=['c','sl','x','il','x','x','x','x','x','x','uc','usl','x',  'uil']
    types_nsp=['int8','int16','x','int32','x','x','x','x','x','x','uint8','uint16','x','uint32']
    s=F1.get[n=3,type='il'];
    mn=s(1)*s(2);
    it=s(3);
    typ = types_id(it);
    if typ == 'x'  then 
      error('int matrix badly stored\n');
      return
    end
    x=F1.get[n=mn,type=typ];
    x=matrix(x,s(1),s(2));
    x=m2i(x,types_nsp(it));
  endfunction

  function Str=sci_load_smat(F1)
  // load string matrix 
  // utility function for sci_load
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    s=F1.get[n=3,type='il']; // s(3) is unused 
    mn=s(1)*s(2);
    if mn == 0 then Str=m2s([]);
    else
      x=F1.get[n=mn+1,type='il']; // start and end of each string
      sz= x(2:$)-x(1:$-1); // size of each string 
      Str="";
      for i=1:mn do
	xv=F1.get[n=sz(i),type='il']; // characters in scilab code of str(i)
	Str(i)=scilab_code2str(xv);
      end
      // this should be renamed matrix XXXXXX 
      Str=redim(Str,s(1),s(2));
    end
  endfunction

  function sci_load_fun(F1)
  // load a function 
  // utility function for sci_load
  // not that we do not try to convert the function code 
  // we just read the corresponding data to be able to skip 
  // saved function 
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    nout=F1.get[n=1,type='il'];
    nsiz=6;vv=F1.get[n=nout*nsiz+1,type='il'];
    nin=vv($);
    vv=F1.get[n=nin*nsiz+1,type='il'];
    n=vv($);
    vv=F1.get[n=n,type='il'];
  endfunction

  function L=sci_load_list(F1,type)
  // load a list 
  // utility function for sci_load
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    s=F1.get[n=1,type='il']; // list size 
    x=F1.get[n=s+1,type='il'];// elts positions 
    L=list();
    //printf('reloading a list\n');
    for i=1:s do
      //element is undefined if x(i+1)-x(i)==0
      if x(i+1)<>x(i) then 
	//printf('reloading element %d\n',i);
	obj_type=F1.get[n=1,type='il'];
	select obj_type 
	 case 1 then xval=sci_load_mat(F1);
	 case 4 then xval=sci_load_bmat(F1);
	 case 5 then xval=sci_load_spmat(F1);
	 case 8 then xval=sci_load_imat(F1);
	 case 11 then sci_load_fun(F1);xval=[];
	  printf("Warning: Scilab uncompiled function ignored in list %s\n",str);
	 case 13 then sci_load_fun(F1);xval=[];
	  printf("Warning: Scilab coded function ignored in list %s\n",str);
	 case 10 then xval=sci_load_smat(F1);
	 case 15 then xval=sci_load_list(F1,15);
	 case 16 then xval=sci_load_list(F1,16);
	 case 17 then xval=sci_load_list(F1,17);
	else
	  error(sprintf('unknown type %d  while reloading a list element %d',obj_type,i));
	  return;
	end
	if obj_type<>13 then; L(i)=xval;end 
      end
    end
    if type == 16 then 
      L=tlist(L(1),L(2:$));
    elseif type == 17 then 
      L=mlist(L(1),L(2:$));
    end
  endfunction


  function str=scilab_code2str(id)
  // From internal Scilab code to ascii string 
  // all the code is not complete in alphb ?
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    csiz=63;
    str='';
    alpha="0123456789abcdefghijklmnopqrstuvwxyz_#!$ ();:+-*/\=.,''[]%|&<>~^"; 
    alphb="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ00?0\t00000000$000""{}000`0@0";
    for l=1:size(id,'*') do
      ch=id(l)
      if abs(ch) >= csiz then 
	str = str + '*';
      else 
	if ch >= 0  then
	  str=str+part(alpha,ch+1);
	else
	  str=str+part(alphb,-ch+1);
	end
      end
    end
    str=stripblanks(str);
  endfunction

  H=hash(0);
  varargin.compact[];
  Str=[];
  if length(varargin) > 1 then 
    error('extra arguments must be strings\n');
    return;
  end
  if length(varargin) == 1 then 
    Str = varargin(1);
    if type(Str,'short') <> 's' then 
      error('extra arguments must be strings\n');
      return;
    end
  end
  F1=fopen(fname,mode='rb');
  // Note that id1=F1.get[n=4*6,type='c'];
  // does not work when the codes are negatives.
  while %t then 
    str=sci_load_get_name(F1);
    if isempty(str) then break;end 
    obj_type=F1.get[n=1,type='il'];
    select obj_type 
     case 1 then xval=sci_load_mat(F1);
     case 4 then xval=sci_load_bmat(F1);
     case 5 then xval=sci_load_spmat(F1);
     case 8 then xval=sci_load_imat(F1);
     case 10 then xval=sci_load_smat(F1);
     case 11 then sci_load_fun(F1);xval=[];
      printf("Warning: Scilab uncompiled function ignored in list %s\n",str);
     case 13 then sci_load_fun(F1);xval=[];
      printf("Warning: Scilab coded function %s ignored\n",str);
     case 15 then xval=sci_load_list(F1,15);
     case 16 then xval=sci_load_list(F1,16);
     case 17 then xval=sci_load_list(F1,17);
    else
      error(sprintf('unknown type %d',obj_type));
      break;
      return;
    end
    if isempty(Str) || ~isempty(find(Str==str)) then 
      if nargout < 1 then 
	execstr('resume('+str+'=xval);');
      else
	execstr('H('''+str+''')=xval;');
      end
    end
  end
  F1.close[];
endfunction


function sci_save(fname,varargopt)
// Save in scilab binary mode the 
// arguments which are given as name=val
// Copyright (C) 2006-2015 Jean-Philippe Chancelier

  function sci_save_mat(F1,val)
  // save a matrix 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    F1.put[1,type='il'];
    nn=size(val);
    if and(imag(val)==0) then m_type=0;else m_type=1;end
    F1.put[[nn,m_type],type='il'];
    F1.put[real(val),type='dl'];
    if m_type==1 then 
      F1.put[imag(val),type='dl'];
    end
  endfunction

  function sci_save_imat(F1,val)
  // save an integer matrix 
  // utility for sci_save
  // Copyright (C) 2010-2015  Jean-Philippe Chancelier
    types_id=['c','sl','x','il','x','x','x','x','x','x','uc','usl','x',  'uil']
    types_nsp=['int8','int16','x','int32','x','x','x','x','x','x','uint8','uint16','x','uint32']
    I=find( val.itype[]== types_nsp);
    if isempty(I) then 
      error(sprintf('integer of type %s are not supported in scicoslab and cannot be saved\n",val.itype[]))
      return;
    end
    F1.put[I,type='il'];
    nn=size(val);
    F1.put[[nn,0],type='il'];
    F1.put[i2m(val),type=types_id(I)];
  endfunction

  function sci_save_spmat(F1,val)
  // save a matrix 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    F1.put[5,type='il'];
    nn=size(val);
    // get the triplet of the transpose
    [Jc,Ir,Pr]=spget_mtlb(val');
    if nnz(imag(val))==0 then m_type=0;else m_type=1;end
    // [m,n,type_rc,nnz]
    F1.put[[nn,m_type,length(Ir)],type='il']
    // number of non-null elements for each row 
    row= Jc(2:$)-Jc(1:$-1);
    F1.put[row,type='il'];
    F1.put[Ir+1,type='il'];
    F1.put[real(Pr),type='dl'];
    if m_type == 1 then 
      F1.put[imag(Pr),type='dl'];
    end
  endfunction

  function sci_save_bmat(F1,val)
  // save a boolean 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    F1.put[4,type='il'];
    nn=size(val);
    F1.put[nn,type='il'];
    F1.put[b2m(val),type='il'];
  endfunction

  function sci_save_smat(F1,val)
  // string matrix 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    if size(val,'*')==0 then 
      // emty string matrix does not exists ? 
      sci_save_mat(F1,[]);
    else
      F1.put[10,type='il'];
      nn=size(val);
      F1.put[[nn,0],type='il'];
      if size(val,'*') <> 0 then 
	sz= length(val);
	sz=sz(:)';
	x=cumsum([1,sz]);
	F1.put[x,type='il']; // start and end of each string
	for j=1:size(val,'*') do
	  xv=sci_str2code(val(j));
	  F1.put[xv,type='il']; // start and end of each string
	end
      end
    end
  endfunction


  function sci_save_list(F1,val,obj_type)
  // A list
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    if nargin<=2 then obj_type=15;end
    F1.put[obj_type,type='il'];
    nn=size(val);
    F1.put[nn,type='il'];
    x=ones_new(1,nn+1);
    for i=1:nn do
      ok=execstr('elt=val(i)',errcatch=%t);
      if ok then 
	select type(elt,'short')
	 case 'm' then x(i+1) = sci_save_count_mat(elt);
	 case 'sp' then x(i+1) = sci_save_count_spmat(elt);
	 case 'b' then x(i+1) = sci_save_count_bmat(elt);
	 case 's' then x(i+1) = sci_save_count_smat(elt);
	 case 'l' then x(i+1) = sci_save_count_list(elt);
	 case 'h' then x(i+1) = sci_save_count_hash(elt);
	else 
	  error(sprintf('unknown type %s',type(elt,'short')));
	  return;
	end 
      else
	x(i+1)=0;
      end
    end
    x=cumsum(x);
    F1.put[x,type='il'];
    for i=1:nn do
      ok=execstr('elt=val(i)',errcatch=%t);
      if ok then 
	select type(elt,'short')
	 case 'm' then sci_save_mat( F1,elt);
	 case 'sp' then sci_save_spmat( F1,elt);
	 case 'b' then sci_save_bmat( F1,elt);
	 case 's' then sci_save_smat( F1,elt);
	 case 'l' then sci_save_list( F1,elt);
	 case 'h' then sci_save_hash(F1,elt);
	else 
	  error(sprintf('unknown type %s',type(elt,'short')));
	  break;
	  return;
	end 
      end
    end
  endfunction


  function sci_save_hash(F1,val);
  // save hash table as struct 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    if val.iskey['type'] then 
      // is key type is present 
      sci_type = val('type');
      strs=val.__keys';
      I=find(strs=='type');strs(I)=[];
      tmlist = 17;
      if val.iskey['mlist'] then     
	I=find(strs=='mlist');strs(I)=[];
	tmlist=17;
      end
      if val.iskey['tlist'] then 
	I=find(strs=='tlist');strs(I)=[];
	tmlist=16;
      end
      // backward compatibility for scicos
      if sci_type== 'cpr' then tmlist=16;end
      nn=[sci_type,strs];
      L=list(nn);
      for s=2:size(nn,'*') do L(s)= val(nn(s));end 
      sci_save_list(F1,L,tmlist);
    else
      // save a hash table as a scilab struct
      nn=['st';'dims';val.__keys]';
      L=list(nn,[1,1]);
      for s=3:size(nn,'*') do L(s)= val(nn(s));end 
      sci_save_list(F1,L,17);
    end
  endfunction

  function y=sci_save_count_mat(val)
  // count the space needed to save mat 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    nn=size(val,'*');
    if and(imag(val)==0) then m_type=0;else m_type=1;end
    y= 2 + nn*(1+m_type);
  endfunction

  function y=sci_save_count_bmat(val)
  // save a boolean  
  // utility for sci_save
    nn=size(val,'*'); 
    y= ceil((3+nn)/2);
  endfunction

  function y=sci_save_count_smat(val)
  // string matrix 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    nn=size(val,'*');
    if nn==0 then 
      // emty string matrix does not exists ? 
      y=sci_save_count_mat([]);
    else
      y = ceil(( 4 +(nn+1)+ sum(length(val)))/2);
    end
  endfunction

  function y=sci_save_count_spmat(val)
  // sparse matrix
  // utility for sci_save
  // Copyright (C) 2007-2015 Jean-Philippe Chancelier
    nn=size(val,'*');
    if nnz(imag(val))==0 then m_type=0;else m_type=1;end
    y = 2 + nn(1) + nnz(val)*(2+m_type);
  endfunction


  function y=sci_save_count_list(val)
  // A list
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    nn=size(val);
    y = ceil((2 + nn+1)/2);
    for i=1:nn do
      elt=val(i);
      select type(elt,'short')
       case 'm' then y = y + sci_save_count_mat(elt);
       case 'sp' then y = y + sci_save_count_spmat(elt);
       case 'b' then y = y + sci_save_count_bmat(elt);
       case 's' then y = y + sci_save_count_smat(elt);
       case 'l' then y = y + sci_save_count_list(elt)
       case 'h' then y = y + sci_save_count_hash(elt)
      else 
	error(sprintf('unknown type %s',type(elt,'short')));
	return;
      end 
    end
  endfunction

  function  y=sci_save_count_hash(val);
  // save hash table as struct 
  // utility for sci_save
  // Copyright (C) 2006-2015 Jean-Philippe Chancelier
    if val.iskey['type'] then 
      // is key type is present 
      sci_type = val('type');
      strs=val.__keys';
      I=find(strs=='type');strs(I)=[];
      tmlist = 17;
      if val.iskey['mlist'] then     
	I=find(strs=='mlist');strs(I)=[];
	tmlist=17;
      end
      if val.iskey['tlist'] then 
	I=find(strs=='tlist');strs(I)=[];
	tmlist=16;
      end
      // backward compatibility
      if sci_type== 'cpr' then tmlist=16;end
      nn=[sci_type,strs];
      L=list(nn);
      for s=2:size(nn,'*') do L(s)= val(nn(s));end 
    else
      // save a hash table as a scilab struct
      nn=['st';'dims';val.__keys]';
      L=list(nn,[1,1]);
      for s=3:size(nn,'*') do L(s)= val(nn(s));end 
    end
    y=sci_save_count_list(L);
  endfunction

    
  S=varargopt.__keys;
  if size(S,'*')==0 then 
    error("You must specify the names of arguments to be saved as name=value");
    return;
  end
  F1=fopen(fname,mode='wb');
  for i=1:size(S,'*') do
    vname=S(i);
    id=sci_str2code(vname);
    id = id(1:min(24,length(id)));
    if length(id) < 24 then id=[id,40*ones_new(1,24-length(id))];end
    id=[1,256,256^2,256^3]*matrix(id,4,6);
    F1.put[id,type='il'];
    val=varargopt(S(i));
    obj_type=type(val,'short') 
    select obj_type
     case 'm' then 
      sci_save_mat(F1,val);
     case 'sp' then 
      sci_save_spmat(F1,val);
     case 'b' then 
      sci_save_bmat(F1,val);
     case 's' then 
      sci_save_smat(F1,val);
     case 'l' then 
      sci_save_list(F1,val);
     case 'h' then 
      sci_save_hash(F1,val);
     case 'i' then 
      sci_save_imat(F1,val);
    else
      error(sprintf('unknown type %s',obj_type));
      break;
    end
  end
  F1.close[];
endfunction

function id=sci_str2code(str)
// from string to scilab coded string 
// Copyright (C) 2006-2015 Jean-Philippe Chancelier
  taba2s = [ 100,101,102,103,104,105,106,107,108,-40,...
	     110,111,112,113,114,115,116,117,118,119,...
	     120,121,122,123,124,125,126,127,128,129,...
	     130,131, 40, 38,-53, 37, 39, 56, 58, 53,...
	     41, 42, 47, 45, 52, 46, 51, 48,  0,  1,...
	     2,  3,  4,  5,  6,  7,  8,  9, 44, 43,...
	     59, 50, 60,-38,-61,-10,-11,-12,-13,-14,...
	     -15,-16,-17,-18,-19,-20,-21,-22,-23,-24,...
	     -25,-26,-27,-28,-29,-30,-31,-32,-33,-34,...
	     -35, 54, 49, 55, 62, 36,-59, 10, 11, 12,...
	     13, 14, 15, 16, 17, 18, 19, 20, 21, 22,...
	     23, 24, 25, 26, 27, 28, 29, 30, 31, 32,...
	     33, 34, 35,-54, 57,-55, 61,227 ];
  id = ascii(str);
  I=find(id <= 127);
  id(I) = taba2s(1+ id(I));
  I=find(id > 127);
  id(I) = id(I)+100;
endfunction
