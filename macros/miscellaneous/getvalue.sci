function [ok,%1,%2,%3,%4,%5,%6,%7,%8,%9,%10,%11,%12,%13,%14,%15,%16,%17,%18]=getvalue(%desc,%labels,%typ,%ini)
// getvalues - %window dialog for data acquisition 
//%Synta%
//  [ok,%1,..,%11]=getvalue(desc,labels,typ,ini)
//%Parameters
//  desc    : column vector of strings, dialog general comment 
//  labels  : n column vector of strings, labels(i) is the label of 
//            the ith required value
//  typ     : list(typ1,dim1,..,typn,dimn)
//            typi : defines the type of the ith required value
//                   if may have the following values:
//                   'mat' : stands for matrix of scalars 
//                   'col' : stands for column vector of scalars
//                   'row' : stands for row vector of scalars
//                   'vec' : stands for  vector of scalars
//                   'str' : stands for vector of strings
//                   'lis' : stands for list
//                   'pol' : stands for polynomials
//                   'r'   : stands for rational
//            dimi : defines the size of the ith required value
//                   it must be 
//                    - an integer or a 2-vector of integers (-1 stands for 
//                      arbitrary dimension)
//                    - an evaluatable character string 
//  ini     : n column vector of strings, ini(i) gives the suggested
//            response for the ith required value
//  ok      : boolean ,%t if ok button pressed, %f if cancel button pressed
//  xi      : contains the ith required value if ok==%t
//%Description
// getvalues macro encapsulate x_mdialog function with error checking,
// evaluation of numerical response, ...
//%Remarks
// All correct scilab syntax may be used as responses, for matrices 
// and vectors getvalues automatically adds [ ] around the given response
// before numerical evaluation
//%Example
// labels=['magnitude';'frequency';'phase    '];
// [ampl,freq,ph]=getvalue('define sine signal',labels,..
//            list('vec',1,'vec',1,'vec',1),['0.85';'10^2';'%pi/3'])
// 
//%See also
// x_mdialog, x_dialog
//!
// Copyright INRIA

  if ~exists('%scicos_context') then 
    exec_context = hash_create(10);
  else
    exec_context = %scicos_context;
  end
    
  %nn=size(%labels,0);
  if size(%typ,0)<>2*%nn then
    error('%typ : list(''type'',[sizes],...)')
  end
  %1=[];%2=[];%3=[];%4=[];%5=[];%6=[];%7=[];%8=[];%9=[];%10=[];%11=[];
  %12=[];%13=[];%14=[];%15=[];%16=[];%17=[];%18=[];

  if nargin < 4  then %ini=smat_create(%nn,1);end;
  
  ok=%t
  
  while %t do
    %str1=x_mdialog(%desc,%labels,%ini)
    if size(%str1,1)==0 then ok=%f,%str=[];break,end
    %str=%str1;
    for %kk=1:%nn
      %cod=ascii(%str(%kk))
      %spe=find(%cod==10)
      if ~isempty(%spe) then
	%semi=ascii(';')
	%cod(%spe)=%semi*ones(size(%spe'))
	%str(%kk)=ascii(%cod)
      end
    end

    %nok=0
    for %kk=1:%nn
      select part(%typ(2*%kk-1),1:3)
       case 'mat'
	[%ierr,%H]=execstr('%vv=['+%str(%kk)+']',env=exec_context, errcatch=%t);
	if %ierr==%f then %nok=-%kk;break,end
	%vv=%H.%vv;
	if type(%vv,'string')<>"Mat" then %nok=-%kk,break,end
	%sz=%typ(2*%kk);
	if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	[%mv,%nv]=size(%vv)
	%ssz=string(%sz(1))+' x '+string(%sz(2))
	if %mv*%nv==0 then
	  if  %sz(1)>=0&%sz(2)>=0&%sz(1)*%sz(2)<>0 then %nok=%kk,break,end
	else
	  if %sz(1)>=0 then 
	    if %mv<>%sz(1) then %nok=%kk,break,end,end
	  if %sz(2)>=0 then 
	    if %nv<>%sz(2) then %nok=%kk,break,end,end
	end
       case 'vec'
	[%ierr,%H]=execstr('%vv=['+%str(%kk)+']',env=exec_context,errcatch=%t)
	if %ierr==%f then %nok=-%kk;break,end // 
	%vv=%H.%vv;
	if type(%vv,'string')<>"Mat" then %nok=-%kk,break,end
	%sz=%typ(2*%kk);if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	%ssz=string(%sz(1))
	%nv=prod(size(%vv))
	if %sz(1)>=0 then 
	  if %nv<>%sz(1) then %nok=%kk,break, end
	end
       case 'pol'
	[%ierr,%H]=execstr('%vv=['+%str(%kk)+']',env=exec_context,errcatch=%t);
	if %ierr==%f then %nok=-%kk;break,end
	%vv=%H.%vv;
	if type(%vv)>2 then %nok=-%kk,break,end
	%sz=%typ(2*%kk);if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	%ssz=string(%sz(1))
	%nv=prod(size(%vv))
	if %sz(1)>=0 then 
	  if %nv<>%sz(1) then %nok=%kk,break,end,end
       case 'row'
	[%ierr,%H]=execstr('%vv=['+%str(%kk)+']',env=exec_context,errcatch=%t);
	if %ierr==%f then %nok=-%kk;break,end
	%vv=%H.%vv;
	if type(%vv,'string')<>"Mat" then %nok=-%kk,break,end
	%sz=%typ(2*%kk);if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	if %sz(1)<0 then
	  %ssz='1 x *'
	else
	  %ssz='1 x '+string(%sz(1))
	end
	[%mv,%nv]=size(%vv)
	if %mv<>1 then %nok=%kk,break,end,
	if %sz(1)>=0 then 
	  if %nv<>%sz(1) then %nok=%kk,break,end,end
       case 'col'
	[%ierr,%H]=execstr('%vv=['+%str(%kk)+']',env=exec_context,errcatch=%t);
	if %ierr==%f then %nok=-%kk;break,end      
	%vv=%H.%vv;
	if type(%vv,'string')<>"Mat" then %nok=-%kk,break,end
	%sz=%typ(2*%kk);if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	if %sz(1)<0 then
	  %ssz='* x 1'
	else
	  %ssz=string(%sz(1))+' x 1'
	end
	[%mv,%nv]=size(%vv)
	if %nv<>1 then %nok=%kk,break,end,
	if %sz(1)>=0 then 
	  if %mv<>%sz(1) then %nok=%kk,break,end,end
       case 'str'
	%sde=%str1(%kk)
	%spe=find(ascii(%str1(%kk))==10)
	%spe($+1)=length(%sde)+1
	%vv=m2s([]);%kk1=1
	for %kkk=1:size(%spe,'*')
	  %vv(%kkk,1)=part(%sde,%kk1:%spe(%kkk)-1)
	  %kk1=%spe(%kkk)+1
	end
	%sz=%typ(2*%kk);
	if type(%sz,'string')=='SMat' then %sz=evstr(%sz,exec_context),end
	%ssz=m2s(%sz(1),'%.0f'); // XXX string 
	%nv=prod(size(%vv))
	if %sz(1)>=0 then
	  if %nv<>%sz(1) then %nok=%kk,break,end,end
       case 'lis'
	[%ierr,%H]=execstr('%vv='+%str(%kk),env=exec_context,errcatch=%t);
	if %ierr==%f then %nok=-%kk;break,end      
	%vv=%H.%vv;
	if type(%vv,'string')<>'List' then %nok=-%kk,break,end
	%sz=%typ(2*%kk);if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	%ssz=string(%sz(1))
	%nv=size(%vv)
	if %sz(1)>=0 then 
	  if %nv<>%sz(1) then %nok=%kk,break,end,end
       case 'r  '
	[%ierr,%H]=execstr('%vv=['+%str(%kk)+']',env=exec_context,errcatch=%t);
	if %ierr==%f then %nok=-%kk;break,end 
	%vv=%H.%vv;
	if type(%vv,'string')<>'List' then %nok=-%kk,break,end
	if typeof(%vv)<>'rational' then %nok=-%kk,break,end
	%sz=%typ(2*%kk);if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
	[%mv,%nv]=size(%vv(2))
	%ssz=string(%sz(1))+' x '+string(%sz(2))
	if %mv*%nv==0 then
	  if  %sz(1)>=0&%sz(2)>=0&%sz(1)*%sz(2)<>0 then %nok=%kk,break,end
	else
	  if %sz(1)>=0 then 
	    if %mv<>%sz(1) then %nok=%kk,break,end,
	  end
	  if %sz(2)>=0 then 
	    if %nv<>%sz(2) then %nok=%kk,break,end,end
	end
      else
	error('type non gere :'+%typ(2*%kk-1))
      end
      execstr('%'+m2s(%kk,'%.0f')+'=%vv'); // string 
    end
    if %nok>0 then 
      x_message(['answer given for  '+%labels(%nok);
		 'has invalid dimension: ';
		 'waiting for dimension  '+%ssz])
      %ini=%str
    elseif %nok<0 then
      if %ierr then
	x_message(['answer given for  '+%labels(-%nok);
		   'has incorrect type :'+ %typ(-2*%nok-1)])
      else
	x_message(['answer given for  '+%labels(-%nok);
		   'is incorrect see error message in scilab window'])
      end
      %ini=%str
    else
      break
    end 
  end
  if nargout==%nn+2 then
    execstr('%'+string(nargout-1)+'=%str')
  end
endfunction

function ok=check_dims(val,dims)
// check that dims and check_dims coincide 
  if type(dims,'string') == 'SMat' then execstr('dims='+dims); end 
  if size(dims,0)== 1 then 
    if dims== -1 then ok=%t; return; end 
    if dims<>size(val,0) then ok=%f; return ; end 
    ok=%t;  return ;
  end
  if dims==size(val) then ok=%t; else ok=%f ; end 
endfunction

function [y,err]=evstr(str, exec_context)
  if nargin < 2 then 
    exec_context = hash_create(10);
  end
  [m,n]=size(str);
  if m*n == 1 then 
    y=[];
    [ok,H]=execstr('y='+str,env=exec_context,errcatch=%t);
    err= ~ok;
    if ok then   y=H.y; else y=[];end 
  else
    A_evstr=zeros_new(m,n);
    err=%t;
    for i=1:m;
      for j=1:n,
	[ok,H]=execstr('%rep='+str(i,j),env=exec_context,errcatch=%t);
	err=err & ok;
	if ok then 
	  A_evstr(i,j)=H.%rep;
	end
      end
    end
    err = ~err;
    y=A_evstr;
  end
endfunction
