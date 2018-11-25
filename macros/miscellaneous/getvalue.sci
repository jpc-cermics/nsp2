function [ok,%1,%2,%3,%4,%5,%6,%7,%8,%9,%10,%11,%12,%13,%14,%15,%16,%17,%18,%19,%20]=...
	 getvalue(%desc,%labels,%typ,%ini)
  // getvalue is limited with respect to the number of outputs
  // use getvalue_list instead
  function L=xfun(desc,list,flag) L=x_choices(desc,list,flag);endfunction
  [ok,%L,%L2]= getvalue_internal_list(%desc,%labels,%typ,%ini,use_dialog=%t,ch_fun=xfun);
  str = '['+ catenate('%'+m2s(1:length(%L))',sep=',')+']';
  for i=1:length(%L) do execstr( '%'+m2s(i,"%.0f")+ '=%L(i)'); end
  i =length(%L)+1; execstr( '%'+m2s(i,"%.0f")+ '=%L2;');
  if nargout > length(%L)+2 then
    for i=length(%L)+2:nargout-1 do execstr( '%'+m2s(i,"%.0f")+ '=[];'); end
  end
endfunction

function [ok,L,exprs]=getvalue_list(%desc,%labels,%typ,%ini)
  function L=xfun(desc,list,flag) L=x_choices(desc,list,flag);endfunction
  [ok,L,exprs] = getvalue_internal_list(%desc,%labels,%typ,%ini,use_dialog=%t,ch_fun=xfun);
endfunction

function [ok,%L,%exprs]=getvalue_internal_list(%desc,%labels,%typ,%ini,use_dialog=%t,ch_fun=xfun)
  // getvalues - launch dialogs for acquiring data 
  //   then check that the entered values have the correct types 
  //   and evaluate them in the context environment.
  //   nsp syntax is to be used for entering responses. For matrices 
  //   and vectors getvalues automatically adds [ ] around the given
  //   response when evaluating the expression. 
  //
  // labels=['magnitude';'frequency';'phase    '];
  // [ampl,freq,ph]=getvalue('define sine signal',labels,..
  //            list('vec',1,'vec',1,'vec',1),['0.85';'10^2';'%pi/3'])
  //
  // syntax: 
  // [ok,%1,..,%11]=getvalue(desc,labels,typ,ini)
  // Parameters:
  //  desc    : column vector of strings giving the dialog general comment 
  //  labels  : n column vector of strings, labels(i) is the label of 
  //            the ith required value
  //  typ     : list(typ1,dim1,..,typn,dimn)
  //            typi : defines the type of the ith required value
  //                   if may have the following values:
  //                   'mat' : stands for matrix of scalars 
  //                   'col' : stands for column vector of scalars
  //                   'row' : stands for row vector of scalars
  //                   'vec' : stands for vector of scalars
  //                   'str' : stands for vector of strings
  //                   'lis' : stands for list
  //                   'pol' : stands for polynomials
  //                   'r'   : stands for rational
  //                   'combo': stands for a choice in strings 
  //                          'combo',['a','b','c'];
  //            dimi : defines the size of the ith required value
  //                   it must be 
  //                    - an integer or a 2-vector of integers (strictly
  //                    negative value stands for arbitrary dimension)
  //                    - an evaluatable character string 
  //  ini     : n column vector of strings, ini(i) gives the suggested
  //            response for the ith required value
  //  ok      : boolean ,%t if ok button pressed, %f if cancel button pressed
  //  xi      : contains the ith required value if ok==%t
  // Copyright INRIA
  // 
  // 
  // get the context value 

  function ok=check_dims(val,dims)
    // check that dims and check_dims coincide 
    // unused 
    ok=%t;
    if type(val,'short')== 'l' then m=size(val);n=1; else [m,n]=size(val);end;
    if type(dims,'short')<>'m' && ~or(length(dims)==[0,1,2]) then 
      ok=%f;
      return;
    end
    if isempty(dims) then ok=%t; return;end 
    if size(dims,0)== 1 then 
      ok = dims < 0 || dims.equal[size(val,0)];
      return;
    end
    if max(dims) < 0 then 
      ok = %t;
      return;
    end
    if dims(1) < 0 then 
      ok = dims(2).equal[n]
      return;
    end
    if dims(2) < 0 then 
      ok = dims(1).equal[m]
      return;
    end
    ok = dims.equal[[m,n]]; 
  endfunction
  
  function str=string_dims(dims)
    // check that dims and check_dims coincide 
    // unused 
    str = '[' + catenate(strsubst(string(dims),'-1','*'),sep=',')+']';
  endfunction

  function num=std_message(strings ,buttons)
    if nargin == 1 then 
      num=1; x_message(strings)  
    else 
      num=x_message(strings ,buttons)
    end 
  endfunction
  
  // the message function can be changed by callers
  if ~exists('message','nsp-function') then 
    message=std_message;
  end
  
  function error_size(str,sz)
    if length(str) > 200 then str = part(str,1:200)+'... (truncated)';end
    message(['Answer given for '+str+' entry';
	     'has invalid dimensions. ';
	     'Expecting size to be: '+sz]);
  endfunction

  function [%vv,ok]=check_eval(label,str,typ)
    ok=%f;%vv=[];
    [ierr,%H]=execstr('%vv=[' + str + ']',env=exec_context, errcatch=%t);
    if ierr==%f then 
      if length(str) > 200 then str = part(str,1:200)+'... (truncated)';end
      message(['Answer given for '+label+' is wrong and';
	       'cannot be evaluated:'; 'v=[' + str + ']']);
      lasterror();
      return;
    end 
    %vv=%H.%vv;
    if ~isempty(typ) && and(type(%vv,'string')<>typ) then
      message(['Entry '+label+ ' has incorrect type';
	       'Expecting a ' + catenate(typ,sep= ', or ')]);
      return;
    end
    ok=%t;
  endfunction
  // evaluation 
  if ~exists('%scicos_context') then 
    exec_context = hash_create(10);
  else
    exec_context = %scicos_context;
  end
  // check arguments 
  %nn=size(%labels,0);
  if size(%typ,0)<>2*%nn then
    error('%typ : list(''type'',[sizes],...)')
  end

  %L = list();
  
  // %ini 
  if nargin < 4  then %ini=smat_create(%nn,1);end
  
  if type(%ini,'short')== 'l' then 
    ini1=m2s([]);
    for i=1:length(%ini)
      ini1.concatd[catenate(%ini(i))];
    end
    %ini = ini1;
  end
  
  ok=%t
  
  while %t do
    // acquire data through dialog 
    if use_dialog then 
      %str1=getvalue_dialog(%desc,%labels,%typ,%ini,ch_fun)
    else
      %str1= %ini;
    end
    if isempty(%str1) then ok=%f,%str=[];break,end
    %str=strsubst(%str1,'\n',';');
    
    ok=%t;
    // check the types of returned values
    for %kk=1:%nn
      // expected size.
      %sz=%typ(2*%kk); 
      if type(%sz,'string')=="SMat" then %sz=evstr(%sz,exec_context),end
      ok = %t;
      // switch according to types 
      select part(%typ(2*%kk-1),1:3)
	case 'com' 
	  //---- a combo choice 
	  %vv= %str(%kk);
	case 'mat'
	  //---- matrix 
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),['Mat','IMat']); 
	  if ~ok then break;end
	  ok = check_dims(%vv,%sz);
	  if ~ok then  error_size(%labels(%kk),string_dims(%sz));break;end 
	case 'row'
	  //---- row vector
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),['Mat','IMat']); 
	  if ~ok then break;end
	  %nv= prod(%sz);
	  if %nv <= 0 then 
	    ok = check_dims(%vv,[%nv,1]) || check_dims(%vv,[0,0]);
	  else
	    ok = check_dims(%vv,[%nv,1]);
	  end
	  if ~ok then  error_size(%labels(%kk),string_dims([%nv,1]));break;end
	case 'col'
	  //---- column vector 
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),['Mat','IMat']); 
	  if ~ok then break;end
	  %nv= prod(%sz);
	  if %nv <= 0 then 
	    ok = check_dims(%vv,[1,%nv]) || check_dims(%vv,[0,0]);
	  else
	    ok = check_dims(%vv,[1,%nv]);
	  end
	  if ~ok then error_size(%labels(%kk),string_dims([1,%nv]));break;end
	case 'vec'
	  //---- vector 
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),['Mat','IMat']); 
	  if ~ok then break;end
	  if %sz.equal[-1] then 
	    // Temporary: we accept 'vec',-1 to be equivalent to 'mat',-1
	    // i.e we do not force answer to be a matrix. 
	    ok = check_dims(%vv,%sz);
	  else 
	    %nv=prod(%sz);
	    ok = check_dims(%vv,[1,%nv]) || check_dims(%vv,[%nv,1]);
	    if %nv <= 0 then 
	      ok = ok || check_dims(%vv,[0,0]);
	    end
	  end
	  if ~ok then error_size(%labels(%kk),string_dims(%nv));break;end 
	case 'pol'
	  //---- polynom 
	  // for polynoms we add z and s in the context 
	  // we accept a polynom or a matrix 
	  // conversion is to be made in blocks.
	  exec_context.z=poly(0,'z');
	  exec_context.s=poly(0,'s');
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),['PMat','Mat']); 
	  if ~ok then break;end
	  ok = check_dims(%vv,%sz);
	  if ~ok then  error_size(%labels(%kk),string_dims(%sz));break;end 
	case 'gen'
	  // 
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),m2s([])); 
	  if ~ok then break;end
	  ok = check_dims(%vv,%sz);
	  if ~ok then  error_size(%labels(%kk),string_dims(%sz));break;end 
	case 'str'
	  //---- strings 
	  str=strsubst(%str1(%kk),'\\n','\n');
	  %vv = split(str,sep='\n')';
	  ok = check_dims(%vv,%sz);
	  if ~ok then  error_size(%labels(%kk),string_dims(%sz));break;end 
	case 'lis'
	  //---- a list 
	  [%vv,ok]=check_eval(%labels(%kk),%str(%kk),'List'); 
	  if ~ok then break;end
	  %nsz= prod(%sz);
	  ok = %nsz < 0 || length(%vv)== abs(%nsz);
	  if ~ok then 
	    error_size(%labels(%kk),string(abs(%nsz)));break;
	  end 
	case 'r  '
	  //---- a rational (to be done).
	  ok=%f 
	  printf('rational is to be done\n");
      else
	error('unknow type in getvalue :'+%typ(2*%kk-1))
      end
      execstr('%L(' + m2s(%kk,'%.0f') + ')=%vv' ); // string
      // we also need that %i exists for cross size checking 
      execstr('%'+m2s(%kk,'%.0f')+'=%vv'); // string 
    end
    // do not loop if not using a dialog 
    if ~use_dialog then break;end 
    // break or continue
    if ok then; break;end 
    %ini=%str;
  end
  execstr('%exprs=%str;')
endfunction

function str=getvalue_dialog(desc,labels,typ,ini,ch_fun)
  if %f then 
    // old version 
    str = x_mdialog(des,labels,ini);
  else
    // new version with extensions 
    Li=list();
    for i=1:size(labels,'*')
      // switch according to types 
      if typ(2*i-1)=='combo' then 
	k=find(ini(i)==typ(2*i));
	if isempty(k) then k=1;end 
	Li(i)= list('combo',labels(i),k,typ(2*i));
      else
	Li(i)= list('entry',labels(i),1,ini(i));
      end
    end
    L= ch_fun(desc,Li,%t); 
    str=m2s([]);
    if isempty(L) then return;end
    for i=1:size(labels,'*')
      if typ(2*i-1)=='combo' then 
	str.concatd[typ(2*i)(L(i))];
      else
	str.concatd[L(i)];
      end
    end
  end
endfunction
