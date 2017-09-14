function load_toolbox(name,register=%t)

  function register_toolbox(name)
    if ~file('exists','~/.nsp') then 
      file("mkdir","~/.nsp");
      F=fopen("~/.nsp/startup.sce",mode="w");
      F.close[];
    else
      if ~file('exists','~/.nsp/startup.sce') then 
	F=fopen("~/.nsp/startup.sce",mode="w");
	F.close[];
      end
    end
    F=fopen("~/.nsp/startup.sce",mode="r");
    S=F.get_lines[-1];
    F.close[];
    str= sprintf('load_toolbox(''%s'');',name);
    I=strstr(S,str);
    I=find(I==1);
    added=%f;
    if isempty(I) then 
      printf("Automatic loading of toolbox %s is inserted in your startup"+...
	     " file ~/.nsp/startup.sce\n",name);
      if strstr('modnum_43',name)<>0 || strstr('coselica-0.4.8-nsp',name)<>0 then 
	added = %t;
	S=[S;sprintf('load_toolbox(''%s'');','scicos-4.4')];
      end
      S=[S;str];
      F=fopen("~/.nsp/startup.sce",mode="w");
      F.put_smatrix[S];
      F.close[];
    end
    if added then 
      // scicos was added check for duplicates 
      str= sprintf('load_toolbox(''%s'');','scicos-4.4');
      F=fopen("~/.nsp/startup.sce",mode="r");
      S=F.get_lines[-1];
      F.close[];
      I=strstr(S,str);
      I=find(I==1);
      if size(I,'*')<>1 then 
	I=I(2:$);
	S(I)=[];// '//'+S(I);
      end
      F=fopen("~/.nsp/startup.sce",mode="w");
      F.put_smatrix[S];
      F.close[];
    end
  endfunction
  
  function [tb,val]=get_toolbox(names,tbxes)
    val=x_choose(names,'Choose a toolbox');
    if val > 0 then 
      tb=tbxes(val);
    else
    tb=[];
    end
  endfunction

  function [names,tbxes]=toolboxes()
    tbxes=glob('NSP/toolboxes/*/loader.sce');
    names = strsubst(tbxes,getenv('NSP')+'/toolboxes/','');
    names = strsubst(names,'/loader.sce','');
  endfunction
  
  [names,tbxes]=toolboxes()
  if nargin <= 0 then 
    [tb,val]=get_toolbox(names,tbxes);
    if size(tb,'*')==0 then
      return
    end
    full_name = names(val);
  else
    tb= find(strstr(names,name)<>0);
    full_name = names(tb);
    tb= tbxes(tb);
  end

  if size(tb,'*')== 1 then 
    [ok,H]=exec(tb,errcatch=%t);
    if ~ok then 
      error(catenate(lasterror()));
    else
      if register then 
	register_toolbox(full_name);
	//      val = x_message(sprintf('Do you  want to load %s at startup ?',full_name),["gtk-yes","gtk-no"]);
	// if val == 1 then  end
	if size(H,1) > 0 then 
	  // resume could be changed to do nothing when called with 0-args
	  resume(H(:));
	end
      end
    end
  else
    if size(tb,'*')== 0 then 
      printf("%s toolbox not loaded (not found in toolboxes)\n",name)
    else
      error("Error: The name "+name+" is ambiguous");
    end
  end
endfunction

