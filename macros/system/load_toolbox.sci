
function load_toolbox(name)
  
  function tb=get_toolbox(names,tbxes)
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
    tb=get_toolbox(names,tbxes);
  else
    tb= find(strstr(names,name)<>0);
    tb= tbxes(tb);
  end

  if size(tb,'*')== 1 then 
    [ok,H]=exec(tb,errcatch=%t);
    if ~ok then 
      error(catenate(lasterror()));
    else
      resume(H(:));
    end
  else
    if size(tb,'*')== 0 then 
      error("Error: cannot find a toolbox named "+name+" !")
    else
      error("Error: The name "+name+" is ambiguous");
    end
  end
endfunction
