function load_toolbox()
  __tb=get_toolbox();
  if ~isempty(__tb) then 
    exec(__tb);
  end
  clear __tb;
endfunction
  
function tb=get_toolbox()
  tbxes=glob('NSP/toolboxes/*/loader.sce');
  names = strsubst(tbxes,getenv('NSP')+'/toolboxes/','');
  names = strsubst(names,'/loader.sce','');
  val=x_choose(names,'Choose a toolbox');
  if val > 0 then 
    tb=tbxes(val);
  else
    tb=[];
  end
endfunction

