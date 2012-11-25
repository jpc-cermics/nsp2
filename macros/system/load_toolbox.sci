function load_toolbox()
// choose a toolbox 
  tbxes=glob('NSP/toolboxes/*/loader.sce');
  names = strsubst(tbxes,getenv('NSP')+'/toolboxes/','');
  names = strsubst(names,'/loader.sce','');
  val=x_choose(names,'Choose a toolbox');
  if val > 0 then 
    exec(tbxes(val));
  end
endfunction
  
  
