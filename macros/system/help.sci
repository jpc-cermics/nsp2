function nsp_help(topic)
  function h=man_hash_table()
    idx=getenv('NSP')+'/man/html/generated/manual.4dx";
    F=fopen(idx,mode="r");
    Str=F.get_smatrix[];
    F.close[];
    h=hcreate(size(Str,'*'));
    // remove \indexentry{
    for i=1:size(Str,'*')
      Str(i)=regsub(Str(i),'^[^{]*{','h.');
      // remove enclosing \LNK{xx} -> xx 
      Str(i)=regsub(Str(i),'\|LNK{([^{]*)}','=""\\1""');
      // remove trailing 
      Str(i)=regsub(Str(i),'{[^$]*}','');
    end
    execstr(Str);
  endfunction
  
  global %help_table;
  if %help_table ==[] then 
    %help_table = man_hash_table()
  end
  if %help_table.iskey[topic] then 
    help(getenv('NSP')+'/man/html/generated/'+%help_table(topic));
  else
    printf('No man for %s\n",topic);
  end
endfunction
  



