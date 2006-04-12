function nsp_help(topic)
// moved in the help C-source 
// can be used to find the html file for the manual of topic.
  function h=man_hash_table()
    idx=getenv('NSP')+'/man/html/generated/manual.4dx";
    F=fopen(idx,mode="r");
    Str=F.get_smatrix[];
    F.close[];
    h=hcreate(size(Str,'*'));
    // remove \indexentry{
    Str=regsub(Str,'^[^{]*{','h.');
    // remove enclosing \LNK{xx} -> xx 
    Str=regsub(Str,'\|LNK{([^{]*)}','=""\\1""');
    // remove trailing 
    Str=regsub(Str,'{[^$]*}','');
    execstr(Str);
  endfunction
  
  global %help_table;
  if %help_table ==[] then 
    %help_table = man_hash_table()
  end
  if %help_table.iskey[topic] then 
    printf("%s\n",getenv('NSP')+'/man/html/generated/'+%help_table(topic));
  else
    printf('No man for %s\n",topic);
  end
endfunction
  



