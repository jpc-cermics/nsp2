function scihelp(topic)
// show the manual for a scicoslab function 
// the index file is obtained from nsp2_dev/man-scilab
// can be used to find the html file for the manual of topic.
  function h=man_hash_table()
    idx=getenv('NSP')+'/man/scicoslab.4dx';
    F=fopen(idx,mode="r");
    Str=F.get_smatrix[];
    F.close[];
    h=hash_create(size(Str,'*'));
    // remove \indexentry{
    Str=regsub(Str,'^[^{]*{','h(""');
    // remove enclosing \LNK{xx} -> xx 
    Str=regsub(Str,'\|LNK{([^{]*)}','"")=""\\1""');
    // remove trailing 
    Str=regsub(Str,'{[^$]*}','');
    // 
    Str=strsubst(Str,'\_','_');
    ok=execstr(Str,errcatch=%t);
    if ~ok then 
      error('Failed to insert manual index');
      return;
    end
  endfunction
  global %sci_help_table;
  if isempty(%sci_help_table) then 
    %sci_help_table = man_hash_table()
  end
  if %sci_help_table.iskey[topic] then 
    help('http://cermics.enpc.fr/~jpc/nsp-tiddly/oldman/'+%sci_help_table(topic));
  else
    printf('No man for %s in scicoslab\n",topic);
  end
endfunction
