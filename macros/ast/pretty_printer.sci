function txt=pretty_printer(fname,target="term",color=%t, columns=90, outfile="")
  // pretty print the contents of a file with various possible targets
  
  S=getfile(fname);
  ast=parse(S);
  
  target_name = file('rootname',fname);
  select target 
    case "term" then 
      target_name = target_name + "_pp" + file('extension',fname);
      S=ast.sprint[columns=columns];
    case "html" then 
      target_name = target_name + ".html";
      S=ast.sprint[target="html",color=color, columns=columns];
    case "gtk" then 
      target_name = target_name + ".txt";
      S=ast.sprint[target="gtk",color=color, columns=columns];
    case "tex4ht" then
      // This target is no more usefull since
      // latex can do the same 
      target_name = target_name + ".tex";
      S=ast.sprint[target="html",color=color, columns=columns];
      // to be protected for tex4ht
      S=strsubst(S,"\\","&#92;");
      S=strsubst(S,"%","&#37;");	
      S=strsubst(S,"#","\#");
      S=strsubst(S,"~","&#126;");
      S=['\HCode{';
	 S+ "\Hnewline";
	 '}'];
    case "latex" then 
      target_name = target_name + ".tex";
      S=ast.sprint[target="latex",color=color, columns=columns];
      S=['\\begin{Verbatim}[commandchars=\\\\\\{\\}]';S;'\\end{Verbatim}'];
    else
      printf("Warning: unknown target ""%s""\n",target);
    return 
  end
  if type(outfile,'short')=='s' && length(outfile)<>0 then
    target_name=outfile;
  end
  txt = S;
  if nargout == 1 then 
    return;
  else
    F=fopen(target_name,mode="w");
    F.put_smatrix[S];
    F.close[]
  end
endfunction

