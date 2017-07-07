function txt=pretty_printer(fname,target="nsp",color=%f)
  
  S=getfile(fname);
  // S=stripblanks(S);
  // S1=m2s([]);for i =1:size(S,'*'), S1($+1)=S(i);S1($+1)="";end S=S1(:);
  // I=find( S == "");
  // S(I)= "//#@#@#";
  // F=fopen("pipo.sci",mode="w");F.put_smatrix[S]; F.close[]
  ast=parse(S);
  
  target_name = file('rootname',fname);
  select target 
   case "nsp" then 
    target_name = target_name + "_pp" + file('extension',fname);
    S=ast.sprint[];
    // S=strsubst(S,"//#@#@#","");
   case "html" then 
    target_name = target_name + ".html";
    S=ast.sprint[target="html",color=color];
    // S=strsubst(S,"//#@#@#","");
   case "gtk" then 
    target_name = target_name + ".txt";
    S=ast.sprint[target="gtk",color=color];
    // S=strsubst(S,"//#@#@#","");
   case "tex4ht" then 
    target_name = target_name + ".tex";
    S=ast.sprint[target="html",color=color];
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
    S=ast.sprint[target="latex",color=color];
    S=strsubst(S,"\\","\\nspb{}");
    S=strsubst(S,"@@","\\");
    S=strsubst(S,"%","\\nspp{}");	
    S=strsubst(S,"#","\\nspd{}");
    S=strsubst(S,"~","\\nspt{}");
    S=['\\begin{Verbatim}';S;'\\end{Verbatim}'];
    
  else
    printf("Warning: unknown target ""%s""\n",target);
    return 
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
  
