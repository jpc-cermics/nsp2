function pretty_printer(fname,target="nsp")

  function S1=latin1_letters_to_html(S)
    table = [ "�","&Agrave;";
	      "�","&Egrave;";
	      "�","&Igrave;";
	      "�","&Ograve;";
	      "�","&Ugrave;";
	      "�","&agrave;";
	      "�","&egrave;";
	      "�","&igrave;";
	      "�","&ograve;";
	      "�","&ugrave;";
	      "�","&Aacute;";
	      "�","&Eacute;";
	      "�","&Iacute;";
	      "�","&Oacute;";
	      "�","&Uacute;";
	      "�","&Yacute;";
	      "�","&aacute;";
	      "�","&eacute;";
	      "�","&iacute;";
	      "�","&oacute;";
	      "�","&uacute;";
	      "�","&yacute;";
	      "�","&Acirc;";
	      "�","&Ecirc;";
	      "�","&Icirc;";
	      "�","&Ocirc;";
	      "�","&Ucirc;";
	      "�","&acirc;";
	      "�","&ecirc;";
	      "�","&icirc;";
	      "�","&ocirc;";
	      "�","&ucirc;";
	      "�","&Atilde;";
	      "�","&Ntilde;";
	      "�","&Otilde;";
	      "�","&atilde;";
	      "�","&ntilde;";
	      "�","&otilde;";
	      "�","&Auml;";
	      "�","&Euml;";
	      "�","&Iuml;";
	      "�","&Ouml;";
	      "�","&Uuml;"];
    S1=strsubst(S,table(:,1),table(:,2));
  endfunction
    
  S=getfile(fname);
  S=stripblanks(S);
  I=find( S == "");
  S(I)= "//#@#@#";
  ast=parse(S);
  
  target_name = file('rootname',fname);
  select target 
   case "nsp" then 
    target_name = target_name + "_pp" + file('extension',fname);
    S=ast.sprint[];
    S=strsubst(S,"//#@#@#","");
   case "html" then 
    target_name = target_name + ".html";
    S=ast.sprint[html=%t];
    S=strsubst(S,"//#@#@#","");
   case "tex4ht" then 
    target_name = target_name + ".tex";
    S=ast.sprint[html=%t];
    S=strsubst(S,"//#@#@#","");
    // to be protected for tex4ht
    S=strsubst(S,"\","&#92;");
    S=strsubst(S,"%","&#37;");	
    S=strsubst(S,"#","\#");	
    S=latin1_letters_to_html(S);
    S=['\HCode{';
       S+ "\Hnewline";
       '}'];
  else
    printf("Warning: unknown target ""%s""\n",target);
    return 
  end
  F=fopen(target_name,mode="w");
  F.put_smatrix[S];
  F.close[]
endfunction
  