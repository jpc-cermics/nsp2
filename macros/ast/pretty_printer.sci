function txt=pretty_printer(fname,target="nsp")
  
  function S1=latin1_letters_to_html(S)
    table = [ "À","&Agrave;";
	      "È","&Egrave;";
	      "Ì","&Igrave;";
	      "Ò","&Ograve;";
	      "Ù","&Ugrave;";
	      "à","&agrave;";
	      "è","&egrave;";
	      "ì","&igrave;";
	      "ò","&ograve;";
	      "ù","&ugrave;";
	      "Á","&Aacute;";
	      "É","&Eacute;";
	      "Í","&Iacute;";
	      "Ó","&Oacute;";
	      "Ú","&Uacute;";
	      "Ý","&Yacute;";
	      "á","&aacute;";
	      "é","&eacute;";
	      "í","&iacute;";
	      "ó","&oacute;";
	      "ú","&uacute;";
	      "ý","&yacute;";
	      "Â","&Acirc;";
	      "Ê","&Ecirc;";
	      "Î","&Icirc;";
	      "Ô","&Ocirc;";
	      "Û","&Ucirc;";
	      "â","&acirc;";
	      "ê","&ecirc;";
	      "î","&icirc;";
	      "ô","&ocirc;";
	      "û","&ucirc;";
	      "Ã","&Atilde;";
	      "Ñ","&Ntilde;";
	      "Õ","&Otilde;";
	      "ã","&atilde;";
	      "ñ","&ntilde;";
	      "õ","&otilde;";
	      "Ä","&Auml;";
	      "Ë","&Euml;";
	      "Ï","&Iuml;";
	      "Ö","&Ouml;";
	      "Ü","&Uuml;"];
    S1=strsubst(S,table(:,1),table(:,2));
  endfunction
    
  S=getfile(fname);
  S=stripblanks(S);
  // S1=m2s([]);for i =1:size(S,'*'), S1($+1)=S(i);S1($+1)="";end S=S1(:);
  I=find( S == "");
  S(I)= "//#@#@#";
  // F=fopen("pipo.sci",mode="w");F.put_smatrix[S]; F.close[]
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
   case "gtk" then 
    target_name = target_name + ".txt";
    S=ast.sprint[gtk=%t];
    S=strsubst(S,"//#@#@#","");
   case "tex4ht" then 
    target_name = target_name + ".tex";
    S=ast.sprint[html=%t];
    S=strsubst(S,"//#@#@#","");
    // to be protected for tex4ht
    S=strsubst(S,"\","&#92;");
    S=strsubst(S,"%","&#37;");	
    S=strsubst(S,"#","\#");
    S=strsubst(S,"~","&#126;");
    // S=latin1_letters_to_html(S);
    S=['\HCode{';
       S+ "\Hnewline";
       '}'];
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
  
