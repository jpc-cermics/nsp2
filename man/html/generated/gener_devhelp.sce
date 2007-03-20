
Res=fopen("nsp.devhelp",mode="w");
Res.putstr["<?xml version=""1.0"" encoding=""utf-8"" standalone=""no""?>"];
Res.putstr["<!DOCTYPE book PUBLIC ""-//W3C//DTD HTML 4.01 Transitional//EN"" """">"];
Res.putstr["<book xmlns=""http://www.devhelp.net/book"" title=""Nsp manual"" link=""index.html"" author="""" name=""nspdoc"">"];
Res.putstr["  <chapters>"];
chaps=glob("manualch*");
for i=1:size(chaps,'*') 
  F=fopen(chaps(i));
  chap_str=F.get_lines[10];
  F.close[];
  I=strstr(chap_str,"<title>");
  I1=find(I<>0);
  if ~isempty(I1) then 
    chap_str=chap_str(I1);
    chap_str=regsub(chap_str,"<head><title>[0-9]*([^{]*)</title>","\\1");
    Res.printf["    <sub name=""%s"" link=""%s""/>\n",chap_str,chaps(i)];
  end
end
Res.putstr["  </chapters>"];
Res.putstr["  <functions>\n"];
F=fopen("manual.4dx");
while %t 
  str=F.get_lines[1];
  if isempty(str) then break;end;
  name=regsub(str,"^[^{]*{([^{]*)\\|LNK{([^{]*)}{[^$]*}","\\1");
  fname=regsub(str,"^[^{]*{([^{]*)\\|LNK{([^{]*)}{[^$]*}","\\2");
  name = strsubst(name,"\_","_");
  str1=sprintf("    <function name=""%s"" link=""%s""/>\n",name,fname);
  Res.putstr[str1];
end
Res.putstr["  <functions>\n"];
Res.putstr["</book>"];
F.close[]
Res.close[];
