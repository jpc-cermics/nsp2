function extractman(name)

  function txt=verbatim(path)
    F=fopen(path,mode="r");
    S=F.get_lines[-1];
    F.close[];
    I=strstr(S,"begin{mintednsp}")
    I1=find(I<>0);
    J=strstr(S,"end{mintednsp}")
    J1=find(J<>0);
    if size(I1,'*')<>size(J1,'*') then 
      error("unclosed mintednsp\n");
    end
    txt=m2s([])
    for i=1:size(I1,'*');
      txt.concatd[S((I1(i)+1):(J1(i)-1),:)];
    end
  endfunction

  name=strsubst(name,'man-','');
  
  path = sprintf("../man/src/%s/*.tex",name);
  files=glob(path);
  txt=m2s([])
  for i=1:size(files,'*')
    txt.concatd[sprintf("// File: %s\n",files(i))];
    part=verbatim(files(i));
    txt.concatd[part(:)];
  end
  putfile(sprintf("man-%s.tst",name),txt);
endfunction
