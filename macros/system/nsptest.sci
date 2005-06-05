function y=nsptest(fname)
// run a script in diary mode 
// Copyright Cermics/Enpc Jean-Philippe Chancelier 
// 
  exten= file('extension',fname);
  if exten == "" then 
    fname_dia = fname+'.dia';
  else
    fname_dia = strsubst(fname,exten,'.dia');
    if fname_dia == fname then fname_dia = fname_dia+'1';end 
  end
  fname_ref = fname_dia+'.ref';
  diary(fname_dia,%f);
  // diary without standard output 
  y=exec(fname,display=%t,echo =%t,errcatch=%t,pausecatch=%t)
  diary(); 
  if y == %f then 
    printf("Error: test %s failed\n",fname);
    printf("%s",lasterror());
    return;
  end 
  dia_file= fopen(fname_dia);
  dia_ = dia_file.get_smatrix[];
  dia_file.close[];
  if file('exists',fname_ref) then 
    ref_file= fopen(fname_ref)
    ref_ = ref_file.get_smatrix[];
    ref_file.close[];
    if ref_ <> dia_ then 
      printf("diff %s %s failed\n",fname_dia,fname_ref);
      y=%f 
      return 
    end
  end
  y=%t
  printf("test %s passed\n",fname_dia,fname_ref);
endfunction
