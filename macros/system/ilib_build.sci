function ilib_build(ilib_name,table,files,libs,...
		    makename='Makelib',ldflags="",cflags="",fflags="",verbose=%t,compile=%t)
  // generate files and make a shared library 
  // Copyright Enpc 
  // generate the gateway file
  if verbose then printf('   generate a gateway file\n');end
  ilib_gen_gateway(ilib_name,table)
  // generate a loader file
  if verbose then printf('   generate a loader file\n');end
  ilib_gen_loader(ilib_name,table,libs);
  // generate a Makefile
  if verbose then printf('   generate a Makefile: %s\n',makename);end
  if isempty(table) then with_gateway=%f ; else with_gateway=%t ;end 
  ilib_gen_Make(ilib_name,table,files,libs,makename,with_gateway,ldflags,cflags,fflags);
  if verbose then printf('   running the makefile\n');end
  // a little trick here: all the previous functions generate files
  // using the current_exec_dir which is not the case for ilib_compile
  // thus we change the path of makefile to compile in the proper
  // directory.
  if file('pathtype',makename)=='relative' then 
    makename = file('join',[file('split',get_current_exec_dir(absolute=%t));makename]);
  end
  if compile then 
    if verbose then printf('   running the makefile: %s\n',makename);end
    ilib_compile(ilib_name,makename,files,verbose=verbose);
  end
endfunction

function ilib_gen_gateway(name,tables)
// Copyright Enpc 
//------------------------------------
// generate an interface gateway named name
// from table table taking into account 
// attributes of function i.e mex fmex or std interface 
//
// The gateway is generated in the directory given by the 
// dirname of name 
//------------------------------------
  
  function [gate,names,cast,declar,have_mex]=new_names(table) 
  // change names according to types 
    [mt,nt]=size(table);
    have_mex=%f;
    gate = smat_create(mt,1,"NULL");
    names= smat_create(mt,1," "); 
    cast = smat_create(mt,1,"");
    declar=smat_create(mt,1, "function");
    for i=1:mt 
      select table(i,3) 
       case 'cmex' then 
	have_mex=%t;
	declar(i)="mexfun";
	cast(i) = "(function *)";
	gate(i)="(function_wrapper *) nsp_mex_wrapper"; 
	names(i) = "mex_" + table(i,2)
       case 'fmex' then 
	have_mex=%t;
	declar(i)="mexfun";
	cast(i) = "(function *)";
	gate(i)="(function_wrapper *) nsp_mex_wrapper"; 
	names(i) = "C2F(mex" + table(i,2) + ")"
       case 'Fmex' then 
	have_mex=%t;
	declar(i)="mexfun";
	cast(i) = "(function *)";
	gate(i)="(function_wrapper *) nsp_mex_wrapper"; 
	names(i) = "C2F(mex" + table(i,2) + ")"
       case 'csci'  then 
	names(i) = table(i,2)
       case 'cnsp'  then 
	names(i) = table(i,2)
       case 'fsci'  then 
	names(i) = "C2F(" + table(i,2) + ")"
       case 'fnsp'  then 
	names(i) = "C2F(" + table(i,2) + ")"
       case 'direct'  then 
	names(i) = "C2F(" + table(i,2) + ")"
      else error("wrong interface type "+table(i,3)); 
      end 
    end 
  endfunction
  
  arg_name=name;
  name=file('tail',name);
  sufix=file('extension',name);
  name=strsubst(name,sufix,"");
  path=file('dirname',arg_name);
  
  if type(tables,'short')<>'l' then 
    if isempty(tables) then return;end 
    tables= list(tables)
  end
  
  L=length(tables); 
  
  for itable=1:L 
    // loop on a list of tables 
    if L<> 1 then 
      tname = name +m2s(itable,'%.f');
    else 
      tname = name ;
    end
    table = tables(itable);
    [mt,nt]=size(table);
    if nt==2 then col= "csci"; table = [table, col(ones_new(mt,1))];nt = 3 ; end 
    if nt<>3 then error('second argument has wrong size ');end 
    [gate,names,cast,declar,have_mex]=new_names(table); 
    mex_include='';
    if have_mex then mex_include = '#include ""mex/mex.h""';end
    t=['/* do not edit : generated by ilib_ */';
       '#include ""nsp/machine.h""';
       '#include ""nsp/interf.h""';
       mex_include;
       '';
       'extern ' + declar(:) + ' ' +  names(:) + ';';
       '';
       'static OpWrapTab '+tname+'_func[] = {';
       '  {""' + table(:,1) + '"",' + cast(:) + ' ' + names(:) + ',' + ...
       gate(:) + '},';
       '  {(char *) 0, NULL, NULL},';
       '};';
       'int '+tname+'_Interf (int i, Stack stack, int rhs, int opt, int lhs)';
       '{';
       '  if ( ' + tname + '_func[i].wrapper == NULL)';
       '     return (*(' + tname + '_func[i].fonc)) (stack, rhs, opt, lhs);';
       '  else ';
       '     return (*(' + tname + '_func[i].wrapper)) (stack, rhs, opt, lhs,'+tname+'_func[i].fonc);';
       '}';
       'void ' + tname + '_Interf_Info (int i, char **fname, function (**f))';
       '{';
       ' *fname = ' + tname + '_func[i].name;';
       ' *f = ' + tname + '_func[i].fonc;';
       '}'];
    
    fname= file('join',[path,tname+'.c']);
    gener=%t
    if file('exists',fname)
      // first chek if we already have a gateway 
      fd=fopen(fname,mode='r');
      t1=fd.get_smatrix[];
      fd.close[]
      if t1==t then gener=%f;end
    end
    if gener== %t then 
      // printf('\n');
      // need to regenerate a gateway
      fd=fopen(fname,mode='w');
      fd.put_smatrix[t];
      fd.close[];
    else
      // printf('(unchanged)\n');
    end
  end
endfunction


function ilib_gen_loader(name,tables,libs)
// Copyright Enpc 
//------------------------------------
// generate a loader file for gateway
  if nargin < 3 then libs=[];end 

  if type(tables,'short')<>'l' then 
    tables= list(tables)
  end
  L=length(tables); 

  for it=1:L 
    [mt,nt]=size(tables(it));
    if nt<>0 & nt<>3 & nt<>2 then error('second argument has wrong size ');end 
  end
  
  lib_suf= %shext ;

  fd=fopen('loader.sce',mode="w");
  fd.printf["// generated by builder.sce: Please do not edit this file\n"];
  fd.printf["// ------------------------------------------------------\n\n"];

  nl=size(libs,'*') 
  for i=1:nl 
    fd.printf["%s_path=file(''join'',[''.'',''%s%s'']);\n",name,libs(i),lib_suf];
    fd.printf["link(%s_path);\n",name];
  end 

  if L == 1 then 
    // direct call to addinter 
    table = tables(1);
    fd.printf["%s_path=file(''join'',[''.'',''%s%s'']);\n",name,name,lib_suf];
    fd.printf["addinter(%s_path,''%s'');\n",name,name];
  else
    // on link then a set of addinter 
    fd.printf["%s_path=file(''join'',[''.'',''%s%s'']);\n",name,name,lib_suf];
    fd.printf["ilib=link(%s_path);\n",name];
    for itable=1:L 
      // loop on a list of tables 
      table = tables(itable);
      fd.printf["addinter(ilib,''%s'');\n",name+m2s(itable,'%.f')];
    end
  end
  fd.close[];
endfunction

function Makename=ilib_gen_Make(name,tables,files,libs,makename,with_gateway,ldflags,cflags,fflags,flag)
// Copyright Enpc 
//------------------------------------
// generate a Makefile for gateway
  if nargin < 6 then with_gateway=%t,ldflags='',cflags='',fflags='';end
  if ~isempty(files) then 
    // we use .o for all objects 
    files=strsubst(strsubst(files,'.obj','') ,'.o','');
  end
  // change table if necessary 
  if type(tables,'short')<>'l' then 
    tables= list(tables)
  end
  L=length(tables); 
  for it = 1:L 
    table = tables(it);
    [mt,nt]=size(table);
    if nt==2 then col= "csci"; table = [table, col(ones_new(mt,1))];nt=3; end 
    if nt<>0 & nt<>3 then error('second argument has wrong size ');end 
    tables(it)=table;
  end
  if %win32  then
    // the target is win32 
    // check if we have a msvc compiler 
    [msvc,path,is64] = msvc_get_compiler();
    if msvc <> "unknown"  then
      Makename = makename+'.mak';
      ilib_gen_Make_win32(name,tables,files,libs,Makename,with_gateway,  ldflags,cflags,fflags);
    else
      // we assume that we are cross compiling or using cygwin 
      Makename = makename;
      ilib_gen_Make_unix(name,tables,files,libs,Makename,with_gateway, 	 ldflags,cflags,fflags)
    end
    // extra generation for testing
    //Makenamep = makename+'.lcc'
    //ilib_gen_Make_lcc(name,tables,files,libs,Makenamep,with_gateway,ldflags,cflags,fflags,'c');
  else
    // unix 
    Makename = makename;
    ilib_gen_Make_unix(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
  end
endfunction

function ilib_gen_Make_unix(name,tables,files,libs,Makename,with_gateway, ...
			    ldflags,cflags,fflags)
  // to be controled 
  with_cygwin = %f 
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# generated by builder.sce: Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------\n");
  // get nsp path 
  NSP = getenv('NSP');
  // do not use a win32 path when cross compiling
  if %win32 && part(NSP,2)==":" then NSP=part(NSP,3:length(NSP));end
  fprintf(fd,"SCIDIR = %s\n",NSP);
  fprintf(fd,"OBJS = ")
  for x=files(:)' ; 
    fprintf(fd," %s.o",x);
  end
  
  if type(tables,'short')<>'l' then 
    tables= list(tables)
  end

  L=length(tables); 
  
  if with_gateway then 
    if L==1 then 
      fprintf(fd," %s.o",name);
    else
      for i=1:L , fprintf(fd," %s.o",name+m2s(i,'%.f'));end 
    end
  end
  for it=1:L 
    table = tables(it)
    [mt,nt]=size(table);
    for i=1:mt ; 
      // mex files to be added 
      if table(i,3)=='cmex' | table(i,3)=='fmex' | table(i,3)=='Fmex' 
	fprintf(fd," %s.o",table(i,2));
      end
    end
  end
  fprintf(fd,"\n") ;
  fprintf(fd,"LIBRARY = %s.a\n",name);
  fprintf(fd,"include $(SCIDIR)/Makefile.incl\n");
  if %win32  && with_cygwin == %t then
    // for cygwin 
    fprintf(fd,"OTHERLIBS = ");
    for x=libs(:)' ; fprintf(fd," %s.a",x);end
    fprintf(fd,"\n");
    fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/include\"""+...
	    " -Dmexfunction_=mex$*_  -DmexFunction=mex_$* "+ cflags +" \n"); 
    fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/include\"""+...
	    " -Dmexfunction=mex$* "+ fflags +"\n"); 
  else
    fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -DmexFunction=mex_$* "+cflags+ "\n");
    fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -Dmexfunction=mex$* "+fflags+  "\n");
    fprintf(fd,"CXXFLAGS = $(CXX_OPTIONS) -DFORDLL -DmexFunction=mex_$* "+cflags+ "\n");
  end
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+ "\n");
  if  %win32  && with_cygwin then
    // we assume cross compilation 
    fprintf(fd,"include $(SCIDIR)/config/Makecygdll.incl\n");
  else
    fprintf(fd,"include $(SCIDIR)/config/Makeso.incl\n");
  end
  fd.close[];
endfunction


function ilib_gen_Make_win32(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# generated by builder.sce: Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------\n");
  fprintf(fd,"SHELL = /bin/sh\n");
  NSP = getenv('NSP');
  fprintf(fd,"SCIDIR =%s\n",NSP);
  fprintf(fd,"SCIDIR1 =%s\n",file('native',NSP));
  fprintf(fd,"# name of the dll to be built\n"); 
  fprintf(fd,"LIBRARY = %s\n",name);
  fprintf(fd,"# list of objects file\n");
  fprintf(fd,"OBJS =");
  for x=files(:)' ; fprintf(fd," %s.obj",x);end

  if type(tables,'short')<>'l' then 
    tables= list(tables)
  end
  L=length(tables); 

  if with_gateway then 
    if L==1 then 
      fprintf(fd," %s.obj",name);
    else
      for i=1:L , fprintf(fd," %s.obj",name+m2s(i,'%.f'));end 
    end
  end
  
  for it=1:L 
    table = tables(it)
    [mt,nt]=size(table);
    
    for i=1:mt ; 
      // mex files to be added 
      if table(i,3)=='cmex' | table(i,3)=='fmex' | table(i,3)=='Fmex' 
	fprintf(fd," %s.obj",table(i,2));
      end
    end
  end
  fprintf(fd,"\n# added libraries \n");
  fprintf(fd,"OTHERLIBS = ");
  for x=libs(:)' ; fprintf(fd," %s.ilib",x);end
  fprintf(fd,"\n");
  fprintf(fd,"!include $(SCIDIR1)\\config\\Makefile.incl.mak\n");
  fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -I""$(SCIDIR)/include"""+...
	  " -Dmexfunction_=mex$*_  -DmexFunction=mex_$* "+ cflags +" \n"); 
  fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -I""$(SCIDIR)/include"""+...
	  " -Dmexfunction=mex$* "+ fflags +"\n"); 
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+"\n");
  fprintf(fd,"!include $(SCIDIR1)\\config\\Makedll.incl \n");
  fd.close[];
endfunction


function ilib_gen_Make_lcc(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags,flag)
// Allan CORNET
//INRIA 2004
  [lcc,path,is64]=lcc_get_compiler();
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# ------------------------------------------------------------\n");
  fprintf(fd,"# generated by builder.sce (lcc 1): Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------------\n\n");
  NSP = getenv('NSP');
  // do not use a win32 path when cross compiling
  fprintf(fd,"SCIDIR =%s\n",NSP);
  fprintf(fd,"SCIDIR1 =%s\n",file('native',NSP));
  fprintf(fd,"DUMPEXTS=""$(SCIDIR1)\\bin\\dumpexts-nsp""\n");
  fprintf(fd,"SCIIMPLIB=$(SCIDIR1)\\bin\\libnsp.lib\n\n");
  fprintf(fd,sprintf("CC=%s\\lcc\n",path));
  fprintf(fd,sprintf("LINKER=%s\\lcclnk\n",path));
  fprintf(fd,"CFLAGS= -s -ansic -Z1 msvcrt.lib -I""$(SCIDIR)/routines"" -I""$(SCIDIR)/routines/f2c"" -Dmexfunction_=mex$*_ -DmexFunction=mex_$* -DWIN32 -DSTRICT -DFORDLL -D__STDC__ "+ cflags +" \n"); 
  
  fprintf(fd,"LINKER_FLAGS=-dll -nounderscores\n");
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+"\n");
  fprintf(fd,"O=.obj\n");
  
  fprintf(fd,"# name of the dll to be built\n"); 
  fprintf(fd,"LIBRARY = %s\n",name);
  fprintf(fd,"# list of objects file\n");
  fprintf(fd,"OBJS =");
  for x=files(:)' ; fprintf(fd," %s$(O)",x);end

  if type(tables,'short')<>'l' then 
    tables= list(tables)
  end
  
  L=length(tables);
  
  if with_gateway then 
    if L==1 then 
      fprintf(fd," %s$(O)",name);
    else
      for i=1:L , fprintf(fd," %s$(O)",name+m2s(i,'%.f'));end 
    end
  end
  
  for it=1:L 
    table = tables(it)
    [mt,nt]=size(table);
    
    for i=1:mt ; 
      // mex files to be added 
      if table(i,3)=='cmex' | table(i,3)=='fmex' | table(i,3)=='Fmex' 
	fprintf(fd," %s$(O)",table(i,2));
      end
    end
  end
  
  
  fprintf(fd,"\n\n# added libraries \n");
  fprintf(fd,"OTHERLIBS =");
  
  for x=libs(:)' ; fprintf(fd," %s.ilib",x);end
  fprintf(fd,"\n");
  
  fprintf(fd,"\n");
  
  fprintf(fd,"\nall :: $(LIBRARY).dll\n");
  fprintf(fd,"\n$(LIBRARY).dll: $(OBJS)\n");
  fprintf(fd,"	$(DUMPEXTS) -o ""$(LIBRARY).def"" ""$*"" $(OBJS)\n");
  fprintf(fd,"	$(LINKER) $(LINKER_FLAGS) $(OBJS) $(OTHERLIBS) $(SCIIMPLIB) $(XLIBSBIN) $(TERMCAPLIB) $(EXTRA_LDFLAGS) $*.def -o $(LIBRARY).dll\n\n");
  

  for x=files(:)' ;
    x=strsubst(x,".obj","");
    x=strsubst(x,".o","");
    fprintf(fd,"%s$(O):\n",x);
    fprintf(fd,"	$(CC) $(CFLAGS) $*.c\n\n");
  end


  if with_gateway then 
    if L==1 then 
      fprintf(fd,"\n%s$(O):\n",name);
      fprintf(fd,"	$(CC) $(CFLAGS) $*.c\n");
    else
      for i=1:L ;
	fprintf(fd,"\n%s$(O):\n",name+m2s(i,'%.f'));
	fprintf(fd,"	$(CC) $(CFLAGS) $*.c\n");
      end 
    end
  end
  
  
  fprintf(fd,"\nclean:\n");
  fprintf(fd,"	del *.obj\n");
  fprintf(fd,"	del *.dll\n");
  fprintf(fd,"	del *.lib\n");
  fprintf(fd,"	del *.def\n");
  
  fd.close[];

endfunction

function [libn,ok]=ilib_compile(lib_name,makename,files,verbose=%t)
// Copyright ENPC
// call make for target files or objects depending on OS and compilers
// if files is given the make is performed on each 
// target contained in files then a whole make is performed 
// makename can be a path leading to a Makefile in that case 
// a chdir is performed before running Makefile 
//-------------------------------------------------
  ok=%t,
  if nargin < 3 || isempty(files) then files=m2s([]); end 
  if type(lib_name,'short')<>'s' then
    error('ilib_compile: first argument must be a string');
    return ;
  end
  // if necessary detect win32 compilers 
  msvc_configure();
  oldpath=getcwd();
  [make_command,lib_name_make,lib_name,path,makename,files]= ...
      ilib_compile_get_names(lib_name,makename,files)  
  if path=="." then path = "./";end 
  if path<>"" && path<>"./" then chdir(path);  end 
  // the name of the shared lib
  try 
    libn= file('join',[path,lib_name_make]);
    // perform the make with system function 
    // step by step 
    // first try to build each file step by step 
    nf = size(files,'*');
    ok=%t;
    for i=1:nf 
      str = [make_command,makename,files(i)];
      strc = catenate(str,sep=" ");
      if verbose then printf('   '+strc + '\n');end
      //ok = ilib_spawn_sync(str);
      [ok,stdout,stderr,msgerr,exitst]=ilib_spawn_sync(str);
      stderr=strsubst(stderr,'%','%%');stdout=strsubst(stdout,'%','%%');
      stdout = catenate(stdout,sep="   ");
      if ~ok then
	printf('   failed to compile %s\n',files(i));
	if length(stdout)<>0 then 
	  printf('   '+stdout + '\n');
	end
	if length(stderr)<>0 then 
	  stderr = catenate(stderr,sep="\n   ");
	  printf('   '+stderr + '\n');
	end
	if length(msgerr)<>0 then
	  msgerr = catenate(msgerr,sep="\n   ");
	  printf('   '+ msgerr + '\n');
	end
	break;
      else
	// XXXX here it should be possible to see the warnings 
	// when compilation was ok 
	// warnings are not in ok,stdout,stderr,msgerr,exitst
      end
      if verbose then printf('   '+stdout + '\n');end
    end

    if ok then
      // then the shared library 
      if verbose then printf('   building shared library\n');end
      str = [make_command, makename, lib_name];
      strc=  catenate(str,sep=" ");
      if verbose then printf('   '+strc + '\n');end
      //ok = ilib_spawn_sync(str);
      [ok,stdout,stderr,msgerr,exitst]=ilib_spawn_sync(str);
      stderr=strsubst(stderr,'%','%%');stdout=strsubst(stdout,'%','%%');
      stdout = catenate(stdout,sep="   ");
      if ~ok then
	printf('   '+stdout + '\n');
	stderr = catenate(stderr,sep="\n   ");
	printf('   '+stderr + '\n');
      elseif verbose then
	printf('   '+stdout + '\n');
      end
    end
  catch 
    printf('%s',catenate(lasterror()));
  finally
    chdir(oldpath); 
  end
endfunction

function [make_command,lib_name_make,lib_name,path,makename,files]=ilib_compile_get_names(lib_name,makename,files) 
// return is res the correct name for 
// makefile, libname, files 
// get file names with suffixes 
  if ~isempty(files) then 
    files=strsubst(strsubst(files,'.obj','') ,'.o','');
  end
  arg_makename=makename;
  makename=file('tail',makename);
  path=file('dirname',arg_makename);
  if %win32 then
    // check if we have a msvc compiler 
    [msvc,msvc_path,is64] = msvc_get_compiler();
    with_msvc = msvc <> "unknown";
    if with_msvc then 
      lib_name=lib_name+'.dll'
      lib_name_make=lib_name;
      makename = makename + '.mak' ; 
      make_command = ['nmake','/y','/nologo','/f'];
      if ~isempty(files) then 
	files = files + '.obj' ;
      end
    else
      // assume that we are cross compiling or using cygwin 
      lib_name_make=lib_name+%shext ;
      lib_name = lib_name+'.la'; 
      make_command = ['/usr/bin/make','-f'];
      if ~isempty(files)  then 
	files = files + '.lo';
      end
    end
    if %f then 
      with_lcc()==%T 
      lib_name_make=lib_name;
      lib_name=lib_name+'.dll'
      makename = makename + '.lcc' ; 
      make_command = ['make','-f'];
      if ~isempty(files) then 
	files = files + '.obj' ;
      end
    end
  else
     // Unixes 
     if ~isempty(files)  then 
       files = files + '.lo';
     end
     lib_name_make=lib_name+%shext ;
     lib_name = lib_name+'.la'; 
     make_command = ['make','-f'];
  end
endfunction 

function [ok,stdout,stderr,msgerr,exitst]=ilib_spawn_sync(str);
  ok=%t
  msok = msvc_configure();
  if msok=='unknown' then
    // for cross compilation prefer  w32mode=%f
    // ok=spawn_sync(str, w32mode=%f);
    [ok,stdout,stderr,msgerr,exitst]=spawn_sync(str, w32mode=%f);
    xpause(0,%t);
    return;
  end;
  // a modified version of spawn_sync 
  // for win32 version i.e with msvc 
  // because stdout and stderr are 
  // not properly redirected when using 
  // nmake. 
  // 
  if %t then 
    // this is with w32mode=%t by default 
    // it means that str is executed viw Comspec 
    // with io redirected to files. 
    // This is usefull on windows to prevent 
    // console popup and properly get nmake output.
    //ok=spawn_sync(str)
    [ok,stdout,stderr,msgerr,exitst]=spawn_sync(str);
    xpause(0,%t);
  else
    // this should give the same behaviour as above 
    // but a console will popup.
    tmp=getenv('NSP_TMPDIR');
    tmp='c:/temp';
    nsp=getenv('NSP');
    cmd=file('join',[nsp,'config/makedll.bat']);
    sto = file('join',[tmp,'spawn_out']);
    ste = file('join',[tmp,'spawn_err']);
    file('delete',sto);
    file('delete',ste);
    ok=spawn_sync([cmd,str($),'>',sto,'2>',ste],w32mode=%f);
    if  file('exists',sto) then
      fd=fopen(sto,mode="r");
      So=fd.get_lines[-1];
      fd.close[];
      So.to_utf8[];
      printf('%s\n',So);
    end
    if  file('exists',ste) then
      fd=fopen(ste,mode="r");
      So=fd.get_lines[-1];
      fd.close[];
      So.to_utf8[];
      printf('%s\n',So);
    end
    xpause(0,%t);
  end
endfunction




