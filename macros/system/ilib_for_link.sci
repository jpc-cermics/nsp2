function [libn,ok]=ilib_for_link(names,files,libs,flag,makename='Makelib',verbose=%t,...
				 loadername='loader.sce',libname="",ldflags="",cflags="",fflags="",cc="",compile=%t)
  // Copyright Enpc
  // Generate a shared library which can be used by link command. 
  // names = names of entry points or the name of the library to 
  // be built (when flag == 'g') 
  // files = object files to be built 
  // flag = 'c' or 'f' or '
  // 
  // generate a loader file
  if verbose then printf('   generate a loader file: %s\n',loadername);end
  ilib_link_gen_loader(names,flag,loadername=loadername,libs=libs,libname=libname);
  // generate a Makefile
  if verbose then printf('   generate a Makefile: %s\n',makename);end
  ilib_link_gen_Make(names,files,libs,makename,libname,...
		     ldflags,cflags,fflags,cc,flag);
  // we call make if requested 
  if compile then 
    if verbose then printf('   running the makefile\n');end
    if libname=="" then libname = names(1);end
    [libn,ok]=ilib_compile('lib'+libname,makename,files,verbose=verbose);
  else
    if verbose then printf('   you need to manually run the makefile: %s\n',makename);end
    libn=0;ok=%t;
  end
endfunction

function ilib_link_gen_loader(names,flag,loadername='loader.sce',libs=[],libname="")
// suffix to be used for dll
  lib_suf = %shext;
  if libname=="" then libname = names(1);end 
  fd=fopen(loadername,mode="w");
  path=file('dirname',loadername);
  // if path== '.' then path=getcwd();end
  fprintf(fd,"// generated by builder.sce: Please do not edit this file\n");
  fprintf(fd,"// ------------------------------------------------------\n");
  nl=size(libs,'*') 
  for i=1:nl do
    if file('pathtype',libs(i))== 'absolute' then
      fprintf(fd,"link(''%s'');\n",libs(i)+lib_suf);
    else
      fprintf(fd,"link(''%s'');\n",file('join',[path,libs(i)+lib_suf]));
    end
  end 
  names=names(:)';
  n = size(names,'*');
  if n == 0 then 
    fprintf(fd,"link(''%s'');\n",file('join',[path,'lib'+libname+lib_suf]));
  else 
    fprintf(fd,"link(''%s'',[",file('join',[path,'lib'+libname+lib_suf]));
    for i=1:n do
      fprintf(fd,"''%s''",names(i))
      if i <>n ; fprintf(fd,","); else fprintf(fd,"],");end
    end
    fprintf(fd,"''%s'');\n",flag);
  end
  fd.close[];
endfunction


function ilib_link_gen_Make(names,files,libs,makename,libname,ldflags,cflags,fflags,cc,flag)
//------------------------------------
// generate a Makefile for gateway
  if nargin <= 2 then libs = [];end
  if nargin <= 3 then makename = 'Makelib';end
  if nargin <= 4 then libname = "";end
  if nargin <= 5 then ldflags = ""; end 
  if nargin <= 6 then cflags  = ""; end 
  if nargin <= 7 then fflags  = ""; end 
  if nargin <= 8 then cc  = ""; end 
  if nargin <= 9 then flag  = "c"; end 
  if %win32 then
    // the target is win32 
    // check if we have a msvc compiler 
    [msvc] = msvc_get_compiler();
    if msvc <> "unknown" then 
      // Makename = makename+'.lcc';
      // ilib_link_gen_Make_lcc(names,files,libs,Makename,libname,...
      // ldflags,cflags,fflags,cc,flag)
      Makename = makename+'.mak'
      ilib_link_gen_Make_win32(names,files,libs,Makename,libname,...
			       ldflags,cflags,fflags,cc)
    else
      // we assume that we are cross compiling or using cygwin 
      Makename = makename;
      ilib_link_gen_Make_unix(names,files,libs,Makename,libname,...
				ldflags,cflags,fflags,cc)
    end
  else
    // unixes 
    Makename = makename;
    ilib_link_gen_Make_unix(names,files,libs,Makename,libname,...
			    ldflags,cflags,fflags,cc)
  end
endfunction

function ilib_link_gen_Make_unix(names,files,libs,Makename,libname, ...
				 ldflags,cflags,fflags,cc)
  // to be controled 
  with_cygwin = %f 
  if libname=="" then libname = names(1);end 
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# generated by builder.sce: Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------\n");
  // get nsp path 
  NSP = getenv('NSP');
  if %win32 then
      // since we are on gen_Make_unix and %win32 is true means that
      // we are compiling with a linux env on windows
      // with msys2 we should keep the volume name
      // if we use msys2 we keep the volume name 
      // part(NSP,2)==":" then NSP=part(NSP,3:length(NSP));end
  end	       
  fprintf(fd,"SCIDIR = %s\n",NSP);
  fprintf(fd,"OBJS = ")
  for x=files(:)' do fprintf(fd," %s",x);end
  fprintf(fd,"\n") ;
  fprintf(fd,"OTHERLIBS = ")
  for x=libs(:)' do fprintf(fd," %s",x);end
  fprintf(fd,"\n") ;
  fprintf(fd,"LIBRARY = lib%s\n",libname);
  fprintf(fd,"include $(SCIDIR)/Makefile.incl\n");
  if cc<>"" then 
    fprintf(fd,"CC="+cc+ "\n");
  end
  if %win32  && with_cygwin == %t then
    // cygwin 
    fprintf(fd,"OTHERLIBS = ");
    for x=libs(:)' do fprintf(fd," %s.a",x);end
    fprintf(fd,"\n");
    fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/include\"""+...
	    " -Dmexfunction_=mex$*_  -DmexFunction=mex_$* "+ cflags +" \n"); 
    fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/include\"""+...
	    " -Dmexfunction=mex$* "+ fflags +"\n"); 
  else
    fprintf(fd,"CFLAGS = $(CC_OPTIONS)  -DFORDLL "+cflags+ "\n");
    fprintf(fd,"FFLAGS = $(FC_OPTIONS)  -DFORDLL "+fflags+ "\n");
  end
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+ "\n");
  if %win32  && with_cygwin == %t then
    fprintf(fd,"include $(SCIDIR)/config/Makecygdll.incl\n");
  else
    fprintf(fd,"include $(SCIDIR)/config/Makeso.incl\n");
  end
  fd.close[];
endfunction

function ilib_link_gen_Make_win32(names,files,libs,Makename,libname,ldflags, ...
				  cflags,fflags,cc)
  
  if libname=="" then libname = names(1);end 
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# generated by builder.sce : Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------\n" );
  NSP = getenv('NSP');
  fprintf(fd,"SCIDIR =%s\n",NSP);
  fprintf(fd,"SCIDIR1 =%s\n",file('native',NSP));
  fprintf(fd,"# name of the dll to be built\n"); 
  fprintf(fd,"LIBRARY = lib%s\n",libname);
  fprintf(fd,"# list of objects file\n");
  fprintf(fd,"OBJS =");
  for x=files(:)' do fprintf(fd," %s",strsubst(x,".o",".obj"));end
  fprintf(fd,"\n# added libraries \n");
  fprintf(fd,"OTHERLIBS = ");
  for x=libs(:)' do fprintf(fd," %s.lib",x);end
  fprintf(fd,"\n");
  fprintf(fd,"!include $(SCIDIR1)\\config\\Makefile.incl.mak\n");
  if cc<>"" then 
    fprintf(fd,"CC="+cc+ "\n");
  end
  fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -I""$(SCIDIR)/include/"""+...
	  " -Dmexfunction_=mex$*_  -DmexFunction=mex_$* "+ cflags +" \n"); 
  fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -I""$(SCIDIR)/include/"""+...
	  " -Dmexfunction=mex$* "+ fflags +"\n"); 
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+"\n");
  fprintf(fd,"!include $(SCIDIR1)\\config\\Makedll.incl \n");
  fd.close[];
endfunction

