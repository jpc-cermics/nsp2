function ilib_build(ilib_name,table,files,libs,...
		    makename='Makelib',ldflags="",cflags="",fflags="",verbose=%t)
  // generate the gateway file
  if verbose then printf('   generate a gateway file\n');end
  ilib_gen_gateway(ilib_name,table)
  // generate a loader file
  if verbose then printf('   generate a loader file\n');end
  ilib_gen_loader(ilib_name,table,libs);
  // generate a Makefile
  if verbose then printf('   generate a Makefile: %s\n',makename);end
  if table == [] then with_gateway=%f ; else with_gateway=%t ;end 
  ilib_gen_Make(ilib_name,table,files,libs,makename,with_gateway,ldflags,cflags,fflags);
  // we call make
  if verbose then printf('   running the makefile\n');end
  ilib_compile(ilib_name,makename,files);
endfunction

function ilib_gen_gateway(name,tables)
// Copyright Enpc 
//------------------------------------
// generate an interface gateway named name
// from table table taking into account 
// attributes of function i.e mex fmex or std interface 
//------------------------------------
// extract the 
  arg_name=name;
  name=file('tail',name);
  sufix=file('extension',name);
  name=strsubst(name,sufix,"");
  path=file('dirname',arg_name);
  
  if type(tables,'short')<>'l' then 
    if tables==[] then return;end 
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
    if nt==2 then col= "csci"; table = [table, col(ones(mt,1))];nt = 3 ; end 
    if nt<>3 then error('second argument has wrong size ');end 
    [gate,names,cast,declar,have_mex]=new_names(table); 
    mex_include='';
    if have_mex then mex_include = '#include ""mex/mex.h""';end
    t=['/* do not edit : generated by ilib_ */';
       '#include ""nsp/machine.h""';
       '#include ""nsp/interf.h""';
       mex_include;
       '';
       'extern ' + declar(:) +' '+  names(:) + ';';
       '';
       'static OpWrapTab '+tname+'_func[] = {';
       '  {""'+table(:,1)+'"",'+ cast(:)+' '+ names(:) +','+ gate(:) +'},';
       '  {(char *) 0, NULL, NULL},';
       '};';
       'int '+tname+'_Interf (int i, Stack stack, int rhs, int opt, int lhs)';
       '{';
       '  if ( ' + tname +'_func[i].wrapper == NULL)';
       '     return (*('+tname+'_func[i].fonc)) (stack, rhs, opt, lhs);';
       '  else ';
       '     return (*('+tname+'_func[i].wrapper)) (stack, rhs, opt, lhs,'+tname+'_func[i].fonc);';
       '}';
       'void '+tname+'_Interf_Info (int i, char **fname, function (**f))';
       '{';
       ' *fname = '+tname+'_func[i].name;';
       ' *f = '+tname+'_func[i].fonc;';
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

function [gate,names,cast,declar,have_mex]=new_names(table) 
// change names according to types 
  [mt,nt]=size(table);
  have_mex=%f;
  gate= "NULL";
  gate = gate(ones(mt,1));
  names= " "; 
  names= names(ones(mt,1)); 
  cast = "";
  cast = cast(ones(mt,1)); 
  declar= "function";
  declar = declar(ones(mt,1)); 
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
    else error(999,"wrong interface type "+table(i,3)); 
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
  fd.printf["// ------------------------------------------------------\n"];
  fd.printf["%s_path=""./"" // get_file_path(''loader.sce'');\n",name];

  nl=size(libs,'*') 
  for i=1:nl 
    fd.printf["link(%s_path+''/%s%s'');\n",name,libs(i),lib_suf];
  end 

  if L == 1 then 
    // direct call to addinter 
    table = tables(1);
    fd.printf["addinter(%s_path+''/%s%s'',''%s'');\n",name, name,lib_suf,name];
  else
     // on link then a set of addinter 
     fd.printf["ilib=link(%s_path+''/%s%s'');\n",name,name,lib_suf];
     for itable=1:L 
       // loop on a list of tables 
       table = tables(itable);
       fd.printf["addinter(ilib,''%s'');\n",name+m2s(itable,'%.f')];
     end
  end
  fd.close[];
endfunction

function Makename=ilib_gen_Make(name,tables,files,libs,makename,with_gateway,ldflags,cflags,fflags,flag)
// flag 'c' or 'f' used with lcc 
// Copyright Enpc 
//------------------------------------
// generate a Makefile for gateway
  if nargin < 6 then with_gateway=%t,ldflags='',cflags='',fflags='';end
  if files<>[] then 
    files=strsubst(strsubst(files,'.obj','') ,'.o',''); //compat
  end
  // change table if necessary 
  if type(tables,'short')<>'l' then 
    tables= list(tables)
  end
  L=length(tables); 
  for it = 1:L 
    table = tables(it);
    [mt,nt]=size(table);
    if nt==2 then col= "csci"; table = [table, col(ones(mt,1))];nt=3; end 
    if nt<>0 & nt<>3 then error('second argument has wrong size ');end 
    tables(it)=table;
  end
      
  if getenv('WIN32','NO')=='OK' then
    comp_target = COMPILER;
    if with_lcc() == %T then
      Makename = makename+'.lcc'
      flag='c'
      ilib_gen_Make_lcc(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags,flag)
    else
      select comp_target
       case 'VC++'   then Makename = makename+'.mak'
	ilib_gen_Make_win32(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
       case 'ABSOFT' then Makename = makename+'.amk'
	ilib_gen_Make_win32(name,tables,files,libs,Makename,with_gateway, ...
			    ldflags,cflags,fflags)
       case 'gcc' then 
	Makename = makename;
	ilib_gen_Make_unix(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
      else
	Makename = makename;
	ilib_gen_Make_win32(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
      end
    end
  else
    Makename = makename;
    ilib_gen_Make_unix(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
  end
endfunction

function ilib_gen_Make_unix(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags)
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# generated by builder.sce: Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------\n");
  fprintf(fd,"SCIDIR = %s\n",getenv('SCI'));
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
  if getenv('WIN32','NO')=='OK' then
    // for cygwin 
    fprintf(fd,"OTHERLIBS = ");
    for x=libs(:)' ; fprintf(fd," %s.a",x);end
    fprintf(fd,"\n");
    fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/routines\"""+...
	     " -Dmexfunction_=mex$*_  -DmexFunction=mex_$* "+ cflags +" \n"); 
    fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/routines\"""+...
	     " -Dmexfunction=mex$* "+ fflags +"\n"); 
  else
     fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DmexFunction=mex_$* "+cflags+ "\n");
     fprintf(fd,"FFLAGS = $(FC_OPTIONS) -Dmexfunction=mex$* "+fflags+ "\n");
  end
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+ "\n");
  if getenv('WIN32','NO')=='OK' then
    // cygwin assumed : we use a specific makedll 
    // and not libtool up to now XXX 
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
  fprintf(fd,"SCIDIR =%s\n",SCI);
  fprintf(fd,"SCIDIR1 =%s\n",pathconvert(SCI,%f,%f,'w'));
  fprintf(fd,"# name of the dll to be built\n"); 
  fprintf(fd,"LIBRARY = %s\n",name);
  fprintf(fd,"# list of objects file\n");
  fprintf(fd,"OBJS =");
  for x=files(:)' ; fprintf(fd," %s.obj",x);end

  if typeof(tables)<>'list' then 
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
  fprintf(fd,"!include $(SCIDIR1)\\Makefile.incl.mak\n");
  fprintf(fd,"CFLAGS = $(CC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/routines\"""+...
	   " -Dmexfunction_=mex$*_  -DmexFunction=mex_$* "+ cflags +" \n"); 
  fprintf(fd,"FFLAGS = $(FC_OPTIONS) -DFORDLL -I\""$(SCIDIR)/routines\"""+...
	   " -Dmexfunction=mex$* "+ fflags +"\n"); 
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+"\n");
  fprintf(fd,"!include $(SCIDIR1)\\config\\Makedll.incl \n");
  fd.close[];
endfunction

         
function ilib_gen_Make_lcc(name,tables,files,libs,Makename,with_gateway,ldflags,cflags,fflags,flag)
// Allan CORNET
//INRIA 2004
  fd=fopen(Makename,mode="w");
  fprintf(fd,"# ------------------------------------------------------------\n");
  fprintf(fd,"# generated by builder.sce (lcc 1): Please do not edit this file\n");
  fprintf(fd,"# ------------------------------------------------------------\n\n");
  fprintf(fd,"SCIDIR =%s\n",SCI);
  fprintf(fd,"SCIDIR1 =%s\n",pathconvert(SCI,%f,%f,'w'));
  fprintf(fd,"DUMPEXTS=""$(SCIDIR1)\\bin\\dumpexts""\n");
  if ( with_atlas()==%T ) then
  	fprintf(fd,"SCIIMPLIB=$(SCIDIR1)\\bin\\LibScilabLCC.lib $(SCIDIR1)\\bin\\atlaslcc.lib\n\n");
  else
  	fprintf(fd,"SCIIMPLIB=$(SCIDIR1)\\bin\\LibScilabLCC.lib\n\n");
  end
  fprintf(fd,"CC=lcc\n");
  fprintf(fd,"LINKER=lcclnk\n");
  fprintf(fd,"CFLAGS= -s -ansic -Z1 msvcrt.lib -I""$(SCIDIR)/routines"" -I""$(SCIDIR)/routines/f2c"" -Dmexfunction_=mex$*_ -DmexFunction=mex_$* -DWIN32 -DSTRICT -DFORDLL -D__STDC__ "+ cflags +" \n"); 
  
  fprintf(fd,"LINKER_FLAGS=-dll -nounderscores\n");
  fprintf(fd,"EXTRA_LDFLAGS = "+ ldflags+"\n");
  fprintf(fd,"O=.obj\n");
 
  fprintf(fd,"# name of the dll to be built\n"); 
  fprintf(fd,"LIBRARY = %s\n",name);
  fprintf(fd,"# list of objects file\n");
  fprintf(fd,"OBJS =");
  for x=files(:)' ; fprintf(fd," %s$(O)",x);end

  if typeof(tables)<>'list' then 
    tables= list(tables)
  end
  
  L=length(table); 

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

function libn=ilib_compile(lib_name,makename,files)
// Copyright ENPC
// call make for target files or objects depending
// on OS and compilers
// very similar to G_make
// if files is given the make is performed on each 
// target contained in files then a whole make is performed 
//-------------------------------------------------
  if nargin < 3 then files=[]; end 
  if type(lib_name,'short')<>'s' then
    error('ilib_compile: first argument must be a string');
    return ;
  end
  oldpath=getcwd();
  files=files(:)';
  if files<>[] then 
    files1=strsubst(strsubst(files,'.obj','') ,'.o','');
  else 
    files1=m2s([]);
  end
  [make_command,lib_name_make,lib_name,path,makename,files]= ...
      ilib_compile_get_names(lib_name,makename,files)  
  if path<> '';  chdir(path);  end 
  // first try to build each file step by step 
  nf = size(files,'*');
  for i=1:nf 
    printf('   compilation of '+files1(i)+'\n');
    system(make_command+makename + ' '+ files(i)); 
  end
  // then the shared library 
  printf('   building shared library (be patient)\n');
  system(make_command+makename + ' '+ lib_name); 
  // a revoir 
  if path=="." then path = "./";end 
  if part(path,length(path))<>"/" then path=path+"/";end
  libn=path+lib_name_make ; 
  chdir(oldpath)
endfunction

function [make_command,lib_name_make,lib_name,path,makename,files]=ilib_compile_get_names(lib_name,makename,files) 
// return is res the correct name for 
// makefile, libname, files 
  if files<>[] then 
    files=strsubst(strsubst(files,'.obj','') ,'.o',''); //compat
  end
  arg_makename=makename;
  makename=file('tail',makename);
  path=file('dirname',arg_makename);
  if getenv('WIN32','NO')=='OK' then
    comp_target = COMPILER;
    if with_lcc()==%T then
      lib_name=lib_name+'.dll'
      lib_name_make=lib_name;
      makename = makename + '.lcc' ; 
      make_command = 'make -f '
      if files<>[] then 
	files = files + '.obj' ;
      end
    else
      lib_name=lib_name+'.dll'
      lib_name_make=lib_name;
      select comp_target
       case 'VC++' then 
	makename = makename + '.mak' ; 
	make_command = 'nmake /nologo /f '
	if files<>[] then 
	  files = files + '.obj' ;
	end
       case 'ABSOFT' then 
	makename = makename + '.amk ';
	make_command = 'amake /f '
	if files<>[] then 
	  files = files + '.obj' ;
	end
       case 'gcc' then 
	makename = makename;
	make_command = 'make -f '
	if files<>[] then 
	  files = files + '.o' ;
	end
      else // like gnuwin32 
	makename = makename;
	make_command = 'make -f '
	if files<>[] then 
	  files = files + '.o' ;
	end
      end
    end
  else
    // Unixes 
    if files <> [] then 
      files = files + '.o';
    end
    lib_name_make=lib_name+%shext ;
    lib_name = lib_name+'.la'; 
    make_command = 'make -f ';
  end
endfunction 


  




