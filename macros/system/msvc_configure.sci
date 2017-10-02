
function [msvc_compiler,name,configured]=msvc_configure(verbose=%f)
// try to detect a msvc compiler using a bat file 
// msvc_compiler: a nickname for the compiler or "unknown"
// name: the key for product dir
// configured:  %t if succedeed in setting env variables
  
  global(msvc_pref='unset');  
  msvc_compiler='unknown'; // default value
  name = 'unknown';
  configured=%f;
  if ~%win32 then return;end
  
  table= msvc_table();
  if msvc_pref <> 'unset' then 
    // restrict the search the prefered compiler 
    I=find(table(:,2)== msvc_pref);
    table = table(I,:);
  end
  
  // stop at first configured compiler 
  for i=1:size(table,1) do
    name = table(i,1); // returned value
    //    printf("Search %s\n",name);
    if %win64 then 
      target="amd64";
      [msvc_compiler,configured]=msvc_check_product(table(i,2),table(i,1),target,verbose);
      if ~isempty(msvc_compiler) && configured then break;end 
      target="x86_amd64";
      [msvc_compiler,configured]=msvc_check_product(table(i,2),table(i,1),target,verbose);
      if ~isempty(msvc_compiler) && configured then break;end 
    else
      target = "x86";
      [msvc_compiler,configured]=msvc_check_product(table(i,2),table(i,1),target,verbose);
      if ~isempty(msvc_compiler) && configured then break ;end
    end
  end
  
  // usefull for the 64bits version
  
  if msvc_compiler == 'msvc90express' then
    // Microsoft Visual C++ Express 9.0: search sdk
    name1 = 'Software\\Microsoft\\MicrosoftSDK\\InstalledSDKs\\D2FF9F89-8AA2-4373-8A31-C838BF4DBBE1';
    ok1=execstr("rep=registry(''HKEY_LOCAL_MACHINE'',name1,''Install Dir'');",errcatch=%t);
    name1 = 'Software\\Microsoft\\MicrosoftSDK\\InstalledSDKs\\8F9E5EF3-A9A5-491B-A889-C58EFFECE8B3'
    ok2=execstr("rep=registry(''HKEY_LOCAL_MACHINE'',name1,''Install Dir'');",errcatch=%t);
    name1 = 'Software\\Microsoft\\Microsoft SDKs\\Windows';
    ok3=execstr("rep=registry(''HKEY_LOCAL_MACHINE'',name1,''CurrentInstallFolder'');",errcatch=%t);
    if or([ok1,ok2,ok3]) then
      return;
    else
      printf('\nWarning : Microsoft Visual C++ 2008 Express Edition has been detected,\n');
      printf('but not Microsoft Platform SDK for Windows Server 2003 R2 or more.\n');
      printf('Please install this SDK if you want to use dynamic link with scilab.\n');
      lasterror(); // The error message is cleared
    end
  else
    lasterror(); // The error message is cleared
  end

  if msvc_compiler == 'msvc80express' then
    // Microsoft Visual C++ Express 8.0: search SDK
    name1 = 'Software\\Microsoft\\MicrosoftSDK\\InstalledSDKs\\D2FF9F89-8AA2-4373-8A31-C838BF4DBBE1';
    ok1=execstr("rep=registry(''HKEY_LOCAL_MACHINE'',name,''Install Dir'');",errcatch=%t);
    name1 = 'Software\\Microsoft\\MicrosoftSDK\\InstalledSDKs\\8F9E5EF3-A9A5-491B-A889-C58EFFECE8B3';
    ok2=execstr("rep=registry(''HKEY_LOCAL_MACHINE'',name,''Install Dir'');",errcatch=%t);
    name1 = 'Software\\Microsoft\\Microsoft SDKs\\Windows';
    ok3=execstr("rep=registry(''HKEY_LOCAL_MACHINE'',name,''CurrentInstallFolder'');",errcatch=%t);
    if or([ok1,ok2,ok3]) then
      return;
    else
      printf('\nWarning: Microsoft Visual C++ 2005 Express Edition has been detected,\n');
      printf('but not Microsoft Platform SDK for Windows Server 2003 R2 or more.\n');
      printf('Please install this SDK if you want to use dynamic link with scilab.\n');
      lasterror(); // The error message is cleared
    end
  else
    lasterror(); // The error message is cleared
  end
  
  if isempty(msvc_compiler) then
    // no compiler found
    msvc_compiler='unknown';
    name = 'unknown';
    configured=%f;
  end
  setenv('MSVC_COMPILER',msvc_compiler);// to detect if msvc_configure
                                        // was already called
  setenv('MSVC_NAME',name);// to detect if msvc_configure
endfunction

function mc=msvc_select(verbose=%f)
// obtain all the msvc compilers which are able to work 
  global(msvc_pref='unset');  
  msvc_compiler='unknown'; // default value
  name = 'unknown';
  configured=%f;
  if ~%win32 then mc = m2s([]); return;end
  
  table= msvc_table();
  table_ok=[];
  
  for i=1:size(table,1) do
    name = table(i,1); // returned value
    // if nsp is 64 we must produce 64 executables 
    if %win64 then 
      // first check if we have a 64bits compiler producing 64bits code 
      target="amd64";
      [msvc_compiler,configured]=msvc_check_product(table(i,2),table(i,1),target,verbose);
      if ~isempty(msvc_compiler) && configured then 
	table_ok.concatd[i] ;
      else
	// check if we have a 32bits compiler producing 64bits code 
	target="x86_amd64";
	[msvc_compiler,configured]=msvc_check_product(table(i,2),table(i,1),target,verbose);
	if ~isempty(msvc_compiler) && configured then 
	  table_ok.concatd[i] ;
	end
      end
    else target="x86"; 
      [msvc_compiler,configured]=msvc_check_product(table(i,2),table(i,1),target,verbose);
      if ~isempty(msvc_compiler) && configured then table_ok.concatd[i] ;end
    end
  end
  mc = m2s([]);
  if isempty(table_ok) then 
    printf("Cannot find a working Microsoft C compiler\n");
  else
    names = table(table_ok,2);
    val=x_choose(names,'Choose a compiler');
    if val > 0 then mc=names(val);msvc_pref=mc; end 
  end
endfunction

function table= msvc_table()
  table = ['Software\\Microsoft\\VCExpress\\15.0\\Setup\\VS', 'msvc150'
	   'Software\\Microsoft\\VisualStudio\\15.0\\Setup\\VS', 'msvc150'
	   'Software\\Microsoft\\VCExpress\\14.0\\Setup\\VS', 'msvc140'
	   'Software\\Microsoft\\VisualStudio\\14.0\\Setup\\VS', 'msvc140'
	   'Software\\Microsoft\\VCExpress\\12.0\\Setup\\VS', 'msvc120express';
	   'Software\\Microsoft\\VisualStudio\\12.0\\Setup\\VS',  'msvc120pro';
	   'Software\\Microsoft\\VCExpress\\11.0\\Setup\\VS',  'msvc110express';
	   'Software\\Microsoft\\VisualStudio\\11.0\\Setup\\VS',  'msvc110pro';
  // Microsoft Visual 2010
	   'Software\\Microsoft\\VCExpress\\10.0\\Setup\\VS',  'msvc100express';
	   'Software\\Microsoft\\VisualStudio\\10.0\\Setup\\VS',  'msvc100pro';
  // Microsoft Visual 2008
	   'Software\\Microsoft\\VCExpress\\9.0\\Setup\\VS',  'msvc90express';
	   'Software\\Microsoft\\VisualStudio\\9.0\\Setup\\VS\\Pro',  'msvc90pro';
	   'Software\\Microsoft\\VisualStudio\\9.0\\Setup\\VS\\Std',  'msvc90std';
  // Microsoft Visual 2005
	   'Software\\Microsoft\\VCExpress\\8.0\\Setup\\VS',  'msvc80express';
	   'Software\\Microsoft\\VisualStudio\\8.0\\Setup\\VS\\Pro',  'msvc80pro';
	   'Software\\Microsoft\\VisualStudio\\8.0\\Setup\\VS\\Std',  'msvc80std';
  // Microsoft Visual Studio .NET 2003
	   'SOFTWARE\\Microsoft\\VisualStudio\\7.1\\Setup\\VC',  'msvc71';
  // Microsoft Visual Studio .NET 2002
	   'SOFTWARE\\Microsoft\\VisualStudio\\7.0\\Setup\\VC',  'msvc70';
  // Microsoft Visual Studio 6
	   'SOFTWARE\\Microsoft\\DevStudio\\6.0\\Products\\Microsoft Visual C++','msvc60';
  // Microsoft Visual Studio 5
	   'SOFTWARE\\Microsoft\\DevStudio\\5.0\\Directories',  'msvc50'];
  
  if %win32 && %win64 then 	   
    table = [table;
	     "Software\\Wow6432Node\\Microsoft\\VisualStudio\\15.0\\Setup\\VS","msvc150pro";
	     "Software\\Wow6432Node\\Microsoft\\VCExpress\\15.0\\Setup\\VS","msvc150express";
	     "Software\\Wow6432Node\\Microsoft\\VisualStudio\\14.0\\Setup\\VS","msvc140pro";
	     "Software\\Wow6432Node\\Microsoft\\VCExpress\\14.0\\Setup\\VS","msvc140express";
	     
	     "Software\\Wow6432Node\\Microsoft\\VisualStudio\\12.0\\Setup\\VS","msvc120pro";
	     "Software\\Wow6432Node\\Microsoft\\VCExpress\\12.0\\Setup\\VS","msvc120express";
	     
	     "Software\\Wow6432Node\\Microsoft\\VisualStudio\\11.0\\Setup\\VS","msvc110pro";
	     "Software\\Wow6432Node\\Microsoft\\VCExpress\\11.0\\Setup\\VS","msvc110express";
	     
	     "Software\\Wow6432Node\\Microsoft\\VisualStudio\\10.0\\Setup\\VS" ,"msvc100pro";
	     "Software\\Wow6432Node\\Microsoft\\VCExpress\\10.0\\Setup\\VS" ,"msvc100pro"];
  end
endfunction

function [msvc_compiler,configured]=msvc_check_product(rep,name,target,verbose)
// check if a version of visual exists and set env variables accordingly
// msvc_compiler is set to '' if the visual version corresponding to name
// is not found.
// configure is set to %t if setting env variables succeeded.
  
  if rep == 'msvc150' then 
    [msvc_compiler,configured]=msvc_check_product_2017(rep,name,target,verbose);
    return;
  end
  
  configured = %f;
  ok=execstr("vsdir=registry(''HKEY_LOCAL_MACHINE'',name,''ProductDir'');",errcatch=%t);
  if ok then
    msvc_compiler=rep;
  else
    lasterror();
    msvc_compiler=m2s([]);
    return;
  end
  if verbose then printf("C compiler found in registry at: ""%s""\n",vsdir); end
  // execute vcvarsall to set env variables
  VC= sprintf("%sVC",vsdir);
  cmd = sprintf('""%s/bin/vc.bat"" ""%s"" %s',getenv('NSP'),VC,target);
  //  S=unix_g(cmd)
  [ok,S,stderr,msgerr,exitst]=spawn_sync(cmd);
  if ~ok then S=m2s([]);end
  
  vars= ['DevEnvDir';
	 'INCLUDE';
	 'LIB';
	 'LIBPATH';
	 'PATH';
	 'VCINSTALLDIR';
	 'VS100COMNTOOLS';
	 'VSINSTALLDIR';
	 'WindowsSdkDir'];
  
  for i=1:size(vars,'*') do
    tag = vars(i)+'=';
    ntag= length(tag);
    for j=1:size(S,1) do
      if toupper(part(S(j,:),1:ntag)) == toupper(tag) then
	val = part(S(j,:),(ntag+1):length(S(j,:)));
	setenv(vars(i),val);
	//	printf("set %s=%s\n",vars(i),val);
	continue
      end
    end
  end
  if getenv('VCINSTALLDIR','')<>'' then
    configured = %t
    if verbose then 
      printf("C compiler env. variables found with ""vcvarsall.bat %s""\n",target);
    end
  else
    configured = %f
    if verbose then 
      printf("Cannot find C compiler env. variables with ""vcvarsall.bat %s""\n",target);
    end
  end
endfunction

function [msvc_compiler,configured]=msvc_check_product_2017(rep,name,target,verbose)
  // 2017 is special since we have no entries in regedit 
  // we search in standard pathes thus it will not work if the
  // installation is not standard
  
  if verbose then
    printf('Check for Microsoft Visual Studio 2017\n');
  end
    
  configured = %f;
  efiles = [ 'VC/Auxiliary/Build']
  efiles = [ 'Community/'; 'Professional/' ; 'Enterprise/'] + efiles;
  efiles = 'Microsoft Visual Studio/2017/' + efiles;
  files = [ 'C:/Program Files/' + efiles;
	    'C:/Program Files (x86)/' + efiles];
  msvc_compiler=m2s([]);
  for k=1:size(files,'r')
    if verbose then
      printf('search for file %s\n',files(k));
    end
    if file('exists',files(k)+'/vcvarsall.bat') then
      if verbose then
	printf('found %s\n',files(k));
      end
      // execute vcvarsall to set env variables
      VC= sprintf("%s",files(k));
      cmd = sprintf('""%s/bin/vc.bat"" ""%s"" %s',getenv('NSP'),VC,target);
      //  S=unix_g(cmd)
      [ok,S,stderr,msgerr,exitst]=spawn_sync(cmd);
      if ~ok then
	S=m2s([]);
	continue;
      end
            
      vars= ['DevEnvDir';
	     'INCLUDE';
	     'LIB';
	     'LIBPATH';
	     'PATH';
	     'VCINSTALLDIR';
	     'VS100COMNTOOLS';
	     'VSINSTALLDIR';
	     'WindowsSdkDir'];
      for i=1:size(vars,'*') do
	tag = vars(i)+'=';
	ntag= length(tag);
	for j=1:size(S,1) do
	  if toupper(part(S(j,:),1:ntag)) == toupper(tag) then
	    val = part(S(j,:),(ntag+1):length(S(j,:)));
	    setenv(vars(i),val);
	    //	printf("set %s=%s\n",vars(i),val);
	    continue
	  end
	end
      end
      if getenv('VCINSTALLDIR','')<>'' then
	configured = %t
	if verbose then 
	  printf("C compiler env. variables found with ""vcvarsall.bat %s""\n",target);
	end
	msvc_compiler = rep ; 
	return
      end
    end
  end
  if verbose then 
    printf("Cannot find 2017 compiler\n");
  end
endfunction

function [msvc_compiler,name,is64]=msvc_get_compiler()
// returns the nickname of msvc compiler or unknown
  msvc_compiler= getenv('MSVC_COMPILER','unknown');
  if msvc_compiler.equal['unknown'] then 
    [msvc_compiler,name,configured]=msvc_configure();
  else	
    name =  getenv('MSVC_NAME');
  end
  is64 = %win64;
endfunction

function y=have_compiler()
  y = ~%win32 || msvc_get_compiler() <> 'unknown';
endfunction

  
