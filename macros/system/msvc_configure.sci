function ok = msvc_configure() 

  function path = msvc_get_sdk()
    path = "";
    entries = ["Software\Microsoft\Microsoft SDKs\Windows" "CurrentInstallFolder" ; // Vista & Seven SDK
	       "Software\Microsoft\MicrosoftSDK\InstalledSDKs\D2FF9F89-8AA2-4373-8A31-C838BF4DBBE1" "Install Dir" ; // Windows 2003 R2 SDK
	       "Software\Microsoft\MicrosoftSDK\InstalledSDKs\8F9E5EF3-A9A5-491B-A889-C58EFFECE8B3" "Install Dir"]; // Windows 2003 SDK
    for i = 1:size(entries,'r')
      ok = execstr("path =registry(''HKEY_LOCAL_MACHINE'',key,''ProductDir'')",errcatch=%t);
      if ok && file('exists',path') then break;end 
    end
  endfunction
  
  ok = %f;
  [name,path,is64] = msvc_get_compiler() 
  sdk_path = msvc_get_sdk();
  if name == "unknown" then return;end 
  select msvc 
   case  'msvc100pro' then // Microsoft Visual 2010 Studio Professional
    ok = msvc_setenv_vc10(name,path,sdk_path, is64);
   case  'msvc100express' then // Microsoft Visual 2010 Express
    ok = msvc_setenv_vc10(name,path,sdk_path, is64);
   case  'msvc90pro' then // Microsoft Visual 2008 Studio Professional
    ok = msvc_setenv_vc90(name,path,sdk_path, is64);
   case  'msvc90std' then     // Microsoft Visual 2008 Studio Standard
    ok = msvc_setenv_vc90(name,path,sdk_path, is64);
   case  'msvc90express' then    // Microsoft Visual 2008 Express
    ok = msvc_setenv_vc90(name,path,sdk_path, is64);
   case  'msvc80pro' then    // Microsoft Visual 2005 Studio Professional
    ok = msvc_setenv_vc80(name,path,sdk_path, is64);
   case  'msvc80std' then    // Microsoft Visual 2005 Studio Standard
    ok = msvc_setenv_vc80(name,path,sdk_path, is64);
   case  'msvc80express' then// Microsoft Visual 2005 Express
    ok = msvc_setenv_vc80(name,path,sdk_path, is64);
   case  'msvc71' then     // Microsoft Visual Studio .NET 2003
    ok = msvc_setenv_vc7x(name,path,sdk_path, is64);
   case  'msvc70' then // Microsoft Visual Studio .NET 2002
    ok = msvc_setenv_vc7x(name,path,sdk_path, is64);
  end
endfunction

function ok = msvc_setenv_vc10_vc90(path, sdk_path, IsExpress, is64)
  
  function  ok=setNewLIB( path, sdk_path, bIsExpress, is64)
    ok=%f;
    LIB = getenv('LIB', '');
    if is64 then 
      newLIB = [path + '\\VC\\ATLMFC\\LIB\\amd64';
		path + '\\VC\\LIB\\amd64'
		sdk_path + '\\lib\\x64';
		LIB];
    else
      newLIB = [  path + '\\VC\\ATLMFC\\LIB'
		  path +  '\\VC\\LIB'
		  sdk_path +  '\\lib'
		  LIB];
    end
    if bIsExpress then newLIB(1)=[];end 
    newLIB =catenate(newLIB,sep=';');
    setenv('LIB',newLIB );
    if getenv('LIB','ndef') <> newLIB then return;end
    ok=%t;
  endfunction

  function ok=setNewPATH( path, sdk_path, bIsExpress, is64)
    ok=%f;
    PATH = getenv('PATH', '');
    if is64 then 
      newPATH = [ path + '\\VC\\BIN\\amd64'
		  path + '\\VC\\VCPackages'
		  path + '\\Common7\\IDE'
		  path + '\\Common7\\Tools'
		  path + '\\Common7\\Tools\\bin'
		  sdk_path + '\\bin\\x64'
		  sdk_path + '\\bin\\win64\\x64'
		  sdk_path + '\\bin'
		  PATH];
    else
      newPATH = [path + '\\Common7\\IDE\\'
		 path + '\\VC\\bin'
		 path + '\\Common7\\Tools'
		 path + '\\VC\\VCPackages'
		 sdk_path + '\\bin'
		 PATH];
    end
    newPATH=catenate(newPATH,sep=';');
    setenv('PATH',newPATH);
    if getenv('PATH','ndef') <> newPATH then return;end
    ok=%t;
  endfunction

  function ok=setNewLIBPATH( path, sdk_path, bIsExpress,  is64)
    ok=%f;
    LIBPATH = getenv('LIBPATH', '');
    if is64 then 
      newLIBPATH = [path + '\\VC\\ATLMFC\\LIB\\amd64';
		    path + '\\VC\\LIB\\amd64'; 
		    LIBPATH];
    else
      newLIBPATH = [path + '\\VC\\ATLMFC\\LIB'
		    path + '\\VC\\LIB';
		    LIBPATH];
    end
    if bIsExpress then newLIBPATH(1)=[];end 
     newLIBPATH=catenate(newLIBPATH,sep=';');
    setenv('LIBPATH', newLIBPATH);
    if getenv('LIBPATH','ndef') <> newLIBPATH then return;end
    ok=%t;
  endfunction

  function ok=setNewINCLUDE( path, sdk_path, bIsExpress,  is64)
    ok=%f;
    INCLUDE = getenv('INCLUDE', '');
    if is64 then 
      newINCLUDE = [ path + '\\VC\\INCLUDE'
		     path + '\\VC\\ATLMFC\\INCLUDE'
		     sdk_path + '\\include'
		     INCLUDE];
      if bIsExpress then newINCLUDE(2)=[];end 
    else
      newINCLUDE = [ path + '\\VC\\ATLMFC\\INCLUDE';
		     path + '\\VC\\INCLUDE';
		     sdk_path + '\\include';
		     INCLUDE];
      if bIsExpress then newINCLUDE(1)=[];end 
    end
    newINCLUDE = catenate(newINCLUDE,sep=';');
    setenv('INCLUDE',newINCLUDE);
    if getenv('INCLUDE','ndef') <> newINCLUDE then return;end
    ok=%t;
  endfunction

  if sdk_path <> "" then
    setenv('WindowsSdkDir', sdk_path);
  end
  setenv('VSINSTALLDIR', path);
  DevEnvDir = path + '\\Common7\\IDE';
  setenv('DevEnvDir', DevEnvDir);
  setenv('VCINSTALLDIR', path + '\\VC\\');
  ok=setNewLIB(path, sdk_path, IsExpress, is64);
  ok=ok && setNewPATH(path, sdk_path, IsExpress, is64);
  ok=ok && setNewINCLUDE(path, sdk_path, IsExpress, is64);
  ok=ok && setNewLIBPATH(path, sdk_path, IsExpress, is64);

endfunction

function ok = msvc_setenv_vc10(msvc,path,sdk_path, is64)
// set up for vc 2010
  IsExpress = (msvc == 'msvc100express');
  setenv('VS100COMNTOOLS',path + '\\Common7\\Tools\')
  ok= msvc_setenv_vc10_vc90(path,sdk_path, IsExpress, is64);
endfunction

function ok=msvc_setenv_vc90(msvc,path,sdk_path, is64)
// set up for vc 90 
  IsExpress = (msvc == 'msvc90express');
  setenv('VS90COMNTOOLS', path + '\\Common7\\Tools\\')
  ok= msvc_setenv_vc10_vc90(path,sdk_path, IsExpress, is64);
endfunction


function ok=msvc_setenv_vc80(msvc,path,sdk_path, is64)
// set up for vc 80 
  ok = %f;
  if is64 then return ;end 
  PATH = getenv('PATH','ndef');
  if (PATH == 'ndef') then; return ; end 
  setenv('VSINSTALLDIR', path) 
  MSVCDir = path + '\\VC';
  setenv('VCINSTALLDIR', MSVCDir);
  DevEnvDir = path + '\\Common7\\IDE';
  setenv('DevEnvDir', DevEnvDir);
  if getenv('DevEnvDir','ndef') <> DevEnvDir then; return ; end 
  pathes = [ DevEnvDir;
	     MSVCDir + '\\bin'
	     path + '\\Common7\\Tools'
	     path + '\\SDK\\v2.0\\bin'
	     MSVCDir + '\\VCPackages'
	     PATH];
  pathes=catenate(pathes,sep=";");
  setenv('PATH',pathes);
  if getenv('PATH','ndef') <> pathes then return;end 
    
  LIB = getenv('LIB', '');
  INCLUDE = getenv('INCLUDE', '');
  
  if ( msvc  == 'msvc80express') then
    LIB = [MSVCDir + '\\LIB'
	   path + '\\SDK\v2.0\lib'
	   sdk_path + '\\Lib'
	   LIB];
    INCLUDE = [ MSVCDir + '\\INCLUDE' 
		sdk_path + '\\INCLUDE']
  else
    LIB = [ MSVCDir + '\\LIB'
	    path + '\\SDK\\v2.0\\lib'
	    path + '\\VC\\PlatformSDK\\lib'
	    LIB];

    INCLUDE = [ MSVCDir + '\\INCLUDE'
		MSVCDir + '\\PlatformSDK\\include' 
		path + '\\SDK\\v2.0\\include'
		INCLUDE];
  end

  pathes=catenate(LIB,sep=";");
  setenv('LIB',pathes);
  if getenv('LIB','ndef') <> pathes then return;end 

  pathes=catenate(INCLUDE,sep=";");
  setenv('INCLUDE',pathes);
  if getenv('INCLUDE','ndef') <> pathes then return;end 
  ok=%t
endfunction

function ok = msvc_setenv_vc7x(msvc,path,sdk_path, is64)
// set up for vc 70 and 71 
  ok = %f 
  if is64 then return ;end 
  setenv('MSVCDir', path);
  setenv('DevEnvDir', path + '\\..\\Common7\\Tools');
  DevEnvDir = getenv('DevEnvDir', 'ndef');
  if DevEnvDir == 'ndef' then return ;end 
  PATH = getenv('PATH', 'ndef');
  if PATH == 'ndef' then return ;end 
  pathes=[ path+'\\BIN';
	   DevEnvDir;
	   DevEnvDir + '\\bin';
	   path + '\\..\\Common7\\IDE';
	   PATH];
  pathes= catenate(pathes,sep=";");
  setenv('PATH',pathes);
  if getenv('PATH','ndef') <> pathes then return;end 
  INCLUDE = getenv('INCLUDE', '');
  pathes =[ path + '\\ATLMFC\\INCLUDE';
	    path + '\\INCLUDE';
	    path + '\\PlatformSDK\\include';
	    INCLUDE];
  pathes= catenate(pathes,sep=";");
  setenv('INCLUDE', pathes)
  if getenv('INCLUDE','ndef') <> pathes then return;end 
  LIB = getenv('LIB', '');
  pathes =[ path + '\\ATLMFC\\LIB'
	    path + '\\LIB'
            path + '\\PlatformSDK\\lib'
	    LIB]; 
  pathes= catenate(pathes,sep=";");
  setenv('LIB', pathes);
  if getenv('LIB','ndef') <> pathes then return;end 
  ok=%t;
endfunction




