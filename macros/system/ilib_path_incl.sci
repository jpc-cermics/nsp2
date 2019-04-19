function ilib_path_incl(relative=%t)
// write a Path.incl file 
// trying to preserve relative path to nsp 
  dir=getcwd();
  nsp=getenv('SCI');
  msvc = msvc_get_compiler();
  if msvc == "unknown" then 
    // we assume that we are cross compiling 
    // do not use a win32 path when cross compiling i.e 
    // remove the volume: part 
    // get nsp path 
    nsp = getenv('NSP');
    // use_msys2 = or ( getenv('MSYSTEM', 'unknown') == ['MINGW64','MINGW32']);
    // do not use a win32 path when cross compiling (removed when using msys2)
    // if %win32 && part(nsp,2)==":" then nsp=part(nsp,3:length(nsp));end
  end
  if relative then 
    nsp_s = file('split',nsp);
    dir_s = file('split',dir);
    if numel(dir_s) >= numel(nsp_s) && nsp_s.equal[dir_s(1:size(nsp_s,'*'))] then 
      // dir is a subdir of nsp 
      // compute a relative path 
      k = size(dir_s,'*') - size(nsp_s,'*');
      nsp=file('join',smat_create(1,k,".."));
    else
      k =  size(dir_s,'*');
      nsp=file('join',[smat_create(k-1,1,"..");nsp_s(2:$)]);
    end
  end
  F=fopen('Path.incl',mode='w');
  fprintf(F,'SCIDIR='+nsp+'\n');
  fprintf(F,'SCIDIR1='+strsubst(nsp,'/','\\')+'\n');
  F.close[];
endfunction

