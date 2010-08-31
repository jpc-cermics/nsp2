function ilib_path_incl()
// write a Path.incl file 
// trying to preserve relative path to nsp 
  dir=getcwd();
  nsp=getenv('SCI');
  nsp_s = file('split',nsp);
  dir_s = file('split',dir);
  if numel(dir_s) >= numel(nsp_s) && nsp_s.equal[dir_s(1:size(nsp_s,'*'))] then 
    // dir is a subdir of nsp 
    // compute a relative path 
    k = size(dir_s,'*') - size(nsp_s,'*');
    nsp=file('join',smat_create(1,k,".."));
  end
  F=fopen('Path.incl',mode='w');
  fprintf(F,'SCIDIR='+nsp+'\n');
  fprintf(F,'SCIDIR1='+strsubst(nsp,'/','\\')+'\n');
  F.close[];
endfunction

