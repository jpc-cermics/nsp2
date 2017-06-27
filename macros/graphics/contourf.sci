function contourf(x,y,z,varargopt)
// varargopt
// nv=[], plot2d options.

  if nargin==0,
    s_mat=['t=-%pi:0.1:%pi;m=sin(t)''*cos(t);contourf(t,t,m);'];
    print(s_mat);
    execstr(s_mat);
    return;
  end
  if nargin <= 1 then y=1:10;end
  if nargin <= 2 then z=rand(size(x,'*'),size(y,'*'));end

  if type(z,'short')=='pl' then
    z_fun=z;
    z=feval(x,y,z_fun);
  end

  zmin=min(z);zmax=max(z);nv_def= zmin + (1:10)*(zmax-zmin)./(11);
  nv=varargopt.find['nv',def=nv_def];
  varargopt.delete['nv'];

  if isempty(x) then x=1:size(z,'r');end
  if isempty(y) then y=1:size(z,'c');end
  nvs=size(nv,'*') ;
  if nvs==1 then nvs=nv;nv = zmin + (1:nvs)*(zmax-zmin)./(nvs+1);end

  [mz,nz] = size(z);

  lp=xget('lastpattern');
  if nvs > lp ; printf(sprintf('Colormap is too small for %d levels\n',nvs));return ;end
  min_nv=min(nv);
  max_nv=max(nv);
  // fill the contours
  grayplot(x,y,z, varargopt(:),shade=%t,colminmax=[1,nvs-1],colout=[0,nvs],...
	   zminmax=[min(nv),max(nv)]);
  // draw the boundaries
  contour2d(x,y,z,nv,varargopt(:));
endfunction
