function []=fchamp(macr_f,fch_t,fch_xr,fch_yr,varargopt)
// Draws a 2D vector field where the vector field is
//   given by y=f(x,t,[u]),
//    f : vector field. CAn be either :
//	 a function y=f(t,x,[u])
//       a list such as  list(f1,u1) with f1 a function
//       y=f1(t,x,u) and u1=current value of u
//    t : instants vector
//    xr,yr: two vectors defining the grid
//
  if nargin <= 0 then
    s_mat=[ ' function [xdot] = derpol(t,x,u);'
	    '   xdot=[x(2); -x(1) + (1 - x(1)**2)*x(2)];'
	    ' endfunction';
	    ' xset(''colormap'',jetcolormap(32));'
	    ' fchamp(derpol,0,-1:0.1:1,-1:0.1:1,champ1=%t);'];
    printf("%s",catenate(s_mat,sep="\n"));
    printf("\n");
    execstr(s_mat);
    return
  end
  if nargin <= 2,fch_xr=-1:0.1:1;end
  if nargin <= 3,fch_yr=-1:0.1:1;end
  function [yy]=mmm(x1,x2,args)
    xx=macr_f(fch_t,[x1;x2],args(1));
    yy=xx(1)+%i*xx(2);
  endfunction
  if varargopt.iskey['args'] then
    fch_v=feval(fch_xr,fch_yr,mmm,args=list(varagopt('args')));
    varargopt.delete['args'];
  else
    fch_v=feval(fch_xr,fch_yr,mmm,args=list(0));
  end
  if varargopt.iskey['champ1'] then
    varargopt.delete['champ1'];
    champ1(fch_xr,fch_yr,real(fch_v),imag(fch_v),varargopt(:));
  else
    champ(fch_xr,fch_yr,real(fch_v),imag(fch_v),varargopt(:));
  end

endfunction
