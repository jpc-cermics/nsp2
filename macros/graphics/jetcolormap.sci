function [cmap] = jetcolormap(nb_colors)
//   PURPOSE
//      to get the usual classic colormap which goes from 
//      blue - lightblue - green - yellow - orange then red
//   AUTHOR
//      Bruno Pincon
//
   r = [0.000 0.125 0.375 0.625 0.875 1.000];
   rv= [0.000 0.000 0.000 1.000 1.000 0.500];
   g = [0.000 0.125 0.375 0.625 0.875 1.000];
   gv= [0.000 0.000 1.000 1.000 0.000 0.000];
   b = [0.000 0.125 0.375 0.625 0.875 1.000];
   bv= [0.500 1.000 1.000 0.000 0.000 0.000];
   d = 0.5/nb_colors
   x = linspace(d,1-d, nb_colors)
   cmap = [ interpolate(x,xref=r,yref=rv);...
	    interpolate(x,xref=g,yref=gv);...
	    interpolate(x,xref=b,yref=bv) ]';
   cmap = min(1, max(0 , cmap))  // normaly not necessary
endfunction
