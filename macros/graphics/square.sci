function []=square(xmin,ymin,xmax,ymax)
//definit un  environnement  graphique
//permettant un trace isometrique.
//
//xmin,ymin, (xmax,ymax) specifient les abscisses et ordonnees minimum
//           (maximum) du dessin. defaut (-2,-2,2,2)
//
//square modifie le facteur d'echelle sur l'axe  Ox. Pour conserver la
//meme taille de dessin, utiliser plutot la macro isoview. Les extrema
//sont imposes.
//!
// Copyright INRIA
  if nargin <=0 then xmin=-2;ymin=-2;xmax=2;ymax=2;end
  wdim=xget("wdim");
  xset("wdim",maxi(wdim),maxi(wdim));
  plot2d(0,0,style=1,rect=[xmin,ymin,xmax,ymax]);
endfunction
