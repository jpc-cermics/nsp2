//driver('Rec')
valeurs=-3:3;
noms='noms '+string(valeurs);

function diag_effets(valeurs,noms,titre,couleur)
// Un diagramme d'effets 
//  set('figure_style','new')
// avec le nouveau graphique 
  valeurs=valeurs(:);
  //noms=noms(:);
  //a=gca();
  //a.font_size=3;
  n = size(noms,'*');
  nv = size(valeurs,'*')
  if nv<> n then 
    error('diag_effets: first and second argument should have the same size');
    return 
  end
  pause
  //plot2d([],[],rect=[1,min(valeurs),n+1,max(valeurs)]);
  xsetech(frect=[1,min(valeurs),n+1,max(valeurs)],arect=[0.125,0.125,0.125,0.35]);
  alpha=0.125;
  x=ones(4,1)*(1:n) + [alpha;1-alpha;1-alpha;alpha]*ones(1,n);
  y=[0*valeurs,0*valeurs,valeurs,valeurs]';
  xfpolys(x,y,couleur*ones(1,n));
  // Il semble pas possible d'avoir une graduation en y sans l'avoir en x 
  for i=1:n 
    rect=xstringl(0,0,noms(i))
    xstring(i+0.5,min(valeurs) - 2*rect(4),noms(i),60);
  end
  xtitle(titre);
endfunction

diag_effets(valeurs,noms,'Diagramme d''effets',3);
////xs2ps(0,'diag_effet.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 diag_effet.ps')  /// \sleftarrow{calling a Unix script}
