//driver('Rec')
valeurs=10*rand(1,5,'u');
n= size(valeurs,'*');
noms='noms '+string(1:n);

function pareto(valeurs,valeurs1,noms,titre,couleur)
  valeurs=valeurs(:);
  valeurs1=valeurs1(:);
  //noms=noms(:);
  xset('font size',3);
  n = size(noms,'*');
  nv = size(valeurs,'*')
  if nv<> n then 
    error('diag_effets: first and second argument should have the same size');
    return 
  end
  xsetech(wrect=[0,0,1,1],frect=[1,0,n+1,max(valeurs1)],arect=[0.125,0.125,0.125,0.35]);
  //plot2d([],[],rect=[1,0,n+1,max(valeurs1)]);
  alpha=0.125;
  x=ones_new(4,1)*(1:n) + [alpha;1-alpha;1-alpha;alpha]*ones_new(1,n);
  y=[0*valeurs,0*valeurs,valeurs,valeurs]';
  xfpolys(x,y,couleur*ones_new(1,n));
  // Il semble pas possible d'avoir une graduation en y sans l'avoir en x 
  for i=1:n 
    rect=xstringl(0,0,noms(i))
    xstring(i+0.5, - 2*rect(4),noms(i),60);
  end
  xpoly(0.5+(1:n)',valeurs1,type='lines');
  xset('mark',3);
  xpoly(0.5+(1:n)',valeurs1,type='marks');
  xtitle(titre);
endfunction

pareto(valeurs,cumsum(valeurs),noms,'Diagramme de Pareto ...',3);
////xs2ps(0,'pareto1.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 pareto1.ps')  /// \sleftarrow{calling a Unix script}

xclear();
pareto(cumsum(valeurs),cumsum(valeurs),noms,'Diagramme de Pareto cumule ...',3);
////xs2ps(0,'pareto2.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 pareto2.ps')  /// \sleftarrow{calling a Unix script}
