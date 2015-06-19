
function diag_effets(valeurs,noms,titre,couleur)
// Un diagramme d'effets 
  valeurs=valeurs(:);
  n = size(noms,'*');
  nv = size(valeurs,'*')
  if nv<> n then 
    error('diag_effets: first and second argument should have the same size');
    return 
  end
  rect=[1,min(valeurs),n+1,max(valeurs)];
  xsetech(frect=rect,arect=[0.125,0.125,0.125,0.35],fixed=%t,clip=%f,axesflag=0);
  alpha=0.125;
  x=ones_new(4,1)*(1:n) + [alpha;1-alpha;1-alpha;alpha]*ones_new(1,n);
  y=[0*valeurs,0*valeurs,valeurs,valeurs]';
  xfpolys(x,y,couleur*ones_new(1,n));
  // Il ne semble pas possible d'avoir une graduation en y sans l'avoir en x 
  for i=1:n 
    rect=xstringl(0,0,noms(i))
    xstring(i+0.5,min(valeurs) - 2*rect(4),noms(i),60);
  end
  xtitle(titre);
endfunction

valeurs=-3:3;
noms='noms '+string(valeurs);
diag_effets(valeurs,noms,'Diagramme d''effets',3);

