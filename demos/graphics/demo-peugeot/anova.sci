function  bidon() /// @@prerequisite
  k = max(Grp);  // nombre de groupes
  for i=1:k, 
    n(i) = length(X(Grp==i)); // calcul de l'effectif n(i),
    moy(i) = sum(X(Grp==i))/n(i);  //  de la moyenne moy(i)
    ss(i)=sum(X(Grp==i).^2);
    s2(i)=ss(i)/(n(i)-1)- n(i)/(n(i)-1)*moy(i)^2;// et de la variance s2(i) du groupe i
  end;
  [n,moy,s2] // effectif, moyenne et variance par groupe
endfunction /// @@prerequisite 

function anova(n,moy,s2,alpha)
// Calcul de la table d'ANOVA
  N=sum(n);
  mg=sum(moy.*n)/N;
  SSM=sum(n.*(moy-mg).^2);
  SSE=sum((n-1).*s2);
  k=length(n);
  MSM = SSM/(k-1);  
  MSE = SSE/(N-k);
  // Fisher
  F = MSM/MSE; 
  // p-valeur
  [p, pv] = cdff("PQ",F,k-1,N-k); 
  // Quantile de la loi de Fisher
  x=cdff("F",k-1,N-k,1-alpha, alpha); 
  // Affichage de la table d'ANOVA
  printf("\n"); printf("TABLE D''ANALYSE DE LA VARIANCE\n\n");
  printf("Variabilite   SS     DF     MS  Fisher  p-valeur\n"); 
  printf("Interclasse %6.1f  %3d  %6.1f  %4.1f   %f\n",SSM,k-1,MSM,F,pv);
  printf("Intraclasse %6.1f  %3d  %6.1f\n",SSE,N-k,MSE);
  printf("Totale      %6.1f\n\n",SSE+SSM);
  printf("Quantile de la loi de Fisher d''ordre %f: \n",alpha);
  printf("%f\n",x);
endfunction;

