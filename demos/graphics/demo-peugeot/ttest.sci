function [Tobs,vc,p_val]=ttest2(n1,Xbar,SX2,n2,Ybar,SY2,alpha)
  // Calcul de la statistique de student, Tobs
  // Calcul de la valeur critique du test, vc, pour le niveau alpha
  // Calcul de la p-valeur, p_val.
	k=n1+n2-2; //nbre degre de liberte
	t=(Xbar-Ybar)*sqrt(k)/sqrt(1/n1+1/n2);
	Tobs=t/sqrt((n1-1)*SX2+(n2-1)*SY2);
	vc= cdft("T",k,1-alpha,alpha);
	[p,p_val] = cdft("PQ",Tobs,k);
	mot="\nResultat du test de Student:\n T observe=%f,\n '
	mot=mot+'Region critique =[%f,infini[,\n p_val=%f.\n\n"
	printf(mot,Tobs,vc,p_val);
endfunction;
