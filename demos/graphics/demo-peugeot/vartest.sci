function [Fobs,f_c1, f_c2,p_val]=vartest2(n1,Xbar,SX2,n2,Ybar,SY2,alpha)
  // Calcul de la statistique de Fisher Snedecor, Fobs
  // Calcul de les valeurs critiques du test bilateral, f_c1 et f_c2
  // pour le niveau alpha
  // Calcul de la p-valeur, p_val.
	printf("Comparaison des variances\n")
	Fobs=SX2/SY2;
	k1=n1-1;
	k2=n2-1;
	f_c1 = cdff("F",k1,k2,alpha/2,1-alpha/2);
	f_c2 = cdff("F",k1,k2,1-alpha/2,alpha/2);
	[p,q] = cdff("PQ",Fobs,k1,k2);
	p_val=2*min(p,q);
	printf("\nResultat test de Fisher:\n f=%f,\n R_c=]0,%f]+[%f,infini[,\n"+...
		   " p_val=%f",Fobs,f_c1,f_c2,p_val); 
endfunction;

