// Cette fonction retourne la p valeur P(chi2>zeta_n) 
// du test du chi2 d'adequation de loi
// N est un vecteur ligne des occurences observees
// p0 est un vecteur ligne correspondant a la loi sous H0

function[proba]=test_chi2(N,p0)
  n=sum(N);// taille de l'echantillon observe
  // calcul de zeta_ n
  zeta_n=n*sum(((N/n-p0).^2)./p0);
  // nombre de degres de liberte  (= nombre de classes dans N-1)
  d= length(N)-1;
  // on calcule la proba pour un chi 2 à d-1 degres d'etre superieur a zeta
  [p,q]=cdfchi("PQ",zeta_n,d);
  proba=q;
endfunction;
