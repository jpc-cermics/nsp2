// calcul de la densite
function y=densite_norm(m,s2,z)
    y=exp(-(z-m).^2/2/s2)/sqrt(2*%pi*s2);
endfunction;

function  bidon() /// @@prerequisite
// simulation
y=grand(m,n,'norm',m, s1)
endfunction /// @@prerequisite 

