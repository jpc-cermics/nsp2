// See https://fr.wikipedia.org/wiki/Fonction_elliptique_de_Jacobi

u=linspace(-5,5,100);
kv=[0.1:0.05:0.95,0.96,0.97,0.98,0.99,0.999];
A=ones(size(kv,'*'),size(u,'*'));
for k=1:size(kv,'*');
  A(k,:)=amell(u,kv(k));
end

plot2d(u',A');

xclick()
xbasc();
plot2d(u',sin(A'));

xclick()
xbasc();
plot2d(u',cos(A'));

xclick()
xbasc();
sn=sin(A');
B=0*sn;
for k=1:size(kv,'*');
    B(:,k) = sqrt(1- kv(k)^2*(sn(:,k).*sn(:,k)))
end

plot2d(u',B);

