function []=emc2(filename)
// Visualise un resultat de type GNU.*
// generes par NSCK2E 
// en general un ensemble de segments 
// Copyright ENPC
  F=fopen(File_name,mode='r')
  xx=F.get_matrix[];
  F.close[];
  x=xx(:,1) 
  y=xx(:,2)
  y1=matrix(xx(:,2),2,size(xx,'r')/2); 
  x1=matrix(xx(:,1),2,size(xx,'r')/2); 
  rect=[min(x1),min(y1),max(x1),max(y1)];
  plot2d(1,1,[1],"031"," ",rect)
  xsegs(x1,y1);
endfunction 

function []=amdbaR(File_name)
// Read a file describing a MESH of finite elements 
// of triangle type 
// The file is of amdba type 
  if nargin == 0 ; File_name='MESH';end 
  F=fopen(File_name,mode='r')
  x=F.get_matrix[];
  noeuds=x(1);
  triang=x(2);
  noeul=F.get_matrix[];
  trianl=F.get_matrix[];
  noeuds=x(1);
  triang=x(2);
  F.close[];
  resume(noeuds,triang,noeul,trianl);
endfunction 

function []=meshvisu(col,rect)
// Mesh visualisation 
// uses global variables 
  if nargin<=0 then col=1;end
  if nargin<=1 then 
    rect=[min(noeul(:,2)),min(noeul(:,3)),max(noeul(:,2)),max(noeul(:,3))];end
  if nargin<=2;iso='1';end
  plot2d([],[],strf="031",rect=rect)
  xset("clipgrf");
  xx=trianl(:,2:4); xx=matrix(xx,prod(size(xx)),1);
  x=noeul(xx,2)
  triang=size(x,'*')/3
  x=matrix(x,triang,3);
  y=noeul(xx,3)
  y=matrix(y,triang,3);
  x=[x,x(:,1)]';y=[y,y(:,1)]'
  xpolys(x,y,col*ones(1,triang));
  xset("clipoff");
endfunction 

function []=nvisu(rect)
// Visualisation des noeuds 
  if nargin <=0; then 
    rect=[min(noeul(:,2)),min(noeul(:,3)),max(noeul(:,2)),max(noeul(:,3))];
  end
  plot2d([],[],strf="031",rect=rect)
  xset("clipgrf");
  bords=noeul(find(noeul(:,4)>0),:);
  [no,ign]=size(bords)
  for i=1:no
    xstring(bords(i,2),bords(i,3),string(bords(i,4)));
  end
  xset("clipoff");
endfunction 

function []=emc2V(i,j,k,sca,FN,rect)
// Visualise un champ de vitesse 
// la taille des vecteur est constante
// mais on change la couleur suivant l'intensite du champ
// Les vecteurs sont lus dans les colonnes i et j du fichier 
// du fichier FN qui continet k colonnes 
  plot2d([],[],strf="031",rect=rect)
  xset("clipgrf");
  if nargin <= 0 ; FN='MESH';end
  F=fopen(FN,mode='r');
  resu=F.get_matrix[];
  F.close[];
  resu=resu(:,[i,j]);
  nm=[]
  for i=1:noeuds; nm1=sqrt(resu(i,:)*resu(i,:)');nm=[nm,nm1];
    if nm1<>0,resu(i,:)=resu(i,:)/nm1;end
  end
  nmax=max(nm);if nmax<>0; nm=nm/nmax;end
  xarrows([noeul(:,2)-(1/sca)*resu(:,1),noeul(:,2)+(1/sca)*resu(:,1)]',...
	[noeul(:,3)-(1/sca)*resu(:,2),noeul(:,3)+(1/sca)*resu(:,2)]',...
	style=16*ones(nm)-16*nm);
  xset("clipoff");
endfunction 

function []=emc2C(i,j,FN,rect)
// iso contour de la fonction lineaire sur les 
// triangles du maillage on lit les valeurs de la fonction 
// aux noeuds du maillage sur la ieme colonne 
// du fichier file qui en contient j 
  F=fopen(FN,mode='r');
  resu=F.get_matrix[];
  F.close[];
  resu=resu(:,i);
  if nargin <=3; then 
    rect=[min(noeul(:,2)),min(noeul(:,3)),max(noeul(:,2)),max(noeul(:,3))];end
  fec(noeul(:,2),noeul(:,3),trianl,resu,strf="131",rect=rect);
endfunction 




