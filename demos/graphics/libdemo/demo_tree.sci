
function []=demo_tree(n,m)
// a graphic display of a 3d tree 
// Quentin Quadrat 
  
  function F=feuille3D(X,M,e,vv)
    VertF=vv*ones(4,1);
    F=[e*[Feuille1;Feuille2;Feuille3;Feuille4;Feuille5;Feuille6;..
	  Feuille7;Feuille8;Feuille9;Feuille10;Feuille11;..
	  Feuille12;Feuille13],[VertF;VertF;VertF;VertF;VertF;VertF;..
		    VertF;VertF;VertF;VertF;VertF;jaune;jaune]];
    F=TransIm(RotIm(F,M),X)
  endfunction

  function [E]=entrenoeud3D(X,M,e)
    E=[e/0.6*[Entrenoeud1;Entrenoeud2;Entrenoeud3;Entrenoeud4;Entrenoeud5;Entrenoeud6;..
	      Entrenoeud7;Entrenoeud8],[Brun1;Brun1;Brun2;Brun2;Brun2;Brun3;Brun3;Brun3]]
    E=TransIm(RotIm(E,M),X)
  endfunction

  function [B]=bourgeon3D(X,M,e)
    B=[e*[Bourgeon1;Bourgeon2;Bourgeon3;Bourgeon4;Bourgeon5;Bourgeon6;Bourgeon7;Bourgeon8;..
	  Bourgeon9],[rose1;Mauve4;rose2;rose3;rose2;Mauve1;Mauve2;Mauve3;rose1]];
    B=TransIm(RotIm(B,M),X)
  endfunction

  function [A]=arbre3D(n,X,M,e)
    A=bourgeon3D(Ze,Id,3*e);
    for i=1:n,
      vv=12+ceil(3*rand(9,1));
      j=ceil(3*rand(1,1));
      j=2;
      
      if j==1 then
	//3 branches
	if i<n/4 then
	  dchape= exp(e*log(2));
	  A=[feuille3D([0;0;e;0],Id,2*e,vv(1));..
	     feuille3D([0;0;dchape;0],Ma,2*e,vv(2));..
	     feuille3D([0;0;3*e;0],Maa,2*e,vv(3));..
	     feuille3D([0;0;4*e;0],Ma,2*e,vv(4));..  // XXXX a faire / 1/Ma -> Ma 
	     feuille3D([0;0;5*e;0],Maa,2*e,vv(5));..// XXXX a faire / 1/Maa -> Maa
	     entrenoeud3D(Ze,Id,5*e);..
	     Simil([0;0;5*e;0],Ma,.9,A);..
	     Simil([0;0;5*e;0],Maa*Mb,.9,A)];

	else
	  A=[entrenoeud3D(Ze,Id,5*e);..
	     Simil([0;0;5*e;0],Ma,.9,A);..//
	     Simil([0;0;5*e;0],Maa*Mb,.9,A);..//
	     Simil([0;0;5*e;0],Mba,.8,A)];//
	end

      elseif j==2 then
	//2 branches
	if i<n/2 then
	  // en attendant que ^ soit implémenté 
	  dchape= exp(e*log(2));
	  A=[feuille3D([0;0;e;0],Id,2*e,vv(1));..
	     feuille3D([0;0;dchape;0],Ma,2*e,vv(2));..
	     feuille3D([0;0;3*e;0],Maa,2*e,vv(3));..
	     feuille3D([0;0;4*e;0],Ma,2*e,vv(4));..  // XXXX a faire / 1/Ma -> Ma 
	     feuille3D([0;0;5*e;0],Maa,2*e,vv(5));..// XXXX a faire / 1/Maa -> Maa
	     entrenoeud3D(Ze,Id,5*e);..
	     Simil([0;0;5*e;0],Ma,.9,A);..//
	     Simil([0;0;5*e;0],Mba,.7,A)];//
	else
	  A=[entrenoeud3D(Ze,Id,5*e);..
	     Simil([0;0;5*e;0],Ma,.9,A);..//
	     Simil([0;0;5*e;0],Mba,.8,A)];//
	end

      elseif j==3 then
	//branche morte
	// i=n;  // XXXX interdit en nsp pour l'instant on n'a pas le droit de changer 
	// une variable de boucle et non protégé pour l'instant je 
	// fais un break.
	A=[entrenoeud3D(Ze,Id,5*e);..
	   Simil([0;0;5*e;0],Ma,.9,A);..//
	   Simil([0;0;5*e;0],Mba,.7,A)];//
	break;
      end;  
    end;
    A=TransIm(RotIm(A,M),X);		
  endfunction

  function Imo=Simil(T,M,e,Imn)
    Mp=M'
    [np,nd]=size(Imn)
    Tp=ones(np,1)*T'
    S=e*eye(4,4)
    S(4,4)=1
    Imo=Imn*S*Mp+Tp
  endfunction

  function Imo=RotIm(Imn,M)
    Mp=M'
    Imo=Imn*Mp
  endfunction

  function Imo=TransIm(Imn,T)
    [np,nd]=size(Imn)
    Tp=ones(np,1)*T'
    Imo=Imn+Tp
  endfunction

  function []=plotT3(T3)
    [np,nd]=size(T3);
    nf=np/4;
    // XXX syntax in nsp  list(matrix(T3(:,3),4,nf),matrix(T3(:,4),4,nf)),
    X=matrix(T3(:,1),4,nf),
    Y=matrix(T3(:,2),4,nf),
    Z=matrix(T3(:,3),4,nf),
    plot3d(X,Y,Z,colors=matrix(T3(:,4),4,nf),flag=[2,2,0]);
  endfunction

  
  a=%pi/2; b=-%pi/6; f=1/3; q=1/24;
  Id=eye(4,4);
  Ze=zeros(4,1);

  //vv=13;
  //VertF=vv*ones(4,1);
  Vert=15*ones(4,1);
  Brun1=26*ones(4,1);
  Brun2=25*ones(4,1);
  Brun3=27*ones(4,1);
  Mauve1=22*ones(4,1);
  Mauve2=23*ones(4,1);
  Mauve3=24*ones(4,1);
  Mauve4=31*ones(4,1);
  jaune=3*ones(4,1);
  rose1=28*ones(4,1);
  rose2=29*ones(4,1);
  rose3=30*ones(4,1);
  rose4=32*ones(4,1);
  bleu=12*ones(4,1);

  Bourgeon1=0.05*[1,1,0;-1,1,0;-1,-1,0;1,-1,0];
  Bourgeon2=0.05*[-1,1,0;1,1,0;2,2,2;-2,2,2];
  Bourgeon3=0.05*[1,-1,0;2,-2,2;2,2,2;1,1,0];
  Bourgeon4=0.05*[-2,-2,2;2,-2,2;1,-1,0;-1,-1,0];
  Bourgeon5=0.05*[-2,2,2;-2,-2,2;-1,-1,0;-1,1,0];
  Bourgeon6=0.05*[2,2,2;0,0,15;0,0,15;2,-2,2];
  Bourgeon7=0.05*[2,-2,2;-2,-2,2;0,0,15;0,0,15];
  Bourgeon8=0.05*[-2,2,2;0,0,15;0,0,15;-2,-2,2];
  Bourgeon9=0.05*[-2,2,2;0,0,15;0,0,15;2,2,2];

  nb=0.2;
  Entrenoeud1=0.03*[1,0,0;1-nb,0-nb,20;sqrt(2)/2-nb,sqrt(2)/2-nb,20;sqrt(2)/2,sqrt(2)/2,0];
  Entrenoeud2=0.03*[sqrt(2)/2,sqrt(2)/2,0;sqrt(2)/2-nb,sqrt(2)/2-nb,20;0-nb,1-nb,20;0,1,0];
  Entrenoeud3=0.03*[-sqrt(2)/2,sqrt(2)/2,0;0,1,0;0-nb,1-nb,20;-sqrt(2)/2+nb,sqrt(2)/2-nb,20];
  Entrenoeud4=0.03*[-1,0,0;-sqrt(2)/2,sqrt(2)/2,0;-sqrt(2)/2+nb,sqrt(2)/2-nb,20;-1+nb,0-nb,20];
  Entrenoeud5=0.03*[0,-1,0;-sqrt(2)/2,-sqrt(2)/2,0;-sqrt(2)/2+nb,-sqrt(2)/2+nb,20;0-nb,-1+nb,20];
  Entrenoeud6=0.03*[-sqrt(2)/2,-sqrt(2)/2,0;-1,0,0;-1+nb,0-nb,20;-sqrt(2)/2+nb,-sqrt(2)/2+nb,20];
  Entrenoeud7=0.03*[1,0,0;sqrt(2)/2,-sqrt(2)/2,0;sqrt(2)/2-nb,-sqrt(2)/2+nb,20;1-nb,0-nb,20];
  Entrenoeud8=0.03*[sqrt(2)/2,-sqrt(2)/2,0;0,-1,0;0-nb,-1+nb,20;sqrt(2)/2-nb,-sqrt(2)/2+nb,20];

  Feuille9=[0.5,0.5,0;1.43,0.93,0;1.1,0.34,0;0.7,0.05,0];
  Feuille11=[0.7,0.05,0;1.1,0.34,0;1.2,0.1,0;0.95,0,0];
  Feuille10=[1.1,0.34,0;1.43,0.93,0;1.7,0.75,0;1.5,0.35,0];
  Feuille8=[1.29,1.05,0;1.84,1.6,0;1.71,1,0;1.45,0.95,0];
  Feuille7=[1.3,1.05,0;0.85,1.15,0;1.06,1.4,0;1.85,1.6,0]; 
  Feuille6=[1.06,1.4,0;1.1,1.57,0;1.35,1.75,0;1.85,1.6,0];
  Feuille5=[0.5,0.5,0;0.85,1.15,0;1.3,1.05,0;1.43,0.93,0];  
  Feuille4=[0.15,0.65,0;0.37,1.2,0;0.85,1.15,0;0.5,0.5,0];   
  Feuille3=[0.37,1.2,0;1,1.85,0;1.05,1.4,0;0.85,1.15,0];
  Feuille2=[0.37,1.2,0;0.4,1.6,0;0.65,1.8,0;1,1.85,0];
  Feuille1=[0.15,0.65,0;0,1,0;0.1,1.35,0;0.37,1.2,0];
  Feuille12=[-0.02,0.02,0.01;1,1,0.01;1.02,0.98,0.01;0.02,-0.02,0.01];
  Feuille13=[1,1,0.01;1.81,1.62,0.01;1.83,1.57,0.01;1.02,0.98,0.01];

  Ma=[cos(a),-sin(a),0,0; sin(a), cos(a),0,0; 0,0,1,0;0,0,0,1];
  Maa=Ma*Ma;
  Maaa=Ma*Maa;
  Mb=[1,0,0,0;0,cos(b),-sin(b),0;0,sin(b),cos(b),0;0,0,0,1];
  Mba=Mb*Ma;
  //age=ceil(m*rand(n,1));
  age=m;
  posxy=15*rand(n,2);
  //posxy=0*rand(n,2); // FIXME
  zer2=zeros(n,2);
  pos=[posxy,zer2];
  A=zeros(0,4);
  //A=fond();
  for i=1:n
    A=[A;arbre3D(m,pos(i,:)',Id,0.3)];
  end;
  plotT3(A);
endfunction



