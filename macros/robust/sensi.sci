function [Se,R,T]=sensi(G,Sk,flag)
// [Se,R,T]=sensi(G,Sk) computes sensitivity functions
// If flag='o' or no flag:
// [Se;R;T]= [inv(eye+G*K);K*inv(eye+G*K);G*K*inv(eye+G*K)];
// 
// flag='i'
// [Si,Ri,Ti]= [inv(eye+K*G);G*inv(eye+K*G);K*G*inv(eye+K*G)];
//!
// Copyright INRIA

  if nargin==2 then flag='o';end
  select flag
   case 'o'
    ssflag=0;
    if type(G,'short')=='r' then G=tf2ss(G);ssflag=1;end
    if type(Sk,'short')=='r' then Sk=tf2ss(Sk);ssflag=ssflag+1;end
    [ny,nu]=size(G);Iu=eye(nu,nu);Iy=eye(ny,ny);
    Ouy=zeros(nu,ny);Oyu=zeros(ny,nu);Ouu=zeros(nu,nu);
    Oyy=zeros(ny,ny);
    W1=[Iy,Oyu,Oyy;
	Ouy,Iu,Ouy;
	-Iy,Oyu,Iy;
	Iy,Oyu,Oyy];
    W2=[Iy,-G;
	Ouy,Iu;
	Iy,Oyu];
    SRT=lft(W1*W2,Sk);
    Se=SRT(1:ny,:);
    R=SRT((ny+1):(ny+nu),:);
    T=SRT((nu+ny+1):(nu+ny+ny),:);
    if ssflag >0 then
      Se=ss2tf(Se);R=ss2tf(R);T=ss2tf(T);
    end
   case 'i'
    ssflag=0;
    if type(G,'short')=='r' then G=tf2ss(G);ssflag=1;end
    if type(Sk,'short')=='r' then Sk=tf2ss(Sk);ssflag=ssflag+1;end
    [ny,nu]=size(G);Iu=eye(nu,nu);Iy=eye(ny,ny);
    Ouy=zeros(nu,ny);Oyu=zeros(ny,nu);Ouu=zeros(nu,nu);
    Oyy=zeros(ny,ny);
    W1=[Iu,-Iu;
	Oyu,Oyu;
	Ouu,Iu;
	Oyu,Oyu];
    W2=[Ouy;Iy;Ouy;Iy];
    W3=[Iu,-Iu];
    P=W1+W2*G*W3;
    SRT=lft(P,Sk);
    Se=SRT(1:nu,:);
    R=SRT((nu+1):(ny+nu),:);
    T=SRT((nu+ny+1):(nu+ny+nu),:);
    if ssflag >0 then
      Se=ss2tf(Se);R=ss2tf(R);T=ss2tf(T);
    end
  end
endfunction
