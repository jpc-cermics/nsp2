function [xx,yy,zz]=tubelet(x,y,z,w,nf)
  //
  // This program is free software; you can redistribute it and/or modify
  // it under the terms of the GNU General Public License as published by
  // the Free Software Foundation; either version 2 of the License, or
  // (at your option) any later version.
  //
  // This program is distributed in the hope that it will be useful,
  // but WITHOUT ANY WARRANTY; without even the implied warranty of
  // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  // GNU General Public License for more details.
  //
  // You should have received a copy of the GNU General Public License
  // along with this program; if not, write to the Free Software
  // Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  //
  // Adapted from Enrico Segre scicoslab toolbox.
  // Copyright (C) 1998-2017 - Enrico Segre


  xx=[];

  if nargin<5 then nf=3; end

  if nargin<3 then 
    printf("%s\n",'tubelet() wants one or more trajectories x,y,z !\n')
    return
  end

  if nargin==3 then
    w=max([max(x)-min(x), max(y)-min(y), max(z)-min(z)])/20
  end  //just a given value

  if size(x)~=size(y) | size(z)~=size(y) | size(x)~=size(z) then
    printf("%s\n",'inconsistency in x,y,z lengths\n')
    return
  end

  np=size(x,1); nv=size(x,2);
  if np==1 then 
    printf("%s\n",'cannot make tubes of single points!\n')
    return
  end

  if size(w)==[1,np] then
    w=w'
  end
  if size(w)==[np,1] then
    w=w*ones(1,nv)
  end

  if size(w,1)<>np | size(w,2)<>nv then 
    w=w(1)*ones(np,nv);
  end

  fd=[cos(2*%pi*(1:nf)/nf);sin(2*%pi*(1:nf)/nf)]

  //vectorized code for np=2. Much more efficient, less checks.
  if np==2 then
    //array of versors of the segments
    un=[x(2,:)-x(1,:); y(2,:)-y(1,:); z(2,:)-z(1,:)];
    nn=sqrt(sum(un.^2,'r')); 
    nn=[nn;nn;nn];
    un=un./nn;
    //compute 2 local normals to each point of the trail
    //this is for sure perpendicular to un
    N1=[un(2,:);-un(1,:);zeros(1,nv)]
    //fix segments // z
    j=find(abs(un(1,:))<10*%eps & abs(un(2,:))<10*%eps)
    N1(:,j)=[ones(size(j));zeros(size(j));zeros(size(j))]
    nN1=sqrt(sum(N1.^2,'r')); 
    //normalization
    N1=N1./[nN1;nN1;nN1];
    //intermediate points and last: second normal=binormal to the previous
    i123=1:3; i231=[2 3 1]; i312=[3 1 2]
    N2=un(i231,:).*N1(i312,:)-un(i312,:).*N1(i231,:);
    //N2 is already normalized
    //generate a vector of vertices of the facelet, still to sort
    //common subarrays are out of the loop
    M1=[N1(1,:); N2(1,:)]'*fd 
    M2=[N1(2,:); N2(2,:)]'*fd   
    M3=[N1(3,:); N2(3,:)]'*fd 
    jk1=ones(1,nf); jk2=2*jk1;
    xt1=x(jk1,:)'+M1.*w(jk1,:)'   //xt1(nv,nf)
    yt1=y(jk1,:)'+M2.*w(jk1,:)'
    zt1=z(jk1,:)'+M3.*w(jk1,:)'
    xt2=x(jk2,:)'+M1.*w(jk2,:)'
    yt2=y(jk2,:)'+M2.*w(jk2,:)'
    zt2=z(jk2,:)'+M3.*w(jk2,:)'

    // generate the faces of the tube
    jpf1=[1:nf,2:nf,1]; jpf2=[2:nf,1,1:nf];
    xx=matrix([xt2(:,jpf1), xt1(:,jpf2)],nf*nv,4)'
    yy=matrix([yt2(:,jpf1), yt1(:,jpf2)],nf*nv,4)'
    zz=matrix([zt2(:,jpf1), zt1(:,jpf2)],nf*nv,4)'
  else
    //code for np>2:
    xx=zeros(4,nf*(np-1)*nv); yy=xx; zz=yy;  // only (np-1)*nv tubelets,
    //common arrays dimensioned out of the loop
    xt=zeros(np,nf); yt=xt; zt=yt; 
    un=zeros(np-1,3);
    N1=zeros(np,3);    N2=zeros(np,3);
    np1=1:(np-1); np2=2:np; np1m=1:(np-2); np2m=2:(np-1)
    k1=1:nf; k2=[2:nf,1]; np1f=(np-1)*nf;
    np1k1=(ones(size(k1'))*np1 + (k1'-1)*ones(size(np1))*np)'; np1k1=np1k1(:)
    np1k2=(ones(size(k2'))*np1 + (k2'-1)*ones(size(np1))*np)'; np1k2=np1k2(:)
    np2k1=(ones(size(k1'))*np2 + (k1'-1)*ones(size(np2))*np)'; np2k1=np2k1(:)
    np2k2=(ones(size(k2'))*np2 + (k2'-1)*ones(size(np2))*np)'; np2k2=np2k2(:)
    i123=1:3; i231=[2 3 1]; i312=[3 1 2]

    for j=1:nv
      xj=x(:,j); yj=y(:,j); zj=z(:,j); wj=w(:,j); 
      //array of versors of the segments
      un=[xj(np2)-xj(np1), yj(np2)-yj(np1), zj(np2)-zj(np1)];
      nn=sqrt(sum(un.^2,'c')); 
      // take care of possible coincident points: assign an un<>0
      // even to those coincident
      isep=find(nn>10*%eps);
      if ~isempty(isep) then   
	// if all the points coincide, don't even try to mend anything
	// (a division by 0 will result soon)
	un(1:(isep(1)-1),1)=un(isep(1),1);
	un(1:(isep(1)-1),2)=un(isep(1),2);
	un(1:(isep(1)-1),3)=un(isep(1),3);
	nn(1:(isep(1)-1))=nn(isep(1))
	//so if the first points were coincident, they take the un of their
	//  followers
      else
	printf("%s\n","all the points of line "+string(j)+" coincide!")
	break
      end
      // now each nn=0 has for sure a predecessor <>0
      for i=find(nn<=10*%eps); 
	un(i,:)=un(i-1,:); nn(i)=nn(i-1);
      end
      // the loop above is not vectorizable! Feedback! 
      nn=[nn,nn,nn];
      un=un./nn;
      //compute 2 local normals to each point of the trail
      //intermediate points: first normal=bisectrix of the vertex
      N1(np2m,:)=un(np1m,:)-un(np2m,:); 
      //extremes: equal that at the next (previous) point
      N1(1,:)=-N1(2,:); N1(np,:)=-N1(np-1,:);
      //really no idea of why N1(1,:)=-N1(2,:) with minus, but solves a bug.
      //now, we could have N1(:,i)=[0;0;0] either for coincident
      // points or for colinear segments. Let's take care of it.
      // We cannot just set N1 to an arbitrary direction, because
      //  that could be too different from its neighbor.
      nN1=sqrt(sum(N1.^2,'c'));
      if isempty(find(abs(nN1)>10*%eps)) | np==2 then
	//if _all_ the points are colinear (this can happen in arrow mode)
	if isempty(find(abs(un(:,1))>10*%eps|abs(un(:,2))>10*%eps)) then
	  //if the whole trail is parallel to the z axis
	  N1=[ones(np,1),zeros(np,2)]
	else
	  N1(np2,:)=[un(:,2),-un(:,1),zeros(np-1,1)]       
	  //this is for sure perpendicular to un 
	  N1(1,:)=N1(2,:); N1(np,:)=N1(np-1,:);
	end
	nN1=sqrt(sum(N1.^2,'c')); // recalculate for the points which
	// have been taken care of
      else
	//now, if not all points are colinear, lets put the missing N1s
	// (for those which are still colinear). Let's start from the
	// beginning of the trail:
	ibent=find(nN1>10*%eps); ib1=1:(ibent(1)-1)
	N1(ib1,1)=N1(ibent(1),1);
	N1(ib1,2)=N1(ibent(1),2);
	N1(ib1,3)=N1(ibent(1),3);
	nN1(ib1)=nN1(ibent(1))
	// now each N1=0 has for sure a predecessor <>0
	for i=find(nN1<=10*%eps);
	  N1(i,:)=N1(i-1,:); nN1(i)=nN1(i-1);
	end
	// still one thing to do - if the trail is even a little zigzag,
	//  neighboring points can have almost opposite N1. Let's flip
	//  those which have a negative projection on their predecessor
	P1=sign(sum(N1(np1,:).*N1(np2,:),2))
	iflip=find(cumprod(P1)<0)+1    // I'm really clever!
	N1(iflip,:)=-N1(iflip,:)
      end
      //normalization
      N1=N1./[nN1,nN1,nN1];
      //intermediate points and last: second normal=binormal to the previous
      N2(np2,i123)=un(np1,i231).*N1(np2,i312)-un(np1,i312).*N1(np2,i231);   
      //first point: equal to the next
      N2(1,:)=N2(2,:);
      //normalization
      nN1=sqrt(sum(N2.^2,'c'));
      nN1(find(abs(nN1)<%eps))=1;
      N2=N2./[nN1,nN1,nN1];
      //Sometimes the intrinsic twist of the line is such, that it is better
      // to further rotate N1 and N2 of an integer number of quarter of turns
      // around their commom normal, i.e. send (N1,N2) --> (+-N2,+-N1)
      // in order to minimize the angles between corresponding normals of
      // neighboring points. This should be done here.

      // TO DO

      //generate a vector of vertices of the facelet, still to sort
      //common subarrays out of the loop
      M1=[N1(:,1) N2(:,1)]; M2=[N1(:,2) N2(:,2)]; M3=[N1(:,3) N2(:,3)]
      jk=ones(nf,1)*(1:np)
      xt=matrix(xj(jk),nf,np)'+(M1*fd).*matrix(wj(jk),nf,np)'
      yt=matrix(yj(jk),nf,np)'+(M2*fd).*matrix(wj(jk),nf,np)'
      zt=matrix(zj(jk),nf,np)'+(M3*fd).*matrix(wj(jk),nf,np)'

      // generate the faces of the tube
      jpf=(j-1)*np*nf+(1:np1f)
      xx(:,jpf)=[xt(np1k1) xt(np2k1) xt(np2k2) xt(np1k2)]'
      yy(:,jpf)=[yt(np1k1) yt(np2k1) yt(np2k2) yt(np1k2)]'
      zz(:,jpf)=[zt(np1k1) zt(np2k1) zt(np2k2) zt(np1k2)]'
    end
  end
endfunction

