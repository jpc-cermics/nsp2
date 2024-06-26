// -*- Mode: nsp -*- 
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - Fran�ois Delebecque (INRIA)
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

function A=testmat1(a,n)
//eigen values are given by a dilation of nth roots of 1
  A=diag(a*ones(1,n-1),1)+diag((1/a)*ones(1,n-1),-1)
  A(1,n)=1/a;A(n,1)=a
endfunction

//-----------------------------------------------------------
//---------- op(A)*X + X*op(B) = C, (1a) --------------------
//-----------------------------------------------------------

//Small dimension
n=4;m=3;
//A and B full
A=testmat1(2.5,n);C=rand(n,m);B=testmat1(0.3,m);

// check that wrong calls are detected 
if execstr('X = linmeq(5,A,B,C)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,B)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,rand(3,3),B,C)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,rand(4,3),B,C)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,rand(3,2),C)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,B,rand(m,n))',errcatch=%t)==%t then pause,end; lasterror();

if execstr('X = linmeq(1,A,B,C,[0,0,0],8)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,B,C,[0,0,0],0,4)',errcatch=%t)==%t then pause,end; lasterror();

flag=[0,0,0];  
X = linmeq(1,A,B,C);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(1,A,B,C,flag,trans);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end

Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end

//A in Schur form
[U,A]=schur(A);flag=[0,1,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end

Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end


//A and B in schur form
[U,B]=schur(B);flag=[0,1,1];
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end

Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end

//B in schur form
A=testmat1(2.5,n);flag=[0,0,1];
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end


//A in hess form
[U,A]=hess(A);flag=[0,2,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end

Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end

//A and B in hess form
[U,B]=hess(B);flag=[0,2,2];
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end


//B in hess form
A=testmat1(2.5,n);flag=[0,0,2];
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B'-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X+X*B-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X+X*B'-C,1)>1000*%eps then pause,end


//Large dimension
n=23;m=17;
//A and B full
A=testmat1(2.5,n);C=rand(n,m);B=testmat1(0.3,m);
X = linmeq(1,A,B,C);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end
X = linmeq(1,A,B,C,flag);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
X = linmeq(1,A,B,C,[0,1,0]);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end
//A and B in schur form
[U,B]=schur(B);
X = linmeq(1,A,B,C,[0,1,1]);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end
//B in schur form
A=testmat1(2.5,n);
X = linmeq(1,A,B,C,[0,0,1]);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end

//A in hess form
[U,A]=hess(A);
X = linmeq(1,A,B,C,[0,2,0]);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end
//A and B in hess form
[U,B]=hess(B);
X = linmeq(1,A,B,C,[0,2,2]);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end
//B in hess form
A=testmat1(2.5,n);
X = linmeq(1,A,B,C,[0,0,2]);
if norm(A*X+X*B-C,1)>100000*%eps then pause,end
//-----------------------------------------------------------
//---------- op(A)*X*op(B) + X = C, (1b) --------------------
//-----------------------------------------------------------
//Small dimension
n=4;m=3;
//A and B full
A=testmat1(2.5,n);C=rand(n,m);B=testmat1(0.3,m);flag=[1,0,0];
// check that wrong calls are detected 
if execstr('X = linmeq(5,A,B,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,rand(3,3),B,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,rand(4,3),B,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,rand(3,2),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,B,rand(m,n),flag)',errcatch=%t)==%t then pause,end; lasterror();
 
if execstr('X = linmeq(1,A,B,C,flag,8)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(1,A,B,C,flag,0,4)',errcatch=%t)==%t then pause,end; lasterror();

X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end


Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end


//A in schur form
[U,A]=schur(A);
flag=[1,1,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end

//A and B in schur form
[U,B]=schur(B);
flag=[1,1,1];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end

//B in schur form
A=testmat1(2.5,n);
flag=[1,0,1];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end

//A in hess form
[U,A]=hess(A);
flag=[1,2,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end

//A and B in hess form
[U,B]=hess(B);
flag=[1,2,2];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end

//B in hess form
A=testmat1(2.5,n);
flag=[1,0,2];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
Schur=1;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end
Schur=2;
trans=0;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B+X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B'+X-C,1)>1000*%eps then pause,end
trans=2;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A'*X*B+X-C,1)>1000*%eps then pause,end
trans=3;
X = linmeq(1,A,B,C,flag,trans,Schur);
if norm(A*X*B'+X-C,1)>1000*%eps then pause,end

//Large dimension
n=23;m=17;
//A and B full
A=testmat1(2.5,n);C=rand(n,m);B=testmat1(0.3,m);
flag=[1,0,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
flag=[1,1,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end
//A and B in schur form
[U,B]=schur(B);
flag=[1,1,1];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end
//B in schur form
A=testmat1(2.5,n);
flag=[1,0,1];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end

//A in hess form
[U,A]=hess(A);
flag=[1,2,0];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end
//A and B in hess form
[U,B]=hess(B);
flag=[1,2,2];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end
//B in hess form
A=testmat1(2.5,n);
flag=[1,0,2];
X = linmeq(1,A,B,C,flag);
if norm(A*X*B+X-C,1)>100000*%eps then pause,end
//-----------------------------------------------------------
//---------- op(A)'*X + X*op(A) = C, (2a)--------------------
//-----------------------------------------------------------
//Small dimension
n=4;
//A  full
A=testmat1(2.5,n);C=rand(n,n);C=C+C';

// check that wrong calls are detected 
// if execstr('X = linmeq(2,A,C)',errcatch=%t)==%t then pause,end; lasterror();

if execstr('X = linmeq(2,A,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(2,rand(3,3),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(2,rand(4,3),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(2,A,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(2,A,rand(4,3),flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(2,A,rand(3,3),flag)',errcatch=%t)==%t then pause,end; lasterror();
 
if execstr('X = linmeq(2,A,C,flag,8)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(2,A,C,flag,0,4)',errcatch=%t)==%t then pause,end; lasterror();

function A=testmat2(a,n)
  A=testmat1(a,n); A = A+ eye(size(A))+diag([1,1],n-2); 
endfunction

A=testmat2(2.5,n);
X = linmeq(2,A,C);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end
flag=[0,0];
X = linmeq(2,A,C,flag);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(2,A,C,flag,trans);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(2,A,C,flag,trans);
if norm(A*X+X*A'-C,1)>1000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
flag=[0,1];
X = linmeq(2,A,C,flag);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(2,A,C,flag,trans);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(2,A,C,flag,trans);
if norm(A*X+X*A'-C,1)>1000*%eps then pause,end


//A in hess form
A=testmat2(2.5,n);[U,A]=hess(A);
flag=[0,2];
X = linmeq(2,A,C,flag);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(2,A,C,flag,trans);
if norm(A'*X+X*A-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(2,A,C,flag,trans);
if norm(A*X+X*A'-C,1)>1000*%eps then pause,end

//Large dimension
n=23;
//A  full
A=testmat2(2.5,n);C=rand(n,n);C=C+C';
X = linmeq(2,A,C);
if norm(A'*X+X*A-C,1)>100000*%eps then pause,end
flag=[0,0];
X = linmeq(2,A,C,flag);
if norm(A'*X+X*A-C,1)>100000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
flag=[0,1];
X = linmeq(2,A,C,flag);
if norm(A'*X+X*A-C,1)>100000*%eps then pause,end

//A in hess form
A=testmat2(2.5,n);[U,A]=hess(A);
flag=[0,2];
X = linmeq(2,A,C,flag);
if norm(A'*X+X*A-C,1)>100000*%eps then pause,end

//-----------------------------------------------------------
//---------- op(A)'*X*op(A) - X = C, (2b)--------------------
//-----------------------------------------------------------
//Small dimension
n=4;
//A  full
A=testmat1(2.5,n);C=rand(n,n);C=C+C';
flag=[1,0];
X = linmeq(2,A,C,flag);
if norm(A'*X*A-X-C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(2,A,C,flag,trans);
if norm(A'*X*A-X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(2,A,C,flag,trans);
if norm(A*X*A'-X-C,1)>1000*%eps then pause,end


//A in schur form
[U,A]=schur(A);
flag=[1,1];
X = linmeq(2,A,C,flag);
if norm(A'*X*A-X-C,1)>1000*%eps then pause,end
trans=0;
X = linmeq(2,A,C,flag,trans);
if norm(A'*X*A-X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(2,A,C,flag,trans);
if norm(A*X*A'-X-C,1)>1000*%eps then pause,end

//A in hess form
A=testmat1(2.5,n);[U,A]=hess(A);
flag=[1,2];
X = linmeq(2,A,C,flag);
if norm(A'*X*A-X-C,1)>1000*%eps then pause,end
trans=0;
X = linmeq(2,A,C,flag,trans);
if norm(A'*X*A-X-C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(2,A,C,flag,trans);
if norm(A*X*A'-X-C,1)>1000*%eps then pause,end

//Large dimension
n=23;
//A  full
A=testmat1(2.5,n);C=rand(n,n);C=C+C';
flag=[1,0];
X = linmeq(2,A,C,flag);
if norm(A'*X*A-X-C,1)>100000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
flag=[1,1];
X = linmeq(2,A,C,flag);
if norm(A'*X*A-X-C,1)>100000*%eps then pause,end

//A in hess form
A=testmat1(2.5,n);[U,A]=hess(A);
flag=[1,2];
X = linmeq(2,A,C,flag);
if norm(A'*X*A-X-C,1)>100000*%eps then pause,end
//-----------------------------------------------------------
//---   op(A)'*(op(X)'*op(X)) + (op(X)'*op(X))*op(A) =     --
//---                  -  op(C)'*op(C),               (3a) --
//-----------------------------------------------------------
//Small dimension
n=4;
//A  full
C=rand(n,n);A=testmat1(2.5,n);flag=[0,0];

// check that wrong calls are detected 
if execstr('X = linmeq(3,A,C,flag)',errcatch=%t)==%t then pause,end ; lasterror();
if execstr('X = linmeq(3,rand(3,3),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,rand(4,3),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,rand(4,3),flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,rand(3,3),flag)',errcatch=%t)==%t then pause,end; lasterror();
 
if execstr('X = linmeq(3,A,C,flag,8)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,C,flag,0,4)',errcatch=%t)==%t then pause,end; lasterror();

if execstr('X = linmeq(3,A,C)',errcatch=%t)==%t then pause,end; lasterror();
//shift poles to make all of them negative

function A=testmat3(a,n)
  A=testmat1(a,n);
  A= A - (2+a^2)/a*eye(size(A));
endfunction

A=testmat3(2.5,n);
X = linmeq(3,A,C);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end
flag=[0,0];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(3,A,C,flag,trans);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(3,A,C,flag,trans);
if norm(A*(X*X')+(X*X')*A'+C*C',1)>1000*%eps then pause,end


//A in schur form
[U,A]=schur(A);
flag=[0,1];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end
trans=0;
X = linmeq(3,A,C,flag,trans);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(3,A,C,flag,trans);
if norm(A*(X*X')+(X*X')*A'+C*C',1)>1000*%eps then pause,end

//A in hess form
A=testmat3(2.5,n);[U,A]=hess(A);
flag=[0,2];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end
trans=0;
X = linmeq(3,A,C,flag,trans);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(3,A,C,flag,trans);
if norm(A*(X*X')+(X*X')*A'+C*C',1)>1000*%eps then pause,end

//Large dimension
n=23;
//A  full
A=testmat3(2.5,n);C=rand(n,n);
X = linmeq(3,A,C);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>100000*%eps then pause,end
flag=[0,0];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>100000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
flag=[0,1];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>100000*%eps then pause,end

//A in hess form
A=testmat3(2.5,n);[U,A]=hess(A);
flag=[0,2];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)+(X'*X)*A+C'*C,1)>100000*%eps then pause,end
//-----------------------------------------------------------
//---   op(A)'*(op(X)'*op(X))*op(A) - op(X)'*op(X) =       --
//---                  -  op(C)'*op(C),               (3a) --
//-----------------------------------------------------------
//Small dimension
n=4;
//A  full
C=rand(n,n);
A=testmat1(2.5,n);
flag=[1,0];

if execstr('X = linmeq(3,A,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,rand(3,3),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,rand(4,3),C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,C,flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,rand(4,3),flag)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,rand(3,3),flag)',errcatch=%t)==%t then pause,end; lasterror();
 
if execstr('X = linmeq(3,A,C,flag,8)',errcatch=%t)==%t then pause,end; lasterror();
if execstr('X = linmeq(3,A,C,flag,0,4)',errcatch=%t)==%t then pause,end; lasterror();

//shift poles to make all of them negative
function A=testmat4(a,n),A=testmat1(a,n)/((2+a^2)/a),endfunction
A=testmat4(2.5,n);
flag=[1,0];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>1000*%eps then pause,end

trans=0;
X = linmeq(3,A,C,flag,trans);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(3,A,C,flag,trans);
if norm(A*(X*X')*A'-X*X'+C*C',1)>1000*%eps then pause,end


//A in schur form
[U,A]=schur(A);
flag=[1,1];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>1000*%eps then pause,end
trans=0;
X = linmeq(3,A,C,flag,trans);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(3,A,C,flag,trans);
if norm(A*(X*X')*A'-X*X'+C*C',1)>1000*%eps then pause,end

//A in hess form
A=testmat4(2.5,n);[U,A]=hess(A);
flag=[1,2];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>1000*%eps then pause,end
trans=0;
X = linmeq(3,A,C,flag,trans);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>1000*%eps then pause,end
trans=1;
X = linmeq(3,A,C,flag,trans);
if norm(A*(X*X')*A'-X*X'+C*C',1)>1000*%eps then pause,end

//Large dimension
n=23;
//A  full
A=testmat4(2.5,n);C=rand(n,n);
flag=[1,0];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>100000*%eps then pause,end
flag=[1,0];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>100000*%eps then pause,end

//A in schur form
[U,A]=schur(A);
flag=[1,1];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>100000*%eps then pause,end

//A in hess form
A=testmat4(2.5,n);[U,A]=hess(A);
flag=[1,2];
X = linmeq(3,A,C,flag);
if norm(A'*(X'*X)*A-X'*X+C'*C,1)>100000*%eps then pause,end


// ============================================================================
// Tests for riccati() function
// Added : 25/06/2007
// Thanks to Sabine GAUZERE
// ============================================================================

n = 10;
A = rand(n,n);
B = rand(n,n);
C = rand(n,n);
C = C*C';
R = rand(n,n);
R = R*R'+eye(size(R*R'));
B = B*inv(R)*B';

// Test de l'�quation en temps continu
X = riccati(A,B,C,'c','eigen');

// V�rification
C_cont = A'*X+X*A-X*B*X;

// Test de l'�quation en temps discret
F      = A;
B      = rand(n,n);
G1     = B;
G2     = R;
G      = G1/G2*G1';
H      = C;
[X1,X2]= riccati(F,G,H,'d','schur');

// V�rification
X      = X1/X2;
C_disc = F'*X*F-(F'*X*G1/(G2+G1'*X*G1))*(G1'*X*F)-X;

// Comparaison des diff�rents r�sultats obtenus
if norm(C_cont+C,1) > (100000*%eps) then pause,end
if norm(C_disc+H,1) > (100000*%eps) then pause,end
