// -*- Mode: scilab -*- 
// Copyright (C) 2010 J.-Ph. Chancelier 
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
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 

//   {"close", int_smio_fclose},
//   {"putstr", int_smio_putstr},
//   {"getstr", int_smio_getstr },
//   {"eof", int_smio_eof},
//   {"seek", int_smio_seek },
//   {"tell", int_smio_tell },
//   {"clearerr", int_smio_clearerr },
//   {"error", int_smio_error },
//   {"get_matrix",int_smio_get_matrix},
//   {"get_lines", int_smio_get_lines}, 
//   {"get_smatrix",int_smio_get_smatrix},
//   {"put_matrix",int_smio_put_matrix}, 
//   {"put_smatrix",int_smio_put_smatrix}, 
//   {"print",int_smio_print},
//   {"printf",int_smio_printf},
//   {"scanf",int_smio_scanf},

// create a SMio object 
// sopen(int | string), note that int is just the initial size

// compress and uncompress
//------------------------
S=sopen(20);
S.printf["an uncompressed string"]
n=S.tell[];
D=S.compress[n]; // compress n characters in the buffer (starting at
                 // position 0) and store the result in a new SMio
E=D.uncompress[n]; // uncompress n characters in a new SMio
res=E.getstr[n=n]; 
S.seek[0]; // rewind S 
if res<>S.getstr[n=n]; then pause;end 


S=sopen(20);
n1=1000;
S.put[1:n1,type='d'];
n=S.tell[];
D=S.compress[n]; // compress n characters in the buffer (starting at
                 // position 0) and store the result in a new SMio
E=D.uncompress[n]; // uncompress n characters in a new SMio
res=E.get[n=n1,type='d']; 
if ~res.equal[(1:n1)] then pause;end 

// test get method 
// ---------------
str= 'abcdefghijk';
n=length(str);
S=sopen('abcdefghijk');
x=S.get[n=n,type='c'];
if ascii(x)<>str then pause;end
x=S.get[n=1,type='c'];
if ~isempty(x) then pause;end
if ~S.eof[] then pause;end 
// back 
S.seek[0];
x=S.get[n=2,type='d'];
if length(x)<> 1 then pause;end 
S.seek[0];
x=S.get[n=10,type='i'];
if length(x)<> 2 then pause;end 

// put then get 
//------------- 
S.seek[0]
S.put[1:4,type='d']
S.seek[0]
x=S.get[n=4,type='d']
if ~x.equal[1:4] then pause;end 
// int 
S.seek[0]
S.put[1:4,type='ui']
S.seek[0]
x=S.get[n=4,type='ui']
if ~x.equal[1:4] then pause;end 
// int little endian 
S.seek[0]
S.put[1:4,type='uil']
S.seek[0]
x=S.get[n=4,type='uil']
if ~x.equal[1:4] then pause;end 
// int big endian 
S.seek[0]
S.put[1:4,type='uib']
S.seek[0]
x=S.get[n=4,type='uib']
if ~x.equal[1:4] then pause;end 

// length, clear, resize 
// ----------------------
S=sopen(32);
if S.length[]<>32 then pause;end
S.put[34*ones(1,32),type='c'];
S.resize[64];
S.seek[0];
x=S.get[n=64,type='c'];
if or(x(1:32)<>34) then pause;end
if S.length[]<>64 then pause;end
S.clear[];
S.seek[0];
x=S.get[n=64,type='c'];
if or(x(1:64)<>-1) then pause;end








