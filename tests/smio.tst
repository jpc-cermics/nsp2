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
//   {"put", int_smio_put },
//   {"get", int_smio_get },
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
//   {"compress",int_smio_compress},
//   {"uncompress",int_smio_uncompress},
//   {"can_compress",int_smio_can_compress},
//   {"length", int_smio_length},
//   {"clear", int_smio_clear},
//   {"resize", int_smio_resize},

S=sopen(20);
S.printf["an uncompressed string"]
n=S.tell[];
D=S.compress[n]; // compress n characters in the buffer (starting at
                 // position 0) and store the result in a new SMio
E=D.uncompress[n]; // uncompress n characters in a new SMio
res=E.getstr[n=n]; 
S.seek[0]; // rewind S 
if res<>S.getstr[n=n]; then pause;end 






