#ifndef NSP_LINK 
#define NSP_LINK 

/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

extern void GetDynFunc ( int ii, int (**realop)());
extern int SearchInDynLinks (char *op, int (**realop)());
extern void SciLinkInit (void);
extern void  ShowDynLinks (void);
extern void SciLink (int iflag,int *rhs,int *ilib,char *files[],
			     char *en_names[],char *strf);

extern void SciDynLoad (char **files,char **en_names,char *strf,
				int *ilib,int iflag,int *rhs);

extern void C2F(iislink) (   char *buf,   integer *irep);
extern void C2F(isciulink) ( integer *i);
extern int nsp_link_status  (void);

void AddInter (char **files, char *iname, char **enames, int *err);
int  BlankInterface (int i, char *fname, int first, int rhs, int opt, int lhs);
void RemoveInterf (int Nshared);

#endif 
