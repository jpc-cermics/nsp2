#ifndef NSP_ADDINTER 
#define NSP_ADDINTER 

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

/* the first dynamic interface is at position DYN_INTERF_START+1 */
#define DYN_INTERF_START 500
#define MAXINTERF 50

typedef struct 
{
  char name[NAME_MAXL];  /* name of interface */
  int (*func)();         /* entrypoint for the interface */
  int (*func_info)();    /* entrypoint for the interface */
  int Nshared;           /* id of the shared library */
  int ok;                /* flag set to 1 if entrypoint can be used */
} Iel;

extern Iel DynInterf[MAXINTERF];
extern int LastInterf;

int nsp_dynamic_interface(nsp_const_string shared_lib,nsp_const_string interface,int ilib);
int BlankInterface (int i, char *fname, int first, int rhs, int opt, int lhs);
void RemoveInterf (int Nshared);


#endif 
