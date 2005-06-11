/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * Nsp object packing, sending and receiving for pvm 
 *     Only implemented for string and matrices 
 *     FIXME: This should be changed when all the object will 
 *     be serialized in char *.
 *--------------------------------------------------------------------------*/

#include <stdlib.h>
#include <string.h>
#include "../../pvm3/include/pvm3.h"
#include "nsp/machine.h"
#include "nsp/object.h"
#include "sci_pvm.h"


int  nsp_pvm_pkmatrix(NspMatrix *M)
{
  int info,type=0;
  info = pvm_pkint(&type, 1, 1);
  if (info < 0) return info;
  info = pvm_pkint(&M->m, 1, 1);
  if (info < 0) return info;
  info = pvm_pkint(&M->n, 1, 1);
  if (info < 0) return info;
  info = pvm_pkbyte(&M->rc_type, 1, 1);
  if (info < 0) return info;
  if ( M->rc_type == 'r') 
    info = pvm_pkdouble(M->R,M->mn, 1);
  else     
    info = pvm_pkdouble(M->R,2*M->mn, 1);
  if (info < 0) return info;
  return 0;
} 

int nsp_pvm_upkmatrix(NspMatrix **M)
{
  int info,m,n;
  char c;
  info = pvm_upkint(&m, 1, 1);
  if (info < 0) return info ;
  info = pvm_upkint(&n, 1, 1);
  if (info < 0) return info ;
  info = pvm_upkbyte(&c, 1, 1);
  if (info < 0) return info ;
  if ((*M=nsp_matrix_create(NVOID,c,m,n))==NULLMAT) return -1; /* XXX */
  if ( (*M)->rc_type == 'r') 
    info = pvm_upkdouble((*M)->R,(*M)->mn, 1);
  else     
    info = pvm_upkdouble((*M)->R,2*(*M)->mn, 1);
  if (info < 0) return info;
  return 0;
} 

int  nsp_pvm_pksmatrix(NspSMatrix *M)
{
  int info,i,type=1;
  info = pvm_pkint(&type, 1, 1);
  if (info < 0) return info;
  info = pvm_pkint(&M->m, 1, 1);
  if (info < 0) return info;
  info = pvm_pkint(&M->n, 1, 1);
  if (info < 0) return info;
  for ( i = 0 ; i < M->mn ; i++)
    {
      int k=strlen(M->S[i]);
      info = pvm_pkint(&k, 1, 1);
      if (info < 0) return info;
      info = pvm_pkstr(M->S[i]);
      if (info < 0) return info;
    }
  return 0;
} 

int nsp_pvm_upksmatrix(NspSMatrix **M)
{
  int info,m,n,i;
  info = pvm_upkint(&m, 1, 1);
  if (info < 0) return info ;
  info = pvm_upkint(&n, 1, 1);
  if (info < 0) return info ;
  if ((*M=nsp_smatrix_create_with_length(NVOID,m,n,-1))==NULLSMAT) return -1; 
  for ( i = 0 ; i < (*M)->mn ; i++)
    {
      int size;
      info = pvm_upkint(&size,1,1);
      if (info < 0) return info;
      if (((*M)->S[i]= new_nsp_string_n(size))==NULLSTRING) return -1;
      info = pvm_upkstr((*M)->S[i]);
      if (info < 0) return info;
    }
  return 0;
} 

int nsp_pvm_send(int *tids,int ntids,NspObject *M,int msgtag)
{
  int info, bufid;
  bufid = pvm_initsend(PvmDataDefault);
  if (bufid < 0) {
    Scierror( "Error pvm_send - init: %d\n", bufid);
    return bufid;
  }
  if ( IsMat(M) )
    info = nsp_pvm_pkmatrix((NspMatrix *)M);
  else if ( IsSMat(M)) 
    info = nsp_pvm_pksmatrix((NspSMatrix *)M);
  else 
    {
      Scierror( "Error: pvm_send only works for Mat and SMat\n");
      return -1;
    }
  if (info < 0) {
    Scierror( "Error in nsp_pvm_pkmatrix %d\n", info);
    pvm_freebuf(bufid);
    return info;
  }
  return (ntids == 1) ? pvm_send(tids[0], msgtag):  pvm_mcast(tids,ntids, msgtag);
} 

int nsp_pvm_recv(int tid,int tag,NspObject **M)
{
  int bufid,msgbyte,msgtag,msgtid;
  int info,type;

  bufid = pvm_recv(tid,tag);
  if (bufid < 0) {
    Scierror( "Error pvm_recv: %d\n", bufid);
    return bufid ;
  }
  info = pvm_bufinfo(bufid, &msgbyte, &msgtag, &msgtid);
  if (info < 0) {
    Scierror( "Error pvm_recv: -bufinfo- %d\n", info);
    pvm_freebuf(bufid);
    return info;
  }
  
  info = pvm_upkint(&type, 1, 1);
  if (info < 0) return info;
  switch (type)
    {
    case 0: 
      info = nsp_pvm_upkmatrix((NspMatrix **) M);break;
    case 1: 
      info = nsp_pvm_upksmatrix((NspSMatrix **)M);break;
    default: 
      Scierror( "Error: pvm_recv wrong object type\n");
      return -1;
    }
  if ( info < 0) {
    Scierror( "Error malloc in pvm_recv\n");
    pvm_freebuf(bufid);
    return info;
  }
  return 0;
} 

