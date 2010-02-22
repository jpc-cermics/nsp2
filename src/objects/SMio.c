/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

/* See also the Copyright at the end of the file for the swapxxx 
 * functions. 
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

#include <math.h>
#include <stdio.h>
#include <string.h> 
#include <string.h>
#include <ctype.h>  /* isdigit */

#include "nsp/object.h"
#include "nsp/interf.h"
#include "../system/files.h" /* FSIZE */
#include "nsp/smio.h"

static void swapb(const char *l, char *f, int n);
static unsigned short swapw(unsigned short us);
static unsigned long swapl(long unsigned int ul);
static unsigned int swapi(unsigned int ul);
static float swapf(float uf);
static double swapd(double df);
char *strerror(int errcode);
static int swap = 0;  /* A corriger XXXXX */

/**
 * nsp_smio_wopen:
 * @mode: 
 * @xdr_on: 
 * @swap_on: 
 * @len: 
 * 
 * 
 * 
 * Returns: 
 **/

NspSMio *nsp_smio_wopen(char *mode,int xdr_on,int swap_on,unsigned int len)
{
  NspSMio *F;
  if ((F =nsp_smio_wcreate(NVOID,mode,0,len)) == NULLSMIO )
    {
      return(NULLSMIO);
    }
  if ( xdr_on == TRUE) XDR_ON(F->obj->flag);
  if ( swap_on == TRUE) SWAP_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  return F;
}

/**
 * nsp_smio_ropen:
 * @mode: 
 * @xdr_on: 
 * @swap_on: 
 * @str: 
 * @len: 
 * 
 * 
 * 
 * Returns: 
 **/

NspSMio *nsp_smio_ropen(char *mode,int xdr_on,int swap_on,const char *str, unsigned int len)
{
  NspSMio *F;
  if ((F =nsp_smio_rcreate(NVOID,mode,0,str,len)) == NULLSMIO )
    {
      return(NULLSMIO);
    }
  if ( xdr_on == TRUE) XDR_ON(F->obj->flag);
  if ( swap_on == TRUE) SWAP_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  return F;
}


/**
 *nsp_smio_close:
 * @F: a  #Nspfile Object
 * 
 * Close the file described by F. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_smio_close(NspSMio  *F)
{
  if ( !IS_OPENED(F->obj->flag))
    {
      Sciprintf("Warning: string is already closed\n");
      return OK;
    }
  OPEN_OFF(F->obj->flag);
  F->obj->pos = 0;
  return OK;
}

#define assertR(ex) {if (!(ex)){ Scierror("Error: xdr read failed\n");return(FAIL);}}
#define assertW(ex) {if (!(ex)){ Scierror("Error: xdr write failed\n");return(FAIL);}}

/*
 * Open a SMio for xdr reading 
 */

#define SCIF_V 256

/**
 *nsp_smio_open_xdr_r:
 * @fname: file name 
 * 
 * Opens a file given its name for xdr reading 
 * 
 * Return value: a #NspSMio object 
 **/

NspSMio *nsp_smio_open_xdr_r(const char *fname)
{
#if defined(__STDC__) ||  defined(_MSC_VER)
  static char openf[]="rb";
#else
  static char openf[]="r";
#endif
  char SciF_version[SCIF_V];
  FILE *f;
  NspSMio *F;
  if((f=fopen(fname,openf)) == NULL)
    {
      Scierror("Error: fopen failed for file %s\n",fname) ;
      return(NULLSMIO);
    }
  if ((F =nsp_smio_rcreate(NVOID,openf,0,NULL,512)) == NULLSMIO )
    {
      fclose(f);
      return(NULLSMIO);
    }
  XDR_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  /* XXX 
  xdrstdio_create(F->obj->xdrs,F->obj->file, XDR_DECODE) ;
  */
  if (nsp_xdr_load_string(F->obj->xdrs,SciF_version,SCIF_V) == FAIL ) 
    {
      /* clear the xdr message */
      nsp_error_message_clear();
      Scierror("Error: Wrong xdr header in file: %s expecting %s\n",fname,"NspXdr_1.0");
      return NULLSMIO;
    }
  if  (strcmp(SciF_version,"NspXdr_1.0") != 0 && strcmp(SciF_version,"SciXdr1.0") != 0 )
    {
      Scierror("Error: SMio %s with Wrong header %s, expecting  %s or %s.\n",
		fname,SciF_version,"NspXdr_1.0","SciXdr1.0");
      return NULLSMIO;
    }
  return F;
}

/*
 * Close a SMio opened for xdr read 
 */

#define TYPE_S 256

/**
 *nsp_smio_close_xdr_r:
 * @F: a  #Nspfile Object
 * 
 * Close the file described by F. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_smio_close_xdr_r(NspSMio  *F)
{
  int rep =OK;
  static char type[TYPE_S];
  if ( !IS_XDR(F->obj->flag))
    {
      Scierror("Error:string is not an xdr stream\n");
    }
  if ( !IS_OPENED(F->obj->flag))
    {
      Sciprintf("Warning:string is already closed\n");
      return OK;
    }
  nsp_xdr_load_string(F->obj->xdrs,type,TYPE_S) ;
  if ( strcmp(type,"endsave") != 0)
    {
      Scierror("Warning: Closing xdr stream while not at end of file\n");
      rep = FAIL;
    }
  /* FIXME : here assertR does not work on macOSX 
     assertR(fflush((FILE *) F->obj->xdrs->x_private) != EOF) ; 
     xdr_destroy(F->obj->xdrs);
     assertR(fclose(F->obj->file) != EOF) ;
  */
  fflush((FILE *) F->obj->xdrs->x_private);
  xdr_destroy(F->obj->xdrs);
  /* fclose(F->obj->file); */
  OPEN_OFF(F->obj->flag);
  return rep;
}

/**
 *nsp_smio_open_xdr_w:
 * @fname: file name 
 * 
 * Opens a file given its name for xdr writing 
 * 
 * Return value: a #NspSMio object 
 **/

NspSMio *nsp_smio_open_xdr_w(const char *fname)
{
  char scis[]={"NspXdr_1.0"};
  FILE *f;
  NspSMio *F;
#if defined(__STDC__) ||  defined(_MSC_VER)
  static char openf[]="wb";
#else
  static char openf[]="w";
#endif
  if((f=fopen(fname,openf)) == NULL)
    {
      Scierror("Error: fopen failed for file %s\n",fname) ;
      return(NULLSMIO);
    }
  if ((F =nsp_smio_wcreate(NVOID,openf,0,512)) == NULLSMIO )
    {
      fclose(f);
      return(NULLSMIO);
    }
  XDR_ON(F->obj->flag);
  OPEN_ON(F->obj->flag);
  /* 
     xdrstdio_create(F->obj->xdrs, F->obj->file, XDR_ENCODE) ;
  */
  nsp_xdr_save_string(F->obj->xdrs,scis);
  return F;
}

/**
 *nsp_smio_close_xdr_w:
 * @F: a  #Nspfile Object
 * 
 * Close the file described by @F. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_smio_close_xdr_w(NspSMio  *F)
{
  if ( !IS_XDR(F->obj->flag))
    {
      Scierror("Error: close: string stream was not opened for xdr writting\n");
      return FAIL;
    }
  if ( !IS_OPENED(F->obj->flag))
    {
      Sciprintf("Warning: string stream is already closed\n");
      return OK;
    }
  nsp_xdr_save_string(F->obj->xdrs,"endsave");
  assertW(fflush((FILE *) F->obj->xdrs->x_private) != EOF) ; 
  xdr_destroy(F->obj->xdrs);
  /*
    assertW(fclose(F->obj->file) != EOF) ;
  */
  OPEN_OFF(F->obj->flag);
  return OK;
}

/*-------------------------------------------------------
 * A set of functions for reading and writing binary 
 * datas in a machine independant way.
 *-------------------------------------------------------*/

/**
 * nsp_smio_eof:
 * @F: a #NspSMio
 * 
 * checks if eof was reached in #NspSMio @f.
 *  
 * Return value: %TRUE or %FALSE
 **/

int nsp_smio_eof(NspSMio *F)
{       
  if ( !IS_OPENED(F->obj->flag)) return TRUE;
  return ( F->obj->pos < F->obj->len ) ? FALSE: TRUE;
}

/**
 * nsp_smio_error:
 * @f: a #NspSMio
 * 
 * checks if an error was raised in #NspSMio @f.
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_smio_error(NspSMio *f)
{       
  if ( !IS_OPENED(f->obj->flag)) return FALSE;
  return FALSE;
}

/**
 * nsp_smio_clearerr:
 * @f: a #NspSMio
 * 
 * clears raised error if any in #NspSMio @f
 * 
 **/

void nsp_smio_clearerr(NspSMio *f)
{       
  if ( !IS_OPENED(f->obj->flag)) return ; 
}

/**
 * nsp_smio_seek:
 * @F: a #NspSMio
 * @offset: a long int 
 * @flag: a string from "set",or "cur" ,or "end"
 * 
 * call the fseek function. 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_seek(NspSMio *F,long int offset,const char *flag)
{     
#if (defined(sun) && !defined(SYSV)) || defined(sgi)
  int irep;
#endif
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  if ( strncmp(flag,"set",3)==0 ) 
    {
      F->obj->pos= offset ;
    }
  else if ( strncmp(flag,"cur",3)==0 ) 
    {
      F->obj->pos += offset ;
    }
  else if ( strncmp(flag,"end",3)==0 ) 
    {
      F->obj->pos = F->obj->len + offset;
    }
  else 
    {
      Scierror("fseek : flag = %s not recognized\n",flag);
      return FAIL;
    }
  F->obj->pos = Min(Max(F->obj->pos,0),F->obj->len);
  return OK;
}

/**
 * nsp_smio_tell:
 * @F: a #NspSMio
 * @offset: a pointer to a long int 
 * 
 * calls the tell function 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_tell(NspSMio *F,long int *offset)
{     
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  *offset = F->obj->pos;
  return OK;
}

/*
 * mput 
 *
 */

#define MPUT_CHARS_(x,n,obj,Type)					\
  if ( obj->pos + n*sizeof(Type) > obj->len)				\
    {									\
      obj->D = realloc(obj->D,obj->pos + n*sizeof(Type));		\
      if ( obj->D == NULL)						\
	{								\
	  Scierror("Error: cannot enlarge smio buffer\n");		\
	  return FAIL;							\
        }								\
      obj->len = obj->pos + n*sizeof(Type);				\
    }									\
  memcpy(obj->D+obj->pos,(Type *) x,n*sizeof(Type));			\
  obj->pos += n*sizeof(Type);


#define MPUT_(x,n,obj,swap,Type,Fswap,str)				\
  {									\
    Type *val = (Type *) x ;						\
    Type vali;								\
    if ( obj->pos + n*sizeof(Type) > obj->len)				\
      {									\
	obj->D = realloc(obj->D,obj->pos + n*sizeof(Type));		\
	if ( obj->D == NULL)						\
	  {								\
	    Scierror("Error: cannot enlarge smio buffer\n");		\
	    return FAIL;						\
	  }								\
	obj->len = obj->pos + n*sizeof(Type);				\
      }									\
    for ( i=0; i < n; i++)						\
      {									\
	vali = *val++;							\
	if (swap) vali = Fswap(vali);					\
	memcpy(obj->D+obj->pos,&vali,sizeof(Type));			\
	obj->pos += sizeof(Type);					\
      }									\
  }

/**
 * nsp_smio_put:
 * @F: a #NspSMio
 * @x: a pointer to an array 
 * @n: size of array as an integer 
 * @type: type of @x coded as string.
 *
 * writes array @x of size @n in the stream described by @F 
 * using type described by @type and controlling the byte order.
 * This routine assume that the array @x is type compatible wih @type
 * i.e Type *val = (Type *) x and *val++ works for iterating on x 
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_smio_put(NspSMio *F,void *x,int n, char *type)
{  
  char swap_c,c1,*data;
  int i,swap;
  if ( strlen(type) == 0) 
    {
      Scierror("mput: format is of length 0\n");
      return FAIL;
    }
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  
  swap = ( USE_SWAP(F->obj->flag) ) ? (( is_little_endian() ) ? FALSE :  TRUE) : FALSE; 
  /* check if swap was given in type */
  swap_c = type[strlen(type)-1]; 
  if (swap_c == 'b' ) 
    swap = ( is_little_endian() ) ? TRUE : FALSE ;
  else if (swap_c == 'l' ) 
    swap = ( is_little_endian() ) ? FALSE : TRUE ;

  data = F->obj->D+ F->obj->pos;
  
  switch ( type[0] )
    {
    case 'i' : MPUT_(x,n,F->obj,swap,int,swapi,"int");       break;
    case 'l' : MPUT_(x,n,F->obj,swap,long,swapl,"long");      break;
    case 's' : MPUT_(x,n,F->obj,swap,short,swapw,"short");     break;
    case 'c' : MPUT_CHARS_(x,n,F->obj,char) ;                 break;
    case 'd' : MPUT_(x,n,F->obj,swap,double,swapd,"double");    break;
    case 'f' : MPUT_(x,n,F->obj,swap,float,swapf,"float");     break;
    case 'u' :
      c1=( strlen(type) < 1) ? ' ': type[1];
      switch ( c1)
	{
	case 'i' :  MPUT_(x,n,F->obj,swap,unsigned int,swapi,"int"); break;
	case 'l' :  MPUT_(x,n,F->obj,swap,unsigned long,swapl,"long"); break;
	case 's' :  MPUT_(x,n,F->obj,swap,unsigned short,swapw,"short"); break;
	case ' ' :  MPUT_(x,n,F->obj,swap,unsigned int,swapi,"int"); break;
	case 'c' :  MPUT_CHARS_(x,n,F->obj,unsigned char); break;
	default : 
	  Scierror("mput '%c' unrecognized conversion character\n",c1);
	  return FAIL;
	}
      break; 
    default : 
      Scierror("mput '%c' unrecognized conversion character\n",type[0]);
      return FAIL;
      break;
    }
  return OK;
}



#define MGET_CHARS_(x,n,obj,Type)					\
  {									\
    int nc = obj->len - obj->pos ;					\
    items= Min(nc/sizeof(Type),n);					\
    memcpy((Type *) x,obj->D+obj->pos,items*sizeof(Type));		\
    obj->pos += items*sizeof(Type);					\
  }

#define MGET_(x,n,obj,swap,Type,Fswap)					\
  {									\
    int nc = obj->len - obj->pos ;					\
    Type *val = (Type *) x ;						\
    items= Min(nc/sizeof(Type),n);					\
    memcpy(val,obj->D+obj->pos,items*sizeof(Type));			\
    if (swap) for (i=0;i<items;i++) val[i]=Fswap(val[i]);		\
    obj->pos += items*sizeof(Type);					\
  }

/**
 * nsp_smio_get:
 * @F: a #NspSMio 
 * @x: an array pointer 
 * @n: an integer 
 * @type: a string coding type of @x.
 * @items_read: an int pointer 
 * 
 * reads data from #NspSMio the byte order being controled by 
 * the way @F was opened. This function can be used to read data coded 
 * in little or big endian 
 * if read fails @items_read will contain the number of properly read items. 
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_smio_get(NspSMio *F,void *x,int n,const char *type,int *items_read)
{  
  char c1,swap_c;
  int i,items=n; 
  int nc=strlen(type);
  if ( nc == 0) 
    {
      Scierror("mget: format is of length 0\r\n");
      return FAIL;
    }
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  
  swap = ( USE_SWAP(F->obj->flag) ) ? (( is_little_endian() ) ? FALSE :  TRUE) : FALSE; 
  /* check if swap was given in type */
  swap_c = type[strlen(type)-1]; 
  if (swap_c == 'b' ) 
    swap = ( is_little_endian() ) ? TRUE : FALSE ;
  else if (swap_c == 'l' ) 
    swap = ( is_little_endian() ) ? FALSE : TRUE ;

  switch ( type[0] )
    {
    case 'i' : MGET_(x,n,F->obj,swap,int,swapi);break;
    case 'l' : MGET_(x,n,F->obj,swap,long,swapl);break;
    case 's' : MGET_(x,n,F->obj,swap,short,swapw);break;
    case 'c' : MGET_CHARS_(x,n,F->obj,char) ; break;
    case 'd' : MGET_(x,n,F->obj,swap,double,swapd);break;
    case 'f' : MGET_(x,n,F->obj,swap,float,swapf);break;
    case 'u' :
      c1=( strlen(type) < 1) ? ' ': type[1];
      switch ( c1)
	{
	case 'i' :  MGET_(x,n,F->obj,swap,unsigned int,swapi); break;
	case 'l' :  MGET_(x,n,F->obj,swap,unsigned long,swapl); break;
	case 's' :  MGET_(x,n,F->obj,swap,unsigned short,swapw); break;
	case ' ' :  MGET_(x,n,F->obj,swap,unsigned int,swapi); break;
	case 'c' :  MGET_CHARS_(x,n,F->obj,unsigned char); break;
	default :  
	  Scierror("mget '%c' unrecognized conversion character\n",c1);
	  return FAIL;
	}
      break;
    default :
      Scierror("mget '%c' unrecognized conversion character\n",type[0]);
      return FAIL;
      break;
    }
  *items_read = items;
  return OK;
}

/**
 * nsp_smio_getstr:
 * @F: a #NspSMio 
 * @start: pointer to string to be returned 
 * @n: an integer 
 * 
 * allocate and read a string of size @n which is 
 * returned in @start.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_getstr (NspSMio *F, char *start, int n,int *n_read)
{ 
  int count;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  count = Min(n, F->obj->len- F->obj->pos);
  memcpy(start,F->obj->D + F->obj->pos,count*sizeof(char));
  F->obj->pos += count;
  start[count]='\0';
  *n_read = count;
  return OK;
}

/**
 * nsp_smio_putstr:
 * @F: a #NspSMio
 * @str: a char pointer 
 * 
 * writes string @str in stream @F.
 * 
 * Return value: 
 **/

int nsp_smio_putstr(NspSMio *F, const char *str)
{ 
  int n, nok;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  n = F->obj->pos + strlen(str)+1;
  if ( n > F->obj->len ) 
    {
      F->obj->D = realloc(F->obj->D, n);
      if ( F->obj->D == NULL)					
	{							
	  Scierror("Error: cannot enlarge smio buffer\n");	
	  return FAIL;						
        }			
      F->obj->len = n; 
    }
  nok = sprintf(F->obj->D+F->obj->pos,"%s",str);
  F->obj->pos += strlen(str);
  return OK;
}

/**
 * nsp_smio_getc:
 * @F: a #NspSMio
 * 
 * gets next available character.
 * 
 * Return value: 
 **/

int nsp_smio_getc(NspSMio *F)
{ 
  int c = EOF;
  if ( F->obj->pos < F->obj->len )
    {
      c = *(F->obj->D + F->obj->pos);
      F->obj->pos++;
    }
  return c;
}

/**
 * nsp_smio_ungetc:
 * @F: a #NspSMio
 * 
 * gets next available character.
 * 
 * Return value: 
 **/

int nsp_smio_ungetc(NspSMio *F,int c)
{ 
  if ( F->obj->pos > 0 ) 
    {
      F->obj->pos--;
      return c;
    }
  return EOF;
}

/*
 * nsp_fscan_matrix 
 */ 

#define INFOSIZE 1024

static int  Info_size = 0;
static char *Info= NULL;
static int nsp_smio_read_line(NspSMio *fd,int *mem);
static int count_tokens(char *string);

/**
 * nsp_smio_scanf_matrix:
 * @F: a #NspSMio 
 * @format: a string 
 * @M: A #NspMatrix 
 * @flag: an integer 
 * @S: a #NspSMatrix
 * 
 * reads a scalar matrix in the file given by @F.
 * The first non numeric values found in file @F are stored in a string matrix @S 
 * (only if @flag is %TRUE). The matrix is returned in @M. 
 * @format is unused.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_scanf_matrix(NspSMio *F,char *format,NspMatrix **M,int flag,NspSMatrix **S)
{
  int mem=OK,c, i,j,rows,cols,n, vl = -1;
  char *start,*end;
  long int offset;
  double x;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }

  if ( Info == NULL && (Info =new_nsp_string_n(INFOSIZE)) == NULLSTRING) return FAIL;
  Info_size = INFOSIZE;
  
  /* mark position */
  
  nsp_smio_tell(F,&offset) ;
  
  /* first pass to get colums and rows ***/
  strcpy(Info,"--------");
  n =0; 
  while ( sscanf(Info,"%lf",&x) <= 0 && n != EOF ) 
    { 
      n=nsp_smio_read_line(F,&mem); 
      if ( mem == FAIL) return FAIL;
      vl++;
    }
  if ( n == EOF )
    {
      Scierror("fscanfMat: cannot find matrix data \n");
      return FAIL;
    }
  cols = count_tokens(Info);
  rows = 1;
  while (1) 
    { 
      int cols1;
      n=nsp_smio_read_line(F,&mem);
      if ( mem == FAIL) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      if ( sscanf(Info,"%lf",&x) <= 0) break;
      cols1 = count_tokens(Info);
      if ( cols1 != cols ) 
	{
	  break; 
	}
      rows++;
    }
  if ( cols == 0 || rows == 0) rows=cols=0;
  
  if ((*M = nsp_matrix_create(NVOID,'r',rows,cols))== NULLMAT) return FAIL;

  /* second pass to read data **/
  
  nsp_smio_seek(F,offset,"set");
  
  /* non numeric lines in S **/
  if ( flag == TRUE ) 
    {
      if (( *S=nsp_smatrix_create_with_length(NVOID,vl,1,-1))== NULLSMAT) return FAIL;
    }

  for ( i = 0 ; i < vl ; i++) 
    {
      nsp_smio_read_line(F,&mem);
      if ( mem == FAIL) return FAIL;
      if ( flag == TRUE ) 
	{
	  if (((*S)->S[i]=new_nsp_string(Info))==NULL) return FAIL;
	}
      
    }

  /* numeric data in M */
  start = F->obj->D + F->obj->pos;
  end = start;
  for (i=0; i < rows ;i++)
    {
      for (j=0 ; j < cols;j++)
	{ 
	  (*M)->R[i+rows*j]= strtod(end,&end);
	  
	}
    }
  F->obj->pos += end - start;

  while ( (c = nsp_smio_getc(F)) != '\n' && c != EOF) {};
  
  /* just in case Info is too Big */ 

  if ( Info_size > INFOSIZE ) 
    {
      Info_size = INFOSIZE;
      Info = realloc(Info,(Info_size+1)*sizeof(char));
    }
  return OK;
}  


static int nsp_smio_read_line(NspSMio *F,int *mem)
{
  int n=0;
  while (1)
    {
      char c = nsp_smio_getc(F);
      if ( n > Info_size ) 
	{
	  Info_size += INFOSIZE;
	  if (( Info = realloc(Info,(Info_size+1)*sizeof(char)))==NULL) {
	    Scierror("Error: no more memory\n");
	    *mem=FAIL;
	    return EOF;
	  }
	}
      Info[n]= c ; 
      if ( c == '\n' || ( c == EOF && n > 0 ) ) 
	{ 
	  Info[n] = '\0' ; 
	  if ( n > 0 && Info[n-1] == '\r')  Info[n-1] = '\0' ; 
	  return 1;
	}
      else if ( c == (char)EOF ) 
	{
	  nsp_smio_ungetc(F,c);
	  return EOF;  
	}
      n++;
    }
}

static int count_tokens(char *string)
{
  char *copy = string; 
  int ntok=0;
  /* gobble spaces */
  while ( *copy==' ' || *copy=='\t' || *copy=='\n') copy++;
  if ( *copy == '\0') return 0;
  while (1) 
    {
      /* gobble token */
      while ( *copy != '\0' && *copy !=' ' && *copy !='\t' && *copy !='\n') copy++;
      ntok++;
      /* gobble spaces */
      while ( *copy==' ' || *copy=='\t' || *copy=='\n') copy++;
      if ( *copy == '\0' ) 
	return ntok;
    }
  return -1;
}


/**
 * nsp_smio_read_lines:
 * @F: a #NspSMio 
 * @S: a #NspSMatrix
 * @nlines: an integer 
 * 
 * Fills a #NspSMatrix object with lines read from file @F. 
 * @nlines gives the maximum number of lines to read. 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_read_lines(NspSMio *F,NspSMatrix **S,int nlines)
{
  int mem=OK,i,n;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  if ( Info == NULL && (Info =new_nsp_string_n(INFOSIZE)) == NULLSTRING) return FAIL;
  Info_size = INFOSIZE;
  if ((*S =nsp_smatrix_create_with_length(NVOID,nlines,1,-1))== NULLSMAT) return FAIL;
  for ( i= 0 ; i < nlines ; i++)
    {
      n = nsp_smio_read_line(F,&mem);
      if ( mem == FAIL) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      if (((*S)->S[i]=new_nsp_string(Info))==NULL) return FAIL;
    }
  if ( i < nlines ) 
    {
      /* we have read less than nlines */
      NspSMatrix *S1; 
      int j;
      if ((S1 =nsp_smatrix_create_with_length(NVOID,i,1,-1))== NULLSMAT) return FAIL;
      for ( j= 0 ; j < i ; j++) 
	{
	  S1->S[j] = (*S)->S[j];
	  (*S)->S[j]= NULL;
	}
      for ( j = i ; j < nlines ; j++) (*S)->S[j]=NULL;
      nsp_smatrix_destroy(*S);
      *S = S1;
    }

  /* just in case Info is too Big */ 
  if ( Info_size > INFOSIZE ) 
    {
      Info_size = INFOSIZE;
      Info = realloc(Info,(Info_size+1)*sizeof(char));
    }
  return OK;
}

/**
 * nsp_smio_scanf_smatrix:
 * @F: a #NspSMio
 * @S: a #NspSMatrix
 * 
 * Creates and fills a #NspSMatrix object with file contents.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_scanf_smatrix(NspSMio *F,NspSMatrix **S)
{
  long int offset;
  int mem=OK;
  int rows=0,n;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }

  if ( Info == NULL && (Info =new_nsp_string_n(INFOSIZE)) == NULLSTRING) return FAIL;
  Info_size = INFOSIZE;

  /* mark position */
  
  nsp_smio_tell(F,&offset) ;

  /* first pass to get number of rows ***/
  
  while (1) 
    { 
      n=nsp_smio_read_line(F,&mem);
      if ( mem == FAIL) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      rows++;
    }
  if ( rows == 0) 
    {
      Scierror("get_smatrix: cannot find smatrix data\n");
      return FAIL;
    }

  if ((*S =nsp_smatrix_create_with_length(NVOID,rows,1,-1))== NULLSMAT) return FAIL;

  /* second pass to read data **/
  
  nsp_smio_seek(F,offset,"set");
  rows=0;
  while (1)
    {
      n = nsp_smio_read_line(F,&mem);
      if ( mem == FAIL) return FAIL;
      if ( n == EOF ||  n == 0 ) break;
      if (((*S)->S[rows++]=new_nsp_string(Info))==NULL) return FAIL;
    }

  /* just in case Info is too Big */ 

  if ( Info_size > INFOSIZE ) 
    {
      Info_size = INFOSIZE;
      Info = realloc(Info,(Info_size+1)*sizeof(char));
    }
  return OK;
}  

/**
 * nsp_smio_printf_matrix:
 * @F: a #NspSMio 
 * @format: a string.
 * @sep: a string 
 * @M: a #NspMatrix 
 * @S: a #NspSMatrix
 * 
 * prints matrix @M in file @F using @format for formating numbers and 
 * @sep as separator. if @S is non null it is writen first in the file 
 * and stands as comments.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_printf_matrix(NspSMio *F,char *format,char *sep,NspMatrix *M,NspSMatrix *S)
{
  char buf[512];
  int i,j;
  const char *fmt = "%f"; 
  const char *separator = " ";
  if ( format != NULL) fmt= format; 
  if ( sep != NULL) separator = sep;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  if ( S != NULL) 
    for ( i=0 ; i < S->mn ; i++) 
      {
	if (  nsp_smio_putstr(F, S->S[i])== FAIL) 
	  return FAIL;
	if (  nsp_smio_putstr(F, "\n")== FAIL) 
	  return FAIL;
      }
  
  for (i = 0 ; i < M->m ; i++ ) 
    {
      for ( j = 0 ; j < M->n ; j++) 
	{
	  snprintf(buf,512,fmt,M->R[i+M->m*j]);
	  if (  nsp_smio_putstr(F, buf)== FAIL) 
	    return FAIL;
	  if (  nsp_smio_putstr(F, separator)== FAIL) 
	    return FAIL;
	}
      if (  nsp_smio_putstr(F, "\n")== FAIL) 
	return FAIL;
    }
  return OK;
}  

/**
 * nsp_smio_printf_smatrix:
 * @F: a #NspSMio 
 * @S:  a #NspSMatrix
 * 
 * prints the contents of @S in @F.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smio_printf_smatrix(NspSMio *F,NspSMatrix *S)
{
  int i, size=1;
  if ( !IS_OPENED(F->obj->flag)) 
    {
      Scierror("SMio is not opened\n");
      return FAIL;
    }
  /* enlarge the buffer at first pass to 
   * avoid multiple realloc in putstr 
   */
  for ( i=0 ; i < S->mn ; i++) 
    size += strlen( S->S[i]) + 1;

  if ( F->obj->pos + size  > F->obj->len)
    {
      F->obj->D = realloc(F->obj->D, F->obj->pos + size);
      if ( F->obj->D == NULL)
	{
	  Scierror("Error: cannot enlarge smio buffer\n");
	  return FAIL;
	}
      F->obj->len = F->obj->pos + size;    
    }
  
  for ( i=0 ; i < S->mn ; i++) 
    {
      if (  nsp_smio_putstr(F, S->S[i])== FAIL) 
	return FAIL;
      if (  nsp_smio_putstr(F, "\n") == FAIL) 
	return FAIL;
    }
  return OK;
}  


/*----------------------------------------------------------------
 * Utilities: 
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 *-------------------------------------------------------------*/

static const char readerr[] = "Premature EOF while reading sample file.";
static const char writerr[] = "Error writing sample file.  You are probably out of disk space.";

/* generic swap routine */

static void swapb(const char *l, char *f, int n)
{    
  int i;
  for (i= 0; i< n; i++) f[i]= l[n-i-1];
}

/* Byte swappers */

static unsigned short swapw(unsigned short us)
{
  return ((us >> 8) | (us << 8)) & 0xffff;
}

/* swapl : swap a long : note that a long size is machine dependant **/

static unsigned long swapl(long unsigned int ul)
{
  unsigned long  sdf;
  swapb((char *) &ul,(char *) &sdf, sizeof(unsigned long));
  return (sdf);
}

/* swap an int assumed to be on 4 bytes **/

static unsigned int swapi(unsigned int ul)
{
  return (ul >> 24) | ((ul >> 8) & 0xff00) | ((ul << 8) & 0xff0000) | (ul << 24);
}

/* return swapped 32-bit float */

static float swapf(float uf)
{
  if (sizeof(long) == sizeof(float)){
    union {
      unsigned long l;  /* we assume here long is 4 bytes **/
      float f;
    } u;
    u.f= uf;
    u.l= (u.l>>24) | ((u.l>>8)&0xff00) | ((u.l<<8)&0xff0000) | (u.l<<24);
    return u.f;
  }
  else {
    union {
      unsigned int l;  /* we assume here int is 4 bytes **/
      float f;
    } u;
    u.f= uf;
    u.l= (u.l>>24) | ((u.l>>8)&0xff00) | ((u.l<<8)&0xff0000) | (u.l<<24);
    return u.f;
  }
}

static double swapd(double df)
{
  double sdf;
  swapb((char *) &df,(char *) &sdf, sizeof(double));
  return (sdf);
}

#ifndef HAVE_STRERROR
/* strerror function */
char *strerror(int errcode)
{
  static char  nomesg[30];
  extern int sys_nerr;
  extern char *sys_errlist[];
  if (errcode < sys_nerr)
    return (sys_errlist[errcode]);
  else
    {
      sprintf (nomesg, "Undocumented error %d", errcode);
      return (nomesg);
    }
}
#endif


