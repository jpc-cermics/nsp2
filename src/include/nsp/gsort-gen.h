#ifndef NSP_INC_GSORT_
#define NSP_INC_GSORT_

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/*
 * Generic code for Sorting Matrices a[i+n*j] 
 * This code is inserted in gsort.c 
 * with ELT_TYPE == double or int
 * 
 sed -e "s/ELT_TYPE/double/g" -e "s/NSP_INC_GSORT_/NSP_INC_GSORT_DOUBLE/" gsort-gen.h >! gsort-double.h
 sed -e "s/ELT_TYPE/int/g" -e "s/NSP_INC_GSORT_/NSP_INC_GSORT_INT/" gsort-gen.h >! gsort-int.h
 *
 */

static int CNAME(swapcode,ELT_TYPE)(char *parmi,char* parmj,int n) 
{ 		
  int i = n;
  register ELT_TYPE *pi = (ELT_TYPE *) (parmi); 		
  register ELT_TYPE *pj = (ELT_TYPE *) (parmj); 
  do { 						
    register ELT_TYPE t = *pi;		
    *pi++ = *pj;				
    *pj++ = t;				
  } while (--i > 0);				
  return(0);
}

static int CNAME(compareC,ELT_TYPE)(char *i,char *j)
{
  if ( *((ELT_TYPE *)i) > *((ELT_TYPE *)j))
    return (1);
  if ( *((ELT_TYPE *)i) < *((ELT_TYPE *)j))
    return (-1);
  return (0);
}

static int CNAME(compareD,ELT_TYPE)(char *i,char *j)
{
  if ( *((ELT_TYPE *)i) < *((ELT_TYPE *)j))
    return (1);
  if ( *((ELT_TYPE *)i) > *((ELT_TYPE *)j))
    return (-1);
  return (0);
}

/******************************************************
 * Column sort of a matrix 
 ******************************************************/

static void CNAME(ColSort,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i,j;
  if ( flag == 1) 
    {
      for ( j= 0 ; j < p ; j++ ) 
	{
	  for ( i = 0 ; i < n ; i++) 
	    ind[i+n*j]= i+1;
	}
    }
  for ( j= 0 ; j < p ; j++ ) 
    {
      nsp_qsort((char *) (a+n*j),(char *) (ind+n*j),flag, n, 
		sizeof(ELT_TYPE),sizeof(int), 
		(dir == 'i' ) ? CNAME(compareC,ELT_TYPE) : CNAME(compareD,ELT_TYPE),
		CNAME(swapcode,ELT_TYPE),swapcodeind);
    }
}

/******************************************************
 * Row sort of a matrix 
 ******************************************************/

static void CNAME(RowSort,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{  
  int i,j;
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n ; i++) 
	{
	  for ( j= 0 ; j < p ; j++ ) 
	    {
	      ind[i+n*j]= j+1;
	    }
	}
    }
  for ( i = 0 ; i < n ; i++) 
    {
      nsp_qsort((char *) (a+i),(char *) (ind+i),flag, p, 
		n*sizeof(ELT_TYPE),n*sizeof(int), 
		(dir == 'i' ) ? CNAME(compareC,ELT_TYPE):CNAME(compareD,ELT_TYPE),
		CNAME(swapcode,ELT_TYPE),swapcodeind);
    }
}


/******************************************************
 * Global sort of a Matrix
 ******************************************************/

static void CNAME(GlobalSort,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{  
  int i;
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n*p ; i++) 
	ind[i]= i+1;
    }
  nsp_qsort((char *) (a),(char *) (ind),flag, n*p, 
	    sizeof(ELT_TYPE),sizeof(int), 
	    (dir == 'i' ) ? CNAME(compareC,ELT_TYPE):CNAME(compareD,ELT_TYPE),
	    CNAME(swapcode,ELT_TYPE),swapcodeind);
}

/*******************************************************
 *  lexicographic order with Rows ind is of size n
 *  ind gives the permutation of the rows which is applied 
 *  to sort them 
 ******************************************************/

static int CNAME(lexicols,ELT_TYPE) =1;
static int CNAME(lexirows,ELT_TYPE) =1;

static void CNAME(setLexiSize,ELT_TYPE)(int n,int p) 
{
  CNAME(lexicols,ELT_TYPE) = p;
  CNAME(lexirows,ELT_TYPE) = n;
}

static  int CNAME(LexiRowcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int jc;
  for ( jc = 0 ; jc < CNAME(lexicols,ELT_TYPE) ; jc++) 
    {
      if (*i > *j)
	return (1);
      if (*i < *j)
	return (-1);
      i += CNAME(lexirows,ELT_TYPE);
      j += CNAME(lexirows,ELT_TYPE);
    }
  return (0);
}
static  int CNAME(LexiRowcompareD,ELT_TYPE)(ELT_TYPE *i, ELT_TYPE*j)
{
  int jc;
  for ( jc = 0 ; jc < CNAME(lexicols,ELT_TYPE) ; jc++) 
    {
      if (*i < *j)
	return (1);
      if (*i > *j)
	return (-1);
      i += CNAME(lexirows,ELT_TYPE);
      j += CNAME(lexirows,ELT_TYPE);
    }
  return (0);
}

static int CNAME(LexiRowswapcode,ELT_TYPE)(char *parmi,char * parmj,int n) 
{ 		
  int i = n,j;
  register ELT_TYPE *pi = (ELT_TYPE *) (parmi); 		
  register ELT_TYPE *pj = (ELT_TYPE *) (parmj); 
  /* if ( n!= 1) printf(" swapcode avec n != 1\n"); */
  do { 
    for ( j = 0 ; j < CNAME(lexicols,ELT_TYPE) ; j++) 
      {
	register ELT_TYPE t = *(pi +CNAME(lexirows,ELT_TYPE)*j);		
	*(pi + CNAME(lexirows,ELT_TYPE)*j) = *(pj+CNAME(lexirows,ELT_TYPE)*j);				
	*(pj + CNAME(lexirows,ELT_TYPE)*j) = t;	
      }
    pi++;
    pj++;
  } while (--i > 0);				
  return(0);
}


static void CNAME(LexiRow,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i;
  CNAME(setLexiSize,ELT_TYPE)(n,p);
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n ; i++) 
	ind[i]= i+1;
    }
  nsp_qsort((char *) (a),(char *) (ind),flag, n, 
	    sizeof(ELT_TYPE),sizeof(int), 
	    (dir == 'i' ) ? CNAME(LexiRowcompareC,ELT_TYPE):CNAME(LexiRowcompareD,ELT_TYPE),
	    CNAME(LexiRowswapcode,ELT_TYPE),swapcodeind);
}

/******************************************************
 *  lexicographic order with Cols ind is of size p
 *  ind gives the permutation of the column which is applied 
 *  to sort them 
 ******************************************************/

static  int CNAME(LexiColcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < CNAME(lexirows,ELT_TYPE) ; ic++) 
    {
      if (*i > *j)
	return (1);
      if (*i < *j)
	return (-1);
      i++;
      j++;
    }
  return (0);
}
static  int CNAME(LexiColcompareD,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < CNAME(lexirows,ELT_TYPE) ; ic++) 
    {
      if (*i < *j)
	return (1);
      if (*i > *j)
	return (-1);
      i++;
      j++;
    }
  return (0);
}

static int CNAME(LexiColswapcode,ELT_TYPE)(char *parmi,char* parmj,int n) 
{ 		
  int i = n,ir;
  register ELT_TYPE *pi = (ELT_TYPE *) (parmi); 		
  register ELT_TYPE *pj = (ELT_TYPE *) (parmj); 
  /* if ( n!= 1) printf(" swapcode avec n != 1\n"); */
  do { 
    for ( ir = 0 ; ir < CNAME(lexirows,ELT_TYPE) ; ir++) 
      {
	register ELT_TYPE t = *(pi +ir);		
	*(pi +ir) = *(pj+ir);				
	*(pj +ir) = t;	
      }
    pi += CNAME(lexirows,ELT_TYPE) ;
    pj += CNAME(lexirows,ELT_TYPE) ;
  } while (--i > 0);				
  return(0);
}


static void CNAME(LexiCol,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i;
  CNAME(setLexiSize,ELT_TYPE)(n,p);
  if ( flag == 1) 
    {
      for ( i = 0 ; i < p ; i++) 
	ind[i]= i+1;
    }
  nsp_qsort((char *) (a),(char *) (ind),flag, p, 
	    n*sizeof(ELT_TYPE),sizeof(int), 
	    (dir == 'i' ) ? CNAME(LexiColcompareC,ELT_TYPE):CNAME(LexiColcompareD,ELT_TYPE),
	    CNAME(LexiColswapcode,ELT_TYPE),
	    swapcodeind);
}

#endif 






