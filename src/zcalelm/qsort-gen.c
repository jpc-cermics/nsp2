/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * Generic code for Sorting Matrices a[i+n*j] 
 * This code is inserted in qsort.c 
 *
 * comparizon functions in case of double have been
 * modified so that nan are considered > Inf 
 * (Bruno Pincon, Nov 21, 2007)
 */

/* we want y to be expanded in CNAME thus we use XCNAME ! */

#define XCNAME(x,y) CNAME(x,y)

static int XCNAME(swapcode,ELT_TYPE)(char *parmi,char* parmj,int n, int inc) 
{ 		
  int i = n , inc1 = inc/sizeof(ELT_TYPE);
  register ELT_TYPE *pi = (ELT_TYPE *) (parmi); 		
  register ELT_TYPE *pj = (ELT_TYPE *) (parmj); 
  do { 						
    register ELT_TYPE t = *pi;		
    *pi = *pj;				
    *pj = t;				
    pi += inc1;
    pj += inc1;
  } while (--i > 0);				
  return(0);
}

#ifdef STRING_ONLY 
static int XCNAME(compareC,ELT_TYPE)(char *i,char *j)
{
  return(strcmp(*((ELT_TYPE *) i),*((ELT_TYPE*) j)));
}

static int XCNAME(compareD,ELT_TYPE)(char *i,char *j)
{
  return(- strcmp(*((ELT_TYPE *) i),*((ELT_TYPE*) j)));
}
#else 
#ifdef DOUBLE_ONLY
static int XCNAME(compareC,ELT_TYPE)(char *i,char *j)
{
  double a = *((double *)i);
  double b = *((double *)j);
  if ( ISNAN(a) )
    {
      if ( ! ISNAN(b) ) return 1;
    }
  else if ( ISNAN(b) )
    return -1;
  else if ( a > b )
    return 1;
  else if ( a < b )
    return -1;

  return 0;
}

static int XCNAME(compareD,ELT_TYPE)(char *i,char *j)
{
  double a = *((double *)i);
  double b = *((double *)j);
  if ( ISNAN(a) )
    {
      if ( ! ISNAN(b) ) return -1;
    }
  else if ( ISNAN(b) )
    return 1;
  else if ( a < b )
    return 1;
  else if ( a > b )
    return -1;

  return 0;
}

#else

static int XCNAME(compareC,ELT_TYPE)(char *i,char *j)
{
  if ( *((ELT_TYPE *)i) > *((ELT_TYPE *)j))
    return (1);
  if ( *((ELT_TYPE *)i) < *((ELT_TYPE *)j))
    return (-1);
  return (0);
}

static int XCNAME(compareD,ELT_TYPE)(char *i,char *j)
{
  if ( *((ELT_TYPE *)i) < *((ELT_TYPE *)j))
    return (1);
  if ( *((ELT_TYPE *)i) > *((ELT_TYPE *)j))
    return (-1);
  return (0);
}

#endif
#endif 

/******************************************************
 * Column sort of a matrix 
 ******************************************************/

void XCNAME(nsp_qsort_gen_col_sort_,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
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
		(dir == 'i' ) ? XCNAME(compareC,ELT_TYPE) : XCNAME(compareD,ELT_TYPE),
		XCNAME(swapcode,ELT_TYPE),swapcodeind);
    }
}

/******************************************************
 * Row sort of a matrix 
 ******************************************************/

void XCNAME(nsp_qsort_gen_row_sort_,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
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
		(dir == 'i' ) ? XCNAME(compareC,ELT_TYPE):XCNAME(compareD,ELT_TYPE),
		XCNAME(swapcode,ELT_TYPE),swapcodeind);
    }
}

/******************************************************
 * Global sort of a Matrix
 ******************************************************/

void XCNAME(nsp_qsort_gen_,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{  
  int i;
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n*p ; i++) 
	ind[i]= i+1;
    }
  nsp_qsort((char *) (a),(char *) (ind),flag, n*p, 
	    sizeof(ELT_TYPE),sizeof(int), 
	    (dir == 'i' ) ? XCNAME(compareC,ELT_TYPE):XCNAME(compareD,ELT_TYPE),
	    XCNAME(swapcode,ELT_TYPE),swapcodeind);
}

/*******************************************************
 *  lexicographic order with Rows ind is of size n
 *  ind gives the permutation of the rows which is applied 
 *  to sort them 
 ******************************************************/

static int XCNAME(lexicols,ELT_TYPE) =1;
static int XCNAME(lexirows,ELT_TYPE) =1;

static void XCNAME(setLexiSize,ELT_TYPE)(int n,int p) 
{
  XCNAME(lexicols,ELT_TYPE) = p;
  XCNAME(lexirows,ELT_TYPE) = n;
}

#ifdef STRING_ONLY 
static  int XCNAME(LexiRowcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int jc;
  for ( jc = 0 ; jc < XCNAME(lexicols,ELT_TYPE) ; jc++) 
    {
      int k = strcmp(*i,*j);
      if ( k != 0) return(k);
      i += XCNAME(lexirows,ELT_TYPE);
      j += XCNAME(lexirows,ELT_TYPE);
    }
  return (0);
}

static  int XCNAME(LexiRowcompareD,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int jc;
  for ( jc = 0 ; jc < XCNAME(lexicols,ELT_TYPE) ; jc++) 
    {
      int k = strcmp(*i,*j);
      if ( k != 0) return(-k);
      i += XCNAME(lexirows,ELT_TYPE);
      j += XCNAME(lexirows,ELT_TYPE);
    }
  return (0);
}

#else 
#ifdef DOUBLE_ONLY
static  int XCNAME(LexiRowcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int jc;
  for ( jc = 0 ; jc < XCNAME(lexicols,ELT_TYPE) ; jc++)
    {
      if ( ISNAN(*i) )
	{
	  if ( ! ISNAN(*j) ) return 1;
	}
      else if ( ISNAN(*j) )
	{
	  return -1;
	}
      else
	{
	  if (*i > *j)
	    return 1;
	  if (*i < *j)
	    return -1;
	}
      i += XCNAME(lexirows,ELT_TYPE);
      j += XCNAME(lexirows,ELT_TYPE);
    }
  return 0;
}

static  int XCNAME(LexiRowcompareD,ELT_TYPE)(ELT_TYPE *i, ELT_TYPE*j)
{
  int jc;
  for ( jc = 0 ; jc < XCNAME(lexicols,ELT_TYPE) ; jc++)
    {
      if ( ISNAN(*i) )
	{
	  if ( ! ISNAN(*j) ) return -1;
	}
      else if ( ISNAN(*j) )
	{
	  return 1;
	}
      else
	{
	  if (*i < *j)
	    return 1;
	  if (*i > *j)
	    return -1;
	}
      i += XCNAME(lexirows,ELT_TYPE);
      j += XCNAME(lexirows,ELT_TYPE);
    }
  return (0);
}
#else
static  int XCNAME(LexiRowcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int jc;
  for ( jc = 0 ; jc < XCNAME(lexicols,ELT_TYPE) ; jc++) 
    {
      if (*i > *j)
	return (1);
      if (*i < *j)
	return (-1);
      i += XCNAME(lexirows,ELT_TYPE);
      j += XCNAME(lexirows,ELT_TYPE);
    }
  return (0);
}
static  int XCNAME(LexiRowcompareD,ELT_TYPE)(ELT_TYPE *i, ELT_TYPE*j)
{
  int jc;
  for ( jc = 0 ; jc < XCNAME(lexicols,ELT_TYPE) ; jc++) 
    {
      if (*i < *j)
	return (1);
      if (*i > *j)
	return (-1);
      i += XCNAME(lexirows,ELT_TYPE);
      j += XCNAME(lexirows,ELT_TYPE);
    }
  return (0);
}
#endif
#endif

static int XCNAME(LexiRowswapcode,ELT_TYPE)(char *parmi,char * parmj,int n,int inc) 
{ 		
  int i = n,j;
  register ELT_TYPE *pi = (ELT_TYPE *) (parmi); 		
  register ELT_TYPE *pj = (ELT_TYPE *) (parmj); 
  /* if ( n!= 1) printf(" swapcode avec n != 1\n"); */
  do { 
    for ( j = 0 ; j < XCNAME(lexicols,ELT_TYPE) ; j++) 
      {
	register ELT_TYPE t = *(pi +XCNAME(lexirows,ELT_TYPE)*j);		
	*(pi + XCNAME(lexirows,ELT_TYPE)*j) = *(pj+XCNAME(lexirows,ELT_TYPE)*j);				
	*(pj + XCNAME(lexirows,ELT_TYPE)*j) = t;	
      }
    pi++;
    pj++;
  } while (--i > 0);				
  return(0);
}


void XCNAME(nsp_qsort_gen_lexirow_,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i;
  XCNAME(setLexiSize,ELT_TYPE)(n,p);
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n ; i++) 
	ind[i]= i+1;
    }
  nsp_qsort((char *) (a),(char *) (ind),flag, n, 
	    sizeof(ELT_TYPE),sizeof(int), 
	    (dir == 'i' ) ? XCNAME(LexiRowcompareC,ELT_TYPE):XCNAME(LexiRowcompareD,ELT_TYPE),
	    XCNAME(LexiRowswapcode,ELT_TYPE),swapcodeind);
}

/******************************************************
 *  lexicographic order with Cols ind is of size p
 *  ind gives the permutation of the column which is applied 
 *  to sort them 
 ******************************************************/

#ifdef STRING_ONLY 

static  int XCNAME(LexiColcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < XCNAME(lexirows,ELT_TYPE) ; ic++) 
    {
      int k = strcmp(*i,*j);
      if ( k != 0) return(k);
      i++;
      j++;
    }
  return (0);
}

static  int XCNAME(LexiColcompareD,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < XCNAME(lexirows,ELT_TYPE) ; ic++) 
    {
      int k = strcmp(*i,*j);
      if ( k != 0) return(-k);
      i++;
      j++;
    }
  return (0);
}

#else 

#ifdef DOUBLE_ONLY
static  int XCNAME(LexiColcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < XCNAME(lexirows,ELT_TYPE) ; ic++) 
    {
      if ( ISNAN(*i) )
	{
	  if ( ! ISNAN(*j) ) return 1;
	}
      else if ( ISNAN(*j) )
	{
	  return -1;
	}
      else
	{
	  if (*i > *j)
	    return 1;
	  if (*i < *j)
	    return -1;
	}
      i++;
      j++;
    }
  return 0;
}

static  int XCNAME(LexiColcompareD,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < XCNAME(lexirows,ELT_TYPE) ; ic++) 
    {
      if ( ISNAN(*i) )
	{
	  if ( ! ISNAN(*j) ) return -1;
	}
      else if ( ISNAN(*j) )
	{
	  return 1;
	}
      else
	{
	  if (*i < *j)
	    return 1;
	  if (*i > *j)
	    return -1;
	}
      i++;
      j++;
    }
  return 0;
}

#else
static  int XCNAME(LexiColcompareC,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < XCNAME(lexirows,ELT_TYPE) ; ic++) 
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
static  int XCNAME(LexiColcompareD,ELT_TYPE)(ELT_TYPE *i,ELT_TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < XCNAME(lexirows,ELT_TYPE) ; ic++) 
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

#endif 
#endif 

static int XCNAME(LexiColswapcode,ELT_TYPE)(char *parmi,char* parmj,int n,int inc) 
{ 		
  int i = n,ir;
  register ELT_TYPE *pi = (ELT_TYPE *) (parmi); 		
  register ELT_TYPE *pj = (ELT_TYPE *) (parmj); 
  /* if ( n!= 1) printf(" swapcode avec n != 1\n"); */
  do { 
    for ( ir = 0 ; ir < XCNAME(lexirows,ELT_TYPE) ; ir++) 
      {
	register ELT_TYPE t = *(pi +ir);		
	*(pi +ir) = *(pj+ir);				
	*(pj +ir) = t;	
      }
    pi += XCNAME(lexirows,ELT_TYPE) ;
    pj += XCNAME(lexirows,ELT_TYPE) ;
  } while (--i > 0);				
  return(0);
}


void XCNAME(nsp_qsort_gen_lexicol_,ELT_TYPE)(ELT_TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i;
  XCNAME(setLexiSize,ELT_TYPE)(n,p);
  if ( flag == 1) 
    {
      for ( i = 0 ; i < p ; i++) 
	ind[i]= i+1;
    }
  nsp_qsort((char *) (a),(char *) (ind),flag, p, 
	    n*sizeof(ELT_TYPE),sizeof(int), 
	    (dir == 'i' ) ? XCNAME(LexiColcompareC,ELT_TYPE):XCNAME(LexiColcompareD,ELT_TYPE),
	    XCNAME(LexiColswapcode,ELT_TYPE),
	    swapcodeind);
}









