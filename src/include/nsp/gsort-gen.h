/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 **
 *********************************************************************/

static void CNAME(ColSort,TYPE)();
static void CNAME(RowSort,TYPE)();
static void CNAME(GlobalSort,TYPE)();
static void CNAME(LexiRow,TYPE)();
static void CNAME(LexiCol,TYPE)();
static void CNAME(inita,TYPE)();
static void CNAME(afficher,TYPE)();
static void CNAME(sorttest,TYPE)();


/******************************************************
 * Generic code for Sorting Matrices a[i+n*j] 
 * This code is inserted in gsort.c 
 * with TYPE == double or type = int 
 ******************************************************/

static int CNAME(swapcode,TYPE)(char * parmi,char * parmj,int n) 
{ 		
  int i = n;
  register TYPE *pi = (TYPE *) (parmi); 		
  register TYPE *pj = (TYPE *) (parmj); 
  do { 						
    register TYPE t = *pi;		
    *pi++ = *pj;				
    *pj++ = t;				
  } while (--i > 0);				
  return(0);
}

static int CNAME(compareC,TYPE)(char *i,char *j)
{
  if ( *((TYPE *)i) > *((TYPE *)j))
    return (1);
  if ( *((TYPE *)i) < *((TYPE *)j))
    return (-1);
  return (0);
}

static int CNAME(compareD,TYPE)(char *i,char *j)
{
  if ( *((TYPE *)i) < *((TYPE *)j))
    return (1);
  if ( *((TYPE *)i) > *((TYPE *)j))
    return (-1);
  return (0);
}

/******************************************************
 * Column sort of a matrix 
 ******************************************************/

static void CNAME(ColSort,TYPE)(TYPE *a,int *ind,int flag,int n,int p,char dir)
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
      sciqsort((char *) (a+n*j),(char *) (ind+n*j),flag, n, 
	       sizeof(TYPE),sizeof(int), 
	       (dir == 'c' ) ? CNAME(compareC,TYPE) : CNAME(compareD,TYPE),
	       CNAME(swapcode,TYPE),swapcodeind);
    }
}

/******************************************************
 * Row sort of a matrix 
 ******************************************************/

static void CNAME(RowSort,TYPE)(TYPE *a,int *ind,int flag,int n,int p,char dir)
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
      sciqsort((char *) (a+i),(char *) (ind+i),flag, p, 
	       n*sizeof(TYPE),n*sizeof(int), 
	       (dir == 'c' ) ? CNAME(compareC,TYPE):CNAME(compareD,TYPE),
	       CNAME(swapcode,TYPE),swapcodeind);
    }
}


/******************************************************
 * Global sort of a Matrix
 ******************************************************/

static void CNAME(GlobalSort,TYPE)(TYPE *a,int *ind,int flag,int n,int p,char dir)
{  
  int i,j;
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n*p ; i++) 
	ind[i]= i+1;
    }
  sciqsort((char *) (a),(char *) (ind),flag, n*p, 
	   sizeof(TYPE),sizeof(int), 
	   (dir == 'c' ) ? CNAME(compareC,TYPE):CNAME(compareD,TYPE),
	   CNAME(swapcode,TYPE),swapcodeind);
}

/*******************************************************
 *  lexicographic order with Rows ind is of size n
 *  ind gives the permutation of the rows which is applied 
 *  to sort them 
 ******************************************************/

static int CNAME(lexicols,TYPE) =1;
static int CNAME(lexirows,TYPE) =1;

static int CNAME(setLexiSize,TYPE)(int n,int p)
{
  CNAME(lexicols,TYPE) = p;
  CNAME(lexirows,TYPE) = n;
}

static  int CNAME(LexiRowcompareC,TYPE)(int *i, int *j)
{
  int jc;
  for ( jc = 0 ; jc < CNAME(lexicols,TYPE) ; jc++) 
    {
      if (*i > *j)
	return (1);
      if (*i < *j)
	return (-1);
      i += CNAME(lexirows,TYPE);
      j += CNAME(lexirows,TYPE);
    }
  return (0);
}
static  int CNAME(LexiRowcompareD,TYPE)(int *i, int *j)
{
  int jc;
  for ( jc = 0 ; jc < CNAME(lexicols,TYPE) ; jc++) 
    {
      if (*i < *j)
	return (1);
      if (*i > *j)
	return (-1);
      i += CNAME(lexirows,TYPE);
      j += CNAME(lexirows,TYPE);
    }
  return (0);
}

static int CNAME(LexiRowswapcode,TYPE)(char *parmi,char * parmj,int n) 
{ 		
  int i = n,j;
  register TYPE *pi = (TYPE *) (parmi); 		
  register TYPE *pj = (TYPE *) (parmj); 
  /* if ( n!= 1) printf(" swapcode avec n != 1\n"); */
  do { 
    for ( j = 0 ; j < CNAME(lexicols,TYPE) ; j++) 
      {
	register TYPE t = *(pi +CNAME(lexirows,TYPE)*j);		
	*(pi + CNAME(lexirows,TYPE)*j) = *(pj+CNAME(lexirows,TYPE)*j);				
	*(pj + CNAME(lexirows,TYPE)*j) = t;	
      }
    pi++;
    pj++;
  } while (--i > 0);				
  return(0);
}


static void CNAME(LexiRow,TYPE)(TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i,j;
  CNAME(setLexiSize,TYPE)(n,p);
  if ( flag == 1) 
    {
      for ( i = 0 ; i < n ; i++) 
	  ind[i]= i+1;
    }
  sciqsort((char *) (a),(char *) (ind),flag, n, 
	   sizeof(TYPE),sizeof(int), 
	   (dir == 'c' ) ? CNAME(LexiRowcompareC,TYPE):CNAME(LexiRowcompareD,TYPE),
	   CNAME(LexiRowswapcode,TYPE),swapcodeind);
}

/******************************************************
 *  lexicographic order with Cols ind is of size p
 *  ind gives the permutation of the column which is applied 
 *  to sort them 
 ******************************************************/

static  int CNAME(LexiColcompareC,TYPE)(TYPE *i,TYPE *j;
{
  int ic;
  for ( ic = 0 ; ic < CNAME(lexirows,TYPE) ; ic++) 
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
static  int CNAME(LexiColcompareD,TYPE)(TYPE *i,TYPE *j)
{
  int ic;
  for ( ic = 0 ; ic < CNAME(lexirows,TYPE) ; ic++) 
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

static int CNAME(LexiColswapcode,TYPE)(char *parmi,char *parmj,int n) 
{ 		
  int i = n,ir;
  register TYPE *pi = (TYPE *) (parmi); 		
  register TYPE *pj = (TYPE *) (parmj); 
  /* if ( n!= 1) printf(" swapcode avec n != 1\n"); */
  do { 
    for ( ir = 0 ; ir < CNAME(lexirows,TYPE) ; ir++) 
      {
	register TYPE t = *(pi +ir);		
	*(pi +ir) = *(pj+ir);				
	*(pj +ir) = t;	
      }
    pi += CNAME(lexirows,TYPE) ;
    pj += CNAME(lexirows,TYPE) ;
  } while (--i > 0);				
  return(0);
}


static void CNAME(LexiCol,TYPE)(TYPE *a,int *ind,int flag,int n,int p,char dir)
{
  int i,j;
  CNAME(setLexiSize,TYPE)(n,p);
  if ( flag == 1) 
    {
      for ( i = 0 ; i < p ; i++) 
	  ind[i]= i+1;
    }
  sciqsort((char *) (a),(char *) (ind),flag, p, 
	   n*sizeof(TYPE),sizeof(int), 
	   (dir == 'c' ) ? CNAME(LexiColcompareC,TYPE):CNAME(LexiColcompareD,TYPE),
	   CNAME(LexiColswapcode,TYPE),
	   swapcodeind);
}



#ifdef TEST 

#define N 2
#define P 2 

static TYPE CNAME(aa,TYPE)[4*4] = {4,4,1,1,6,7,2,1,3,4,5,2,9,8,7,6};
/*static TYPE aa[4*4] = {6,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5}; */

static void CNAME(inita,TYPE)(a,n,p)
     TYPE *a;
     int n,p;
{
  int i;
  if ( n == 4 && p == 4  ) 
    for (i=0; i < n*p; i++) a[i]=CNAME(aa,TYPE)[i];
  else 
    for (i=0; i < n*p; i++) a[i]=n*p-i;
  CNAME(afficher,TYPE)(a,"a",n,p,sizeof(TYPE));
}


static void CNAME(afficher,TYPE)(a,name,n,p)
     char *name;
     TYPE *a;
     int n,p;
{
  int i,j;
  printf("%s=\n",name);
  for ( i = 0 ; i < n ; i++) 
    {
      for ( j= 0 ; j < p ; j++ ) 
	{
	  printf("%4.2f ", a[i+N*j]);
	}
      printf("\n");
    }
}


static void CNAME(sorttest,TYPE)()
{
  TYPE a[N*P],b[N*P];
  int ind[N*P];
  int i,flag,j;
  int n=N,p=N;
  flag=1;

  /** Global sort example **/
  CNAME(inita,TYPE)(a,n,p) ;
  CNAME(GlobalSort,TYPE)(a,ind,flag,n,p,'c');
  CNAME(afficher,TYPE)(a,"glob a",n,p);
  afficherint(ind,"glob ind",n,p);

  /** Column sort example **/
  CNAME(inita,TYPE)(a,n,p) ;
  CNAME(ColSort,TYPE)(a,ind,flag,n,p,'c');
  CNAME(afficher,TYPE)(a,"col a",n,p);
  afficherint(ind,"col ind",n,p);

  /** Row sort example **/
  CNAME(inita,TYPE)(a,n,p) ;
  CNAME(RowSort,TYPE)(a,ind,flag,n,p,'c');
  CNAME(afficher,TYPE)(a,"row a",n,p);
  afficherint(ind,"row ind",n,p);

  /** Lexicographic Col sort **/
  CNAME(inita,TYPE)(a,n,p) ;
  CNAME(LexiCol,TYPE)(a,ind,flag,n,p,'c');
  CNAME(afficher,TYPE)(a,"lexico col a",n,p);
  afficherint(ind,"lexico col ind",1,p);

  /** Lexicographic Row sort **/
  CNAME(inita,TYPE)(a,n,p) ;
  CNAME(LexiRow,TYPE)(a,ind,flag,n,p,'c');
  CNAME(afficher,TYPE)(a,"lexico Row a",n,p);
  afficherint(ind,"lexico Row ind",n,1);
}

#endif





