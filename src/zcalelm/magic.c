/**************************************************************************
 *  MAGIC SQUARE
 *  Programed by;  Shin, Kwon Young.
 *  http://user.chollian.net/~brainstm/MagicSquare.htm
 * 
 * A magic square is an arrangement of the numbers from 1 to n^2 (n-squared) in an nxn matrix, 
 * with each number occurring exactly once, and such that the sum of the entries of any row, 
 * any column, or any main diagonal is the same. It is not hard to show that this sum must 
 * be n(n^2+1)/2.
 * 
 ***************************************************************************/

#include <stdlib.h>

static void odd_num(double *m,int n);
static void even_num(double *m,int n);
static void swap_double(double *m,int n,int i1, int j1, int i2, int j2);


void nsp_magic_matrix_fill(double *m,int n)
{
  if(n < 3 ) return ;
  if( n % 2) 
    odd_num(m,n);
  else 
    even_num(m,n);
}

static void odd_num(double *m,int n)
{
  int i,j,num=1;
  int nn=n*3/2;

  for(i=0; i < n; i++)
    for(j=0; j < n; j++)
      m[((j-i+nn)%n) + n*((i*2-j+n)%n)]=num++;
}

static void even_num(double *m,int n)
{
  int i,j,num=1;
  int nminus=n-1,nmiddle=n/2,nn=n*n+1;
  int osl=0;
  int switch_row[2];
  int last_switch_column;
  int first_block=(n-2)/4,second_block=nminus-first_block;
  int first_inside=n/4,second_inside=nminus-first_inside;

  for(j=0; j < n; j++)
    for(i=0; i < n; i++) {
      if(i >= first_inside && i <= second_inside && j >= first_inside && j <= second_inside)
	m[i+n*j]=num;
      else if((i > first_block && i < second_block) || (j > first_block && j < second_block))
	m[i+n*j]=nn-num;
      else m[i+n*j]=num;
      num++;
    }
  if(!(n%4)) return;

#if 0 
  switch_row[0]=random(nmiddle-1)+first_block+1;
  switch_row[1]=random(nmiddle-1);
  if(switch_row[1] >= first_block) switch_row[1]+=(nmiddle+1);
  last_switch_column=random(nmiddle-1);
  if(last_switch_column >= first_block) last_switch_column+=(nmiddle+1);
#else 
  switch_row[0]=nmiddle;
  switch_row[1]=0;
  last_switch_column=0;
#endif 

  for(i=0; i < nmiddle; i++) {
    if(i==first_block || i==second_block) {
      osl=1-osl;
      continue;
    }
    swap_double(m,n,second_block, i, second_block, nminus-i);
    swap_double(m,n,i, first_block, nminus-i, first_block);
    swap_double(m,n,i, second_block, nminus-i, second_block);
    swap_double(m,n,i, switch_row[osl], nminus-i, switch_row[osl]);
  }
  for(i=first_block+1; i < second_block; i++) {
    swap_double(m,n,first_block, i, second_block, i);
    swap_double(m,n,i, first_block, i, second_block);
  }
  swap_double(m,n,first_block, nmiddle, second_block, nmiddle);
  swap_double(m,n,last_switch_column, first_block, last_switch_column, second_block);
}

static void swap_double(double *m,int n,int i1, int j1, int i2, int j2)
{
  int k;
  k=m[i1+n*j1];
  m[i1+n*j1]=m[i2+n*j2];
  m[i2+n*j2]=k;
}

