/*****************************************************
 * Modified code from sox to read/write  wav files 
 *
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 * 
 *****************************************************/

#include "st.h"
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include "nsp/machine.h"
#include "nsp/interf.h"
#include "sox.h" 

struct soundstream informat;
ft_t ft;

static void checkformat (ft_t ft);
static void cleanup  (void);
static void init  (void);
static int filetype (int fd);

/************************************************************
 * loadwave 
 * --------
 * read a wav file and store results in res
 * if flag == 1, res is read 
 * if flag == 0, size of res is computed but res is not read
 ************************************************************/


int C2F(loadwave)(char * filename,double *res, integer * size_res,
		  integer flag,WavInfo *Wi, integer *ierr)
{
  long i,size_max;
#if defined(__alpha)|defined(__ia64__)
  int buf[BUFSIZ];
#else
  long buf[BUFSIZ];
#endif
  int olen;
  double *res1;
  *ierr=0;
  init();
  /* Get input format options */
  ft = &informat;
  ft->ierr=*ierr;
  /* Get input file */
  if ((ft->fp = fopen(filename, READBINARY)) == NULL)
    {
      Scierror("Can't open input file '%s': %s\n", 
	       filename, strerror(errno));
      *ierr=1;
      return 0;
    }
  ft->filename = filename;
#if defined(DOS) || defined(__OS2__) || defined(WIN32) || defined (__MWERKS__)
  informat.seekable  = 1;
#else
  informat.seekable  = (filetype(fileno(informat.fp)) == S_IFREG);
#endif
  informat.comment = informat.filename;

  /* Read and write starters can change their formats. */
  wavstartread(&informat,Wi,flag);
  if ( ft->ierr > 0 ) 
    {
      Scierror("Error while reading \n");
      *ierr=1;
      return 0;
    }
  checkformat(&informat);
  if ( ft->ierr > 0 ) 
    {
      Scierror("Error while reading \n");
      *ierr=1;
      return 0;
    }
  if ( flag == 1) 
    {
      Scierror("Input file: using sample rate %lu\n", 
	       informat.info.rate);
      Scierror("\tsize %s, style %s, %d %s\n",
	       sizes[informat.info.size], 
	       styles[informat.info.style], informat.info.channels, 
	       (informat.info.channels > 1) ? "channels" : "channel");
  }
  /* read chunk */
  size_max = *size_res ; 
  *size_res  = 0;
  olen = 1;
  res1=res;
  while ( olen > 0 )
    {
      olen = wavread(&informat,buf, (long) BUFSIZ);
      if ( ft->ierr > 0 ) 
	{
	  Scierror("Error while reading \n");
	  *ierr=1;
	  return 0;
	}
      *size_res += olen ;
      if (flag == 1 &&  *size_res > size_max ) 
	{
	  Scierror(" Sorry wav file too big \n");
	  return 0;
	}
      /** Scierror("2 premier nombres du bloc \n"); **/
      if (flag == 1) 
	for ( i = 0 ; i < olen ; i++ ) 
	  {
	    *res1++ = buf[i];
	  }
    }
  
  fclose(informat.fp);

  if ( flag == 1 ) 
    {
      for ( i = 0 ; i < *size_res ; i++ ) 
	{
	  if ( informat.info.size /8 == 1 )
	    res[i] = (res[i]-128)/128;
	  else 
	    res[i] = ((res[i])/32768)/65536;
	}
    }
  *ierr= ft->ierr;
  return 0 ;
}

/************************************************************
 * savewave 
 ************************************************************/

int C2F(savewave)(char * filename,double *res,integer * rate,
		  integer *size_res,integer *ierr)
{
  long buf[BUFSIZ];
  long i,size_max;
  int count;
  double m,*loc;
  *ierr=0;
  init();
  /* Get input format options */
  ft = &informat;
  ft->ierr=*ierr;
  /* Get input file */
  if ((ft->fp = fopen(filename, WRITEBINARY)) == NULL)
    {
      Scierror("Can't open output file '%s': %s\n", 
	   filename, strerror(errno));
      *ierr=1;
      return 0;
    }

  ft->filename = filename;
#if defined(DOS) || defined(__OS2__) || defined(WIN32) || defined (__MWERKS__)
  informat.seekable  = 1;
#else
  informat.seekable  = (filetype(fileno(informat.fp)) == S_IFREG);
#endif
  informat.comment = informat.filename;
  
  /* changing the formats. */
  informat.info.size = WORD;
  informat.info.rate = *rate ;
  informat.info.style = SIGN2;
  informat.info.channels =1;

  m=1.0;
  /* 
  m=res[0];
  for ( i = 0 ; i < *size_res ; i++) 
    {
      if ( Abs(res[i]) > m ) m = Abs(res[i]);
    }
  */
  wavstartwrite(&informat);
  if ( ft->ierr > 0 ) 
    {
      *ierr= ft->ierr;
      cleanup();
      return 0;
    }
  /* read chunk */
  size_max = *size_res ; 
  *size_res  = 0;
  count = 0 ;
  loc= res;
  while ( count < size_max ) 
    {
      double x;
      long int len, num;
      len = count + BUFSIZ;
      len = ( len > size_max ) ? size_max : len;
      for ( i = count ; i < len   ; i++ ) 
	{
	  /* x= v/m*2**(31-1); */
	  x= (*loc++)/m*2147483647;
	  buf[i-count ] = (long) x;
	  /** if (i < 2) Scierror("Ecriture d'un long %f %ld\n", x,buf[i-count]); **/
	}
      num = len - count ;
      wavwrite(&informat,buf, num );
      if ( ft->ierr > 0 ) 
	{
	  *ierr= ft->ierr;
	  cleanup();
	  return 0;
	}
      count = len;
    }
  wavstopwrite(&informat);
  fclose(informat.fp);
  res[0] = ((double)(size_max))/((double) (*rate));
  *ierr= ft->ierr;
  return 0;
}


void init(void) {
  /* init files */
  informat.info.rate      = 0;
  informat.info.size      = -1;
  informat.info.style     = -1;
  informat.info.channels  = -1;
  informat.comment   = NULL;
  informat.swap      = 0;
  informat.filetype  = "wav";
  informat.fp        = stdin;
  informat.filename  = "input";
}

/* 
 * Process input file -> effect table -> output file
 *	one buffer at a time
 */
#if defined(DOS) || defined(__OS2__) || defined(WIN32) || defined (__MWERKS__)
int filetype(int fd) { return 0;}
#else 
int filetype(int fd)
{
  struct stat st;
  fstat(fd, &st);
  return st.st_mode & S_IFMT;
}
#endif

/* called from util.c:fail */

void cleanup(void)
{
  if (informat.fp)
    fclose(informat.fp);
}

/* check that all settings have been given */

static void checkformat(ft_t ft)
{
  if (ft->info.rate == 0)
    {
      Scierror("Sampling rate for %s file was not given\n", ft->filename);
      ft->ierr=1;
      return;
    }
  if ((ft->info.rate < 100) || (ft->info.rate > 50000))
    {
      Scierror("Sampling rate %lu for %s file is bogus\n", 
	   ft->info.rate, ft->filename);
      ft->ierr=1;
      return;
    }

  if (ft->info.size == -1)
    {
      Scierror("Data size was not given for %s file\n", ft->filename);
      ft->ierr=1;
      return;
    }
  if (ft->info.style == -1 && ft->info.size != FLOAT)
    {
      Scierror("Data style was not given for %s file\n", ft->filename);
      ft->ierr=1;
      return;
    }
  /* it's so common, might as well default */
  if (ft->info.channels == -1)
    ft->info.channels = 1;
}



