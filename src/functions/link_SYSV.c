/*
 *  Link version for SYSV machine 
 *  using dlopen 
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>

#ifndef hppa
#include <dlfcn.h>
#else
#include <dl.h>
#endif

#if (defined(sun) && defined(SYSV)) 
#include <unistd.h>
#include <sys/wait.h>
#endif

#if defined(sun) 
#if defined(SYSV)
#include <sys/vnode.h>
#include <archives.h>
#else
#define RTLD_NOW 1
#include <dlfcn.h>
#endif
#endif

#if  defined(linux) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/wait.h>
#endif 

#if !defined(linux) && !defined(netbsd)  && !defined(freebsd) && !defined(__APPLE__)  && !defined(hppa) && !defined(__CYGWIN__)
#if (defined(sun) && ! defined(SYSV)) 
#else 
#include <sys/mode.h>
#endif
#endif 

#ifndef linux 
#ifdef __alpha
#include <c_asm.h>
#endif
#endif


#ifdef sgi
#define vfork fork
#endif

#if defined  __alpha || defined sgi
#include <a.out.h>
#endif

#include <string.h>

#define Min(x,y)	(((x)<(y))?(x):(y))
#define Max(x,y)	(((x)>(y))?(x):(y))

static void nsp_delete_symbols (int );
static int nsp_dlopen(nsp_const_string shared_path,int global);
static int nsp_dlsym(nsp_const_string ename, int ishared, char strf);
static int SetArgv  (char *argv[], char *files[],int first,int max,int *err);
static int SetArgv1  (char *argv[], char *files,int first,int max,int *err);
int CreateShared_unused  ( char *loaded_files[], char *tmp_file);

/*************************************
 * New version : link entry names 
 *   from new shared lib created with 
 *   files.
 *   return in ilib the number of the shared archive 
 *   or -1 or -5
 *   -1 : the shared archive was not loaded 
 *   -5 : pb with one of the entry point 
 *************************************/

void nsp_link_library(int iflag, int *rhs,int *ilib,nsp_const_string shared_path, char **en_names, char strf)
{
  int i;
  if ( iflag == 0 )
    {
      /* if no entry names are given we try a dl_open with global option*/
      *ilib  = nsp_dlopen(shared_path,( *rhs == 1 ) ? TRUE : FALSE );
    }
  if (*ilib  == -1 ) return;
  if ( *rhs >= 2) 
    {
      i=0 ;
      while ( en_names[i] != (char *) 0)
	{
	  if ( nsp_dlsym(en_names[i],*ilib,strf) == FAIL) 
	    *ilib=-5;
	  i++;
	}
    }
}

/**
 * nsp_link_status:
 * @void: 
 * 
 * 
 * 
 * Returns: 
 **/

int nsp_link_status(void)
{
  return(1);
}



/*
 * load a shared archive 
 */

#define MAXARGV 128

/*
 * a set of macros for hppa to emulate dl functions 
 * should be obsolete i.e dl family should be used on hp 
 */

#ifdef hppa 
static function dlsym(void *handle, const char *symbol)
{
  l_function f;
  dl handle hd1 = (shl_t) handle;
  irep= shl_findsym(&hd1,symbol,TYPE_PROCEDURE,&f);
  return ( irep == -1 ) ? NULL: f;
}

#define dlopen(x,y) ((x)== NULL) ? PROG_HANDLE : shl_load(x, BIND_IMMEDIATE | BIND_VERBOSE ,0L) 
#define dlclose(x) shl_unload((shl_t)(x))
#define dlhandle  shl_t 
#else /* hppa */
#define dlhandle  void *
#endif /* hppa */


/**
 * nsp_dlopen:
 * @shared_path: a null-terminated string giving a pathname 
 * @global: %TRUE or %FALSE 
 * 
 * loads  the dynamic library file named by @shared_path.
 * If @global is %TRUE then (RTLD_NOW| RTLD_GLOBAL) is passed to 
 * dlopen else RTLD_NOW is used. 
 * Note that @shared_path can be set to "nsp". In that case symbols 
 * from nsp executable can de searched. 
 * 
 * 
 * Returns: -1 in case of failure or the id as an integer of the 
 * loaded  dynamic library 
 **/

static int nsp_dlopen(nsp_const_string shared_path,int global)
{
  int i=0;
  dlhandle hd1;
  if ( strncmp(shared_path,"nsp",3) ==0 
       || strncmp(shared_path,"scilab",6) ==0  /* backward comp */
       )
    {
      /* try to open symbols from nsp executable 
       * does not work on all architectures 
       */
      hd1 = dlopen(NULL, RTLD_NOW);
    }
  else
    {
      int flag = ( global == TRUE) ? (RTLD_NOW| RTLD_GLOBAL) : RTLD_NOW;
      /* this will load the shared library */
      hd1 = dlopen(shared_path,flag);
    }
  if ( hd1 == NULL ) 
    {
#ifndef hppa
      char *loc = dlerror();
      if ( loc != NULL) Scierror("%s\n",loc);
      return(-1);
#else
      Scierror("link error\n");
      return(-1);
#endif
    }

  /* store the shared library in table 
   * first try to detect an unoccupied zone
   */
  for ( i = 0 ; i < Nshared ; i++ ) 
    {
      if ( hd[i].ok == FAIL) 
	{
	  hd[i].shl =  (unsigned long)hd1; 
	  strcpy(hd[i].tmp_file,shared_path); 
	  hd[i].ok = OK; 
	  /* Ok we stop */
	  return(i); 
	} 
    }
  /* we use the last position */
  if ( Nshared == ENTRYMAX ) 
    {
      Scierror("Error: cannot open shared library maxentry %d is reached\n",ENTRYMAX);
      return -1;
    }
  strcpy(hd[Nshared].tmp_file,shared_path);
  hd[Nshared].shl = (unsigned long)hd1;
  hd[Nshared].ok = OK;
  Nshared ++;
  return (Nshared-1);
}


/**
 * nsp_dlsym:
 * @ename: a string giving a symbol name 
 * @ishared: the id of a previously loaded shared library 
 * @strf: 'c' or 'f' 
 * 
 * Using the id @ishared of a dynamic library returned  by  nsp_dlopen and a symbol 
 * name, this function gets the address where that symbol is loaded into memory 
 * and store the symbol in the link table.
 * 
 * Returns: %OK or %FAIL 
 **/

static int nsp_dlsym(nsp_const_string ename, int ishared, char strf)
{
  int ish = Min(Max(0,ishared),ENTRYMAX-1);
  char enamebuf[NAME_MAXL];
  if ( strf == 'f' )
    Underscores(1,ename,enamebuf);
  else 
    Underscores(0,ename,enamebuf);

  /* lookup the address of the function to be called */
  if ( NEpoints == ENTRYMAX ) 
    {
      Scierror("Error: cannot link more functions maxentry %d reached\n",ENTRYMAX);
      return(FAIL);
    }
  if ( hd[ish].ok == FAIL ) 
    {
      Scierror("Error: Shared library %d does not exists\n",ish);
      return(FAIL);
    }
  /* entry was previously loaded */
  if ( SearchFandS(ename,ish) >= 0 ) 
    {
      Scierror("Warning: Entry name %s is already loaded from lib %d\n",ename,ish);
      return(OK);
    }
  EP[NEpoints].epoint = (l_function) dlsym((void *) hd[ish].shl, enamebuf);
  if ( (unsigned long) EP[NEpoints].epoint == (unsigned long) 0 )
    {
#ifndef hppa
      const char *loc = dlerror();
      if ( loc != NULL) Scierror("Error: %s\n",loc);
#else
      Scierror("Error: %s is not an entry point\n",enamebuf);
#endif
      return(FAIL);
    }
  else 
    {
      /* we don't add the _ in the table */
      strncpy(EP[NEpoints].name,ename,NAME_MAXL);
      EP[NEpoints].Nshared = ish;
      NEpoints++;
    }
  return(OK);  
}

/**
 * nsp_delete_symbols:
 * @ishared: integer 
 * 
 * remove from link table the entries which were 
 * linked from shared library @ishared.
 *
 **/

static void nsp_delete_symbols(int ishared)
{
  int ish = Min(Max(0,ishared),ENTRYMAX-1);
  int i=0;
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      if ( EP[i].Nshared == ish )
	{
	  int j;
	  for ( j = i ; j <= NEpoints - 2 ; j++ )
	    {
	      EP[j].epoint = EP[j+1].epoint;
	      EP[j].Nshared = EP[j+1].Nshared;
	      strcpy(EP[j].name,EP[j+1].name);
	    }
	  NEpoints--;
	}
    }
  
  if ( hd[ish].ok != FAIL)
    {
      dlclose((void *) hd[ish].shl);
      /* unlink(hd[ish].tmp_file);*/
      hd[ish].ok = FAIL;
    }
}

  

/*
 * creates a shared executable from the set of files 
 */

int CreateShared_unused (char **loaded_files, char *tmp_file)
{
  int argc=3,err=0;
  static int count=0;
  int i=0;
  char *libs;
  libs=nsp_getenv("SYSLIBS");
  Sciprintf("linking files ");
  while ( loaded_files[i] != NULL) 
    {
      Sciprintf("%s ",loaded_files[i]);
      i++;
    }
  Sciprintf(" to create a shared executable\n");
  count++;
  sprintf(tmp_file, "/tmp/SD_%d_/SL_%d_XXXXXX",(int) getpid(),count);
#ifdef HAVE_MKSTEMP 
  if ( mkstemp(tmp_file) == -1 ) 
    {
      Sciprintf("can't create a temporary file: \n");
      return(-1);
    }
#else 
  mktemp(tmp_file);
#endif
  {
    int pid, status, wpid;
    static char *argv[MAXARGV] = {
      /*   0        1         2    3  4   */
#ifdef sun
#if defined(SYSV)
      "/usr/ucb/ld", "-r", "-o", 0, 0
#else 
      "/usr/bin/ld", "-o", 0, 0,0 
#endif
#else
#ifdef linux
      "/usr/bin/ld", "-shared", "-o", 0, 0  
#else
#ifdef hppa
      "/bin/ld", "-b", "-o", 0, 0
#else
      "/bin/ld", "-shared", "-o", 0, 0  
#endif
#endif
#endif
     };
#if (defined(sun) && !defined(SYSV)) 
     argc = 2;
#endif
     argv[argc] = tmp_file; argc++;
     argc = SetArgv(argv,loaded_files,argc,MAXARGV,&err);
     if ( err == 1 ) return(-1);
     if (libs) 
       {
	 argc = SetArgv1(argv,libs,argc,MAXARGV,&err);
	 if ( err == 1 ) return(-1);
       }
     argv[argc] = (char *) 0;
	
#ifdef DEBUG
     for ( i=0 ; i < argc ; i++) 
       Sciprintf("arg[%d]=%s\n",i,argv[i]);
#endif	
     
     if ((pid = vfork()) == 0) {
       execv(argv[0], argv);
       _exit(1);
     }
     if (pid < 0) {
       Sciprintf("can't create new process: \n");
       return(-1);
     }
     while ((wpid = wait(&status)) != pid)
       if (wpid < 0) {
	 Sciprintf("no child !\n");
	 return(-1);
       }
     if (status != 0) {
       Sciprintf("ld returned bad status: %x\n", status);
       return(-1);
     }
   }
   return 0;
}

/*
 * Utility function 
 * files is a null terminated array of char pointers 
 * files[i] is broken into pieces ( separated by ' ') 
 * and positions are stored in argv starting at position 
 * first 
 */


static int SetArgv(char **argv, char **files, int first, int max, int *err)
{
  int i=0,j=first;
  *err=0;
  while ( files[i] != (char *) NULL) 
    {
      j= SetArgv1(argv,files[i],j,max,err);
      if (*err == 1) return(j);
      i++;
    }
  return(j);
}

static int SetArgv1(char **argv, char *files, int first, int max, int *err)
{
  int j=first;
  char *loc = files;
  while ( *loc == ' ' && *loc != '\0'  ) loc++;
  while ( *loc != '\0' )
    {
      argv[j] = loc; j++;
      if ( j == max ) 
	{
	  Sciprintf("Link too many files \n");
	  *err=1;
	  break;
	}
      if ( ( loc  = strchr(loc, ' ')) != (char *) 0) 
	{ 
	  *loc = 0;	loc++;
	  while ( *loc == ' ' && *loc != '\0'  ) loc++;
	}
      else
	{
	  break;
	}
    }
  return(j);
}
  
