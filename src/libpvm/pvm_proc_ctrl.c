/* Copyright (c) 1997 by LIP ENS-LYON.  All Rights Reserved */

#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "nsp/machine.h"
#include "nsp/object.h"
#include "nsp/sciio.h"
#include "nsp/nsptcl.h"
#include <nsp/system.h> /* FSIZE+1 */
#include "../../pvm3/include/pvm3.h"
#include "sci_pvm.h"

/* stat function */
#if (defined __MSC__) || (defined __ABSC__) || defined(__MINGW32__) 
#define stat _stat 
#endif 

extern int pvmendtask(void);

/*------------------------------------------------------------------------
 * start pvm 
 *------------------------------------------------------------------------*/

int nsp_pvm_start(char *hostfile)
{
  char path[FSIZE+1];
  struct stat buf;
  const char *home =nsp_getenv("HOME") , *sci = nsp_getenv("SCI");
  char * argv[2];
  int argc=0;
  argv[0] = "";
  argv[1] = NULL;
  if ( hostfile == NULL ) 
    {
      /* try first $HOME/.pvmd.conf 
       * $SCI/.pvmd.conf sinon on laisse
       *	 faire pvmd...
       */ 
      if ( home != NULL) 
	{
	  sprintf(path,"%s/.pvmd.conf",home);
	  if (stat(path, &buf) == 0)
	    {
	      argc = 1;
	      argv[0] = path;
	      Sciprintf("pvm: using %s\n", path);
	    }
	};
      if ( argc != 1 &&  sci != NULL)
	{
	  sprintf(path,"%s/.pvmd.conf",sci);
	  if (stat(path, &buf) == 0)
	    {
	      argc = 1;
	      argv[0]= path;
	      Sciprintf("pvm: using $SCI/.pvmd.conf\n\t$SCI=%s\n",sci);
	      Sciprintf("\tSCI will have to be set on remote hosts \n");
	      Sciprintf("\tin order to spawn scilab\n");
	    }
	}
      if ( argc != 1 ) 
	{
	  Sciprintf("Warning: $HOME/.pvmd.conf or $SCI/.pvmd.conf not found.\n");
	  Sciprintf("\tassuming that PVM and scilab are in standard place on your net\n");
	  Sciprintf("\t (Cf. man pvmd3)\n");
	}
    }
  else 
    {
      if ( stat(hostfile, &buf) == 0 ) 
	{
	  argc = 1;
	  argv[0] = hostfile;
	}
      else 
	{
	  Sciprintf("%s: No such file or directory\n", hostfile);
	  return -1;
	}
    }
  return pvm_start_pvmd(argc, argv, 1);
} 


/*
 * halt pvm 
 */

int nsp_pvm_halt(void)
{
  int res ;
  /* Catch the SIGTERM */
  if (SIG_ERR == signal(SIGTERM,SIG_IGN)){
    Sciprintf("Error: pvm_halt - signal\n");
    return -1;
  }
  res =  pvm_halt(); 
  if (res == 0) {
    res= pvmendtask();
  }
  /* Catch the SIGPIPE and deflect SIGTERM */
#ifdef SIGPIPE
  if (SIG_ERR == signal(SIGPIPE,SIG_IGN)){
    Sciprintf("Error: pvm_halt - signal\n");
    return -1;
  }
#endif 
  if (SIG_ERR == signal(SIGTERM,SIG_DFL)){
    Sciprintf("Error: pvm_halt - signal\n");
    return -1;
  }   
  return res;
} 

