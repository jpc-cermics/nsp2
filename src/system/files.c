/*------------------------------------------------------------------
 * Copyright ENPC 2003 
 * Jean-Philippe Chancelier Enpc/Cermics
 * jpc@cermics.enpc.fr 
 *------------------------------------------------------------------*/

/*-------------------------------------------------------------------
 * dealing with files and directories 
 * XXXXX : SCI -> NSP 
 *-------------------------------------------------------------------*/


#include "files.h" 
#include "Sun.h"
#include "nsp/sciio.h"

static char tmp_dir[FSIZE+1],buf[FSIZE+1],cur_dir[FSIZE+1];

#define MAXDATA 5 

static char *dataStrings[] = {
  "$MANCHAPTERS",
  "exec('SCI/scilab.star',-1);",         /* start_up instructions file   */
  "exec('SCI/demos/alldems.dem');",	 /* demos instructions file      */
  "home/scilab.hist",			 /* history file                */
  "home/scilab.save",			 /* on crash save file          */
  "exec('SCI/scilab.quit',-1);quit;"	 /* exit instructions file      */
};

/*----------------------------------------------
 *  get string .....
 *----------------------------------------------*/

char *get_sci_data_strings(int n)
{
  return dataStrings[Max(Min(n,MAXDATA),0)];
}


/*----------------------------------------------
 *  create a tmp directory and record its name
 *  in TMPDIR and in tmp_dir  
 *----------------------------------------------*/

void set_nsp_tmpdir(void)
{
  static int first =0;
  if ( first == 0 ) 
    {
      first++;
#ifdef WIN32 
      if (!getenv("TEMP")) {
	sprintf(tmp_dir,"C:/tmp/SD_%d_",(int) getpid());
      } else {
	sprintf(tmp_dir,"%s\\SD_%d_",getenv("TEMP"),(int) getpid());
      }
      SciCreateDirectory(tmp_dir);
#else 
      sprintf(tmp_dir,"/tmp/SD_%d_",(int) getpid());
      sprintf(buf,"umask 000;if test ! -d %s; then mkdir %s; fi ",tmp_dir,tmp_dir);
      system(buf);
#endif 
      sprintf(buf,"TMPDIR=%s",tmp_dir);
      putenv(buf);
    }
}

/*----------------------------------------------
 * get tmp_dir path 
 *----------------------------------------------*/

char *get_nsp_tmpdir(void)
{
  /* just in case */ 
  set_nsp_tmpdir();
  return tmp_dir;
}

/*----------------------------------------------
 * clean tmp_dir 
 *----------------------------------------------*/

#if (defined(hppa))
extern void hppa_sci_unlink_shared();
#endif

void clean_tmpdir(void)
{
  char *tmp_dir = get_nsp_tmpdir(); 
#ifdef WIN32 
  SciRemoveDirectory(tmp_dir);
#else 
#if (defined(hppa))
  hppa_sci_unlink_shared();
#endif
  sprintf(buf,"rm -f -r %s >/dev/null  2>/dev/null",tmp_dir);
  system(buf);
  sprintf(buf,"rm -f -r /tmp/%d.metanet.* > /dev/null  2>/dev/null",
	  (int) getpid());
  system(buf);
#endif 
}


/*------------------------------------------------
 * Changes scilab current directory 
 *------------------------------*/

int nsp_change_curdir(char *path)
{
  if (path == (char*) 0) return OK;
#ifndef __ABSC__
  if (chdir(path) == -1) 
#else
  if (chdir_(path,strlen(path)) != 0) 
#endif
    {
      Sciprintf("Can't go to directory %s \n", path); 
      /** XXX : a remettre , sys_errlist[errno]); **/
      return FAIL;
    } 
  /** a rajouter en XWindow ? pour transmettre l'info au menu 
      if (get_directory()==0) 
      *err=1; **/
  /* scilab_status_show(path); XXXX en attente */ 
  return OK;
}

/*------------------------------
 * Get nsp current directory 
 * FIXME: when getcwd returns a NULL 
 *        pointer ERANGE should be checked
 *------------------------------*/

char * nsp_get_curdir()
{
#ifndef __ABSC__
  if (GETCWD(cur_dir, FSIZE) == (char*) 0)
#else
    if (GETCWD(cur_dir,FSIZE) != 0)
#endif
      {	/* get current working dir */
	Sciprintf("Can't get current directory\n");
	return NULL;
      }
#ifndef __ABSC__
  return cur_dir;
#else
  return strtok(cur_dir,"  ");
#endif
}


/*---------------------------------------------
 * expand in_name to produce out_name 
 *---------------------------------------------*/

static char *SCI_a[] = {  "SCI/", "sci/", "$SCI", "SCI\\", "sci\\",
			  "NSP/", "nsp/", "$NSP", "NSP\\", "nsp\\", (char *) 0 };
static char *HOME_a[] = {  "HOME/", "home/", "~/" , "HOME\\", "home\\", "~\\" ,"$HOME", (char *) 0};
static char *TMP_a[] = {  "TMPDIR/", "tmpdir/","TMPDIR\\", "tmpdir\\", "$TMPDIR", (char *) 0};

static int expand_aliases(char *env, char **alias,const char *in_name, char *out_name,int out_size);
static int get_env(char *var,char *buf,int buflen,int iflag);

void nsp_path_expand(const char *in_name, char *out_name, int out_size)
{
  int  nc= FSIZE+1;
  static char SCI[FSIZE+1],HOME[FSIZE+1],TMP[FSIZE+1];
  static int firstentry=0,k;
  if ( firstentry == 0 ) 
    {
      get_env("SCI",SCI,nc,1);
      get_env("HOME",HOME,nc,1);
      set_nsp_tmpdir();
      if ( get_env("TMPDIR",TMP,nc,0) == FAIL) 
	{
	  strcpy(TMP,"/tmp");
	}
      firstentry++;
    }
  if ( expand_aliases(SCI,SCI_a,in_name,out_name,out_size) == 0 )
    if ( expand_aliases(HOME,HOME_a,in_name,out_name,out_size) == 0 )
      if ( expand_aliases(TMP,TMP_a,in_name,out_name,out_size) == 0 )
	strncpy(out_name,in_name, out_size);
  /*strncpy(out_name,in_name,lout); */
#if defined(THINK_C)||defined(__MWERKS__)
  for (k=0 ; k < strlen(out_name) ;k++) if ( out_name[k]=='/') out_name[k]=':';
#else
#if defined(WIN32)
  for (k=0 ; k < strlen(out_name) ;k++) if ( out_name[k]=='/') out_name[k]='\\';
#else
  for (k=0 ; k < strlen(out_name) ;k++) if ( out_name[k]=='\\') out_name[k]='/';
#endif
#endif
}

/*---------------------------------------------
 * expand in_name to produce out_name 
 *     try to find alias[i] at the begining of in_name 
 *     and replaces it by env in out_name 
 *     out_name must be large enough to get the result 
 *              else result is truncated 
 *---------------------------------------------*/

static int expand_aliases(char *env, char **alias,const char *in_name, char *out_name,int out_size)
{
  char *out = out_name;
  char *out_last = out_name + out_size;
  int i=0;
  if ( env[0] == '\0' ) return 0;
  while ( alias[i] != (char *) 0) 
    {
      if ( strncmp(alias[i],in_name,strlen(alias[i])) == 0)
	{
	  strncpy(out,env,(unsigned int) (out_last -out));
	  out += strlen(env);
	  strncpy(out,in_name+strlen(alias[i])-1,(unsigned int) (out_last -out));
	  return 1;
	}
      i++;
    }
  return 0;
}


/*---------------------------------------------
 * get_env 
 *---------------------------------------------*/

static int get_env(char *var,char *buf,int buflen,int iflag)
{
  char *local;
  if (( local=getenv(var)) == NULL)
    {
      if ( iflag == 1 ) Sciprintf("Warning: environment variable %s not found\n",var);
      return FAIL;
    }
  else 
    {
      char *last; 
      strncpy(buf,local,buflen);
      /* is it useful ? */
      last = &buf[strlen(buf)-1];
      while ( *last == ' ' ) { last = '\0' ;  last--;}
    }
  return OK;
}

