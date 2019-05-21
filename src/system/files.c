/* Nsp
 * Copyright (C) 2003-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * system utilities.
 *------------------------------------------------------------------*/

#include <nsp/sciio.h>
#include <nsp/object.h>
#include <nsp/system.h>
#include <nsp/nsptcl.h>

#ifdef WIN32
#include <process.h> /* for getpid */
#include "tcl/generic/tclInt.h"
#endif

static char tmp_dir[FSIZE+1],buf[2*FSIZE+100],cur_dir[FSIZE+1];

#define MAXDATA 6

static char *dataStrings[] = {
  "$MANCHAPTERS",
  "~/.nsp/startup.sce",                  /* user start up               */
  "exec('SCI/demos/alldems.dem');",	 /* demos instructions file     */
  "home/scilab.hist",			 /* history file                */
  "home/scilab.save",			 /* on crash save file          */
  "exec('SCI/scilab.quit',-1);quit;",	 /* exit instructions file      */
  "exec('SCI/demos3/alldems.dem');",	 /* demos instructions file     */
};

/*----------------------------------------------
 *  get string .....
 *----------------------------------------------*/

char *get_sci_data_strings(int n)
{
  return dataStrings[Max(Min(n,MAXDATA),0)];
}


/**
 * set_nsp_tmpdir:
 * @void:
 *
 *  creates a tmp directory and record its name
 *  in %TMPDIR and in tmp_dir
 *
 **/

void set_nsp_tmpdir(void)
{
#ifndef WIN32
  int status;
#endif
  static int first =0;
  if ( first == 0 )
    {
      first++;
#ifdef WIN32
      if (! nsp_getenv("TEMP")) {
	sprintf(tmp_dir,"C:/tmp/SD_%d_",(int) getpid());
      } else {
	sprintf(tmp_dir,"%s\\SD_%d_",nsp_getenv("TEMP"),(int) getpid());
      }
      nsp_create_directory(tmp_dir);
#else
      sprintf(tmp_dir,"/tmp/SD_%d_",(int) getpid());
      sprintf(buf,"umask 000;if test ! -d %s; then mkdir %s; fi ",tmp_dir,tmp_dir);
      if ((status = system(buf)) == -1)
	{
	  Sciprintf("failed to create TMPDIR\n");
	}
#endif
      sprintf(buf,"NSP_TMPDIR=%s",tmp_dir);
      nsp_setenv("NSP_TMPDIR",tmp_dir);
    }
}


/**
 * get_nsp_tmpdir:
 * @void:
 *
 * returns the filename of temporary directory
 *
 * Return value: a pointer to the tmp_dir value
 **/

char *get_nsp_tmpdir(void)
{
  /* just in case */
  set_nsp_tmpdir();
  return tmp_dir;
}


/**
 * clean_tmpdir:
 * @void:
 *
 * clean nsp tmp directory.
 **/

#if (defined(hppa))
extern void hppa_sci_unlink_shared();
#endif

void clean_tmpdir(void)
{
#ifndef WIN32
  int status ;
#endif
  char *tmp_dir = get_nsp_tmpdir();
#ifdef WIN32
  nsp_remove_directory(tmp_dir,1,NULL);
#else
#if (defined(hppa))
  hppa_sci_unlink_shared();
#endif
  sprintf(buf,"rm -f -r %s >/dev/null  2>/dev/null",tmp_dir);
  status = system(buf);
  sprintf(buf,"rm -f -r /tmp/%d.metanet.* > /dev/null  2>/dev/null",
	  (int) getpid());
  if ((status = system(buf))== -1)
    {
    }
#endif
}


/**
 * nsp_change_curdir:
 * @path:
 *
 * change the value of current directory
 *
 * Return value: %OK or %FAIL
 **/

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
  /* a rajouter en XWindow ? pour transmettre l'info au menu
   * if (get_directory()==0)
   * *err=1;
   */
  /* scilab_status_show(path); XXXX en attente */
  return OK;
}


/**
 * nsp_get_curdir:
 * @void:
 *
 * returns a pointer to the value of current directory.
 *
 * Return value: a pointer to the current directory or %NULL.
 **/

char * nsp_get_curdir(void)
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


static char *SCI_a[] = {  "SCI/", "sci/", "$SCI", "SCI\\", "sci\\",
			  "NSP/", "nsp/", "$NSP", "NSP\\", "nsp\\", (char *) 0 };
static char *HOME_a[] = {  "HOME/", "home/", "~/" , "HOME\\", "home\\", "~\\" ,"$HOME", (char *) 0};
static char *TMP_a[] = {  "TMPDIR/","NSP_TMPDIR/", "tmpdir/","TMPDIR\\","NSP_TMPDIR\\",
			  "tmpdir\\", "$TMPDIR","$NSP_TMPDIR", (char *) 0};

static int expand_aliases(char *env, char **alias,const char *in_name, char *out_name,int out_size);

static int get_env(char *var,char *buf,int buflen,int iflag);


/**
 * nsp_path_expand:
 * @in_name: file name to be expanded
 * @out_name: buffer to store the result
 * @out_size: maximum number of characters that can be writen in @out_name.
 *
 * expand @in_name in @out_name.
 **/

void nsp_path_expand(const char *in_name, char *out_name, int out_size)
{
  int  nc= FSIZE+1, expanded = FALSE;
#ifdef WIN32
  int k;
#endif
  static char SCI[FSIZE+1],HOME[FSIZE+1],TMP[FSIZE+1];
  static int firstentry=0,  ok_sci=FALSE, ok_home = FALSE;
  if ( firstentry == 0 )
    {
      ok_sci= get_env("SCI",SCI,nc,1);
      ok_home= get_env("HOME",HOME,nc,1);
      set_nsp_tmpdir();
      if ( get_env("NSP_TMPDIR",TMP,nc,0) == FAIL)
	{
	  strcpy(TMP,"/tmp");
	}
      firstentry++;
    }
  /* try to expand */
  if ( ok_sci == OK)
    expanded = expand_aliases(SCI,SCI_a,in_name,out_name,out_size);
  if ( ok_home == OK && expanded == FALSE )
    expanded = expand_aliases(HOME,HOME_a,in_name,out_name,out_size);
  if ( expanded == FALSE )
    expanded = expand_aliases(TMP,TMP_a,in_name,out_name,out_size);
  if (expanded  == FALSE )
    strncpy(out_name,in_name, out_size);
  /* just keep the unix style    */
#ifdef WIN32
  for (k=0 ; k < strlen(out_name) ;k++) if ( out_name[k]=='\\') out_name[k]='/';
#endif

}

/*
 * expand in_name to produce out_name
 *     try to find alias[i] at the begining of in_name
 *     and replaces it by env in out_name
 *     out_name must be large enough to get the result
 *     else result is truncated
 * returns TRUE is alias expansion was performed else return FALSE
 */

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
	  if (alias[i][0] == '$' )
	    strncpy(out,in_name+strlen(alias[i]),(unsigned int) (out_last -out));
	  else
	    strncpy(out,in_name+strlen(alias[i])-1,(unsigned int) (out_last -out));
	  return TRUE ;
	}
      i++;
    }
  return FALSE;
}


/*
 * get_env
 */

static int get_env(char *var,char *buf,int buflen,int iflag)
{
  const char *local;
  if (( local=nsp_getenv(var)) == NULL)
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
