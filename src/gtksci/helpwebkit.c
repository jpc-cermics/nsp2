/*
 * Copyright (C) 2006, 2007 Apple Inc.
 * Copyright (C) 2007 Alp Toker <alp@atoker.com>
 * Copyright (C) 2008-2016 Jean-Philippe Chancelier <jpc@cermics.enpc.fr>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE COMPUTER, INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE COMPUTER, INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <gtk/gtk.h>
/* #include <webkit/webkit.h> */

#include <nsp/nsp.h>
#include <nsp/gtksci.h>
#include <nsp/system.h>
#include <nsp/hash.h>
#include <nsp/file.h>
#include <nsp/smatrix.h>

#include "../system/regexp.h"
#include "eggfindbar.h"

/* on windows TRUE and FALSE are undef by
 * "nsp/object.h"
 */

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#include <nsp/interf.h>
#include <nsp/nsptcl.h>
#include <nsp/nspthreads.h>

extern int nsp_open_webkit_window (const gchar *help_path,const gchar *locale,const gchar *help_file);

static int nsp_help_topic(const char *topic,char *buf);
static int nsp_help_browser_internal(char *mandir,char *locale,char *help_file);

typedef struct _nsp_webkit_data nsp_webkit_data;
struct _nsp_webkit_data {GAsyncQueue *queue;int ans; char *mandir,*locale,*help_file;};

/* utility function for nsp_help_browser  */

#ifdef NSP_WITH_MAIN_GTK_THREAD
static gboolean nsp_help_browser_idle(gpointer user_data)
{
  nsp_webkit_data *data = user_data;
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  data->ans= nsp_help_browser(data->mandir,data->locale,data->help_file);
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave();
#endif
  g_async_queue_push(data->queue,data);
  return FALSE;
}
#endif

/* It is safe to call this function from
 * non gtk thread.
 */

#ifdef NSP_WITH_MAIN_GTK_THREAD
int nsp_help_browser(char *mandir,char *locale,char *help_file)
{
  if (  g_thread_self() == nsp_gtk_main_thread())
    {
      return nsp_help_browser_internal(mandir,locale,help_file);
    }
  else
    {
      nsp_webkit_data data = {g_async_queue_new (),0,mandir,locale,help_file};
      g_idle_add(nsp_help_browser_idle,(gpointer) &data);
      g_async_queue_pop(data.queue);
      g_async_queue_unref(data.queue);
      return data.ans;
    }
  return 0;
}
#else
int nsp_help_browser(char *mandir,char *locale,char *help_file)
{
  return nsp_help_browser_internal(mandir,locale,help_file);
}
#endif



/*
 * mandir = man path or null (SCI+'man')
 * locale = "eng" or "fr"
 * help_file = null or absolute (XXX) file name
 * returns 0 on success and 1 if index.html not found
 */

static int nsp_help_topic(const char *topic,char *buf);

static int nsp_help_browser_internal(char *mandir,char *locale,char *help_file)
{
  int free = FALSE;
  char buf[FSIZE+32];
  const char *sci = nsp_getenv("SCI");
  char *l = locale ; /* (locale == NULL) ? "eng": locale ;  */

  if ( sci == NULL )
    {
      Sciprintf("Error: Cannot find manual, SCI is undefined\n");
      return 0;
    }
  if ( mandir == NULL)
    {
      free = TRUE;
      mandir = g_strconcat (sci, G_DIR_SEPARATOR_S, "man",G_DIR_SEPARATOR_S,  "html",  NULL);
    }

  /* expand topic -> filename in buf */
  if ( help_file == NULL )
    {
#ifdef WIN32
      int i;
      strcpy(buf,( strncmp(mandir,"//",2) == 0) ? "file:" : "file:///");
      strcat(buf, mandir);
      for ( i = 0 ; i < strlen(buf) ; i++)
	{
	  if ( buf[i] == '\\') buf[i]= '/';
	}
#else
      strcpy(buf,mandir);
#endif
      strcat(buf,"/generated/manual.html");
      /* Sciprintf("trying to see [%s]\n",buf); */
      start_sci_gtk(); /* in case gtk was not initialized */
      nsp_open_webkit_window(mandir,l,buf);
    }
  else if ( strncmp(help_file,"file:",5)==0 || strncmp(help_file,"http:",5)==0)
    {
      strcpy(buf,help_file);
      start_sci_gtk(); /* in case gtk was not initialized */
      nsp_open_webkit_window(mandir,l,buf);
    }
  else
    {
      if ( nsp_help_topic(help_file,buf)== FAIL ) return FAIL;
      if ( buf[0]== '\0') return OK;
      /* Sciprintf("XXX Help with [%s] [%s] [%s] %d\n",mandir,l,buf,FSIZE); */
      start_sci_gtk(); /* in case gtk was not initialized */
      nsp_open_webkit_window(mandir,l,buf);
    }
  if ( free == TRUE ) g_free(mandir);
  return 0;
}









static NspHash *nsp_help_table = NULLHASH;

static int nsp_help_fill_help_table(const char *index_file)
{
  nsp_string dirname = NULL;
  nsp_tcldstring name,filename;
  int all=TRUE;
  char buf[FSIZE+32];
  NspSMatrix *Sb = NULL;
  int xdr= FALSE,swap = TRUE,i;
#ifdef WIN32
  int j;
#endif
  NspFile *F = NULL;
  char *mode = "r";
  if ( index_file != NULL)
    nsp_path_expand(index_file,buf,FSIZE);
  else
    nsp_path_expand("NSP/man/html/generated/manual.4dx",buf,FSIZE);
  
  if ((dirname = nsp_dirname (buf))== NULL)return FAIL;

  /* Sciprintf(" nsp_help_fill_help_table: with buf=%s\n",buf); */
  /* Sciprintf(" nsp_help_fill_help_table: with dirname=%s\n",dirname); */

  if ((F=nsp_file_open(buf,mode,xdr,swap)) == NULLSCIFILE)
    {
      Scierror("Error %f not found\n",buf);
      return FAIL;
    }
  if ( nsp_fscanf_smatrix(F,&Sb) == FAIL)
    {
      nsp_file_close(F);
      nsp_file_destroy(F);
      return FAIL;
    }
  if (nsp_file_close(F) == FAIL  )
    {
      nsp_file_destroy(F);
      return FAIL;
    }
  nsp_file_destroy(F);
  /* initialize hash table for help */
  if (  nsp_help_table == NULLHASH)
    {
      if ( ( nsp_help_table = nsp_hash_create("%help",Sb->mn) ) == NULLHASH)
	return  FAIL;
    }
  for ( i = 0 ; i < Sb->mn ; i++)
    {
      char *str,*str1;
      NspObject *Obj;
      int nmatch;
      char patterns[]={ "^[^{]*{([^{]*)\\|LNK{([^{]*)}{[^$]*}"};
      Tcl_RegExp regExpr;
      nsp_tcldstring_init(&name);
      if (( regExpr = nsp_tclregexp_compile(patterns)) == NULL) goto bug;
      if ((nsp_tcl_regsub(Sb->S[i],regExpr,"\\1",&name,&nmatch,all))==FAIL)  goto bug;

      nsp_tcldstring_init(&filename);
      if ((nsp_tcl_regsub(Sb->S[i],regExpr,"\\2",&filename,&nmatch,all))==FAIL)  goto bug;
      str = str1= nsp_tcldstring_value(&name);
      /* remove \ from names */
      while ( *str1 != '\0')
	{
	  if (*str1 == '\\' ) str1++;
	  else *str++=*str1++;
	}
      *str='\0';
      /* fprintf(stderr,"val=[%s]\n",nsp_tcldstring_value(&name));*/
      /* need a join here */
      /* Sciprintf("dirname for help [%s]\n",dirname); */
#ifdef WIN32
      strcpy(buf,( strncmp(dirname,"//",2) == 0) ? "file:" : "file:///");
      strcat(buf, dirname);
      for ( j = 0 ; j < strlen(buf) ; j++)
	{
	  if ( buf[i] == '\\') buf[i]= '/';
	}
#else
      strcpy(buf,dirname);
#endif
      strcat(buf,"/");
      strcat(buf,nsp_tcldstring_value(&filename));
      if (( Obj = nsp_new_string_obj(nsp_tcldstring_value(&name),buf,-1))
	  == NULLOBJ)
	goto bug;
      /* Sciprintf(" nsp_help_fill_help_table: Enter %s with buf=%s\n",nsp_tcldstring_value(&name),buf); */
      nsp_tcldstring_free(&name);
      nsp_tcldstring_free(&filename);

      if (nsp_hash_enter(nsp_help_table,Obj) == FAIL) goto bug;
    }
  nsp_string_destroy(&dirname);
  nsp_smatrix_destroy(Sb);
  return OK;
 bug:
  if ( dirname != NULL ) nsp_string_destroy(&dirname);
  nsp_tcldstring_free(&name);
  nsp_tcldstring_free(&filename);
  nsp_smatrix_destroy(Sb);
  return FAIL;
}


static int nsp_help_topic(const char *topic, char *buf)
{
  NspObject *Obj;
  if ( strcmp(topic, "nsp")==0 )
    {
      /* re-initialize to scilab */
      if ( nsp_help_fill_help_table(NULL) == FAIL
	   || nsp_help_table == NULLHASH)
	{
	  Scierror("Error: cannot build help table \n");
	  return FAIL;
	}
      strcpy(buf,"");
      return OK;
    }
  if ( strcmp(topic, "scicoslab")==0 )
    {
      if ( nsp_help_fill_help_table("NSP/contrib/scicoslab/man/html/generated/manual.4dx")
	   || nsp_help_table == NULLHASH)
	{
	  Scierror("Error: cannot build help table \n");
	  return FAIL;
	}
      strcpy(buf,"");
      return OK;
    }

  if ( nsp_help_table == NULLHASH )
    {
      /* initialize */
      if ( nsp_help_fill_help_table(NULL) == FAIL
	   || nsp_help_table == NULLHASH)
	{
	  Scierror("Error: cannot build help table \n");
	  return FAIL;
	}
    }
  if ( strcmp(topic + strlen(topic) -3,"4dx")==0)
    {
      /* add contents of index file to help table */
      if ( nsp_help_fill_help_table(topic) == FAIL )
	{
	  Scierror("Error: cannot add index file to help table \n");
	  return FAIL;
	}
      strcpy(buf,"");
      return OK;
    }

  if ( nsp_hash_find(nsp_help_table,topic,&Obj)== FAIL)
    {
      Sciprintf("No man found for topic %s\n",topic);
      strcpy(buf,"");
    }
  else
    {
      strcpy(buf,((NspSMatrix *) Obj)->S[0]);
    }
  return OK;
}

