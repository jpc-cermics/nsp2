/*
 * Copyright (C) 2006, 2007 Apple Inc.
 * Copyright (C) 2007 Alp Toker <alp@atoker.com>
 * Copyright (C) 2008 Jean-Philippe Chancelier <jpc@cermics.enpc.fr>
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
#include <webkit/webkit.h>

/* XXXX */
#include "nsp/config.h"
#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/gtksci.h"
#include "queue.h"
#include "uri.h"
#include "../system/files.h"
#include "../system/regexp.h"
#include "nsp/object.h"
#include "nsp/interf.h"
#include "nsp/nsptcl.h"

static GtkWidget* main_window=NULL;
static GtkWidget* uri_entry;
static GtkStatusbar* main_statusbar;
static WebKitWebView* web_view;
static gchar* main_title;
static gint load_progress;
static guint status_context_id;

static void
activate_uri_entry_cb (GtkWidget* entry, gpointer data)
{
  const gchar* uri = gtk_entry_get_text (GTK_ENTRY (entry));
  g_assert (uri);
  webkit_web_view_open (web_view, uri);
}

static void
update_title (GtkWindow* window)
{
  GString* string = g_string_new (main_title);
  g_string_append (string, " - WebKit Launcher");
  if (load_progress < 100)
    g_string_append_printf (string, " (%d%%)", load_progress);
  gchar* title = g_string_free (string, FALSE);
  gtk_window_set_title (window, title);
  g_free (title);
}

static void
link_hover_cb (WebKitWebView* page, const gchar* title, const gchar* link, gpointer data)
{
  /* underflow is allowed */
  gtk_statusbar_pop (main_statusbar, status_context_id);
  if (link)
    gtk_statusbar_push (main_statusbar, status_context_id, link);
}

static void
title_change_cb (WebKitWebView* web_view, WebKitWebFrame* web_frame, const gchar* title, gpointer data)
{
  if (main_title)
    g_free (main_title);
  main_title = g_strdup (title);
  update_title (GTK_WINDOW (main_window));
}

static void
progress_change_cb (WebKitWebView* page, gint progress, gpointer data)
{
  load_progress = progress;
  update_title (GTK_WINDOW (main_window));
}

static void
load_commit_cb (WebKitWebView* page, WebKitWebFrame* frame, gpointer data)
{
  const gchar* uri = webkit_web_frame_get_uri(frame);
  if (uri)
    gtk_entry_set_text (GTK_ENTRY (uri_entry), uri);
}

static void
destroy_cb (GtkWidget* widget, gpointer data)
{
  gtk_widget_destroy(main_window); 
  main_window=NULL;
}

static void
go_back_cb (GtkWidget* widget, gpointer data)
{
  webkit_web_view_go_back (web_view);
}

static void
go_forward_cb (GtkWidget* widget, gpointer data)
{
  webkit_web_view_go_forward (web_view);
}

#ifdef WEBKIT_ZOOM
static void
go_zoom_in_cb (GtkWidget* widget, gpointer data)
{
  webkit_web_view_zoom_in (web_view);
}

static void
go_zoom_out_cb (GtkWidget* widget, gpointer data)
{
  webkit_web_view_zoom_out (web_view);
}

static void
go_zoom_100_cb (GtkWidget* widget, gpointer data)
{
  double zl=webkit_web_view_get_zoom_level(web_view);
  if ( zl != 1.0) 
    webkit_web_view_set_zoom_level(web_view,1.0);
}
#endif

static GtkWidget*
create_browser ()
{
  GtkWidget* scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  web_view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET (web_view));

  g_signal_connect (G_OBJECT (web_view), "title-changed", G_CALLBACK (title_change_cb), web_view);
  g_signal_connect (G_OBJECT (web_view), "load-progress-changed", G_CALLBACK (progress_change_cb), web_view);
  g_signal_connect (G_OBJECT (web_view), "load-committed", G_CALLBACK (load_commit_cb), web_view);
  g_signal_connect (G_OBJECT (web_view), "hovering-over-link", G_CALLBACK (link_hover_cb), web_view);

  return scrolled_window;
}

static GtkWidget*
create_statusbar ()
{
  main_statusbar = GTK_STATUSBAR (gtk_statusbar_new ());
  status_context_id = gtk_statusbar_get_context_id (main_statusbar, "Link Hover");

  return (GtkWidget*)main_statusbar;
}

static GtkWidget*
create_toolbar ()
{
  GtkWidget* toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation (GTK_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);

  GtkToolItem* item;

  /* the back button */
  item = gtk_tool_button_new_from_stock (GTK_STOCK_GO_BACK);
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_back_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  /* The forward button */
  item = gtk_tool_button_new_from_stock (GTK_STOCK_GO_FORWARD);
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_forward_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);


#ifdef WEBKIT_ZOOM
  /* The zooms buttons */
  item = gtk_tool_button_new_from_stock (GTK_STOCK_ZOOM_IN);
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_zoom_in_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_ZOOM_OUT);
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_zoom_out_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_ZOOM_100);
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_zoom_100_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);
#endif 

  /* The URL entry */
  item = gtk_tool_item_new ();
  gtk_tool_item_set_expand (item, TRUE);
  uri_entry = gtk_entry_new ();
  gtk_container_add (GTK_CONTAINER (item), uri_entry);
  g_signal_connect (G_OBJECT (uri_entry), "activate", G_CALLBACK (activate_uri_entry_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  /* The go button */
  item = gtk_tool_button_new_from_stock (GTK_STOCK_OK);
  g_signal_connect_swapped (G_OBJECT (item), "clicked", G_CALLBACK (activate_uri_entry_cb), (gpointer)uri_entry);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  return toolbar;
}

static GtkWidget*
create_window ()
{
  GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size (GTK_WINDOW (window), 800, 600);
  gtk_widget_set_name (window, "GtkLauncher");
  g_signal_connect (G_OBJECT (window), "destroy", G_CALLBACK (destroy_cb), NULL);

  return window;
}


static int open_webkit_window (const gchar *help_path,const gchar *locale,const gchar *help_file)
{
  GtkWidget* vbox; 

  start_sci_gtk(); /* in case gtk was not initialized */

  if ( main_window == NULL) 
    {
      main_window = create_window ();
      vbox = gtk_vbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), create_toolbar (), FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), create_browser (), TRUE, TRUE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), create_statusbar (), FALSE, FALSE, 0);
      gtk_container_add (GTK_CONTAINER (main_window), vbox);
      /* uri = g_strconcat ("file://", help_path, "/", locale, "/", NULL); */
      webkit_web_view_open (web_view, help_file );
      /* gtk_widget_grab_focus (GTK_WIDGET (web_view)); */
      gtk_widget_show_all (main_window);
    }
  else 
    {
      webkit_web_view_open (web_view, help_file );
    }
  return 0;
}


/*
 * mandir = man path or null (SCI+'man')
 * locale = "eng" or "fr" 
 * help_file = null or absolute (XXX) file name 
 * returns 0 on success and 1 if index.html not found 
 */

int nsp_help_topic(const char *topic,char *buf);

int Sci_Help(char *mandir,char *locale,char *help_file) 
{
  char buf[FSIZE+1];
  char *sci = nsp_getenv("SCI"); 
  char *l = locale ; /* (locale == NULL) ? "eng": locale ;  */
  if ( mandir == NULL && sci != NULL) 
    mandir = g_strconcat (sci, G_DIR_SEPARATOR_S, "man",G_DIR_SEPARATOR_S,  "html",  NULL);

  /* expand topic -> filename in buf */
  if ( help_file == NULL )
    {
#ifdef WIN32 
      /* X: -> //X/ and \ -> / */
      if ( strncmp(mandir+1,":",1)==0) 
	{
	  char *str = mandir+2;
	  int i;
	  strcpy(buf,"//");buf[2]=mandir[0];
	  for ( i = 0 ; i < strlen(str)+1 ; i++)
	    {
	      if ( str[i] == '\\') buf[i+2]='/';
	      else buf[i+2]= str[i];
	    }
	}
      else 
	{
	  strcat(buf,mandir);
	}
#else 
      strcpy(buf,mandir);
#endif 
      strcat(buf,"/generated/manual.html");
      open_webkit_window(mandir,l,buf);
    }
  else if ( strncmp(help_file,"file:",5)==0 || strncmp(help_file,"http:",5)==0) 
    {
      strcpy(buf,help_file);
      open_webkit_window(mandir,l,buf);
    }
  else  
    {
      if ( nsp_help_topic(help_file,buf)== FAIL ) return FAIL; 
      if ( buf[0]== '\0') return OK; 
      open_webkit_window(mandir,l,buf);
    }
  return 0; 
}

static NspHash *nsp_help_table = NULLHASH;

/* XXX*/
extern nsp_string nsp_dirname(char *fileName);

static int nsp_help_fill_help_table(const char *index_file)
{
  nsp_string dirname;
  nsp_tcldstring name,filename;
  int all=TRUE;
  char buf[FSIZE+1];
  NspSMatrix *S = NULL;
  int xdr= FALSE,swap = TRUE,i;
  NspFile *F;
  char *mode = "r";
  if ( index_file != NULL) 
    nsp_path_expand(index_file,buf,FSIZE);
  else 
    nsp_path_expand("NSP/man/html/generated/manual.4dx",buf,FSIZE);

  if ((dirname = nsp_dirname (buf))== NULL)return FAIL;

  if ((F=nsp_file_open(buf,mode,xdr,swap)) == NULLSCIFILE)
    {
      Scierror("Error %f not found\n",buf);
      return FAIL;
    }
  if ( nsp_fscanf_smatrix(F,&S) == FAIL) 
    {
      nsp_file_close(F);
      return FAIL;
    }
  if (nsp_file_close(F) == FAIL  ) return FAIL;
  /* initialize hash table for help */
  if (  nsp_help_table == NULLHASH) 
    {
      if ( ( nsp_help_table = nsp_hash_create("%help",S->mn) ) == NULLHASH) 
	return  FAIL;
    }
  for ( i = 0 ; i < S->mn ; i++)
    {
      char *str,*str1;
      NspObject *Obj;
      int nmatch;
      char patterns[]={ "^[^{]*{([^{]*)\\|LNK{([^{]*)}{[^$]*}"};
      Tcl_RegExp regExpr;
      nsp_tcldstring_init(&name);
      if (( regExpr = nsp_tclregexp_compile(patterns)) == NULL) goto bug;
      if ((nsp_tcl_regsub(S->S[i],regExpr,"\\1",&name,&nmatch,all))==FAIL)  goto bug;

      nsp_tcldstring_init(&filename);
      if ((nsp_tcl_regsub(S->S[i],regExpr,"\\2",&filename,&nmatch,all))==FAIL)  goto bug;
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
      if ( strncmp(dirname+1,":",1)==0) 
	{
	  strcpy(buf,"//X");buf[2]=dirname[0];
	}
      else 
	{
	  buf[0]=dirname[0];buf[1]=dirname[1];
	}
      strcat(buf,dirname+2);
#else 
      strcpy(buf,dirname);
#endif 
      strcat(buf,"/");
      strcat(buf,nsp_tcldstring_value(&filename));
      if (( Obj = nsp_new_string_obj(nsp_tcldstring_value(&name),buf,-1))
	  == NULLOBJ)
	goto bug;
      nsp_tcldstring_free(&name);	 
      nsp_tcldstring_free(&filename);	
      
      if (nsp_hash_enter(nsp_help_table,Obj) == FAIL) goto bug;
    }
  return OK;
 bug:
  nsp_tcldstring_free(&name);	 
  nsp_tcldstring_free(&filename);	
  return FAIL;
}


int nsp_help_topic(const char *topic, char *buf)
{
  NspObject *Obj;
  if ( nsp_help_table == NULLHASH)
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
      return OK;
    }

  if ( nsp_hash_find(nsp_help_table,topic,&Obj)== FAIL) 
    {
      Sciprintf("No man for %s\n",topic);
      strcpy(buf,"");
      return OK;
    }
  else
    {
      strcpy(buf,((NspSMatrix *) Obj)->S[0]);
      /* Sciprintf("topic help file: [%s]\n",buf);  */
    }
  return OK;
}


